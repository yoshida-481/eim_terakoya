package eim.command.business.service;

import common.bo.AttributeUpdater;
import common.util.AppConstant;
import common.util.AppObjectConditionHelper;
import common.util.AppObjectUtil;
import common.util.AppSecurityUtils;
import common.util.AttributeUtil;
import eim.bo.EIMAccessRole;
import eim.bo.EIMException;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMRelationType;
import eim.bo.EIMSecurity;
import eim.command.common.util.EIMCommandConstant;
import eim.command.common.util.FolderServiceErrorConstants;
import eim.net.EIMSession;
import eim.util.AccessUtils;
import eim.util.EIMConfig;
import eim.util.EIMConstant;
import eim.util.ObjectUtils;
import eim.util.OperationHistoryUtils;
import eim.util.RelationUtils;
import eim.util.SecurityUtils;
import eim.util.StringUtils;
import eim.util.WorkFlowUtils;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * フォルダ操作内部I/F
 * 
 */
public class FolderService {

	/**
	 * セッションはスレッドローカル（EIMThreadContext）より取得したセッション（EIMSession）を利用します.<br/>
	 */
	public FolderService() {
	}

	/**
	 * フォルダを作成する. タイプは「一般フォルダ」固定.
	 * @param parentObject 親オブジェクト
	 * @param folderName 作成するフォルダ名称
	 * @return EIMObject 作成したフォルダのオブジェクト
	 * @throws EIMException エラーケース及びエラーメッセージのキー/内容については基本設計書を参照してください.
	 */
	public EIMObject createFolder(EIMObject parentObject, String folderName) throws EIMException {
		EIMSession sess = null;
		try {
			sess = EIMThreadContext.getEIMSession();
			EIMObjectType folderType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("EIM.COMMAND.MKDIR.DEFAULT.FOLDER.TYPE"));
			return this.createFolder(parentObject, folderType, folderName);
		} catch(EIMException eimException) {
			throw eimException; 
		} catch(Exception e) {
			// ロールバック
			this.rollback(sess);
			throw new EIMException(sess, FolderServiceErrorConstants.SYSTEMERROR.getKey());
		}
	}

	/**
	 * フォルダを作成する.
	 * @param parentObject 親オブジェクト
	 * @param folderType 作成するフォルダタイプ
	 * @param folderName 作成するフォルダ名称
	 * @return EIMObject 作成したフォルダのオブジェクト
	 * @throws EIMException エラーケース及びエラーメッセージのキー/内容については基本設計書を参照してください.
	 */
	public EIMObject createFolder(EIMObject parentObject, EIMObjectType folderType, String folderName) throws EIMException {
		EIMSession sess = null;
		try {
			sess = EIMThreadContext.getEIMSession();
			AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
			
			long sec_id = this.check(sess, helper, parentObject, folderType, folderName);
			
			// オブジェクト作成.
			EIMObject object = ObjectUtils.createObject(sess, folderType, folderName);
			
			// 指定親オブジェクトと①のオブジェクトの関連付けを行う.
			EIMRelationType relType = RelationUtils.getRelationTypeByName(sess, "ドキュメント");
			RelationUtils.createRelation(sess, relType, parentObject, object, EIMConstant.DEPU_CHECK_NAME);
			// 属性設定.
			String path = AppObjectUtil.getPath(parentObject);
			if(path == null){
				// ワークスペースの場合、パス属性の値を保持していない.
				path = "/";
			}
			path += parentObject.getName() + "/";
			// パス.
			AppObjectUtil.setPath(sess, object, path);
			// セキュリティ.
			EIMSecurity sec = parentObject.getSecurity();
			if(sec != null) {
				SecurityUtils.setSecurity(sess, object, sec);
			}
			
			// WF付フォルダ直下に作成する場合.
			if (helper.isTypeOfFolderWithWorkflow(parentObject)) {
				// 属性「上位WFフォルダ」設定.
				AppObjectUtil.setAttr(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_HIGHER_WF_FOLDER"), parentObject.getId());						
				// 上位WF付フォルダのステータスを設定(引継ぐ).
				WorkFlowUtils.updateObjectStatus(sess, object, parentObject.getStatus());						
			} 
			else {
				// 親フォルダの「上位WFフォルダ」属性を取得.
				long higherWfFolderId = AppObjectUtil.getIntAttr(sess, parentObject, EIMConfig.get("ATTR_NAME_DOCUMENT_HIGHER_WF_FOLDER"), Integer.MIN_VALUE);
				
				if (higherWfFolderId != Integer.MIN_VALUE) {
					// 属性「上位WFフォルダ」設定.
					AppObjectUtil.setAttr(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_HIGHER_WF_FOLDER"), higherWfFolderId);
					// 上位WFフォルダ取得.
					EIMObject higherWfFolder = ObjectUtils.getObjectById(sess, higherWfFolderId);
					// 上位WF付フォルダのステータスを設定(引継ぐ)
					WorkFlowUtils.updateObjectStatus(sess, object, higherWfFolder.getStatus());												
				}
			}
			
			// 下位フォルダ管理セキュリティ.
			if (sec_id != Integer.MIN_VALUE) {
				AppObjectUtil.setAttr(sess, object, EIMConfig.get("ATTR_NAME_WORKSPACE_LOW_FOLDER_SEC"), sec_id);			
			}
			
			// 下位への引継ぎ属性、その他親オブジェクトからの引継ぎ属性.
			AttributeUpdater attUpdater = new AttributeUpdater(object.getId(), folderName, null);
			AttributeUtil.updateAttribute(sess, attUpdater);
			
			// 操作履歴.
			OperationHistoryUtils.create(sess, 
					EIMCommandConstant.COMMAND, 
					EIMCommandConstant.CREATE_FOLDER, 
					EIMConstant.TARGET_CREATE, 
					EIMConstant.OBJECT, 
					object,
					null, 
					null, 
					null, 
					EIMCommandService.VERSION + ":" + path);
			
			// アクセス履歴.
			AccessUtils.createAccess(sess, object, "EIM.ACCESS.TYPE.EXIF.MKDIR");
			
			sess.commit();
			
			return object;
			
		} catch(EIMException eimException) {
			// ロールバック
			this.rollback(sess);
			
			// フォルダ名称がDBで保持できる最大文字数を超過.
			if("EIM.ERROR.LOGIC.OBJECT.NAME.LENGTH.OVER".equals(eimException.getMessageKey())) {
				throw new EIMException(sess, FolderServiceErrorConstants.OVERFLOW_FOLDERNAME.getKey());
			}
			// 親オブジェクト下に既に同名フォルダが存在する.
			else if ("EIM.ERROR.LOGIC.OBJTYPE.NAME.DUPLICATE".equals(eimException.getMessageKey())) {
				throw new EIMException(sess, FolderServiceErrorConstants.DUPLICATE_FOLDERNAME.getKey());
			}
			else {
				throw eimException;
			}
			
		} catch(Exception e) {
			// ロールバック
			this.rollback(sess);
			throw new EIMException(sess, FolderServiceErrorConstants.SYSTEMERROR.getKey());
		}
	}
	
	/**
	 * 各種チェックを行い、後続処理で必要な下位フォルダ管理セキュリティIDを返却する.
	 * @param sess EIMSession
	 * @param helper ドキュメント管理オブジェクト判定の支援クラス
	 * @param parentObject 親オブジェクト
	 * @param folderType フォルダタイプ
	 * @param folderName フォルダ名
	 * @return sec_id 下位フォルダ管理セキュリティID
	 * @throws Exception
	 */
	private long check(EIMSession sess, 
			AppObjectConditionHelper helper, 
			EIMObject parentObject, 
			EIMObjectType folderType, 
			String folderName) throws Exception {
		
			// セッションがNULL
			if(sess == null) {
				throw new EIMException(sess, FolderServiceErrorConstants.SESSION_NULL.getKey());
			}

			// パラメータの親オブジェクトがnull
			if(parentObject == null) {
				throw new EIMException(sess, FolderServiceErrorConstants.PARENTOBJ_NULL.getKey());
			}
			// パラメータのフォルダタイプがnull
			if(folderType == null) {
				throw new EIMException(sess, FolderServiceErrorConstants.OBJTYPE_NULL.getKey());
			}
			// パラメータのフォルダ名称がnull (空文字も×)
			if(StringUtils.isBlank(folderName)) {
				throw new EIMException(sess, FolderServiceErrorConstants.FOLDERNAME_NULL.getKey());
			}
			// 親オブジェクトへの参照権限がない
			if (!SecurityUtils.authorized(sess, parentObject, sess.getUser(), EIMAccessRole.READ)) {
				throw new EIMException(sess, FolderServiceErrorConstants.PARENTOBJ_NOTFOUND.getKey());
			}

			// 親オブジェクトへの更新権限がない
			if (!SecurityUtils.authorized(sess, parentObject, sess.getUser(), EIMAccessRole.UPDATE)) {
				throw new EIMException(sess, FolderServiceErrorConstants.NO_CREATEROLE.getKey());
			}
			// 親オブジェクトのステータスがあり、「編集中」以外の場合
			if(parentObject.getStatus() != null && 
					parentObject.getStatus().getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_EDITTING) {
				throw new EIMException(sess, FolderServiceErrorConstants.STATUS_NOTEDITING.getKey());
			}
			
			long sec_id = AppObjectUtil.getIntAttr(sess, parentObject, EIMConfig.get("ATTR_NAME_WORKSPACE_LOW_FOLDER_SEC"), Integer.MIN_VALUE);
			
			// 親オブジェクトのフォルダ構成管理制限があり、作成権限がない
			if(sec_id != Integer.MIN_VALUE) {
				// 下位フォルダ管理セキュリティのロールチェック	
				if (!AppSecurityUtils.authorizedLowFolderSecurity(sess, parentObject, sess.getUser(), EIMAccessRole.CREATE)) {
					throw new EIMException(sess, FolderServiceErrorConstants.NO_FOLDERMANAGEMENTSEC.getKey());
				}
			}
			
			// 親オブジェクトがごみ箱
			if(AppObjectUtil.isObjectInRecycle(sess, parentObject)) {
				throw new EIMException(sess, FolderServiceErrorConstants.UNDER_RECYCLEBIN.getKey());
			}
			
			// 親オブジェクトのオブジェクトタイプがフォルダ または ワークスペース でない
			if(!helper.isTypeOfWorkspaceOrFolder(parentObject.getType())) {
				throw new EIMException(sess, FolderServiceErrorConstants.PARENTOBJ_NOTFOLDER.getKey());
			}
			
			// 指定したオブジェクトタイプの更新権限がない
			if(!AppSecurityUtils.authorizedByObjectType(sess, folderType, EIMAccessRole.UPDATE)) {
				throw new EIMException(sess, FolderServiceErrorConstants.OBJTYPE_NOTFOUND.getKey());
			}

			// 指定したオブジェクトタイプがフォルダでない
			if(!helper.isTypeOfFolder(folderType)) {
				throw new EIMException(sess, FolderServiceErrorConstants.OBJTYPE_NOTFOLDER.getKey());
			}
			
			// フォルダ名称に禁止文字が含まれている EIM.WARN.LOGIC.INVALIDNAME
			AppObjectUtil.checkValidateFName(sess, folderName);
			
			// 親オブジェクトがワークフロー付きフォルダで、かつ指定したフォルダタイプがワークフロー付きである場合
			
			if((helper.isTypeOfFolderWithWorkflow(parentObject) ||
					helper.isTypeOfFolderUnderFolderWithWorkflow(parentObject)) && 
					WorkFlowUtils.getWorkFlowByType(sess, folderType) != null) {
				throw new EIMException(sess, FolderServiceErrorConstants.UNDER_WFFOLDER.getKey());
			}
			
			return sec_id;
	}
	
	/**
	 * ロールバックを行う.
	 * @param sess EIMSession
	 */
	private void rollback(EIMSession sess) {
		if(sess != null) {
			try {
				sess.rollback();
			}
			catch(Exception e) {
				e.printStackTrace();
			}
		}
	}
}
