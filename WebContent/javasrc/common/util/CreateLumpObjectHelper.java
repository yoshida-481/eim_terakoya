package common.util;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang3.ArrayUtils;

import common.bo.AttributeUpdater;
import eim.bo.EIMAccessRole;
import eim.bo.EIMAttribute;
import eim.bo.EIMAttributeType;
import eim.bo.EIMDirectory;
import eim.bo.EIMException;
import eim.bo.EIMFile;
import eim.bo.EIMFormat;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMRelation;
import eim.bo.EIMRelationType;
import eim.bo.EIMResource;
import eim.bo.EIMSecurity;
import eim.bo.EIMUser;
import eim.bo.EIMVersion;
import eim.bo.EIMWorkFlow;
import eim.net.EIMSession;
import eim.util.AccessUtils;
import eim.util.AttributeUtils;
import eim.util.DateUtils;
import eim.util.EIMConfig;
import eim.util.EIMConstant;
import eim.util.FileUtils;
import eim.util.ObjectAttributeUtils;
import eim.util.ObjectUtils;
import eim.util.RelationUtils;
import eim.util.SecurityUtils;
import eim.util.StringUtils;
import eim.util.TypeConvertUtils;
import eim.util.UserUtils;
import eim.util.VersionUtils;
import eim.util.WorkFlowUtils;
import jp.co.ctc_g.eim.app.document.business.domain.DocumentAttributeTypeLayoutDomain;
import jp.co.ctc_g.eim.app.form.business.domain.AttributeTypeLayoutDomain;
import jp.co.ctc_g.eim.app.form.business.domain.ObjectTypeLayoutDomain;
import jp.co.ctc_g.eim.app.form.business.service.ObjectTypeLayoutService;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.service.ObjectService;
import jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader;

public class CreateLumpObjectHelper
{
	/** セッション **/
	private final EIMSession _sess;
	/** ドキュメントオブジェクトタイプ **/
	private final EIMObjectType _docObjType;
	/** 一時格納フォルダオブジェクトのオブジェクトID **/
	private final String _tmpFolderObjId;
	/** 登録ユーザID **/
	private final String _createUserId;
	/** プロパティ **/
	private final String _property;
	/** 有効期限（YYYY-MM-DD(日), MM-DD-YYYY(英) **/
	private final String _expDate;

	/** ヘルパー **/
	private final AppObjectConditionHelper _helper;
	/** セッションに含まれる作成者情報 **/
	private final EIMUser _createUser;


	/** ドキュメントオブジェクト関連の文字列 **/
	private static final String DOCUMENT_OBJ_TYPE = EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT");
	private static final String PROPERTY = EIMConfig.get("ATTR_NAME_DOCUMENT_PROP");
	private static final String HIGHER_WF_FOLDER = EIMConfig.get("ATTR_NAME_DOCUMENT_HIGHER_WF_FOLDER");
	private static final String EXPIRE_DATE = EIMConfig.get("ATTR_NAME_DOCUMENT_EFFECT_DATE");

	/** フォルダ／ワークスペースオブジェクト関連の文字列 **/
	private static final String FOLDER_OBJ_TYPE = EIMConfig.get("OBJECT_TYPE_NAME_FOLDER");
	private static final String DOCUMENT_CREATER = EIMConfig.get("ATTR_NAME_DOCUMENT_CREATE");
	private static final String LOW_FOLDER_SECURITY = EIMConfig.get("ATTR_NAME_WORKSPACE_LOW_FOLDER_SEC");

	/** その他 **/
	private static final String DOCUMENT_RELATION_TYPE = EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT");
	private static final String REGIST_PATH = EIMConfig.get("ATTR_NAME_REGIST_PATH");
	private static final String TMP_FOLDER_PATH = EIMConfig.get("TEMP");
	private static final String FORMAT_PUBLIC = EIMConfig.get("FORMAT_NAME_PUBLIC");

	/**
	 * コンストラクタ
	 * @param _s セッション
	 * @param _dType ドキュメントのオブジェクトタイプ
	 * @param _oId 一時格納フォルダオブジェクトのオブジェクトID
	 * @param _uId 登録ユーザID
	 * @param _p プロパティ
	 * @param _exp 有効期限
	 */
	public CreateLumpObjectHelper(
			EIMSession _s, EIMObjectType _dType, String _oId,
			String _uId, String _p, String _exp)
	{
		_sess = _s;
		_tmpFolderObjId = _oId;
		_docObjType = _dType;
		_createUserId = _uId;
		_property = _p;
		_expDate = _exp;

		_helper = new AppObjectConditionHelper(_sess);
		_createUser = (EIMUser)_sess.getAttribute("USER");
	}

	/**
	 * フォルダを登録する
	 * @param pathMap フルパスとフォルダオブジェクトのマップ
	 * @param path 作成するフォルダオブジェクトのパス
	 */
	public EIMObject createFolder(HashMap pathMap, String path) throws EIMException, Exception
	{
		//親オブジェクト取得
		String parentPath = getParentPath(path);
		EIMObject parentObj = getParentObject(pathMap, parentPath);

		//オブジェクトタイプは「一般フォルダ」固定
		EIMObjectType objType = ObjectUtils.getObjectTypeByName(_sess, FOLDER_OBJ_TYPE);

		/* 親オブジェクトのセキュリティのロールチェック */
		if (!SecurityUtils.authorized(_sess, parentObj,_sess.getUser(), EIMAccessRole.CREATE)) {
			// 作成権限がありません
			throw new EIMException(_sess, "EIM.ERROR.LOGIC.NOCREATEROLE");
		}

		/* ステータスチェック */
		// 上位WFフォルダのステータスを持っているため
		// 作成する直上のオブジェクトのステータスをチェック対象とする
		if (parentObj.getStatus() != null) {
			// 親オブジェクトのステータスが「編集中」以外の場合はエラー
			if (parentObj.getStatus().getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_EDITTING) {
				// 作成権限がありません
				throw new EIMException(_sess, "EIM.ERROR.LOGIC.NOCREATEROLE");
			}
		}

		/* フォルダ構成管理チェック */
		// 下位フォルダ管理セキュリティ取得
		long sec_id = AppObjectUtil.getIntAttr(_sess, parentObj, LOW_FOLDER_SECURITY, Integer.MIN_VALUE);
		if (sec_id != Integer.MIN_VALUE) {
			// 下位フォルダ管理セキュリティのロールチェック
			if (!AppSecurityUtils.authorizedLowFolderSecurity(_sess, parentObj, _sess.getUser(), EIMAccessRole.UPDATE)) {
				// 作成権限がありません
				throw new EIMException(_sess, "EIM.ERROR.LOGIC.NOCREATEROLE");
			}
		}

		//指定されたフォルダオブジェクトの作成
		EIMObject object = ObjectUtils.createObject(_sess, objType, getFolderName(path));
		if(object == null) {
			throw new EIMException(_sess, "EIM.ERROR.LOGIC.FAILMAKEOBJ");
		}

		/*
		 * リレーション作成
		 */
		EIMRelationType relType = RelationUtils.getRelationTypeByName(_sess, DOCUMENT_RELATION_TYPE);
		RelationUtils.createRelation(_sess, relType, parentObj, object, EIMConstant.DEPU_CHECK_NAME);
		AppObjectUtil.setPath(_sess, object, parentPath + "/");

		/*
		 * セキュリティ作成
		 */
		EIMSecurity sec = parentObj.getSecurity();
		if(sec != null) {
			SecurityUtils.setSecurity(_sess, object, sec);
		}

		// WF付フォルダ直下に作成する場合
		AppObjectConditionHelper helper = new AppObjectConditionHelper(_sess);
		if (helper.isTypeOfFolderWithWorkflow(parentObj))
		{
			// 属性「上位WFフォルダ」設定
			AppObjectUtil.setAttr(_sess, object, HIGHER_WF_FOLDER, parentObj.getId());
			// 上位WF付フォルダのステータスを設定(引継ぐ)
			WorkFlowUtils.updateObjectStatus(_sess, object, parentObj.getStatus());
		}
		else
		{
			// 親フォルダの「上位WFフォルダ」属性を取得
			long higherWfFolderId = AppObjectUtil.getIntAttr(_sess, parentObj, HIGHER_WF_FOLDER, Integer.MIN_VALUE);

			if (higherWfFolderId != Integer.MIN_VALUE)
			{
				// 属性「上位WFフォルダ」設定
				AppObjectUtil.setAttr(_sess, object, HIGHER_WF_FOLDER, higherWfFolderId);
				// 上位WFフォルダ取得
				EIMObject higherWfFolder = ObjectUtils.getObjectById(_sess, higherWfFolderId);
				// 上位WF付フォルダのステータスを設定(引継ぐ)
				WorkFlowUtils.updateObjectStatus(_sess, object, higherWfFolder.getStatus());
			}
		}

		// 下位管理セキュリティ設定
		if (sec_id != Integer.MIN_VALUE)
		{
			AppObjectUtil.setAttr(_sess, object, LOW_FOLDER_SECURITY, sec_id);
		}

		// マルチインスタンス暫定対応(2012.4.12)
		// セッションをコミットしないと下位フォルダにトリガが正しく機能しないため、
		// Java側で直接属性値を設定する
		EIMAttributeType attrTypeBelongWS = AttributeUtils.getAttributeTypeByName(_sess, EIMConfig.getValue("ATTR_NAME_DOCUMENT_BELONGING_WS"));
		// 属性タイプ「所属ワークスペース」が未設定の場合はなにもしない
		if(attrTypeBelongWS != null){
			long parentBelongWS = AppObjectUtil.getIntAttr(_sess, parentObj, EIMConfig.getValue("ATTR_NAME_DOCUMENT_BELONGING_WS"), Integer.MIN_VALUE);
			if(parentBelongWS != Integer.MIN_VALUE){
				AppObjectUtil.setAttr(_sess, object, EIMConfig.getValue("ATTR_NAME_DOCUMENT_BELONGING_WS"), parentBelongWS);
			}
		}

		//属性値の更新（リスト値表示色オブジェクトの更新も行う）
		//上位引継ぎ属性以外の属性は設定しないため、nullでOK
		AttributeUpdater attUpdater = new AttributeUpdater(object.getId(), object.getName(), null);
		AttributeUtil.updateAttribute(_sess, attUpdater);

		//Access
		AccessUtils.createAccess(_sess, object, "EIM.ACCESS.TYPE.INITIALREGIST");

		return object;
	}

	/**
	 * ドキュメントを登録する
	 * @param pathMap フルパスとフォルダオブジェクトのマップ
	 * @param fileName ドキュメント名
	 * @param path パス
	 * @return 登録したドキュメント
	 */
	public EIMObject createDocument(HashMap pathMap, String fileName, String path) throws Exception
	{
		EIMObject createObject = null;
		//親オブジェクト取得
		EIMObject parentObj = getParentObject(pathMap, getParentPath(path));
		//同名オブジェクト取得。同名オブジェクトが存在する場合、ドキュメントとドキュメントリンクの判定も行う。
		//ドキュメントとドキュメントリンクの両方が存在する場合は両方ともtrue。
		EIMObject sameNameObj = AppObjectUtil.getObjListByFullPass(_sess, path + fileName, DOCUMENT_OBJ_TYPE);
		boolean isDocLink = false;
		boolean isDocument = false;
		if(sameNameObj != null) {
			isDocLink = _helper.isDocumentLink(parentObj, sameNameObj);
			isDocument = _helper.isDocument(parentObj, sameNameObj);
		}

		if(sameNameObj == null || (isDocLink && isDocument == false)) {
			/* 同名のファイルが登録されていない場合、またはドキュメントリンクのみ存在している場合 */

			// 1.DBに登録する
			EIMObject object = createObject(parentObj, fileName, path);

			// 2.実ファイルをデフォルトフォーマットに移動する
			moveFile(object, parentObj, fileName, path);

			// アクセス履歴を作成
			AccessUtils.createAccess(_sess, object, "EIM.ACCESS.TYPE.INITIALREGIST");

			// SearchFramework 検索FW更新通知 対象：オブジェクト
			AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), "SEARCHFW_LUMP_UPLOAD_CREATE_DOCUMENT");

			createObject = object;
		}
		else {
			/* 同名のファイルが既に登録されている場合 */

			//同名ファイルと同名ドキュメントリンクが同時に存在している場合、確実にドキュメントを
			//取得できるよう、再度取得しなおす
			if(isDocument && isDocLink) {
				List rels = RelationUtils.getChildRelationListByRelType(_sess, parentObj, _helper.getRelationTypeOfDocument(),EIMAccessRole.READ);
				for(int ii = 0; ii < rels.size(); ii++) {
					EIMObject obj = ((EIMRelation)rels.get(ii)).getChild();
					if(sameNameObj.getName().equals(obj.getName())) {
						sameNameObj = obj;
						break;
					}
				}
			}

			//先に処理対象ドキュメントの同一バージョン内のオブジェクト全てに行ロックをかける
			List<EIMObject> objListInVersion = VersionUtils.getVersion(_sess, sameNameObj).getList();
			long[] objIds = new long[objListInVersion.size()];
			for(int i = 0; i < objListInVersion.size(); i++)
			{
				objIds[i] = objListInVersion.get(i).getId();
			}
			AppObjectUtil.lockObjectById(_sess, objIds);
			// 処理待ち中にEIMObjectの値が変更された可能性があるので再取得
			sameNameObj = ObjectUtils.getObjectById(_sess, sameNameObj.getId());

			// 1.チェックアウト
			EIMObject newObject = checkout(sameNameObj);

			// 2.チェックイン
			checkin(newObject, parentObj, fileName, path);

			// アクセス履歴を作成
			AccessUtils.createAccess(_sess, newObject, "EIM.ACCESS.TYPE.CHECKIN");

			// SearchFramework 検索FW更新通知 対象：新しいオブジェクト + 古い同名のオブジェクト
			AppUpdateNoticeUtils.updateNoticeInsert(newObject.getId(), "SEARCHFW_LUMP_UPLOAD_CREATE_DOCUMENT");
			AppUpdateNoticeUtils.updateNoticeInsert(sameNameObj.getId(), "SEARCHFW_LUMP_UPLOAD_OLD_DOCUMENT");

			createObject = newObject;
		}
		return createObject;
	}

	/**
	 * 親オブジェクトを取得する。
	 *
	 */
	public EIMObject getParentObjectByChildPath(HashMap pathMap, String childpath) throws EIMException, Exception
	{
		//親オブジェクト取得
		String parentPath = getParentPath(childpath);
		return getParentObject(pathMap, parentPath);
	}

	/**
	 * 親オブジェクトを取得する。
	 * 一括登録時に作成したフォルダは属性リストのサイズが0であるため、再度DBから取得しなおしマップに登録し直す。
	 * @param pathMap フルパスとフォルダオブジェクトのマップ
	 * @param path 親オブジェクトのパス
	 */
	private EIMObject getParentObject(HashMap pathMap, String path) throws EIMException, Exception
	{
		EIMObject parentObj = (EIMObject)pathMap.get(path);

		if(parentObj == null) {
			throw new EIMException(_sess, "EIM.ERROR.LOGIC.NOFOLDER");
		}
		else if(parentObj.getAttributeList().size() == 0) {
			//一括登録時に作成したフォルダは属性リストのサイズが0。
			//その場合、もう一度DBから取り直し再度マップに登録する。
			parentObj = ObjectUtils.getObjectById(_sess, parentObj.getId());
			if(parentObj == null) {
				throw new EIMException(_sess, "EIM.ERROR.LOGIC.NOFOLDER");
			}
			pathMap.put(path, parentObj);
		}

		return parentObj;
	}

	/**
	 * 親オブジェクト配下に指定ファイルのオブジェクトを登録する
	 * @param parentObj 親オブジェクト
	 * @param fileName ファイル名
	 * @param path パス
	 */
	private EIMObject createObject(EIMObject parentObj, String fileName, String path) throws EIMException, Exception
	{
		//ワークフロー付きフォルダ下の場合は、編集中以外は作成できない
		//ワークフロー付きフォルダ下の場合は、ワークフロー付きドキュメントは作成できない
		if(parentObj.getStatus() != null) {
			if(parentObj.getStatus().getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_EDITTING) {
				throw new EIMException(_sess, "EIM.ERROR.LOGIC.NOCREATEROLE");
			}
			if(WorkFlowUtils.getWorkFlowByType(_sess, _docObjType) != null) {
				throw new EIMException(_sess, "EIM.ERROR.LOGIC.CANTSET.UNDERWF.WFDOCUMENT");
			}
		}

		//アクセス権限チェック
		if(SecurityUtils.authorized(_sess, parentObj, _sess.getUser(),  EIMAccessRole.CREATE) != true) {
			throw new EIMException(_sess, "EIM.ERROR.LOGIC.NOCREATEROLE");
		}

		//指定されたドキュメントオブジェクトの作成
		EIMObject object = ObjectUtils.createObject(_sess, _docObjType, fileName);
		if(object == null) {
			throw new EIMException(_sess, "EIM.ERROR.LOGIC.FAILMAKEOBJ");
		}

		//親オブジェクトとのリレーションを作成
		EIMRelationType relType = RelationUtils.getRelationTypeByName(_sess, DOCUMENT_RELATION_TYPE);
		try{
			RelationUtils.createRelation(_sess, relType, parentObj, object,  EIMConstant.DEPU_CHECK_NAME_REV);
		}
		catch(EIMException eime) {
			throw new EIMException(_sess, "EIM.ERROR.LOGIC.FAILEDMAKEREL");
		}

		//パスを設定
		AppObjectUtil.setPath(_sess, object, path);

		//プロパティを設定
		EIMAttributeType attType = AttributeUtils.getAttributeTypeByName(_sess, PROPERTY);
		ObjectAttributeUtils.setAttribute(_sess, object, attType, _property);

		//作成者を設定
		if(_createUserId != null) {
			EIMUser createUser = UserUtils.getUserById(_sess, Long.parseLong(_createUserId));
			attType = AttributeUtils.getAttributeTypeByName(_sess, DOCUMENT_CREATER);
			ObjectAttributeUtils.setAttribute(_sess, object, attType, createUser.getId());
		}

		//有効期限を設定
		if(_expDate != null && _expDate.length() > 0) {
			Date expireDate = DateUtils.editExpirationDate(_sess,
					StringUtils.getDateFromString(_expDate, EIMResource.getMessage(_sess, "EIM.FORMAT.DATE")));
			attType = AttributeUtils.getAttributeTypeByName(_sess, EXPIRE_DATE);
			ObjectAttributeUtils.setAttribute(_sess, object, attType, expireDate);
		}

		// 連続データIDの取得
		String nextValue = AppObjectTypeUtil.getNextValue(object.getType().getId());
		if (nextValue != null) {
			AppObjectUtil.setAttrForce(_sess, object, "番号", nextValue);
		}

		//上位フォルダからの属性引継ぎ
		long[] parentLowAttrIds = AppObjectUtil.getIntAttrs(_sess, parentObj, _helper.getAttrNameOfToLowAttr());

		if (parentLowAttrIds != null) {
			//上位からの引継ぎ属性の設定内容に従い、parentObjから属性値をコピー
			//ただし、自身のタイプに該当属性が割り当てられているものに限る
			List parentLowAttrTypes = new ArrayList();
			{
				List parentLowAttrIdsInteger = new ArrayList(Arrays.asList(ArrayUtils.toObject(parentLowAttrIds)));
				List objectTypes = ObjectAttributeUtils.getAttributeTypeList(_sess, _docObjType);
				//引継ぎ対象を自身のタイプに該当属性が割り当てられているものにフィルタリング
SearchToLowAttr:for (Iterator i = parentLowAttrIdsInteger.iterator(); i.hasNext();) {
					long attrTypeIdOfParentLowAttrId = ((Long)i.next()).longValue();
					for (Iterator j = objectTypes.iterator(); j.hasNext();) {
						EIMAttributeType attrTypeObj = (EIMAttributeType)j.next();
						if (attrTypeObj.getId() == attrTypeIdOfParentLowAttrId) {
							parentLowAttrTypes.add(attrTypeObj);
							continue SearchToLowAttr;
						}
					}
					i.remove();//自身のタイプに無かったので対象から削除
				}
				parentLowAttrIds = ArrayUtils.toPrimitive((Long[])parentLowAttrIdsInteger.toArray(new Long[parentLowAttrIdsInteger.size()]));
			}
			//「上位からの引継ぎ」属性の値を設定
			ObjectAttributeUtils.setAttribute(_sess, object, _helper.getAttrTypeOfFromHighAttr(), TypeConvertUtils.convertToBuildTypeArray(parentLowAttrIds));

			//各属性値の引継ぎ
			for (Iterator i = parentLowAttrTypes.iterator(); i.hasNext();) {
				EIMAttribute attr = parentObj.getAttribute(((EIMAttributeType)i.next()).getDefName());
				if (attr != null) {
					AppObjectUtil.setAttr(_sess, object, attr);
				}
			}

			//リスト値表示色オブジェクトの引継ぎ
			DisplayColorUtil.inheritDisplayColor(_sess, object, parentLowAttrTypes, parentObj);
		}

		// OCR
		// ワークフロー設定オブジェクトを取得
		EIMWorkFlow workflow = WorkFlowUtils.getWorkFlowByType(_sess, object.getType());
		if(workflow != null){
			EIMObject wfSettingObj = AppObjectUtil.getObject(_sess, EIMConfig.get("OBJECT_TYPE_NAME_WFSETTING"), "" + workflow.getId());
			EIMAttribute defOcrAttribute = wfSettingObj.getAttribute(EIMConfig.get("ATTR_NAME_OCR_SETTING_EXISTENCE"));
			long defOcr = 0;
			if(defOcrAttribute != null){
				defOcr = defOcrAttribute.getInt();
			}

			// 登録するファイルの拡張子を取得
			String fileExt = StringUtils.nullToBlank(StringUtils.getFileExt(fileName));

			// 1:OCR設定有り、かつPDFの場合、OCR処理ステータスに0:処理待を設定する
			if(defOcr == 1 && fileExt != null && fileExt.equalsIgnoreCase(EIMConfig.get("PDF_EXT"))){
				AppOcrUtil.setOcrProcessingStatus(_sess, object, AppConstant.OCR_PROC_STATUS_PROCESS_WAIT);
			}

		}

		// マルチインスタンス暫定対応(2012.4.12)
		// セッションをコミットしないと下位ドキュメントにトリガが正しく機能しないため、
		// Java側で直接属性値を設定する
		EIMAttributeType attrTypeBelongWS = AttributeUtils.getAttributeTypeByName(_sess, EIMConfig.getValue("ATTR_NAME_DOCUMENT_BELONGING_WS"));
		// 属性タイプ「所属ワークスペース」が未設定の場合はなにもしない
		if(attrTypeBelongWS != null){
			long parentBelongWS = AppObjectUtil.getIntAttr(_sess, parentObj, EIMConfig.getValue("ATTR_NAME_DOCUMENT_BELONGING_WS"), Integer.MIN_VALUE);
			if(parentBelongWS != Integer.MIN_VALUE){
				AppObjectUtil.setAttr(_sess, object, EIMConfig.getValue("ATTR_NAME_DOCUMENT_BELONGING_WS"), parentBelongWS);
			}
		}

		//上位フォルダからのステータス引継ぎ
		if (parentObj.getStatus() != null) {
			WorkFlowUtils.updateObjectStatus(_sess, object, parentObj.getStatus());

			//「上位WFフォルダ」属性も登録
			EIMAttribute attrOfHigherWFFolder = parentObj.getAttribute(_helper.getAttrNameDocumentHigherWFFolder());
			if (attrOfHigherWFFolder == null) //WF付フォルダ直下
				ObjectAttributeUtils.setAttribute(_sess, object, _helper.getAttrTypeOfHigherWFFolder(), parentObj.getId());
			else //「WF付フォルダ下のフォルダ」の下
				AppObjectUtil.setAttr(_sess, object, attrOfHigherWFFolder);
		}

		//セキュリティを設定
		EIMSecurity sec = parentObj.getSecurity();
		if(sec != null) {
			SecurityUtils.setSecurity(_sess, object, sec);
		}

		return object;
	}

	/**
	 * 実ファイルをリネーム（[DB登録時のオブジェクトID].[拡張子]）して、デフォルトフォーマットに移動する
	 * @param object 新規作成オブジェクト
	 * @param parentObj 親オブジェクト
	 * @param fileName ファイル名
	 * @param path パス
	 */
	private void moveFile(EIMObject object, EIMObject parentObj, String fileName, String path) throws EIMException, Exception
	{
		EIMObject tmpFolderObject = ObjectUtils.getObjectById(_sess, Long.parseLong(_tmpFolderObjId));
		if(tmpFolderObject == null) {
			throw new EIMException(_sess, "EIM.ERROR.LOGIC.NODOCMENT");
		}

		EIMAttribute registAttribute = (EIMAttribute)tmpFolderObject.getAttribute(REGIST_PATH);
		String realPath = registAttribute.getString();
		String tmpFolderPath = TMP_FOLDER_PATH + tmpFolderObject.getId() + "/unzip";

		String tmpPath = getPath(path.concat(fileName), realPath, tmpFolderPath);
		File tmpFolderFile = new File(tmpPath);
		if(tmpFolderFile.exists() == false) {
			throw new Exception();
		}

		//フォーマット、ディレクトリの取得
		EIMFormat format = FileUtils.getDefaultFormat(_sess, _docObjType);
		if (format == null) {
			//デフォルトフォーマットが設定されていません。
			throw new EIMException(_sess, "EIM.ERROR.LOGIC.NODEFAULTFORMAT");
		}
		EIMDirectory dir = format.getDirectory();

		File renameFolderFile = getRenameFile(dir.getPath(), fileName, object);
		org.apache.commons.io.FileUtils.copyFile(tmpFolderFile, renameFolderFile);

		//チェックイン実行（DBに登録）
		FileUtils.checkin(_sess, object, format, fileName, renameFolderFile.length());//ここの第四引数はテーブルで論理的に保持しているのでどんな値でもよい

		// 作成したドキュメント自身にステータスが無く、かつ上位フォルダにもステータスが無い場合は、
		// WFなしドキュメントとして、即公開する
		if (object.getStatus() == null && parentObj.getStatus() == null) {
			// 公開ドキュメントとして登録
			EIMFormat publicDocumentFormat = FileUtils.getFormatByName(_sess, FORMAT_PUBLIC);
			EIMFile file = FileUtils.getFile(_sess, object, FileUtils.getDefaultFormat(_sess, object.getType()));
			File orgFile = new File(file.getDirectory().getPath() + FileUtils.getFileName(object, file));
			File dstFile = new File(publicDocumentFormat.getDirectory().getPath() + object.getId() + file.getExt());
			FileUtils.createSymbolicLink(orgFile, dstFile);
			FileUtils.checkin(_sess, object, publicDocumentFormat, fileName, file.getSize());
		}
	}

	/**
	 * チェックアウトを行う
	 * @param checkoutObj チェックアウトするオブジェクト
	 */
	private EIMObject checkout(EIMObject checkoutObj) throws EIMException, Exception
	{
		//権限チェック
		boolean enabled = true;
		do {
			//Check REVISION_UP Role
			if(	checkoutObj.getSecurity() != null &&
					SecurityUtils.authorized(_sess, checkoutObj,_sess.getUser(), EIMAccessRole.REVISION_UP) == false)
			{
					enabled = false;
					break;
			}
			//ワークフロー付きフォルダ下の場合は、チェックアウトできない
			if (_helper.isUnderFolderWithWorkflow(checkoutObj)) {
				enabled = false;
				break;
			}
		} while (false);
		if(enabled == false) {
			throw new EIMException(_sess, "EIM.ERROR.LOGIC.NOCHECKOUTROLE");
		}

		//Check Status (WFなしドキュメントもチェックアウトできる)
		if( checkoutObj.getStatus() != null &&
			checkoutObj.getStatus().getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_PUBLIC)
		{
			throw new EIMException(_sess, "EIM.ERROR.LOGIC.NOTPUBLIC", new Object[]{checkoutObj.getName()});
		}

		//Check Lock User
		EIMUser user = checkoutObj.getLockUser();
		if(user != null ||
			(checkoutObj.getStatus() == null // WFなしドキュメントの場合はチェックアウト可否判定をする
						&& AppLogicUtil.isCheckoutEnabledNoWFDoc(_sess, checkoutObj) == false))
		{
			throw new EIMException(_sess, "EIM.ERROR.LOGIC.CHECKOUTED", new Object[]{StringUtils.xmlEncode(checkoutObj.getName())});
		}

		//Lock
		EIMObject newObject = VersionUtils.revisionUp(_sess, checkoutObj);


		// オブジェクト（関連ドキュメント、添付ドキュメント）をリビジョンアップ元から引き継ぐ

		// カスタム属性(レイアウト)含むオブジェクトタイプを取得
		ObjectTypeLayoutService objectTypeLayoutService =
			(ObjectTypeLayoutService)ApplicationContextLoader.getApplicationContext().getBean("documentObjectTypeLayoutService");
		ObjectTypeLayoutDomain objTypeLayoutdomain =
			objectTypeLayoutService.getById(new Long(checkoutObj.getType().getId()));

		// 引継ぎ対象の属性タイプID
		Set<String> inheritanceAttributeTypeIdSet = new HashSet<String>();
		for (AttributeTypeLayoutDomain atttypeLayout : objTypeLayoutdomain.getAttributeLayoutList()) {
			if (!((DocumentAttributeTypeLayoutDomain)atttypeLayout).isInheritanceFlag()) {
				continue;
			}
			inheritanceAttributeTypeIdSet.add(String.valueOf(atttypeLayout.getId()));
		}

		// リビジョンアップ対象（引継ぎ元、先オブジェクト）取得
		ObjectService objectService =
				(ObjectService)ApplicationContextLoader.getApplicationContext().getBean("documentObjectService2");
		ObjectDomain orgObjectDomain = objectService.getById(checkoutObj.getId());
		ObjectDomain newObjectDomain = objectService.getById(newObject.getId());

		// 各属性タイプの内、引継ぎ対象を引継ぎ先に設定する
		List<AttributeDomain> attList = orgObjectDomain.getAttributeList();
		for (int i = 0; i < attList.size(); i++) {
			// Attribute
			AttributeDomain att = attList.get(i);

			// Attribute Type
			AttributeTypeDomain attType = att.getAttributeType();

			// 属性タイプのデータ型がObjectでない場合はコピー対象外
			if (attType.getValueType() != jp.co.ctc_g.eim.framework2.common.enumeration.ValueTypeEnum.OBJECT &&
					attType.getValueType() != jp.co.ctc_g.eim.framework2.common.enumeration.ValueTypeEnum.USER) {
				continue;
			}

			// 属性に設定されたオブジェクトのタイプをチェック
			if (attType.getValueType() == jp.co.ctc_g.eim.framework2.common.enumeration.ValueTypeEnum.OBJECT) {

				// オブジェクト型の場合

				// 引き継ぐ属性内のオブジェクトリスト
				List<ObjectDomain> attributeObjects = new ArrayList<ObjectDomain>();

				// 引継ぎ対象かチェック
				if (!inheritanceAttributeTypeIdSet.contains(String.valueOf(attType.getId()))) {
					continue;
				}

				if (att.getObjectList().get(0).getType().getDefinitionName().equals(
						EIMConfig.getValue("OBJECT_TYPE_NAME_FORM_ATTACH_FILE"))) {
					// 添付ファイルの場合

					// 添付ファイルをコピーして引き継ぐ
					int attSize = att.getObjectList().size();
					for (int j = 0; j < attSize; j++) {
						ObjectDomain orgAttachmentObject = att.getObjectList().get(j);
						ObjectDomain newAttachementObject = new ObjectDomain();

						newAttachementObject.setName(orgAttachmentObject.getName());
						newAttachementObject.setType(orgAttachmentObject.getType());

						// 添付ファイル属性を引き継ぐ
						ObjectService attachementObjectService =
								(ObjectService)ApplicationContextLoader.getApplicationContext().getBean("attachementObjectService");
						newAttachementObject = attachementObjectService.copy(orgAttachmentObject, newAttachementObject);
						attributeObjects.add(newAttachementObject);
					}
				}
				else {
					// 関連ドキュメントの場合

					// 関連ドキュメントのIDを引き継ぐ
					attributeObjects = att.getObjectList();
				}

				if (attType.isMultiple()) {
					objectService.setAttributeMultipleObject(newObjectDomain, attType, attributeObjects);
				} else {
					objectService.setAttributeSingleObject(newObjectDomain, attType, attributeObjects.get(0));
				}
			}
			else {
				// ユーザ型の場合

				// 関連元のユーザ型属性をそのまま引き継ぐ
				if (attType.isMultiple()) {
					objectService.setAttributeMultipleUser(newObjectDomain, attType, att.getUserList());
				} else {
					objectService.setAttributeSingleUser(newObjectDomain, attType, att.getUserList().get(0));
				}
			}

		}

		// WFありの場合、公開フォーマットのメタ情報と実ファイルを削除
		if (_helper.isTypeOfDocumentWithWorkflow(newObject)) {
			EIMFormat newObjPubFormat = FileUtils.getFormatByName(_sess, EIMConfig.get("FORMAT_NAME_PUBLIC"));
			EIMFile newObjPubMetaInfo = FileUtils.getFile(_sess, newObject, newObjPubFormat);
			FileUtils.deleteFile(_sess, newObject, newObjPubFormat);

			File newObjPubFile = new File(newObjPubMetaInfo.getDirectory().getPath() + FileUtils.getFileName(newObject, newObjPubMetaInfo));
			if (newObjPubFile != null) {
				newObjPubFile.delete();
			}
		}

		// 継承しない属性を削除
		AttributeUtil.deleteNonInheritAttribute(_sess, newObject);

		// 属性表示色を更新（オブジェクトをもう一度取り直す）
		newObject = ObjectUtils.getObjectById(_sess, newObject.getId());
		DisplayColorUtil.copyDisplayColor(_sess, newObject, checkoutObj);

		// OCR
		if(OptionConfData.getInstance().ocrFlg){
			if(AppOcrUtil.hasOcrSetting(_sess, newObject)){
				// OCR設定有無の有りの場合、OCR処理ステータスに0:処理待を設定する
				AppOcrUtil.setOcrProcessingStatus(_sess, newObject, AppConstant.OCR_PROC_STATUS_PROCESS_WAIT);
			}
		}

		//パス
		String sameNameObjPath = StringUtils.nullToBlank(AppObjectUtil.getPath(checkoutObj));

		// パス属性は先頭の一つのみをセットする(複数値の2つ目以降は継承元ドキュメントのリンク情報のため)
		AppObjectUtil.setPath(_sess, newObject, sameNameObjPath);

		//ドキュメントはレビジョンアップしても公開するまで最新にはしない
		checkoutObj = VersionUtils.setLatest(_sess, checkoutObj);

		//Access
		AccessUtils.createAccess(_sess, checkoutObj, "EIM.ACCESS.TYPE.LOCK");
		AccessUtils.createAccess(_sess, checkoutObj, "EIM.ACCESS.TYPE.REBISIONUP");

		return newObject;
	}

	/**
	 * チェックインを行う
	 * @param targetObj チェックイン対象オブジェクト
	 * @param parentObj 親オブジェクト
	 * @param fileName ドキュメント名
	 * @param path パス
	 */
	private void checkin(EIMObject targetObj, EIMObject parentObj, String fileName, String path) throws EIMException, Exception
	{
		//作成権限のチェック
		if(!SecurityUtils.authorized(_sess, targetObj, _sess.getUser(), EIMAccessRole.CHECKIN))
		{
			throw new EIMException(_sess, "EIM.ERROR.LOGIC.NOCHECKINROLE");
		}

		//ステータスのチェック
		//WFなしドキュメントでもチェックインは出来るものとする
		if(targetObj.getStatus() != null && targetObj.getStatus().getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_EDITTING)
		{
			throw new EIMException(_sess, "EIM.ERROR.LOGIC.NOTUPDATING");
		}

		//Version
		EIMVersion version = VersionUtils.getVersion(_sess, targetObj);

		//対象が最新のリビジョンかのチェック
		if (targetObj.getRevision() != version.getMaxRevision())
		{
			throw new EIMException(_sess, "EIM.ERROR.LOGIC.NOCHECKINROLE");
		}

		//Check Lock User
		if(targetObj.getRevision() > 0)
		{
			//Lock Object
			EIMObject lockObj = version.getObjectByRev(targetObj.getRevision() - 1);
			// オブジェクトがロックされている場合のみ、ユーザーの比較を行う
			// チェックアウト無しでチェックインする場合は、過去ドキュメントはロックされていない
			if(	lockObj != null &&
				lockObj.getLockUser() != null &&
				lockObj.getLockUser().getId() != _createUser.getId())
			{
				throw new EIMException(_sess, "EIM.ERROR.LOGIC.NOCHECKOUTUSER");
			}
		}

		/*
		 * チェックインオブジェクトが署名・暗号化中かどうかのチェックはここでは行わない。
		 *   理由1. チェックアウト直後にチェックインするため。
		 *           →「チェックアウト→署名・暗号化→チェックイン」することはない
		 *   理由2. チェックイン前にチェックアウトすることがないため。
		 *           →WFなしドキュメントはチェックアウトなしでチェックイン操作ができるが、
		 *            ZIPアップロード機能ではそれがない
		 */

		//プロパティを設定
		EIMAttributeType attType = AttributeUtils.getAttributeTypeByName(_sess, PROPERTY);
		ObjectAttributeUtils.setAttribute(_sess, targetObj, attType, _property);

		//作成者を設定
		if(_createUserId != null) {
			EIMUser createUser = UserUtils.getUserById(_sess, Long.parseLong(_createUserId));
			attType = AttributeUtils.getAttributeTypeByName(_sess, DOCUMENT_CREATER);
			ObjectAttributeUtils.setAttribute(_sess, targetObj, attType, createUser.getId());
		}

		//有効期限を設定
		if(_expDate != null && _expDate.length() > 0) {
			Date expireDate = DateUtils.editExpirationDate(_sess,
					StringUtils.getDateFromString(_expDate, EIMResource.getMessage(_sess, "EIM.FORMAT.DATE")));
			attType = AttributeUtils.getAttributeTypeByName(_sess, EXPIRE_DATE);
			ObjectAttributeUtils.setAttribute(_sess, targetObj, attType, expireDate);
		}

		//Format
		EIMFormat format = FileUtils.getDefaultFormat(_sess, _docObjType);

		//File Name
		// 2007/8/27 by.ik. 拡張子が変わる場合は変更前のファイルを削除
		EIMFile file = FileUtils.getFile(_sess, targetObj, format);

		//Directory
		EIMDirectory dir = file.getDirectory();

		String fileExt = StringUtils.nullToBlank(StringUtils.getFileExt(fileName));
		if (!file.getExt().equals(fileExt)) {
			File target = new File(dir.getPath() + FileUtils.getFileName(targetObj, file));
			target.delete();
		}

		moveFile(targetObj, parentObj, fileName, path);

		//WFなしドキュメントの場合、公開ドキュメントの置換え
		if (targetObj.getStatus() == null) {

			// 公開ドキュメントとして登録
			EIMFormat publicDocumentFormat = FileUtils.getFormatByName(_sess, EIMConfig.get("FORMAT_NAME_PUBLIC"));
			EIMFile baseFile = FileUtils.getFile(_sess, targetObj, FileUtils.getDefaultFormat(_sess, targetObj.getType()));

			File orgFile = new File(baseFile.getDirectory().getPath() + FileUtils.getFileName(targetObj, baseFile));
			File dstFile = new File(publicDocumentFormat.getDirectory().getPath() + targetObj.getId() + baseFile.getExt());
			FileUtils.createSymbolicLink(orgFile, dstFile);
			FileUtils.checkin(_sess, targetObj, publicDocumentFormat, fileName, baseFile.getSize());

			//Delete Relation For Old Revision
			if(targetObj.getRev() > 0)
			{
				//Set Latest
				targetObj = VersionUtils.setLatest(_sess, targetObj);

				//Old Revision
				EIMObject latestObj = version.getObjectByRev(targetObj.getRev() - 1);

				//Unlock (これを実行しないとステータスが「改訂中」のまま残る)
				ObjectUtils.unLock(_sess, latestObj);

				//Parent Relation
				List parentRelList = RelationUtils.getParentRelationListByRelType(_sess, latestObj, RelationUtils.getRelationTypeByName(_sess, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT")),EIMAccessRole.READ);

				//Delete Relation
				for(int i = 0; i < parentRelList.size(); i++)
				{
					EIMRelation relation = (EIMRelation)parentRelList.get(i);
					RelationUtils.deleteRelation(_sess, relation);
				}

				// 前リビジョンがロックされている（チェックアウトされているドキュメントにチェックインを行う）場合、
				// リンク自動更新処理を呼び出す。
				// チェックアウト無しでチェックインする場合は、リンク自動更新処理は呼び出さない。
				if(latestObj.getLockUser() != null) {
					// リンクリレーションの子オブジェクト（ドキュメント）を最新版のドキュメントに設定する
					UpdateObjectLinkUtil.actUpdateObjectLinkByDocument(_sess, targetObj);
				}
			}
		}
	}

	/**
	 * ファイル名を[オブジェクトID].[拡張子]に変更する
	 * @param fullPath フォーマットから取得したパス
	 * @param fileName ファイル名
	 * @param object ドキュメントオブジェクト
	 */
	private File getRenameFile(String path, String fileName, EIMObject object) throws Exception
	{
		String renamePath = new String(path);

		//改名する
		String fileExt = StringUtils.nullToBlank(StringUtils.getFileExt(fileName));
		renamePath = renamePath + String.valueOf(object.getId()) + fileExt;

		return new File(renamePath);
	}

	/**
	 * 一時格納用パス文字列を取得
	 * @param path 元のパス
	 * @param realFolderpath EIM上のパス
	 * @param tmpFolderPath 一時格納フォルダのパス
	 */
	private String getPath(String path, String realFolderpath, String tmpFolderPath)
	{
/*		String ret = new String(path);

		//pathから一時格納フォルダ文字列を削除し、実際のパスを追加する
		ret = tmpFolderPath + "/" + ret.replaceFirst(realFolderpath, "");

		//"//"の部分を"/"に修正
		ret = ret.replaceAll("//", "/");

		return ret;
*/
		int PathLength = realFolderpath.length();

		//PathLengthから1文字引く
		int PathLengths = PathLength -1;

		//指定されたインデックス番号の位置から終端までの文字列を返す
		String finishPath = path.substring(PathLengths);

		//指定された文字列をこの文字列の最後に連結する。
		String ret = tmpFolderPath.concat(finishPath);

		//"//"の部分を"/"に修正
		ret = ret.replaceAll("//", "/");

		return ret;
	}

	/**
	 * 親フォルダのパスを取得する（最後の"/"はなし）
	 * @path パス
	 */
	private String getParentPath(String path)
	{
		return path.substring(0, path.lastIndexOf("/"));
	}

	/**
	 * フォルダ名を取得する
	 * @param path パス
	 */
	private String getFolderName(String path)
	{
		String[] array = path.split("/");
		return array[array.length - 1];
	}
}
