package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.util.List;
import java.util.Map;

import common.util.AppConstant;
import common.util.AppObjectConditionHelper;
import common.util.AppObjectUtil;
import common.util.AppSecurityUtils;
import common.util.AppUpdateNoticeUtils;
import eim.bo.EIMAccessRole;
import eim.bo.EIMAccessRoleType;
import eim.bo.EIMAttribute;
import eim.bo.EIMAttributeType;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMSearchConditionCompare;
import eim.bo.EIMSearchConditionGroup;
import eim.bo.EIMSearchConditionIn;
import eim.bo.EIMSearchConditionLike;
import eim.bo.EIMSearchLimitCountCondition;
import eim.bo.EIMSearchSelectEIMObject;
import eim.net.EIMSession;
import eim.util.AccessUtils;
import eim.util.AttributeUtils;
import eim.util.EIMConfig;
import eim.util.EIMConstant;
import eim.util.ObjectUtils;
import eim.util.OperationHistoryUtils;
import eim.util.SearchUtils;
import eim.util.SecurityUtils;
import eim.util.StringUtils;
import jp.co.ctc_g.eim.framework.business.dao.StatusDao;
import jp.co.ctc_g.eim.framework.business.domain.BaseEventTypeExecDomain;
import jp.co.ctc_g.eim.framework.business.domain.StatusTypeKindDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.StatusDomain;
import jp.co.ctc_g.eim.framework.business.service.impl.BaseEventTypePlugInImpl;
import jp.co.ctc_g.eim.framework.common.exception.EIMAppException;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * ベースイベントタイプ「取戻し」
 *
 */
public class BaseEvtTakeBackRequestPlugInImpl extends BaseEventTypePlugInImpl {
	
	protected StatusDao statusDao = null;
	
	private final static String PROC_FAILURE = "procFailure";
	
	private static String publicProcFailAttrName = null;
	
	
	/**
	 * 実行権限を判定します。 
	 * 
	 * @see jp.co.ctc_g.eim.framework.business.service.impl.BaseEventTypePlugInImpl#enabled(jp.co.ctc_g.eim.framework.business.domain.BaseEventTypeExecDomain)
	 */
	public boolean enabled(BaseEventTypeExecDomain baseEventTypeExecDomain) throws Exception  {
		
		EIMSession sess = EIMThreadContext.getEIMSession();
		ObjectDomain objDomain = baseEventTypeExecDomain.getObject();
		if(objDomain == null) {
			throw new EIMAppException(sess, "EIM.ERROR.LOGIC.NOTEXIST.DOCFOL");
		}
		
		//パラメータを取得
		Map<String, Object> paramMap = baseEventTypeExecDomain.getParamMap();
		String processType = (String)paramMap.get(AppConstant.PARAM_KEY_REFER_TO_APPROVAL);
		
		//ST遷移予測で呼ばれた場合は無条件でtrue
		if(processType != null && processType.equals(AppConstant.PARAM_VALUE_REFER_TO_APPROVAL)){
			return true;
		}
		
		//オブジェクトを取得
		EIMObject object = objDomain.createEIMObject();
		
		// 実行権限チェック
		if(objDomain.getSecurity() != null) 
		{
			//ベースイベントタイプに設定されたアクセス権限を取得
			String aclKey = baseEventTypeExecDomain.getBaseEventType().getAclKey();
			EIMAccessRoleType acrType = SecurityUtils.getAccessRoleTypeByName(sess, aclKey);
			
			// システム管理権限を保有するかを取得
			Boolean isSystemSecurityAuth = AppSecurityUtils.authorizedBySystemSecurity(sess, EIMAccessRole.STATUS_UP);
			
			if(!isSystemSecurityAuth && !SecurityUtils.authorized(sess, object, sess.getUser(), acrType.getId())) {
				
				throw new EIMAppException(sess, "EIM.ERROR.LOGIC.NOTAKEBACK");
			}
		}
		
		
		// ステータスチェック（「公開処理中」ステータスかどうかのチェック）
		StatusDomain statusDomain = statusDao.getById(baseEventTypeExecDomain.getObject().getStatus().getId());
		StatusTypeKindDomain stTypeKindDomain = statusDomain.getStatusType().getStatusTypeKind();
		if (stTypeKindDomain.getId() != AppConstant.STATUS_TYPE_KIND_ID_PROCESSING_PUBLIC) 
		{
			throw new EIMAppException(sess, "EIM.ERROR.LOGIC.NOTPROCESSINGPUBLIC", new Object[]{objDomain.getName()});
		}

		
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
		EIMObjectType type = object.getType();
		
		if(helper.isTypeOfDocument(type)) 
		{
			/*
			 * 【ドキュメントの場合】
			 * 
			 * 「公開処理失敗」属性が0の場合、取戻しできない旨のエラーメッセージを表示
			 */
			if(AppObjectUtil.isProcFailDocument(object) == false)
			{
				throw new EIMAppException(sess, "EIM.ERROR.LOGIC.NOTPUBLICFAILED.DOC");
			}
		}
		else if(helper.isTypeOfFolder(type))
		{
			/*
			 * 【フォルダの場合】
			 * 
			 * 1. 以下の条件に合致するオブジェクトを取得する
			 *     ・「パス」属性が2のフルパスと前方一致する（フォルダ配下の全オブジェクト）
			 *     ・ドキュメントタイプ
			 *     ・「公開処理失敗」属性が1のもの（処理に失敗しているもの）
			 * 2. 取得したオブジェクトの中に「公開処理失敗」属性が1のオブジェクトが
			 *    2-1. 無い場合
			 *           →取戻しできない旨のエラーメッセージを表示
			 *    2-2. ある場合
			 *           →属性が1のオブジェクトをドメインに格納する
			 */
			List<EIMObject> publicProcessingFailureList = getPublicProcFailList(sess, object);
			if(publicProcessingFailureList.size() == 0) {
				throw new EIMAppException(sess, "EIM.ERROR.LOGIC.NOTINCLUDE.PUBLICFAILED.DOC");
			}
			
			paramMap.put(PROC_FAILURE, publicProcessingFailureList);
		}
		else
		{
			/*
			 * 【ドキュメントやフォルダではない場合】
			 * 
			 * 既にチェックを行っているため予期せぬエラーとして処理する
			 */
			throw new EIMAppException(sess, "EIM.ERROR.SYSTEMERROR");
		}
		
		return true;
	}
	
	/**
	 * ベースイベントタイプアクションを実行します。 
	 * 
	 * @see jp.co.ctc_g.eim.framework.business.service.impl.BaseEventTypePlugInImpl#doAction(jp.co.ctc_g.eim.framework.business.domain.BaseEventTypeExecDomain)
	 */
	public void doAction(BaseEventTypeExecDomain baseEventTypeExecDomain) throws Exception {
		
		EIMSession sess = EIMThreadContext.getEIMSession();
		EIMObject object = baseEventTypeExecDomain.getObject().createEIMObject();
		
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
		EIMObjectType type = object.getType();
		if(helper.isTypeOfDocument(type)) 
		{
			/*
			 * 【ドキュメントの場合】　 ※ステータスは「編集中」に戻し済み
			 * 
			 * 「公開処理失敗」属性を削除する
			 */
			if(AppObjectUtil.isProcFailDocument(object) == false) {
				throw new EIMAppException(sess, "EIM.ERROR.LOGIC.NOTPUBLICFAILED.DOC");
			}
			// 「公開処理失敗」属性の削除
			AppObjectUtil.deleteAttribute(sess, object, getProcFailAttr());
			// 「PDF変換処理実行日時」属性の削除
			AppObjectUtil.deleteAttribute(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_PDF_CONV_EXEC_DATE"));
		}
		else if(helper.isTypeOfFolder(type))
		{
			/*
			 * 【フォルダの場合】　 ※ステータスは「編集中」に戻し済み
			 * 
			 * 1. 対象フォルダの「公開処理失敗」属性を削除する
			 * 2. 格納していたオブジェクトIDリストから各々のオブジェクトを取得
			 * 3. 「公開処理失敗」属性が1のオブジェクトに対して、「公開処理失敗」属性を削除する
			 */
			if(AppObjectUtil.isProcFailDocument(object)) {
				// 「公開処理失敗」属性の削除
				AppObjectUtil.deleteAttribute(sess, object, getProcFailAttr());
			}
			List<EIMObject> publicProcessingFailureList = (List<EIMObject>)baseEventTypeExecDomain.getParamMap().get(PROC_FAILURE);
			for(EIMObject failureObj : publicProcessingFailureList)
			{
				if(AppObjectUtil.isProcFailDocument(failureObj)) {
					// 「公開処理失敗」属性の削除
					AppObjectUtil.deleteAttribute(sess, failureObj, getProcFailAttr());
					// 「PDF変換処理実行日時」属性の削除
					AppObjectUtil.deleteAttribute(sess, failureObj, EIMConfig.get("ATTR_NAME_DOCUMENT_PDF_CONV_EXEC_DATE"));
				}
			}
		}
		
		// 受信確認オブジェクトを削除する
		AppObjectUtil.deleteReceiveObject(sess, object);
		
		// SearchFramework 検索FW更新通知 対象：WF付きフォルダ、ドキュメントと配下のフォルダ、ドキュメント
		AppUpdateNoticeUtils.updateNoticeInsertWFObject(sess, object,
				"SEARCHFW_BASEEVT_TAKEBACK_DOCUMENT", "SEARCHFW_BASEEVT_TAKEBACK_FOLDER", 
				"SEARCHFW_BASEEVT_TAKEBACK_CHILD_DOCUMENT", "SEARCHFW_BASEEVT_TAKEBACK_CHILD_FOLDER");
		
		/*--- 共通処理 ---*/
		//アクセス履歴作成
		AccessUtils.createAccess(sess, object, "EIM.ACCESS.TYPE.APPROVALTAKEBACK");
		
		// 操作履歴
		OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.APPROVE_TAKEBACK, 
				EIMConstant.TARGET_UPDATE, EIMConstant.OBJECT, object,
				null, null, null, AppObjectUtil.getPath(object));
	}

	/**
	 * 「公開処理失敗」属性名称を取得
	 */
	private String getProcFailAttr() throws Exception
	{
		if(publicProcFailAttrName == null) {
			return EIMConfig.get("ATTR_NAME_DOCUMENT_PUB_PROC_FAIL");
		}
		return publicProcFailAttrName;
	}
	
	/**
	 * 指定オブジェクトの配下のドキュメントを取得する（直下だけではなく再帰的に全て）
	 */
	private List<EIMObject> getPublicProcFailList(EIMSession sess, EIMObject object) throws Exception
	{
		EIMSearchSelectEIMObject.SearchConditionBuildHelper h = new EIMSearchSelectEIMObject.SearchConditionBuildHelper();
		EIMSearchConditionGroup conds = h.group(h.opAnd());
		
		/*
		 * ドキュメントタイプを指定
		 */
		EIMObjectType docObjType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT"));
		if(docObjType == null) {
			//ドキュメントタイプが取得できない場合は予期せぬエラーとして処理する
			throw new EIMAppException(sess, "EIM.ERROR.SYSTEMERROR");
		}
		EIMSearchConditionIn objTypeCond = h.eqObjTypeWithSubClasses(h.opAnd(), docObjType.getId(), sess);
		objTypeCond.setHighPriority();
		conds.addCondition(objTypeCond);
		
		/*
		 * パスを指定（前方一致）
		 */
		EIMAttribute pathAttr = object.getAttribute(EIMConfig.get("ATTR_NAME_FOLDER_PASS"));
		if(pathAttr == null || pathAttr.getStrings() == null || pathAttr.getStrings().length == 0) {
			//パス属性がない場合は予期せぬエラーとして処理する
			throw new EIMAppException(sess, "EIM.ERROR.SYSTEMERROR");
		}
		String searchPath = pathAttr.getStrings()[0] + object.getName();
		
		EIMAttributeType pathType = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_PASS"));
		EIMSearchConditionLike pathCond = h.like(h.opAnd(), pathType, h.opLike(), StringUtils.escapeUserWildcardChars(searchPath) + "*");
		pathCond.setHighPriority();
		conds.addCondition(pathCond);

		/*
		 * 公開処理失敗属性=1を指定
		 */
		EIMAttributeType procFailType = AttributeUtils.getAttributeTypeByName(sess, getProcFailAttr());
		if(procFailType == null) {
			//公開処理失敗タイプが取得できない場合は予期せぬエラーとして処理する
			throw new EIMAppException(sess, "EIM.ERROR.SYSTEMERROR");
		}
		EIMSearchConditionCompare attrTypeCond = h.eq(h.opAnd(), procFailType, 1);
		attrTypeCond.setHighPriority();
		conds.addCondition(attrTypeCond);
		

		/*
		 * 検索する
		 */
		EIMSearchSelectEIMObject selectTarget = new EIMSearchSelectEIMObject();
		selectTarget.setCondition(conds);
		return SearchUtils.searchObjects(sess, selectTarget, new EIMSearchLimitCountCondition(100000, false));
	}
	
	/**
	 * @return the statusDao
	 */
	public StatusDao getStatusDao() {
		return statusDao;
	}

	/**
	 * @param statusDao the statusDao to set
	 */
	public void setStatusDao(StatusDao statusDao) {
		this.statusDao = statusDao;
	}
}
