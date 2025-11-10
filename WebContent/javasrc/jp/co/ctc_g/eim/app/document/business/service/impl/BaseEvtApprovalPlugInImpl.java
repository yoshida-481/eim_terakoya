package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import common.util.AppConstant;
import common.util.AppObjectUtil;
import common.util.AppUpdateNoticeUtils;
import eim.bo.EIMAccessRoleType;
import eim.bo.EIMAttribute;
import eim.bo.EIMAttributeType;
import eim.bo.EIMEvent;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.net.EIMSession;
import eim.util.AccessUtils;
import eim.util.AttributeUtils;
import eim.util.EIMConfig;
import eim.util.EIMConstant;
import eim.util.EventAttributeUtils;
import eim.util.ObjectAttributeUtils;
import eim.util.ObjectUtils;
import eim.util.OperationHistoryUtils;
import eim.util.SecurityUtils;
import eim.util.TypeConvertUtils;
import jp.co.ctc_g.eim.app.document.business.domain.ApprovalReqInfoDomain;
import jp.co.ctc_g.eim.app.document.common.util.AppWorkFlowUtil;
import jp.co.ctc_g.eim.framework.business.dao.AssignDao;
import jp.co.ctc_g.eim.framework.business.dao.AssignPlanDao;
import jp.co.ctc_g.eim.framework.business.dao.StatusDao;
import jp.co.ctc_g.eim.framework.business.domain.AssignDomain;
import jp.co.ctc_g.eim.framework.business.domain.AssignPlanDomain;
import jp.co.ctc_g.eim.framework.business.domain.BaseEventTypeExecDomain;
import jp.co.ctc_g.eim.framework.business.domain.StatusTypeKindDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.StatusDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.StatusTypeDomain;
import jp.co.ctc_g.eim.framework.business.service.impl.BaseEventTypePlugInImpl;
import jp.co.ctc_g.eim.framework.common.exception.EIMAppException;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * ベースイベントタイプ「承認」
 *
 */
public class BaseEvtApprovalPlugInImpl extends BaseEventTypePlugInImpl {

	protected StatusDao statusDao = null;
	protected AssignPlanDao assignPlanDao = null;
	protected AssignDao assignDao = null;

	/**
	 * 実行権限を判定します。
	 *
	 * @see jp.co.ctc_g.eim.framework.business.service.impl.BaseEventTypePlugInImpl#enabled(jp.co.ctc_g.eim.framework.business.domain.BaseEventTypeExecDomain)
	 */
	public boolean enabled(BaseEventTypeExecDomain baseEventTypeExecDomain) throws Exception  {

		EIMSession sess = EIMThreadContext.getEIMSession();

		//パラメータを取得
		Map<String, Object> paramMap = baseEventTypeExecDomain.getParamMap();
		String processType = (String)paramMap.get(AppConstant.PARAM_KEY_REFER_TO_APPROVAL);
		String immediatePublicStr = (String)paramMap.get(AppConstant.PARAM_KEY_IMMEDIATE_PUBLIC_NAME);

		if( processType != null && processType.equals(AppConstant.PARAM_VALUE_REFER_TO_CANCEL_APPROVAL) ){
			return true;
		}

		ObjectDomain objDomain = baseEventTypeExecDomain.getObject();
		//オブジェクトを取得
		EIMObject object = objDomain.createEIMObject();
		if(object == null) {
			throw new EIMAppException(sess, "EIM.ERROR.LOGIC.NOTEXIST.DOCFOL");
		}

		//オブジェクト更新権チェック
		if(objDomain.getSecurity() != null) {

			//ベースイベントタイプに設定されたアクセス権限を取得
			String aclKey = baseEventTypeExecDomain.getBaseEventType().getAclKey();
			EIMAccessRoleType acrType = SecurityUtils.getAccessRoleTypeByName(sess, aclKey);

			if(SecurityUtils.authorized(sess, object, sess.getUser(), acrType.getId()) != true) {

				return false;
			}
		}

		//ロック状態チェック
		if (objDomain.getLUser() != null || objDomain.getLDate() != null) {
			throw new EIMAppException(sess, "EIM.ERROR.LOGIC.OBJECT.LOCKED", new Object[]{objDomain.getName()});
		}
		
		// ドキュメントが公開処理失敗の場合、承認できない旨のエラーメッセージを表示
		if(AppObjectUtil.isProcFailDocument(object))
		{
			throw new EIMAppException(sess, "EIM.ERROR.LOGIC.PUBLICFAILED.DOC.FOR.APPROVE");
		}

		//処理分岐
		boolean rtnFlag = false;
		if( immediatePublicStr != null && immediatePublicStr.equals("true") )
		{
			// 承認不要WFの公開
			rtnFlag=enabledCheckImmediatePublic(sess, objDomain, baseEventTypeExecDomain, processType);
		}
		else
		{
			// 通常の承認
			rtnFlag=enabledCheckApprove(sess, objDomain, baseEventTypeExecDomain, processType);
		}

		return rtnFlag;

	}

	/**
	 * ベースイベントタイプアクションを実行します。
	 *
	 * @see jp.co.ctc_g.eim.framework.business.service.impl.BaseEventTypePlugInImpl#doAction(jp.co.ctc_g.eim.framework.business.domain.BaseEventTypeExecDomain)
	 */
	public void doAction(BaseEventTypeExecDomain baseEventTypeExecDomain) throws Exception {

		EIMSession sess = EIMThreadContext.getEIMSession();
		EIMObject object = baseEventTypeExecDomain.getObject().createEIMObject();

		// パラメータを取得
		ApprovalReqInfoDomain approvalReqInfoDomain = new ApprovalReqInfoDomain(baseEventTypeExecDomain);

		// 承認不要WFの公開処理
		if(approvalReqInfoDomain.getImmediatePublicFlag())
		{
			// 承認依頼時のメール、PDF関連処理
			AppWorkFlowUtil.approvalRequestMailPDFProcess(sess, object, approvalReqInfoDomain, baseEventTypeExecDomain);
		}

		// 「メール通知」オブジェクトを取得
		EIMObjectType objTypeMailNotify = ObjectUtils.getObjectTypeByName(sess, EIMConfig.getValue("OBJECT_TYPE_NAME_MAIL_NOTIFY"));
		EIMObject mailNotifyObj = ObjectUtils.getObjectByTypeAndName(sess, objTypeMailNotify, String.valueOf(object.getId()));

		// 「承認依頼通知タイミング」属性を更新
		if(approvalReqInfoDomain.getTiming() != null && !approvalReqInfoDomain.getTiming().equals("")){
			AppObjectUtil.setAttr(sess, mailNotifyObj, EIMConfig.getValue("ATTR_NAME_MAIL_NOTIFY_REQUEST_NOTIFY"), Long.parseLong(approvalReqInfoDomain.getTiming()));
		}

		// 「公開通知タイミング」属性を更新
		if(approvalReqInfoDomain.getSendNotifyMailTiming() != null && !approvalReqInfoDomain.getSendNotifyMailTiming().equals("")){
			AppObjectUtil.setAttr(sess, mailNotifyObj, EIMConfig.getValue("ATTR_NAME_MAIL_NOTIFY_PUBLIC_TIMING"), Long.parseLong(approvalReqInfoDomain.getSendNotifyMailTiming()));
		}

		// 承認不要WFの公開処理用
		if(approvalReqInfoDomain.getImmediatePublicFlag())
		{
			// この処理はガード条件アクションにて実行されるが、
			// 承認不要WFの場合はPDF署名オブジェクトが存在しないまま実行されるため
			// 処理ステータスが変更されない。PDF署名ステータス変更のための処理。
			EIMObject pdfSignObj = AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_SIGNPDF"), String.valueOf(object.getId()));
			if( pdfSignObj != null ) {
				AppObjectUtil.setAttr(sess, pdfSignObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_STATUS"), 1);
			}
		}

		// イベント属性「コメント」を設定
		EIMEvent event = baseEventTypeExecDomain.getEvent().createEIMEvent();
		EIMAttributeType attTypeComment = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.getValue("EVENT_ATTR_NAME_COMMENT"));
		EventAttributeUtils.setAttribute(sess, event, attTypeComment, approvalReqInfoDomain.getComment());

		// イベント属性「承認依頼通知タイミング」を設定
		EIMAttributeType attTypeApproveTiming = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.getValue("ATTR_NAME_MAIL_NOTIFY_REQUEST_NOTIFY"));
		if(attTypeApproveTiming != null){
			EventAttributeUtils.setAttribute(sess, event, attTypeApproveTiming, Long.parseLong((approvalReqInfoDomain.getTiming())));
		}

		// イベント属性「公開通知タイミング」を設定
		if(event.getEventType().getBaseEventType() == AppConstant.BASE_EVENT_TYPE_ID_REQ_APPROVE){
			EIMAttributeType attTypePublicTiming = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.getValue("ATTR_NAME_MAIL_NOTIFY_PUBLIC_TIMING"));
			if(attTypePublicTiming != null && approvalReqInfoDomain.getSendNotifyMailTiming() != null && !approvalReqInfoDomain.getSendNotifyMailTiming().equals("")){
				EventAttributeUtils.setAttribute(sess, event, attTypePublicTiming, Long.parseLong((approvalReqInfoDomain.getSendNotifyMailTiming())));
			}
		}

		// イベント属性「公開通知コメントログ」を設定
		EIMAttributeType attTypePublicCommentLog = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.getValue("ATTR_NAME_PUBLIC_COMMENT_LOG"));
		if(attTypePublicCommentLog != null){
			EventAttributeUtils.setAttribute(sess, event, attTypePublicCommentLog, approvalReqInfoDomain.getPublicComment());
		}

		// 即承認WF以外、かつ公開通知タイミングが送信された場合
		if( !approvalReqInfoDomain.getImmediatePublicFlag() &&
				approvalReqInfoDomain.getSendNotifyMailTiming() != null && !approvalReqInfoDomain.getSendNotifyMailTiming().equals("")){

			List<String> publisherList = approvalReqInfoDomain.getPublisherList();
			String[] publisherStrs = publisherList.toArray(new String[publisherList.size()]);
			// 公開通知先変更有無のチェック
			String[] srcPublisherStrs = AppObjectUtil.getStrAttrs(sess, mailNotifyObj, EIMConfig.get("ATTR_NAME_MAIL_NOTIFY_PUBLIC_NOTIFY_TO"));
			if( AppWorkFlowUtil.checkPublisherChange(srcPublisherStrs, publisherStrs)){
				// アクセス履歴作成
				AccessUtils.createAccess(sess, object, "EIM.ACCESS.TYPE.CHANGE.PUBLISHER");
				// 操作履歴作成
				OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, AppConstant.CHENGE_PUBLISHER,
						EIMConstant.TARGET_UPDATE, EIMConstant.OBJECT, object,
						null, null, null, AppObjectUtil.getPath(object));
			}
			// メール通知オブジェクトの「公開通知先」属性を設定
			AppObjectUtil.setAttr(sess, mailNotifyObj, EIMConfig.getValue("ATTR_NAME_MAIL_NOTIFY_PUBLIC_NOTIFY_TO"), publisherStrs);

			// メール通知オブジェクトの「公開通知コメント」属性を設定
			AppObjectUtil.setAttr(sess, mailNotifyObj, EIMConfig.getValue("ATTR_NAME_PUBLIC_COMMENT"), approvalReqInfoDomain.getPublicComment());

		}

		// 文書属性「スキップステータスID」を設定
		EIMAttributeType attSkipStatusId = AttributeUtils.getAttributeTypeByName(sess, "スキップステータスタイプID");
		Map<String, Object> paramMap = baseEventTypeExecDomain.getParamMap();
		String skipStatusIdStr = (String)paramMap.get("skipStatusTypeId");

		if(skipStatusIdStr != null && skipStatusIdStr.length() > 0){
			String[] stringValueList = skipStatusIdStr.split(",");
			List<Long> intValueList = new ArrayList<Long>();

			//以前にスキップ設定がされているステータスタイプ情報があれば取得
			EIMAttribute skipStatusTypeIdAttr = object.getAttribute("スキップステータスタイプID");
			if(skipStatusTypeIdAttr != null){
				long[] skipStatusTypeIdList = TypeConvertUtils.convertToLongArray(skipStatusTypeIdAttr.getInts());
				if(skipStatusTypeIdList.length > 0){
					for(long skipStatusTypeId : skipStatusTypeIdList){
						intValueList.add(skipStatusTypeId);
					}
				}
			}
			for(int i = 0; i < stringValueList.length; i++){
				String[] tokensColon = stringValueList[i].split(":");
				if(tokensColon[1].equals("true")){
					intValueList.add(Long.parseLong(tokensColon[0]));
				}
				else if(tokensColon[1].equals("false")){
					if(intValueList.contains(Long.parseLong(tokensColon[0]))){
						intValueList.remove(intValueList.indexOf(Long.parseLong(tokensColon[0])));
					}
				}
			}
			long[] intValueArray = intValueList.stream().mapToLong(i->i).toArray();
			ObjectAttributeUtils.setAttribute(sess, object, attSkipStatusId, TypeConvertUtils.convertToBuildTypeArray(intValueArray));
		}

		// SearchFramework 検索FW更新通知 対象：WF付きフォルダ、ドキュメントと配下のフォルダ、ドキュメント
		AppUpdateNoticeUtils.updateNoticeInsertWFObject(sess, object,
				"SEARCHFW_BASEEVT_APPROVAL_DOCUMENT", "SEARCHFW_BASEEVT_APPROVAL_FOLDER",
				"SEARCHFW_BASEEVT_APPROVAL_CHILD_DOCUMENT", "SEARCHFW_BASEEVT_APPROVAL_CHILD_FOLDER");

		//アクセス履歴作成
		AccessUtils.createAccess(sess, object, "EIM.ACCESS.TYPE.APPROVAL");

		//ヒストリ作成
		OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.APPROVE,
				EIMConstant.TARGET_APPROVE, EIMConstant.OBJECT, object,
				null, null, null, AppObjectUtil.getPath(object));
	}

	/**
	 * 承認不要WFでの公開機能のチェックを実行します。
	 */
	private boolean enabledCheckImmediatePublic(EIMSession sess, ObjectDomain objDomain,
				BaseEventTypeExecDomain baseEventTypeExecDomain, String processType) throws Exception {

		StatusTypeKindDomain stTypeKindDomain = baseEventTypeExecDomain.getObject().getStatus().getStatusType().getStatusTypeKind();
		if (stTypeKindDomain.getId() != AppConstant.STATUS_TYPE_KIND_ID_EDITTING)
		{
			throw new EIMAppException(sess, "EIM.ERROR.LOGIC.NOTUPDATING", new Object[]{objDomain.getName()});
		}
		// アサイン先ユーザであるかのチェックは不要
		return true;
	}

	/**
	 * 要承認WFでの承認機能のチェックを実行します。
	 */
	private boolean enabledCheckApprove(EIMSession sess, ObjectDomain objDomain,
				BaseEventTypeExecDomain baseEventTypeExecDomain, String processType) throws Exception {


		StatusTypeKindDomain stTypeKindDomain = baseEventTypeExecDomain.getObject().getStatus().getStatusType().getStatusTypeKind();
		if (stTypeKindDomain.getId() != AppConstant.STATUS_TYPE_KIND_ID_APPROVE)
		{
			throw new EIMAppException(sess, "EIM.ERROR.LOGIC.NOTAPPROVING", new Object[]{objDomain.getName()});
		}
		// アサインを取得する
		ObjectDomain objectDomain = baseEventTypeExecDomain.getObject();
		StatusDomain statusDomain = objectDomain.getStatus();
		AssignDomain as = new AssignDomain();
		as.setStatus(statusDomain);
		List<AssignDomain> assignDomainList = assignDao.getList(as);

		// アサイン先ユーザであるかチェック
		long userId = sess.getUser().getId();

		for (AssignDomain assign : assignDomainList) {
			if (assign.getOwner().getId() == userId) {

				if (assign.getEvent() == null) {
					if( processType == null ){
						// イベント実行で本メソッドが呼ばれた場合
						// ステータスタイプ毎のアサインプランを更新する
						ApprovalReqInfoDomain reqInfo = new ApprovalReqInfoDomain(baseEventTypeExecDomain);
						Map<Long, List<AssignPlanDomain>> assignPlanListMap = reqInfo.getApproverPlanListMap();
						for (Map.Entry<Long, List<AssignPlanDomain>> assignPlanListEntry : assignPlanListMap.entrySet()) {
							List<AssignPlanDomain> assignPlanList = assignPlanListEntry.getValue();
							if (assignPlanList.size() > 0) {
								// アサインプランの設定がある場合は更新する
								assignPlanDao.update(assignPlanList);
							} else {
								// アサインプランの設定がない場合は削除する
								AssignPlanDomain assignPlan = new AssignPlanDomain();
								assignPlan.setObject(objDomain);
								StatusTypeDomain statusType = new StatusTypeDomain();
								statusType.setId(assignPlanListEntry.getKey());
								assignPlan.setStatusType(statusType );
								assignPlanDao.delete(assignPlan);
							}
						}
					}
					return true;
				}
				else {
					// タスクが完了している
					if( processType == null ){
						throw new EIMAppException(sess, "EIM.ERROR.LOGIC.NOAPPROVEROLE");
					}else{
						return true;
					}
				}
			}
		}

		// アサイン先でない場合の例外
		throw new EIMAppException(sess, "EIM.ERROR.LOGIC.NOAPPROVEROLE");

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

	/**
	 * @return assignPlanDao
	 */
	public AssignPlanDao getAssignPlanDao() {
		return assignPlanDao;
	}

	/**
	 * @param assignPlanDao 設定する assignPlanDao
	 */
	public void setAssignPlanDao(AssignPlanDao assignPlanDao) {
		this.assignPlanDao = assignPlanDao;
	}

	/**
	 * @return the assignDao
	 */
	public AssignDao getAssignDao() {
		return assignDao;
	}

	/**
	 * @param assignDao the assignDao to set
	 */
	public void setAssignDao(AssignDao assignDao) {
		this.assignDao = assignDao;
	}

}
