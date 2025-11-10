package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.util.HashMap;
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
import jp.co.ctc_g.eim.framework.business.dao.AssignPlanDao;
import jp.co.ctc_g.eim.framework.business.domain.AssignPlanDomain;
import jp.co.ctc_g.eim.framework.business.domain.BaseEventTypeExecDomain;
import jp.co.ctc_g.eim.framework.business.domain.StatusTypeKindDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.StatusTypeDomain;
import jp.co.ctc_g.eim.framework.business.service.impl.BaseEventTypePlugInImpl;
import jp.co.ctc_g.eim.framework.common.exception.EIMAppException;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * ベースイベントタイプ「承認依頼」
 *
 */
public class BaseEvtApprovalRequestPlugInImpl extends BaseEventTypePlugInImpl {

	protected AssignPlanDao assignPlanDao = null;

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

		// オブジェクト更新権チェック
		if(objDomain.getSecurity() != null) {

			//ベースイベントタイプに設定されたアクセス権限を取得
			String aclKey = baseEventTypeExecDomain.getBaseEventType().getAclKey();
			EIMAccessRoleType acrType = SecurityUtils.getAccessRoleTypeByName(sess, aclKey);

			EIMObject object = objDomain.createEIMObject();
			if(SecurityUtils.authorized(sess, object, sess.getUser(), acrType.getId()) != true) {

				return false;
			}
		}

		// ステータスチェック
		StatusTypeKindDomain stTypeKindDomain = baseEventTypeExecDomain.getObject().getStatus().getStatusType().getStatusTypeKind();
		if (stTypeKindDomain.getId() != AppConstant.STATUS_TYPE_KIND_ID_EDITTING) {

			throw new EIMAppException(sess, "EIM.ERROR.LOGIC.NOTUPDATING", new Object[]{objDomain.getName()});
		}

		// ロック状態チェック
		if (objDomain.getLUser() != null || objDomain.getLDate() != null) {
			throw new EIMAppException(sess, "EIM.ERROR.LOGIC.OBJECT.LOCKED", new Object[]{objDomain.getName()});
		}

		// オブジェクトを取得
		EIMObject object = objDomain.createEIMObject();

		// ドキュメントが公開処理失敗の場合、承認依頼できない旨のエラーメッセージを表示
		if(AppObjectUtil.isProcFailDocument(object))
		{
			throw new EIMAppException(sess, "EIM.ERROR.LOGIC.PUBLICFAILED.DOC.FOR.APPROVE.REQUEST");
		}

		// パラメータを取得
		Map<String, Object> paramMap = baseEventTypeExecDomain.getParamMap();
		String processType = (String)paramMap.get(AppConstant.PARAM_KEY_REFER_TO_APPROVAL);
		if( processType == null || !processType.equals(AppConstant.PARAM_VALUE_REFER_TO_APPROVAL) ){
			// イベント実行で本メソッドが呼ばれた場合
			// ステータスタイプ毎のアサイン予定を更新する
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

		// 「メール通知」オブジェクトを取得
		EIMObject mailNotifyObj = null;
		EIMAttributeType attTypePublicTo = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.getValue("ATTR_NAME_MAIL_NOTIFY_PUBLIC_NOTIFY_TO"));
		if(attTypePublicTo != null){
			EIMObjectType objTypeMailNotify = ObjectUtils.getObjectTypeByName(sess, EIMConfig.getValue("OBJECT_TYPE_NAME_MAIL_NOTIFY"));
			mailNotifyObj = ObjectUtils.getObjectByTypeAndName(sess, objTypeMailNotify, String.valueOf(object.getId()));
		}

		// メール、PDF関連処理
		AppWorkFlowUtil.approvalRequestMailPDFProcess(sess, object, approvalReqInfoDomain, baseEventTypeExecDomain);

		/*--- 共通処理 ---*/

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
		EIMAttributeType attTypePublicTiming = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.getValue("ATTR_NAME_MAIL_NOTIFY_PUBLIC_TIMING"));
		if(attTypePublicTiming != null && approvalReqInfoDomain.getSendNotifyMailTiming() != null && !approvalReqInfoDomain.getSendNotifyMailTiming().equals("")){
			EventAttributeUtils.setAttribute(sess, event, attTypePublicTiming, Long.parseLong((approvalReqInfoDomain.getSendNotifyMailTiming())));
		}

		// イベント属性「公開通知先」を設定
		if(attTypePublicTo != null){
			// 「公開通知送信先」に設定する値を生成
			List<String> publisherList = approvalReqInfoDomain.getPublisherList();
			String[] publisherStrs = publisherList.toArray(new String[publisherList.size()]);
			EventAttributeUtils.setAttribute(sess, event, attTypePublicTo, publisherStrs);

			if(mailNotifyObj != null){
				// 公開通知先変更有無のチェック
				String[] srcPublisherStrs = AppObjectUtil.getStrAttrs(sess, mailNotifyObj, EIMConfig.get("ATTR_NAME_MAIL_NOTIFY_PUBLIC_NOTIFY_TO"));
				if( AppWorkFlowUtil.checkPublisherChange(srcPublisherStrs, publisherStrs)){
					// アクセス履歴作成(公開通知先変更)
					AccessUtils.createAccess(sess, object, "EIM.ACCESS.TYPE.CHANGE.PUBLISHER");
					// 操作履歴作成(公開通知先変更)
					OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, AppConstant.CHENGE_PUBLISHER,
							EIMConstant.TARGET_UPDATE, EIMConstant.OBJECT, object,
							null, null, null, AppObjectUtil.getPath(object));
				}
			}
		}

		// イベント属性「公開通知コメントログ」を設定
		EIMAttributeType attTypePublicCommentLog = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.getValue("ATTR_NAME_PUBLIC_COMMENT_LOG"));
		if(attTypePublicCommentLog != null){
			EventAttributeUtils.setAttribute(sess, event, attTypePublicCommentLog, approvalReqInfoDomain.getPublicComment());
		}

		// イベント属性「受信確認」を設定
		EIMAttributeType attTypeReply = AttributeUtils.getAttributeTypeByName(EIMThreadContext.getEIMSession(), EIMConfig.getValue("ATTR_NAME_MAIL_NOTIFY_CONFIRM_RECEIVE"));
		if(attTypeReply != null){
			EventAttributeUtils.setAttribute(sess, event, attTypeReply, approvalReqInfoDomain.getReply());
		}


		// 文書属性「スキップステータスID」を設定
		EIMAttributeType attSkipStatusId = AttributeUtils.getAttributeTypeByName(sess, "スキップステータスタイプID");
		Map<String, Object> paramMap = baseEventTypeExecDomain.getParamMap();
		Boolean lastEventFlag = Boolean.valueOf((String)paramMap.get("lastEventFlag")); // 過去「自身」が承認依頼したイベントが存在するかどうか
		String skipIdParam = (String)paramMap.get("skipStatusTypeId");

		Map<Long,String> newSkipParamMap = new HashMap<Long,String>();
		Map<Long,String> deleteSkipParamMap = new HashMap<Long,String>();
		// 画面で変更したスキップ情報を取得
		if(skipIdParam != null && skipIdParam.length() > 0){
			String[] paramArray = skipIdParam.split(",");
			// 画面から返却されるパラメータ取得
			for(int i = 0; i < paramArray.length; i++) {
				String[] param = paramArray[i].split(":");
				long statusTypeId = Long.valueOf(param[0]);
				String skipFlag = param[1];

				if (skipFlag.equals("true")) {
					// 画面でスキップ「する」に設定したもの
					newSkipParamMap.put(statusTypeId, skipFlag);
				} else {
					// 画面でスキップ「しない」に設定したもの
					deleteSkipParamMap.put(statusTypeId, skipFlag);
				}
			}
		}

		// 過去の履歴を復元する場合
		if (lastEventFlag) {
			//以前にスキップ設定がされているステータスタイプ情報があれば取得
			EIMAttribute skipStatusTypeIdAttr = object.getAttribute("スキップステータスタイプID");
			if(skipStatusTypeIdAttr != null){
				long[] oldSkipArray = TypeConvertUtils.convertToLongArray(skipStatusTypeIdAttr.getInts());
				if(oldSkipArray.length > 0){
					for(long oldSkipId : oldSkipArray){
						if ( deleteSkipParamMap.containsKey(oldSkipId)) {
							// スキップしない
						} else {
							// 画面でスキップの変更がなく、かつ過去スキップの設定がある場合復元する(承認依頼者が同一の場合に限る)
							newSkipParamMap.put(oldSkipId, "true");
						}
					}
				}
			}
		}
		// スキップステータスタイプID属性を更新
		long[] skipArrays = new long[newSkipParamMap.size()];
		int count = 0;
		for (long skipId : newSkipParamMap.keySet()) {
			skipArrays[count] = skipId;
			count++;
		}
		ObjectAttributeUtils.setAttribute(sess, object, attSkipStatusId, TypeConvertUtils.convertToBuildTypeArray(skipArrays));
		/*
		} else {
			// 画面の変更なし、かつ過去の履歴を復元する必要なし
			if (!lastEventFlag) {
				// 属性をクリア
				ObjectAttributeUtils.setAttribute(sess, object, attSkipStatusId, new int[0]);
			}
		}*/

		// SearchFramework 検索FW更新通知 対象：WF付きフォルダ、ドキュメントと配下のフォルダ、ドキュメント
		AppUpdateNoticeUtils.updateNoticeInsertWFObject(sess, object,
				"SEARCHFW_BASEEVT_APPRVLREQ_DOCUMENT", "SEARCHFW_BASEEVT_APPRVLREQ_FOLDER",
				"SEARCHFW_BASEEVT_APPRVLREQ_CHILD_DOCUMENT", "SEARCHFW_BASEEVT_APPRVLREQ_CHILD_FOLDER");

		//アクセス履歴作成
		AccessUtils.createAccess(sess, object, "EIM.ACCESS.TYPE.APPROVALREQUEST");

		//ヒストリ作成
		OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.REQUEST_APPROVE,
				EIMConstant.TARGET_APPROVE, EIMConstant.OBJECT, object,
				null, null, null, AppObjectUtil.getPath(object));

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

}
