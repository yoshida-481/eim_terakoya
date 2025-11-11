package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.util.List;
import java.util.Map;

import common.util.AppConstant;
import common.util.AppObjectUtil;
import common.util.AppUpdateNoticeUtils;
import eim.bo.EIMAccessRoleType;
import eim.bo.EIMObject;
import eim.net.EIMSession;
import eim.util.AccessUtils;
import eim.util.EIMConstant;
import eim.util.OperationHistoryUtils;
import eim.util.SecurityUtils;
import jp.co.ctc_g.eim.app.document.common.util.AppWorkFlowUtil;
import jp.co.ctc_g.eim.framework.business.dao.AssignDao;
import jp.co.ctc_g.eim.framework.business.dao.EventHistoryDao;
import jp.co.ctc_g.eim.framework.business.dao.StatusDao;
import jp.co.ctc_g.eim.framework.business.domain.AssignDomain;
import jp.co.ctc_g.eim.framework.business.domain.BaseEventTypeExecDomain;
import jp.co.ctc_g.eim.framework.business.domain.StatusTypeKindDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.EventDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.StatusDomain;
import jp.co.ctc_g.eim.framework.business.service.impl.BaseEventTypePlugInImpl;
import jp.co.ctc_g.eim.framework.common.exception.EIMAppException;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * ベースイベントタイプ「承認依頼取消」
 *
 */
public class BaseEvtCancelApprovalRequestPlugInImpl extends BaseEventTypePlugInImpl {
	
	protected EventHistoryDao eventHistoryDao = null;
	protected StatusDao statusDao = null;
	protected AssignDao assignDao = null;
	
	/**
	 * 実行権限を判定します。 
	 * 
	 * @see jp.co.ctc_g.eim.framework.business.service.impl.BaseEventTypePlugInImpl#enabled(jp.co.ctc_g.eim.framework.business.domain.BaseEventTypeExecDomain)
	 */
	public boolean enabled(BaseEventTypeExecDomain baseEventTypeExecDomain) throws Exception  {
		
		EIMSession sess = EIMThreadContext.getEIMSession();
		ObjectDomain objDomain = baseEventTypeExecDomain.getObject();
		
		//パラメータを取得
		Map<String, Object> paramMap = baseEventTypeExecDomain.getParamMap();
		String processType = (String)paramMap.get(AppConstant.PARAM_KEY_REFER_TO_APPROVAL);
		
		//ST遷移予測で呼ばれた場合は無条件でtrue
		if(processType != null && processType.equals(AppConstant.PARAM_VALUE_REFER_TO_APPROVAL)){
			return true;
		}
		
		// 実行権限チェック
		if(objDomain.getSecurity() != null) {
			
			//ベースイベントタイプに設定されたアクセス権限を取得
			String aclKey = baseEventTypeExecDomain.getBaseEventType().getAclKey();
			EIMAccessRoleType acrType = SecurityUtils.getAccessRoleTypeByName(sess, aclKey);
			
			EIMObject object = objDomain.createEIMObject();
			if(SecurityUtils.authorized(sess, object, sess.getUser(), acrType.getId()) != true) {
				
				return false;
			}
		}

		// ステータスドメインを取得
		StatusDomain statusDomain = statusDao.getById(baseEventTypeExecDomain.getObject().getStatus().getId());
		
		//ステータスチェック
		StatusTypeKindDomain stTypeKindDomain = statusDomain.getStatusType().getStatusTypeKind();
		if (stTypeKindDomain.getId() != AppConstant.STATUS_TYPE_KIND_ID_APPROVE) {
			
			throw new EIMAppException(sess, "EIM.ERROR.LOGIC.NOTUPDATING", new Object[]{objDomain.getName()});
		}
		
		//ロック状態チェック
		if (objDomain.getLUser() != null || objDomain.getLDate() != null) {
			throw new EIMAppException(sess, "EIM.ERROR.LOGIC.OBJECT.LOCKED", new Object[]{objDomain.getName()});
		}

		// イベント実行ユーザID
		long userId = sess.getUser().getId();
		
		//現在のドキュメントのステータスタイプ種別を取得
		long docthrough = AppWorkFlowUtil.getDocThrough(objDomain.getStatus().getStatusType().createEIMStatusType());
		
		//アサイン先を取得する
		ObjectDomain objectDomain = baseEventTypeExecDomain.getObject();
		AssignDomain prmAssignDomain = new AssignDomain();
		prmAssignDomain.setStatus(objectDomain.getStatus());
		List<AssignDomain> assignDomainList = assignDao.getList(prmAssignDomain);

		
		// 実行ユーザが現ステータスの承認者かチェック
		for (AssignDomain assign : assignDomainList) {
			if (assign.getOwner().getId() == userId && docthrough == AppConstant.THROUGH_APPROVE_ALL && assign.getEvent() != null) {
				return true;
			}
		}
		
		//現在のステータスのタスク済の数を取得する
		long doneAssignCnt = 0;
		for (AssignDomain assign : assignDomainList) {
			if (assign.getEvent() != null) {
				doneAssignCnt++;
			}
		}
		boolean allUnTask = doneAssignCnt == 0 ? true : false; 
		
		// 実行ユーザが前のステータスの承認依頼者かチェック
		// 直近の処理依頼/承認イベントを取得するため、イベント履歴を取得
		List<EventDomain> eventDomainList = AppWorkFlowUtil.getOneAheadApprovalRequestEvent(objDomain.createEIMObject());
		for (EventDomain eventDomain : eventDomainList){
			//ログインユーザが承認依頼者と同じ、かつ現在のステータスのタスクが全て未タスク
			//（この条件を入れないと承認依頼者と承認者が同じ場合、一部が承認済みでも取消できてしまうため）
			if (eventDomain.getCUser().getId() == userId && allUnTask) {
				return true;
			}
		}
		
		throw new EIMAppException(sess, "EIM.ERROR.LOGIC.NOAPPROVEROLE");
	}
	
	/**
	 * ベースイベントタイプアクションを実行します。 
	 * 
	 * @see jp.co.ctc_g.eim.framework.business.service.impl.BaseEventTypePlugInImpl#doAction(jp.co.ctc_g.eim.framework.business.domain.BaseEventTypeExecDomain)
	 */
	public void doAction(BaseEventTypeExecDomain baseEventTypeExecDomain) throws Exception {
		
		EIMSession sess = EIMThreadContext.getEIMSession();
		EIMObject object = baseEventTypeExecDomain.getObject().createEIMObject();
		
		// ステータスが「編集中」まで戻る場合、受信確認オブジェクトを削除する
		if (baseEventTypeExecDomain.getEvent().getToStatus().getStatusType().getStatusTypeKind().getId() 
				== AppConstant.STATUS_TYPE_KIND_ID_EDITTING) {
			
			AppObjectUtil.deleteReceiveObject(sess, object);
		}
		
		// SearchFramework 検索FW更新通知 対象：WF付きフォルダ、ドキュメントと配下のフォルダ、ドキュメント
		AppUpdateNoticeUtils.updateNoticeInsertWFObject(sess, object,
				"SEARCHFW_BASEEVT_CANCELAPPRVLREQ_DOCUMENT", "SEARCHFW_BASEEVT_CANCELAPPRVLREQ_FOLDER", 
				"SEARCHFW_BASEEVT_CANCELAPPRVLREQ_CHILD_DOCUMENT", "SEARCHFW_BASEEVT_CANCELAPPRVLREQ_CHILD_FOLDER");
		
		/*--- 共通処理 ---*/
		
		//アクセス履歴作成
		AccessUtils.createAccess(sess, object, "EIM.ACCESS.TYPE.APPROVALREQUESTCANCEL");
		
		// 操作履歴
		OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.CANCEL_REQ_APPROVE, 
				EIMConstant.TARGET_APPROVE, EIMConstant.OBJECT, object,
				null, null, null, AppObjectUtil.getPath(object));
		
	}

	/**
	 * @return the eventHistoryDao
	 */
	public EventHistoryDao getEventHistoryDao() {
		return eventHistoryDao;
	}

	/**
	 * @param eventHistoryDao the eventHistoryDao to set
	 */
	public void setEventHistoryDao(EventHistoryDao eventHistoryDao) {
		this.eventHistoryDao = eventHistoryDao;
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
