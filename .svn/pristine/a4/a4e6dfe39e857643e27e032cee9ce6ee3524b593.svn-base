package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.util.List;
import java.util.Map;

import common.util.AppConstant;
import eim.net.EIMSession;
import jp.co.ctc_g.eim.app.document.common.util.AppWorkFlowUtil;
import jp.co.ctc_g.eim.framework.business.dao.EventHistoryDao;
import jp.co.ctc_g.eim.framework.business.dao.StatusDao;
import jp.co.ctc_g.eim.framework.business.domain.AssignDomain;
import jp.co.ctc_g.eim.framework.business.domain.GuardConditionExecDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.EventDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.StatusDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.StatusTypeDomain;
import jp.co.ctc_g.eim.framework.business.service.impl.GuardConditionPlugInImpl;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * ガード条件「実行者が現ステータスの承認依頼者」
 * 
 */
public class GuardCndCurrentApprovalRequestPlugInImpl extends GuardConditionPlugInImpl {
	
	protected EventHistoryDao eventHistoryDao = null;
	protected StatusDao statusDao = null;
	
	/**
	 * ガード条件を判定します。
	 * 
	 * @see jp.co.ctc_g.eim.framework.business.service.impl.GuardConditionPlugInImpl#judge(jp.co.ctc_g.eim.framework.business.domain.GuardConditionExecDomain)
	 */
	@Override
	public boolean judge(GuardConditionExecDomain guardConditionExecDomain) throws Exception
	{
		EIMSession sess = EIMThreadContext.getEIMSession();
		ObjectDomain objDomain = guardConditionExecDomain.getObject();
		
		//パラメータを取得
		Map<String, Object> paramMap = guardConditionExecDomain.getParamMap();
		String processType = (String)paramMap.get(AppConstant.PARAM_KEY_REFER_TO_APPROVAL);
		
		//ST遷移予測で呼ばれた場合は無条件でtrue
		if(processType != null && processType.equals(AppConstant.PARAM_VALUE_REFER_TO_APPROVAL)){
			return true;
		}
		
		/*--- 現ステータスの承認依頼者か ---*/
		boolean isRequestUser = false;
		// 遷移先のステータスタイプが直近の承認/承認依頼イベントの遷移元か
		boolean isFromStatusType = false;
		// イベント実行ユーザID
		long userId = sess.getUser().getId();
		// 遷移元ステータス
		StatusTypeDomain toStatusType = guardConditionExecDomain.getEventType().getToStatusType();
		
		// 直近の処理依頼/承認イベントを取得するため、イベント履歴を取得
		List<EventDomain> eventDomainList = AppWorkFlowUtil.getOneAheadApprovalRequestEvent(objDomain.createEIMObject());
		for (EventDomain eventDomain : eventDomainList){
			// 実行ユーザが前のステータスの承認依頼者かチェック
			if (eventDomain.getCUser().getId() == userId) {
				isRequestUser = true;
			}
			// 遷移先のステータスタイプが直近の承認/承認依頼イベントの遷移元かチェック
			if(eventDomain.getFromStatus().getStatusType().getId() == toStatusType.getId()){
				isFromStatusType = true;
			}
		}
		
		
		if (!isRequestUser || !isFromStatusType) {
			return false;
		}
		
		/*--- 現在のステータスのアサイン先が全て未タスクか ---*/
		
		// ステータスドメインを取得
		StatusDomain statusDomain = statusDao.getById(guardConditionExecDomain.getObject().getStatus().getId());
		
		// タスク済のアサイン先があるか
		for (AssignDomain assignDomain : statusDomain.getAssignList()) {
			
			if (assignDomain.getEvent() != null) {
				
				return false;
			}
		}
		
		return true;
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
}
