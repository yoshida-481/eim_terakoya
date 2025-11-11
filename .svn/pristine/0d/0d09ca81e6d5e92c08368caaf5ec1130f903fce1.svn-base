package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.util.List;
import java.util.Map;

import common.util.AppConstant;
import eim.net.EIMSession;
import jp.co.ctc_g.eim.framework.business.dao.AssignDao;
import jp.co.ctc_g.eim.framework.business.dao.StatusDao;
import jp.co.ctc_g.eim.framework.business.domain.AssignDomain;
import jp.co.ctc_g.eim.framework.business.domain.GuardConditionExecDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.EventDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.StatusDomain;
import jp.co.ctc_g.eim.framework.business.service.impl.GuardConditionPlugInImpl;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * ガード条件「実行者が現ステータスの承認者」
 * 
 */
public class GuardCndCurrentApprovalClientPlugInImpl extends GuardConditionPlugInImpl {
	
	protected AssignDao assignDao = null;
	protected StatusDao statusDao = null;
	
	/**
	 * ガード条件を判定します。
	 * 
	 * @see jp.co.ctc_g.eim.framework.business.service.impl.GuardConditionPlugInImpl#judge(jp.co.ctc_g.eim.framework.business.domain.GuardConditionExecDomain)
	 */
	public boolean judge(GuardConditionExecDomain guardConditionExecDomain) throws Exception
	{
		EIMSession sess = EIMThreadContext.getEIMSession();
		
		//パラメータを取得
		Map<String, Object> paramMap = guardConditionExecDomain.getParamMap();
		String processType = (String)paramMap.get(AppConstant.PARAM_KEY_REFER_TO_APPROVAL);
		
		//ST遷移予測で呼ばれた場合は無条件でfalse
		//(ガード条件「実行者が現ステータスの承認依頼者」をtrueにするため)
		if(processType != null && processType.equals(AppConstant.PARAM_VALUE_REFER_TO_APPROVAL)){
			return false;
		}
		
		// ステータスドメインを取得
		//未タスクのものが取得される
		//StatusDomain statusDomain = statusDao.getById(guardConditionExecDomain.getObject().getStatus().getId());
		ObjectDomain objectDomain = guardConditionExecDomain.getObject();
		StatusDomain statusDomain = objectDomain.getStatus();
		AssignDomain assign = new AssignDomain();
		assign.setStatus(statusDomain);
		List<AssignDomain> assignDomainList = assignDao.getList(assign);
		
		// ログインユーザのタスクが存在する場合はtrue
		long userId = sess.getUser().getId();
		for (AssignDomain assignDomain : assignDomainList) {
			
			if(assignDomain.getOwner().getId() == userId
					&& ( assignDomain.getEvent() != null &&  assignDomain.getEvent().getId() != 0)
			)
			{
				return true;
			}
		}
		return false;
	}
	
	/**
	 * ガード条件アクションを実行します。
	 * 
	 * @see jp.co.ctc_g.eim.framework.business.service.impl.GuardConditionPlugInImpl#doAction(jp.co.ctc_g.eim.framework.business.domain.GuardConditionExecDomain)
	 */
	public void doAction(GuardConditionExecDomain guardConditionExecDomain) throws Exception
	{
		EventDomain event = new EventDomain();
		event.setId(0);
		
		// 現ステータスのアサイン先全員を未タスクにする
		AssignDomain prmAssignDomain = new AssignDomain();
		StatusDomain statusDomain = guardConditionExecDomain.getObject().getStatus();
		prmAssignDomain.setStatus(statusDomain);
		List<AssignDomain> assignDomainList = assignDao.getList(prmAssignDomain);
		for (AssignDomain assign : assignDomainList) {
			if (assign.getEvent() != null) {
				assign.setEvent(event);
				assignDao.update(assign);
			}
		}
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
