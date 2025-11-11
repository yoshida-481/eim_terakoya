package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.util.List;
import java.util.Map;

import common.util.AppConstant;
import eim.net.EIMSession;
import jp.co.ctc_g.eim.framework.business.dao.AssignDao;
import jp.co.ctc_g.eim.framework.business.dao.StatusDao;
import jp.co.ctc_g.eim.framework.business.domain.AssignDomain;
import jp.co.ctc_g.eim.framework.business.domain.GuardConditionExecDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.StatusDomain;
import jp.co.ctc_g.eim.framework.business.service.impl.GuardConditionPlugInImpl;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * ガード条件「実行者が最後の承認者でない」
 *
 */
public class GuardCndNotLastApproverPlugInImpl extends GuardConditionPlugInImpl {
	
	protected StatusDao statusDao = null;
	protected AssignDao assignDao = null;
	
	/**
	 * ガード条件を判定します。
	 * 
	 * @see jp.co.ctc_g.eim.framework.business.service.impl.GuardConditionPlugInImpl#judge(jp.co.ctc_g.eim.framework.business.domain.GuardConditionExecDomain)
	 */
	public boolean judge(GuardConditionExecDomain guardConditionExecDomain) throws Exception
	{
		EIMSession sess = EIMThreadContext.getEIMSession();
		
		Map<String, Object> paramMap = guardConditionExecDomain.getParamMap();
		String processType = (String)paramMap.get(AppConstant.PARAM_KEY_REFER_TO_APPROVAL);
		if(processType != null) return false;
		long userId = sess.getUser().getId();
		
		// ログインユーザ以外のタスクが残っている場合、全員承認済みではない
		AssignDomain prmAssignDomain = new AssignDomain();
		StatusDomain statusDomain = guardConditionExecDomain.getObject().getStatus();
		prmAssignDomain.setStatus(statusDomain);
		List<AssignDomain> assignDomainList = assignDao.getList(prmAssignDomain);
		long assignCnt = assignDomainList.size();
		long doneAssignCnt = 0;
		//ログインユーザがイベントを実行している場合はtrue
		boolean isApproverDoTask = false;
		for (AssignDomain assign : assignDomainList) {
			if (assign.getEvent() != null) {
				doneAssignCnt++;
				if(assign.getOwner().getId() == userId) isApproverDoTask = true;
			}
		}
		if((assignCnt - doneAssignCnt) != 1) return true;
		if(isApproverDoTask) return true;
			
		
		return false;
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
