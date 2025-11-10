package jp.co.ctc_g.eim.app.document.business.domain;

import java.util.ArrayList;
import java.util.List;

import jp.co.ctc_g.eim.framework.business.domain.EventHistoryDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.StatusDomain;

public class WorkFlowHistoryDomain extends EventHistoryDomain {

	private List<StatusDomain> statusList = new ArrayList<StatusDomain>();

	/**
	 * @return the statusList
	 */
	public List<StatusDomain> getStatusList() {
		return statusList;
	}

	/**
	 * @param statusList the statusList to set
	 */
	public void setStatusList(List<StatusDomain> statusList) {
		this.statusList = statusList;
	}
}
