package jp.co.ctc_g.eim.common.presentation.web.dto;

import java.util.List;
import java.util.Map;

import eim.bo.EIMWorkFlow;
import jp.co.ctc_g.eim.app.document.business.domain.WorkFlowHistoryDomain;
import jp.co.ctc_g.eim.common.presentation.web.dto.EventLogDTO.EventLogList;
import jp.co.ctc_g.eim.common.presentation.web.dto.StatusTypeDefDTO.StatusTypeList;
import jp.co.ctc_g.eim.framework.business.domain.EventLogDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.StatusDomain;

/**
 * ワークフロー定義DTO
 * <p>
 * Struts->SpringMVCへの移行に際して、クライアントへ送信するXMLデータを再現したデータ転送用オブジェクトです。
 */
public class WorkFlowHistoryDTO {


	/** ステータスタイプリスト */
	private StatusTypeList statusTypeList = null;;

	/** イベントタイプタイプリスト */
	private EventLogList eventList = null;

	/**
	 * コンストラクタ
	 * 
	 * @param wfHistoryDomain 
	 * @param workFlow 
	 * @param objDomain 
	 * @param statusMap 
	 * @param eventMap
	 * @throws Exception
	 */
	public WorkFlowHistoryDTO(WorkFlowHistoryDomain wfHistoryDomain, EIMWorkFlow workFlow, ObjectDomain objDomain, 
			Map<Long, StatusDomain> statusMap, Map<Long, List<EventLogDomain>> eventMap) throws Exception {

		statusTypeList = new StatusTypeList(wfHistoryDomain,workFlow,objDomain,statusMap,eventMap);
		eventList = new EventLogList(wfHistoryDomain.getEventLogList());
		
	}

	/**
	 * @return statusTypeList を取得します。
	 */
	public StatusTypeList getStatusTypeList() {
		return statusTypeList;
	}

	/**
	 * @param statusTypeListを設定します。
	 */
	public void setStatusTypeList(StatusTypeList statusTypeList) {
		this.statusTypeList = statusTypeList;
	}

	/**
	 * @return eventList を取得します。
	 */
	public EventLogList getEventList() {
		return eventList;
	}

	/**
	 * @param eventListを設定します。
	 */
	public void setEventList(EventLogList eventList) {
		this.eventList = eventList;
	}
}
