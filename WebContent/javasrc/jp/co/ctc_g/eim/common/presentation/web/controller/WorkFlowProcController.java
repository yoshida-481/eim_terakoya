package jp.co.ctc_g.eim.common.presentation.web.controller;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Controller;
import org.springframework.util.MultiValueMap;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import eim.bo.EIMWorkFlow;
import jp.co.ctc_g.eim.app.document.business.domain.WorkFlowHistoryDomain;
import jp.co.ctc_g.eim.common.presentation.web.dto.WorkFlowHistoryDTO;
import jp.co.ctc_g.eim.framework.business.domain.EventExecDomain;
import jp.co.ctc_g.eim.framework.business.domain.EventHistoryDomain;
import jp.co.ctc_g.eim.framework.business.domain.EventLogDomain;
import jp.co.ctc_g.eim.framework.business.domain.MailMethod;
import jp.co.ctc_g.eim.framework.business.domain.MailTypeDomain;
import jp.co.ctc_g.eim.framework.business.domain.NoticeMailDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.StatusDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.WorkFlowDomain;
import jp.co.ctc_g.eim.framework.business.service.EventExecService;
import jp.co.ctc_g.eim.framework.business.service.EventHistoryService;
import jp.co.ctc_g.eim.framework.common.util.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework2.presentation.web.controller.RestController;

/**
 * ワークフロー管理コントローラ
 *
 */
@Controller
@RequestMapping({ "/rest/app/document/workflow" })
public class WorkFlowProcController extends RestController {



	/**
	 * イベントを実行します。
	 *
	 * @param mapping このインスタンスを選択するために使用したActionMapping
	 * @param form イベント実行ドメイン
	 * @param request HTTPリクエスト
	 * @param response HTTPレスポンス
	 * @return 
	 * @throws Exception ビジネスロジックが例外をスローした場合
	 */
	@RequestMapping(value = "/doEvent", method=RequestMethod.POST)
	@ResponseBody
	public String doEvent(@RequestBody MultiValueMap<String,String> form) throws Exception {

		EventExecDomain eventExecDomain = new EventExecDomain();
		eventExecDomain.getObject().setStatus(new StatusDomain());
		//DynaBean dynaBean = (DynaBean)form;

		//=================================================================
		//formより、各リクエストパラメータを取得
		//=================================================================

		//オブジェクトID
		String strObjId = form.getFirst("objId");
		long objId = Long.valueOf(strObjId);
		eventExecDomain.getObject().setId(objId);

		//ベースイベントタイプID
		String strBaseEventTypeId = form.getFirst("baseEventTypeId");
		long baseEventTypeId = Long.valueOf(strBaseEventTypeId);
		eventExecDomain.getBaseEventType().setId(baseEventTypeId);

		//ステータスの最終更新日時
		String strStatusMDateLong = form.getFirst("statusMDateLong");
		long statusMDateLong = Long.valueOf(strStatusMDateLong);
		eventExecDomain.getObject().getStatus().setMDate(new Date(statusMDateLong));

		//実行イベントで｢即時送信｣を選択したオブジェクトIDのリスト
		List<String> immediateMailTypeList = form.get("immediateMailTypeId");
		if (immediateMailTypeList != null){
			for (String immediateMailType: immediateMailTypeList) {
				NoticeMailDomain noticeMailDomain = new NoticeMailDomain();
				long mailId = Long.valueOf(immediateMailType);
				noticeMailDomain.setId(mailId);
				noticeMailDomain.setMailMethod(MailMethod.IMMEDIATE);

				// メールタイプ種別判定対応で、旧仕様でも新仕様でもメール送信方法判定できるように項目追加
				MailTypeDomain mailTypeDomain = new MailTypeDomain();
				mailTypeDomain.setId(mailId);
				noticeMailDomain.setMailType(mailTypeDomain);

				eventExecDomain.getImmediateMailList().add(noticeMailDomain);
			}
		}

		//実行イベントで｢一括送信｣を選択したオブジェクトIDのリスト
		List<String> accumulateMailTypeList = form.get("accumulateMailTypeId");
		if (accumulateMailTypeList != null){
			for (String accumulateMailType: accumulateMailTypeList) {
				NoticeMailDomain noticeMailDomain = new NoticeMailDomain();
				long mailId = Long.valueOf(accumulateMailType);
				noticeMailDomain.setId(mailId);
				noticeMailDomain.setMailMethod(MailMethod.ACCUMULATE);

				// メールタイプ種別判定対応で、旧仕様でも新仕様でもメール送信方法判定できるように項目追加
				MailTypeDomain mailTypeDomain = new MailTypeDomain();
				mailTypeDomain.setId(mailId);
				noticeMailDomain.setMailType(mailTypeDomain);

				eventExecDomain.getAccumlateMailList().add(noticeMailDomain);
			}	
		}

		//実行イベントで｢なし｣を選択したオブジェクトIDのリスト
		List<String> nothingMailTypeList = form.get("nothingMailTypeId");
		if (nothingMailTypeList != null){
			for (String nothingMailType: nothingMailTypeList) {
				NoticeMailDomain noticeMailDomain = new NoticeMailDomain();
				long mailId = Long.valueOf(nothingMailType);
				noticeMailDomain.setId(mailId);

				// メールタイプ種別判定対応で、旧仕様でも新仕様でもメール送信方法判定できるように項目追加
				MailTypeDomain mailTypeDomain = new MailTypeDomain();
				mailTypeDomain.setId(mailId);
				noticeMailDomain.setMailType(mailTypeDomain);

				// noticeMailDomain.setMailMethod();	← ｢なし｣の場合は設定しない(EventExecServiceImplで使用しない為)
				eventExecDomain.getNothingMailList().add(noticeMailDomain);
			}
		}

		//任意のパラメータ（値が無い、もしくは固定のパラメータはスルーする）
		for (Entry<String, List<String>> entry : form.entrySet()){
			if (!entry.getKey().equals(objId)
					&& !entry.getKey().equals("baseEventTypeId")
					&& !entry.getKey().equals("statusMDateLong")
					&& !entry.getKey().equals("immediateMailTypeId")
					&& !entry.getKey().equals("accumulateMailTypeId") 
					&& !entry.getKey().equals("nothingMailTypeId"))
			{
				eventExecDomain.getParamMap().put(entry.getKey(), entry.getValue().get(0));
			}
		}
	
		// Serviceクラスのインスタンス化
		ApplicationContext context = ApplicationContextLoader.getContext();
		EventExecService eventExecService = (EventExecService)context.getBean("eventExecService");

		// イベントの実行
		eventExecService.doEvent(eventExecDomain);

		return "";
	}
	

	/**
	 * 指定されたオブジェクトが保持する、イベント履歴を取得します。
	 *
	 * @param mapping このインスタンスを選択するために使用したActionMapping
	 * @param form オブジェクトID
	 * @param request HTTPリクエスト
	 * @param response HTTPレスポンス
	 * @return ActionForwardを返送する
	 * @throws Exception ビジネスロジックが例外をスローした場合
	 */
	@RequestMapping(value = "/getWorkFlowHistory", method=RequestMethod.GET)
	@ResponseBody
	public WorkFlowHistoryDTO getEventListByObjId(@RequestParam("objId") long objId) throws Exception {

		// Serviceクラスのインスタンス化
		ApplicationContext context = ApplicationContextLoader.getContext();
		EventHistoryService eventHistoryService = (EventHistoryService)context.getBean("workflowHistoryService");

		// 履歴の取得
		EventHistoryDomain eventHistoryDomain = eventHistoryService.getByObjId(objId);

		// -------------------------------------DTO用事前処理-------------------------------------
		WorkFlowHistoryDomain wfHistoryDomain = (WorkFlowHistoryDomain)eventHistoryDomain;
		// ワークフロー
		WorkFlowDomain wfDomain = wfHistoryDomain.getWorkflow();
		EIMWorkFlow workFlow = wfDomain.createEIMWorkFlow();

		// ステータスからオブジェクトを取得する
		// WFつきフォルダ以下のドキュメントを選択した場合、ここで取得されるオブジェクトは上位WFつきフォルダ
		List<StatusDomain> stDomainList = wfHistoryDomain.getStatusList();
		ObjectDomain objDomain = stDomainList.get(stDomainList.size() - 1).getObject();

		// 「キー：ステータスタイプID、値：ステータス(同ステータスタイプ中で最新のもの)」のマップ
		Map<Long, StatusDomain> statusMap = new HashMap<Long, StatusDomain>();
		for (StatusDomain stDomain : wfHistoryDomain.getStatusList()) {
			
			long stTypeId = stDomain.getStatusType().getId();
			if (statusMap.containsKey(stTypeId)) {
				
				if (stDomain.getMDate().getTime() < statusMap.get(stTypeId).getMDate().getTime()) {
					continue;
				}
			}
			
			// 同ドキュメント同ステータスタイプのステータスで最新の可能性がある場合、マップに追加
			statusMap.put(stTypeId , stDomain);
		}
		
		// 「キー：ステータスID、値：ステータスを遷移元/遷移先ステータスとするイベントログのリスト」のマップ
		Map<Long, List<EventLogDomain>> eventMap = new HashMap<Long, List<EventLogDomain>>();
		// 空のリストでマッピングを作成
		for (StatusDomain stDomain : new ArrayList<StatusDomain>(statusMap.values())) {
			
			long statusId = stDomain.getId();
			eventMap.put(statusId, new ArrayList<EventLogDomain>());
		}
		// 遷移元/遷移先ステータスIDのマッピングが存在する場合、それぞれリストに追加
		for (EventLogDomain eventLogDomain : wfHistoryDomain.getEventLogList()) {
			
			jp.co.ctc_g.eim.framework.business.domain.entity.EventDomain eventDomain = eventLogDomain.getEvent();
			
			long fromStatusId = eventDomain.getFromStatus().getId();
			if (eventMap.containsKey(fromStatusId) ) {
				eventMap.get(fromStatusId).add(eventLogDomain);
			}
			// 遷移元ステータス・遷移先ステータスが同じ場合に二重に登録するのを防ぐ
			long toStatusId = eventDomain.getToStatus().getId();
			if (eventMap.containsKey(toStatusId) && fromStatusId != toStatusId) {
				eventMap.get(toStatusId).add(eventLogDomain);
			}
		}
		
		WorkFlowHistoryDTO dto = new WorkFlowHistoryDTO(wfHistoryDomain,workFlow,objDomain,statusMap,eventMap);
		return dto;
	}


}