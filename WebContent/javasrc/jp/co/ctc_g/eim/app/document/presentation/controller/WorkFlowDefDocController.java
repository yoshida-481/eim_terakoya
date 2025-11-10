package jp.co.ctc_g.eim.app.document.presentation.controller;

import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.apache.xerces.parsers.DOMParser;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Controller;
import org.springframework.util.MultiValueMap;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

import common.util.AppConstant;
import common.util.NamespaceUtil;
import common.util.OptionConfData;
import jp.co.ctc_g.eim.app.document.business.domain.EventTypeDocDomain;
import jp.co.ctc_g.eim.app.document.business.domain.WorkFlowDocDomain;
import jp.co.ctc_g.eim.app.document.business.domain.WorkFlowDocPDFSignatureDomain;
import jp.co.ctc_g.eim.app.document.business.service.WorkFlowDefDocService;
import jp.co.ctc_g.eim.app.document.presentation.dto.WorkFlowDefDocDTO;
import jp.co.ctc_g.eim.common.presentation.web.controller.WorkFlowDefController;
import jp.co.ctc_g.eim.framework.business.domain.MailMethod;
import jp.co.ctc_g.eim.framework.business.domain.NoticeMailDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.EventTypeDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.OtherNameDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.StatusTypeDomain;
import jp.co.ctc_g.eim.framework.common.exception.EIMAppException;
import jp.co.ctc_g.eim.framework.common.util.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * ワークフロー定義　コントローラ
 *
 */
@Controller
@RequestMapping({ "/rest/app/document/workflow" })
public class WorkFlowDefDocController extends WorkFlowDefController {

	///////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//
	// コントローラ 実装
	//
	///////////////////////////////////////////////////////////////////////////////////////////////////////////////
	/**
	 * ワークフローIDを指定して、ワークフロー定義を取得する。
	 *
	 * <li>WorkFlowDefService.getDefById()をコールしてワークフロー定義を取得する。
	 *
	 * @param form ワークフローVO
	 * @return WorkFlowDefDocDTO
	 * @throws Exception ビジネスロジックが例外をスローした場合
	 */
	@RequestMapping(value = "/getWorkFlowDefDocById", method=RequestMethod.POST)
	@ResponseBody
	public WorkFlowDefDocDTO getWorkFlowDefById(@RequestBody MultiValueMap<String,String> form) throws Exception {

		// formより、ワークフローIDを取得
		String strWorkFlowId = form.getFirst("id");
		long workFlowId = Long.valueOf(strWorkFlowId);

		// Serviceクラスのインスタンス化
		ApplicationContext contxt = ApplicationContextLoader.getContext();
		WorkFlowDefDocService workFlowDefDocService = (WorkFlowDefDocService)contxt.getBean("workFlowDefDocService");

		// ワークフロー定義取得
		WorkFlowDocDomain workFlowDocDomain = (WorkFlowDocDomain) workFlowDefDocService.getDefById(workFlowId);

		if(workFlowDocDomain == null){
			throw new EIMAppException("EIM.ERROR.LOGIC.WORKFLOW.NOTFOUND");
		}

		WorkFlowDefDocDTO dto = new WorkFlowDefDocDTO(workFlowDocDomain);

		return dto;
	}
	/**
	 * ワークフロー定義(ドキュメント用)を新規作成します。
	 *
	 * <li>WorkFlowDefService.updateDef()をコールしてワークフローを編集する。
	 *
	 * @param form ワークフローVO
	 * @return WorkFlowDefDocDTOを返送する
	 * @throws Exception ビジネスロジックが例外をスローした場合
	 */
	@RequestMapping(value = "/createWorkFlowDef", method=RequestMethod.POST)
	@ResponseBody
	public WorkFlowDefDocDTO createWorkFlowDef(@RequestBody MultiValueMap<String,String> form) throws Exception {

		//=====================================================================
		// 	リクエストパラメータ取得
		//=====================================================================
		String workFlowXml = form.getFirst("workFlowDefXml");

		//=====================================================================
		// ワークフロー定義ドメイン設定
		//=====================================================================
		WorkFlowDocDomain workFlowDocDomain = new WorkFlowDocDomain();				//return用

		//入力されたXMLからDOMを生成、ルート要素(workFlow)を取得
		DOMParser parser = new DOMParser();
		//脆弱性対応のため、外部実態参照を無効化
		parser.setFeature("http://xml.org/sax/features/external-parameter-entities" ,false);
		parser.parse(new InputSource(new StringReader(workFlowXml)));
		Document document = parser.getDocument();
		Element rootElement = document.getDocumentElement();

		// ワークフロー情報
		workFlowDocDomain = setWorkflowByXml(workFlowDocDomain, rootElement);

		//定義名称にネームスペースをセット
		setNamespaceToWorkflowDomain(workFlowDocDomain, rootElement);

		// イベントタイプ取得
		Element eventTypeListElement = (Element)rootElement.getElementsByTagName("eventTypeList").item(0);

		// ステータスタイプ
		Element statusTypeListElement = (Element)rootElement.getElementsByTagName("statusTypeList").item(0);
		super.setStatusTypeListByXml(workFlowDocDomain.getStatusTypeList(), statusTypeListElement);

		// イベントタイプ
		if( workFlowDocDomain.getDefBossApproval().equals("necessary") )
		{
			workFlowDocDomain = setEventTypeListDefaultByXml(workFlowDocDomain, statusTypeListElement, eventTypeListElement, workFlowDocDomain.getStatusTypeList());
		}
		else if( workFlowDocDomain.getDefBossApproval().equals("unnecessary") )
		{
			workFlowDocDomain = setEventTypeListDefaultByXmlUnnecessary(workFlowDocDomain, statusTypeListElement, workFlowDocDomain.getStatusTypeList());
		}
		else
		{
			setEventTypeListByXml(workFlowDocDomain.getEventTypeList(), eventTypeListElement, workFlowDocDomain.getStatusTypeList());
		}

		// 公開処理設定情報
		workFlowDocDomain = setPublshProcessByXml(workFlowDocDomain, rootElement);

		// 公開通知先参照情報 ==>
		Element publishNotifyListElement = (Element)rootElement.getElementsByTagName("publishNotifyList").item(0);
		super.setAssignEntryByXml(workFlowDocDomain.getPublishNotyfyAssignEntryDomain(), publishNotifyListElement);

		//================================================
		// Serviceの実行
		//================================================
		ApplicationContext contxt = ApplicationContextLoader.getContext();
		WorkFlowDefDocService workFlowDefDocService = (WorkFlowDefDocService)contxt.getBean("workFlowDefDocService");
		workFlowDefDocService.create(workFlowDocDomain);

		WorkFlowDefDocDTO dto = new WorkFlowDefDocDTO(workFlowDocDomain);

		return dto;
	}

	/**
	 * ワークフロー定義(ドキュメント用)を新規作成します。
	 *
	 * <li>WorkFlowDefService.updateDef()をコールしてワークフローを編集する。
	 *
	 * @param form ワークフローVOを格納したActionForm
	 * @return WorkFlowDocDomain
	 * @throws Exception ビジネスロジックが例外をスローした場合
	 */
	@RequestMapping(value = "/deleteWorkFlowDef", method=RequestMethod.POST)
	@ResponseBody
	public WorkFlowDocDomain deleteWorkFlowDef(@RequestBody MultiValueMap<String,String> form) throws Exception {


		//=====================================================================
		// 	リクエストパラメータ取得
		//=====================================================================
		String strWorkFlowId = form.getFirst("id");
		long workFlowId = Long.valueOf(strWorkFlowId);

		//=====================================================================
		// ワークフロー定義ドメイン設定
		//=====================================================================
		WorkFlowDocDomain workFlowDocDomain = new WorkFlowDocDomain();				//return用
		workFlowDocDomain.setId(workFlowId);


		//================================================
		// Serviceの実行
		//================================================
		ApplicationContext contxt = ApplicationContextLoader.getContext();
		WorkFlowDefDocService workFlowDefDocService = (WorkFlowDefDocService)contxt.getBean("workFlowDefDocService");
		workFlowDefDocService.delete(workFlowDocDomain);

		//================================================
		// 終了処理
		//================================================

		return workFlowDocDomain;
	}

	/**
	 * ワークフロー定義(ドキュメント用)を更新します。
	 *
	 * <li>WorkFlowDefService.updateDef()をコールしてワークフローを編集する。
	 *
	 * @param form ワークフローVO
	 * @return WorkFlowDefDocDTOを返送する
	 * @throws Exception ビジネスロジックが例外をスローした場合
	 */
	@RequestMapping(value = "/updateWorkFlowDef", method=RequestMethod.POST)
	@ResponseBody
	public WorkFlowDefDocDTO updateWorkFlowDef(@RequestBody MultiValueMap<String,String> form) throws Exception {

		//=====================================================================
		// 	リクエストパラメータ　取得
		//=====================================================================
		String workFlowXml = form.getFirst("workFlowDefXml");


		//=====================================================================
		// ワークフロー定義ドメイン設定
		//=====================================================================
		WorkFlowDocDomain workFlowDocDomain = new WorkFlowDocDomain();				//return用

		//入力されたXMLからDOMを生成、ルート要素(workFlow)を取得
		DOMParser parser = new DOMParser();
		//脆弱性対応のため、外部実態参照を無効化
		parser.setFeature("http://xml.org/sax/features/external-parameter-entities" ,false);
		parser.parse(new InputSource(new StringReader(workFlowXml)));
		Document document = parser.getDocument();
		Element rootElement = document.getDocumentElement();

		// ワークフロー情報
		workFlowDocDomain = setWorkflowByXml(workFlowDocDomain, rootElement);

		//定義名称にネームスペースをセット
		setNamespaceToWorkflowDomain(workFlowDocDomain, rootElement);

		// ステータスタイプ
		Element statusTypeListElement = (Element)rootElement.getElementsByTagName("statusTypeList").item(0);
		setStatusTypeListByXml(workFlowDocDomain.getStatusTypeList(), statusTypeListElement);

		// イベントタイプ
		Element eventTypeListElement = (Element)rootElement.getElementsByTagName("eventTypeList").item(0);
		setEventTypeListByXml(workFlowDocDomain.getEventTypeList(), eventTypeListElement, workFlowDocDomain.getStatusTypeList());

		// 公開処理設定情報
		workFlowDocDomain = setPublshProcessByXml(workFlowDocDomain, rootElement);

		// 公開通知先参照情報 ==>
		Element publishNotifyListElement = (Element)rootElement.getElementsByTagName("publishNotifyList").item(0);
		super.setAssignEntryByXml(workFlowDocDomain.getPublishNotyfyAssignEntryDomain(), publishNotifyListElement);

		//=====================================================================
		// Serviceの実行
		//=====================================================================
		ApplicationContext contxt = ApplicationContextLoader.getContext();
		WorkFlowDefDocService workFlowDefDocService = (WorkFlowDefDocService)contxt.getBean("workFlowDefDocService");
		workFlowDefDocService.updateWorkFlowDef(workFlowDocDomain);

		//=====================================================================
		// 終了処理
		//=====================================================================

		WorkFlowDefDocDTO dto = new WorkFlowDefDocDTO(workFlowDocDomain);

		return dto;

	}

	/**
	 * ステータス構成(ドキュメント用)を更新します。
	 *
	 * <li>WorkFlowDefService.updateDef()をコールしてワークフローを編集する。
	 *
	 * @param mapping このインスタンスを選択するために使用したActionMapping
	 * @param form ワークフローVOを格納したActionForm
	 * @param request HTTPリクエスト
	 * @param response HTTPレスポンス
	 * @return ActionForwardを返送する
	 * @throws Exception ビジネスロジックが例外をスローした場合
	 */
	@RequestMapping(value = "/updateConfigurationStatus", method=RequestMethod.POST)
	@ResponseBody
	public WorkFlowDefDocDTO updateConfigurationStatus(@RequestBody MultiValueMap<String,String> form) throws Exception {

		//=====================================================================
		// 	リクエストパラメータ　取得
		//=====================================================================
		String workFlowXml = form.getFirst("workFlowDefXml");

		//=====================================================================
		// ワークフロー定義ドメイン設定
		//=====================================================================
		WorkFlowDocDomain workFlowDocDomain = new WorkFlowDocDomain();				//return用

		//入力されたXMLからDOMを生成、ルート要素(workFlow)を取得
		DOMParser parser = new DOMParser();
		//脆弱性対応のため、外部実態参照を無効化
		parser.setFeature("http://xml.org/sax/features/external-parameter-entities" ,false);
		parser.parse(new InputSource(new StringReader(workFlowXml)));
		Document document = parser.getDocument();
		Element rootElement = document.getDocumentElement();

		// ワークフロー情報
		workFlowDocDomain = setWorkflowByXml(workFlowDocDomain, rootElement);

		// ステータスタイプ
		Element statusTypeListElement = (Element)rootElement.getElementsByTagName("statusTypeList").item(0);
		setStatusTypeListByXml(workFlowDocDomain.getStatusTypeList(), statusTypeListElement);

		// イベントタイプ
		Element eventTypeListElement = (Element)rootElement.getElementsByTagName("eventTypeList").item(0);
		workFlowDocDomain = setEventTypeListDefaultByXml(workFlowDocDomain, statusTypeListElement, eventTypeListElement , workFlowDocDomain.getStatusTypeList());

		// 公開処理設定情報
		workFlowDocDomain = setPublshProcessByXml(workFlowDocDomain, rootElement);

		// 公開通知先参照情報 ==>
		Element publishNotifyListElement = (Element)rootElement.getElementsByTagName("publishNotifyList").item(0);
		super.setAssignEntryByXml(workFlowDocDomain.getPublishNotyfyAssignEntryDomain(), publishNotifyListElement);

		//================================================
		// Serviceの実行
		//================================================
		ApplicationContext contxt = ApplicationContextLoader.getContext();
		WorkFlowDefDocService workFlowDefDocService = (WorkFlowDefDocService)contxt.getBean("workFlowDefDocService");
		workFlowDefDocService.updateConfigurationStatus(workFlowDocDomain);

		//================================================
		// 終了処理
		//================================================

		WorkFlowDefDocDTO dto = new WorkFlowDefDocDTO(workFlowDocDomain);

		return dto;
	}

	///////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//
	// privateメソッド
	//
	// 以下、XMLからDomainを生成するために使用するprivateメソッドを定義する。
	//
	///////////////////////////////////////////////////////////////////////////////////////////////////////////////
	/**
	 * ワークフロードメイン設定
	 *
	 * ワークフロー定義のXMLから取得した情報をワークフロードメインに格納する。
	 * StatusTypeDomainのid, attrTypeListは本サービスの守備範囲外なのでセットしない。
	 * @param statusTypeList 格納先リスト
	 * @param statusTypeListElement 解析するXML（ワークフロー定義XMLのworkFlow.StatusTypeList部分）
	 * @throws Exception
	 */
	private WorkFlowDocDomain setWorkflowByXml(WorkFlowDocDomain workFlowDocDomain, Element workflowElement) throws Exception {

		// 属性情報設定
		if(workflowElement.getAttribute("id") != null && workflowElement.getAttribute("id").length() > 0){
			workFlowDocDomain.setId(Long.parseLong(workflowElement.getAttribute("id")));
		}
		workFlowDocDomain.setDefName(workflowElement.getAttribute("name"));
		workFlowDocDomain.setDefBossApproval(workflowElement.getAttribute("defBossApproval"));
		workFlowDocDomain.setDefNotifyMail(workflowElement.getAttribute("defNotifyMail"));
		if(workflowElement.getAttribute("defApproveRequest") != null && workflowElement.getAttribute("defApproveRequest").length() > 0){
			workFlowDocDomain.setDefApproveRequest(Integer.parseInt(workflowElement.getAttribute("defApproveRequest")));
		}
		if(workflowElement.getAttribute("processWaitPopup") != null && workflowElement.getAttribute("processWaitPopup").length() > 0){
			workFlowDocDomain.setProcessWaitPopup(Integer.parseInt(workflowElement.getAttribute("processWaitPopup")));
		}
		if(workflowElement.getAttribute("backMail") != null && workflowElement.getAttribute("backMail").length() > 0){
			workFlowDocDomain.setBackMail(Integer.parseInt(workflowElement.getAttribute("backMail")));
		}
		if(workflowElement.getAttribute("defOcr") != null && workflowElement.getAttribute("defOcr").length() > 0){
			workFlowDocDomain.setDefOcr(Integer.parseInt(workflowElement.getAttribute("defOcr")));
		}
		if(workflowElement.getAttribute("publishNotifyMail") != null && workflowElement.getAttribute("publishNotifyMail").length() > 0){
			workFlowDocDomain.setPublishNotifyMail(Integer.parseInt(workflowElement.getAttribute("publishNotifyMail")));
		}
		if(workflowElement.getAttribute("isCreateCopy") != null && workflowElement.getAttribute("isCreateCopy").length() > 0){
			workFlowDocDomain.setCreateCopy(new Boolean(workflowElement.getAttribute("isCreateCopy")));
		}
		if(workflowElement.getAttribute("originWorkflowId") != null && workflowElement.getAttribute("originWorkflowId").length() > 0){
			workFlowDocDomain.setOriginWorkflowId(Long.parseLong(workflowElement.getAttribute("originWorkflowId")));
		}


		// 子要素設定
		Element nameListElement = (Element)workflowElement.getElementsByTagName("nameList").item(0);
		setNameListByXml(workFlowDocDomain.getNameList(), nameListElement);

		return workFlowDocDomain;
	}

	/**
	 * ワークフロー定義の定義名称にネームスペースを設定する。
	 * 入力されたXMLのルート要素
	 * @param workFlowDocDomain ワークフロー定義のXMLから形成済みのワークフロードメイン
	 * @param workflowElement
	 * @throws Exception
	 */
	private void setNamespaceToWorkflowDomain(WorkFlowDocDomain workFlowDocDomain, Element workflowElement) throws Exception {

		String defName = workflowElement.getAttribute("name");
		String namespace = workflowElement.getAttribute("nameSpace");
		if (namespace != null && !namespace.equals(""))
		{
			defName = NamespaceUtil.concatenate(namespace, defName);
			workFlowDocDomain.setDefName(defName);
		}
	}

	/**
	 * イベントタイプドメイン設定。
	 *
	 * EventTypeDomainのid, attrTypeListは本サービスの守備範囲外なのでセットしない。
	 * @param workFlowDocDomain ワークフロードメイン
	 * @param statusTypeListElement 解析するステータスXML（ワークフロー定義XMLのworkFlow.StatusTypeList部分）
	 * @param eventTypeListElement 解析するスキップイベント用XML（ワークフロー定義XMLのworkFlow.eventTypeList部分）
	 * @param statusTypeList 情報格納済みのステータスタイプドメインのリスト（生成するEventTypeDomainのfromStatusType/toStatusTypeとして使用する）
	 * @throws Exception
	 */
	private WorkFlowDocDomain setEventTypeListDefaultByXml(
			WorkFlowDocDomain workFlowDocDomain, Element statusTypeListElement, Element eventTypeListElement, List<StatusTypeDomain> statusTypeList) throws Exception
	{
		List<EventTypeDomain> eventTypeList = workFlowDocDomain.getEventTypeList();

		EventTypeDomain eventTypeDomain = null;

		NodeList nodeList = statusTypeListElement.getElementsByTagName("statusType");

		// スキップイベント作成
		if(eventTypeListElement != null ){
			NodeList eventElementList = eventTypeListElement.getElementsByTagName("eventType");
			eventTypeList = createSkipEvent(eventTypeList, eventElementList, statusTypeList);
		}

		int statusCnt = nodeList.getLength();		// ステータスタイプのカウント取得
		for (int i = 0; i < statusCnt ; i++) {
			Element statusTypeElement = (Element)nodeList.item(i);
			long stTypeKind = Long.parseLong(statusTypeElement.getAttribute("kind"));	// ステータスタイプ種別ID 取得
			int seq = Integer.parseInt(statusTypeElement.getAttribute("seq"));			// イベント優先度 取得

			// 通知メールドメインリスト
			List<NoticeMailDomain> noticeMailDomainList;
			NoticeMailDomain noticeMailDomain;

			// 他言語名称ドメインリスト
			List<OtherNameDomain> otherNameDomainList;
			OtherNameDomain otherNameDomain;

			if ( stTypeKind == AppConstant.STATUS_TYPE_KIND_ID_EDITTING ) {			// 編集中
				//===============================================
				// 承認依頼
				//===============================================
				// 通知メールドメインリスト
				noticeMailDomainList = new ArrayList<NoticeMailDomain>();
				noticeMailDomain = new NoticeMailDomain();
				noticeMailDomain.getMailType().setId(-12001);			// メール種別ID：承認依頼通知
				noticeMailDomain.setMailMethod(MailMethod.SELECTABLE);	// 通知方法
				noticeMailDomainList.add(noticeMailDomain);

				// 他言語名称ドメインリスト
				otherNameDomainList = new ArrayList<OtherNameDomain>();
				otherNameDomain = new OtherNameDomain();
				otherNameDomain.setLangId("JA");
				otherNameDomain.setName("承認依頼");
				otherNameDomainList.add(otherNameDomain);

				otherNameDomain = new OtherNameDomain();
				otherNameDomain.setLangId("EN");
				otherNameDomain.setName("Approval request");
				otherNameDomainList.add(otherNameDomain);

				// イベントタイプドメイン設定
				eventTypeDomain = setEventTypeDomain(
						1,															// イベント優先度
						super.getStatusTypeDomainBySeq(statusTypeList, seq),		// イベント始点ステータスタイプDomain
						super.getStatusTypeDomainBySeq(statusTypeList, seq+1),		// イベント終点ステータスタイプDomain
						AppConstant.BASE_EVENT_TYPE_ID_REQ_APPROVE,					// ベースイベントタイプID：承認依頼
						AppConstant.GUARD_COND_ID_NOHTING,							// ガード条件ID
						noticeMailDomainList,										// メールリスト
						otherNameDomainList,										// 言語別名称リスト
						setDefaultName(otherNameDomainList)							// デフォルト名称
				);
				eventTypeList.add(eventTypeDomain);			//イベントタイプリストに追加
			}
			else if (stTypeKind == AppConstant.STATUS_TYPE_KIND_ID_APPROVE) {		// 処理依頼中
				int cancelApprovalReqSeq = 1;	// イベントタイプ｢承認依頼取消｣ 優先順位
				int approvalSeq = 1;			// イベントタイプ｢承認｣ 優先順位

				//==============================================================================================================
				// 通過条件：全員
				//==============================================================================================================
				int through = Integer.parseInt(statusTypeElement.getAttribute("through"));	// 通過条件 取得
				if (through == 2 ) {			// 処理依頼中(全員)
					//--------------------------------------------------------------
					// 承認(実行者が最後の承認者でない)
					//--------------------------------------------------------------
					// 通知メールドメインリスト
					noticeMailDomainList = new ArrayList<NoticeMailDomain>();

					// 他言語名称ドメインリスト
					otherNameDomainList = new ArrayList<OtherNameDomain>();
					otherNameDomain = new OtherNameDomain();
					otherNameDomain.setLangId("JA");
					otherNameDomain.setName("承認");
					otherNameDomainList.add(otherNameDomain);

					otherNameDomain = new OtherNameDomain();
					otherNameDomain.setLangId("EN");
					otherNameDomain.setName("Approval");
					otherNameDomainList.add(otherNameDomain);

					// イベントタイプドメイン設定
					eventTypeDomain = setEventTypeDomain(
							approvalSeq,												// イベント優先度
							super.getStatusTypeDomainBySeq(statusTypeList, seq),		// イベント始点ステータスタイプDomain
							super.getStatusTypeDomainBySeq(statusTypeList, seq),		// イベント終点ステータスタイプDomain
							AppConstant.BASE_EVENT_TYPE_ID_APPROVAL,					// ベースイベントタイプID：承認
							AppConstant.GUARD_COND_ID_FINAL_APPROVED_IS_NOT,			// ガード条件ID：最後の承認者ではない
							noticeMailDomainList,										// メールリスト
							otherNameDomainList,										// 言語別名称リスト
							setDefaultName(otherNameDomainList)							// デフォルト名称
					);
					approvalSeq++;
					eventTypeList.add(eventTypeDomain);			//イベントタイプリストに追加
				}

				if (isLastProcessingRequest(statusCnt, seq)) {	// 最後の｢承認依頼中｣の場合

					//--------------------------------------------------------------
					// 承認(最終承認、PDF変換対象)
					//--------------------------------------------------------------
					// 通知メールドメインリスト
					noticeMailDomainList = new ArrayList<NoticeMailDomain>();
					noticeMailDomain = new NoticeMailDomain();
					noticeMailDomain.getMailType().setId(-12002);			// メール種別ID:承認通知
					noticeMailDomain.setMailMethod(MailMethod.IMMEDIATE);	// 通知方法
					noticeMailDomainList.add(noticeMailDomain);

					// 他言語名称ドメインリスト
					otherNameDomainList = new ArrayList<OtherNameDomain>();
					otherNameDomain = new OtherNameDomain();
					otherNameDomain.setLangId("JA");
					otherNameDomain.setName("承認");
					otherNameDomainList.add(otherNameDomain);

					otherNameDomain = new OtherNameDomain();
					otherNameDomain.setLangId("EN");
					otherNameDomain.setName("Approval");
					otherNameDomainList.add(otherNameDomain);

					// イベントタイプドメイン設定
					eventTypeDomain = setEventTypeDomain(
							approvalSeq,												// イベント優先度
							super.getStatusTypeDomainBySeq(statusTypeList, seq),		// イベント始点ステータスタイプDomain
							super.getStatusTypeDomainBySeq(statusTypeList, seq+1),		// イベント終点ステータスタイプDomain
							AppConstant.BASE_EVENT_TYPE_ID_APPROVAL,					// ベースイベントタイプID：承認
							AppConstant.GUARD_COND_ID_PDF_CONVERSION,					// ガード条件ID：PDF変換対象
							noticeMailDomainList,										// メールリスト
							otherNameDomainList,										// 言語別名称リスト
							setDefaultName(otherNameDomainList)							// デフォルト名称
					);
					approvalSeq++;
					eventTypeList.add(eventTypeDomain);			//イベントタイプリストに追加

					//--------------------------------------------------------------
					// 承認(最終承認、PDF変換対象外)
					//--------------------------------------------------------------
					// 通知メールドメインリスト
					noticeMailDomainList = new ArrayList<NoticeMailDomain>();
					noticeMailDomain = new NoticeMailDomain();
					noticeMailDomain.getMailType().setId(-12002);			// メール種別ID:承認通知
					noticeMailDomain.setMailMethod(MailMethod.IMMEDIATE);	// 通知方法
					noticeMailDomainList.add(noticeMailDomain);

					noticeMailDomain = new NoticeMailDomain();
					noticeMailDomain.getMailType().setId(-12005);			// メール種別ID:公開通知
					noticeMailDomain.setMailMethod(MailMethod.SELECTABLE);	// 通知方法
					noticeMailDomainList.add(noticeMailDomain);

					// 他言語名称ドメインリスト
					otherNameDomainList = new ArrayList<OtherNameDomain>();
					otherNameDomain = new OtherNameDomain();
					otherNameDomain.setLangId("JA");
					otherNameDomain.setName("承認");
					otherNameDomainList.add(otherNameDomain);

					otherNameDomain = new OtherNameDomain();
					otherNameDomain.setLangId("EN");
					otherNameDomain.setName("Approval");
					otherNameDomainList.add(otherNameDomain);

					// イベントタイプドメイン設定
					eventTypeDomain = setEventTypeDomain(
							approvalSeq,												// イベント優先度
							super.getStatusTypeDomainBySeq(statusTypeList, seq),		// イベント始点ステータスタイプDomain
							super.getStatusTypeDomainBySeq(statusTypeList, statusCnt),	// イベント終点ステータスタイプDomain
							AppConstant.BASE_EVENT_TYPE_ID_APPROVAL,					// ベースイベントタイプID:承認
							AppConstant.GUARD_COND_ID_NON_PDF_CONVERSION,				// ガード条件ID:PDF変換対象外
							noticeMailDomainList,										// メールリスト
							otherNameDomainList,										// 言語別名称リスト
							setDefaultName(otherNameDomainList)							// デフォルト名称
					);
					approvalSeq++;
					eventTypeList.add(eventTypeDomain);			//イベントタイプリストに追加
				}
				else {	// 最後の｢承認依頼中｣ではない場合
					//--------------------------------------------------------------
					// 承認(承認者が最後ではない)
					//--------------------------------------------------------------
					// 通知メールドメインリスト
					noticeMailDomainList = new ArrayList<NoticeMailDomain>();
					noticeMailDomain = new NoticeMailDomain();
					noticeMailDomain.getMailType().setId(-12001);			// メール種別ID：承認依頼通知
					noticeMailDomain.setMailMethod(MailMethod.SELECTABLE);	// 通知方法
					noticeMailDomainList.add(noticeMailDomain);

					// 他言語名称ドメインリスト
					otherNameDomainList = new ArrayList<OtherNameDomain>();
					otherNameDomain = new OtherNameDomain();
					otherNameDomain.setLangId("JA");
					otherNameDomain.setName("承認");
					otherNameDomainList.add(otherNameDomain);

					otherNameDomain = new OtherNameDomain();
					otherNameDomain.setLangId("EN");
					otherNameDomain.setName("Approval");
					otherNameDomainList.add(otherNameDomain);

					// イベントタイプドメイン設定
					eventTypeDomain = setEventTypeDomain(
							approvalSeq,												// イベント優先度
							getStatusTypeDomainBySeq(statusTypeList, seq),				// イベント始点ステータスタイプDomain
							getStatusTypeDomainBySeq(statusTypeList, seq+1),			// イベント終点ステータスタイプDomain
							AppConstant.BASE_EVENT_TYPE_ID_APPROVAL,					// ベースイベントタイプID:承認
							AppConstant.GUARD_COND_ID_NOHTING,							// ガード条件ID:なし
							noticeMailDomainList,										// メールリスト
							otherNameDomainList,										// 言語別名称リスト
							setDefaultName(otherNameDomainList)							// デフォルト名称
					);
					approvalSeq++;
					eventTypeList.add(eventTypeDomain);			//イベントタイプリストに追加
				}

				if (through == 2 ) {			// 処理依頼中(全員)
					//--------------------------------------------------------------
					// 承認依頼取消(承認者の取消)
					//--------------------------------------------------------------
					// 通知メールドメインリスト
					noticeMailDomainList = new ArrayList<NoticeMailDomain>();
					noticeMailDomain = new NoticeMailDomain();
					noticeMailDomain.getMailType().setId(-12004);			// メール種別ID：承認依頼取消通知
					noticeMailDomain.setMailMethod(MailMethod.SELECTABLE);	// 通知方法 : 選択
					noticeMailDomainList.add(noticeMailDomain);

					// 他言語名称ドメインリスト
					otherNameDomainList = new ArrayList<OtherNameDomain>();
					otherNameDomain = new OtherNameDomain();
					otherNameDomain.setLangId("JA");
					otherNameDomain.setName("承認依頼取消");
					otherNameDomainList.add(otherNameDomain);

					otherNameDomain = new OtherNameDomain();
					otherNameDomain.setLangId("EN");
					otherNameDomain.setName("Cancel approval request");
					otherNameDomainList.add(otherNameDomain);

					// イベントタイプドメイン設定
					eventTypeDomain = setEventTypeDomain(
							cancelApprovalReqSeq,										// イベント優先度
							super.getStatusTypeDomainBySeq(statusTypeList, seq),		// イベント始点ステータスタイプDomain
							super.getStatusTypeDomainBySeq(statusTypeList, seq),		// イベント終点ステータスタイプDomain
							AppConstant.BASE_EVENT_TYPE_ID_CANCEL_REQ_APPROVE,			// ベースイベントタイプID：承認依頼取消
							AppConstant.GUARD_COND_ID_CURRENT_APPROVAL_CLIENT,			// ガード条件ID：実行者が現ステータスの承認者
							noticeMailDomainList,										// メールリスト
							otherNameDomainList,										// 言語別名称リスト
							setDefaultName(otherNameDomainList)							// デフォルト名称
					);
					cancelApprovalReqSeq++;
					eventTypeList.add(eventTypeDomain);			//イベントタイプリストに追加
				}

				//==============================================================================================================
				// 通過条件：一人・全員(共通のイベント)
				//==============================================================================================================
				//--------------------------------------------------------------
				// 承認依頼取消(承認依頼者、一つ前のステータスへ)
				//--------------------------------------------------------------
				// 通知メールドメインリスト
				noticeMailDomainList = new ArrayList<NoticeMailDomain>();
				noticeMailDomain = new NoticeMailDomain();
				noticeMailDomain.getMailType().setId(-12004);			// メール種別ID：承認依頼取消通知
				noticeMailDomain.setMailMethod(MailMethod.SELECTABLE);	// 通知方法 : 選択
				noticeMailDomainList.add(noticeMailDomain);

				// 他言語名称ドメインリスト
				otherNameDomainList = new ArrayList<OtherNameDomain>();
				otherNameDomain = new OtherNameDomain();
				otherNameDomain.setLangId("JA");
				otherNameDomain.setName("承認依頼取消");
				otherNameDomainList.add(otherNameDomain);

				otherNameDomain = new OtherNameDomain();
				otherNameDomain.setLangId("EN");
				otherNameDomain.setName("Cancel approval request");
				otherNameDomainList.add(otherNameDomain);

				// イベントタイプドメイン設定
				eventTypeDomain = setEventTypeDomain(
						cancelApprovalReqSeq,										// イベント優先度
						super.getStatusTypeDomainBySeq(statusTypeList, seq),		// イベント始点ステータスタイプDomain
						super.getStatusTypeDomainBySeq(statusTypeList, seq-1),		// イベント終点ステータスタイプDomain
						AppConstant.BASE_EVENT_TYPE_ID_CANCEL_REQ_APPROVE,			// ベースイベントタイプID：承認依頼取消
						AppConstant.GUARD_COND_ID_CURRENT_APPROVAL_REQUEST,			// ガード条件ID：実行者が現ステータスの承認依頼者
						noticeMailDomainList,										// メールリスト
						otherNameDomainList,										// 言語別名称リスト
						setDefaultName(otherNameDomainList)							// デフォルト名称
				);
				cancelApprovalReqSeq++;
				eventTypeList.add(eventTypeDomain);			//イベントタイプリストに追加

				//--------------------------------------------------------------
				// 差戻し(編集中へ)
				//--------------------------------------------------------------
				// 通知メールドメインリスト
				noticeMailDomainList = new ArrayList<NoticeMailDomain>();
				noticeMailDomain = new NoticeMailDomain();
				noticeMailDomain.getMailType().setId(-12003);			// メール種別ID：差戻し通知
				noticeMailDomain.setMailMethod(MailMethod.SELECTABLE);	// 通知方法 : 選択
				noticeMailDomainList.add(noticeMailDomain);

				// 他言語名称ドメインリスト
				otherNameDomainList = new ArrayList<OtherNameDomain>();
				otherNameDomain = new OtherNameDomain();
				otherNameDomain.setLangId("JA");
				otherNameDomain.setName("差戻し");
				otherNameDomainList.add(otherNameDomain);

				otherNameDomain = new OtherNameDomain();
				otherNameDomain.setLangId("EN");
				otherNameDomain.setName("Send back");
				otherNameDomainList.add(otherNameDomain);

				// イベントタイプドメイン設定
				eventTypeDomain = setEventTypeDomain(
						1,															// イベント優先度
						super.getStatusTypeDomainBySeq(statusTypeList, seq),		// イベント始点ステータスタイプDomain
						super.getStatusTypeDomainBySeq(statusTypeList, 1),			// イベント終点ステータスタイプDomain
						AppConstant.BASE_EVENT_TYPE_ID_SEND_BACK,					// ベースイベントタイプID：差戻し
						AppConstant.GUARD_COND_ID_NOHTING,							// ガード条件ID：なし
						noticeMailDomainList,										// メールリスト
						otherNameDomainList,										// 言語別名称リスト
						setDefaultName(otherNameDomainList)							// デフォルト名称
				);
				eventTypeList.add(eventTypeDomain);			//イベントタイプリストに追加

			}
			else if (stTypeKind == AppConstant.STATUS_TYPE_KIND_ID_PROCESSING_PUBLIC) {			// 公開処理中(固定)
				//===============================================
				// 取戻し
				//===============================================
				// 通知メールドメインリスト
				noticeMailDomainList = new ArrayList<NoticeMailDomain>();
				/**
				noticeMailDomain = new NoticeMailDomain();
				noticeMailDomain.getMailType().setId(-12001);			// メール種別ID：承認依頼通知
				noticeMailDomain.setMailMethod(MailMethod.SELECTABLE);	// 通知方法
				noticeMailDomainList.add(noticeMailDomain);
				 **/

				// 他言語名称ドメインリスト
				otherNameDomainList = new ArrayList<OtherNameDomain>();
				otherNameDomain = new OtherNameDomain();
				otherNameDomain.setLangId("JA");
				otherNameDomain.setName("取戻し");
				otherNameDomainList.add(otherNameDomain);

				otherNameDomain = new OtherNameDomain();
				otherNameDomain.setLangId("EN");
				otherNameDomain.setName("Take back");
				otherNameDomainList.add(otherNameDomain);

				// イベントタイプドメイン設定
				eventTypeDomain = setEventTypeDomain(
						1,															// イベント優先度
						super.getStatusTypeDomainBySeq(statusTypeList, seq),		// イベント始点ステータスタイプDomain
						super.getStatusTypeDomainBySeq(statusTypeList, 1),		// イベント終点ステータスタイプDomain
						AppConstant.BASE_EVENT_TYPE_ID_TAKE_BACK,					// ベースイベントタイプID：取戻し
						AppConstant.GUARD_COND_ID_NOHTING,							// ガード条件ID
						noticeMailDomainList,										// メールリスト
						otherNameDomainList,										// 言語別名称リスト
						setDefaultName(otherNameDomainList)							// デフォルト名称
				);
				eventTypeList.add(eventTypeDomain);			//イベントタイプリストに追加


				//===============================================
				// 公開
				//===============================================
				// 通知メールドメインリスト
				noticeMailDomainList = new ArrayList<NoticeMailDomain>();
				noticeMailDomain = new NoticeMailDomain();
				noticeMailDomain.getMailType().setId(-12005);			// メール種別ID：公開通知
				noticeMailDomain.setMailMethod(MailMethod.SELECTABLE);	// 通知方法
				noticeMailDomainList.add(noticeMailDomain);

				// 他言語名称ドメインリスト
				otherNameDomainList = new ArrayList<OtherNameDomain>();
				otherNameDomain = new OtherNameDomain();
				otherNameDomain.setLangId("JA");
				otherNameDomain.setName("公開");
				otherNameDomainList.add(otherNameDomain);

				otherNameDomain = new OtherNameDomain();
				otherNameDomain.setLangId("EN");
				otherNameDomain.setName("Public");
				otherNameDomainList.add(otherNameDomain);

				// イベントタイプドメイン設定
				eventTypeDomain = setEventTypeDomain(
						1,															// イベント優先度
						super.getStatusTypeDomainBySeq(statusTypeList, seq),		// イベント始点ステータスタイプDomain
						super.getStatusTypeDomainBySeq(statusTypeList, statusCnt),	// イベント終点ステータスタイプDomain
						AppConstant.BASE_EVENT_TYPE_ID_PUBLIC,						// ベースイベントタイプID:公開
						AppConstant.GUARD_COND_ID_NOHTING,							// ガード条件ID:なし
						noticeMailDomainList,										// メールリスト
						otherNameDomainList,										// 言語別名称リスト
						setDefaultName(otherNameDomainList)							// デフォルト名称
				);
				eventTypeList.add(eventTypeDomain);			//イベントタイプリストに追加
			}
			else if (stTypeKind == AppConstant.STATUS_TYPE_KIND_ID_PUBLIC) {			// 公開済(固定)
				//===============================================
				// 公開取消
				//===============================================
				// 通知メールドメインリスト
				noticeMailDomainList = new ArrayList<NoticeMailDomain>();

				noticeMailDomain = new NoticeMailDomain();
				noticeMailDomain.getMailType().setId(-12006);			// メール種別ID：公開取消通知
				noticeMailDomain.setMailMethod(MailMethod.IMMEDIATE);	// 通知方法：即時
				noticeMailDomainList.add(noticeMailDomain);


				// 他言語名称ドメインリスト
				otherNameDomainList = new ArrayList<OtherNameDomain>();
				otherNameDomain = new OtherNameDomain();
				otherNameDomain.setLangId("JA");
				otherNameDomain.setName("公開取消");
				otherNameDomainList.add(otherNameDomain);

				otherNameDomain = new OtherNameDomain();
				otherNameDomain.setLangId("EN");
				otherNameDomain.setName("Public cancel");
				otherNameDomainList.add(otherNameDomain);

				// イベントタイプドメイン設定
				eventTypeDomain = setEventTypeDomain(
						1,															// イベント優先度
						super.getStatusTypeDomainBySeq(statusTypeList, seq),		// イベント始点ステータスタイプDomain
						super.getStatusTypeDomainBySeq(statusTypeList, 1),			// イベント終点ステータスタイプDomain
						AppConstant.BASE_EVENT_TYPE_ID_PUBLIC_CANCEL,				// ベースイベントタイプID：公開取消
						AppConstant.GUARD_COND_ID_NOHTING,							// ガード条件ID
						noticeMailDomainList,										// メールリスト
						otherNameDomainList,										// 言語別名称リスト
						setDefaultName(otherNameDomainList)							// デフォルト名称
				);
				eventTypeList.add(eventTypeDomain);			//イベントタイプリストに追加
			}
			else {
				continue;
			}
		}
		return workFlowDocDomain;
	}

	/**
	 * スキップ用イベントタイプリスト作成
	 * @param eventTypeList
	 * @param skipEventElementList
	 * @param statusTypeList
	 * @return eventTypeList
	 * @throws Exception
	 */
	private List<EventTypeDomain> createSkipEvent(List<EventTypeDomain> eventTypeList, NodeList skipEventElementList, List<StatusTypeDomain> statusTypeList) throws Exception{

		EventTypeDocDomain eventTypeDocDomain = null;
		EventTypeDomain eventTypeDomain = null;
		int requestApprovePriority = 2;
		int approvePriority = 4;
		int backPriority = 3;
		for (int i = 0; i < skipEventElementList.getLength() ; i++) {
			Element eventTypeElement = (Element)skipEventElementList.item(i);
			int fromSeq = Integer.parseInt(eventTypeElement.getAttribute("fromStatusTypeSeq"));
			int toSeq = Integer.parseInt(eventTypeElement.getAttribute("toStatusTypeSeq"));
			boolean skipFlag = Boolean.valueOf( eventTypeElement.getAttribute("skipFlag"));
			// スキップイベントかチェック
			if(eventTypeElement.getAttribute("skipFlag") == null || skipFlag == false){
				// skipFlagがnullもしくはfalseの場合スキップイベントは作成しない
				continue;
			}

			if(toSeq == statusTypeList.size()){
				// toSeqが公開済の場合、公開処理中用イベント、公開済用イベント作成
				// 公開処理中用イベント作成
				eventTypeDocDomain = new EventTypeDocDomain();
				eventTypeDocDomain.setBaseEventTypeID(AppConstant.BASE_EVENT_TYPE_ID_APPROVAL);
				eventTypeDocDomain.setEventSeq(approvePriority);
				eventTypeDocDomain.setGuardCondID(AppConstant.GUARD_COND_ID_PDF_CONVERSION);
				eventTypeDocDomain.setFromStatysTypeSeq(fromSeq);
				eventTypeDocDomain.setToStatysTypeSeq(toSeq-1);
				eventTypeDocDomain.setOtherEventTypeNameJA("承認");
				eventTypeDocDomain.setOtherEventTypeNameEN("Approval");
				eventTypeDocDomain.setMailTypeID(-12002);
				eventTypeDocDomain.setMailMethod(MailMethod.IMMEDIATE);
				eventTypeDomain = createEventType(eventTypeDocDomain, statusTypeList);
				eventTypeList.add(eventTypeDomain);
				approvePriority++;

				// 公開済用イベント作成
				eventTypeDocDomain = new EventTypeDocDomain();
				eventTypeDocDomain.setBaseEventTypeID(AppConstant.BASE_EVENT_TYPE_ID_APPROVAL);
				eventTypeDocDomain.setEventSeq(approvePriority);
				eventTypeDocDomain.setGuardCondID(AppConstant.GUARD_COND_ID_NON_PDF_CONVERSION);
				eventTypeDocDomain.setFromStatysTypeSeq(fromSeq);
				eventTypeDocDomain.setToStatysTypeSeq(toSeq);
				eventTypeDocDomain.setOtherEventTypeNameJA("承認");
				eventTypeDocDomain.setOtherEventTypeNameEN("Approval");
				List<Long> mailTypeIDlist = new ArrayList<Long>();
				long mailTypeId = -12002;
				mailTypeIDlist.add(mailTypeId);
				mailTypeId = -12005;
				mailTypeIDlist.add(mailTypeId);
				eventTypeDocDomain.setMailTypeIDList(mailTypeIDlist);
				List<MailMethod> mailMethodlist = new ArrayList<MailMethod>();
				mailMethodlist.add(MailMethod.IMMEDIATE);
				mailMethodlist.add(MailMethod.SELECTABLE);
				eventTypeDocDomain.setMailMethodList(mailMethodlist);
				eventTypeDomain = createEventType(eventTypeDocDomain, statusTypeList);
				eventTypeList.add(eventTypeDomain);
				approvePriority++;
			}else{
				// 承認の場合

				if(fromSeq == 1){
					// 始点が1の場合、承認依頼用イベントタイプ作成
					eventTypeDocDomain = new EventTypeDocDomain();
					eventTypeDocDomain.setBaseEventTypeID(AppConstant.BASE_EVENT_TYPE_ID_REQ_APPROVE);
					eventTypeDocDomain.setEventSeq(requestApprovePriority);
					eventTypeDocDomain.setGuardCondID(AppConstant.GUARD_COND_ID_NOHTING);
					eventTypeDocDomain.setFromStatysTypeSeq(1);
					eventTypeDocDomain.setToStatysTypeSeq(toSeq);
					eventTypeDocDomain.setOtherEventTypeNameJA("承認依頼");
					eventTypeDocDomain.setOtherEventTypeNameEN("Approval request");
					eventTypeDocDomain.setMailTypeID(-12001);
					eventTypeDocDomain.setMailMethod(MailMethod.SELECTABLE);
					eventTypeDomain = createEventType(eventTypeDocDomain, statusTypeList);
					requestApprovePriority++;
				}else{
					// 承認用イベントタイプ作成
					eventTypeDocDomain = new EventTypeDocDomain();
					eventTypeDocDomain.setBaseEventTypeID(AppConstant.BASE_EVENT_TYPE_ID_APPROVAL);
					eventTypeDocDomain.setEventSeq(approvePriority);
					eventTypeDocDomain.setGuardCondID(AppConstant.GUARD_COND_ID_NOHTING);
					eventTypeDocDomain.setFromStatysTypeSeq(fromSeq);
					eventTypeDocDomain.setToStatysTypeSeq(toSeq);
					eventTypeDocDomain.setOtherEventTypeNameJA("承認");
					eventTypeDocDomain.setOtherEventTypeNameEN("Approval");
					eventTypeDocDomain.setMailTypeID(-12001);
					eventTypeDocDomain.setMailMethod(MailMethod.SELECTABLE);
					eventTypeDomain = createEventType(eventTypeDocDomain, statusTypeList);
					approvePriority++;
				}
				eventTypeList.add(eventTypeDomain);

				// 承認依頼取消用イベント作成
				eventTypeDocDomain = new EventTypeDocDomain();
				eventTypeDocDomain.setBaseEventTypeID(AppConstant.BASE_EVENT_TYPE_ID_CANCEL_REQ_APPROVE);
				eventTypeDocDomain.setEventSeq(backPriority);
				eventTypeDocDomain.setGuardCondID(AppConstant.GUARD_COND_ID_CURRENT_APPROVAL_REQUEST);
				eventTypeDocDomain.setFromStatysTypeSeq(toSeq);
				eventTypeDocDomain.setToStatysTypeSeq(fromSeq);
				eventTypeDocDomain.setOtherEventTypeNameJA("承認依頼取消");
				eventTypeDocDomain.setOtherEventTypeNameEN("Cancel approval request");
				eventTypeDocDomain.setMailTypeID(-12004);
				eventTypeDocDomain.setMailMethod(MailMethod.SELECTABLE);
				eventTypeDomain = createEventType(eventTypeDocDomain, statusTypeList);
				eventTypeList.add(eventTypeDomain);
				backPriority++;
			}

		}
		return eventTypeList;
	}


	/**
	 * イベントタイプドメインの作成
	 *
	 * @param eventTypeDocDomain
	 * @param statusTypeList
	 * @return eventTypeDomain
	 * @throws Exception
	 */
	private EventTypeDomain createEventType(EventTypeDocDomain eventTypeDocDomain, List<StatusTypeDomain> statusTypeList) throws Exception{
		EventTypeDomain eventTypeDomain = null;

		// 通知メールドメインリスト
		List<NoticeMailDomain> noticeMailDomainList = new ArrayList<NoticeMailDomain>();
		if(eventTypeDocDomain.getMailTypeIDList().size() < 1){
			// 複数でない場合
			NoticeMailDomain noticeMailDomain = new NoticeMailDomain();
			noticeMailDomain.getMailType().setId(eventTypeDocDomain.getMailTypeID());			// メール種別ID
			noticeMailDomain.setMailMethod(eventTypeDocDomain.getMailMethod());					// 通知方法
			noticeMailDomainList.add(noticeMailDomain);
		}else{
			// 複数の場合
			for(int i = 0; i < eventTypeDocDomain.getMailTypeIDList().size(); i++){
				NoticeMailDomain noticeMailDomain = new NoticeMailDomain();
				noticeMailDomain.getMailType().setId(eventTypeDocDomain.getMailTypeIDList().get(i));
				noticeMailDomain.setMailMethod(eventTypeDocDomain.getMailMethodList().get(i));
				noticeMailDomainList.add(noticeMailDomain);
			}
		}

		// 他言語名称ドメインリスト
		List<OtherNameDomain> otherNameDomainList = new ArrayList<OtherNameDomain>();
		OtherNameDomain otherNameDomain = new OtherNameDomain();
		otherNameDomain.setLangId("JA");
		otherNameDomain.setName(eventTypeDocDomain.getOtherEventTypeNameJA());
		otherNameDomainList.add(otherNameDomain);

		otherNameDomain = new OtherNameDomain();
		otherNameDomain.setLangId("EN");
		otherNameDomain.setName(eventTypeDocDomain.getOtherEventTypeNameEN());
		otherNameDomainList.add(otherNameDomain);

		// イベントタイプドメイン設定
		eventTypeDomain = setEventTypeDomain(
				eventTypeDocDomain.getEventSeq(),															// イベント優先度
				super.getStatusTypeDomainBySeq(statusTypeList, eventTypeDocDomain.getFromStatysTypeSeq()),	// イベント始点ステータスタイプDomain
				super.getStatusTypeDomainBySeq(statusTypeList, eventTypeDocDomain.getToStatysTypeSeq()),		// イベント終点ステータスタイプDomain
				eventTypeDocDomain.getBaseEventTypeID(),					// ベースイベントタイプID
				eventTypeDocDomain.getGuardCondID(),						// ガード条件ID
				noticeMailDomainList,										// メールリスト
				otherNameDomainList,										// 言語別名称リスト
				setDefaultName(otherNameDomainList)							// デフォルト名称
		);
		return eventTypeDomain;
	}


	/**
	 * デフォルト言語の取得
	 *
	 * @param otherNameDomainList 他言語名称ドメインリスト
	 * @return
	 */
	private String setDefaultName(List<OtherNameDomain> otherNameDomainList) throws Exception {
		String defName = "";
		String defLang = EIMThreadContext.getEIMSession().getLangId();

		for(int i=0; i < otherNameDomainList.size(); i++){
			OtherNameDomain otherNameDomain = (OtherNameDomain)otherNameDomainList.get(i);
			if (defLang.equals(otherNameDomain.getLangId())) {
				defName = otherNameDomain.getName();
			}
		}

		return defName;
	}

	/**
	 * ステータスタイプが最後の｢処理依頼中｣か判定
	 *
	 * @param nodeList ステータスタイプ情報
	 * @param currentSeq
	 * @return
	 * @throws Exception
	 */
	private Boolean isLastProcessingRequest(int statusCnt, int currentSeq) throws Exception {
		// 最後の｢承認依頼中｣ = ステータスタイプ数 - 1｢(公開処理中)｣ - 1(｢公開済み｣)
		int lastProcessingRequestSeq = statusCnt - 2;
		if(lastProcessingRequestSeq == currentSeq) {
			return true;
		}
		return false;
	}

	/**
	 * イベントタイプドメイン設定
	 *
	 * @param seq						並び順
	 * @param fromStatusTypeDomain		イベント始点ステータスタイプDomain
	 * @param toStatusTypeDomain		イベント終点ステータスタイプDomain
	 * @param baseEventTypeId			ベースイベントタイプDomain
	 * @param guardConditionId			ガード条件ID
	 * @param mailListElement			メールリスト
	 * @param nameListElement			言語別名称リスト
	 * @param defName					デフォルト名称
	 * @return
	 * @throws Exception
	 */
	private EventTypeDomain setEventTypeDomain (int seq, StatusTypeDomain fromStatusTypeDomain, StatusTypeDomain toStatusTypeDomain, long baseEventTypeId,
												long guardConditionId, List<NoticeMailDomain> noticeMailDomainList, List<OtherNameDomain> otherNameDomainList, String defName) throws Exception {
		EventTypeDomain eventTypeDomain = new EventTypeDomain();

		//seq
		eventTypeDomain.setSeq(seq);

		//fromStatusType（statusTypeList内のStatusTypeDomainインスタンスへの参照としてセットする）
		eventTypeDomain.setFromStatusType(fromStatusTypeDomain);

		//toStatusType（statusTypeList内のStatusTypeDomainインスタンスへの参照としてセットする）
		eventTypeDomain.setToStatusType(toStatusTypeDomain);

		//baseEventType(Domain)
		eventTypeDomain.getBaseEventType().setId(baseEventTypeId);

		//guardCondition(Domain)
		eventTypeDomain.getGuardCondition().setId(guardConditionId);

		//mailList(DomainのList)
		if(noticeMailDomainList.size() > 0) {
			eventTypeDomain.setMailList(noticeMailDomainList);
		}

		//nameList
		eventTypeDomain.setNameList(otherNameDomainList);
		// defName
		eventTypeDomain.setDefName(defName);

		return eventTypeDomain;
	}

	/**
	 * ワークフロードメイン設定 公開処理設定情報
	 *
	 * ワークフロー定義のXMLから取得した公開処理設定情報をワークフロードメインに格納。
	 *
	 * @param statusTypeList 格納先リスト
	 * @param statusTypeListElement 解析するXML（ワークフロー定義XMLのworkFlow.StatusTypeList部分）
	 * @throws Exception
	 */
	private WorkFlowDocDomain setPublshProcessByXml(WorkFlowDocDomain workFlowDocDomain, Element rootElement) throws Exception {
		NodeList nodeList = rootElement.getElementsByTagName("pdfsignature");

		if(nodeList.getLength() == 0) return workFlowDocDomain;

		Element statusTypeElement = (Element)nodeList.item(0);

		workFlowDocDomain.setDoPDFConvert(new Boolean(statusTypeElement.getAttribute("doPDFConvert")));		// 公開ファイルをPDF化
		workFlowDocDomain.setDoSetTerm(new Boolean(statusTypeElement.getAttribute("doSetTerm")));			// 有効期限設定：設定フラグ
		if (statusTypeElement.getAttribute("termNumParam") != null && statusTypeElement.getAttribute("termNumParam").length() > 0) {
			workFlowDocDomain.setTermNumParam(Integer.parseInt(statusTypeElement.getAttribute("termNumParam")));		// 有効期限設定：期間数字
		}
		workFlowDocDomain.setTermUnitParam(statusTypeElement.getAttribute("termUnitParam"));	// 有効期限設定：期間単位
		workFlowDocDomain.setDoPDFURL(new Boolean(statusTypeElement.getAttribute("doPDFURL")));		// 公開ファイルにURLを挿入

		// 電子署名／セキュリティ設定
		WorkFlowDocPDFSignatureDomain pdfSignatureDomain = new WorkFlowDocPDFSignatureDomain(
				statusTypeElement.getAttribute("doSignAndSetSecurity"),
				statusTypeElement.getAttribute("doSignPDF"),
				statusTypeElement.getAttribute("insertApproveDate"),
				statusTypeElement.getAttribute("insertApproveUser"),
				statusTypeElement.getAttribute("insertPage"),
				statusTypeElement.getAttribute("insertPlace"),
				statusTypeElement.getAttribute("insertPlaceX"),
				statusTypeElement.getAttribute("insertPlaceY"),
				statusTypeElement.getAttribute("approveNamelang"),
				statusTypeElement.getAttribute("signJobName"),
				statusTypeElement.getAttribute("doSetSecurity"),
				statusTypeElement.getAttribute("doSetSecurityPassword"),
				statusTypeElement.getAttribute("securityPassword"),
				statusTypeElement.getAttribute("doSetReferencePassword"),
				statusTypeElement.getAttribute("referencePassword"),
				statusTypeElement.getAttribute("forbidPrint"),
				statusTypeElement.getAttribute("forbidEdit"),
				statusTypeElement.getAttribute("forbidAnnotate"),
				statusTypeElement.getAttribute("forbidReproduce")
		);
		workFlowDocDomain.setSignatureCommand(pdfSignatureDomain);

		return workFlowDocDomain;
	}

	/**
	 * ワークフロー定義(ドキュメント用)を更新します。
	 *
	 * <li>WorkFlowDefService.updateDef()をコールしてワークフローを編集する。
	 *
	 * @param form ワークフローVO
	 * @return WorkFlowDocDomainを返送する
	 * @throws Exception ビジネスロジックが例外をスローした場合
	 */
	@RequestMapping(value = "/updateStatusNameAndAssigns", method=RequestMethod.POST)
	@ResponseBody
	public WorkFlowDocDomain updateStatusNameAndAssigns(@RequestBody MultiValueMap<String,String> form) throws Exception {

		//=====================================================================
		// 	リクエストパラメータ　取得
		//=====================================================================
		String workFlowXml = form.getFirst("workFlowDefXml");

		String strWorkFlowId = form.getFirst("workFlowId");
		long workFlowId = Long.valueOf(strWorkFlowId);

		boolean strUpdateFlag;
		if ("true".equals(form.getFirst("updateFlag"))){
			strUpdateFlag = true;
		} else {
			strUpdateFlag = false;
		}

		//=====================================================================
		// ワークフロー定義ドメイン設定
		//=====================================================================
		WorkFlowDocDomain workFlowDocDomain = new WorkFlowDocDomain();				//return用

		//入力されたXMLからDOMを生成、ルート要素(workFlow)を取得
		DOMParser parser = new DOMParser();
		//脆弱性対応のため、外部実態参照を無効化
		parser.setFeature("http://xml.org/sax/features/external-parameter-entities" ,false);
		parser.parse(new InputSource(new StringReader(workFlowXml)));
		Document document = parser.getDocument();
		Element rootElement = document.getDocumentElement();


		// ワークフロー情報
		workFlowDocDomain = setWorkflowByXml(workFlowDocDomain, rootElement);
		workFlowDocDomain.setId(workFlowId);

		// ステータスタイプ
		Element statusTypeListElement = (Element)rootElement.getElementsByTagName("statusTypeList").item(0);
		setStatusTypeListByXml(workFlowDocDomain.getStatusTypeList(), statusTypeListElement);

		// チェックイン可否、上長のみ表示を「ワークフロー設定オブジェクト」に設定する
		setStatusTypeSettingByXml(workFlowDocDomain, statusTypeListElement);

		// イベントタイプ
		Element eventTypeListElement = (Element)rootElement.getElementsByTagName("eventTypeList").item(0);
		setEventTypeListByXml(workFlowDocDomain.getEventTypeList(), eventTypeListElement, workFlowDocDomain.getStatusTypeList());

		//=====================================================================
		// Serviceの実行
		//=====================================================================
		ApplicationContext contxt = ApplicationContextLoader.getContext();
		WorkFlowDefDocService workFlowDefDocService = (WorkFlowDefDocService)contxt.getBean("workFlowDefDocService");
		workFlowDefDocService.updateStatusNameAndAssigns(workFlowDocDomain, strUpdateFlag);

		//=====================================================================
		// 終了処理
		//=====================================================================

		return workFlowDocDomain;

	}

	@Override
	protected void setEventTypeListByXml(List<EventTypeDomain> eventTypeList,
			Element eventTypeListElement, List<StatusTypeDomain> statusTypeList)
			throws Exception {
		super.setEventTypeListByXml(eventTypeList, eventTypeListElement, statusTypeList);
		NodeList eventTypeNodeList = eventTypeListElement.getElementsByTagName("eventType");

		for (int i = 0; i < eventTypeNodeList.getLength() ; i++)
		{
			Element eventTypeElement = (Element)eventTypeNodeList.item(i);
			String etid = eventTypeElement.getAttribute("id");
			if(etid != null && etid.length() > 0) {
				eventTypeList.get(i).setId(Long.parseLong(etid));

			}
		}
	}

	@Override
	protected void setStatusTypeListByXml(
			List<StatusTypeDomain> statusTypeList, Element statusTypeListElement)
			throws Exception {
		super.setStatusTypeListByXml(statusTypeList, statusTypeListElement);
		NodeList statusTypeNodeList = statusTypeListElement.getElementsByTagName("statusType");

		for (int i = 0; i < statusTypeNodeList.getLength() ; i++)
		{
			Element statusTypeElement = (Element)statusTypeNodeList.item(i);
			//statustype id
			String stid = statusTypeElement.getAttribute("id");
			if(stid != null && stid.length() > 0) {
				statusTypeList.get(i).setId(Long.parseLong(stid));

			}
		}
	}

	/**
	 * 「ワークフロー設定」オブジェクトに
	 * ・承認中のチェックイン可能なステータスタイプIDを設定する
	 * ・上長のみ表示デフォルト設定ステータスタイプIDを設定する
	 *
	 * @param workFlowDocDomain ワークフローオブジェクト
	 * @param statusTypeListElement クライアントから渡ってきたステータス情報
	 * @throws Exception ビジネスロジックが例外をスローした場合
	 */
	private void setStatusTypeSettingByXml(WorkFlowDocDomain workFlowDocDomain, Element statusTypeListElement) throws Exception {

		List<Long> enableCheckinStatusTypeList = new ArrayList<Long>();
		List<Long> defBossOnlyStatusTypeList = new ArrayList<Long>();
		// ステータスタイプ数ループ
		NodeList statusTypeNodeList = statusTypeListElement.getElementsByTagName("statusType");
		for (int i = 0; i < statusTypeNodeList.getLength(); i++) {
			Element statusTypeElement = (Element)statusTypeNodeList.item(i);
			String stid = statusTypeElement.getAttribute("id");
			// 承認中チェックイン可否フラグを抽出
			String enableCheckIn = statusTypeElement.getAttribute("enableCheckIn");
			// 上長のみ表示デフォルト設定を抽出
			String defBossOnly = statusTypeElement.getAttribute("defBossOnly");

			if (!StringUtils.isEmpty(stid)){
				// 承認中チェックイン可否フラグがONの場合
				if( Boolean.valueOf(enableCheckIn)) {
					enableCheckinStatusTypeList.add(Long.valueOf(stid));
				}
				// 上長のみ表示デフォルト設定がONの場合
				if( Boolean.valueOf(defBossOnly) ) {
					defBossOnlyStatusTypeList.add(Long.valueOf(stid));
				}
			}
		}
		// 承認依頼中チェックイン可否オプションがONの場合
		if( OptionConfData.getInstance().enableApproverCheckin ) {
			// 「ワークフロー設定」に「承認中チェックイン可能ステータスタイプのID」を設定する
			long[] enableCheckinStatusTypeArr = new long[enableCheckinStatusTypeList.size()];
			for (int i = 0; i < enableCheckinStatusTypeList.size(); i++) {
				enableCheckinStatusTypeArr[i] = enableCheckinStatusTypeList.get(i);
			}
			workFlowDocDomain.setEnableCheckinStatusTypeArr(enableCheckinStatusTypeArr);
		}

		// 「ワークフロー設定」に「上長のみ表示デフォルト設定ステータスタイプのID」を設定する
		long[] defBossOnlyStatusTypeArr = new long[defBossOnlyStatusTypeList.size()];
		for (int i = 0; i < defBossOnlyStatusTypeList.size(); i++) {
			defBossOnlyStatusTypeArr[i] = defBossOnlyStatusTypeList.get(i);
		}
		workFlowDocDomain.setDefBossOnlyStatusTypeArr(defBossOnlyStatusTypeArr);
	}


	/**
	 * イベントタイプドメイン設定(承認不要WF)
	 *
	 * EventTypeDomainのid, attrTypeListは本サービスの守備範囲外なのでセットしない。
	 * @param eventTypeList 格納先リスト
	 * @param eventTypeListElement 解析するXML（ワークフロー定義XMLのworkFlow.StatusTypeList部分）
	 * @param statusTypeList ステータスタイプリスト
	 * @throws Exception
	 */
	private WorkFlowDocDomain setEventTypeListDefaultByXmlUnnecessary(WorkFlowDocDomain workFlowDocDomain, Element statusTypeListElement, List<StatusTypeDomain> statusTypeList) throws Exception
	{
		//
		List<EventTypeDomain> eventTypeList = workFlowDocDomain.getEventTypeList();

		EventTypeDomain eventTypeDomain = null;
		StatusTypeDomain statusTypeDomainEditing = super.getStatusTypeDomainBySeq(statusTypeList, 1);
		StatusTypeDomain statusTypeDomainProcPublic = super.getStatusTypeDomainBySeq(statusTypeList, 2);
		StatusTypeDomain statusTypeDomainPublic = super.getStatusTypeDomainBySeq(statusTypeList, 3);

		int approvalSeq = 1;

		//--------------------------------------------------------------
		// ベースイベントタイプ：承認(PDF変換対象)
		// ステータスタイプ：編集中(1)→公開処理中(2)
		//--------------------------------------------------------------
		eventTypeDomain = noApproveSetDomainApprovePDF(approvalSeq, statusTypeDomainEditing, statusTypeDomainProcPublic);
		approvalSeq++;
		eventTypeList.add(eventTypeDomain);			//イベントタイプリストに追加

		//--------------------------------------------------------------
		// ベースイベントタイプ：承認(PDF変換対象外)
		// ステータスタイプ：編集中(1)→公開済(3)
		//--------------------------------------------------------------
		eventTypeDomain = noApproveSetDomainApprove(approvalSeq, statusTypeDomainEditing, statusTypeDomainPublic);
		approvalSeq++;
		eventTypeList.add(eventTypeDomain);			//イベントタイプリストに追加

		//===============================================
		// ベースイベントタイプ：取戻し
		// ステータスタイプ：公開処理中(2)→編集中(1)
		//===============================================
		eventTypeDomain = noApproveSetDomainTakeBack(statusTypeDomainProcPublic,statusTypeDomainEditing);
		eventTypeList.add(eventTypeDomain);			//イベントタイプリストに追加

		//===============================================
		// ベースイベントタイプ：公開
		// ステータスタイプ：公開処理中(2)→公開済(3)
		//===============================================
		eventTypeDomain = noApproveSetDomainPublic(statusTypeDomainProcPublic,statusTypeDomainPublic);
		eventTypeList.add(eventTypeDomain);			//イベントタイプリストに追加

		//===============================================
		// ベースイベントタイプ：公開取消
		// ステータスタイプ：公開済(3)→編集中(1)
		//===============================================
		eventTypeDomain = noApproveSetDomainPublicCancel(statusTypeDomainPublic,statusTypeDomainEditing);
		eventTypeList.add(eventTypeDomain);			//イベントタイプリストに追加

		return workFlowDocDomain;
	}

	// 承認不要WFイベントセット-承認(PDF変換対象)
	private EventTypeDomain noApproveSetDomainApprovePDF(int approvalSeq, StatusTypeDomain fromStatusType, StatusTypeDomain toStatusType) throws Exception
	{
		// 通知メールドメインリスト
		List<NoticeMailDomain> noticeMailDomainList;
		NoticeMailDomain noticeMailDomain;

		// 他言語名称ドメインリスト
		List<OtherNameDomain> otherNameDomainList;
		OtherNameDomain otherNameDomain;

		EventTypeDomain eventTypeDomain = new EventTypeDomain();

		// 通知メールドメインリスト
		noticeMailDomainList = new ArrayList<NoticeMailDomain>();

		// 他言語名称ドメインリスト
		otherNameDomainList = new ArrayList<OtherNameDomain>();
		otherNameDomain = new OtherNameDomain();
		otherNameDomain.setLangId("JA");
		otherNameDomain.setName("承認");
		otherNameDomainList.add(otherNameDomain);

		otherNameDomain = new OtherNameDomain();
		otherNameDomain.setLangId("EN");
		otherNameDomain.setName("Approval");
		otherNameDomainList.add(otherNameDomain);

		// イベントタイプドメイン設定
		eventTypeDomain = setEventTypeDomain(
				approvalSeq,												// イベント優先度
				fromStatusType,												// イベント始点ステータスタイプDomain
				toStatusType,												// イベント終点ステータスタイプDomain
				AppConstant.BASE_EVENT_TYPE_ID_APPROVAL,					// ベースイベントタイプID：承認
				AppConstant.GUARD_COND_ID_PDF_CONVERSION,					// ガード条件ID：PDF変換対象
				noticeMailDomainList,										// メールリスト
				otherNameDomainList,										// 言語別名称リスト
				setDefaultName(otherNameDomainList)							// デフォルト名称
		);
		return eventTypeDomain;
	}

	// 承認不要WFイベントセット-承認(PDF変換対象外)
	private EventTypeDomain noApproveSetDomainApprove(int approvalSeq, StatusTypeDomain fromStatusType, StatusTypeDomain toStatusType) throws Exception
	{
		// 通知メールドメインリスト
		List<NoticeMailDomain> noticeMailDomainList;
		NoticeMailDomain noticeMailDomain;

		// 他言語名称ドメインリスト
		List<OtherNameDomain> otherNameDomainList;
		OtherNameDomain otherNameDomain;

		EventTypeDomain eventTypeDomain = new EventTypeDomain();

		// 通知メールドメインリスト
		noticeMailDomainList = new ArrayList<NoticeMailDomain>();
		noticeMailDomain = new NoticeMailDomain();
		noticeMailDomain.getMailType().setId(-12005);			// メール種別ID:公開通知
		noticeMailDomain.setMailMethod(MailMethod.SELECTABLE);	// 通知方法
		noticeMailDomainList.add(noticeMailDomain);

		// 他言語名称ドメインリスト
		otherNameDomainList = new ArrayList<OtherNameDomain>();
		otherNameDomain = new OtherNameDomain();
		otherNameDomain.setLangId("JA");
		otherNameDomain.setName("承認");
		otherNameDomainList.add(otherNameDomain);

		otherNameDomain = new OtherNameDomain();
		otherNameDomain.setLangId("EN");
		otherNameDomain.setName("Approval");
		otherNameDomainList.add(otherNameDomain);

		// イベントタイプドメイン設定
		eventTypeDomain = setEventTypeDomain(
				approvalSeq,												// イベント優先度
				fromStatusType,												// イベント始点ステータスタイプDomain
				toStatusType,												// イベント終点ステータスタイプDomain
				AppConstant.BASE_EVENT_TYPE_ID_APPROVAL,					// ベースイベントタイプID:承認
				AppConstant.GUARD_COND_ID_NON_PDF_CONVERSION,				// ガード条件ID:PDF変換対象外
				noticeMailDomainList,										// メールリスト
				otherNameDomainList,										// 言語別名称リスト
				setDefaultName(otherNameDomainList)							// デフォルト名称
		);
		return eventTypeDomain;
	}

	// 承認不要WFイベントセット-取戻し
	private EventTypeDomain noApproveSetDomainTakeBack(StatusTypeDomain fromStatusType, StatusTypeDomain toStatusType) throws Exception
	{
		// 通知メールドメインリスト
		List<NoticeMailDomain> noticeMailDomainList;

		// 他言語名称ドメインリスト
		List<OtherNameDomain> otherNameDomainList;
		OtherNameDomain otherNameDomain;

		EventTypeDomain eventTypeDomain = new EventTypeDomain();

		// 通知メールドメインリスト
		noticeMailDomainList = new ArrayList<NoticeMailDomain>();

		// 他言語名称ドメインリスト
		otherNameDomainList = new ArrayList<OtherNameDomain>();
		otherNameDomain = new OtherNameDomain();
		otherNameDomain.setLangId("JA");
		otherNameDomain.setName("取戻し");
		otherNameDomainList.add(otherNameDomain);

		otherNameDomain = new OtherNameDomain();
		otherNameDomain.setLangId("EN");
		otherNameDomain.setName("Take back");
		otherNameDomainList.add(otherNameDomain);

		// イベントタイプドメイン設定
		eventTypeDomain = setEventTypeDomain(
				1,															// イベント優先度
				fromStatusType,												// イベント始点ステータスタイプDomain
				toStatusType,												// イベント終点ステータスタイプDomain
				AppConstant.BASE_EVENT_TYPE_ID_TAKE_BACK,					// ベースイベントタイプID：取戻し
				AppConstant.GUARD_COND_ID_NOHTING,							// ガード条件ID
				noticeMailDomainList,										// メールリスト
				otherNameDomainList,										// 言語別名称リスト
				setDefaultName(otherNameDomainList)							// デフォルト名称
		);

		return eventTypeDomain;
	}

	// 承認不要WFイベントセット-公開
	private EventTypeDomain noApproveSetDomainPublic(StatusTypeDomain fromStatusType, StatusTypeDomain toStatusType) throws Exception
	{
		// 通知メールドメインリスト
		List<NoticeMailDomain> noticeMailDomainList;
		NoticeMailDomain noticeMailDomain;

		// 他言語名称ドメインリスト
		List<OtherNameDomain> otherNameDomainList;
		OtherNameDomain otherNameDomain;

		EventTypeDomain eventTypeDomain = new EventTypeDomain();

		// 通知メールドメインリスト
		noticeMailDomainList = new ArrayList<NoticeMailDomain>();
		noticeMailDomain = new NoticeMailDomain();
		noticeMailDomain.getMailType().setId(-12005);			// メール種別ID：公開通知
		noticeMailDomain.setMailMethod(MailMethod.SELECTABLE);	// 通知方法
		noticeMailDomainList.add(noticeMailDomain);

		// 他言語名称ドメインリスト
		otherNameDomainList = new ArrayList<OtherNameDomain>();
		otherNameDomain = new OtherNameDomain();
		otherNameDomain.setLangId("JA");
		otherNameDomain.setName("公開");
		otherNameDomainList.add(otherNameDomain);

		otherNameDomain = new OtherNameDomain();
		otherNameDomain.setLangId("EN");
		otherNameDomain.setName("Public");
		otherNameDomainList.add(otherNameDomain);

		// イベントタイプドメイン設定
		eventTypeDomain = setEventTypeDomain(
				1,															// イベント優先度
				fromStatusType,												// イベント始点ステータスタイプDomain
				toStatusType,												// イベント終点ステータスタイプDomain
				AppConstant.BASE_EVENT_TYPE_ID_PUBLIC,						// ベースイベントタイプID:公開
				AppConstant.GUARD_COND_ID_NOHTING,							// ガード条件ID:なし
				noticeMailDomainList,										// メールリスト
				otherNameDomainList,										// 言語別名称リスト
				setDefaultName(otherNameDomainList)							// デフォルト名称
		);
		return eventTypeDomain;
	}

	// 承認不要WFイベントセット-公開取消
	private EventTypeDomain noApproveSetDomainPublicCancel(StatusTypeDomain fromStatusType, StatusTypeDomain toStatusType) throws Exception
	{
		// 通知メールドメインリスト
		List<NoticeMailDomain> noticeMailDomainList;

		// 他言語名称ドメインリスト
		List<OtherNameDomain> otherNameDomainList;
		OtherNameDomain otherNameDomain;

		EventTypeDomain eventTypeDomain = new EventTypeDomain();

		// 通知メールドメインリスト
		noticeMailDomainList = new ArrayList<NoticeMailDomain>();
		NoticeMailDomain noticeMailDomain = new NoticeMailDomain();
		noticeMailDomain.getMailType().setId(-12006);			// メール種別ID：公開取消通知
		noticeMailDomain.setMailMethod(MailMethod.IMMEDIATE);	// 通知方法：即時
		noticeMailDomainList.add(noticeMailDomain);

		// 他言語名称ドメインリスト
		otherNameDomainList = new ArrayList<OtherNameDomain>();
		otherNameDomain = new OtherNameDomain();
		otherNameDomain.setLangId("JA");
		otherNameDomain.setName("公開取消");
		otherNameDomainList.add(otherNameDomain);

		otherNameDomain = new OtherNameDomain();
		otherNameDomain.setLangId("EN");
		otherNameDomain.setName("Public cancel");
		otherNameDomainList.add(otherNameDomain);

		// イベントタイプドメイン設定
		eventTypeDomain = setEventTypeDomain(
				1,															// イベント優先度
				fromStatusType,												// イベント始点ステータスタイプDomain
				toStatusType,												// イベント終点ステータスタイプDomain
				AppConstant.BASE_EVENT_TYPE_ID_PUBLIC_CANCEL,					// ベースイベントタイプID：公開取消
				AppConstant.GUARD_COND_ID_NOHTING,							// ガード条件ID
				noticeMailDomainList,										// メールリスト
				otherNameDomainList,										// 言語別名称リスト
				setDefaultName(otherNameDomainList)							// デフォルト名称
		);

		return eventTypeDomain;
	}
}
