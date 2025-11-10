package jp.co.ctc_g.eim.common.presentation.web.controller;

import java.io.StringReader;
import java.util.List;

import org.apache.xerces.parsers.DOMParser;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

import jp.co.ctc_g.eim.common.presentation.web.dto.MailTypeListDTO;
import jp.co.ctc_g.eim.common.presentation.web.dto.WorkFlowConfDTO;
import jp.co.ctc_g.eim.framework.business.domain.AssignEntryDomain;
import jp.co.ctc_g.eim.framework.business.domain.BelongDomain;
import jp.co.ctc_g.eim.framework.business.domain.BelongType;
import jp.co.ctc_g.eim.framework.business.domain.EntryDomain;
import jp.co.ctc_g.eim.framework.business.domain.MailMethod;
import jp.co.ctc_g.eim.framework.business.domain.MailTypeDomain;
import jp.co.ctc_g.eim.framework.business.domain.NoticeMailDomain;
import jp.co.ctc_g.eim.framework.business.domain.WorkFlowConfDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.EventTypeDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.OtherNameDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.StatusTypeDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.WorkFlowDomain;
import jp.co.ctc_g.eim.framework.business.service.MailTypeService;
import jp.co.ctc_g.eim.framework.business.service.WorkFlowConfService;
import jp.co.ctc_g.eim.framework.common.exception.EIMSysException;
import jp.co.ctc_g.eim.framework.common.util.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.presentation.web.controller.RestController;

/**
 * ワークフロー定義アクション
 *
 */
@Controller
@RequestMapping({ "/rest/common/workflow" })
public class WorkFlowDefController extends RestController {

	/**
	 * ワークフロー名称（デフォルト）を取得するキー名称
	 */
	private static final String DEF_NAME_TYPE = "JA";

	/**
	 * ワークフロー関係の設定一覧を取得します。<br>
	 * <ul>
	 * <li>ステータスタイプ種別一覧</li>
	 * <li>ベースイベントタイプ一覧</li>
	 * <li>ガード条件一覧</li>
	 * </ul>
	 *
	 * @return WorkFlowConfDTOを返送する
	 * @throws Exception ビジネスロジックが例外をスローした場合
	 */
	@RequestMapping(value = "/getWorkFlowConfList", method=RequestMethod.GET)
	@ResponseBody
	public WorkFlowConfDTO getWorkFlowConfList() throws Exception {

		// Serviceクラスのインスタンス化
		ApplicationContext contxt = ApplicationContextLoader.getContext();
		WorkFlowConfService workFlowConfService = (WorkFlowConfService)contxt.getBean("workFlowConfService");

		// ワークフロー関係の設定一覧取得
		WorkFlowConfDomain workFlowConfDomain = workFlowConfService.get();

		WorkFlowConfDTO dto = new WorkFlowConfDTO(workFlowConfDomain);
		
		return dto;
	}



	/**
	 * メール種別一覧を取得します。<br>
	 * <ul>
	 * <li>メール種別一覧</li>
	 * </ul>
	 *
	 * @return MailTypeListDTOを返送する
	 * @throws Exception ビジネスロジックが例外をスローした場合
	 */
	@RequestMapping(value = "/getMailTypeList", method=RequestMethod.GET)
	@ResponseBody
	public MailTypeListDTO getMailTypeList() throws Exception {

		ApplicationContext contxt = ApplicationContextLoader.getContext();

		// サービスの取得
		MailTypeService mailTypeService = (MailTypeService)contxt.getBean("mailTypeService");

		// メール種別の一覧を取得
		List<MailTypeDomain> mailTypeList = mailTypeService.get();

		MailTypeListDTO dto = new MailTypeListDTO(mailTypeList);

		return dto;
	}

	/*================================================================================
	// 以下、XMLからDomainを生成するために使用するprivateメソッドを定義する。
	/*================================================================================

	/**
	 * ワークフロー定義のXML文字列からワークフロー定義ドメインを取得します。
	 * @param workFlowId ワークフロー定義が対応するワークフローのID
	 * @param xml ワークフロー定義のXML文字列
	 * @return ワークフロー定義ドメイン
	 */
	protected WorkFlowDomain xmlToDomain(long workFlowId, String xml) throws Exception
	{
		//return用
		WorkFlowDomain workFlowDefDomain = new WorkFlowDomain();

		//入力されたXMLからDOMを生成、ルート要素(workFlow)を取得
		DOMParser parser = new DOMParser();
		//脆弱性対応のため、外部実態参照を無効化
		parser.setFeature("http://xml.org/sax/features/external-parameter-entities" ,false);
		parser.parse(new InputSource(new StringReader(xml)));
		Document document = parser.getDocument();
		Element rootElement = document.getDocumentElement();

		/*-------------------------------------------------------
		 * 入力された情報をworkFlowDefDomainに詰め替える。
		 * workFlowDefDomainのプロパティdefName/nameListは本サービスの
		 * 守備範囲外なのでセットしない。
		 * （workFlowDefDomainのid, statusTypeList, eventTypeListのみセットする。）
		 *------------------------------------------------------*/

		workFlowDefDomain.setId(workFlowId);

		Element nameListElement = (Element)rootElement.getElementsByTagName("nameList").item(0);
		setNameListByXml(workFlowDefDomain.getNameList(), nameListElement);

		String defName = getDefNameByNameList(nameListElement);
		workFlowDefDomain.setDefName(defName);

		Element statusTypeListElement = (Element)rootElement.getElementsByTagName("statusTypeList").item(0);
		setStatusTypeListByXml(workFlowDefDomain.getStatusTypeList(), statusTypeListElement);

		Element eventTypeListElement = (Element)rootElement.getElementsByTagName("eventTypeList").item(0);
		setEventTypeListByXml(workFlowDefDomain.getEventTypeList(), eventTypeListElement, workFlowDefDomain.getStatusTypeList());

		return workFlowDefDomain;
	}

	/**
	 * イベントタイプ定義のXMLから取得した情報をイベントタイプドメインのリストに格納する。
	 * EventTypeDomainのid, attrTypeListは本サービスの守備範囲外なのでセットしない。
	 * @param eventTypeList 格納先リスト
	 * @param eventTypeListElement 解析するXML（ワークフロー定義XMLのworkFlow.EventTypeList部分）
	 * @param statusTypeList 情報格納済みのステータスタイプドメインのリスト（生成するEventTypeDomainのfromStatusType/toStatusTypeとして使用する）
	 * @throws Exception
	 */
	protected void setEventTypeListByXml(List<EventTypeDomain> eventTypeList, Element eventTypeListElement, List<StatusTypeDomain> statusTypeList) throws Exception
	{
		NodeList eventTypeNodeList = eventTypeListElement.getElementsByTagName("eventType");

		for (int i = 0; i < eventTypeNodeList.getLength() ; i++)
		{
			Element eventTypeElement = (Element)eventTypeNodeList.item(i);
			EventTypeDomain eventTypeDomain = new EventTypeDomain();

			//seq
			int seq = Integer.parseInt(eventTypeElement.getAttribute("seq"));
			eventTypeDomain.setSeq(seq);

			//fromStatusType（statusTypeList内のStatusTypeDomainインスタンスへの参照としてセットする）
			int fromStatusTypeSeq = Integer.parseInt(eventTypeElement.getAttribute("fromStatusTypeSeq"));
			StatusTypeDomain fromStatusTypeDomain = getStatusTypeDomainBySeq(statusTypeList, fromStatusTypeSeq);
			eventTypeDomain.setFromStatusType(fromStatusTypeDomain);

			//toStatusType（statusTypeList内のStatusTypeDomainインスタンスへの参照としてセットする）
			int toStatusTypeSeq = Integer.parseInt(eventTypeElement.getAttribute("toStatusTypeSeq"));
			StatusTypeDomain toStatusTypeDomain = getStatusTypeDomainBySeq(statusTypeList, toStatusTypeSeq);
			eventTypeDomain.setToStatusType(toStatusTypeDomain);

			//baseEventType(Domain)
			long baseEventTypeId = Long.parseLong(eventTypeElement.getAttribute("baseEventTypeId"));
			eventTypeDomain.getBaseEventType().setId(baseEventTypeId);

			//guardCondition(Domain)
			long guardConditionId = Long.parseLong(eventTypeElement.getAttribute("guardConditionId"));
			eventTypeDomain.getGuardCondition().setId(guardConditionId);

			//mailList(DomainのList)
			Element mailListElement = (Element)eventTypeElement.getElementsByTagName("mailList").item(0);
			setMailListByXml(eventTypeDomain.getMailList(), mailListElement);

			//nameList
			Element nameListElement = (Element)eventTypeElement.getElementsByTagName("nameList").item(0);
			setNameListByXml(eventTypeDomain.getNameList(), nameListElement);

			// defName
			String defName = getDefNameByNameList(nameListElement);
			eventTypeDomain.setDefName(defName);

			//リストに追加
			eventTypeList.add(eventTypeDomain);
		}

	}

	/**
	 * ステータスタイプリストから指定されたシーケンス番号のステータスタイプを取得します。
	 * @param statusTypeList ステータスタイプリスト
	 * @param seq シーケンス番号
	 * @return 取得したステータスタイプ
	 * @throws EIMSysException シーケンス番号が一致するステータスタイプが見つからなかった。
	 */
	protected StatusTypeDomain getStatusTypeDomainBySeq(List<StatusTypeDomain> statusTypeList, int seq) throws EIMSysException
	{
		for (int i=0; i<statusTypeList.size(); i++)
		{
			StatusTypeDomain statusTypeDomain = statusTypeList.get(i);
			if (statusTypeDomain.getSeq() == seq)
			{
				return statusTypeDomain;
			}
		}
		throw new EIMSysException(EIMThreadContext.getEIMSession(), "EIM.ERROR.LOGIC.STATUSTYPE.NOTFOUND");
	}

	/**
	 * ステータスタイプ定義のXMLから取得した情報をステータスタイプドメインのリストに格納する。
	 * StatusTypeDomainのid, attrTypeListは本サービスの守備範囲外なのでセットしない。
	 * @param statusTypeList 格納先リスト
	 * @param statusTypeListElement 解析するXML（ワークフロー定義XMLのworkFlow.StatusTypeList部分）
	 * @throws Exception
	 */
	protected void setStatusTypeListByXml(List<StatusTypeDomain> statusTypeList, Element statusTypeListElement) throws Exception
	{
		NodeList statusTypeNodeList = statusTypeListElement.getElementsByTagName("statusType");

		for (int i = 0; i < statusTypeNodeList.getLength() ; i++)
		{
			Element statusTypeElement = (Element)statusTypeNodeList.item(i);
			StatusTypeDomain statusTypeDomain = new StatusTypeDomain();

			//seq
			int seq = Integer.parseInt(statusTypeElement.getAttribute("seq"));
			statusTypeDomain.setSeq(seq);

			//auto
			boolean auto = Boolean.parseBoolean(statusTypeElement.getAttribute("auto"));
			statusTypeDomain.setAuto(auto);

			//statusTypeKind（idのみセットする）
			long kindId = Long.parseLong(statusTypeElement.getAttribute("kind"));
			statusTypeDomain.getStatusTypeKind().setId(kindId);

			//assignEntry
			Element asEntryListElement = (Element)statusTypeElement.getElementsByTagName("asEntryList").item(0);
			setAssignEntryByXml(statusTypeDomain.getAssignEntry(), asEntryListElement);

			//nameList
			Element nameListElement = (Element)statusTypeElement.getElementsByTagName("nameList").item(0);
			setNameListByXml(statusTypeDomain.getNameList(), nameListElement);

			// defName
			String defName = getDefNameByNameList(nameListElement);
			statusTypeDomain.setDefName(defName);

			//リストに追加
			statusTypeList.add(statusTypeDomain);
		}

	}

	/**
	 * アサイン先エントリー定義のXMLから取得した情報をアサイン先エントリードメインのリストに格納する。
	 * @param assignEntryDomain 格納先
	 * @param assignEntryListElement 解析するXML（ワークフロー定義XMLのworkFlow.StatusTypeList.asEntryList部分）
	 * @throws Exception
	 */
	protected void setAssignEntryByXml(AssignEntryDomain assignEntryDomain, Element assignEntryListElement) throws Exception
	{
		NodeList asEntryNodeList = assignEntryListElement.getElementsByTagName("asEntry");
		BelongDomain belong;
		EntryDomain belonging;

		for (int i=0; i<asEntryNodeList.getLength(); i++)
		{
			Element asEntryElement = (Element)asEntryNodeList.item(i);
			long id = Long.parseLong(asEntryElement.getAttribute("id"));
			String type = asEntryElement.getAttribute("type");

			if (type.equals("user")) {
				// 新ソース
				belong = new BelongDomain();
				belonging = new EntryDomain();
				belonging.setId(id);
				belong.setBelonging(belonging);
				belong.setBelongType(BelongType.USER);
				assignEntryDomain.getBelongList().add(belong);
			}
			else if (type.equals("group")) {
				// 新ソース
				belong = new BelongDomain();
				belonging = new EntryDomain();
				belonging.setId(id);
				belong.setBelonging(belonging);
				belong.setBelongType(BelongType.GROUP);
				assignEntryDomain.getBelongList().add(belong);
			}
			else if (type.equals("role")) {
				// 新ソース
				belong = new BelongDomain();
				belonging = new EntryDomain();
				belonging.setId(id);
				belong.setBelonging(belonging);
				belong.setBelongType(BelongType.ROLE);
				assignEntryDomain.getBelongList().add(belong);

			}
			else if (type.equals("compGroup")) {
				// 新ソース
				belong = new BelongDomain();
				belonging = new EntryDomain();
				belonging.setId(id);
				belong.setBelonging(belonging);
				belong.setBelongType(BelongType.COMPGROUP);
				assignEntryDomain.getBelongList().add(belong);

			}
			else if (type.equals("userDefGroup")) {
				// 新ソース
				belong = new BelongDomain();
				belonging = new EntryDomain();
				belonging.setId(id);
				belong.setBelonging(belonging);
				belong.setBelongType(BelongType.USERDEFGROUP);
				assignEntryDomain.getBelongList().add(belong);

			}
			else if (type.equals("sysFunc")) {
				// 新ソース
				belong = new BelongDomain();
				belonging = new EntryDomain();
				belonging.setId(id);
				belong.setBelonging(belonging);
				belong.setBelongType(BelongType.SYSFUNC);
				assignEntryDomain.getBelongList().add(belong);

			}
			else if (type.equals("objectRole")) {
				// 新ソース
				belong = new BelongDomain();
				belonging = new EntryDomain();
				belonging.setId(id);
				belong.setBelonging(belonging);
				belong.setBelongType(BelongType.OBJECTROLE);
				assignEntryDomain.getBelongList().add(belong);

			}
		}
	}

	/**
	 * 他言語名称定義のXMLから取得した情報を他言語名称ドメインのリストに格納する。
	 * @param langLabelDomainList 格納先
	 * @param nameListElement 解析するXML（ワークフロー定義XMLのworkFlow.StatusTypeList.nameList部分）
	 * @throws Exception
	 */
	protected void setNameListByXml(List<OtherNameDomain> langLabelDomainList, Element nameListElement) throws Exception
	{
		NodeList nameNodeList = nameListElement.getElementsByTagName("name");
		for (int i=0; i<nameNodeList.getLength(); i++)
		{
			Element nameElement = (Element)nameNodeList.item(i);
			OtherNameDomain otherNameDomain = new OtherNameDomain();

			//lang
			String lang = nameElement.getAttribute("lang");
			otherNameDomain.setLangId(lang);

			//name
			String value = nameElement.getAttribute("value");
			otherNameDomain.setName(value);

			//リストに追加
			langLabelDomainList.add(otherNameDomain);
		}
	}

	/**
	 * 通知メール定義のXMLから取得した情報を通知メールドメインのリストに格納する。
	 * @param noticeMailDomainList 知メールドメインのリスト
	 * @param mailListElement 通知メール定義のXML
	 * @throws Exception
	 */
	protected void setMailListByXml(List<NoticeMailDomain> noticeMailDomainList, Element mailListElement) throws Exception
	{
		NodeList mailNodeList = mailListElement.getElementsByTagName("mail");
		for (int i=0; i<mailNodeList.getLength(); i++)
		{
			Element mailElement = (Element)mailNodeList.item(i);
			NoticeMailDomain noticeMailDomain = new NoticeMailDomain();

			//mailType.id
			long mailTypeId = Long.parseLong(mailElement.getAttribute("id"));
			noticeMailDomain.getMailType().setId(mailTypeId);

			//method
			String method = mailElement.getAttribute("method");
			if (method.equals("immediate")) {
				noticeMailDomain.setMailMethod(MailMethod.IMMEDIATE);
			} else if (method.equals("accumulate")) {
				noticeMailDomain.setMailMethod(MailMethod.ACCUMULATE);
			} else {
				noticeMailDomain.setMailMethod(MailMethod.SELECTABLE);
			}

			//リストに追加
			noticeMailDomainList.add(noticeMailDomain);
		}

	}


	/**
	 * 他言語名称定義のXMLから取得した情報からデフォルト名称を取得する。
	 * @param nameListElement 解析するXML（ワークフロー定義XMLのworkFlow.StatusTypeList.nameList部分）
	 * @return 他言語名称定義のXMLからJAをキーに取得した値
	 * @throws Exception
	 */
	protected String getDefNameByNameList(Element nameListElement) throws Exception
	{
		String defName = "";

		NodeList nameNodeList = nameListElement.getElementsByTagName("name");
		for (int i=0; i<nameNodeList.getLength(); i++)
		{
			Element nameElement = (Element)nameNodeList.item(i);
			//lang
			String lang = nameElement.getAttribute("lang");

			// langがJAの場合、デフォルト名称を取得する。
			if (lang != null && lang.equals(DEF_NAME_TYPE)) {
				//name
				defName = nameElement.getAttribute("value");
			}
		}

		return defName;
	}

}
