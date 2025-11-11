package jp.co.ctc_g.eim.admin.presentation.web.controller;

import java.io.StringReader;
import java.util.List;
import java.util.Map;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

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
import eim.net.EIMSession;
import eim.util.EIMConfig;
import jp.co.ctc_g.eim.admin.business.domain.WorkflowAdminDomain;
import jp.co.ctc_g.eim.admin.business.service.WorkFlowDefAdminService;
import jp.co.ctc_g.eim.app.form.business.domain.AttributeTypeLayoutDomain;
import jp.co.ctc_g.eim.app.form.business.domain.StatusTypeLayoutDomain;
import jp.co.ctc_g.eim.common.presentation.web.controller.WorkFlowDefController;
import jp.co.ctc_g.eim.common.presentation.web.dto.WorkFlowDefDTO;
import jp.co.ctc_g.eim.framework.business.domain.entity.AttributeTypeDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.StatusTypeDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.WorkFlowDomain;
import jp.co.ctc_g.eim.framework.common.util.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * ワークフロー定義　コントローラ
 *
 */
@Controller
@RequestMapping({ "/rest/admin/workflow" })
public class WorkFlowDefAdminController extends WorkFlowDefController {

	/**
	 * @see jp.co.ctc_g.eim.framework.presentation.web.action.WorkFlowDefAction#getWorkFlowDefById(ActionMapping,HttpServletRequest,HttpServletResponse)
	 * @since Ver6.0
	 */
	@RequestMapping(value = "/getWorkFlowDefAdminById", method=RequestMethod.POST)
	@ResponseBody
	public WorkFlowDefDTO getWorkFlowDefById(@RequestBody MultiValueMap<String,String> form) throws Exception {

		// formより、ワークフローIDを取得
		String strWorkFlowId = form.getFirst("id");
		long workFlowId = Long.valueOf(strWorkFlowId);

		//Session
		EIMSession sess = EIMThreadContext.getEIMSession();
		String adminAppId = (String)sess.getAttribute("ADMIN_APP_ID");
		String namespace = (String)sess.getAttribute("ADMIN_NAMESPACE");
		
		// Serviceクラスのインスタンス化
		ApplicationContext contxt = ApplicationContextLoader.getContext();
		WorkFlowDefAdminService workFlowDefAdminService = 
			(WorkFlowDefAdminService)contxt.getBean("workFlowDefAdminService");
		
		// ワークフロー定義取得
		WorkflowAdminDomain workflowAdmin = workFlowDefAdminService.getDefById(adminAppId, namespace, workFlowId);

		// 表示する定義名称
		String wfDefName = "";
		// 汎用の場合
		if (adminAppId.equals(AppConstant.ADMIN_APP_ID_GENERAL)) {
			// ネームスペース括弧付の定義名称
			wfDefName = NamespaceUtil.getDefNameWithNamespaceParentheses(
					workflowAdmin.getName(), workflowAdmin.getDefName());
		} else {
			// ネームスペース無しの定義名称
			wfDefName = NamespaceUtil.getDefNamenWhichExceptedNamespace(workflowAdmin.getDefName());
		}
		
		WorkFlowDefDTO dto = new WorkFlowDefDTO(workflowAdmin, wfDefName);
		
		return dto;
	}
	
	/**
	 * @see jp.co.ctc_g.eim.framework.presentation.web.action.WorkFlowDefAction#updateWorkFlowDef(ActionMapping,HttpServletRequest,HttpServletResponse)
	 * @since Ver6.0
	 */
	@RequestMapping(value = "/updateWorkFlowDefAdmin", method=RequestMethod.POST)
	@ResponseBody
	public WorkflowAdminDomain updateWorkFlowDef(@RequestBody MultiValueMap<String,String> form) throws Exception {

		// formより、ワークフローID、ワークフロードメインを取得
		String strWorkFlowId = form.getFirst("workFlowId");
		long workFlowId = Long.valueOf(strWorkFlowId);

		//Session
		EIMSession sess = EIMThreadContext.getEIMSession();
		String adminAppId = (String)sess.getAttribute("ADMIN_APP_ID");
		
		String workFlowXml = form.getFirst("workFlowXml");

		WorkflowAdminDomain workflowAdminDomain = xmlToDomain(workFlowId, workFlowXml, true);

		// Serviceクラスのインスタンス化
		ApplicationContext contxt = ApplicationContextLoader.getContext();
		WorkFlowDefAdminService workFlowDefAdminService = 
			(WorkFlowDefAdminService)contxt.getBean("workFlowDefAdminService");

		// ワークフロー定義の更新
		workFlowDefAdminService.updateDef(adminAppId, workflowAdminDomain);

		// フォワード
		return workflowAdminDomain;
	}

	/**
	 * @see jp.co.ctc_g.eim.framework.presentation.web.action.WorkFlowDefAction#copyWorkFlowDef(ActionMapping,HttpServletRequest,HttpServletResponse)
	 * @since Ver6.0
	 */
	@RequestMapping(value = "/copyWorkFlowDefAdmin", method=RequestMethod.POST)
	@ResponseBody
	public WorkFlowDefDTO copyWorkFlowDef(@RequestBody MultiValueMap<String,String> form) throws Exception {

		// formよりワークフローID、ワークフロードメインを取得
		String strWorkFlowId = form.getFirst("workFlowId");
		long workFlowId = Long.valueOf(strWorkFlowId);
		String workFlowXml = form.getFirst("workFlowXml");

		//Session
		EIMSession sess = EIMThreadContext.getEIMSession();
		String adminAppId = (String)sess.getAttribute("ADMIN_APP_ID");
		
		WorkflowAdminDomain workFlowAdminDomain = xmlToDomain(workFlowId, workFlowXml, false);

		// ネームスペース付き定義名称設定用の一時ドメイン
		WorkFlowDomain tmpWorkFlowDomain = new WorkFlowDomain();
		tmpWorkFlowDomain.setDefName(workFlowAdminDomain.getDefName());
		
		// ワークフローの定義名称にネームスペースを設定する。
		setNamespaceToWorkflowDomain(tmpWorkFlowDomain, workFlowXml);
		
		// 設定結果(ネームスペース付き定義名称)を引数ドメインに反映
		workFlowAdminDomain.setDefName(tmpWorkFlowDomain.getDefName());
		
		// Serviceクラスのインスタンス化
		ApplicationContext contxt = ApplicationContextLoader.getContext();
		WorkFlowDefAdminService workFlowDefAdminService = (WorkFlowDefAdminService)contxt.getBean("workFlowDefAdminService");

		// ワークフロー定義の複写
		WorkflowAdminDomain workflowAdmin = workFlowDefAdminService.copyDef(adminAppId, workFlowAdminDomain);
		
		// 表示する定義名称
		String wfDefName = "";
		// 汎用の場合
		if (adminAppId.equals(AppConstant.ADMIN_APP_ID_GENERAL)) {
			// ネームスペース括弧付の定義名称
			wfDefName = NamespaceUtil.getDefNameWithNamespaceParentheses(
					workflowAdmin.getName(), workflowAdmin.getDefName());
		} else {
			// ネームスペース無しの定義名称
			wfDefName = NamespaceUtil.getDefNamenWhichExceptedNamespace(workflowAdmin.getDefName());
		}
		
		WorkFlowDefDTO dto = new WorkFlowDefDTO(workflowAdmin, wfDefName);
		return dto;
	}
	
	/**
	 * ワークフロー定義のXML文字列からワークフロー定義ドメインを取得します。
	 * @param workFlowId ワークフロー定義が対応するワークフローのID
	 * @param xml ワークフロー定義のXML文字列
	 * @param updateFlag ワークフロー定義更新かどうかのフラグ
	 * @return ワークフロー定義ドメイン
	 * @param releaseAttTypeMap 解除属性タイプドメインリストのMap
	 */
	protected WorkflowAdminDomain xmlToDomain(long workFlowId, String xml, boolean updateFlag) throws Exception
	{
		//return用
		WorkflowAdminDomain workFlowAdminDomain = new WorkflowAdminDomain();

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

		workFlowAdminDomain.setId(workFlowId);

		Element nameListElement = (Element)rootElement.getElementsByTagName("nameList").item(0);
		if (nameListElement != null) {
			setNameListByXml(workFlowAdminDomain.getNameList(), nameListElement);
			String defName = getDefNameByNameList(nameListElement);
			workFlowAdminDomain.setDefName(defName);
		}

		Element statusTypeListElement = (Element)rootElement.getElementsByTagName("statusTypeList").item(0);
		setStatusTypeListByXml(workFlowAdminDomain.getStatusTypeList(), statusTypeListElement, updateFlag);
		
		// ワークフロー定義を更新する場合(※複写時はDB情報を複写するため設定不要)
		if (updateFlag) {
			setStatusTypeLayoutMapByXml(workFlowAdminDomain.getStatusTypeLayoutMap(), statusTypeListElement);
		}

		Element eventTypeListElement = (Element)rootElement.getElementsByTagName("eventTypeList").item(0);
		setEventTypeListByXml(workFlowAdminDomain.getEventTypeList(), eventTypeListElement, workFlowAdminDomain.getStatusTypeList());

		return workFlowAdminDomain;
	}
	
	/**
	 * ステータスタイプ定義のXMLから取得した情報をステータスタイプドメインのリストに格納する。
	 * StatusTypeDomainのid, attrTypeListは本サービスの守備範囲外なのでセットしない。
	 * @param statusTypeList 格納先リスト
	 * @param statusTypeListElement 解析するXML（ワークフロー定義XMLのworkFlow.StatusTypeList部分）
	 * @param updateFlag ワークフロー定義更新かどうかのフラグ
	 * @throws Exception
	 */
	protected void setStatusTypeListByXml(
			List<StatusTypeDomain> statusTypeList, Element statusTypeListElement, boolean updateFlag) throws Exception
	{
		NodeList statusTypeNodeList = statusTypeListElement.getElementsByTagName("statusType");

		for (int i = 0; i < statusTypeNodeList.getLength() ; i++)
		{
			Element statusTypeElement = (Element)statusTypeNodeList.item(i);
			StatusTypeDomain statusTypeDomain = new StatusTypeDomain();

			//seq
			int seq = Integer.parseInt(statusTypeElement.getAttribute("seq"));
			statusTypeDomain.setSeq(seq);

			//id
			long id = Long.parseLong(statusTypeElement.getAttribute("id"));
			statusTypeDomain.setId(id);

			//auto
			boolean auto = Boolean.parseBoolean(statusTypeElement.getAttribute("auto"));
			statusTypeDomain.setAuto(auto);

			//statusTypeKind
			long kindId = Long.parseLong(statusTypeElement.getAttribute("kind"));
			statusTypeDomain.getStatusTypeKind().setId(kindId);
			
			// ワークフロー定義を更新する場合(※複写時はDB情報を複写するため設定不要)
			if (updateFlag) {
				//attributeTypeList
				Element attTypeListElement = (Element)statusTypeElement.getElementsByTagName("attTypes").item(0);
				setAttTypeListByXml(statusTypeDomain.getAttrTypeList(), attTypeListElement);
			}
			
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
	 * ワークフロー定義の定義名称にネームスペースを設定する。
	 * @param workflowDomain XML文字列から形成済みのワークフロー定義ドメイン
	 * @param xml ワークフロー定義のXML文字列
	 * @throws Exception
	 */
	protected void setNamespaceToWorkflowDomain(WorkFlowDomain workflowDomain, String xml) throws Exception
	{
		//入力されたXMLからDOMを生成、ルート要素(workFlow)を取得
		DOMParser parser = new DOMParser();
		//脆弱性対応のため、外部実態参照を無効化
		parser.setFeature("http://xml.org/sax/features/external-parameter-entities" ,false);
		parser.parse(new InputSource(new StringReader(xml)));
		Document document = parser.getDocument();
		Element rootElement = document.getDocumentElement();

		//ワークフローにネームスペースをセットする。
		String namespace = rootElement.getAttribute("namespace");
		if (namespace != null && !namespace.equals(""))
		{
			workflowDomain.setDefName(namespace + EIMConfig.get("NAMESPACE_DIVID_CHAR") + workflowDomain.getDefName());
		}
	}
	
	/**
	 * 適用属性タイプ定義のXMLから取得した情報を属性タイプドメインのリストに格納する。
	 * @param attTypeDomainList 格納先
	 * @param attTypeListElement 解析するXML（ワークフロー定義XMLのworkFlow.StatusTypeList.attType部分）
	 * @throws Exception
	 */
	private void setAttTypeListByXml(List<AttributeTypeDomain> attTypeDomainList, Element attTypeListElement) throws Exception {
		
		NodeList attTypeNodeList = attTypeListElement.getElementsByTagName("attType");
		
		for (int i = 0; i< attTypeNodeList.getLength(); i++)
		{
			Element attTypeElement = (Element)attTypeNodeList.item(i);
			AttributeTypeDomain attTypeDomain = new AttributeTypeDomain();

			//id
			String id = attTypeElement.getAttribute("attTypeId");
			attTypeDomain.setId(Long.valueOf(id));

			//リストに追加
			attTypeDomainList.add(attTypeDomain);
		}
	}
	
	/**
	 * ステータスタイプ定義のXMLから取得した情報をステータスタイプドメイン(カスタム属性)のマップに格納する。
	 * @param statusTypeLayoutMap 格納先
	 * @param statusTypeListElement 解析するXML（ワークフロー定義XMLのworkFlow.StatusTypeList.attType部分）
	 * @throws Exception
	 */
	private void setStatusTypeLayoutMapByXml(
			Map<Integer, StatusTypeLayoutDomain> statusTypeLayoutMap, Element statusTypeListElement) throws Exception
	{
		NodeList statusTypeNodeList = statusTypeListElement.getElementsByTagName("statusType");

		for (int i = 0; i < statusTypeNodeList.getLength() ; i++)
		{
			Element statusTypeElement = (Element)statusTypeNodeList.item(i);
			StatusTypeLayoutDomain statusTypeLayoutDomain = new StatusTypeLayoutDomain();

			//seq
			int seq = Integer.parseInt(statusTypeElement.getAttribute("seq"));
			statusTypeLayoutDomain.setSequence(seq);

			//attributeTypeList
			Element attTypeListElement = (Element)statusTypeElement.getElementsByTagName("attTypes").item(0);
			setAttTypeLayoutListByXml(statusTypeLayoutDomain.getAttributeLayoutList(), attTypeListElement);
			
			// 表示順情報が存在する場合
			if (statusTypeLayoutDomain.getAttributeLayoutList().size() > 0) {
				//マッピング
				statusTypeLayoutMap.put(statusTypeLayoutDomain.getSequence(), statusTypeLayoutDomain);
			}
		}
	}
	
	/**
	 * 属性タイプ定義のXMLから取得した情報を属性タイプドメインのリストに表示順で格納する。<br>
	 * ※前提条件：XML内で既に表示順でソート済み
	 * 
	 * @param attTypeLayoutDomainList 格納先
	 * @param attTypeListElement 解析するXML（ワークフロー定義XMLのworkFlow.StatusTypeList.attType部分）
	 * @throws Exception
	 */
	private void setAttTypeLayoutListByXml(
			List<AttributeTypeLayoutDomain> attTypeLayoutDomainList, Element attTypeListElement) throws Exception {
		
		NodeList attTypeNodeList = attTypeListElement.getElementsByTagName("attType");
		
		for (int i = 0; i< attTypeNodeList.getLength(); i++)
		{
			Element attTypeElement = (Element)attTypeNodeList.item(i);
			AttributeTypeLayoutDomain attTypeLayoutDomain = new AttributeTypeLayoutDomain();
			
			//dispOrder
			String dispOrderStr = attTypeElement.getAttribute("dispOrder");
			//newCopyFlag
			String newCopyFlagStr = attTypeElement.getAttribute("newCopyFlag");
			
			//dispOrderを詰める
			if (dispOrderStr == null || dispOrderStr.length() == 0) {
				attTypeLayoutDomain.setOrderSetFlag(false);
			}else if (dispOrderStr != null || dispOrderStr.length() != 0) {
				attTypeLayoutDomain.setOrderSetFlag(true);
			}
			//複製フラグを詰める
			attTypeLayoutDomain.setNewCopyFlag(Boolean.parseBoolean(newCopyFlagStr));
			//id
			String id = attTypeElement.getAttribute("attTypeId");
			attTypeLayoutDomain.setId(Long.valueOf(id));
			
			//リストに追加
			attTypeLayoutDomainList.add(attTypeLayoutDomain);
		}
	}

}
