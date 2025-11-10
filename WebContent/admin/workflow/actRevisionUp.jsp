<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>

<%@ page import = "java.util.*" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.TransactionContext" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.WorkflowService"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.StatusTypeService"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.EventTypeService"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.*"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.exception.EIMException"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.OperationHistoryService"%>

<%@ page import = "jp.co.ctc_g.eim.app.form.business.service.StatusTypeLayoutService"%>
<%@ page import = "jp.co.ctc_g.eim.app.form.business.domain.StatusTypeLayoutDomain"%>
<%@ page import = "jp.co.ctc_g.eim.admin.business.service.WorkFlowDefAdminService"%>
<%@ page import = "jp.co.ctc_g.eim.admin.business.domain.WorkflowAdminDomain"%>
<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	//Session
	EIMSession sess = null;
	EIMUser user = null;
	boolean sessPutFlag = false;

	//Parameter
	String prmSrcId = EIMUtils.getParameter(request, "prmSrcId");
	String prmNmSpace = EIMUtils.getParameter(request, "nmSpace");
	String prmNewDefNameJP = EIMUtils.getParameter(request, "newDefNameJP");
	String prmWfNameJP = EIMUtils.getParameter(request, "wfNameJP");
	String prmwfNameEN = EIMUtils.getParameter(request, "wfNameEN");
	String prmRegistFlg = EIMUtils.getParameter(request, "registFlg");

	//Message
	String message = null;
	Object[] paramId = { "prmSrcId=" + prmSrcId, "prmNmSpace=" + prmNmSpace, "prmNewDefNameJP=" + prmNewDefNameJP,
			"prmWfNameJP=" + prmWfNameJP, "prmwfNameEN=" + prmwfNameEN, };

	//タグの署名・暗号化状態判定フラグ
	boolean checkTagSignFlag = false;

	try {
		//Session
		sess = EIMUtils.getSession(request);
		if (sess == null) {
			message = EIMResource.getMessage(request, "EIM.ERROR.SESSIONTIMEOUT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.SESSIONTIMEOUT");
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}
		String appId = sess.getAttribute("ADMIN_APP_ID").toString();

		user = (EIMUser) sess.getAttribute("USER");

		//定義名称のあいまい検索を行うためV5APIを使用
		if (EIMThreadContext.getTransactionContext() != null) {
			EIMThreadContext.removeTransactionContext();
		}

		TransactionContext context = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
		EIMThreadContext.putTransactionContext(context);
		context.setLangId(sess.getLangId());
		context.setDBConnection(sess.getDBConnection());
		context.setUser(ConvertUtils.toUserDomain(user));

		WorkflowService workflowService = (WorkflowService) ApplicationContextLoader.getApplicationContext().getBean("workflowService2");
		StatusTypeService statusTypeService = (StatusTypeService) ApplicationContextLoader.getApplicationContext().getBean("statusTypeService2");
		EventTypeService eventTypeService = (EventTypeService) ApplicationContextLoader.getApplicationContext().getBean("eventTypeService2");
		OperationHistoryService operationHistoryService = (OperationHistoryService) ApplicationContextLoader.getApplicationContext().getBean("operationHistoryService2");
		
		// WorkflowSrc
		WorkflowDomain workflowSrc = workflowService.getById(Long.parseLong(prmSrcId));
		// ステータスタイプリスト
		List<StatusTypeDomain> statusTypeDomainList = workflowSrc.getStatusTypeList();
		List<StatusTypeDomain> statusTypeList = new ArrayList<StatusTypeDomain>();
		for(StatusTypeDomain statusTypeDomain : statusTypeDomainList){
			StatusTypeDomain statusType = statusTypeService.getById(statusTypeDomain.getId());
			statusTypeList.add(statusType);
		}
		workflowSrc.setStatusTypeList(statusTypeList);
		
		// イベントタイプリスト
		List<EventTypeDomain> eventTypeDomainList = workflowSrc.getEventTypeList();
		// イベントタイプをID順にソートする
		List<EventTypeDomain> sortedEventTypeDomainList = AppObjectUtil.getLongSortedList(eventTypeDomainList, "getId", true);
		List<EventTypeDomain> eventTypeList = new ArrayList<EventTypeDomain>();
		for(EventTypeDomain eventTypeDomain : sortedEventTypeDomainList){
			EventTypeDomain eventType = eventTypeService.getById(eventTypeDomain.getId());
			eventTypeList.add(eventType);
		}
		workflowSrc.setEventTypeList(eventTypeList);
		
		// WorkflowDest
		WorkflowDomain workflowDest = new WorkflowDomain();
		if (StringUtils.isBlank(prmNmSpace)) {
			workflowDest.setDefinitionName(prmNewDefNameJP);
		} else {
			workflowDest.setDefinitionName(prmNmSpace + ":" + prmNewDefNameJP);
		}

		// ワークフロー名称一覧
		List<OtherNameDomain> nameList = new ArrayList<OtherNameDomain>();
		if (!StringUtils.isBlank(prmWfNameJP)) {
			OtherNameDomain otherNameDomain = new OtherNameDomain();
			otherNameDomain.setLangId("JA");
			otherNameDomain.setName(prmWfNameJP);
			nameList.add(otherNameDomain);
		}
		if (!StringUtils.isBlank(prmwfNameEN)) {
			OtherNameDomain otherNameDomain = new OtherNameDomain();
			otherNameDomain.setLangId("EN");
			otherNameDomain.setName(prmwfNameEN);
			nameList.add(otherNameDomain);
		}
		if (nameList.size() > 0) {
			workflowDest.setNameList(nameList);
		}

		if ("false".equals(prmRegistFlg)) {
			workflowSrc.setStatusTypeList(null);
			workflowSrc.setEventTypeList(null);
		}

		WorkflowDomain workflow = workflowService.revisionUp(workflowSrc, workflowDest);
		if (workflow == null || workflow.getId() == 0) {
			//message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODOCORFOLORTAG");
			//out.println(AppMessageUtils.makeErrorTagByMessage(message));
			//message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODOCORFOLORTAG");
			//log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}

		if(appId.equals(AppConstant.ADMIN_APP_ID_DOCUMENT)) {
			// ワークフロー設定オブジェクトのコピー 
			AppObjectUtil.copyDocWorkFlowSettingObject(sess, workflowSrc.getId(), workflow.getId());

			// 公開通知エントリーオブジェクトのコピー
			AppObjectUtil.copyDocWorkflowPublishNotifyObject(sess, workflowSrc.getId(), workflow.getId());
			
			// メニューからのリビジョンアップの場合に実行
			if("true".equals(prmRegistFlg)) {
				// ワークフロー公開処理オブジェクトのコピー
				AppObjectUtil.copyDocWorkflowPublishObject(sess, workflowSrc.getId(), workflow.getId(), true);
				
				// ワークフロー設定オブジェクト(ステータス毎に設定されている属性)のコピー 
				AppObjectUtil.copyDocWorkFlowSettingObjectForStatusType(sess, workflowSrc, workflow);
			
				// リビジョンアップ後ワークフローにステータスセキュリティを作成する
				EIMWorkFlow workflowSrcBO = new EIMWorkFlow(workflow.getId(), workflow.getName(), "");
				AppSecurityUtils.updateStatusSecurityForDocByWf(sess, workflowSrcBO);
			}
			
		} else if (appId.equals(AppConstant.ADMIN_APP_ID_FORM)) {
			// 並び替えを設定
			StatusTypeLayoutService statusTypeLayoutService = (StatusTypeLayoutService) ApplicationContextLoader.getApplicationContext().getBean("adminStatusTypeLayoutService");
			WorkFlowDefAdminService workFlowDefAdminService = (WorkFlowDefAdminService) ApplicationContextLoader.getApplicationContext().getBean("workFlowDefAdminService");
			if (workflowSrc != null && workflowSrc.getStatusTypeList() != null) {
				for (int x = 0; x < workflowSrc.getStatusTypeList().size(); x++) {
					StatusTypeDomain statusTypesrc = workflowSrc.getStatusTypeList().get(x);
					StatusTypeLayoutDomain statusTypeLayoutDomainsrc = statusTypeLayoutService.getById(statusTypesrc.getId());
					if (statusTypeLayoutDomainsrc != null) {
						for (int y = 0; y < statusTypeLayoutDomainsrc.getAttributeLayoutList().size(); y++) {
							if (statusTypeLayoutDomainsrc.getAttributeLayoutList().get(y).isOrderSetFlag()) {
								jp.co.ctc_g.eim.framework.common.util.EIMThreadContext.putEIMSession(sess);
								// ワークフロー定義取得(更新後のステータスタイプは取得できないため、ワークフローIDから更新後のステータスタイプを取得)
								WorkflowAdminDomain workflowAdmin = workFlowDefAdminService.getDefById("form","app.form.user", workflow.getId());
								
								// 更新対象のステータスタイプを取得
								jp.co.ctc_g.eim.framework.business.domain.entity.StatusTypeDomain statusType = workflowAdmin.getStatusTypeList().get(x);
								StatusTypeLayoutDomain statusTypeLayoutDomaindest = statusTypeLayoutService.getById(statusType.getId());
								
								// 更新対象のステータスタイプを更新
								statusTypeLayoutDomaindest.getAttributeLayoutList().clear();
								statusTypeLayoutDomaindest.setAttributeLayoutList(statusTypeLayoutDomainsrc.getAttributeLayoutList());
								statusTypeLayoutService.createStatusTypeObject(statusTypeLayoutDomaindest);
								break;
							}
						}
					}
				}
			}
		}
		
		//現状、システム管理の操作履歴に関する定数値がEIMConstantに定義されているが、
		//本来、システム管理側のソース、もしくは、設定ファイルに定義あるいは設定されるべきである。
		//今回は暫定的にJSPに直接定数定義しておく。
		long OPERATIONHISTORY_OPERATIONTYPE_REVISIONUP_WORKFLOW = 1132;	// ワークフローリビジョンアップ
		long OPERATIONHISTORY_RECORDTYPE_WORKFLOW = 8; 					// 操作種別ID：ワークフロー
		long OPERATIONHISTORY_RECORDINFO_REVISIONUP = 34; 		// 操作情報ID：リビジョンアップ対象
		
		//Create Operation History
		OperationHistoryDomain operationHistory = new OperationHistoryDomain();
		operationHistory.setApplicationTypeId(Long.parseLong(AppConstant.SYSTEM));
		operationHistory.setOperationTypeId(OPERATIONHISTORY_OPERATIONTYPE_REVISIONUP_WORKFLOW);
		operationHistory.setRecordTypeIdA(OPERATIONHISTORY_RECORDTYPE_WORKFLOW);	// 操作種別ID
		operationHistory.setRecordInfoIdA(OPERATIONHISTORY_RECORDINFO_REVISIONUP);	// 操作情報ID
		operationHistory.setRecordObjectA(workflow);	// 操作対象
		operationHistoryService.create(operationHistory);	
		
		//Commit
		sess.commit();
		//XML
		out.println("<workFlow");
			out.println(" workFlowId=\"" + workflow.getId() + "\"");
			out.println(">");
		out.println("</workFlow>");
	} catch (EIMException eime) {
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(user.getId(), eime.getMessage(), paramId), eime);
		try {
			if (sess != null) {
				sess.rollback();
			}
		} catch (Exception se) {
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
	} catch (Exception e) {
		out.clear();
		message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
		try {
			if (sess != null) {
				sess.rollback();
			}
		} catch (Exception se) {
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
	} finally {
		try {
			if (sessPutFlag) {
				EIMThreadContext.removeTransactionContext();
				sessPutFlag = false;
			}
			if (sess != null) {
				sess.close();
			}
		} catch (Exception se) {
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
	}
%>