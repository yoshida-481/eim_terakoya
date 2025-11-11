<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>

<%@ page import = "java.util.*" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.WorkflowService"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.*"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.TransactionContext" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.exception.EIMException"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils" %>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	//Session
	EIMSession sess = null;
	EIMUser user = null;

	//Parameter
	String prmWorkflowId = request.getParameter("workflowId");

	//Message
	String message = null;
	Object[] paramId = { "workflowId=" + prmWorkflowId };

	try {
		// Session
		sess = EIMUtils.getSession(request);
		if (sess == null) {
			message = EIMResource.getMessage(request, "EIM.ERROR.SESSIONTIMEOUT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.SESSIONTIMEOUT");
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}
		user = (EIMUser) sess.getAttribute("USER");
		
		//定義名称のあいまい検索を行うためV5APIを使用
		if(EIMThreadContext.getTransactionContext() != null)
		{
			EIMThreadContext.removeTransactionContext();
		}
		
		TransactionContext context = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
		EIMThreadContext.putTransactionContext(context);
		context.setLangId(sess.getLangId());
		context.setDBConnection(sess.getDBConnection());
		context.setUser(ConvertUtils.toUserDomain(user));

		WorkflowService workflowService = (WorkflowService) ApplicationContextLoader.getApplicationContext().getBean("workflowService2");
		// Workflow
		WorkflowDomain workflow = workflowService.getById(Long.parseLong(prmWorkflowId));

		if (workflow == null) {
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODOCORFOLORTAG");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODOCORFOLORTAG");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}

		//XML
		out.println("<workflow");

		out.println(" workflowId=\"" + workflow.getId() + "\"");
		if(workflow.getDefinitionName().contains(":")){
			String[] nameSpaceAndDefName = workflow.getDefinitionName().split(":",2);
			out.println(" workflowDefName=\"" + StringUtils.xmlEncode(nameSpaceAndDefName[1]) + "\"");
			out.println(" workflowNamespace=\"" + StringUtils.xmlEncode(nameSpaceAndDefName[0]) + "\"");
		}else{
		out.println(" workflowDefName=\"" + StringUtils.xmlEncode(workflow.getDefinitionName()) + "\"");
		}
		//out.println(" rev=\"" + object.getRev() + "\"");
		//out.println(" latest=\"" + object.getLatest() + "\"");
		//out.println(" createDate=\"" + object.getCreateDate() + "\"");
		//out.println(" modifyDate=\"" + object.getModifyDate() + "\"");
		for (OtherNameDomain otherNameDomain : workflow.getNameList()) {
			if ("JA".equals(otherNameDomain.getLangId())) {
				out.println(" workflowNameJP=\"" + StringUtils.xmlEncode(otherNameDomain.getName()) + "\"");
			} else if ("EN".equals(otherNameDomain.getLangId())) {
				out.println(" workflowNameEN=\"" + StringUtils.xmlEncode(otherNameDomain.getName()) + "\"");
			}
		}

		out.println(">");
		out.println("</workflow>");
	} catch (EIMException eime) {
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(user.getId(), eime.getMessage(), paramId), eime);
	} catch (Exception e) {
		out.clear();
		message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
	} finally {
		try {
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
