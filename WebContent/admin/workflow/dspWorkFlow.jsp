<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	//Session
	EIMSession sess = null;
	EIMUser loginUser = null;

	//Parameter
	String prmWorkFlowId = request.getParameter("workFlowId");

	//Message
	String message = null;
	Object[] paramId = {
			"workFlowId=" + prmWorkFlowId
			};

	try
	{
		/*
		 * Parameter check
		 */
		//Session
		sess = EIMUtils.getSession(request);
		if(sess == null)
		{
			message = EIMResource.getMessage(request, "EIM.ERROR.SESSIONTIMEOUT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.SESSIONTIMEOUT");
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}

		//User
		loginUser = (EIMUser)sess.getAttribute("USER");
		if(!AdminAuthUtil.hasSpecifiedOrGeneralAuth(loginUser, AppConstant.ADMIN_AUTH_ID_WORKFLOW))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.NOTADMINISTRATOR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.NOTADMINISTRATOR");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message));
			return;
		}

		/*
		 * Get WorkFlow
		 */
		//WorkFlow
		EIMWorkFlow workFlow = WorkFlowUtils.getWorkFlowById(sess, Long.parseLong(prmWorkFlowId));
		if(workFlow == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.WORKFLOW.NOTFOUND");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.WORKFLOW.NOTFOUND");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}

		//Other WorkFlow
		List otherList = WorkFlowUtils.getOtherWorkFlowNameList(sess, workFlow.getId());


		//XML
		out.println("<workFlow");
			out.println(" workFlowId=\"" + workFlow.getId() + "\"");
			//定義名称からネームスペースを取得
			out.println(" namespace=\"" + StringUtils.xmlEncode(NamespaceUtil.getNamespaceByDefName(workFlow.getDefName())) + "\"");

		if(otherList != null)
		{
			out.println(" " + LanguageFieldUtil.PARAM_OTHRE_CNT + "=\"" + otherList.size() + "\"");
		}
		out.println(">");
		for(int i=0;i<otherList.size();i++){
			EIMOtherName eimOtherName = (EIMOtherName)otherList.get(i);
			out.println("<lang");
				out.println(" " + LanguageFieldUtil.PARAM_OTHRE_LID + "=\"" + eimOtherName.getLangId() + "\"");
				out.println(" " + LanguageFieldUtil.PARAM_OTHRE_NAME + "=\"" + StringUtils.xmlEncode(eimOtherName.getName()) + "\"");
			out.println("/>");
		}
		out.println("</workFlow>");
	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), eime.getMessage(), paramId), eime);
	}
	catch(Exception e)
	{
		out.clear();
		message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
	}
	finally
	{
		try{
			if(sess != null){
				sess.close();
			}
		}
		catch (Exception se) {
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
	}
%>
