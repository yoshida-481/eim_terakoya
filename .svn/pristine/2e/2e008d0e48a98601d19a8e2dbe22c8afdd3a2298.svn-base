<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>
<%@ page import = "common.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "jp.co.ctc_g.eim.admin.business.domain.NamespaceDomain" %>

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
	String prmObjTypeId = request.getParameter("objTypeId");

	//Message
	String message = null;
	Object[] paramId = {
			"objTypeId=" + prmObjTypeId
			};

	try
	{
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
		if(!AdminAuthUtil.hasSpecifiedOrGeneralAuth(loginUser, AppConstant.ADMIN_AUTH_ID_DOCUMENTTYPE))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.NOTADMINISTRATOR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.NOTADMINISTRATOR");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message));
			return;
		}

		//Namespace
		NamespaceDomain namespaceDomain = (NamespaceDomain)sess.getAttribute("namespace");
		String adminAppId = (String)session.getAttribute("ADMIN_APP_ID");

		//Object Type
		EIMObjectType objType = ObjectUtils.getObjectTypeById(sess, Long.parseLong(prmObjTypeId));
		if(objType == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.OBJTYPE.NOTFOUND");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.OBJTYPE.NOTFOUND");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}

		//WorkFlow
		EIMWorkFlow workFlow = WorkFlowUtils.getWorkFlowByType(sess, objType);
		if(workFlow != null)
		{
			//Status Type List
			List statusList = workFlow.getStatusTypeList();

			//Root Node
			out.println("<statusTypeList");
				out.println(" workflowId=\"" + workFlow.getId() + "\"");
				if (namespaceDomain == null || namespaceDomain.getName().equals(""))
				{
					if (AppConstant.ADMIN_APP_ID_GENERAL.equals(adminAppId)) {
						// 汎用システム管理は定義名をネームスペース付で表示
						out.println(" workflowName=\"" + StringUtils.xmlEncode(NamespaceUtil.getDefNameWithNamespaceParentheses(workFlow.getName(), workFlow.getDefName())) + "\"");
					} else {
						out.println(" workflowName=\"" + StringUtils.xmlEncode(workFlow.getName()) + "\"");
					}
				}
				else
				{
					out.println(" workflowName=\"" + StringUtils.xmlEncode(workFlow.getName()) + "\"");
				}
			out.println(">");

			for(int i = 0; i < statusList.size(); i++)
			{
				//Status Type
				EIMStatusType statusType = (EIMStatusType)statusList.get(i);

				//XML
				out.println("<statusType");
					out.println(" statusTypeId=\"" + statusType.getId() + "\"");
					out.println(" statusTypeName=\"" + StringUtils.xmlEncode(statusType.getName()) + "\"");
					out.println(" step=\"" + statusType.getStep() + "\"");
				out.println(">");
				out.println("</statusType>");
			}

			//End Root Node
			out.println("</statusTypeList>");
		}
		else {
			out.println("<OK></OK>");
		}
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
