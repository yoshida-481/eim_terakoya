<%@page import="jp.co.ctc_g.eim.framework.common.util.EIMThreadContext"%>
<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>

<%@ page import = "jp.co.ctc_g.eim.framework.common.util.*" %>
<%@ page import = "jp.co.ctc_g.eim.framework.business.service.*" %>
<%@ page import = "jp.co.ctc_g.eim.framework.business.domain.*" %>
<%@ page import = "jp.co.ctc_g.eim.framework.common.component.*"%>

<%@ page import = "java.util.*" %>
<%@ page import = "common.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "org.springframework.context.*" %>

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
	String prmViewSource = request.getParameter("viewSource");	// セキュリティ or ワークフロー

	//Message
	String message = null;

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
		if(!AdminAuthUtil.hasAnyAuth(loginUser))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.NOTADMINISTRATOR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.NOTADMINISTRATOR");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message));
			return;
		}

		//Valiables
		UserDefGroupDomain userDefGroup = null;
		List userDefGroupList = null;
		boolean flg = false;

		try
		{
			if(EIMThreadContext.getEIMSession() == null)
			{
				flg = true;
				EIMThreadContext.putEIMSession(sess);
			}

			//Root UserDefGroupList
			ApplicationContext context = ApplicationContextLoader.getContext();
			UserDefGroupConfService uds = (UserDefGroupConfService)context.getBean("UserDefGroupConfService");

			userDefGroupList = uds.getUserDefGroupList();
		}
		catch(Exception e)
		{
			throw e;
		}
		finally
		{	//Remove Session from Thread Local Table
			if(flg == true)
				EIMThreadContext.removeEIMSession();
		}

		//Root Node
		out.println("<userDefGroups>");

		for(int i = 0; i < userDefGroupList.size(); i++)
		{
			//UserDefGroup
			userDefGroup = (UserDefGroupDomain)userDefGroupList.get(i);
			// セキュリティ管理画面から呼ばれた場合
			if (prmViewSource != null && prmViewSource.equals("0")) {
				if (userDefGroup.getSupportGetSearchCondition()) {
					out.println("<userDefGroup");
						out.println(" label=\"" + StringUtils.xmlEncode(userDefGroup.getName()) + "\"");
						out.println(" isBranch=\"" + "false" + "\"");
						out.println(" userDefGroupId=\"" + userDefGroup.getId() + "\"");
					out.println("/>");
				}
			// ワークフロー画面から呼ばれた場合
			} else {
				if (userDefGroup.getSupportGetUserList()) {
					out.println("<userDefGroup");
						out.println(" label=\"" + StringUtils.xmlEncode(userDefGroup.getName()) + "\"");
						out.println(" isBranch=\"" + "false" + "\"");
						out.println(" userDefGroupId=\"" + userDefGroup.getId() + "\"");
					out.println("/>");
				}
			}
		}

		//End Root Node
		out.println("</userDefGroups>");
	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), eime.getMessage()), eime);
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
			if(EIMThreadContext.getEIMSession() != null){
				EIMThreadContext.removeEIMSession();
			}
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
