<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>

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
	String prmUserId = EIMUtils.getParameter(request, "userId");
	
	//Message
	String message = null;
	Object[] paramId = {
			"userId=" + prmUserId
			};

	try{
		sess = EIMUtils.getSession(request);
		if(sess == null)
		{
			message = EIMResource.getMessage(request, "EIM.ERROR.SESSIONTIMEOUT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.SESSIONTIMEOUT");
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}

		loginUser = (EIMUser)sess.getAttribute("USER");

		//User
		EIMUser user = UserUtils.getUserById(sess, Long.parseLong(prmUserId));		
		//ユーザーが取得できない場合はエラーにする
		if(user == null)
		{
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.USER.NOTFOUND");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.USER.NOTFOUND");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}
			
		//Disable
		if(user.getDisable() == 1)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOSELECT.INVALIDITY.USER");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOSELECT.INVALIDITY.USER");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;			
		}
		
		out.println("<user");
		out.println(" userId=\"" + user.getId() + "\"");
		out.println(" userName=\"" + StringUtils.xmlEncode(user.getName()) + "\"");
		out.println(">");
		out.println("</user>");
				
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
