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
	String prmCompId = EIMUtils.getParameter(request, "compId");
	String prmIsNotDisplayInvalidityUser = EIMUtils.getParameter(request, "isNotDisplayInvalidityUser");

	//Message
	String message = null;
	Object[] paramId = {
			"compId=" + prmCompId,
			"isNotDisplayInvalidityUser" + prmIsNotDisplayInvalidityUser			
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
		
		//Comp
		EIMComp comp = CompUtils.getCompById(sess, Long.parseLong(prmCompId));
		if(comp == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.COMP.NOTFOUND");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.COMP.NOTFOUND");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}
		
		//User List
		List userList = null;
		
		if (prmIsNotDisplayInvalidityUser == null)
		{
			//全ユーザを検索対象とする
			userList = CompUtils.getUserList(sess, comp);
		}
        //エントリー参照画面からコールされた場合
		else
		{
			//有効ユーザのみ検索対象とする
			userList = CompUtils.getUserList(sess, comp, 0);			
		}
		
		//Root Node
		out.println("<userList>");
		
		for(int i = 0; i < userList.size(); i++)
		{
			//User
			EIMUser user = (EIMUser)userList.get(i);
			
			//かな
			String userKana = "";
			if(user.getKana() != null)
			{
				userKana = user.getKana();
			}
			
			//Mail
			String userMail = "";
			if(user.getMail() != null)
			{
				userMail = user.getMail();
			}
			
			//XML
			out.println("<user");
				out.println(" userId=\"" + user.getId() + "\"");
				out.println(" userCode=\"" + StringUtils.xmlEncode(user.getCode()) + "\"");
				out.println(" userName=\"" + StringUtils.xmlEncode(user.getName()) + "\"");
				out.println(" userKana=\"" + StringUtils.xmlEncode(userKana) + "\"");
				out.println(" userMail=\"" + StringUtils.xmlEncode(userMail) + "\"");
				out.println(">");
			out.println("</user>");
		}
		
		//End Root Node
		out.println("</userList>");
		
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
