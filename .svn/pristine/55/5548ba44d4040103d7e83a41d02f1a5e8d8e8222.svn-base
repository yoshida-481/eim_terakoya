<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>
<%@ page import = "java.util.*" %>

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
		if(!AdminAuthUtil.hasSpecifiedOrGeneralAuth(loginUser, AppConstant.ADMIN_AUTH_ID_USER))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.NOTADMINISTRATOR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.NOTADMINISTRATOR");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message));
			return;
		}
		
		/*
		 * Get User
		 */
		//User
		EIMUser user = UserUtils.getUserById(sess, Long.parseLong(prmUserId));
		if(user == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.USER.NOTFOUND");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.USER.NOTFOUND");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}

		//Other User
		List otherList = UserUtils.getOtherUserNameList(sess, user.getId());	//値がない場合は空欄で表示
		
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
		//Lang
		String userLang = "";
		if(user.getLang() != null)
		{
			userLang = user.getLang();
		}
		
		//XML
		out.println("<user");
			out.println(" userId=\"" + user.getId() + "\"");
			out.println(" userCode=\"" + StringUtils.xmlEncode(user.getCode()) + "\"");
			out.println(" userKana=\"" + StringUtils.xmlEncode(userKana) + "\"");
			out.println(" userPass=\"" + StringUtils.xmlEncode(user.getPass()) + "\"");
			out.println(" userMail=\"" + StringUtils.xmlEncode(userMail) + "\"");
			out.println(" userAdmin=\"" + user.getAdmin() + "\"");
			out.println(" userDisable=\"" + user.getDisable() + "\"");
			out.println(" userLang=\"" + StringUtils.xmlEncode(userLang) + "\"");
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
