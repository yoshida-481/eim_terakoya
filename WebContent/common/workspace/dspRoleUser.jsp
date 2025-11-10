<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>
<%@ page import = "common.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>

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
	String prmRoleId = EIMUtils.getParameter(request, "roleId");
	String prmIsNotDisplayInvalidityUser = EIMUtils.getParameter(request, "isNotDisplayInvalidityUser");

	//Message
	String message = null;
	Object[] paramId = {
			"roleId=" + prmRoleId,
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
		
		//Role
		EIMRole role = RoleUtils.getRoleById(sess, Long.parseLong(prmRoleId));
		if(role == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.ROLE.NOTFOUND");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.ROLE.NOTFOUND");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}
		
		//User List
		List userList = null;
		
		if (prmIsNotDisplayInvalidityUser == null)
		{
			//全ユーザを検索対象とする
			userList = RoleUtils.getUserList(sess, role, false);
		}
        //エントリー参照画面からコールされた場合
		else
		{
			//有効ユーザのみ検索対象とする
			userList = RoleUtils.getUserList(sess, role, 0, false);			
		}
		
		//Root Node
		out.println("<users>");
				
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
			
			//Group
			String groupName = "";
			List groupList = GroupUtils.getGroupByUser(sess, user);
			for(int j = 0; j < groupList.size(); j++)
			{
				EIMGroup group = (EIMGroup)groupList.get(j);
				if(j == 0)
				{
					groupName += group.getName();
				}
				else
				{
					groupName += "," + group.getName();
				}
			}
			
			//XML
			out.println("<user");
				out.println(" userId=\"" + user.getId() + "\"");
				out.println(" userCode=\"" + StringUtils.xmlEncode(user.getCode()) + "\"");
				out.println(" userName=\"" + StringUtils.xmlEncode(user.getName()) + "\"");
				out.println(" userKana=\"" + StringUtils.xmlEncode(userKana) + "\"");
				out.println(" userMail=\"" + StringUtils.xmlEncode(userMail) + "\"");
				out.println(" groupName=\"" + StringUtils.xmlEncode(groupName) + "\"");
				out.println(">");
			out.println("</user>");
		}
		
		//End Root Node
		out.println("</users>");
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
