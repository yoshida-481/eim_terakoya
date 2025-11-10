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
	String prmObjId = EIMUtils.getParameter(request, "objId");
	String prmUserId = EIMUtils.getParameter(request, "userId");
	
	//Message
	String message = null;
	Object[] paramId = {
			"objId=" + prmObjId,
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
		//Object
		EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		if(object == null)
		{
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODOCUMENT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODOCUMENT");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}

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
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOPUBLISHER.INVALIDITY.USER", new Object[]{user.getName()});
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOPUBLISHER.INVALIDITY.USER", new Object[]{user.getName()});
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;			
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
		
		//Role
		String roleName = "";
		List roleList = RoleUtils.getRoleByUser(sess, user);
		for(int j = 0; j < roleList.size(); j++)
		{
			EIMRole role = (EIMRole)roleList.get(j);
			if(j == 0)
			{
				roleName += role.getName();
			}
			else
			{
				roleName += "," + role.getName();
			}
		}
		
		// 公開読み取り権限判定はメール送信時に行うので、ここでは判定しない
		
		// XML出力
		out.println("<user");
			out.println(" userId=\"" + user.getId() + "\"");
			out.println(" userName=\"" + StringUtils.xmlEncode(user.getName()) + "\"");
			out.println(" groupName=\"" + StringUtils.xmlEncode(groupName) + "\"");
			out.println(" roleName=\"" + StringUtils.xmlEncode(roleName) + "\"");
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
