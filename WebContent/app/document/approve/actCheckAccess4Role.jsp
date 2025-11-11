<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>

<%
	class Users
	{
		private List repeatCheckList = new ArrayList();
		
		public void add(	EIMUser		user,
							JspWriter	out
						)
		throws Exception
		{
			if (!repeatCheckList.contains(String.valueOf(user.getId())))
			{
				out.println("<user");
					out.println(" label=\"" + StringUtils.xmlEncode(user.getName()) + "\"");
					out.println(" type=\"user\"");
					out.println(" userId=\"" + user.getId() + "\"");
					out.println(" userCode=\"" + StringUtils.xmlEncode(user.getCode()) + "\"");
					out.println(" userName=\"" + StringUtils.xmlEncode(user.getName()) + "\"");
					out.println(" userKana=\"" + StringUtils.xmlEncode(user.getKana()) + "\"");
					out.println(" userMail=\"" + StringUtils.xmlEncode(user.getMail()) + "\"");
				out.println(">");
				out.println("</user>");
				
				repeatCheckList.add(String.valueOf(user.getId()));
			}
		}
	}
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
	String prmRole = EIMUtils.getParameter(request, "roleId");
	
	//Message
	String message = null;
	Object[] paramId = {
			"objId=" + prmObjId,
			"roleId=" + prmRole
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
		
		//Role
		EIMRole role = RoleUtils.getRoleById(sess, Long.parseLong(prmRole));
		//ロールが取得できない場合はエラーにする
		if(role == null)
		{
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.ROLE.NOTFOUND");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.ROLE.NOTFOUND");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}
		
		//Member
		List userList = RoleUtils.getUserList(sess, role, 0, false);
		
		//Security
		EIMSecurity sec = object.getSecurity();
		if(sec != null)
		{

			for(int i = 0; i < userList.size(); i++)
			{
				//User
				EIMUser user = (EIMUser)userList.get(i);
				
				//Check
				if(!SecurityUtils.authorized(sess, object, user, EIMAccessRole.STATUS_UP))
				{
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOAPPROVER.ROLE", 
							new Object[]{role.getName(), user.getName()});
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
					return;
				}
			}
		}
		
		//ユーザ名称でソート
		userList = AppObjectUtil.getStrSortedList(userList, "getName", true);
		
		//XML
		out.println("<role label=\"" + StringUtils.xmlEncode(role.getName()) + "\""
				        + " type=\"role\""
				        + " roleId=\"" + role.getId() + "\""
				        + " roleName=\"" + StringUtils.xmlEncode(role.getName()) + "\">");
		Users users = new Users();
		for (int k = 0 ; k < userList.size() ; k++)
		{
			users.add((EIMUser)userList.get(k), out);
		}
		out.println("</role>");
		
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
