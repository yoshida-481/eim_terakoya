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
		if(object == null || !SecurityUtils.authorized(sess, object, sess.getUser(),EIMAccessRole.READ))
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
		
		// ロール所属ユーザーについて「公開読取権」チェックは行わない。
		// (公開通知メール送信時にチェックする)
		//Security
//		EIMSecurity sec = object.getSecurity();
//		if(sec != null)
//		{
			//Member
//			List userList = RoleUtils.getUserList(sess, role);
//			for(int i = 0; i < userList.size(); i++)
//			{
				//User
//				EIMUser user = (EIMUser)userList.get(i);
				
				//Check
//				if(!SecurityUtils.authorized(sess, object, user, EIMAccessRole.READ))
//				{
//					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOACCESS.ROLE", 
//							new Object[]{role.getName(), user.getName()});
//					out.println(AppMessageUtils.makeErrorTagByMessage(message));
//					message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOACCESS.ROLE", 
//							new Object[]{role.getName(), user.getName()});
//					log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
//					return;
//				}
//			}
//		}
		
		//XML
		out.println("<role");
			out.println(" roleId=\"" + role.getId() + "\"");
			out.println(" roleName=\"" + StringUtils.xmlEncode(role.getName()) + "\"");
			out.println(">");
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
