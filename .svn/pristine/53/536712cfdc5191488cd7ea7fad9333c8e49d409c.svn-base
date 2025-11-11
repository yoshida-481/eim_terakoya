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
	String prmRoleId = EIMUtils.getParameter(request, "roleId");

	//Message
	String message = null;
	Object[] paramId = {
			"roleId=" + prmRoleId
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
		 * Get Role
		 */
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

		//Other Role
		List otherList = RoleUtils.getOtherRoleNameList(sess, role.getId());

		//Parent Role
		EIMRole parentRole = null;
		if(role.getParentId() != 0)
		{
			parentRole = RoleUtils.getRoleById(sess, role.getParentId());
		}
		
		//XML
		out.println("<role");
			out.println(" roleId=\"" + role.getId() + "\"");
			if(otherList != null)
			{
				out.println(" " + LanguageFieldUtil.PARAM_OTHRE_CNT + "=\"" + otherList.size() + "\"");
			}
			if(parentRole != null)
			{
				out.println(" parentRoleId=\"" + parentRole.getId() + "\"");
				out.println(" parentRoleName=\"" + StringUtils.xmlEncode(parentRole.getName()) + "\"");
			}
			out.println(">");
			for(int i=0;i<otherList.size();i++){
				EIMOtherName eimOtherName = (EIMOtherName)otherList.get(i);
				out.println("<lang");
					out.println(" " + LanguageFieldUtil.PARAM_OTHRE_LID + "=\"" + eimOtherName.getLangId() + "\"");
					out.println(" " + LanguageFieldUtil.PARAM_OTHRE_NAME + "=\"" + StringUtils.xmlEncode(eimOtherName.getName()) + "\"");
				out.println("/>");
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
