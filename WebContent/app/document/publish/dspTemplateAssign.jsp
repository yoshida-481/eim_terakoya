<%@ page contentType="text/xml; charset=UTF-8" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.io.*" %>
<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>

<%

	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");
	
	//PrintWriter
	PrintWriter pw = null;
	
	//Session
	EIMSession sess = null;
	
	//Parameter
	String prmTemplateId = null;
	
	//Message
	String message = null;
	
	try{
		//PrintWriter
		pw = response.getWriter();
		
		//Session
		sess = EIMUtils.getSession(request);
		
		//Parameter
		prmTemplateId = EIMUtils.getParameter(request, "templateId");
		
		//Template Object
		EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmTemplateId));
		
		EIMAttribute attrUsers = object.getAttribute(EIMConfig.get("ATTR_NAME_PUBLIC_NOTIFICATION_TEMPLATE_USER_ID"));
		EIMAttribute attrGroups = object.getAttribute(EIMConfig.get("ATTR_NAME_PUBLIC_NOTIFICATION_TEMPLATE_GROUP_ID"));
		EIMAttribute attrRoles = object.getAttribute(EIMConfig.get("ATTR_NAME_PUBLIC_NOTIFICATION_TEMPLATE_ROLE_ID"));
		EIMAttribute attrComps = object.getAttribute(EIMConfig.get("ATTR_NAME_PUBLIC_NOTIFICATION_TEMPLATE_COMP_ID"));
		
		//Root Node
		pw.println("<assigns>");
		
		// User
		if( attrUsers != null){
			long userIds[] = attrUsers.getInts();
			for(int i = 0; i < userIds.length; i++){
				EIMUser user = UserUtils.getUserById(sess, userIds[i]);
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
				pw.println("<user");
					pw.println(" userId=\"" + user.getId() + "\"");
					pw.println(" userCode=\"" + StringUtils.xmlEncode(user.getCode()) + "\"");
					pw.println(" userName=\"" + StringUtils.xmlEncode(user.getName()) + "\"");
					pw.println(" userKana=\"" + StringUtils.xmlEncode(userKana) + "\"");
					pw.println(" userMail=\"" + StringUtils.xmlEncode(userMail) + "\"");
				pw.println("/>");
			}
		}
		
		//Group
		if( attrGroups != null){
			long groupIds[] = attrGroups.getInts();
			for(int i = 0; i < groupIds.length; i++){
				EIMGroup group = GroupUtils.getGroupById(sess, groupIds[i]);
				pw.println("<group");
				pw.println(" groupId=\"" + group.getId() + "\"");
				pw.println(" groupName=\"" + StringUtils.xmlEncode(group.getName()) + "\"");
				pw.println("/>");
			}
		}
		
		//Role
		if( attrRoles != null){
			long roleIds[] = attrRoles.getInts();
			for(int i = 0; i < roleIds.length; i++){
				EIMRole role = RoleUtils.getRoleById(sess, roleIds[i]);
				pw.println("<role");
				pw.println(" roleId=\"" + role.getId() + "\"");
				pw.println(" roleName=\"" + StringUtils.xmlEncode(role.getName()) + "\"");
				pw.println("/>");
			}
		}
		
		//Comp
		if( attrComps != null){
			long compIds[] = attrComps.getInts();
			for(int i = 0; i < compIds.length; i++){
				EIMComp comp = CompUtils.getCompById(sess, compIds[i]);
				pw.println("<comp");
				pw.println(" compId=\"" + comp.getId() + "\"");
				pw.println(" compName=\"" + StringUtils.xmlEncode(comp.getName()) + "\"");
				pw.println(" groupId=\"" + comp.getGroup().getId() + "\"");
				pw.println(" groupName=\"" + StringUtils.xmlEncode(comp.getGroup().getName()) + "\"");
				pw.println(" roleId=\"" + comp.getRole().getId() + "\"");
				pw.println(" roleName=\"" + StringUtils.xmlEncode(comp.getRole().getName()) + "\"");
				pw.println("/>");
			}
		}
		
		//End Root Node
		pw.println("</assigns>");

	}
	catch(Exception e)
	{
		out.clear();
		message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
		try{
			if(sess != null){
				sess.rollback();
			}
		}
		catch (Exception se) {
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
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
