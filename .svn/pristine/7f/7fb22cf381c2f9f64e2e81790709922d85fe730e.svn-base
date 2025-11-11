<%@ page contentType="text/xml; charset=UTF-8" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>
<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>
<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//Message
	String message = null;
	
	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");
	
	//Session
	EIMSession sess = null;
	
	//Attribute Type
	EIMAttributeType attType = null;
	
	try{
		//Session
		sess = EIMUtils.getSession(request);
		
		//Parameter
		String prmObjId = EIMUtils.getParameter(request, "objId");
		String prmTemplateName = EIMUtils.getParameter(request, "templateName");
		String prmUserCount = EIMUtils.getParameter(request, "userCount");
		String prmGroupCount = EIMUtils.getParameter(request, "groupCount");
		String prmRoleCount = EIMUtils.getParameter(request, "roleCount");
		String prmCompCount = EIMUtils.getParameter(request, "compCount");
		
		//atoi
		int userCount = 0;
		int groupCount = 0;
		int roleCount = 0;
		int compCount = 0;
		if (!StringUtils.isBlank(prmUserCount)) {
			userCount = Integer.parseInt(prmUserCount);
		}
		if (!StringUtils.isBlank(prmGroupCount)) {
			groupCount = Integer.parseInt(prmGroupCount);
		}
		if (!StringUtils.isBlank(prmRoleCount)) {
			roleCount = Integer.parseInt(prmRoleCount);
		}
		if (!StringUtils.isBlank(prmCompCount)) {
			compCount = Integer.parseInt(prmCompCount);
		}
		
		//Create Object
		EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		EIMAttributeType newTemplateName = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_PUBLIC_NOTIFICATION_TEMPLATE_NAME"));
		EIMAttributeType newAttrUser = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_PUBLIC_NOTIFICATION_TEMPLATE_USER_ID"));
		EIMAttributeType newAttrGroup = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_PUBLIC_NOTIFICATION_TEMPLATE_GROUP_ID"));
		EIMAttributeType newAttrRole = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_PUBLIC_NOTIFICATION_TEMPLATE_ROLE_ID"));
		EIMAttributeType newAttrComp = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_PUBLIC_NOTIFICATION_TEMPLATE_COMP_ID"));
		
		//Template name
		if(prmTemplateName != null){
			ObjectAttributeUtils.setAttribute(sess, object, newTemplateName, prmTemplateName);
		}
		
		//User
		if(userCount != 0){
			long arrUser[] = new long[userCount];
			int idx = 0;
			
			for(int i = 0; i < userCount; i++)
			{
				arrUser[idx] = Long.parseLong(EIMUtils.getParameter(request, "userId_" + i));
				idx++;
			}
			ObjectAttributeUtils.setAttribute(sess, object, newAttrUser, arrUser);
		}else{
			EIMAttribute attr = object.getAttribute(EIMConfig.get("ATTR_NAME_PUBLIC_NOTIFICATION_TEMPLATE_USER_ID"));
			if (attr != null && attr.getInts() != null && attr.getInts().length > 0) {
				ObjectAttributeUtils.deleteAttribute(sess, object, newAttrUser);
			}
		}
		
		//Group
		if(groupCount != 0){
			long arrGroup[] = new long[groupCount];
			int idx = 0;
			
			for(int i = 0; i < groupCount; i++)
			{
				arrGroup[idx] = Long.parseLong(EIMUtils.getParameter(request, "groupId_" + i));
				idx++;
			}
			ObjectAttributeUtils.setAttribute(sess, object, newAttrGroup, arrGroup);
		}else{
			EIMAttribute attr = object.getAttribute(EIMConfig.get("ATTR_NAME_PUBLIC_NOTIFICATION_TEMPLATE_GROUP_ID"));
			if (attr != null && attr.getInts() != null && attr.getInts().length > 0) {
				ObjectAttributeUtils.deleteAttribute(sess, object, newAttrGroup);
			}
		}
		
		//Role
		if(roleCount != 0){
			long arrRole[] = new long[roleCount];
			int idx = 0;
			
			for(int i = 0; i < roleCount; i++)
			{
				arrRole[idx] = Long.parseLong(EIMUtils.getParameter(request, "roleId_" + i));
				idx++;
			}
			ObjectAttributeUtils.setAttribute(sess, object, newAttrRole, arrRole);
		}else{
			EIMAttribute attr = object.getAttribute(EIMConfig.get("ATTR_NAME_PUBLIC_NOTIFICATION_TEMPLATE_ROLE_ID"));
			if (attr != null && attr.getInts() != null && attr.getInts().length > 0) {
				ObjectAttributeUtils.deleteAttribute(sess, object, newAttrRole);
			}
		}
		
		//Comp
		if(compCount != 0){
			long arrComp[] = new long[compCount];
			int idx = 0;
			
			for(int i = 0; i < compCount; i++)
			{
				arrComp[idx] = Long.parseLong(EIMUtils.getParameter(request, "compId_" + i));
				idx++;
			}
			ObjectAttributeUtils.setAttribute(sess, object, newAttrComp, arrComp);
		}else{
			EIMAttribute attr = object.getAttribute(EIMConfig.get("ATTR_NAME_PUBLIC_NOTIFICATION_TEMPLATE_COMP_ID"));
			if (attr != null && attr.getInts() != null && attr.getInts().length > 0) {
				ObjectAttributeUtils.deleteAttribute(sess, object, newAttrComp);
			}
		}
		
		//Create Operation History
		String templateName = prmTemplateName;
		if (templateName == null) {
			templateName = object.getAttribute(EIMConfig.get("ATTR_NAME_PUBLIC_NOTIFICATION_TEMPLATE_NAME")).getString();
		}
		object.setName(templateName);
		OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, AppConstant.UPDATE_PUBLISH_TEMPLATE,
				EIMConstant.TARGET_UPDATE, AppConstant.TEMPLATE, object,
				null, null, null, null);
		
		//XML
		out.println("<object");
			out.println(" objId=\"" + prmObjId + "\"");
			out.println(" createUserName=\"" + object.getName() + "\"");
			out.println(">");
		out.println("</object>");
		sess.commit();
	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(sess.getUser().getId(), eime.getMessage()), eime);
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
