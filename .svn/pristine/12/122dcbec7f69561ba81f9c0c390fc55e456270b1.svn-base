<%@ page contentType="text/xml; charset=UTF-8" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>
<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());
	
	//Session
	EIMSession sess = null;
	
	//Message
	String message = null;
	
	try{
		//ContentType
		response.setContentType("text/xml; charset=UTF-8");
		response.setHeader("Cache-Control", "max-age=0, must-revalidate");
		
		//Session
		sess = EIMUtils.getSession(request);
		
		//Parameter
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
		
		//Object Type
		EIMObjectType newObjType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_PUBLIC_NOTIFICATION_TEMPLATE"));
		EIMAttributeType newTemplateName = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_PUBLIC_NOTIFICATION_TEMPLATE_NAME"));
		EIMAttributeType newAttrUser = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_PUBLIC_NOTIFICATION_TEMPLATE_USER_ID"));
		EIMAttributeType newAttrGroup = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_PUBLIC_NOTIFICATION_TEMPLATE_GROUP_ID"));
		EIMAttributeType newAttrRole = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_PUBLIC_NOTIFICATION_TEMPLATE_ROLE_ID"));
		EIMAttributeType newAttrComp = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_PUBLIC_NOTIFICATION_TEMPLATE_COMP_ID"));
		
		String objName = Long.toString(sess.getUser().getId());
		
		//Create Object
		EIMObject newObject = ObjectUtils.createObject(sess, newObjType, objName);
		ObjectAttributeUtils.setAttribute(sess, newObject, newTemplateName, prmTemplateName);
		
		//User
		if(userCount != 0){
			long arrUser[] = new long[userCount];
			int idx = 0;
			
			for(int i = 0; i < userCount; i++)
			{
				arrUser[idx] = Long.parseLong(EIMUtils.getParameter(request, "userId_" + i));
				idx++;
			}
			ObjectAttributeUtils.setAttribute(sess, newObject, newAttrUser, arrUser);
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
			ObjectAttributeUtils.setAttribute(sess, newObject, newAttrGroup, arrGroup);
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
			ObjectAttributeUtils.setAttribute(sess, newObject, newAttrRole, arrRole);
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
			ObjectAttributeUtils.setAttribute(sess, newObject, newAttrComp, arrComp);
		}
		
		//Create Operation History
		newObject.setName(prmTemplateName);
		OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, AppConstant.CREATE_PUBLISH_TEMPLATE,
				EIMConstant.TARGET_CREATE, AppConstant.TEMPLATE, newObject,
				null, null, null, null);
		
		//XML
		out.println("<object");
			out.println(" templateId=\"" + newObject.getId() + "\"");
			out.println(" templateName=\"" + StringUtils.xmlEncode(prmTemplateName) + "\"");
			out.println(" createUserName=\"" + objName + "\"");
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
