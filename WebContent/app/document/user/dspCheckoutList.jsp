<%@page import="app.document.search.EIMDocSearchType"%>
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
	EIMUser user = null;

	//Message
	String message = null;
	
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
		user = (EIMUser)sess.getAttribute("USER");
		List result = AppSearchUtils.searchObjectsByConditionMaker(sess, EIMDocSearchType.DISPLAY_CHECKOUTITEM,
				EIMAccessRole.READ, new EIMSearchLimitCountCondition(EIMSearchLimitCountCondition.NOT_SPECIFIED, false), null);

		//essential attribute
        EIMAttributeType attTypeOfModifyUser = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_MODIFY_USER"));
		EIMAttributeType attTypeOfModifyDate = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_MODIFY_DATE"));
		EIMAttributeType attTypeOfCreateDate = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_CREATE_DATE"));
		EIMAttributeType attTypeOfFizeSize = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_SIZE"));
		
		//Root Node
		out.println("<objList>");
		
		for(int i = 0; i < result.size(); i++)
		{
			//Object
			EIMObject object = (EIMObject)result.get(i);

			String modifyDate = DateUtils.getDBTzToCLTzDate(sess, object.getModifyDate());
			String createDate = DateUtils.getDBTzToCLTzDate(sess, object.getCreateDate());
			EIMFile file = FileUtils.getFile(sess, object, FileUtils.getDefaultFormat(sess, object.getType()));
			
			//XML
			out.println("<object");
				out.println(" objId=\"" + object.getId() + "\"");
				out.println(" objTypeId=\"" + object.getType().getId() + "\"");
				out.println(" objTypeName=\"" + StringUtils.xmlEncode(object.getType().getName()) + "\"");
				out.println(" objName=\"" + StringUtils.xmlEncode(object.getName()) + "\"");
				out.println(" rev=\"" + object.getRev() + "\"");
				out.println(" modifyUserName=\"" + StringUtils.xmlEncode(object.getModifyUser().getName()) + "\"");
				out.println(" modifyDate=\"" + DateUtils.getDBTzToCLTzDate(sess, object.getModifyDate()) + "\"");
				
				//Status
				if(object.getStatus() != null)
				{
					out.println(" statusId=\"" + object.getStatus().getId() + "\"");
					out.println(" statusTypeName=\"" + StringUtils.xmlEncode(object.getStatus().getType().getName()) + "\"");
				}
				
				//Lock User
				if(object.getLockUser() != null)
				{
					out.println(" lockUserName=\"" + StringUtils.xmlEncode(object.getLockUser().getName()) + "\"");
					out.println(" lockDate=\"" + DateUtils.getDBTzToCLTzDate(sess, object.getLockDate(), "EIM.FORMAT.DATETIME") + "\"");
				}
				
				//Path
				String path = StringUtils.nullToBlank(AppObjectUtil.getPath(object));
				out.println(" path=\"" + StringUtils.xmlEncode(path) + "\"");
				
				/*
				 * 有効期限切れ判定
				 */
				boolean expiration = false;
				EIMAttribute expirationDate = object.getAttribute("有効期限");
				if(expirationDate != null)
				{
					expiration = DateUtils.judgeExpirationDate(sess, expirationDate.getDate());
				}
				out.println(" expiration=\"" + expiration + "\"");
				
				out.println(" attType_" + attTypeOfModifyUser.getId() + "=\"" + StringUtils.xmlEncode(object.getModifyUser().getName()) + "\"");
				out.println(" attType_" + attTypeOfModifyDate.getId() + "=\"" + modifyDate + "\"");
				out.println(" attType_" + attTypeOfCreateDate.getId() + "=\"" + createDate + "\"");
				out.println(" attType_" + attTypeOfFizeSize.getId() + "=\"" + file.getSize() + "\"");				
				
			out.println(">");
			out.println("</object>");
		}
		
		//End Root Node
		out.println("</objList>");
	
	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(user.getId(), eime.getMessage()), eime);
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
