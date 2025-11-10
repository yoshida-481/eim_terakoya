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
	
		//Object Type MyDocument
		EIMObjectType objType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_MYDOCUMENT"));
		
		//Get MyDocument Object
		EIMObject myDocObj = ObjectUtils.getObjectByTypeAndName(sess, objType, user.getCode());
		
		if(myDocObj == null || !SecurityUtils.authorized(sess, myDocObj, user, EIMAccessRole.READ))
		{
			out.println("<objList/>");
			return;
		}
		
		List<EIMRelation> childRelList =
			AppSearchUtils.searchRelationByConditionMaker(sess, EIMDocSearchType.DISPLAY_MYDOCUMENT, EIMAccessRole.READ,
					new EIMSearchLimitCountCondition(EIMSearchLimitCountCondition.NOT_SPECIFIED, false), myDocObj);

		//essential attribute
        EIMAttributeType attTypeOfModifyUser = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_MODIFY_USER"));
		EIMAttributeType attTypeOfModifyDate = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_MODIFY_DATE"));
		EIMAttributeType attTypeOfCreateDate = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_CREATE_DATE"));
		EIMAttributeType attTypeOfFizeSize = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_SIZE"));
		
		//Root Node
		out.println("<objList>");
		for(int i = 0; i < childRelList.size(); i++)
		{
			//Relation
			EIMRelation relation = (EIMRelation)childRelList.get(i);
			
			//Object
			EIMObject object = relation.getChild();

			String modifyDate = DateUtils.getDBTzToCLTzDate(sess, object.getModifyDate());
			String createDate = DateUtils.getDBTzToCLTzDate(sess, object.getCreateDate());
			String createDateTime = String.valueOf(object.getCreateDate().getTime() / 1000);
			EIMFile file = FileUtils.getFile(sess, object, FileUtils.getDefaultFormat(sess, object.getType()));
			
			//XML
			out.println("<object");
				out.println(" objId=\"" + object.getId() + "\"");
				out.println(" objTypeId=\"" + object.getType().getId() + "\"");
				out.println(" objTypeName=\"" + StringUtils.xmlEncode(object.getType().getName()) + "\"");
				out.println(" objName=\"" + StringUtils.xmlEncode(object.getName()) + "\"");
				out.println(" revision=\"" + object.getRev() + "\"");
				out.println(" createUserName=\"" + StringUtils.xmlEncode(object.getCreateUser().getName()) + "\"");//作成者属性対応対象外:画面に表示されない
				out.println(" createDate=\"" + DateUtils.getDBTzToCLTzDate(sess, object.getModifyDate()) + "\"");
				out.println(" createDateTime=\"" + createDateTime + "\"");

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
