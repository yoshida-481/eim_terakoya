<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
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
	EIMUser  user = null;

	//Parameter
	String prmObjId = request.getParameter("objId");

	//Message
	String message = null;
	Object[] paramId = {
			"objId=" + prmObjId
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
		user = (EIMUser)sess.getAttribute("USER");
		
		//Object
		EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		if(object == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODOCUMENT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODOCUMENT");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}
		
		// PDF変換オブジェクトのオブジェクトタイプ
//		EIMObjectType objTypePDFConv = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_CONVPDF"));

		// PDF変換失敗のドキュメントかチェック
//		if (!AppObjectUtil.isPDFConvertFailed(sess, object, objTypePDFConv))
		// 公開処理失敗属性を保持するドキュメントのみ差し戻し対象に変更
		long pdfFailFlg = AppObjectUtil.getIntAttr(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_PUB_PROC_FAIL"), AppConstant.FLAG_OFF);
		if (pdfFailFlg == AppConstant.FLAG_OFF)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.INPUT.NOTPDFFALEDDOCUMENT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.INPUT.NOTPDFFALEDDOCUMENT");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}
		
		// Systemセキュリティに属するユーザかチェック
		if(!AppSecurityUtils.authorizedBySystemSecurity(sess, EIMAccessRole.UPDATE))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOREPLACEROLE");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOREPLACEROLE");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}

		//essential attribute
		EIMAttributeType attTypeOfModifyUser = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_MODIFY_USER"));
		EIMAttributeType attTypeOfModifyDate = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_MODIFY_DATE"));
		EIMAttributeType attTypeOfCreateDate = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_CREATE_DATE"));
		EIMAttributeType attTypeOfFizeSize = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_SIZE"));

		String modifyDate = DateUtils.getDBTzToCLTzDate(sess, object.getModifyDate());
		String createDate = DateUtils.getDBTzToCLTzDate(sess, object.getCreateDate());
		EIMFile file = FileUtils.getFile(sess, object, FileUtils.getDefaultFormat(sess, object.getType()));
		
		//XML
		out.println("<object");
		
			//For Create Document - Default Create User Information
			out.println(" userId=\"" + user.getId() + "\"");
			out.println(" userName=\"" + StringUtils.xmlEncode(user.getName()) + "\"");
		
			out.println(" objId=\"" + object.getId() + "\"");
			out.println(" objTypeId=\"" + object.getType().getId() + "\"");
			out.println(" objTypeName=\"" + StringUtils.xmlEncode(object.getType().getDefName()) + "\"");
			out.println(" objName=\"" + StringUtils.xmlEncode(object.getName()) + "\"");
			out.println(" latest=\"" + object.getLatest() + "\"");
			out.println(" createUserName=\"" + StringUtils.xmlEncode(object.getCreateUser().getName()) + "\"");
			out.println(" createDate=\"" + object.getCreateDate() + "\"");
			out.println(" modifyUserName=\"" + StringUtils.xmlEncode(object.getModifyUser().getName()) + "\"");
			out.println(" modifyDate=\"" + object.getModifyDate() + "\"");
			
			//Revision
			out.println(" rev=\"" + "-" + "\"");
			
			//パス
			String path = StringUtils.nullToBlank(AppObjectUtil.getPath(object));
			out.println(" path=\"" + StringUtils.xmlEncode(path) + "\"");

			out.println(" attType_" + attTypeOfModifyUser.getId() + "=\"" + StringUtils.xmlEncode(object.getModifyUser().getName()) + "\"");
			out.println(" attType_" + attTypeOfModifyDate.getId() + "=\"" + modifyDate + "\"");
			out.println(" attType_" + attTypeOfCreateDate.getId() + "=\"" + createDate + "\"");
			out.println(" attType_" + attTypeOfFizeSize.getId() + "=\"" + file.getSize() + "\"");				
			
		out.println(">");
		out.println("</object>");
	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(user.getId(), eime.getMessage(), paramId), eime);
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
