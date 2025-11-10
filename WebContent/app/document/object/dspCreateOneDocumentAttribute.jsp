<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "common.bo.*" %>
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
	
	//Parameter
	String prmObjId = request.getParameter("objId");
	String prmObjTypeId = request.getParameter("objTypeId");
	String prmCreateType = request.getParameter("createType");

	//Message
	String message = null;
	Object[] paramId = {
			"objId=" + prmObjId,
			"objTypeId=" + prmObjTypeId,
			"createType=" + prmCreateType
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
		
		// 親フォルダの取得
		EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		if(object == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOFOLDER");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOFOLDER");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}
		
		// 親フォルダがごみ箱の場合
		if(AppObjectUtil.isObjectInRecycle(sess, object))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.INPUT.CREATEFILEINRECYCLE");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.INPUT.CREATEFILEINRECYCLE");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}
		
		//アクセス権限チェック
		if (SecurityUtils.authorized(sess, object, sess.getUser(), EIMAccessRole.CREATE) != true)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOCREATEROLE");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOCREATEROLE");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}
		
		//		オブジェクトタイプチェック
		if (prmObjTypeId != null && ObjectUtils.getObjectTypeById(sess, Long.parseLong(prmObjTypeId)) == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODOCUMENTTYPE");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODOCUMENTTYPE");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}
		
		//ワークフロー付きフォルダ下の場合は、編集中以外は作成できない
		//ワークフロー付きフォルダ下の場合は、ワークフロー付きドキュメントは作成できない
		EIMObjectType objType = null;
		if (object.getStatus() != null) {
			if (object.getStatus().getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_EDITTING) {
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOCREATEROLE");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOCREATEROLE");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
				return;
			}
			if(prmCreateType.equals("document")) {
				
				if (StringUtils.isBlank(prmObjTypeId))
					objType = ObjectUtils.getObjectTypeByName(sess, "ドキュメント");
				else
					objType = ObjectUtils.getObjectTypeById(sess, Long.parseLong(prmObjTypeId));
				
				if (WorkFlowUtils.getWorkFlowByType(sess, objType) != null) {
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.CANTSET.UNDERWF.WFDOCUMENT");
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.CANTSET.UNDERWF.WFDOCUMENT");
					log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
					return;
				}
			}
		}


		//オブジェクトプロパティ
		out.println("<results>");
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
			String path = AppObjectUtil.getPath(object);
			if(path == null) {
				// ワークスペースの場合、パス属性の値を保持していない
				path = "/";
			}
			out.println(" path=\"" + StringUtils.xmlEncode(path) + "\"");
			
		out.println(">");
		out.println("</object>");
		
		// ワークフロー
		
		if (prmObjTypeId == null || prmObjTypeId.length() == 0)
		{
			// Object Type Document
			objType = ObjectUtils.getObjectTypeByName(sess, "ドキュメント");
		}
		else
		{
			//Object Type
			objType = ObjectUtils.getObjectTypeById(sess, Long.parseLong(prmObjTypeId));
		}
		
		//WorkFlow
		EIMWorkFlow workFlow = WorkFlowUtils.getWorkFlowByType(sess, objType);
		if(workFlow != null)
		{
			//Status Type List
			List statusList = workFlow.getStatusTypeList();
			
			//Root Node
			out.println("<statusTypeList workFlowId=\"" + workFlow.getId() + "\" workFlowName=\"" + StringUtils.xmlEncode(workFlow.getName()) + "\">");
			
			for(int i = 0; i < statusList.size(); i++)
			{
				//Status Type
				EIMStatusType statusType = (EIMStatusType)statusList.get(i);
				
				//XML
				out.println("<statusType");
					out.println(" statusTypeId=\"" + statusType.getId() + "\"");
					out.println(" statusTypeName=\"" + StringUtils.xmlEncode(statusType.getName()) + "\"");
					out.println(" step=\"" + statusType.getStep() + "\"");
				out.println(">");
				out.println("</statusType>");
			}
			
			//End Root Node
			out.println("</statusTypeList>");
		}
		

		out.println("</results>");
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
