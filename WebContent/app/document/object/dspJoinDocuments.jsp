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
	
	//Parameter
	/* EIMObjID of Folder/WorkSpace */
	String prmObjId = request.getParameter("objId");
	/* Document Type ID of Menu-Selected */
	String prmObjTypeId = request.getParameter("objTypeId");
	String prmDocIds = request.getParameter("docIds");

	//Message
	String message = null;
	Object[] paramId = {
			"objId=" + prmObjId,
			"objTypeId=" + prmObjTypeId,
			"docIds=" + prmDocIds
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
		
		// ドキュメントタイプチェック
		if (prmObjTypeId != null && ObjectUtils.getObjectTypeById(sess, Long.parseLong(prmObjTypeId)) == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.DOCTYPE.NOTFOUND");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.DOCTYPE.NOTFOUND");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}
		
		// 親オブジェクトの取得
		EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		if(object == null || !SecurityUtils.authorized(sess, object, sess.getUser(),EIMAccessRole.READ))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.PDFJOIN.NOFOLWS");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.PDFJOIN.NOFOLWS");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}
		
		//権限チェック
		if( prmDocIds != null && !"".equals(prmDocIds) ) {
			// ドキュメントが選択されていた場合
			String[] ids = prmDocIds.split(",");
			if( PublishAddonUtils.checkPDFJoinAuth( sess, prmDocIds.split(","), Long.parseLong(prmObjId)) == false ) {
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOPDFJOINROLE");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOPDFJOINROLE");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
				return;
			}
			
			// ステータスが公開中 and 公開ファイル拡張子が PDF か否か
			EIMFormat publicFormat = FileUtils.getFormatByName(sess, EIMConfig.get("FORMAT_NAME_PUBLIC"));
			for( int i = 0; i < ids.length; i++ ) {
				EIMObject docObj = ObjectUtils.getObjectById(sess,Long.parseLong(ids[i]));
				if( docObj == null ) {
					message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					message = EIMResource.getMessage("EIM.ERROR.SYSTEMERROR");
					log.error(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
					return;
				}
				if( docObj.getStatus() == null ) {
					message = EIMResource.getMessage(sess,"EIM.ERROR.LOGIC.NOTPUBLIC", new Object[]{docObj.getName()});
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					return;
				}
				if( docObj.getStatus().getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_PUBLIC ) {
					message = EIMResource.getMessage(sess,"EIM.ERROR.LOGIC.NOTPUBLIC", new Object[]{docObj.getName()});
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					return;
				}
				EIMFile filePDF = FileUtils.getFile(sess, docObj, publicFormat);
				if (filePDF == null) {
					message = EIMResource.getMessage(sess,"EIM.ERROR.LOGIC.NOTPUBLIC", new Object[]{docObj.getName()});
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					return;
				}
				if( !filePDF.getExt().equals(EIMConfig.get("PDF_EXT")) ) {
					message = EIMResource.getMessage(sess,"EIM.ERROR.LOGIC.PUBLICFILE.NOTPDF", new Object[]{docObj.getName()});
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					return;
				}
			}
		} else {
			// ファイル未選択 → 結合先フォルダの権限チェックのみ
			if( PublishAddonUtils.checkPDFJoinFolderAuth( sess, Long.parseLong(prmObjId)) == false ) {
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOPDFJOINROLE");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOPDFJOINROLE");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
				return;
			}
		}
		
		// 親フォルダがごみ箱の場合
		if(AppObjectUtil.isObjectInRecycle(sess, object))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.INPUT.CREATEFILEINRECYCLE");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
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
		EIMObjectType objType = null;
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
			//ワークフロー付きフォルダ下にワークフロー付きドキュメントは作成できない
			if (object.getStatus() != null) {
				out.clear();
				message = EIMResource.getMessage(sess,"EIM.ERROR.LOGIC.CANTSET.UNDERWF.WFDOCUMENT");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.CANTSET.UNDERWF.WFDOCUMENT");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
				return;
			}

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
