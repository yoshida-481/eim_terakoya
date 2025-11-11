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
	EIMUser user = null;
	
	//Parameter
	String prmObjId = request.getParameter("objId");
	String prmObjTypeId = request.getParameter("objTypeId");

	//Message
	String message = null;
	Object[] paramId = {
			"objId=" + prmObjId,
			"objTypeId=" + prmObjTypeId
			};
	
	try
	{
		//Session
		sess = EIMUtils.getSession(request);
		if (sess == null) {
			message = EIMResource.getMessage(request, "EIM.ERROR.SESSIONTIMEOUT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.SESSIONTIMEOUT");
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}
		
		//User
		user = (EIMUser)sess.getAttribute("USER");
		
		// 親フォルダの取得
		EIMObject parentObject = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		if (parentObject == null) {
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOFOLDER");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOFOLDER");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId))	;
			return;
		}
		
		// 親フォルダがごみ箱の場合
		if (AppObjectUtil.isObjectInRecycle(sess, parentObject)) {
			message = EIMResource.getMessage(sess, "EIM.ERROR.INPUT.CREATEFOLDERINRECYCLE");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.INPUT.CREATEFOLDERINRECYCLE");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}
		

		// フォルダタイプチェック
		if (prmObjTypeId != null && ObjectUtils.getObjectTypeById(sess, Long.parseLong(prmObjTypeId)) == null) {
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOFOLDERTYPE");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOFOLDERTYPE");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId))	;
			return;			
		}
		
		/* ワークフロー付フォルダ配下にワークフロー付フォルダタイプ指定チェック */
		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
		// フォルダタイプ
		EIMObjectType objType = null;
		
		// 一般フォルダ
		if(prmObjTypeId == null || prmObjTypeId.length() == 0)
		{
			objType = ObjectUtils.getObjectTypeByName(sess, "フォルダ");
		}
		else
		{
			objType = ObjectUtils.getObjectTypeById(sess, Long.parseLong(prmObjTypeId));
		}
		
		// ワークフロー付フォルダ配下にワークフロー付フォルダタイプを指定した場合
		if ((helper.isTypeOfFolderWithWorkflow(parentObject) ||
				helper.isTypeOfFolderUnderFolderWithWorkflow(parentObject))
				&& WorkFlowUtils.getWorkFlowByType(sess, objType) != null) {
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.CANTSET.UNDERWF.WFFOLDER");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.CANTSET.UNDERWF.WFFOLDER");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId))	;
			return;								
		}
		
		/* 親オブジェクトのセキュリティのロールチェック */
		if (!SecurityUtils.authorized(sess, parentObject, sess.getUser(), EIMAccessRole.CREATE)) {
			// 作成権限がありません
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOCREATEROLE");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOCREATEROLE");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId))	;
			return;
		}
		
		/* ステータスチェック */
		// 上位WFフォルダのステータスを持っているため
		// 作成する直上のオブジェクトのステータスをチェック対象とする
		if (parentObject.getStatus() != null) {
			// 親オブジェクトのステータスが「編集中」以外の場合はエラー
			if (parentObject.getStatus().getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_EDITTING) {
				// 作成権限がありません
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOCREATEROLE");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOCREATEROLE");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId))	;
				return;
			}			
		}
	
		/* フォルダ構成管理チェック */
		if (!AppSecurityUtils.authorizedLowFolderSecurity(sess, parentObject, sess.getUser(), EIMAccessRole.UPDATE)) {
			// 作成権限がありません
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOCREATEROLE");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOCREATEROLE");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId))	;
			sess.rollback();
			return;							
		}
			
		//オブジェクトプロパティ
		out.println("<results>");
		out.println("<object");
		
			//For Create Document - Default Create User Information
			out.println(" userId=\"" + user.getId() + "\"");
			out.println(" userName=\"" + StringUtils.xmlEncode(user.getName()) + "\"");
		
			out.println(" parentObjId=\"" + parentObject.getId() + "\"");
			out.println(" parentObjTypeId=\"" + parentObject.getType().getId() + "\"");
			out.println(" parentObjTypeName=\"" + StringUtils.xmlEncode(parentObject.getType().getDefName()) + "\"");
			out.println(" parentObjName=\"" + StringUtils.xmlEncode(parentObject.getName()) + "\"");
			out.println(" latest=\"" + parentObject.getLatest() + "\"");
			out.println(" createUserName=\"" + StringUtils.xmlEncode(parentObject.getCreateUser().getName()) + "\"");
			out.println(" createDate=\"" + parentObject.getCreateDate() + "\"");
			out.println(" modifyUserName=\"" + StringUtils.xmlEncode(parentObject.getModifyUser().getName()) + "\"");
			out.println(" modifyDate=\"" + parentObject.getModifyDate() + "\"");
									
			//Revision
			out.println(" rev=\"" + "-" + "\"");
			
			//パス
			String path = AppObjectUtil.getPath(parentObject);
			if (path == null) {
				// ワークスペースの場合、パス属性の値を保持していない
				path = "/";
			}
			out.println(" path=\"" + StringUtils.xmlEncode(path) + "\"");
			
		out.println(">");
		out.println("</object>");
		
		//WorkFlow
		EIMWorkFlow workFlow = WorkFlowUtils.getWorkFlowByType(sess, objType);
		if (workFlow != null) {
			//Status Type List
			List statusList = workFlow.getStatusTypeList();
			
			//Root Node
			out.println("<statusTypeList workFlowId=\"" + workFlow.getId() + "\" workFlowName=\"" + StringUtils.xmlEncode(workFlow.getName()) + "\">");
			
			for(int i = 0; i < statusList.size(); i++) {
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
