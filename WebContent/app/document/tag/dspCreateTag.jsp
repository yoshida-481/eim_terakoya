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
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOWSFOL");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOWSFOL");
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
		
		// 親フォルダがごみ箱の場合
		if (AppObjectUtil.isObjectInRecycle(sess, parentObject)) {
			message = EIMResource.getMessage(sess, "EIM.ERROR.INPUT.CREATETAGINRECYCLE");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.INPUT.CREATETAGINRECYCLE");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}

		/* ワークフロー付フォルダ配下かどうかチェック */
		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
		// ワークフロー付フォルダ配下にワークフロー付フォルダタイプを指定した場合
		if ((helper.isTypeOfFolderWithWorkflow(parentObject) ||
				helper.isTypeOfFolderUnderFolderWithWorkflow(parentObject))) {
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.CANTCREATE.UNDERWF.TAG");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.CANTCREATE.UNDERWF.TAG");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;								
		}

		// タグタイプ存在チェック
		if (prmObjTypeId != null && ObjectUtils.getObjectTypeById(sess, Long.parseLong(prmObjTypeId)) == null) {
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOTAGTYPE");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOTAGTYPE");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId))	;
			return;			
		}
		
		//オブジェクトプロパティ
		out.println("<result>");
		out.println(" <object");
		
			//For Create Document - Default Create User Information
			out.println("  userId=\"" + user.getId() + "\"");
			out.println("  userName=\"" + StringUtils.xmlEncode(user.getName()) + "\"");		
			out.println("  parentObjName=\"" + StringUtils.xmlEncode(parentObject.getName()) + "\"");
			//パス
			String path = AppObjectUtil.getPath(parentObject);
			if (path == null) {
				// ワークスペースの場合、パス属性の値を保持していない
				path = "/";
			}
			out.println(" path=\"" + StringUtils.xmlEncode(path) + "\"");
			
		out.println(" >");
		out.println(" </object>");
		out.println("</result>");
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
