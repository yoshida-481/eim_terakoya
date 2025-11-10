<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>
<%@ page import = "jp.co.ctc_g.eim.framework.common.util.EIMThreadContext"%>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");
	
	//Session
	EIMSession sess = null;
	EIMUser user = null;
	boolean sessPutFlag = false;
	
	//Parameter
	String prmParentObjId = EIMUtils.getParameter(request, "objId");
	String prmObjTypeId = EIMUtils.getParameter(request, "objTypeId");
	String prmCreateObjName = EIMUtils.getParameter(request, "objName");
	String prmCreateUserId = EIMUtils.getParameter(request, "createUserId");
	String prmIsDspAttributeInfo = EIMUtils.getParameter(request, "isDspAttributeInfo");

	//Message
	String message = null;
	Object[] paramId = {
			"objId=" + prmParentObjId,
			"objTypeId=" + prmObjTypeId,
			"objName=" + prmCreateObjName,
			"isDspAttributeInfo=" + prmIsDspAttributeInfo
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
		
		// ThreadLocalコンテキストにセッションを設定
		if(EIMThreadContext.getEIMSession() == null)
		{
			EIMThreadContext.putEIMSession(sess);
			sessPutFlag = true;
		}
		
		user = (EIMUser)sess.getAttribute("USER");

		//Parent Object
		EIMObject parentObject = ObjectUtils.getObjectById(sess, Long.parseLong(prmParentObjId));
		if(parentObject == null)
		{
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

		// タグタイプチェック
		if (prmObjTypeId != null && ObjectUtils.getObjectTypeById(sess, Long.parseLong(prmObjTypeId)) == null) {
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOTAGTYPE");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOTAGTYPE");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId))	;
			return;			
		}
		
		// タグタイプ
		EIMObjectType objType = null;
		
		// 一般タグ
		if(prmObjTypeId == null || prmObjTypeId.length() == 0)
		{
			objType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_TAG"));
		}
		else
		{
			objType = ObjectUtils.getObjectTypeById(sess, Long.parseLong(prmObjTypeId));
		}
		
		//作成者を設定
		EIMUser createUser = null;
		if(prmCreateUserId != null)
		{
			createUser = UserUtils.getUserById(sess, Long.parseLong(prmCreateUserId));
		}
		
		//Path
		String path = AppObjectUtil.getPath(parentObject);
		if(path == null){
			// ワークスペースの場合、パス属性の値を保持していない
			path = "/";
		}
		path += parentObject.getName() + "/";

		// タグオブジェクトの作成
		EIMObject tagObj = TagUtil.createTag(sess, objType, parentObject, prmCreateObjName, createUser);
		
		boolean isUpdateOnlyName = (prmIsDspAttributeInfo.equals("0"));
		//属性情報の更新
		UpdateAttributeHelper.updateAttribute(sess, request, tagObj, isUpdateOnlyName);
		//リスト値表示色オブジェクトの更新
		DisplayColorUtil.updateDisplayColor(sess, request, tagObj);
				
		//Access
		AccessUtils.createAccess(sess, tagObj, "EIM.ACCESS.TYPE.INITIALREGIST");
		
		//Create Operation History
		OperationHistoryUtils.create(sess,AppConstant.DOCUMENT, AppConstant.CREATE_TAG, 
				EIMConstant.TARGET_CREATE, EIMConstant.OBJECT_TYPE, tagObj,
				null, null, null, path);

		// SearchFramework 検索FW更新通知 対象：タグ + 親フォルダ・親ワークスペース   
		AppUpdateNoticeUtils.updateNoticeInsert(tagObj.getId(), "SEARCHFW_CREATE_TAG");
		AppUpdateNoticeUtils.updateNoticeInsertParent(sess, parentObject, "SEARCHFW_CREATE_TAG_PARENT_FOLDER", "SEARCHFW_CREATE_TAG_PARENT_WORKSPACE", null);
		
		//Commit
		sess.commit();
		out.println("<object objId=\"" + tagObj.getId() + "\" />");
	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(user.getId(), eime.getMessage(), paramId), eime);
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
			if(sessPutFlag) {
				EIMThreadContext.removeEIMSession();
				sessPutFlag = false;
			}
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
