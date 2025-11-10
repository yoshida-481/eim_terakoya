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
	String prmParentObjId = request.getParameter("parentObjId");
	String prmObjId = request.getParameter("objId");
	
	//Message
	String message = null;
	Object[] paramId = {
			"objId=" + prmObjId,
			"parentObjId=" + prmParentObjId
			};
	
	try
	{
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
		
		//User
		user = (EIMUser)sess.getAttribute("USER");
		
		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
		
		
		//Object Type Favorite
		//EIMObjectType objType = ObjectUtils.getObjectTypeByName(sess, "お気に入り");
	
		//Object Type MyDocument
		EIMObjectType objTypeMyDoc = ObjectUtils.getObjectTypeByName(sess, "マイドキュメント");
		
		//Get MyDocument Object
		EIMObject myDocObj = ObjectUtils.getObjectByTypeAndName(sess, objTypeMyDoc, user.getCode());
		if(myDocObj == null || !SecurityUtils.authorized(sess, myDocObj, user, EIMAccessRole.READ))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}
		
		//Relation Type
		EIMRelationType relType = RelationUtils.getRelationTypeByName(sess, "ドキュメント");
		
		//Parent Object
		EIMObject parentObj = ObjectUtils.getObjectById(sess, Long.parseLong(prmParentObjId));
		if(parentObj == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOFOLDER");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOFOLDER");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}
		// Security Check
		boolean isEnabledParent = SecurityUtils.authorized(sess, parentObj, sess.getUser(), EIMAccessRole.CREATE);
		if(isEnabledParent != true)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOCREATEROLE");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOCREATEROLE");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}
		
		//ドロップ先がゴミ箱の場合はNG
		if (AppObjectUtil.isObjectInRecycle(sess, parentObj)) {
			message = EIMResource.getMessage(sess, "EIM.ERROR.INPUT.CREATEFILEINRECYCLE");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.INPUT.CREATEFILEINRECYCLE");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}
		
		//ドロップ先がタグの場合はNG
		if(helper.isTypeOfTag(parentObj.getType()))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.CANNOT.MOVE.DOCUMENT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.CANNOT.MOVE.DOCUMENT");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}
		
		//ドロップ先オブジェクトのステータスチェック
		long stsKind = parentObj.getStatus() != null ? parentObj.getStatus().getType().getKind() : AppConstant.STATUS_TYPE_KIND_ID_NONE;
		if ((helper.isTypeOfFolderWithWorkflow(parentObj) || helper.isTypeOfFolderUnderFolderWithWorkflow(parentObj))
				&& stsKind != AppConstant.STATUS_TYPE_KIND_ID_EDITTING) 
		{	
			// 上位WFのステータスが「編集中」以外の場合はエラー
			message = EIMResource.getMessage(sess, "EIM.ERROR.CANNOT.MOVE.DOCUMENT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.CANNOT.MOVE.DOCUMENT");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		
		}
		
		//Object
		EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		if(object == null || !SecurityUtils.authorized(sess, object, user, EIMAccessRole.UPDATE))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODOCUMENT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODOCUMENT");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}
		
		//Relation
		EIMRelation relation = RelationUtils.getRelationByParentAndChild(sess, relType, myDocObj, object);
		if(relation == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODOCUMENT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODOCUMENT");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}
		
		//ドロップ先オブジェクトのパスを取得
		String path = AppObjectUtil.getPath(parentObj);
		if(path == null)
		{
			// ワークスペースの場合、パス属性の値を保持していない
			path = "/";
		}
		path += parentObj.getName() + "/";
		
		//ドキュメントオブジェクトの属性を更新する。
		AttributeUtil.updateAttributeForMove(sess, object, parentObj, myDocObj, path, false, helper);
		AppObjectUtil.setPath(sess, object, path);
		
		//上位フォルダからのステータス引継ぎ
		if (parentObj.getStatus() != null) {
			WorkFlowUtils.updateObjectStatus(sess, object, parentObj.getStatus());
			
			//「上位WFフォルダ」属性も登録
			EIMAttribute attrOfHigherWFFolder = parentObj.getAttribute(helper.getAttrNameDocumentHigherWFFolder());
			if (attrOfHigherWFFolder == null)
			{	//WF付フォルダ直下
				ObjectAttributeUtils.setAttribute(sess, object, helper.getAttrTypeOfHigherWFFolder(), parentObj.getId());
			}
			else 
			{	//「WF付フォルダ下のフォルダ」の下
				AppObjectUtil.setAttr(sess, object, attrOfHigherWFFolder);
			}
		}
		
		//Set Security
		EIMSecurity sec = parentObj.getSecurity();
		SecurityUtils.setSecurity(sess, object, sec);
		
		//Delete Relation
		RelationUtils.deleteRelation(sess, relation);
		
		//Create Relation
		if(!SecurityUtils.authorized(sess, object, user, EIMAccessRole.CREATE_RELATION))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOCREATERELROLE");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOCREATERELROLE");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}
		relation = RelationUtils.createRelation(sess, relType, parentObj, object, EIMConstant.DEPU_CHECK_NAME_REV);

		// ### SEARCH FRAMEWORK 検索FW更新通知 オブジェクトID・処理種別キーを指定
		AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), "SEARCHFW_MYDOCUMENT_DOCUMENT");
		AppUpdateNoticeUtils.updateNoticeInsertParent(sess, parentObj, "SEARCHFW_MYDOCUMENT_PARENT_FOLDER", "SEARCHFW_MYDOCUMENT_PARENT_WORKSPACE", null);
		
		//Access
		AccessUtils.createAccess(sess, object, "EIM.ACCESS.TYPE.INITIALREGIST");

		//Create Operation History
		OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.MOVE_MYDOCUMENT, 
				EIMConstant.TARGET_UPDATE, EIMConstant.OBJECT, object,
				null, null, null, path);
		
		//XML
		out.println("<object");
			out.println(" objId=\"" + object.getId() + "\"");
			out.println(">");
		out.println("</object>");
		
		//Commit
		sess.commit();
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
