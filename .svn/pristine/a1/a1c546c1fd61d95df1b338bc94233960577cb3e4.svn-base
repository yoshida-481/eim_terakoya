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
	String prmIsDspAttributeInfo = EIMUtils.getParameter(request, "isDspAttributeInfo");
	String prmAttTypeNameAllocate = EIMUtils.getParameter(request, "attType_nameAllocate");

	//Message
	String message = null;
	Object[] paramId = {
			"objId=" + prmParentObjId,
			"objTypeId=" + prmObjTypeId,
			"objName=" + prmCreateObjName,
			"isDspAttributeInfo=" + prmIsDspAttributeInfo,
			"attType_nameAllocate=" + prmAttTypeNameAllocate
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
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOFOLDER");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOFOLDER");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId))	;
			return;
		}
		
		// Windows禁止文字チェック
		AppObjectUtil.checkValidateFName(sess, prmCreateObjName);
		
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
		
		// 下位フォルダ管理セキュリティ取得
		long sec_id = AppObjectUtil.getIntAttr(sess, parentObject, EIMConfig.get("ATTR_NAME_WORKSPACE_LOW_FOLDER_SEC"), Integer.MIN_VALUE);		
		if (sec_id != Integer.MIN_VALUE) {					
			// 下位フォルダ管理セキュリティのロールチェック	
			if (!AppSecurityUtils.authorizedLowFolderSecurity(sess, parentObject, user, EIMAccessRole.CREATE)) {
				// 作成権限がありません
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOCREATEROLE");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOCREATEROLE");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId))	;
				return;							
			}
		}
		
		//Path
		String path = AppObjectUtil.getPath(parentObject);
		if(path == null){
			// ワークスペースの場合、パス属性の値を保持していない
			path = "/";
		}
		path += parentObject.getName() + "/";

		//指定されたフォルダオブジェクトの作成
		EIMObject object = ObjectUtils.createObject(sess, objType, prmCreateObjName);
		if(object == null)
		{
			sess.rollback();
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.FAILMAKEOBJ");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			response.sendError(1007);
			return;
		}
		
		//Relation Type Document
		EIMRelationType relType = RelationUtils.getRelationTypeByName(sess, "ドキュメント");
		
		//Create Relation
		 RelationUtils.createRelation(sess, relType, parentObject, object, EIMConstant.DEPU_CHECK_NAME);
		
		//Set Path
		AppObjectUtil.setPath(sess,object,path);
		
		//Security
		EIMSecurity sec = parentObject.getSecurity();
		if(sec != null)
		{
			SecurityUtils.setSecurity(sess, object, sec);
		}
		
		// WF付フォルダ直下に作成する場合
		if (helper.isTypeOfFolderWithWorkflow(parentObject)) 
		{
			// 属性「上位WFフォルダ」設定
			AppObjectUtil.setAttr(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_HIGHER_WF_FOLDER"), parentObject.getId());						
			// 上位WF付フォルダのステータスを設定(引継ぐ)
			WorkFlowUtils.updateObjectStatus(sess, object, parentObject.getStatus());						
		} 
		else 
		{
			// 親フォルダの「上位WFフォルダ」属性を取得
			long higherWfFolderId = AppObjectUtil.getIntAttr(sess, parentObject, EIMConfig.get("ATTR_NAME_DOCUMENT_HIGHER_WF_FOLDER"), Integer.MIN_VALUE);
			
			if (higherWfFolderId != Integer.MIN_VALUE) 
			{
				// 属性「上位WFフォルダ」設定
				AppObjectUtil.setAttr(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_HIGHER_WF_FOLDER"), higherWfFolderId);
				// 上位WFフォルダ取得
				EIMObject higherWfFolder = ObjectUtils.getObjectById(sess, higherWfFolderId);
				// 上位WF付フォルダのステータスを設定(引継ぐ)
				WorkFlowUtils.updateObjectStatus(sess, object, higherWfFolder.getStatus());												
			}
		}
		
		// 下位管理セキュリティ設定
		if (sec_id != Integer.MIN_VALUE)
		{
			AppObjectUtil.setAttr(sess, object, EIMConfig.get("ATTR_NAME_WORKSPACE_LOW_FOLDER_SEC"), sec_id);			
		}
		
		boolean isUpdateOnlyName = (prmIsDspAttributeInfo.equals("0"));
				
		//リスト値表示色オブジェクトの更新
		DisplayColorUtil.updateDisplayColor(sess, request, object);
		
		//属性情報の更新
		UpdateAttributeHelper.updateAttribute(sess, request, object, isUpdateOnlyName);
		
		// SearchFramework 検索FW更新通知 対象：フォルダ + 親フォルダ・親ワークスペース
		AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), "SEARCHFW_CREATE_FOLDER");
		AppUpdateNoticeUtils.updateNoticeInsertParent(sess, parentObject, "SEARCHFW_CREATE_FOLDER_PARENT_FOLDER", "SEARCHFW_CREATE_FOLDER_PARENT_WORKSPACE", null);
		
		//Access
		AccessUtils.createAccess(sess, object, "EIM.ACCESS.TYPE.INITIALREGIST");
		
		//Create Operation History
		OperationHistoryUtils.create(sess,AppConstant.DOCUMENT, EIMConstant.CREATE_FOLDER, 
				EIMConstant.TARGET_CREATE, EIMConstant.OBJECT_TYPE, object,
				null, null, null, path);
				
		//Commit
		sess.commit();
		out.println("<object objId=\"" + object.getId() + "\" />");
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
