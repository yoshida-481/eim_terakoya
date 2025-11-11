<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>

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
	// 注:
	// isDocumentLinkが"true"でlinkParentObjIdの指定がある場合
	// →ドキュメントリンクの親Objectと子Objectが返却されます
	// 　子Objectが存在しない場合は「リンク元ドキュメントがない」と出力されます
	// isDocumentLinkが"true"でlinkParentObjIdの指定がない場合
	// →ドキュメントの親Object(フォルダなら自身のObject)と最新の子Objectが返却されます
	// 　子Objectが存在しない場合は「リンク元ドキュメントがない」と出力されます
	// isDocumentLinkが"false(指定なし)"の場合
	// →ドキュメントの親Object(フォルダなら自身のObject)と最新の子Objectが返却されます
	// 　子Objectが存在しない場合は「ドキュメント/フォルダがない」と出力されます 
	String prmObjId = request.getParameter("objId");
	String prmIsFolder = request.getParameter("isFolder");
	String prmIsURLLogin = request.getParameter("isURLLogin");
	String prmIsDocumentLink = request.getParameter("isDocumentLink");
	String prmLinkParentObjId = request.getParameter("linkParentObjId");

	//Message
	String message = null;
	Object[] paramId = {
			"objId=" + prmObjId,
			"isFolder=" + prmIsFolder,
			"isURLLogin=" + prmIsURLLogin,
			"isDocumentLink=" + prmIsDocumentLink,
			"linkParentObjId=" + prmLinkParentObjId
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

		user = (EIMUser)sess.getAttribute("USER");

		//Object
		EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		if(object == null)
		{
			out.clear();
			
			if( !"true".equals(prmIsDocumentLink) ) {	// ドキュメントリンクではない場合
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOTEXIST.DOCFOLTAG");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOTEXIST.DOCFOLTAG");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			}
			else {	// ドキュメントリンクの場合
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.ZIPDL.DOCLINK.BASE.NOTFOUND");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.ZIPDL.DOCLINK.BASE.NOTFOUND");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));				
			}
			return;
		}

		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

		//アクセス権チェック
		try {
			helper.checkAccessibleStatusSelf(object, true);
		} catch (EIMException e) {
			if (!e.getMessageKey().equals("EIM.ERROR.LOGIC.NOACCESS")) throw e;
			message = e.getMessage();
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage(e.getMessageKey(), e.getMessageParams());
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}

		EIMObject parentObj = null;
		if( (!"true".equals(prmIsDocumentLink)) || (StringUtils.nullToBlank(prmLinkParentObjId) == "") )	{ // ドキュメントリンクではない場合
			if(!object.getLatest())
			{
				//最新バージョンのオブジェクト取得
				object = AppLogicUtil.getLatestObject(sess, object);
			}

			if ("true".equals(prmIsURLLogin)
					&& (helper.isTypeOfFolder(object.getType()) || helper.isTypeOfTag(object.getType()) || helper.isTypeOfWorkspace(object.getType()))
					) {
				//URLからのログインで呼び出された場合(prmIsURLLogin=true)、オブジェクトが「フォルダ、タグ、ワークスペース」のいずれかなら、親は求めずオブジェクト自身を返す
				parentObj = object;
			} else {
				parentObj = helper.getUpperObject(object);
				if (parentObj == null || helper.isTypeOfMyDocument(parentObj.getType())) {
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOFOLDER");
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOFOLDER");
					log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
					return;
				}
			}
		}
		else {	// ドキュメントリンクの場合
			// 親オブジェクトの取得
			parentObj = ObjectUtils.getObjectById(sess, Long.parseLong(prmLinkParentObjId));
			if (parentObj == null) {
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOFOLDER");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOFOLDER");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
				return;
			}
			
			//Relation Type
			EIMRelationType relTypeLink = RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_LINK"));
			// リンクリレーションの存在確認
			if( RelationUtils.getRelationByParentAndChild(sess, relTypeLink, parentObj, object) == null ) {
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOTEXIST.DOCLINK");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOTEXIST.DOCLINK");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
				return;
			}
		}
		
		// 指定の子オブジェクトが画面のファイル一覧に表示可能なオブジェクトか否か
		boolean isChildEnabled = true;
		
		// 親オブジェクトがごみ箱、もしくはごみ箱配下の場合
		if(AppObjectUtil.isObjectInSystemRecycle(sess, parentObj)){

			isChildEnabled = false;
			
			// ごみ箱オブジェクトを親オブジェクトに設定
			parentObj = AppObjectUtil.getObject(sess, 
					EIMConfig.get("OBJECT_TYPE_NAME_RECYCLE"), EIMConfig.get("OBJECT_TYPE_NAME_RECYCLE"));

			if (parentObj == null) {
				message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.SYSTEMERROR");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
				return;
			}
		}
		
		//XML
		out.println("<object");
			out.println(" parentObjId=\"" + parentObj.getId() + "\"");
			out.println(" objId=\"" + object.getId() + "\"");
			out.println(" isChildEnable=\"" + Boolean.toString(isChildEnabled) + "\"");
			if (AppObjectUtil.isWsRecycleObject(sess, parentObj)) {
				out.println(" isWsRecycle=\"true\"");
			}
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
