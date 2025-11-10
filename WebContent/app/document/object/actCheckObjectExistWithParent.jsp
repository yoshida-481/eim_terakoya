<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>

<%
	/* 指定のオブジェクトが存在するか否か、ごみ箱配下か否かを判定します。 */
	/* 親フォルダがごみ箱の場合、有無のみ判定します */

	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");
	
	//Session
	EIMSession sess = null;
	EIMUser user = null;
	
	// Parameter
	String prmObjId =  EIMUtils.getParameter(request, "ObjectId");
	String prmParentObjId = EIMUtils.getParameter(request, "ParentObjectId");
	String prmIsCheckDeleteRoll = request.getParameter("IsCheckDeleteRoll");
	String prmIsDocumentLink = request.getParameter("IsDocumentLink");
	String prmIsCheckCut = request.getParameter("IsCheckCut");

	//Message
	String message = null;
	Object[] paramId = {
			"objId=" + prmObjId,
			"parentObjId" + prmParentObjId,
			"isDocumentLink" + prmIsDocumentLink
			};

	boolean isObjExist = false;	// 指定オブジェクトの存在有無

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
		//User
		user = (EIMUser)sess.getAttribute("USER");
		
		prmIsCheckCut = (prmIsCheckCut == null)? "false":prmIsCheckCut;

		// チェック対象オブジェクトの取得
		EIMObject obj = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));

		if( !prmIsDocumentLink.equals("true")) {	// ドキュメント or フォルダの場合 or タグの場合

			//親オブジェクト内にオブジェクトが存在するか否か
			//(呼び出し元から親オブジェクトIDが渡されてきていない場合はチェックしない)
			if(prmParentObjId == null || AppObjectUtil.isObjectInParent(sess, Long.parseLong(prmParentObjId), Long.parseLong(prmObjId)))
			{
				if(obj != null)
				{
					isObjExist = true;
					if(prmIsCheckDeleteRoll.equals("true"))
					{
						//Check Delete Role
						if(obj.getSecurity() != null)
						{
							if(!SecurityUtils.authorized(sess, obj, sess.getUser(), EIMAccessRole.DELETE))
							{
								message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODELETEROLE");
								out.println(AppMessageUtils.makeErrorTagByMessage(message));
								message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODELETEROLE");
								log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
								return;
							}
						}
					}
				}
			}
			if(!isObjExist)
			{
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODOCORFOLORTAG");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODOCORFOLORTAG");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
				return;
			}
		}
		else {	// ドキュメントリンクの場合
			
			if( prmParentObjId != null ) {
				// 親オブジェクトの取得
				EIMObject parentObj = ObjectUtils.getObjectById(sess, Long.parseLong(prmParentObjId));
				if(parentObj != null && SecurityUtils.authorized(sess, parentObj, sess.getUser(),EIMAccessRole.READ)) {
					// 子オブジェクトの取得
					if( obj != null && SecurityUtils.authorized(sess, obj, sess.getUser(),EIMAccessRole.READ)) {
						//Relation Type
						EIMRelationType relTypeLink = RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_LINK"));
						// リンクリレーションの存在確認
						if( RelationUtils.getRelationByParentAndChild(sess, relTypeLink, parentObj, obj) != null ) {
							isObjExist = true;
						}					
					}
				}
			}
			if(!isObjExist)
			{
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOTEXIST.DOCLINK");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOTEXIST.DOCLINK");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
				return;
			}
			if(prmIsCheckDeleteRoll.equals("true"))
			{
				// 切り取り対象オブジェクトのセキュリティのチェック
				if (obj.getSecurity() != null) {
					if (!SecurityUtils.authorized(sess, obj, sess.getUser(), EIMAccessRole.UPDATE)) {
						// 切り取り/貼付け権限がありません。
						throw new EIMException(sess, "EIM.ERROR.LOGIC.NOCUTANDPASTEROLE");
					}
				}
			}
		}
		
		if(prmIsCheckDeleteRoll.equals("true"))
		{
			// 条件判定ヘルパー作成
			AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
			// 切り取り元フォルダの取得
			EIMObject baseParentObj = ObjectUtils.getObjectById(sess,Long.parseLong(prmParentObjId));;
			if (baseParentObj != null) {
				
				// 切り取り元フォルダの上位WFフォルダのステータスのチェック
				if ((helper.isTypeOfFolderWithWorkflow(baseParentObj) 
						|| helper.isTypeOfFolderUnderFolderWithWorkflow(baseParentObj))
							&& (baseParentObj.getStatus() != null
									&& baseParentObj.getStatus().getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_EDITTING)) {	// 上位WFのステータスが「編集中」以外の場合はエラー
					// 切り取り/貼付け権限がありません。
					throw new EIMException(sess, "EIM.ERROR.LOGIC.NOCUTANDPASTEROLE");
				}
				
				// 対象オブジェクトがフォルダの場合
				if (helper.isTypeOfFolder(obj.getType())) {
					// フォルダ構成管理チェック
					if (!AppSecurityUtils.authorizedLowFolderSecurity(sess, baseParentObj, sess.getUser(), EIMAccessRole.UPDATE)) {
						// 切り取り/貼付け権限がありません。
						throw new EIMException(sess, "EIM.ERROR.LOGIC.NOCUTANDPASTEROLE");
					}
				}
			}
		}
		
		//XML
		out.println("<object_exist");
			out.println(" exist=\"" + (isObjExist ? "true" : "") + "\"");
			out.println(" objId=\"" + prmObjId + "\"");
		out.println("/>");		

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