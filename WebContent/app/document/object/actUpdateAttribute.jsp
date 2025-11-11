<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>

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
		
		// ThreadLocalコンテキストにセッションを設定
		if(EIMThreadContext.getEIMSession() == null)
		{
			EIMThreadContext.putEIMSession(sess);
			sessPutFlag = true;
		}
		
		user = (EIMUser)sess.getAttribute("USER");
		
		//Object
		EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		if(object == null || !SecurityUtils.authorized(sess, object, sess.getUser(),EIMAccessRole.READ))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOTEXIST.DOCFOLWS");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOTEXIST.DOCFOLWS");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}
		//ドキュメントかどうか判定
		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
		
		//アクセス権限チェック
		if (helper.isTypeOfFolder(object.getType())) {
			// ドキュメントのリレーションタイプ取得
			EIMRelationType relType = RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));

            // 親リレーションの取得
			List parentRelList = RelationUtils.getParentRelationListByRelType(sess, object, relType,EIMAccessRole.READ);
			EIMObject parentObj = null;
			if (parentRelList != null && parentRelList.size() > 0) {
				// 親オブジェクトの取得
				parentObj = ((EIMRelation)parentRelList.get(0)).getParent();
			}
			
			if (parentObj != null) {
				// 下位フォルダ管理セキュリティ取得
				long sec_id = AppObjectUtil.getIntAttr(sess, parentObj, EIMConfig.get("ATTR_NAME_WORKSPACE_LOW_FOLDER_SEC"), Integer.MIN_VALUE);
				if (sec_id != Integer.MIN_VALUE) {
					EIMSecurity eimSec = SecurityUtils.getSecurityById(sess, sec_id);
					// Check Role
					if(!SecurityUtils.authorized(sess, object, sess.getUser(), EIMAccessRole.UPDATE))
					{
						message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOUPDATEROLE");
						out.println(AppMessageUtils.makeErrorTagByMessage(message));
						message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOUPDATEROLE");
						log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
						return;
					}
				}
			}
			
			//オブジェクトの書き込み権限をチェック
			if(object.getSecurity() != null)
			{
				if(!SecurityUtils.authorized(sess, object, sess.getUser(), EIMAccessRole.UPDATE))
				{
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOUPDATEROLE");
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOUPDATEROLE");
					log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
					return;
				}
			}
			
		}
		else if (helper.isTypeOfWorkspace(object.getType()))	//<---ワークスペースの場合は別のjspが呼ばれるようになったのでここには入らない（2012/03/02）
		{
			boolean hasUpdateRole = true;
			
			// 下位フォルダ管理セキュリティ取得
			if(!AppSecurityUtils.authorizedLowFolderSecurity(sess, object, sess.getUser(), EIMAccessRole.UPDATE)) {
				hasUpdateRole = false;
			}
			
			//オブジェクトの書き込み権限をチェック
			if(object.getSecurity() != null)
			{
				if(!SecurityUtils.authorized(sess, object, sess.getUser(), EIMAccessRole.UPDATE))
				{
					hasUpdateRole = false;
				}
			}

			// ワークスペース管理権限追加により、セキュリティ権限チェックの代わりとします(2012/03/01)
//			boolean hasSecurityRole = AdminAuthUtil.hasSpecifiedOrGeneralAuth(user, AppConstant.ADMIN_AUTH_ID_SECURITY);
			boolean hasWorkSpaceCreateRole = AdminAuthUtil.hasSpecifiedOrGeneralAuth(user, AppConstant.ADMIN_AUTH_ID_WORKSPACE);
			
			if(hasUpdateRole == false && hasWorkSpaceCreateRole == false)
			{
				//エラーメッセージを変更（EIM.ERROR.LOGIC.NOSECURITYROLE→EIM.ERROR.LOGIC.NOUPDATEROLE）
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOUPDATEROLE");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOUPDATEROLE");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
				return;
			}

		}
		else {		
			if(object.getSecurity() != null)
			{
				if(!SecurityUtils.authorized(sess, object, sess.getUser(), EIMAccessRole.UPDATE))
				{
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOUPDATEROLE");
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOUPDATEROLE");
					log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
					return;
				}
			}
		}
		
		//リスト値表示色オブジェクトの更新
		DisplayColorUtil.updateDisplayColor(sess, request, object);
		
		//属性情報の更新
		UpdateAttributeHelper.updateAttribute(sess, request, object, false);
		
		//パス
		String path = StringUtils.nullToBlank(AppObjectUtil.getPath(object));

		//Create Operation History
		OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.UPDATE_OBJECT_ATTRIBUTE, 
				EIMConstant.TARGET_UPDATE, EIMConstant.OBJECT, object,
				null, null, null, path);

		//Access
		AccessUtils.createAccess(sess, object, "EIM.ACCESS.TYPE.ATTRIBUTEUPDATE");
		
		//Commit
		sess.commit();
		out.println("<ok></ok>");
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
