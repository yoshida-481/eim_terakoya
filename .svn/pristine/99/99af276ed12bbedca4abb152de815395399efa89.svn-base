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
	String prmObjId = request.getParameter("objId");
	String prmIsDspAttributeInfo = EIMUtils.getParameter(request, "isDspAttributeInfo");
		
	//Message
	String message = null;
	Object[] paramId = {
			"objId=" + prmObjId,
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
		
		// Check login user
		user = (EIMUser)sess.getAttribute("USER");
		
		//Object
		EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));

		if(object != null)
		{
			//エラー発生フラグ
			boolean enabled = false;
			
			// 下位フォルダ管理セキュリティ取得
			long sec_id = AppObjectUtil.getIntAttr(sess, object, EIMConfig.get("ATTR_NAME_WORKSPACE_LOW_FOLDER_SEC"), Integer.MIN_VALUE);

			if(sec_id != Integer.MIN_VALUE) 
			{
				// ワークスペースのセキュリティ&下位フォルダ管理セキュリティのロールチェック
				if (SecurityUtils.authorized(sess, object, sess.getUser(), EIMAccessRole.UPDATE)
						&& AppSecurityUtils.authorizedLowFolderSecurity(sess, object, sess.getUser(), EIMAccessRole.UPDATE))
				{
					enabled = true;
				}
			}
			else
			{
                // ワークスペースのセキュリティのみロールチェック
				enabled = SecurityUtils.authorized(sess, object, sess.getUser(), EIMAccessRole.UPDATE);
			}
			
			// セキュリティ管理権限チェック
			if(!enabled)
			{
				enabled = AdminAuthUtil.hasSpecifiedOrGeneralAuth(user, AppConstant.ADMIN_AUTH_ID_SECURITY);
			}
			
			if(!enabled)
			{
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.WORKSPACE.NOEDITROLE");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.WORKSPACE.NOEDITROLE");
				log.warn(AppMessageUtils.makeLogMessage(message));
				return;					
			}
		}
		
		if(object == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOWORKSPACE");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOWORKSPACE");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}
		
		// Check Object Type
		if(!object.getType().getDefName().equals(EIMConfig.get("OBJECT_TYPE_NAME_WORKSPACE")))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.INPUT.NOTWORKSPACE", new Object[]{object.getName()});
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.INPUT.NOTWORKSPACE", new Object[]{object.getName()});
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}

		boolean isUpdateOnlyName = (prmIsDspAttributeInfo.equals("0"));
		
		//属性情報の更新
		UpdateAttributeHelper.updateAttribute(sess, request, object, isUpdateOnlyName);
		
		//属性表示色の更新
		DisplayColorUtil.updateDisplayColor(sess, request, object);
		
		// SearchFramework 検索FW更新通知 対象：ワークスペース + 配下のフォルダ・ドキュメント・タグ・ドキュメントリンク
		AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), "SEARCHFW_UPDATE_WORKSPACE");
		AppUpdateNoticeUtils.updateNoticeInsertChildRecursive(sess, object, "SEARCHFW_UPDATE_WORKSPACE_CHILD_FOLDER",
				"SEARCHFW_UPDATE_WORKSPACE_CHILD_DOCUMENT", "SEARCHFW_UPDATE_WORKSPACE_CHILD_TAG", "SEARCHFW_UPDATE_WORKSPACE_CHILD_LINK");
		
		//Commit
		sess.commit();
		
		out.println("<ok/>");
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
