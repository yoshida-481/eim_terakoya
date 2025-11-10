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
	EIMUser loginUser = null;
	boolean sessPutFlag = false;
	
	//Parameter
	String prmUserId = EIMUtils.getParameter(request, "userId");
	String prmAddGroupIds = EIMUtils.getParameter(request, "addGroupIds");
	String prmRemoveGroupIds = EIMUtils.getParameter(request, "removeGroupIds");
	String prmAddRoleIds = EIMUtils.getParameter(request, "addRoleIds");
	String prmRemoveRoleIds = EIMUtils.getParameter(request, "removeRoleIds");

	//Message
	String message = null;
	Object[] paramId = {
			"userId=" + prmUserId,
			"addGroupIds=" + prmAddGroupIds,
			"removeGroupIds=" + prmRemoveGroupIds,
			"addRoleIds=" + prmAddRoleIds,
			"removeRoleIds=" + prmRemoveRoleIds
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
		
		//User
		loginUser = (EIMUser)sess.getAttribute("USER");
		if(!AdminAuthUtil.hasSpecifiedOrGeneralAuth(loginUser, AppConstant.ADMIN_AUTH_ID_USER)
				&& !AdminAuthUtil.hasSpecifiedOrGeneralAuth(loginUser, AppConstant.ADMIN_AUTH_ID_SECURUTY_ENTRY))				
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.NOTADMINISTRATOR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.NOTADMINISTRATOR");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message));
			return;
		}
		
		//User
		EIMUser user = UserUtils.getUserById(sess, Long.parseLong(prmUserId));
		if(user == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.USER.NOTFOUND");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.USER.NOTFOUND");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}
		
		// 配列に変換
		String[] addGroupList = prmAddGroupIds.split(",");
		String[] removeGroupList = prmRemoveGroupIds.split(",");
		String[] addRoleList = prmAddRoleIds.split(",");
		String[] removeRoleList = prmRemoveRoleIds.split(",");
		
		// グループに所属
		for (String groupId : addGroupList) {
			if ( groupId == null || groupId.isEmpty() ) {
				continue;
			}
			EIMGroup group = GroupUtils.getGroupById(sess, Long.parseLong(groupId));
			if(group == null)
			{
				message = EIMResource.getMessage(sess, "EIM.ERROR.GROUP.NOTFOUND");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.GROUP.NOTFOUND");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				return;
			}
			//Assign
			GroupUtils.assignUser(sess, group, user);
			
			// SearchFramework 検索FW更新通知 対象：グループ
			AppUpdateNoticeUtils.updateNoticeInsert(group.getId(), "SEARCHFW_USER_EDIT_GROUP");
			
			//Create Operation History
			OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.ASSIGN_GROUP_USER, 
					EIMConstant.TARGET_TO_ASSIGN_USER, EIMConstant.GROUP, group,
					EIMConstant.TARGET_ASSIGNED_USER, EIMConstant.USER, user, null);
		}
		
		// グループから除去
		for (String groupId : removeGroupList) {
			if (groupId == null || groupId.isEmpty() ) {
				continue;
			}
			//Group
			EIMGroup group = GroupUtils.getGroupById(sess, Long.parseLong(groupId));
			if(group == null)
			{
				message = EIMResource.getMessage(sess, "EIM.ERROR.GROUP.NOTFOUND");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.GROUP.NOTFOUND");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				return;
			}
			
			//Release
			GroupUtils.releaseUser(sess, group, user);
			
			// SearchFramework 検索FW更新通知 対象：グループ
			AppUpdateNoticeUtils.updateNoticeInsert(group.getId(), "SEARCHFW_USER_EDIT_GROUP");
			
			//Create Operation History
			OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.RELEASE_GROUP_USER, 
					EIMConstant.TARGET_TO_RELEASE_USER, EIMConstant.GROUP, group,
					EIMConstant.TARGET_RELEASED_USER, EIMConstant.USER, user, null );
		}
		
		// ロールに所属
		for (String roleId : addRoleList) {
			if (roleId == null || roleId.isEmpty() ) {
				continue;
			}
			EIMRole role = RoleUtils.getRoleById(sess, Long.parseLong(roleId));
			if(role == null)
			{
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.ROLE.NOTFOUND");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.ROLE.NOTFOUND");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				return;
			}
			//Assign
			RoleUtils.assignUser(sess, role, user);
			
			// SearchFramework 検索FW更新通知 対象：ロール
			AppUpdateNoticeUtils.updateNoticeInsert(role.getId(), "SEARCHFW_USER_EDIT_ROLE");
			
			//Create Operation History
			OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.ASSIGN_ROLE_USER, 
					EIMConstant.TARGET_TO_ASSIGN_USER, EIMConstant.ROLE, role,
					EIMConstant.TARGET_ASSIGNED_USER, EIMConstant.USER, user, null);
		}
		
		// ロールから除去
		for (String roleId : removeRoleList) {
			if (roleId == null || roleId.isEmpty() ) {
				continue;
			}
			EIMRole role = RoleUtils.getRoleById(sess, Long.parseLong(roleId));
			if(role == null)
			{
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.ROLE.NOTFOUND");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.ROLE.NOTFOUND");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				return;
			}
			//Release
			RoleUtils.releaseUser(sess, role, user);
			
			// SearchFramework 検索FW更新通知 対象：ロール
			AppUpdateNoticeUtils.updateNoticeInsert(role.getId(), "SEARCHFW_USER_EDIT_ROLE");
			
			//Create Operation History
			OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.RELEASE_ROLE_USER, 
					EIMConstant.TARGET_TO_RELEASE_USER, EIMConstant.ROLE, role,
					EIMConstant.TARGET_RELEASED_USER, EIMConstant.USER, user, null);
		}
		
		//Commit
		sess.commit();
		
		out.println("<ok/>");
		
	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), eime.getMessage(), paramId), eime);
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
