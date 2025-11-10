<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>
<%@ page import = "jp.co.ctc_g.eim.framework.common.util.EIMThreadContext"%>

<%@page import="jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectRoleDomain"%>
<%@page import="jp.co.ctc_g.eim.framework.business.domain.UserDefGroupDomain"%>

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
	String prmEntryId = EIMUtils.getParameter(request, "entryId");
	String prmSecId = EIMUtils.getParameter(request, "secId");
	String prmStatusTypeId = request.getParameter("statusTypeId");

	//Message
	String message = null;
	Object[] paramId = {
			"entryId=" + prmEntryId,
			"secId=" + prmSecId,
			"statusTypeId=" + prmStatusTypeId
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
		boolean hasAuth;
		if(prmSecId != null && !prmSecId.equals(""))
		{
			// セキュリティ管理
			hasAuth = AdminAuthUtil.hasSpecifiedOrGeneralAuth(loginUser, AppConstant.ADMIN_AUTH_ID_SECURITY);
			if(!hasAuth)
			{
				// セキュリティエントリー管理
				hasAuth = AdminAuthUtil.hasSpecifiedOrGeneralAuth(loginUser, AppConstant.ADMIN_AUTH_ID_SECURUTY_ENTRY);
			}
		}
		else
		{
			// ワークフロー
			hasAuth = AdminAuthUtil.hasSpecifiedOrGeneralAuth(loginUser, AppConstant.ADMIN_AUTH_ID_WORKFLOW);
		}
		
		if(!hasAuth)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.NOTADMINISTRATOR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.NOTADMINISTRATOR");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message));
			return;
		}
		
		//Security
		EIMSecurity sec = null;
		EIMWorkFlow workFlow = null;
		if(prmSecId != null && !prmSecId.equals(""))
		{
			sec = SecurityUtils.getSecurityById(sess, Long.parseLong(prmSecId));
			
			// SearchFramework 検索FW更新通知 対象：セキュリティ
			// 2011/12/15現在、下記ステータスタイプ処理は通らないので、この分岐にのみセキュリティの更新通知処理を加えておく。
			AppUpdateNoticeUtils.updateNoticeInsert(sec.getId(), "SEARCHFW_SECURITY_DELACENTRY_SECURITY");
		}
		else
		{
			//Status
			EIMStatusType statusType = WorkFlowUtils.getStatusTypeById(sess, Long.parseLong(prmStatusTypeId));
			
			sec = (EIMSecurity)statusType;
			
			//Get Work Flow
			workFlow = WorkFlowUtils.getWorkFlowByStatusType(sess, statusType);
		}

		//Entry
		EIMAccessEntry entry = SecurityUtils.getAccessEntryById(sess, Long.parseLong(prmEntryId));
		if(entry == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.ACCESS.ENTRY.NOTFOUND");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.ACCESS.ENTRY.NOTFOUND");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}
		//Get Access Entry Type
		EIMAccessEntryType entryType = entry.getType();
		
		
		//Create Operation History
		//User
		if(entryType.getId() == EIMAccessEntryType.USER)
		{
			EIMUser user = entry.getUser();
			if(workFlow != null)
			{
				OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.UPDATE_STATUS_INFO, 
						EIMConstant.TARGET_PARENT_WORKFLOW, EIMConstant.WORKFLOW, workFlow,
						EIMConstant.TARGET_UPDATE, EIMConstant.STATUS_TYPE, (EIMStatusType)sec, null);

				OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.DELETE_USER_ENTRY, 
						EIMConstant.TARGET_TO_DELETE_ENTRY, EIMConstant.STATUS_TYPE, (EIMStatusType)sec,
						EIMConstant.TARGET_DELETED_ENTRY, EIMConstant.USER, user, null);
			}
			else
			{
				OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.DELETE_USER_ENTRY, 
						EIMConstant.TARGET_TO_DELETE_ENTRY, EIMConstant.SECURITY, sec,
						EIMConstant.TARGET_DELETED_ENTRY, EIMConstant.USER, user, null);

			}
		}
		//Group
		if(entryType.getId() == EIMAccessEntryType.GROUP)
		{
			EIMGroup group = entry.getGroup();
			if(workFlow != null)
			{
				OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.UPDATE_STATUS_INFO, 
						EIMConstant.TARGET_PARENT_WORKFLOW, EIMConstant.WORKFLOW, workFlow,
						EIMConstant.TARGET_UPDATE, EIMConstant.STATUS_TYPE, (EIMStatusType)sec, null);

				OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.DELETE_GROUP_ENTRY, 
						EIMConstant.TARGET_TO_DELETE_ENTRY, EIMConstant.STATUS_TYPE, (EIMStatusType)sec,
						EIMConstant.TARGET_DELETED_ENTRY, EIMConstant.GROUP, group, null);
			}
			else
			{
				OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.DELETE_GROUP_ENTRY, 
						EIMConstant.TARGET_TO_DELETE_ENTRY, EIMConstant.SECURITY, sec,
						EIMConstant.TARGET_DELETED_ENTRY, EIMConstant.GROUP, group, null);
			}
		}
		//Role
		if(entryType.getId() == EIMAccessEntryType.ROLE)
		{
			EIMRole role = entry.getRole();
			if(workFlow != null)
			{
				OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.UPDATE_STATUS_INFO, 
						EIMConstant.TARGET_PARENT_WORKFLOW, EIMConstant.WORKFLOW, workFlow,
						EIMConstant.TARGET_UPDATE, EIMConstant.STATUS_TYPE, (EIMStatusType)sec, null);

				OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.DELETE_ROLE_ENTRY, 
						EIMConstant.TARGET_TO_DELETE_ENTRY, EIMConstant.STATUS_TYPE, (EIMStatusType)sec,
						EIMConstant.TARGET_DELETED_ENTRY, EIMConstant.ROLE, role, null);
			}
			else
			{
				OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.DELETE_ROLE_ENTRY, 
						EIMConstant.TARGET_TO_DELETE_ENTRY, EIMConstant.SECURITY, sec,
						EIMConstant.TARGET_DELETED_ENTRY, EIMConstant.ROLE, role, null);
			}
		}
		//Comp
		if(entryType.getId() == EIMAccessEntryType.COMP)
		{
			EIMComp comp = entry.getComp();
			if(workFlow != null)
			{
				OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.UPDATE_STATUS_INFO, 
						EIMConstant.TARGET_PARENT_WORKFLOW, EIMConstant.WORKFLOW, workFlow,
						EIMConstant.TARGET_UPDATE, EIMConstant.STATUS_TYPE, (EIMStatusType)sec, null);

				OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.DELETE_COMPLEX_GROUP_ENTRY, 
						EIMConstant.TARGET_TO_DELETE_ENTRY, EIMConstant.STATUS_TYPE, (EIMStatusType)sec,
						EIMConstant.TARGET_DELETED_ENTRY, EIMConstant.COMP, comp, null);
			}
			else
			{
				OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.DELETE_COMPLEX_GROUP_ENTRY, 
						EIMConstant.TARGET_TO_DELETE_ENTRY, EIMConstant.SECURITY, sec,
						EIMConstant.TARGET_DELETED_ENTRY, EIMConstant.COMP, comp, null);
			}
		}
		//UserDefGroup
		if(entryType.getId() == EIMAccessEntryType.USERDEF)
		{
			UserDefGroupDomain userDefGroup = entry.getUserDefGroup();
			if(workFlow != null)
			{
				OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.UPDATE_STATUS_INFO, 
						EIMConstant.TARGET_PARENT_WORKFLOW, EIMConstant.WORKFLOW, workFlow,
						EIMConstant.TARGET_UPDATE, EIMConstant.STATUS_TYPE, (EIMStatusType)sec, null);

				OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.DELETE_USER_DEF_GROUP_ENTRY, 
						EIMConstant.TARGET_TO_DELETE_ENTRY, EIMConstant.STATUS_TYPE, (EIMStatusType)sec,
						EIMConstant.TARGET_DELETED_ENTRY, EIMConstant.USERDEF_GROUP, userDefGroup, null);
			}
			else
			{
				OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.DELETE_USER_DEF_GROUP_ENTRY, 
						EIMConstant.TARGET_TO_DELETE_ENTRY, EIMConstant.SECURITY, sec,
						EIMConstant.TARGET_DELETED_ENTRY, EIMConstant.USERDEF_GROUP, userDefGroup, null);
			}
		}
		//ObjectRole
		if(entryType.getId() == EIMAccessEntryType.OBJROLE)
		{
			ObjectRoleDomain objectRole = entry.getObjectRole();
			if(workFlow != null)
			{
				OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.UPDATE_STATUS_INFO, 
						EIMConstant.TARGET_PARENT_WORKFLOW, EIMConstant.WORKFLOW, workFlow,
						EIMConstant.TARGET_UPDATE, EIMConstant.STATUS_TYPE, (EIMStatusType)sec, null);

				OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.DELETE_OBJECT_ROLE_ENTRY, 
						EIMConstant.TARGET_TO_DELETE_ENTRY, EIMConstant.STATUS_TYPE, (EIMStatusType)sec,
						EIMConstant.TARGET_DELETED_ENTRY, EIMConstant.OBJECT_ROLE, objectRole, null);
			}
			else
			{
				OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.DELETE_OBJECT_ROLE_ENTRY, 
						EIMConstant.TARGET_TO_DELETE_ENTRY, EIMConstant.SECURITY, sec,
						EIMConstant.TARGET_DELETED_ENTRY, EIMConstant.OBJECT_ROLE, objectRole, null);
			}
		}
		
		//Delete
		SecurityUtils.deleteAccessEntry(sess, entry);
		
		//Commit
		sess.commit();
		out.println("<OK></OK>");
		
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
