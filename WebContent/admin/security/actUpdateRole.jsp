<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "common.util.*" %>
<%@ page import = "java.util.List" %>
<%@ page import = "org.apache.commons.logging.*" %>
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
	String prmEntryId = EIMUtils.getParameter(request, "entryId");
	String prmSecId = EIMUtils.getParameter(request, "secId");
	String isDefaultSec = EIMUtils.getParameter(request, "isDefaultSec");
	String[] prmRoleNames = EIMUtils.getParameter(request, "roleNameList").split(",");
	
	
	//Message
	String message = null;
	Object[] paramId = {
			"entryId=" + prmEntryId,
			"secId=" + prmSecId,
			"isDefaultSec=" + isDefaultSec
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
		if(!AdminAuthUtil.hasSpecifiedOrGeneralAuth(loginUser, AppConstant.ADMIN_AUTH_ID_SECURITY)
			&& !AdminAuthUtil.hasSpecifiedOrGeneralAuth(loginUser, AppConstant.ADMIN_AUTH_ID_SECURUTY_ENTRY))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.NOTADMINISTRATOR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.NOTADMINISTRATOR");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message));
			return;
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
		EIMAccessEntryType entryType = entry.getType();
		
		// ステータス別セキュリティ
		EIMSecurity sec = SecurityUtils.getSecurityById(sess,Long.valueOf(prmSecId));
		EIMStatusSecurity stSec = null;
		// デフォルトセキュリティでなければ、ステータス別セキュリティを取得
		if ("false".equals(isDefaultSec)) {
			stSec = StatusSecurityUtils.getStatusSecurityById(sess, Long.parseLong(prmSecId));
			if(stSec == null)
			{
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.STATUSSEC.NOTFOUND");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.STATUSSEC.NOTFOUND");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				return;
			}
			
			// デフォルトセキュリティではない場合、prmSecIdはステータス別セキュリティID
			// SearchFramework 検索FW更新通知 対象：セキュリティ
			AppUpdateNoticeUtils.updateNoticeInsert(stSec.getDefaultSec().getId(), "SEARCHFW_SECURITY_SETACR_SECURITY");
		}
		else
		{
			// デフォルトセキュリティの場合、prmSecIdはセキュリティID
			// SearchFramework 検索FW更新通知 対象：セキュリティ
			AppUpdateNoticeUtils.updateNoticeInsert(sec.getId(), "SEARCHFW_SECURITY_SETACR_SECURITY");
		}
		
		// アクセスロール
		EIMAccessRole role = null;
		EIMAccessRoleType roleType = null;
		// パラメータ名の数
		for (int i=0; i<prmRoleNames.length; i++) {
			// パラメータ[roleID],[true | false],[0 | 1 | 2]
			String[] values = EIMUtils.getParameter(request, prmRoleNames[i]).split(",");
			
			// アクセスロールタイプ取得
			roleType = SecurityUtils.getAccessRoleTypeById(sess, 
						Integer.parseInt(values[0]));
			
			// チェックボックスが ＯＮ の場合
			if (stSec == null || (values[1] != null && values[1].equals("true"))) {
				// permitMode を ラジオボタンの値から設定
				role = new EIMAccessRole(roleType.getId(), roleType, stSec, EIMAccessRole.PermitMode.getModeByValue(Integer.parseInt(values[2])));
			// チェックボックスが ＯＦＦ の場合
			} else {
				// permitMode は 3 で固定
				role = new EIMAccessRole(roleType.getId(), roleType, stSec, EIMAccessRole.PermitMode.getModeByValue(3));
			}
			
			SecurityUtils.updateAccessRole(sess, entry, stSec,  role, role.getPermit());
		}
		String adminAppId = (String)session.getAttribute("ADMIN_APP_ID");
		if (AppConstant.ADMIN_APP_ID_DOCUMENT.equals(adminAppId))
		{
			List<EIMWorkFlow> workFlowList = WorkFlowUtils.getWorkFlowList(sess);
			for(EIMWorkFlow workFlow : workFlowList){
				AppSecurityUtils.updateStatusSecurityForDocBySecWfAce(sess, sec, workFlow, entry);
			}
		}

		if (stSec == null) {
			CreateOpeHistUtil.createUpdateSecurityEntryRoleHistory(sess, null, entry, entryType, AppConstant.SYSTEM);
		} else {
			CreateOpeHistUtil.createUpdateStatusSecurityEntryRoleHistor(sess, stSec, entry, entryType, AppConstant.SYSTEM);
		}
		
		//Commit
		sess.commit();
		
		out.println("<OK/>");
		
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
