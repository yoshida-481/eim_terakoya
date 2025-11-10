<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "java.util.List"%>
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
	String prmSecId = EIMUtils.getParameter(request, "secId");
	String prmWfId  = EIMUtils.getParameter(request, "wfId");

	//Message
	String message = null;
	Object[] paramId = {
			"secId=" + prmSecId,
			"wfId=" + prmWfId
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
		if(!AdminAuthUtil.hasSpecifiedOrGeneralAuth(loginUser, AppConstant.ADMIN_AUTH_ID_SECURITY))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.NOTADMINISTRATOR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.NOTADMINISTRATOR");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message));
			return;
		}
		
		
		// デフォルトセキュリティの取得
		EIMSecurity sec = SecurityUtils.getSecurityById(sess, Long.parseLong(prmSecId));
		if(sec == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.SEC.NOTFOUND");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.SEC.NOTFOUND");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}
		
		// ワークフローの取得
		EIMWorkFlow workflow = WorkFlowUtils.getWorkFlowById(sess, Long.parseLong(prmWfId));
		
		// ステータスタイプリストの取得
		List stTypeList = workflow.getStatusTypeList();
		
		// ステータスセキュリティ
		EIMStatusSecurity stSec;
		// ステータスタイプ
		EIMStatusType type;
		
		// 全てのステータスセキュリティを削除する
		for (int i=0; i<stTypeList.size(); i++) {
			// ステータスタイプの取得
			type = (EIMStatusType)stTypeList.get(i);
			// ステータス別セキュリティの取得
			stSec = StatusSecurityUtils.getStatusSecurityBySecurityAndStatusType(sess, sec, type);
			if (stSec != null) {
				// ステータス別セキュリティの削除
				StatusSecurityUtils.deleteStatusSecurity(sess, stSec);
				//履歴追加
				OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.DELETE_STATUS_SECURITY_IN_WF, 
						EIMConstant.TARGET_DELETE, EIMConstant.SECURITY, stSec,
						null, null, null, null );
			}
		}
		
		// SearchFramework 検索FW更新通知 対象：セキュリティ
		AppUpdateNoticeUtils.updateNoticeInsert(sec.getId(), "SEARCHFW_SECURITY_DELWORKFLOW_SECURITY");
		
		//Commit
		sess.commit();
		
		out.println("<security secId=\"" + prmSecId + "\"/>");
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