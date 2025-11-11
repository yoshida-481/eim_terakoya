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
	String prmWorkFlowId = request.getParameter("workFlowId");
	int prmOtherCnt = LanguageFieldUtil.getOtherCnt(request);
	String prmDefNotifyMail = EIMUtils.getParameter(request, "defNotifyMail");
	String prmDefApproveRequest = EIMUtils.getParameter(request, "defApproveRequest");
	String prmProcessWaitPopup = EIMUtils.getParameter(request, "processWaitPopup");
	String prmBackMail = EIMUtils.getParameter(request, "backMail");
	String prmPublishNotifyMail = EIMUtils.getParameter(request, "publishNotifyMail");

	//Message
	String message = null;
	Object[] paramId = {
			"workFlowId=" + prmWorkFlowId,
			"otherCnt=" + prmOtherCnt
			};

	try
	{
		/*
		 * param check
		 */
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
		if(!AdminAuthUtil.hasSpecifiedOrGeneralAuth(loginUser, AppConstant.ADMIN_AUTH_ID_WORKFLOW))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.NOTADMINISTRATOR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.NOTADMINISTRATOR");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message));
			return;
		}

		//WorkFlow
		EIMWorkFlow workFlow = WorkFlowUtils.getWorkFlowById(sess, Long.parseLong(prmWorkFlowId));
		if(workFlow == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.WORKFLOW.NOTFOUND");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.WORKFLOW.NOTFOUND");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}

		//ネームスペース付きの定義名称を取得
		String prmDefName = LanguageFieldUtil.getDefName(sess, null, request, prmOtherCnt);

		/*
		 * Update WorkFlow
		 */
		//Update
		workFlow = WorkFlowUtils.updateWorkFlow(sess, workFlow, prmDefName);

		/*
		 * Update WorkFlow Other
		 */
		for(int i = 0; i < prmOtherCnt; i++)
		{
			String prmOtherName = LanguageFieldUtil.getOtherName(request,i);
			String prmOtherLId = LanguageFieldUtil.getOtherLId(request,i);

			//Update
			WorkFlowUtils.updateOtherWorkFlowName(sess, workFlow.getId(), prmOtherLId, prmOtherName);
		}

//		EIMObject workFlowSettingObj = AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_WFSETTING"), String.valueOf(workFlow.getId()));
//		AppObjectUtil.setAttr(sess, workFlowSettingObj, EIMConfig.get("ATTR_NAME_WFSETTING_MAIL_NOTICE_DFLT"), prmDefNotifyMail);
//		AppObjectUtil.setAttr(sess, workFlowSettingObj, EIMConfig.get("ATTR_NAME_WFSETTING_SETTING_FLG"), Integer.parseInt(prmDefApproveRequest));
//		AppObjectUtil.setAttr(sess, workFlowSettingObj, EIMConfig.get("ATTR_NAME_WFSETTING_POPUP_NOTICE_FLG"), Integer.parseInt(prmProcessWaitPopup));
//		AppObjectUtil.setAttr(sess, workFlowSettingObj, EIMConfig.get("ATTR_NAME_WFSETTING_MAIL_NOTICE_FLG"), Integer.parseInt(prmBackMail));
//		AppObjectUtil.setAttr(sess, workFlowSettingObj, EIMConfig.get("ATTR_NAME_WFSETTING_PUBLISHNOTIFY_FLG"), Integer.parseInt(prmPublishNotifyMail));

		// SearchFrameworkD 検索FW更新通知 対象：ワークフロー
		AppUpdateNoticeUtils.updateNoticeInsert(workFlow.getId(), "SEARCHFW_WORKFLOW_EDIT_WORKFLOW");

		OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.UPDATE_WORKFLOW,
				EIMConstant.TARGET_UPDATE, EIMConstant.WORKFLOW, workFlow,
				null, null, null, null);

		//XML
		out.println("<workFlow");
			out.println(" workFlowId=\"" + workFlow.getId() + "\"");
			out.println(" workFlowName=\"" + StringUtils.xmlEncode(workFlow.getName()) + "\"");
		out.println(">");
		out.println("</workFlow>");

		//Commit
		sess.commit();
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
