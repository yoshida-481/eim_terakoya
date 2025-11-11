<%@page import="common.util.AppLogicUtil.ProcessFolderTreeWalker"%>
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
	String prmObjId[] = request.getParameterValues("objId");
	//String prmOcrSettingValue = request.getParameter("ocrSettingValue");
	String prmOcrSettingValue = request.getParameterValues("ocrSettingValue")[0];
	
	//Message
	String message = null;
	ArrayList paramIdList = new ArrayList();
	for(int i = 0 ; i < prmObjId.length ; i++)
	{
		paramIdList.add("objId[" + i + "]=" + prmObjId[i]);
	}
	Object[] paramId = paramIdList.toArray();
	
	try
	{
		//Session
		sess = EIMUtils.getSession(request);
		if(sess == null) {
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
		
		for (int i = 0; i < prmObjId.length; i++) {
			long objId = Long.parseLong(prmObjId[i]);
			EIMObject object = ObjectUtils.getObjectById(sess, objId);
			
			
			// 更新権限チェック
			if (!SecurityUtils.authorized(sess, object, sess.getUser(), EIMAccessRole.UPDATE)) {
				throw new EIMException(sess, "EIM.ERROR.LOGIC.NOACCESS.OCRSETTING");
			}
			
			// OCR処理設定可否チェック
			if(!AppOcrUtil.isSettable(object)){
				// 設定不可能な場合、スキップ
				continue;
			}
			
			// OCR処理ステータスを更新する
			if(prmOcrSettingValue.equals("1")){
				// OCR処理ステータスを「0:処理待ち」に更新
				AppOcrUtil.setOcrProcessingStatus(sess, object, AppConstant.OCR_PROC_STATUS_PROCESS_WAIT);
			}else{
				// OCR処理ステータスを削除する
				AppOcrUtil.setOcrProcessingStatus(sess, object, AppConstant.OCR_PROC_STATUS_NONE);
			}
			
			// アクセス履歴
			AccessUtils.createAccess(sess, object, "EIM.ACCESS.TYPE.OCR.SETTING");
			
			// 操作履歴
			OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, AppConstant.OCR_SETTING,
					AppConstant.TARGET_TO_SETTING, EIMConstant.OBJECT, object,
					null, null, null,
					AppObjectUtil.getPath(object));
			
		}
		
		out.println("<OK></OK>");

		//Commit
		sess.commit();
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