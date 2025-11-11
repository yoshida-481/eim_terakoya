<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>
<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>

<%
	/* 指定のワークフローのステータスが存在するか否かを判定します。 */

	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");
	
	//Session
	EIMSession sess = null;
	EIMUser user = null;
	
	//Parameter
	String prmWorkFlowId = request.getParameter("workFlowId");
	String prmStatusTypeId = request.getParameter("statusTypeId");

	//Message
	String message = null;
	Object[] paramId = {
			"workFlowId=" + prmWorkFlowId,
			"statusTypeId=" + prmStatusTypeId
			};

	boolean isStatusExist = false;	// ステータスの存在有無
	EIMObject workFlowSettingObj = null; // ワークフロー設定オブジェクトの存在有無
	
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
		
		if (!StringUtils.isBlank(prmWorkFlowId)){
			EIMWorkFlow workFlow = WorkFlowUtils.getWorkFlowById(sess, Long.parseLong(prmWorkFlowId));
			if (workFlow != null) {
				List stsTypeList = workFlow.getStatusTypeList();
				if (stsTypeList != null && stsTypeList.size() > 0) {
					EIMStatusType stsType = (EIMStatusType)stsTypeList.get(0);
					if (stsType != null) {
						// 1番目のステータスタイプのステータスが存在するか否かを判定
						isStatusExist = WorkFlowUtils.isExistStatus(sess, stsType);
					}
				}
			}

			// ワークフロー設定オブジェクト取得
			workFlowSettingObj = AppObjectUtil.getObject(
					  sess
					, EIMConfig.get("OBJECT_TYPE_NAME_WFSETTING")
					, String.valueOf(prmWorkFlowId)
					);
		}
		//XML
		out.println("<status_exist");
			out.println(" exist=\"" + (isStatusExist ? "true" : "") + "\"");
			out.println(" notDocument=\"" + (workFlowSettingObj == null ? "true" : "") + "\"");
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