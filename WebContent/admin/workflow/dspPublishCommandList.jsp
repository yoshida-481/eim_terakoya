<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "addon.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>
<%@ page import = "java.util.*" %>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	//Session
	EIMSession sess = null;
	EIMUser loginUser = null;

	//Parameter
	String prmStatusTypeId = EIMUtils.getParameter(request, "statusTypeId");

	//Message
	String message = null;
	Object[] paramId = {
			"statusTypeId=" + prmStatusTypeId
			};

	try
	{
		/*
		 * Parameter check
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

		EIMObject wfpubObj = null;


		if(prmStatusTypeId != null && prmStatusTypeId.length() > 0) {
			//StatusType
			EIMStatusType stType = WorkFlowUtils.getStatusTypeById(sess, Long.parseLong(prmStatusTypeId));
			if(stType == null)
			{
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.STTYPE.NOTFOUND");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.STTYPE.NOTFOUND");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				return;
			}

			//WorkFlow
			EIMWorkFlow workFlow = WorkFlowUtils.getWorkFlowByStatusType(sess, stType);
			if(workFlow == null)
			{
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.WORKFLOW.NOTFOUND");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.WORKFLOW.NOTFOUND");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				return;
			}

			String stTypeId = String.valueOf(stType.getId());

			// ワークフロー公開処理オブジェクトの取得
			wfpubObj = AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_WFPUB"), String.valueOf(stType.getId()));
		}


		// アドオンクラスのインスタンスリストを取得
		List classList = PublishAddonUtils.getAddonClasses(sess);

		//XML
		out.println("<publishProcessSetting lang=\""+ sess.getLangId() + "\">");
			for(int i = 0; i < classList.size(); i++){
				PublishCommandAddOn addOn = (PublishCommandAddOn)classList.get(i);
				//アドオンオプションが無効の場合、無視するように変更　by lin.chen at 2010/03/18
				String addOnInfos = addOn.getPublishCommandSetting(sess,wfpubObj);
				if(addOnInfos == null || addOnInfos.length() == 0) continue;
				
				out.println(addOnInfos);
			}
		out.println("</publishProcessSetting>");
	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), eime.getMessage(), paramId), eime);
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
