<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>
<%@ page import = "addon.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>

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
	String prmWorkFlowId = request.getParameter("workFlowId");

	//Message
	String message = null;
	Object[] paramId = {
			"workFlowId=" + prmWorkFlowId
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
		
		//Status Type List
		List statusTypeList = workFlow.getStatusTypeList();
		
		//Root Node
		out.println("<statusTypeList>");
		
		for(int i = 0; i < statusTypeList.size(); i++)
		{
			//Status Type
			EIMStatusType statusType = (EIMStatusType)statusTypeList.get(i);
			
			//XML
			out.println("<statusType");
				out.println(" statusTypeId=\"" + statusType.getId() + "\"");
				out.println(" statusTypeName=\"" + StringUtils.xmlEncode(statusType.getName()) + "\"");
				out.println(" statusTypeKind=\"" + statusType.getKind() + "\""); 
				out.println(" step=\"" + statusType.getStep() + "\"");
			out.println(">");

			// ドキュメント管理 かつ ワークフローでステータス種別が「公開処理中」の場合
			if(((String)session.getAttribute("ADMIN_APP_ID")).equals(AppConstant.ADMIN_APP_ID_DOCUMENT) && statusType.getKind() == AppConstant.STATUS_TYPE_KIND_ID_PROCESSING_PUBLIC){

				// ワークフロー公開処理オブジェクトの取得
				EIMObject wfpubObj = AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_WFPUB"), String.valueOf(statusType.getId()));
				
				// アドオンクラスのインスタンスリストを取得
				List classList = PublishAddonUtils.getAddonClasses(sess);
				for(int j = 0; j < classList.size() ; j++){
					PublishCommandAddOn addon = (PublishCommandAddOn)classList.get(j);
					
					// ステータス情報ラベルの取得
					out.println(addon.getPublishStatusLabel(sess, wfpubObj));
				}
			}
			
			out.println("</statusType>");
		}
		
		//End Root Node
		out.println("</statusTypeList>");
		
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
