<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>
<%@ page import = "common.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%
	
//-------------------------------------------------------------------------
//ワークスペース管理機能追加のため、admin/object/dspWorkFlow.jspをコピーし
//追加修正
//※使用可能タイプ選択画面＞詳細情報＞ワークフロー情報取得
//														(2012/02/17)
//-------------------------------------------------------------------------

	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());
	
	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");
	
	//Session
	EIMSession sess = null;
	EIMUser loginUser = null;

	//Parameter
	String prmObjTypeId = request.getParameter("objTypeId");

	//Message
	String message = null;
	Object[] paramId = {
			"objTypeId=" + prmObjTypeId
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
		
		//Object Type
		EIMObjectType objType = ObjectUtils.getObjectTypeById(sess, Long.parseLong(prmObjTypeId));
		if(objType == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOTYPE");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOTYPE");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}
		
		//WorkFlow
		EIMWorkFlow workFlow = WorkFlowUtils.getWorkFlowByType(sess, objType);
		if(workFlow != null)
		{
			//Status Type List
			List statusList = workFlow.getStatusTypeList();
			
			//Root Node
			out.println("<statusTypeList");
				out.println(" workflowId=\"" + workFlow.getId() + "\"");
				out.println(" workflowName=\"" + StringUtils.xmlEncode(workFlow.getName()) + "\"");
			out.println(">");
			
			for(int i = 0; i < statusList.size(); i++)
			{
				//Status Type
				EIMStatusType statusType = (EIMStatusType)statusList.get(i);
				
				//XML
				out.println("<statusType");
					out.println(" statusTypeId=\"" + statusType.getId() + "\"");
					out.println(" statusTypeName=\"" + StringUtils.xmlEncode(statusType.getName()) + "\"");
					out.println(" step=\"" + statusType.getStep() + "\"");
				out.println(">");
				out.println("</statusType>");
			}
			
			//End Root Node
			out.println("</statusTypeList>");
		}
		else {
			out.println("<OK></OK>");
		}
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
