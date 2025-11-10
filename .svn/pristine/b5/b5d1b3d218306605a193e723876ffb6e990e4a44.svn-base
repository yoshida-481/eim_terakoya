<%@page import="jp.co.ctc_g.eim.framework.common.util.EIMThreadContext"%>
<%@page import="app.document.search.EIMDocSearchType"%>
<%@page import="app.document.object.FixedForm"%>
<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>

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
	EIMUser user = null;
	boolean sessPutFlag = false;

	//Message
	String message = null;

	//Parameter
	String prmObjId = request.getParameter("objId");

	try{
		// Session
		sess = EIMUtils.getSession(request);
		if(sess == null)
		{
			message = EIMResource.getMessage(request, "EIM.ERROR.SESSIONTIMEOUT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.SESSIONTIMEOUT");
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}
		sessPutFlag = true;
		
		//User
		user = (EIMUser)sess.getAttribute("USER");
		
		EIMThreadContext.putEIMSession(sess);
		
		// Workspace Object
		EIMObject workspace = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		
		Boolean hasWorkspaceAuth = false;
		
		if(workspace != null){
			// Workspace Management Authority check
			hasWorkspaceAuth = WorkSpaceUtil.isWorkSpaceAdminUser(sess, user, workspace);
		}
		
		// XML出力
		// Start Root Node
		out.println("<root>");
		
		out.println("<workspaceAuth");
		out.println(" flag=\"" + hasWorkspaceAuth + "\"");
		out.println("/>");
		
		// End Root Node
		out.println("</root>");
	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(user.getId(), eime.getMessage()), eime);
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
				if(sessPutFlag){
					EIMThreadContext.removeEIMSession();
				}
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
