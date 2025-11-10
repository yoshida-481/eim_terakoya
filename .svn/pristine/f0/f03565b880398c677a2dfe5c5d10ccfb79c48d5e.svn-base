<%@ page contentType="text/xml; charset=UTF-8" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//Session
	EIMSession sess = null;
	
	//Parameter
	String prmObjId = null;
	
	//Object
	EIMObject object = null;
	
	//Parameter
	String prmCompId = null;

	//Message
	String message = null;
	
	Object[] paramId = {
			"compId=" + prmCompId
			};	
	
	try{
		
		//ContentType
		response.setContentType("text/xml; charset=UTF-8");
		response.setHeader("Cache-Control", "max-age=0, must-revalidate");
		
		//Session
		sess = EIMUtils.getSession(request);
		
		//Parameter
		prmObjId = EIMUtils.getParameter(request, "objId");
		
		//Object
		object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		
		//Parameter
		prmCompId = EIMUtils.getParameter(request, "compId");
		
		//Delete
		ObjectUtils.deleteObject(sess, object);
		
		//Create Operation History
		object.setName(object.getAttribute(EIMConfig.get("ATTR_NAME_PUBLIC_NOTIFICATION_TEMPLATE_NAME")).getString());
		OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, AppConstant.DELETE_PUBLISH_TEMPLATE,
				EIMConstant.TARGET_DELETE, AppConstant.TEMPLATE, object,
				null, null, null, null);
		
		sess.commit();
	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(sess.getUser().getId(), eime.getMessage()), eime);
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
