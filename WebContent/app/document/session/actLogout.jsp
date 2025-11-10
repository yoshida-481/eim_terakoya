<%@ page contentType="text/xml; charset=UTF-8" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "eim.bo.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>

<%@ page import = "org.springframework.security.core.context.SecurityContextHolder" %>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	EIMSession sess = null;

	//Message
	String message = null;

	try
	{
		//Session
		sess = EIMUtils.getSession(request);
		if(sess == null)
		{
			out.println("<windowAction");
			out.println(" windowClose=\"" + EIMConfig.get("LOGOUT_WINDOW_CLOSE") + "\"");
			out.println(">");
			out.println("</windowAction>");
			return;
		}

		//Create Operation History
		OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.LOGOUT, 
				null, null, null,
				null, null, null, null);

		//Commit
		sess.commit();

		out.println("<windowAction");
		out.println(" windowClose=\"" + EIMConfig.get("LOGOUT_WINDOW_CLOSE") + "\"");
		out.println(">");
		out.println("</windowAction>");
	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(eime.getMessage()), eime);
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
		if(sess != null){
			try{
				sess.close();
			}
			catch (Exception se) {
				out.clear();
				message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
			}
		}

		//Invalid Session
		SecurityContextHolder.clearContext();
		session.invalidate();
	}
%>
