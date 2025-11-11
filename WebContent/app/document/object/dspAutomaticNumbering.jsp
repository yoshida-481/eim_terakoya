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

	//Message
	String message = null;

	try{
		// XML出力
		
		// Start Root Node
		out.println("<root>");
		
		// 自動採番の有効化フラグ
		boolean automaticNumberingFlag = false;
		String automaticNumbering = EIMConfig.getValue("ENABLE_AUTOMATIC_NUMBERING");
		
		if(automaticNumbering.toUpperCase().equals("ON")){
			automaticNumberingFlag = true;
		}
		
		out.println("<automaticNumbering");
		out.println(" flag=\"" + String.valueOf(automaticNumberingFlag) + "\"");
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
