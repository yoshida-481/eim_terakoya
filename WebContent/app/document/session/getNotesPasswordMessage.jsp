<%@ page import="java.util.List"%>
<%@ page contentType="text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>
<%@ page import="jp.co.ctc_g.eim.framework2.common.util.ResourceUtils"%>
<%@ page import="java.util.ArrayList"%>
<%@ page import="jp.co.ctc_g.eim.framework2.business.service.*"%>

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
		sess = EIMUtils.getSession(request);
		if(sess == null)
		{
			message = EIMResource.getMessage(request, "EIM.ERROR.SESSIONTIMEOUT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.SESSIONTIMEOUT");
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}

		List<String> notesPasswordMessageList = new ArrayList<String>();
		if(!ResourceUtils.getByKey("EIM.LABEL.PASSWORD.NOTES.CUSTOM").isEmpty()) {
			notesPasswordMessageList.add(ResourceUtils.getByKey("EIM.LABEL.PASSWORD.NOTES.CUSTOM"));
		}else {
			if(!ResourceUtils.getByKey("EIM.LABEL.PASSWORD.NOTES1").isEmpty()){
				notesPasswordMessageList.add(ResourceUtils.getByKey("EIM.LABEL.PASSWORD.NOTES1"));
			}
			if(!ResourceUtils.getByKey("EIM.LABEL.PASSWORD.NOTES2").isEmpty()){
				notesPasswordMessageList.add(ResourceUtils.getByKey("EIM.LABEL.PASSWORD.NOTES2"));
			}
			if(!ResourceUtils.getByKey("EIM.LABEL.PASSWORD.NOTES3").isEmpty()){
				notesPasswordMessageList.add(ResourceUtils.getByKey("EIM.LABEL.PASSWORD.NOTES3"));
			}
			if(!ResourceUtils.getByKey("EIM.LABEL.PASSWORD.NOTES4").isEmpty()){
				notesPasswordMessageList.add(ResourceUtils.getByKey("EIM.LABEL.PASSWORD.NOTES4"));
			}
			if(!ResourceUtils.getByKey("EIM.LABEL.PASSWORD.NOTES5").isEmpty()){
				notesPasswordMessageList.add(ResourceUtils.getByKey("EIM.LABEL.PASSWORD.NOTES5"));
			}
			if(!ResourceUtils.getByKey("EIM.LABEL.PASSWORD.NOTES6").isEmpty()){
				notesPasswordMessageList.add(ResourceUtils.getByKey("EIM.LABEL.PASSWORD.NOTES6"));
			}
			PasswordValidateService passwordValidateService = (PasswordValidateService)jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader.getApplicationContext().getBean("passwordValidateService2");
			notesPasswordMessageList.addAll(passwordValidateService.getNotesMessageList());
		}

		String notesMessage = String.join("\n", notesPasswordMessageList);

		// notesMessage
		out.println("<notesMessage");
		out.println(" message=\"" + notesMessage + "\"");
		out.println(">");
		out.println("</notesMessage>");

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
