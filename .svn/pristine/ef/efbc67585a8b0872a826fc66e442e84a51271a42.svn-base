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
	EIMUser loginUser = null;

	//Message
	String message = null;

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

		List langIdList = EIMXmlConfigLanguage.getLangIdList();
		int langIdNum = langIdList.size();

		//XML
		out.println("<workFlow>");
			out.println("<statusTypeList>");
				out.println("<statusType id='-1' seq='1' kind='-13001' auto='true' through='0'>");
					out.println("<nameList>");
					for(int i = 0; i < langIdNum; i++)
					{
						String langId = (String)langIdList.get(i);
						String otherName = EIMResource.getMessage(langId, "EIM.STATUSTYPE.EDITING");

						out.println("<name lang='" + langId + "' value='" + otherName + "'/>");
					}
					out.println("</nameList>");
					out.println("<asEntryList/>");
				out.println("</statusType>");
				out.println("<statusType id='-2' seq='2' kind='-13002' auto='true' through='3'>");
					out.println("<nameList>");
					for(int i = 0; i < langIdNum; i++)
					{
						String langId = (String)langIdList.get(i);
						String otherName = EIMResource.getMessage(langId, "EIM.STATUSTYPE.REQUESTAPPROVE");

						out.println("<name lang='" + langId + "' value='" + otherName + "'/>");
					}
					out.println("</nameList>");
					out.println("<asEntryList/>");
				out.println("</statusType>");
				out.println("<statusType id='-3' seq='3' kind='-13003' auto='true' through='0'>");
					out.println("<nameList>");
					for(int i = 0; i < langIdNum; i++)
					{
						String langId = (String)langIdList.get(i);
						String otherName = EIMResource.getMessage(langId, "EIM.STATUSTYPE.PROCESSINGPUBLIC");

						out.println("<name lang='" + langId + "' value='" + otherName + "'/>");
					}
					out.println("</nameList>");
					out.println("<asEntryList/>");
				out.println("</statusType>");
				out.println("<statusType id='-4' seq='4' kind='-13004' auto='true' through='0'>");
					out.println("<nameList>");
					for(int i = 0; i < langIdNum; i++)
					{
						String langId = (String)langIdList.get(i);
						String otherName = EIMResource.getMessage(langId, "EIM.STATUSTYPE.PUBLIC");

						out.println("<name lang='" + langId + "' value='" + otherName + "'/>");
					}
					out.println("</nameList>");
					out.println("<asEntryList/>");
				out.println("</statusType>");

			out.println("</statusTypeList>");
			out.println("<eventTypeList/>");
		out.println("</workFlow>");

	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), eime.getMessage()), eime);
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
