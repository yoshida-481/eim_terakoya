<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>
<%@ page import = "java.text.*" %>
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
	String prmFormatId = request.getParameter("formatId");

	//Message
	String message = null;
	Object[] paramId = {
			"formatId=" + prmFormatId
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
		if(!AdminAuthUtil.hasSpecifiedOrGeneralAuth(loginUser, AppConstant.ADMIN_AUTH_ID_FORMAT))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.NOTADMINISTRATOR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.NOTADMINISTRATOR");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message));
			return;
		}
		
		//Format
		EIMFormat format = FileUtils.getFormatById(sess, Long.parseLong(prmFormatId));
		if(format == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.FORMAT.NOTFOUND");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.FORMAT.NOTFOUND");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}
		
		//Root Node
		out.println("<directories>");
		
		//Directory List
		List dirList = FileUtils.getDirectoryList(sess, format);

		for(int i = 0; i < dirList.size(); i++)
		{
			//Directory
			EIMDirectory dir = (EIMDirectory)dirList.get(i);

			// ディレクトリ使用量を取得
			FileUtil.DirectoryUsage directoryUsage = FileUtil.getDirectoryUsage(dir.getPath());
			int fileCount = directoryUsage.fileCount;
			long fileSize = directoryUsage.fileSize;

			//Formater
			NumberFormat formatter = NumberFormat.getNumberInstance();

			//XML
			out.println("<directory");
				out.println(" dirId=\"" + dir.getId() + "\"");
				out.println(" path=\"" + StringUtils.xmlEncode(dir.getPath()) + "\"");
				out.println(" status=\"" + dir.getStatus() + "\"");
				out.println(" fileCount=\"" + fileCount + "\"");
				out.println(" fileSize=\"" + StringUtils.xmlEncode(formatter.format(fileSize)) + "\"");
			out.println(">");
			out.println("</directory>");
		}

		//End Root Node
		out.println("</directories>");
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
