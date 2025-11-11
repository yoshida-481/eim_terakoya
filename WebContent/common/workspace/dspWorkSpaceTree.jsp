<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>
<%@ page import = "common.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>

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
		if(!EIMXmlConfigAdminAuth.hasSpecifiedAuth(loginUser, AppConstant.ADMIN_AUTH_ID_WORKSPACE))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.NOTADMINISTRATOR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.NOTADMINISTRATOR");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message));
			return;
		}

		//Root Node
		out.println("<wsList>");

		//Workspace List
		List wsList = ObjectUtils.getObjectListByType(sess, ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_WORKSPACE")));

		for(int i = 0; i < wsList.size(); i++)
		{
			//WorkSpaceObject
			EIMObject wsObj = (EIMObject)wsList.get(i);

			//下位フォルダ管理セキュリティ
			long lowerSecId = AppObjectUtil.getIntAttr(sess, wsObj, EIMConfig.get("ATTR_NAME_WORKSPACE_LOW_FOLDER_SEC"), Integer.MIN_VALUE);
			String lowerSecName = "";
			if(lowerSecId != Integer.MIN_VALUE){
				EIMSecurity s = SecurityUtils.getSecurityById(sess, lowerSecId);
				lowerSecName = s.getName();
			}

			//XML
			out.println("<workSpace");
				out.println(" wsName=\"" + StringUtils.xmlEncode(wsObj.getName()) + "\"");
				out.println(" wsId=\"" + wsObj.getId() + "\"");
				out.println(" secId=\"" + wsObj.getSecurityId() + "\"");
				if (wsObj.getSecurity() != null) {
					out.println(" secName=\"" + StringUtils.xmlEncode(wsObj.getSecurity().getName()) + "\"");
				}
				out.println(" lowerSuccessionSecId=\"" + lowerSecId + "\"");
				out.println(" lowerSuccessionSecNam=\"" + StringUtils.xmlEncode(lowerSecName) + "\"");
			out.println(">");
			out.println("</workSpace>");
		}

		//End Root Node
		out.println("</wsList>");
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
