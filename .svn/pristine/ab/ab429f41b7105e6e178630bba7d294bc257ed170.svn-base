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
	
	//Parameter
	String prmObjId = request.getParameter("objId");
	String prmUserId = request.getParameter("userId");

	//Message
	String message = null;
	Object[] paramId = {
			"objId=" + prmObjId,
			"userId=" + prmUserId
			};
	try
	{
		//Session
		sess = EIMUtils.getSession(request);
		if(sess == null)
		{
			message = EIMResource.getMessage(request, "EIM.ERROR.SESSIONTIMEOUT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}
		loginUser = (EIMUser)sess.getAttribute("USER");
		
		//Object
		EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		if(object == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODOCUMENT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId))	;
			return;
		}
		
		//user
		EIMUser selectApprover = UserUtils.getUserById(sess, Long.parseLong(prmUserId));
		if(selectApprover == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOUSER");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId))	;
			return;			
		}
		
		//Disable
		if(selectApprover.getDisable() == 1)
		{			
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOAPPROVER.INVALIDITY.USER", new Object[]{selectApprover.getName()});
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOAPPROVER.INVALIDITY.USER", new Object[]{selectApprover.getName()});
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;	
		}
		

		//Security
		EIMSecurity sec = object.getSecurity();
		if(sec != null)
		{
			//Check
			if(SecurityUtils.authorized(sess, object, selectApprover, EIMAccessRole.STATUS_UP))
			{
				//XML
				out.println("<user");
					out.println(" label=\"" + StringUtils.xmlEncode(selectApprover.getName()) + "\"");
					out.println(" type=\"user\"");
					out.println(" userId=\"" + selectApprover.getId() + "\"");
					out.println(" userCode=\"" + StringUtils.xmlEncode(selectApprover.getCode()) + "\"");
					out.println(" userName=\"" + StringUtils.xmlEncode(selectApprover.getName()) + "\"");
					out.println(" userKana=\"" + StringUtils.xmlEncode(selectApprover.getKana()) + "\"");
					out.println(" userMail=\"" + StringUtils.xmlEncode(selectApprover.getMail()) + "\"");
					out.println(">");
				out.println("</user>");	
			}
			else
			{
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOAPPROVER.USER", new Object[]{selectApprover.getName()});
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOAPPROVER.USER", new Object[]{selectApprover.getName()});
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				return;
			}
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
		message = EIMResource.getMessage("EIM.ERROR.SYSTEMERROR");
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
	}
	finally
	{
		try{
			if(sess != null) sess.close();
		}
		catch (Exception se) {
			out.clear();
			message = EIMResource.getMessage("EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(se.getMessage()));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
	}
	
%>
