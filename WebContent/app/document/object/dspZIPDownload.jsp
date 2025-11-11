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
	
	String prmObjId[] = request.getParameterValues("objId");

	//Message
	String message = null;
	ArrayList paramIdList = new ArrayList();
	for(int i = 0 ; i < prmObjId.length ; i++)
	{
		paramIdList.add("objId[" + i + "]=" + prmObjId[i]);
	}
	Object[] paramId = paramIdList.toArray();
	
	try
	{
		sess = EIMUtils.getSession(request);
		if(sess == null)
		{
			message = EIMResource.getMessage(request, "EIM.ERROR.SESSIONTIMEOUT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.SESSIONTIMEOUT");
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}

		user = (EIMUser)sess.getAttribute("USER");

		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

		for(int i = 0; i < prmObjId.length; i++) {
			//Object
			EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId[i]));
			if(object == null)
			{
				out.clear();		
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODOCORFOLORTAG");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODOCORFOLORTAG");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
				return;
			}

			//アクセス権チェック
			try {
				helper.checkAccessibleStatusSelf(object, true);
			} catch (EIMException e) {
				if (!e.getMessageKey().equals("EIM.ERROR.LOGIC.NOACCESS")) throw e;
				message = e.getMessage();
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage(e.getMessageKey(), e.getMessageParams());
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
				return;
			}
		}
		
		//XML
		out.println("<ok/>");
	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(user.getId(), eime.getMessage(), paramId), eime);
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
