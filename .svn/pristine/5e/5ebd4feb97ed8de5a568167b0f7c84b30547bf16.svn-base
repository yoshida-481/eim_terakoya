<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>

<%
	/* 指定のオブジェクトが存在するか否か、ごみ箱配下か否かを判定します。 */

	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");
	
	//Session
	EIMSession sess = null;
	EIMUser user = null;
	
	// Parameter
	String prmObjId =  EIMUtils.getParameter(request, "ObjectId");
	String prmTagObjectIds =  EIMUtils.getParameter(request, "tagObjectIds");	// ルートタグから選択対象までのカンマ区切りID配列 (タグツリー配下選択での画面更新時に使用)

	//Message
	String message = null;
	Object[] paramId = {
			"objId=" + prmObjId,
			"tagObjectIds=" + prmTagObjectIds
			};

	boolean isObjExist = true;	// 指定オブジェクトの存在有無
	
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
		//User
		user = (EIMUser)sess.getAttribute("USER");
		
		if (!StringUtils.isBlank(prmObjId)){
			EIMObject obj = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
			// 指定のオブジェクトが存在するか否か
			if (obj == null || !SecurityUtils.authorized(sess, obj, sess.getUser(),EIMAccessRole.READ)) {
				isObjExist = false;
			}
		}

		//XML
		out.println("<object_exist");
			out.println(" exist=\"" + (isObjExist ? "true" : "") + "\"");
			out.println(" objId=\"" + prmObjId + "\"");
			if (!StringUtils.isBlank(prmTagObjectIds)) {
				out.println(" tagObjectIds=\"" + prmTagObjectIds + "\"");
			}
		out.println("/>");		

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