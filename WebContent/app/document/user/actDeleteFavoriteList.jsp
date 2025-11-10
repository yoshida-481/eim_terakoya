<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());
	
	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");
	
	EIMSession sess = null;
	EIMUser user = null;
	
	//Parameter
	String prmObjId = request.getParameter("objId");
	
	//Message
	String message = null;
	Object[] paramId = {
			"objId=" + prmObjId
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
		user = (EIMUser)sess.getAttribute("USER");

		//get Object
		EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		
		//Object Type Favorite
		EIMObjectType objType = ObjectUtils.getObjectTypeByName(sess, "お気に入り");
		
		// Check Exist
		if(object == null ||
		   object.getType().getId() != objType.getId() ||
		   object.getCreateUser().getId() != user.getId() ||
		   !SecurityUtils.authorized(sess, object, user, EIMAccessRole.DELETE))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOFAVORITE");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOFAVORITE");
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}
		
		//Favorite Folder
		EIMObject favoriteObject = ObjectUtils.getObjectById(sess, Long.parseLong(object.getName()));

		if(favoriteObject != null && SecurityUtils.authorized(sess, favoriteObject, user, EIMAccessRole.READ))
		{
			//パス
			String path = StringUtils.nullToBlank(AppObjectUtil.getPath(favoriteObject));

			//Create Operation History
			OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.DELETE_FAVORITE, 
					EIMConstant.TARGET_DELETE, EIMConstant.OBJECT, favoriteObject,
					null, null, null, path);
		}

		//お気に入りを削除する
		ObjectUtils.deleteObject(sess, object);
		
		out.println("<object");
			out.println(" fvrtObjId=\"" + object.getId() + "\"");
			out.println(">");
		out.println("</object>");
		
		//Commit
		sess.commit();
		
	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(user.getId(), eime.getMessage(), paramId), eime);
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


