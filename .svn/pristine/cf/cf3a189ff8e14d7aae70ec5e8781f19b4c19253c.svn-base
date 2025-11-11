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
		
		//Object Type Favorite
		EIMObjectType objType = ObjectUtils.getObjectTypeByName(sess, "お気に入り");
	
		EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		if(object == null || !SecurityUtils.authorized(sess, object, user, EIMAccessRole.READ))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOFOLDER");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOFOLDER");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}
		
		//選択されたフォルダが、既にお気に入りに登録されているか？
		//object = ObjectUtils.getObjectByTypeAndName(sess, objType, prmObjId);
		List result = AppSearchUtils.searchObject(	sess,
												objType,
												prmObjId,
												false,
												true,
												-1,
												user,
												null,
												null,
												null,
												null,
												null,
												null,
												null,
												null,
												null,
												EIMAccessRole.READ);
		if(result.size() < 1)
		{
			//Create Object
			ObjectUtils.createObject(sess, objType, prmObjId);
			
			//パス
			String path = StringUtils.nullToBlank(AppObjectUtil.getPath(object));

			//Create Operation History
			OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.CREATE_FAVORITE, 
					EIMConstant.TARGET_CREATE, EIMConstant.OBJECT, object,
					null, null, null, path);

			//Commit
			sess.commit();

			out.println("<ok/>");			
		}
		else
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.EXISTFAVORITE");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.EXISTFAVORITE");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}
		
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
