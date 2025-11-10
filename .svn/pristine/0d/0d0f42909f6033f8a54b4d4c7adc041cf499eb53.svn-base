<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>
<%@ page import = "jp.co.ctc_g.eim.framework.common.util.EIMThreadContext"%>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");
	
	//Session
	EIMSession sess = null;
	EIMUser user = null;
	boolean sessPutFlag = false;
	
	//Parameter
	String prmObjName = EIMUtils.getParameter(request, "objName");
	String prmSecId = EIMUtils.getParameter(request, "secId");
	String prmLowerSuccessionSecId = request.getParameter("lowerSuccessionSecId");
	String prmUpdateAttribute = request.getParameter("updateAttribute");

	//Message
	String message = null;
	Object[] paramId = {
			"objName=" + prmObjName,
			"secId=" + prmSecId,
			"lowerSuccessionSecId=" + prmLowerSuccessionSecId,
			"updateAttribute=" + prmUpdateAttribute
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
		
		// ThreadLocalコンテキストにセッションを設定
		if(EIMThreadContext.getEIMSession() == null)
		{
			EIMThreadContext.putEIMSession(sess);
			sessPutFlag = true;
		}
		
		// Check login user
		user = (EIMUser)sess.getAttribute("USER");
		if(!AdminAuthUtil.hasSpecifiedOrGeneralAuth(user, AppConstant.ADMIN_AUTH_ID_SECURITY))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.NOTADMINISTRATOR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.NOTADMINISTRATOR");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId))	;
			return;
		}
		
		// Windows禁止文字チェック
		AppObjectUtil.checkValidateFName(sess, prmObjName);
		
		//セキュリティ
		EIMSecurity sec = SecurityUtils.getSecurityById(sess, Long.parseLong(prmSecId));
		if( sec == null )
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.SEC.NOTFOUND");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.SEC.NOTFOUND");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId))	;
			return;
		}
		
		//Object Type WorkSpace
		EIMObjectType objType = ObjectUtils.getObjectTypeByName(sess, "ワークスペース");
		
		//Create Object
		EIMObject object = ObjectUtils.createObject(sess, objType, prmObjName, EIMConstant.DEPU_CHECK_TYPE_NAME);

		//Security
		SecurityUtils.setSecurity(sess, object, sec);
		
		//下位フォルダ管理セキュリティ設定
		if (prmLowerSuccessionSecId != null && prmLowerSuccessionSecId.length() > 0)
		{
			AppObjectUtil.setAttr(sess, object, 
					EIMConfig.get("ATTR_NAME_WORKSPACE_LOW_FOLDER_SEC"), Long.parseLong(prmLowerSuccessionSecId));			
		}
		
		if (prmUpdateAttribute != null && prmUpdateAttribute.equals("true"))
		{
			//属性情報の更新
			UpdateAttributeHelper.updateAttribute(sess, request, object, false);
			
			//属性表示色の更新
			DisplayColorUtil.updateDisplayColor(sess, request, object);
		}
		
		// SearchFramework 検索FW更新通知 対象：ワークスペース
		AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), "SEARCHFW_CREATE_WORKSPACE");
		
		//Create Operation History
		OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.CREATE_WORKSPACE, 
				EIMConstant.TARGET_CREATE, EIMConstant.OBJECT_TYPE, object,
				null, null, null, null);
		if(!SecurityUtils.authorized(sess,object,sess.getUser(),EIMAccessRole.UPDATE)){
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOCREATEROLE");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOCREATEROLE");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			sess.rollback();
			return;
		}
		//Commit
		sess.commit();
		
		//XML
		out.println("<object");
			out.println(" objId=\"" + object.getId() + "\"");
			out.println(" objName=\"" + StringUtils.xmlEncode(object.getName()) + "\"");
			out.println(">");
		out.println("</object>");
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
			if(sessPutFlag) {
				EIMThreadContext.removeEIMSession();
				sessPutFlag = false;
			}
			
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
