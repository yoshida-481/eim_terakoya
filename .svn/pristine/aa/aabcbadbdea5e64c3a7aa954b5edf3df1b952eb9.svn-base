<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>

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

		// ThreadLocalコンテキストにセッションを設定
		if(EIMThreadContext.getEIMSession() == null)
		{
			EIMThreadContext.putEIMSession(sess);
			sessPutFlag = true;
		}

		user = (EIMUser)sess.getAttribute("USER");

		// Check login user
		user = (EIMUser)sess.getAttribute("USER");

		if(!EIMXmlConfigAdminAuth.hasSpecifiedAuth(user, AppConstant.ADMIN_AUTH_ID_WORKSPACE)){
			message = EIMResource.getMessage(sess, "EIM.ERROR.NOTADMINISTRATOR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.NOTADMINISTRATOR");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId))	;
			return;
		}

		//Object
		EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		if(object == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOWORKSPACE");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOWORKSPACE");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId))	;
			return;
		}

		// Check Object Type
		if(!object.getType().getDefName().equals(EIMConfig.get("OBJECT_TYPE_NAME_WORKSPACE")))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.INPUT.NOTWORKSPACE", new Object[]{object.getName()});
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.INPUT.NOTWORKSPACE", new Object[]{object.getName()});
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId))	;
			return;
		}

		//Relation Type
		EIMRelationType relTypeDoc = RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));

		/* Check Child Objects */
		List wsChildRelList = RelationUtils.getChildRelationListByRelType(sess, object, relTypeDoc, EIMAccessRole.NONE);
		
		// ワークスペース内にワークスペース固有ごみ箱以外のデータが存在する場合
		if(wsChildRelList.size() > 0){
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.EXISTDATA", new Object[]{object.getName()});
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.EXISTDATA", new Object[]{object.getName()});
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId))	;
			return;
		}
		
		//Relation Type
		EIMRelationType relTypeRec = RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_RECYCLE"));
		
		// ワークスペース固有ごみ箱の取得
		List wsRecycleRelList = RelationUtils.getChildRelationListByRelType(sess, object, relTypeRec, EIMAccessRole.NONE);
		if(wsRecycleRelList == null || wsRecycleRelList.size() == 0){
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOWSRECYCLEBOX", new Object[]{object.getName()});
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOWSRECYCLEBOX", new Object[]{object.getName()});
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId))	;
			return;
		}
		
		// ワークスペース固有ごみ箱配下の子オブジェクト取得
		EIMRelation rel = (EIMRelation) wsRecycleRelList.get(0);
		EIMObject wsRecycleObject = rel.getChild();
		List wsRecycleChildRelList = RelationUtils.getChildRelationListByRelType(sess, wsRecycleObject, relTypeDoc, EIMAccessRole.NONE);
		
		// ワークスペース固有ごみ箱内にデータが存在する場合
		if(wsRecycleChildRelList.size() > 0){
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.EXISTDATA", new Object[]{object.getName()});
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.EXISTDATA", new Object[]{object.getName()});
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId))	;
			return;
		}
		
		//ワークスペース固有ごみ箱オブジェクトの削除
		ObjectUtils.deleteObject(sess, wsRecycleObject);

		//Delete
		ObjectUtils.deleteObject(sess, object);

		// 属性表示色オブジェクト削除
		DisplayColorUtil.deleteDisplayColorObject(sess, object);

		// SearchFramework 検索FW更新通知(削除) 対象：ワークスペース
		AppUpdateNoticeUtils.updateNoticeDelete(object.getId(), "SEARCHFW_DELETE_WORKSPACE");

		//Create Operation History
		boolean isAdmin = (session.getAttribute("ADMIN_APP_ID") != null);
		String applicationType = isAdmin ? AppConstant.SYSTEM : AppConstant.DOCUMENT;

		OperationHistoryUtils.create(sess, applicationType, EIMConstant.DELETE_WORKSPACE,
				EIMConstant.TARGET_DELETE, EIMConstant.OBJECT_TYPE, object,
				null, null, null, null);

		//Commit
		sess.commit();

		out.println("<ok/>");
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
