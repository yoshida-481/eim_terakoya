<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>
<%@ page import = "java.util.*" %>

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
	String prmEntryTypeId = EIMUtils.getParameter(request, "entryTypeId");
	String prmEntryObjectId = EIMUtils.getParameter(request, "entryObjectId");
	String prmStatusTypeId = request.getParameter("statusTypeId");

	//Message
	String message = null;
	Object[] paramId = {
			"entryTypeId=" + prmEntryTypeId,
			"entryObjectId=" + prmEntryObjectId,
			"statusTypeId=" + prmStatusTypeId
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
		boolean hasAuth = AdminAuthUtil.hasSpecifiedOrGeneralAuth(loginUser, AppConstant.ADMIN_AUTH_ID_WORKFLOW);
		
		if(!hasAuth)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.NOTADMINISTRATOR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.NOTADMINISTRATOR");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message));
			return;
		}
		
		//Security
		EIMSecurity sec = (EIMSecurity)WorkFlowUtils.getStatusTypeById(sess, Long.parseLong(prmStatusTypeId));

		//Workflow
		EIMWorkFlow workFlow = null;
		EIMStatusType statusType = WorkFlowUtils.getStatusTypeById(sess, Long.parseLong(prmStatusTypeId));
		workFlow = WorkFlowUtils.getWorkFlowByStatusType(sess, statusType);

		//Entry
		EIMObjectType type = ObjectUtils.getObjectTypeByName(sess,EIMConfig.get("OBJECT_TYPE_NAME_WFPUBNOTIFY"));
		EIMObject entry = ObjectUtils.getObjectByTypeAndName(sess,type,String.valueOf(workFlow.getId()));
		if( entry == null ) {
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOACCESSENTRY");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOACCESSENTRY");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}
		//Get Access Entry Type
		EIMAttribute attrTypeId = entry.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_NOTIFY_ENTRY_TYPE"));
		EIMAttribute attrTypeObj = entry.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_NOTIFY_ENTRY_OBJ"));
		// エントリが存在してなかった場合エラー
		if( attrTypeId == null || attrTypeObj == null ) {
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOACCESSENTRY");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOACCESSENTRY");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}
		long[] ids = TypeConvertUtils.convertToLongArray(attrTypeId.getInts());
		long[] types = TypeConvertUtils.convertToLongArray(attrTypeObj.getInts());
		List idList = new ArrayList();
		List typeList = new ArrayList();
		boolean isToDeleteExists = false;
		// 削除対象以外の属性リスト作成
		for( int i = 0; i < ids.length; i++ ) {
			if( types[i] != Long.parseLong(prmEntryObjectId) ) {
				Long k = new Long(ids[i]);
				idList.add(k);
				k = new Long(types[i]);
				typeList.add(k);
			} else {
				isToDeleteExists = true;
			}
		}
		// 除外したエントリがない場合エラー
		if( isToDeleteExists == false ) {
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOACCESSENTRY");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOACCESSENTRY");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}
		long[] newIds = new long[idList.size()];
		for (int i = 0; i < idList.size() ; i++ ){
			newIds[i] = ((Integer)idList.get(i)).intValue();
		} 
		long[] newTypes = new long[typeList.size()];
		for (int i = 0; i < typeList.size() ; i++ ){
			newTypes[i] = ((Integer)typeList.get(i)).intValue();
		}
		// 新属性の設定
		ObjectAttributeUtils.setAttribute(sess,entry,attrTypeId.getType(),TypeConvertUtils.convertToBuildTypeArray(newIds));
		ObjectAttributeUtils.setAttribute(sess,entry,attrTypeObj.getType(),TypeConvertUtils.convertToBuildTypeArray(newTypes));
		//Create Operation History
		//User
		if( Long.parseLong(prmEntryTypeId) == EIMAccessEntryType.USER ) {
			EIMUser entUser = UserUtils.getUserById(sess,Long.parseLong(prmEntryObjectId));
			if( workFlow != null ) {
				OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.UPDATE_STATUS_INFO, 
					EIMConstant.TARGET_PARENT_WORKFLOW, EIMConstant.WORKFLOW, workFlow,
					EIMConstant.TARGET_UPDATE, EIMConstant.STATUS_TYPE, (EIMStatusType)sec, null);

				OperationHistoryUtils.create(sess, AppConstant.SYSTEM, AppConstant.DELETE_USER_ENTRY_FOR_PUBLIC,
						EIMConstant.TARGET_TO_DELETE_ENTRY, EIMConstant.WORKFLOW, workFlow,
						EIMConstant.TARGET_DELETED_ENTRY, EIMConstant.USER, entUser, null);
			}
		}
		//Group
		if( Long.parseLong(prmEntryTypeId) == EIMAccessEntryType.GROUP ) {
			EIMGroup entGroup = GroupUtils.getGroupById(sess,Long.parseLong(prmEntryObjectId));
			if( workFlow != null ) {
				OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.UPDATE_STATUS_INFO, 
					EIMConstant.TARGET_PARENT_WORKFLOW, EIMConstant.WORKFLOW, workFlow,
					EIMConstant.TARGET_UPDATE, EIMConstant.STATUS_TYPE, (EIMStatusType)sec, null);

				OperationHistoryUtils.create(sess, AppConstant.SYSTEM, AppConstant.DELETE_GROUP_ENTRY_FOR_PUBLIC,
						EIMConstant.TARGET_TO_DELETE_ENTRY, EIMConstant.WORKFLOW, workFlow,
						EIMConstant.TARGET_DELETED_ENTRY, EIMConstant.GROUP, entGroup, null);
			}
		}
		//Role
		if( Long.parseLong(prmEntryTypeId) == EIMAccessEntryType.ROLE ) {
			EIMRole entRole = RoleUtils.getRoleById(sess,Long.parseLong(prmEntryObjectId));
			if( workFlow != null ) {
				OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.UPDATE_STATUS_INFO, 
					EIMConstant.TARGET_PARENT_WORKFLOW, EIMConstant.WORKFLOW, workFlow,
					EIMConstant.TARGET_UPDATE, EIMConstant.STATUS_TYPE, (EIMStatusType)sec, null);

				OperationHistoryUtils.create(sess, AppConstant.SYSTEM, AppConstant.DELETE_ROLE_ENTRY_FOR_PUBLIC,
						EIMConstant.TARGET_TO_DELETE_ENTRY, EIMConstant.WORKFLOW, workFlow,
						EIMConstant.TARGET_DELETED_ENTRY, EIMConstant.GROUP, entRole, null);
			}
		}
		//Comp
		if( Long.parseLong(prmEntryTypeId) == EIMAccessEntryType.COMP ) {
			EIMComp entComp = CompUtils.getCompById(sess,Long.parseLong(prmEntryObjectId));
			if( workFlow != null ) {
				OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.UPDATE_STATUS_INFO, 
					EIMConstant.TARGET_PARENT_WORKFLOW, EIMConstant.WORKFLOW, workFlow,
					EIMConstant.TARGET_UPDATE, EIMConstant.STATUS_TYPE, (EIMStatusType)sec, null);

				OperationHistoryUtils.create(sess, AppConstant.SYSTEM, AppConstant.DELETE_COMP_ENTRY_FOR_PUBLIC,
						EIMConstant.TARGET_TO_DELETE_ENTRY, EIMConstant.WORKFLOW, workFlow,
						EIMConstant.TARGET_DELETED_ENTRY, EIMConstant.COMP, entComp, null);
			}
		}
		//Commit
		sess.commit();
		out.println("<OK></OK>");
		
	} catch(EIMException eime) {
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), eime.getMessage(), paramId), eime);
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
	} catch(Exception e) {
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
