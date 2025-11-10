<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "common.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.AppConstant" %>
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
	String prmStatusTypeId = request.getParameter("statusTypeId");
	String prmEntryTypeIds[] = request.getParameterValues("entryTypeId");
	String prmEntryObjIds[] = request.getParameterValues("entryObjId");

	//Message
	String message = null;
	ArrayList paramIdList = new ArrayList();
	paramIdList.add("statusTypeId=" + prmStatusTypeId);
	for(int i = 0 ; i < prmEntryTypeIds.length ; i++)
	{
		paramIdList.add("entryTypeId[" + i + "]=" + prmEntryTypeIds[i]);
		paramIdList.add("entryObjId[" + i + "]=" + prmEntryObjIds[i]);
	}
	Object[] paramId = paramIdList.toArray();

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
		if(!AdminAuthUtil.hasSpecifiedOrGeneralAuth(loginUser, AppConstant.ADMIN_AUTH_ID_WORKFLOW))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.NOTADMINISTRATOR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.NOTADMINISTRATOR");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message));
			return;
		}
		
		//Security
		EIMSecurity sec = (EIMSecurity)WorkFlowUtils.getStatusTypeById(sess, Long.parseLong(prmStatusTypeId));

		// WorkFlow
		EIMStatusType statusType = WorkFlowUtils.getStatusTypeById(sess, Long.parseLong(prmStatusTypeId));
		EIMWorkFlow workFlow = WorkFlowUtils.getWorkFlowByStatusType(sess, statusType);
		
		EIMObjectType objType = ObjectUtils.getObjectTypeByName(sess,EIMConfig.get("OBJECT_TYPE_NAME_WFPUBNOTIFY"));
		EIMObject pubNotifObj = ObjectUtils.getObjectByTypeAndName(sess,objType,Long.toString(workFlow.getId()));
		if( pubNotifObj == null ) {
			pubNotifObj = ObjectUtils.createObject( sess, objType, Long.toString(workFlow.getId()));
		}
		EIMAttributeType attrTypeId = AttributeUtils.getAttributeTypeByName( sess, EIMConfig.get("ATTR_NAME_WFPUB_NOTIFY_ENTRY_TYPE") );
		EIMAttributeType attrTypeObj = AttributeUtils.getAttributeTypeByName( sess, EIMConfig.get("ATTR_NAME_WFPUB_NOTIFY_ENTRY_OBJ") );
		EIMAttribute attrId = pubNotifObj.getAttribute( EIMConfig.get("ATTR_NAME_WFPUB_NOTIFY_ENTRY_TYPE") );
		EIMAttribute attrObj = pubNotifObj.getAttribute( EIMConfig.get("ATTR_NAME_WFPUB_NOTIFY_ENTRY_OBJ") );
		
		ArrayList entryTypeIdList = new ArrayList();
		ArrayList entryObjIdList = new ArrayList();
		long[] origIds = null;
		long[] origObjs = null;
		// 新規作成時: ArrayList は size = 0
		// 属性追加時: ArrayList は既存の属性値を持つ
		if( attrId != null && attrObj != null ) {
			origIds = TypeConvertUtils.convertToLongArray(attrId.getInts());
			origObjs = TypeConvertUtils.convertToLongArray(attrObj.getInts());
			for( int j = 0; j < origIds.length; j++ ) {
				entryTypeIdList.add(entryTypeIdList.size(), new Long(origIds[j]));
				entryObjIdList.add(entryObjIdList.size(), new Long(origObjs[j]));
			}
		}
		// 属性を追加した 新 ArrayList を作成
		for( int i = 0; i < prmEntryTypeIds.length; i++ ) {
			// Duplicate (Error)
			if( entryObjIdList.contains(Long.valueOf(prmEntryObjIds[i]))) {
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.EXISTENTRY");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.EXISTENTRY");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message));
				return;
			}
			entryTypeIdList.add(entryTypeIdList.size(), Long.valueOf(prmEntryTypeIds[i]));
			entryObjIdList.add(entryObjIdList.size(), Long.valueOf(prmEntryObjIds[i]));
			//Create Operation History
			//User
			if( Long.parseLong(prmEntryTypeIds[i]) == EIMAccessEntryType.USER ) {
				EIMUser entUser = UserUtils.getUserById(sess,Long.parseLong(prmEntryObjIds[i]));
				if( entUser == null ) {
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOACCESSENTRY");
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOACCESSENTRY");
					log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message));
					return;
				}
				if( workFlow != null ) {
					OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.UPDATE_STATUS_INFO, 
						EIMConstant.TARGET_PARENT_WORKFLOW, EIMConstant.WORKFLOW, workFlow,
						EIMConstant.TARGET_UPDATE, EIMConstant.STATUS_TYPE, (EIMStatusType)sec, null);

					OperationHistoryUtils.create(sess, AppConstant.SYSTEM, AppConstant.REGIST_USER_ENTRY_FOR_PUBLIC,
							EIMConstant.TARGET_TO_REGIST_ENTRY, EIMConstant.WORKFLOW, workFlow,
							EIMConstant.TARGET_REGISTED_ENTRY, EIMConstant.USER, entUser, null);
				}
			}
			//Group
			if( Long.parseLong(prmEntryTypeIds[i]) == EIMAccessEntryType.GROUP ) {
				EIMGroup entGroup = GroupUtils.getGroupById(sess,Long.parseLong(prmEntryObjIds[i]));
				if( entGroup == null ) {
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOACCESSENTRY");
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOACCESSENTRY");
					log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message));
					return;
				}
				if( workFlow != null ) {
					OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.UPDATE_STATUS_INFO, 
						EIMConstant.TARGET_PARENT_WORKFLOW, EIMConstant.WORKFLOW, workFlow,
						EIMConstant.TARGET_UPDATE, EIMConstant.STATUS_TYPE, (EIMStatusType)sec, null);

					OperationHistoryUtils.create(sess, AppConstant.SYSTEM, AppConstant.REGIST_GROUP_ENTRY_FOR_PUBLIC,
							EIMConstant.TARGET_TO_REGIST_ENTRY, EIMConstant.WORKFLOW, workFlow,
							EIMConstant.TARGET_REGISTED_ENTRY, EIMConstant.GROUP, entGroup, null);
				}
			}
			//Role
			if( Long.parseLong(prmEntryTypeIds[i]) == EIMAccessEntryType.ROLE ) {
				EIMRole entRole = RoleUtils.getRoleById(sess,Long.parseLong(prmEntryObjIds[i]));
				if( entRole == null ) {
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOACCESSENTRY");
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOACCESSENTRY");
					log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message));
					return;
				}
				if( workFlow != null ) {
					OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.UPDATE_STATUS_INFO, 
						EIMConstant.TARGET_PARENT_WORKFLOW, EIMConstant.WORKFLOW, workFlow,
						EIMConstant.TARGET_UPDATE, EIMConstant.STATUS_TYPE, (EIMStatusType)sec, null);

					OperationHistoryUtils.create(sess, AppConstant.SYSTEM, AppConstant.REGIST_ROLE_ENTRY_FOR_PUBLIC,
							EIMConstant.TARGET_TO_REGIST_ENTRY, EIMConstant.WORKFLOW, workFlow,
							EIMConstant.TARGET_REGISTED_ENTRY, EIMConstant.ROLE, entRole, null);
				}
			}
			//Comp
			if( Long.parseLong(prmEntryTypeIds[i]) == EIMAccessEntryType.COMP ) {
				EIMComp entComp = CompUtils.getCompById(sess,Long.parseLong(prmEntryObjIds[i]));
				if( entComp == null ) {
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOACCESSENTRY");
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOACCESSENTRY");
					log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message));
					return;
				}
				if( workFlow != null ) {
					OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.UPDATE_STATUS_INFO, 
						EIMConstant.TARGET_PARENT_WORKFLOW, EIMConstant.WORKFLOW, workFlow,
						EIMConstant.TARGET_UPDATE, EIMConstant.STATUS_TYPE, (EIMStatusType)sec, null);

					OperationHistoryUtils.create(sess, AppConstant.SYSTEM, AppConstant.REGIST_COMP_ENTRY_FOR_PUBLIC,
							EIMConstant.TARGET_TO_REGIST_ENTRY, EIMConstant.WORKFLOW, workFlow,
							EIMConstant.TARGET_REGISTED_ENTRY, EIMConstant.COMP, entComp, null);
				}
			}
		}
		// 新属性の設定
		long[] newIds = new long[entryTypeIdList.size()];
		for (int i = 0; i < entryTypeIdList.size() ; i++ ){
			newIds[i] = ((Integer)entryTypeIdList.get(i)).intValue();
		} 
		long[] newTypes = new long[entryObjIdList.size()];
		for (int i = 0; i < entryObjIdList.size() ; i++ ){
			newTypes[i] = ((Integer)entryObjIdList.get(i)).intValue();
		}
		ObjectAttributeUtils.setAttribute(sess,pubNotifObj,attrTypeId,TypeConvertUtils.convertToBuildTypeArray(newIds));
		ObjectAttributeUtils.setAttribute(sess,pubNotifObj,attrTypeObj,TypeConvertUtils.convertToBuildTypeArray(newTypes));

		//Commit
		sess.commit();
		
		//XML
		out.println("<statusType");
			out.println(" statusTypeId=\"" + statusType.getId() + "\"");
			out.println(" statusTypeName=\"" + StringUtils.xmlEncode(statusType.getName()) + "\"");
			out.println(" statusTypeKind=\"" + statusType.getKind() + "\"");
			out.println(">");
		for(int k=0; k < entryObjIdList.size(); k++)
		{
			out.println("<entry");
				out.println(" entryTypeId=\"" + entryTypeIdList.get(k) + "\"");
				out.println(" entryObjId=\"" + entryObjIdList.get(k) + "\"");
				out.println(">");
			out.println("</entry>");
		}
		out.println("</statusType>");
	}
	catch(EIMException eime)
	{
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
