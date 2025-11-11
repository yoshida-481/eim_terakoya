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

		//WorkFlow
		EIMStatusType statusType = WorkFlowUtils.getStatusTypeById(sess, Long.parseLong(prmStatusTypeId));
		EIMWorkFlow workFlow = WorkFlowUtils.getWorkFlowByStatusType(sess, statusType);
		
		ArrayList entryTypeList = new ArrayList();
		ArrayList entryList = new ArrayList();
		ArrayList entryObjNameList = new ArrayList();
		
		//エントリーごとのループ
		for(int ii = 0 ; ii < prmEntryTypeIds.length ; ii++)
		{
			//Entry Type
			EIMAccessEntryType entryType = new EIMAccessEntryType(Integer.parseInt(prmEntryTypeIds[ii]));
	
			//Entry
			EIMAccessEntry entry = null;
			String entryObjName = null;
		
			//User
			if(entryType.getId() == EIMAccessEntryType.USER)
			{
				EIMUser user = UserUtils.getUserById(sess, Long.parseLong(prmEntryObjIds[ii]));
				entryObjName = user.getName();
				entry = SecurityUtils.createAccessEntry(sess, sec, entryType, (Object)user);

				//Create Operation History
				if(workFlow != null)
				{
					OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.UPDATE_STATUS_INFO, 
							EIMConstant.TARGET_PARENT_WORKFLOW, EIMConstant.WORKFLOW, workFlow,
							EIMConstant.TARGET_UPDATE, EIMConstant.STATUS_TYPE, (EIMStatusType)sec, null);
	
					OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.REGIST_USER_ENTRY, 
							EIMConstant.TARGET_TO_REGIST_ENTRY, EIMConstant.STATUS_TYPE, (EIMStatusType)sec,
							EIMConstant.TARGET_REGISTED_ENTRY, EIMConstant.USER, user, null);
				}
				else
				{
					OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.REGIST_USER_ENTRY, 
							EIMConstant.TARGET_TO_REGIST_ENTRY, EIMConstant.SECURITY, sec,
							EIMConstant.TARGET_REGISTED_ENTRY, EIMConstant.USER, user, null);
				}
			}
			//Group
			if(entryType.getId() == EIMAccessEntryType.GROUP)
			{
				EIMGroup group = GroupUtils.getGroupById(sess, Long.parseLong(prmEntryObjIds[ii]));
				entryObjName = group.getName();
				entry = SecurityUtils.createAccessEntry(sess, sec, entryType, (Object)group);
	
				//Create Operation History
				if(workFlow != null)
				{
					OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.UPDATE_STATUS_INFO, 
							EIMConstant.TARGET_PARENT_WORKFLOW, EIMConstant.WORKFLOW, workFlow,
							EIMConstant.TARGET_UPDATE, EIMConstant.STATUS_TYPE, (EIMStatusType)sec, null);

					OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.REGIST_GROUP_ENTRY, 
							EIMConstant.TARGET_TO_REGIST_ENTRY, EIMConstant.STATUS_TYPE, (EIMStatusType)sec,
							EIMConstant.TARGET_REGISTED_ENTRY, EIMConstant.GROUP, group, null);
				}
				else
				{
					OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.REGIST_GROUP_ENTRY, 
							EIMConstant.TARGET_TO_REGIST_ENTRY, EIMConstant.SECURITY, sec,
							EIMConstant.TARGET_REGISTED_ENTRY, EIMConstant.GROUP, group, null);
				}
			}
			//Role
			if(entryType.getId() == EIMAccessEntryType.ROLE)
			{
				EIMRole role = RoleUtils.getRoleById(sess, Long.parseLong(prmEntryObjIds[ii]));
				entryObjName = role.getName();
				entry = SecurityUtils.createAccessEntry(sess, sec, entryType, (Object)role);

				//Create Operation History
				if(workFlow != null)
				{
					OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.UPDATE_STATUS_INFO, 
							EIMConstant.TARGET_PARENT_WORKFLOW, EIMConstant.WORKFLOW, workFlow,
							EIMConstant.TARGET_UPDATE, EIMConstant.STATUS_TYPE, (EIMStatusType)sec, null);
	
					OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.REGIST_ROLE_ENTRY, 
							EIMConstant.TARGET_TO_REGIST_ENTRY, EIMConstant.STATUS_TYPE, (EIMStatusType)sec,
							EIMConstant.TARGET_REGISTED_ENTRY, EIMConstant.ROLE, role, null);
				}
				else
				{
					OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.REGIST_ROLE_ENTRY, 
							EIMConstant.TARGET_TO_REGIST_ENTRY, EIMConstant.SECURITY, sec,
							EIMConstant.TARGET_REGISTED_ENTRY, EIMConstant.ROLE, role, null);
				}
			}
			//Comp
			if(entryType.getId() == EIMAccessEntryType.COMP)
			{
				EIMComp comp = CompUtils.getCompById(sess, Long.parseLong(prmEntryObjIds[ii]));
				entryObjName = comp.getName();
				entry = SecurityUtils.createAccessEntry(sess, sec, entryType, (Object)comp);

				//Create Operation History
				if(workFlow != null)
				{
					OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.UPDATE_STATUS_INFO, 
							EIMConstant.TARGET_PARENT_WORKFLOW, EIMConstant.WORKFLOW, workFlow,
							EIMConstant.TARGET_UPDATE, EIMConstant.STATUS_TYPE, (EIMStatusType)sec, null);

					OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.REGIST_COMPLEX_GROUP_ENTRY, 
							EIMConstant.TARGET_TO_REGIST_ENTRY, EIMConstant.STATUS_TYPE, (EIMStatusType)sec,
							EIMConstant.TARGET_REGISTED_ENTRY, EIMConstant.COMP, comp, null);
				}
				else
				{
					OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.REGIST_COMPLEX_GROUP_ENTRY, 
							EIMConstant.TARGET_TO_REGIST_ENTRY, EIMConstant.SECURITY, sec,
							EIMConstant.TARGET_REGISTED_ENTRY, EIMConstant.COMP, comp, null);
				}
			}
			//Approve Role
			SecurityUtils.updateAccessRole(sess, entry, new EIMAccessRole(EIMAccessRole.APPROVE), 1);

			// 結果の保存
			entryTypeList.add(entryType);
			entryList.add(entry);
			entryObjNameList.add(entryObjName);
		}

		//Commit
		sess.commit();
		
		//XML
		out.println("<statusType");
			out.println(" statusTypeId=\"" + statusType.getId() + "\"");
			out.println(" statusTypeName=\"" + StringUtils.xmlEncode(statusType.getName()) + "\"");
			out.println(" statusTypeKind=\"" + statusType.getKind() + "\"");
			out.println(">");
		for(int k=0; k < entryTypeList.size(); k++)
		{
			EIMAccessEntryType entryType = (EIMAccessEntryType)entryTypeList.get(k);
			EIMAccessEntry entry = (EIMAccessEntry)entryList.get(k);
			String entryObjName = (String)entryObjNameList.get(k);
			
			out.println("<entry");
				out.println(" entryId=\"" + entry.getId() + "\"");
				out.println(" entryTypeId=\"" + entryType.getId() + "\"");
				out.println(" entryTypeName=\"" + StringUtils.xmlEncode(entryType.getName()) + "\"");
				out.println(" entryObjId=\"" + prmEntryObjIds[k] + "\"");
				out.println(" entryObjName=\"" + StringUtils.xmlEncode(entryObjName) + "\"");
				out.println(" priority=\"" + entry.getPriority() + "\"");
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
