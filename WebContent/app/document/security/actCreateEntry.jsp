<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>


<%@ page import = "jp.co.ctc_g.eim.framework.common.util.*" %>
<%@ page import = "jp.co.ctc_g.eim.framework.business.service.*" %>
<%@ page import = "jp.co.ctc_g.eim.framework.business.domain.*" %>
<%@ page import = "jp.co.ctc_g.eim.framework.common.component.*"%>
<%@ page import = "jp.co.ctc_g.eim.framework.common.util.*"%>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "org.springframework.context.*" %>

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
	boolean sessPutFlag = false;
	
	//Parameter
	String prmStatusTypeId = request.getParameter("statusTypeId");
	String prmSecId = EIMUtils.getParameter(request, "secId");
	String prmEntryTypeIds[] = request.getParameterValues("entryTypeId");
	String prmEntryObjIds[] = request.getParameterValues("entryObjId");

	//Message
	String message = null;

	ArrayList paramIdList = new ArrayList();
	paramIdList.add("statusTypeId=" + prmStatusTypeId);
	paramIdList.add("secId=" + prmSecId);
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
		
		// ThreadLocalコンテキストにセッションを設定
		if(EIMThreadContext.getEIMSession() == null)
		{
			EIMThreadContext.putEIMSession(sess);
			sessPutFlag = true;
		}

		//Security
		EIMSecurity sec = null;
		EIMWorkFlow workFlow = null;
		if(prmSecId != null && !prmSecId.equals(""))
		{
			sec = SecurityUtils.getSecurityById(sess, Long.parseLong(prmSecId));
			
			// SearchFramework 検索FW更新通知 対象：セキュリティ
			// 2011/12/15現在、下記ステータスタイプ処理は通らないので、この分岐にのみセキュリティの更新通知処理を加えておく。
			AppUpdateNoticeUtils.updateNoticeInsert(sec.getId(), "SEARCHFW_SECURITY_ADDACENTRY_SECURITY");
		}
		else
		{
			//Status
			EIMStatusType statusType = WorkFlowUtils.getStatusTypeById(sess, Long.parseLong(prmStatusTypeId));

			sec = (EIMSecurity)statusType;

			//Get Work Flow
			workFlow = WorkFlowUtils.getWorkFlowByStatusType(sess, statusType);
		}
		
		//Security Check
		if( sec == null )
		{
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.SEC.NOTFOUND");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}
		
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
					OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.UPDATE_STATUS_INFO,
							EIMConstant.TARGET_PARENT_WORKFLOW, EIMConstant.WORKFLOW, workFlow,
							EIMConstant.TARGET_UPDATE, EIMConstant.STATUS_TYPE, (EIMStatusType)sec, null);

					OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.REGIST_USER_ENTRY,
							EIMConstant.TARGET_TO_REGIST_ENTRY, EIMConstant.STATUS_TYPE, (EIMStatusType)sec,
							EIMConstant.TARGET_REGISTED_ENTRY, EIMConstant.USER, user, null);
				}
				else
				{
					OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.REGIST_USER_ENTRY,
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
					OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.UPDATE_STATUS_INFO,
							EIMConstant.TARGET_PARENT_WORKFLOW, EIMConstant.WORKFLOW, workFlow,
							EIMConstant.TARGET_UPDATE, EIMConstant.STATUS_TYPE, (EIMStatusType)sec, null);

					OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.REGIST_GROUP_ENTRY,
							EIMConstant.TARGET_TO_REGIST_ENTRY, EIMConstant.STATUS_TYPE, (EIMStatusType)sec,
							EIMConstant.TARGET_REGISTED_ENTRY, EIMConstant.GROUP, group, null);
				}
				else
				{
					OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.REGIST_GROUP_ENTRY,
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
					OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.UPDATE_STATUS_INFO,
							EIMConstant.TARGET_PARENT_WORKFLOW, EIMConstant.WORKFLOW, workFlow,
							EIMConstant.TARGET_UPDATE, EIMConstant.STATUS_TYPE, (EIMStatusType)sec, null);

					OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.REGIST_ROLE_ENTRY,
							EIMConstant.TARGET_TO_REGIST_ENTRY, EIMConstant.STATUS_TYPE, (EIMStatusType)sec,
							EIMConstant.TARGET_REGISTED_ENTRY, EIMConstant.ROLE, role, null);
				}
				else
				{
					OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.REGIST_ROLE_ENTRY,
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
					OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.UPDATE_STATUS_INFO,
							EIMConstant.TARGET_PARENT_WORKFLOW, EIMConstant.WORKFLOW, workFlow,
							EIMConstant.TARGET_UPDATE, EIMConstant.STATUS_TYPE, (EIMStatusType)sec, null);

					OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.REGIST_COMPLEX_GROUP_ENTRY,
							EIMConstant.TARGET_TO_REGIST_ENTRY, EIMConstant.STATUS_TYPE, (EIMStatusType)sec,
							EIMConstant.TARGET_REGISTED_ENTRY, EIMConstant.COMP, comp, null);
				}
				else
				{
					OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.REGIST_COMPLEX_GROUP_ENTRY,
							EIMConstant.TARGET_TO_REGIST_ENTRY, EIMConstant.SECURITY, sec,
							EIMConstant.TARGET_REGISTED_ENTRY, EIMConstant.COMP, comp, null);
				}
			}

			//UserDefGroup
			if(entryType.getId() == EIMAccessEntryType.USERDEF)
			{
				UserDefGroupDomain userDefGroup = null;
				boolean flg = false;

				try
				{
					if(EIMThreadContext.getEIMSession() == null)
					{
						flg = true;
						EIMThreadContext.putEIMSession(sess);
					}

					//Root UserDefGroupList
					ApplicationContext context = ApplicationContextLoader.getContext();
					UserDefGroupConfService uds = (UserDefGroupConfService)context.getBean("UserDefGroupConfService");

					userDefGroup = uds.getUserDefGroup(Long.parseLong(prmEntryObjIds[ii]));
				}
				catch(Exception e)
				{
					throw e;
				}
				finally
				{	//Remove Session from Thread Local Table
					if(flg == true)
						EIMThreadContext.removeEIMSession();
				}

				entryObjName = userDefGroup.getName();
				entry = SecurityUtils.createAccessEntry(sess, sec, entryType, (Object)userDefGroup);

				//Create Operation History
				if(workFlow != null)
				{
					OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.UPDATE_STATUS_INFO,
							EIMConstant.TARGET_PARENT_WORKFLOW, EIMConstant.WORKFLOW, workFlow,
							EIMConstant.TARGET_UPDATE, EIMConstant.STATUS_TYPE, (EIMStatusType)sec, null);

					OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.REGIST_USER_DEF_GROUP_ENTRY,
							EIMConstant.TARGET_TO_REGIST_ENTRY, EIMConstant.STATUS_TYPE, (EIMStatusType)sec,
							EIMConstant.TARGET_REGISTED_ENTRY, EIMConstant.USERDEF_GROUP, userDefGroup, null);
				}
				else
				{
					OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.REGIST_USER_DEF_GROUP_ENTRY,
							EIMConstant.TARGET_TO_REGIST_ENTRY, EIMConstant.SECURITY, sec,
							EIMConstant.TARGET_REGISTED_ENTRY, EIMConstant.USERDEF_GROUP, userDefGroup, null);
				}
			}

			// 結果の保存
			entryTypeList.add(entryType);
			entryList.add(entry);
			entryObjNameList.add(entryObjName);
		}
		
		//Commit
		sess.commit();

		//XML
		out.println("<security");
			out.println(" secId=\"" + sec.getId() + "\"");
			out.println(" secName=\"" + StringUtils.xmlEncode(sec.getName()) + "\"");
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
		out.println("</security>");
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
