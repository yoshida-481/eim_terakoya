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

	class CreateStatusUtils
	{
	   /**
	    * コンストラクタ
	    */
		public CreateStatusUtils()
		{
		}

		private String getDefStatusName(String key){
			return EIMResource.getMessage("JA", key);
		}


		public void createStatusType(EIMSession		sess,
										EIMWorkFlow		workFlow,
										String			key,
										int				kind)
		throws Exception
		{

			/*
			 * Create Status Type
			 */
			String defName = getDefStatusName(key);
			EIMStatusType statusType = WorkFlowUtils.createStatusType(sess, workFlow, defName, kind);

			/*
			 * Create Status Type Other
			 */
			List langIdList = EIMXmlConfigLanguage.getLangIdList();
			int langIdNum = langIdList.size();

			for(int i = 0; i < langIdNum; i++)
			{
				String langId = (String)langIdList.get(i);
				String otherName = EIMResource.getMessage(langId, key);

				// Create Status Type Other
				WorkFlowUtils.addOtherStatusTypeName(sess, statusType.getId(), langId, otherName);
			}
			String adminAppId = (String)sess.getAttribute("ADMIN_APP_ID");
			// 公開処理中ステータス生成の場合
			if(kind == AppConstant.STATUS_TYPE_KIND_ID_PROCESSING_PUBLIC && AppConstant.ADMIN_APP_ID_DOCUMENT.equals(adminAppId)){
				// ワークフロー公開処理オブジェクトの生成
				AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_WFPUB"), String.valueOf(statusType.getId()));
			}

			//Create Operation History
			OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.CREATE_STATUS,
					EIMConstant.TARGET_PARENT_WORKFLOW, EIMConstant.WORKFLOW, workFlow,
					EIMConstant.TARGET_CREATE, EIMConstant.STATUS_TYPE, statusType, null );
		}
	}
	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	//Session
	EIMSession sess = null;
	EIMUser loginUser = null;

	//Parameter
//	String prmWorkFlowName = EIMUtils.getParameter(request, "workFlowName");
	int prmOtherCnt = LanguageFieldUtil.getOtherCnt(request);
	String prmDefNotifyMail = EIMUtils.getParameter(request, "defNotifyMail");
	String prmDefApproveRequest = EIMUtils.getParameter(request, "defApproveRequest");
	String prmProcessWaitPopup = EIMUtils.getParameter(request, "processWaitPopup");
	String prmBackMail = EIMUtils.getParameter(request, "backMail");
	String prmPublishNotifyMail = EIMUtils.getParameter(request, "publishNotifyMail");

	//Message
	String message = null;
	Object[] paramId = {
			"otherCnt=" + prmOtherCnt
			};

	try
	{
		/*
		 * param check
		 */
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

		//ネームスペース付きの定義名称を取得
		String prmDefName = LanguageFieldUtil.getDefName(sess, null, request, prmOtherCnt);

		/*
		 * Create WorkFlow
		 */
		// Create WorkFlow
		EIMWorkFlow workFlow = WorkFlowUtils.createWorkFlow(sess, prmDefName);
//		 EIMWorkFlow workFlow = WorkFlowUtils.createWorkFlow(sess, prmWorkFlowName);

		/*
		 * Create WorkFlow Other
		 */
		for(int i = 0; i < prmOtherCnt; i++)
		{
			String prmOtherName = LanguageFieldUtil.getOtherName(request,i);
			String prmOtherLId = LanguageFieldUtil.getOtherLId(request,i);

			// Create Object Type
			WorkFlowUtils.addOtherWorkFlowName(sess, workFlow.getId(), prmOtherLId, prmOtherName);
		}

		String adminAppId = (String)sess.getAttribute("ADMIN_APP_ID");
		if(AppConstant.ADMIN_APP_ID_DOCUMENT.equals(adminAppId)){
			// Create Work Flow Setting Object
			EIMObject workFlowSettingObj = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_WFSETTING"), String.valueOf(workFlow.getId()));
			AppObjectUtil.setAttr(sess, workFlowSettingObj, EIMConfig.get("ATTR_NAME_WFSETTING_MAIL_NOTICE_DFLT"), prmDefNotifyMail);
			AppObjectUtil.setAttr(sess, workFlowSettingObj, EIMConfig.get("ATTR_NAME_WFSETTING_SETTING_FLG"), Integer.parseInt(prmDefApproveRequest));
			AppObjectUtil.setAttr(sess, workFlowSettingObj, EIMConfig.get("ATTR_NAME_WFSETTING_POPUP_NOTICE_FLG"), Integer.parseInt(prmProcessWaitPopup));
			AppObjectUtil.setAttr(sess, workFlowSettingObj, EIMConfig.get("ATTR_NAME_WFSETTING_MAIL_NOTICE_FLG"), Integer.parseInt(prmBackMail));
			AppObjectUtil.setAttr(sess, workFlowSettingObj, EIMConfig.get("ATTR_NAME_WFSETTING_PUBLISHNOTIFY_FLG"), Integer.parseInt(prmPublishNotifyMail));
		}

		//Create Operation History
		OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.CREATE_WORKFLOW,
				EIMConstant.TARGET_CREATE, EIMConstant.WORKFLOW, workFlow,
				null, null, null, null);

		boolean isAdminAppDocument = ((String)session.getAttribute("ADMIN_APP_ID")).equals(AppConstant.ADMIN_APP_ID_DOCUMENT);

		if (isAdminAppDocument)
		{
			CreateStatusUtils createStatusUtils = new CreateStatusUtils();

			createStatusUtils.createStatusType( sess,
												workFlow,
												"EIM.STATUSTYPE.EDITING",
												AppConstant.STATUS_TYPE_KIND_ID_EDITTING);

			createStatusUtils.createStatusType( sess,
												workFlow,
												"EIM.STATUSTYPE.REQUESTAPPROVE",
												AppConstant.STATUS_TYPE_KIND_ID_APPROVE);

			createStatusUtils.createStatusType( sess,
												workFlow,
												"EIM.STATUSTYPE.PROCESSINGPUBLIC",
												AppConstant.STATUS_TYPE_KIND_ID_PROCESSING_PUBLIC);

			createStatusUtils.createStatusType( sess,
												workFlow,
												"EIM.STATUSTYPE.PUBLIC",
												AppConstant.STATUS_TYPE_KIND_ID_PUBLIC);
		}

		//Commit
		sess.commit();

		//XML
		out.println("<workFlow");
			out.println(" workFlowId=\"" + workFlow.getId() + "\"");
			out.println(" workFlowName=\"" + StringUtils.xmlEncode(workFlow.getName()) + "\"");
			out.println(">");
		out.println("</workFlow>");

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
