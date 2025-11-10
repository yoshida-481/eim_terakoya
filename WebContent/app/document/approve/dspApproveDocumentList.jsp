<%@ page contentType = "text/xml; charset=UTF-8" %>

<%@page import="eim.bo.*"%>
<%@page import="eim.net.*"%>
<%@page import="eim.util.*"%>

<%@page import="common.util.*"%>

<%@page import="java.util.*"%>

<%@page import="org.apache.commons.logging.*"%>
<%@page import="org.springframework.context.*"%>

<%@page import="app.document.approve.ApproveCommonUtil"%>
<%@page import="jp.co.ctc_g.eim.app.document.common.util.AppWorkFlowUtil"%>

<%@page import="jp.co.ctc_g.eim.framework.business.dao.AssignDao"%>
<%@page import="jp.co.ctc_g.eim.framework.business.dao.EventHistoryDao"%>
<%@page import = "jp.co.ctc_g.eim.framework.business.dao.AssignPlanDao" %>
<%@page import="jp.co.ctc_g.eim.framework.business.dao.StatusDao"%>
<%@page import="jp.co.ctc_g.eim.framework.business.domain.AssignDomain"%>
<%@page import ="jp.co.ctc_g.eim.framework.business.domain.AssignPlanDomain" %>
<%@page import="jp.co.ctc_g.eim.framework.business.domain.EventLogDomain"%>
<%@page import="jp.co.ctc_g.eim.framework.business.domain.GuardConditionDomain"%>
<%@page import="jp.co.ctc_g.eim.framework.business.domain.entity.AttributeDomain"%>
<%@page import="jp.co.ctc_g.eim.framework.business.domain.entity.EventDomain"%>
<%@page import="jp.co.ctc_g.eim.framework.business.domain.entity.EventTypeDomain"%>
<%@page import="jp.co.ctc_g.eim.framework.business.domain.entity.ObjectDomain"%>
<%@page import="jp.co.ctc_g.eim.framework.business.domain.entity.StatusDomain"%>
<%@page import="jp.co.ctc_g.eim.framework.business.domain.entity.StatusTypeDomain"%>
<%@page import="jp.co.ctc_g.eim.framework.business.domain.entity.WorkFlowDomain" %>
<%@page import="jp.co.ctc_g.eim.framework.business.service.TaskService"%>
<%@page import="jp.co.ctc_g.eim.framework.business.service.WorkFlowDefService"%>
<%@page import="jp.co.ctc_g.eim.framework.common.util.EIMThreadContext"%>
<%@page import="jp.co.ctc_g.eim.framework.common.util.ApplicationContextLoader"%>
<%@page import="jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils"%>
<%@page import="jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum"%>
<%@page import="jp.co.ctc_g.eim.framework2.common.context.TransactionContext"%>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	//Session
	EIMSession sess = null;
	EIMUser user = null;

	//Message
	String message = null;

	//Parameter
	String prmObjId = request.getParameter("objId");
	Object[] paramId = {
			"objId=" + prmObjId
			};

	boolean sessPutflg = false;

	try{
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

		//前処理
		if(EIMThreadContext.getEIMSession() == null)
		{
			//Service、Dao呼出に必要
			EIMThreadContext.putEIMSession(sess);
			sessPutflg = true;
		}

		// TransactionContextの作成、設定
		if(jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext.getTransactionContext() != null)
		{
			jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext.removeTransactionContext();
		}
		TransactionContext context = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
		jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext.putTransactionContext(context);
		context.setLangId(sess.getLangId());
		context.setDBConnection(sess.getDBConnection());
		context.setUser(ConvertUtils.toUserDomain(user));

		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

		List<EIMObject> resultList = new ArrayList<EIMObject>();
		TaskService taskService = null;

		// Beanの取得
		AssignDao assignDao = (AssignDao)ApplicationContextLoader.getContext().getBean("assignDao");
		AssignPlanDao assignPlanDao = (AssignPlanDao)ApplicationContextLoader.getContext().getBean("assignPlanDao");
		WorkFlowDefService workFlowDefService = (WorkFlowDefService)ApplicationContextLoader.getContext().getBean("workFlowDefService");
		EventHistoryDao eventHistoryDao = (EventHistoryDao)ApplicationContextLoader.getContext().getBean("eventHistoryDaoWithoutAttribute");

		// 取得済み情報を保管するMap
		Map<Long, WorkFlowDomain> workflowMap = new HashMap<>();
		Map<Long, EIMObject> workflowSettingObjMap = new HashMap<>();

		//チェックand取得
		Map docObjTypeIdMap = AppObjectTypeUtil.getObjTypeMap(sess, EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT"));
		if (prmObjId != null && prmObjId.length() > 0)
		{
			//対象オブジェクト取得
			EIMObject obj = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
			if(obj == null)
			{
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOTEXIST.DOCFOL");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOTEXIST.DOCFOL");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
				return;
			}

			//Check Document Type or Folder Type
			boolean isDocument = (docObjTypeIdMap.get(new Long(obj.getType().getId())) != null);
			if (isDocument)
			{
				// Document Type
				long higherFolderId = AppObjectUtil.getIntAttr(sess, obj, EIMConfig.get("ATTR_NAME_DOCUMENT_HIGHER_WF_FOLDER"), -1);
				if (higherFolderId != -1)
				{
					//上位WFありDoc
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.EXIST.HIGHRANK.WFFOLDER", new Object[]{StringUtils.xmlEncode(obj.getName())});
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.EXIST.HIGHRANK.WFFOLDER", new Object[]{StringUtils.xmlEncode(obj.getName())});
					log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
					return;
				}
			}
			else
			{
				// Folder Type
//				if (obj.getStatus() == null)
				if (! AppObjectUtil.isWFFolder(sess, obj)){
					//WFなしフォルダ
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOSELECT.DOC.OR.WFFOLDER", new Object[]{StringUtils.xmlEncode(obj.getName())});
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOSELECT.DOC.OR.WFFOLDER", new Object[]{StringUtils.xmlEncode(obj.getName())});
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
					return;
				}
			}

			//ステータスチェック
			if (obj.getStatus() == null || (obj.getStatus().getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_APPROVE))
			{
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOTAPPROVING", new Object[]{obj.getName()});
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOTAPPROVING", new Object[]{obj.getName()});
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message));
				return;
			}

			//WorkFlow
			EIMWorkFlow workflowObj = WorkFlowUtils.getWorkFlowByStatusType(sess, obj.getStatus().getType());
			if(workflowObj != null)
			{
				//ワークフロー設定オブジェクト取得
				EIMObject wfSettingObj = AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_WFSETTING"), "" + workflowObj.getId());
				workflowSettingObjMap.put(new Long(workflowObj.getId()), wfSettingObj);

				//要承認WF/承認不要WFのチェック
				// 承認、承認待ち一覧の際、WFが承認不要WFならばエラー
				// bossApprovalがnullになるのは、要承認WFのみなのでエラーにしないこと。
				String bossApproval = AppObjectUtil.getStrAttr(sess, wfSettingObj, EIMConfig.get("ATTR_NAME_WFSETTING_BOSS_APPROVAL_FLG"));
				if( bossApproval!=null && !bossApproval.equals("necessary") )
				{
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOT.NECESSARY.WORKFLOW", new Object[]{obj.getName()});
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOT.NECESSARY.WORKFLOW", new Object[]{obj.getName()});
					log.warn(AppMessageUtils.makeLogMessage(user.getId(), message));
					return;
				}
			}

			// システム管理権限を保有するかを取得
			Boolean isSystemSecurityAuth = AppSecurityUtils.authorizedBySystemSecurity(sess, EIMAccessRole.STATUS_UP);

			//「ステータス変更」権限があるかをチェックする
			if(!isSystemSecurityAuth && !SecurityUtils.authorized(sess, obj,sess.getUser(), EIMAccessRole.STATUS_UP))
			{
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOAPPROVEROLE");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOAPPROVEROLE");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message))	;
				return;
			}

			// システム管理権限保有者はタスクのチェックは行わない
			if (!isSystemSecurityAuth) {
				// ログインユーザーのタスクリストを取得する
				List<EIMObject> taskObjectList = ApproveCommonUtil.getApproveRequestedTaskList(sess, user.getId());

				// タスクの存在チェック
				boolean findFlg = false;
				for(EIMObject taskObject : taskObjectList){
					long taskObjId = taskObject.getId();
					if(taskObjId == obj.getId()){
						findFlg = true;
						break;
					}
				}
				if(!findFlg) {
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOAPPROVEROLE");
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOAPPROVEROLE");
					log.warn(AppMessageUtils.makeLogMessage(user.getId(), message))	;
					return;
				}
			}
			resultList.add(obj);

		}
		else
		{
			// 処理待ち一覧の場合
			resultList = ApproveCommonUtil.getApproveRequestedTaskList(sess, user.getId());
		}
		// メール通知オブジェクトタイプ
		EIMObjectType objTypeMailNotify = ObjectUtils.getObjectTypeByName(EIMThreadContext.getEIMSession(), EIMConfig.getValue("OBJECT_TYPE_NAME_MAIL_NOTIFY"));

		//Root Node
		out.println("<objList>");

		// 対象ドキュメントの数分ループする
		for (Iterator<EIMObject> itr = resultList.iterator(); itr.hasNext();) {
			EIMObject object = (EIMObject) itr.next();

			//必要な属性情報が入っていないので、再取得
			if (prmObjId == null || prmObjId.length() <= 0) {
				object = ObjectUtils.getObjectById(sess, object.getId());
			}
			ObjectDomain objectDomain = new ObjectDomain(object);
			boolean isDocument = (docObjTypeIdMap.get(new Long(object.getType().getId())) != null);
			// メール通知オブジェクト取得
			EIMObject mailNotifyObj = ObjectUtils.getObjectByTypeAndName(sess, objTypeMailNotify, Long.toString(object.getId()));

			//Document
			out.println("<object");
				out.println(" objId=\"" + object.getId() + "\"");
				out.println(" objTypeId=\"" + object.getType().getId() + "\"");
				out.println(" objTypeName=\"" + StringUtils.xmlEncode(object.getType().getDefName()) + "\"");
				out.println(" objName=\"" + StringUtils.xmlEncode(object.getName()) + "\"");
				out.println(" rev=\"" + object.getRev() + "\"");
				out.println(" securityId=\"" + object.getSecurityId() + "\"");

				//Modify User
				if(object.getModifyUser() != null){
					out.println(" modifyUserName=\"" + StringUtils.xmlEncode(object.getModifyUser().getName()) + "\"");
					out.println(" modifyDate=\"" + object.getModifyDate() + "\"");
				}

				out.println(" isDocument=\"" + Boolean.toString(isDocument) + "\"");
				out.println(" order=\"" + (isDocument ? "1" : "0") + "\"");
				out.println(" enableFlag=\"" + "true" + "\""); //活性・非活性判定フラグ とりあえずtrueで返す（true = 活性）

				//パス、フルパス
				if(AppObjectUtil.getPath(object) != null)
				{
					out.println(" path=\"" + StringUtils.xmlEncode(AppObjectUtil.getPath(object)) + "\"");
					out.println(" fullPath=\"" + StringUtils.xmlEncode(AppObjectUtil.getPath(object))
							+ StringUtils.xmlEncode(object.getName()) + "\"");
				}

				//Property
				if(object.getAttribute("プロパティ") != null)
				{
					out.println(" property=\"" + StringUtils.xmlEncode(object.getAttribute("プロパティ").getString()) + "\"");
				}

				//Status
				EIMStatus status = object.getStatus();
				if(object.getStatus() != null)
				{
					out.println(" statusId=\"" + object.getStatus().getId() + "\"");
					out.println(" statusTypeId=\"" + object.getStatus().getType().getId() + "\"");
					out.println(" statusTypeName=\"" + StringUtils.xmlEncode(object.getStatus().getType().getDefName()) + "\"");
					out.println(" step=\"" + object.getStatus().getType().getStep() + "\"");
				}

				//workflow
				EIMWorkFlow workflow = WorkFlowUtils.getWorkFlowByStatusType(sess, status.getType());
				out.println(" workFlowId=\"" + workflow.getId() + "\"");

				WorkFlowDomain wfDomain = null;
				EIMObject wfSettingObj = null;

				// ワークフロー定義取得
				if (workflowMap.containsKey(workflow.getId())) {
					wfDomain = workflowMap.get(workflow.getId());
				} else {
					wfDomain = workFlowDefService.getDefById(workflow.getId());
					workflowMap.put(new Long(workflow.getId()), wfDomain);
				}

				//ワークフロー設定オブジェクト取得
				if (workflowSettingObjMap.containsKey(workflow.getId())) {
					wfSettingObj = workflowSettingObjMap.get(workflow.getId());
				} else {
					wfSettingObj = AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_WFSETTING"), String.valueOf(workflow.getId()));
					workflowSettingObjMap.put(new Long(workflow.getId()), wfSettingObj);
				}

				//ワークフロー公開処理オブジェクト取得
				long processingPublic = -1;
				for(int t = 0; t < workflow.getStatusTypeList().size(); t++){
					EIMStatusType sttype = (EIMStatusType)workflow.getStatusTypeList().get(t);
					if(sttype.getKind() == AppConstant.STATUS_TYPE_KIND_ID_PROCESSING_PUBLIC){
						processingPublic = sttype.getId();
						break;
					}
				}

				/*
				 * 有効期限切れ判定
				 */
				boolean expiration = false;
				EIMAttribute expirationDate = object.getAttribute(helper.getAttrNameOfEffectDate());
				if (expirationDate != null) {
					expiration = DateUtils.judgeExpirationDate(sess, expirationDate.getDate());
				}
				out.println(" expiration=\"" + expiration + "\"");

				//Lock User
				if(object.getLockUser() != null)
				{
					out.println(" lockUserName=\"" + StringUtils.xmlEncode(object.getLockUser().getName()) + "\"");
					out.println(" lockDate=\"" + object.getLockDate() + "\"");
				}

				// 署名・暗号化状態
				long signencr = AppObjectUtil.getIntAttr(sess, object, helper.getAttrNameOfSignEncStatus(), AppConstant.SIGNENCR_KIND_NOT_SIGNENCR);
				out.println(" signencr=\"" + signencr + "\"");

				// イベント履歴を取得する
				List<EventLogDomain> eventLogList = eventHistoryDao.getByObjId(object.getId()).getEventLogList();

				// 過去、自身が承認依頼・承認したイベントを取得する
				EventLogDomain eventLogDomain = AppWorkFlowUtil.getLastExecEventLog(eventLogList, object.getStatus(), sess.getUser());
				AttributeDomain lastApproveTimingAttribute = null;
				AttributeDomain lastCommentAttribute = null;

				//次の遷移先を取得
				if(eventLogDomain != null){
					// 過去、自身が承認したイベントが存在する場合
					out.println(" lastEventFlag=\"" + "true" + "\"");
					if(eventLogDomain.getEvent().getAttrList() != null){
						for (AttributeDomain lastEvtAttr : eventLogDomain.getEvent().getAttrList()){
							// 過去のイベント属性を取得する
							if(lastEvtAttr.getAttrType().getDefName().equals(EIMConfig.getValue("ATTR_NAME_MAIL_NOTIFY_REQUEST_NOTIFY"))){
								// 承認依頼通知タイミング
								lastApproveTimingAttribute = lastEvtAttr;
							}
							if(lastEvtAttr.getAttrType().getDefName().equals(EIMConfig.getValue("EVENT_ATTR_NAME_COMMENT"))){
								// コメント属性
								lastCommentAttribute = lastEvtAttr;
							}
						}
					}
				}
				// 次の遷移先は依頼時に設定した遷移先を取得
				long nextStatusTypeId = AppWorkFlowUtil.getNextStatusTypeDoc(object, workflow).getId();
				long docNextStatusTypeKind = AppWorkFlowUtil.getNextStatusTypeDoc(object, workflow).getKind();

				int docThrough = AppWorkFlowUtil.getDocThrough(wfDomain, object.getStatus().getType());
				out.println(" forcastStatusTypeId=\"" + nextStatusTypeId + "\"");
				out.println(" statusMDateLong=\"" + object.getStatus().getMDate().getTime() + "\"");
				out.println(" statusKind=\"" + docNextStatusTypeKind + "\"");
				out.println(" through=\"" + docThrough + "\"");

				long approvalCnt = 0;
				// 全員承認の場合、承認済みリストを取得する
				List<AssignDomain> approverList = new ArrayList<AssignDomain>();
				if (docThrough == AppConstant.THROUGH_APPROVE_ALL) {
					AssignDomain prmAssignDomain = new AssignDomain();
					prmAssignDomain.setStatus(new StatusDomain(status));
					List<AssignDomain> assignDomainList = assignDao.getList(prmAssignDomain);

					for(AssignDomain as : assignDomainList){
						approvalCnt++;
						if(as.getEvent() != null && as.getEvent().getId() != 0){
							approverList.add(as);
						}
					}
				}

				boolean isPartialApproval = true;
				if(docThrough == AppConstant.THROUGH_APPROVE_ONE){
					//一人承認の場合は部分承認はない
					isPartialApproval = false;
				}else if((docThrough == AppConstant.THROUGH_APPROVE_ALL) && ((approvalCnt - approverList.size()) == 1 ) ){
					//全員承認、かつ最終承認の場合
					isPartialApproval = false;
				}else{
					//全員承認、かつ部分承認の場合
					isPartialApproval = true;
				}
				//全員承認で部分承認かどうか
				out.println(" isPartialApproval=\"" + (isPartialApproval == true ? "true" : "false") + "\"");

				// PDF変換フラグ取得
				EIMObject wfPublicObj = AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_WFPUB"), String.valueOf(processingPublic));
				long isChangePDF = 0;
				if(wfPublicObj != null){
					EIMAttribute isChangePDFAttr = wfPublicObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_FLG"));
					if(isChangePDFAttr != null){
						isChangePDF = isChangePDFAttr.getInt();
					}
				}
				out.println(" isChangePDF=\"" + isChangePDF + "\"");

				// 公開PDF事前登録済かどうか
				Date pdfPreRegistDate = AppObjectUtil.getDateAttr(sess, object,  EIMConfig.get("ATTR_NAME_DOCUMENT_PDF_PRE_REGIST_DATE"));
				// nullでなければ公開PDF事前登録済み
				boolean isPdfPreRegistered = pdfPreRegistDate != null ? true : false;
				out.println(" isPdfPreRegistered=\"" + isPdfPreRegistered + "\"");

				//ワークフロー設定差戻し・取戻しメール通知フラグ
				EIMAttribute attrSendingBackAndRegainingMail = wfSettingObj.getAttribute(EIMConfig.get("ATTR_NAME_WFSETTING_MAIL_NOTICE_FLG"));
				out.println(" sendingBackAndRegainingMail=\"" + attrSendingBackAndRegainingMail.getInt() + "\"");

				//メール通知：承認依頼通知タイミング
				long timing = -1;
				String immediate = null;
				if(lastApproveTimingAttribute != null){
					// 過去、自身が承認したイベントが存在する場合
					timing = Integer.parseInt(lastApproveTimingAttribute.getValues()[0].toString());
				}else{
					// 過去、自身が承認したイベントが存在しない場合
					if(mailNotifyObj != null){
						// メール通知オブジェクトが存在する場合
						timing = mailNotifyObj.getAttribute(EIMConfig.get("ATTR_NAME_MAIL_NOTIFY_REQUEST_NOTIFY")).getInt();
					}else{
						// メール通知オブジェクトが存在しない場合、ワークフロー設定オブジェクト：ワークフロー設定差戻し・取戻しメール通知フラグを返却
						immediate = AppObjectUtil.getStrAttr(sess, wfSettingObj, EIMConfig.get("ATTR_NAME_WFSETTING_MAIL_NOTICE_DFLT"));
					}
				}
				if(immediate == null){
					immediate = AppWorkFlowUtil.getMailNoticeImmediate((int)timing);
				}
				out.println(" immediate=\"" + immediate + "\"");
				if (timing == -1) {
					timing = AppWorkFlowUtil.getMailNoticeTiming(immediate);
				}
				out.println(" timing=\"" + timing + "\"");

				String lastComment = "";
				if(lastCommentAttribute != null){
					// 過去自身が承認した場合、入力したコメントを返却する
					lastComment = (String)lastCommentAttribute.getValues()[0];
				}
				out.println(" comment=\"" + lastComment + "\"");

				// 依頼先ユーザ、依頼日
				String requestUserName = "";
				String requestDate = "";
				String requestDateLong = "";

				// 現在のステータスに遷移するために実行された承認イベント、または承認依頼イベントを取得する
				List<EventDomain> eventDomainList = AppWorkFlowUtil.getOneAheadApprovalRequestEvent(eventLogList, wfDomain, object.getStatus().getType().getStep());

				for(int i = 0; i < eventDomainList.size(); i++){
					if (i > 0)
					{
						requestUserName += ",";
					}
					requestUserName += StringUtils.xmlEncode((UserUtils.getUserById(sess, eventDomainList.get(i).getCUser().getId())).getName());
					if(i == (eventDomainList.size() - 1)){
						requestDate += DateUtils.getDBTzToCLTzDate(sess, eventDomainList.get(i).getCDate());
						requestDateLong += eventDomainList.get(i).getCDate().getTime();
					}
				}
				out.println(" requestUserName=\"" + requestUserName + "\"");
				out.println(" requestDate=\"" + requestDate + "\"");
				out.println(" requestDateLong=\"" + requestDateLong + "\"");

				// 公開通知
				String publicComment  = "";
				String pubNotifCSVName  = "";
				String pubNotifCSVID  = "";
				String immediatePublic = "";
				if(mailNotifyObj != null){
					// 公開通知コメント
					publicComment = AppObjectUtil.getTextAttr(sess,mailNotifyObj, EIMConfig.get("ATTR_NAME_PUBLIC_COMMENT"));

					// 公開通知先
					String[] publisherIdList = AppObjectUtil.getStrAttrs(sess, mailNotifyObj, EIMConfig.get("ATTR_NAME_MAIL_NOTIFY_PUBLIC_NOTIFY_TO"));
					if(publisherIdList != null){
						for(String publisherId : publisherIdList) {
							String publisherName = ApproveCommonUtil.getNameFromCode(sess, publisherId).replaceAll("\"", "\"\"");
							if(publisherName.equals("")){
								// 空文字列の場合は無効ユーザの場合のため除外
								continue;
							}
							if(pubNotifCSVName.length() < 1){
								pubNotifCSVName = "\"" + publisherName;
								pubNotifCSVID = publisherId;
							}else{
								pubNotifCSVName = pubNotifCSVName + "\",\"" + publisherName;
								pubNotifCSVID = pubNotifCSVID + "," + publisherId;
							}
						}
						pubNotifCSVName = pubNotifCSVName + "\"";
					}
					long sendNotifyMailTiming = AppObjectUtil.getIntAttr(sess, mailNotifyObj, EIMConfig.get("ATTR_NAME_MAIL_NOTIFY_PUBLIC_TIMING"), -1);

					// 公開通知メールの送信タイミング
					if(sendNotifyMailTiming != -1){
						immediatePublic = AppWorkFlowUtil.getMailNoticeImmediate((int)sendNotifyMailTiming);
					}
				}
				out.println(" publicComment=\"" + StringUtils.xmlEncode(StringUtils.nullToBlank(publicComment)) + "\"");
				out.println(" publisherName=\"" +  StringUtils.xmlEncode(StringUtils.nullToBlank(pubNotifCSVName)) + "\"");
				out.println(" publisherId=\"" +  StringUtils.xmlEncode(StringUtils.nullToBlank(pubNotifCSVID)) + "\"");
				out.println(" immediatePublic=\"" + StringUtils.nullToBlank(immediatePublic) + "\"");

				out.println(">");
				out.println("<statusList>");

				//ワークフローのイベントタイプを取得
				List<EventTypeDomain> eventTypeList = wfDomain.getEventTypeList();

				//Fromがカレントステータスの承認イベントリスト
				List<StatusTypeDomain> toStatusTypeList = new ArrayList<StatusTypeDomain>();
				//イベントリスト分回す
				for(int ii=0 ; ii < eventTypeList.size() ;ii++){
					EventTypeDomain eventType = eventTypeList.get(ii);

					//(イベントタイプが承認)&&(イベントタイプのfromがカレントのシーケンス)
					if( eventType.getBaseEventType().getId() == AppConstant.BASE_EVENT_TYPE_ID_APPROVAL
							&& object.getStatus().getType().getStep() == eventType.getFromStatusType().getSeq()){

						StatusTypeDomain toStatusType = eventType.getToStatusType();
						if(toStatusType.getStatusTypeKind().getId() == AppConstant.STATUS_TYPE_KIND_ID_PROCESSING_PUBLIC){
							// 公開処理中の場合は除外
							continue;
						}
						if(object.getStatus().getType().getStep() == toStatusType.getSeq()){
							// 全承認用のステータスは除外
							continue;
						}
						toStatusTypeList.add(eventType.getToStatusType());
					}
				}
				//ソート後のリスト
				List<StatusTypeDomain> sortedToStatusTypeList = new ArrayList<StatusTypeDomain>();

				//toStatusType分回す
				for(int ii=0 ; ii < toStatusTypeList.size() ;ii++){
					StatusTypeDomain toStatusType = toStatusTypeList.get(ii);
					//遷移先ステータスタイプの情報を取得
					out.println("<status");
					out.println(" statusTypeId=\"" + toStatusType.getId() + "\"");
					out.println(" statusTypeName=\"" + StringUtils.xmlEncode(toStatusType.getDefName()) + "\"");
					long toStatusKind = toStatusType.getStatusTypeKind().getId();
					out.println(" statusKind=\"" + toStatusKind + "\"");		//<ステータス種別>
					out.println(" step=\"" + toStatusType.getSeq() + "\"");
					String statusfinalApprove = (toStatusKind == AppConstant.STATUS_TYPE_KIND_ID_PROCESSING_PUBLIC
							|| toStatusKind == AppConstant.STATUS_TYPE_KIND_ID_PUBLIC) ? "true" : "false";
					out.println(" finalApprove=\"" + statusfinalApprove + "\"");
					docThrough = AppConstant.THROUGH_APPROVE_NONE;
					if (toStatusType.getStatusTypeKind().getId() == AppConstant.STATUS_TYPE_KIND_ID_APPROVE) {
						// 承認依頼中ステータスタイプの通過条件を取得
						// see AppWorkFlowUtil#getDocThrough(EIMStatusType)
						// 該当しない場合は一人承認
						docThrough = AppConstant.THROUGH_APPROVE_ONE;
						for (EventTypeDomain eventTypeDomain : eventTypeList) {
							StatusTypeDomain fromStatusTypeDomain = eventTypeDomain.getFromStatusType();
							if (toStatusType.getId() == fromStatusTypeDomain.getId()) {
								GuardConditionDomain gcDomain = eventTypeDomain.getGuardCondition();
								//ガード条件「最後の承認者ではない」
								if (gcDomain.getId() == AppConstant.GUARD_COND_ID_FINAL_APPROVED_IS_NOT) {
									//FromステータスタイプIDとToステータスタイプIDが同じイベントタイプ
									if (eventTypeDomain.getFromStatusType().getId() == eventTypeDomain.getToStatusType().getId()) {
										//全員承認
										docThrough = AppConstant.THROUGH_APPROVE_ALL;
									}
								}
							}
						}
					}
					out.println(" through=\"" + docThrough + "\"");
					out.println(" functionType=\"" + "approve" + "\"");		//<処理タイプ>承認

					EIMStatusType nextStatusType = new EIMStatusType();
					nextStatusType.setId(toStatusType.getId());
					nextStatusType.setDefName(toStatusType.getDefName());
					//承認依頼先
					List<EIMUser> assignUserList = new ArrayList<EIMUser>();
					//ユーザリストからXML出力
					StringBuffer userNameCSVBuf = new StringBuffer();
					StringBuffer userIdCSVBuf = new StringBuffer();

					//遷移先ステータスに設定されているアサイン予定が存在する場合取得する
					AssignPlanDomain assignPlanCriteria = new AssignPlanDomain();
					assignPlanCriteria.setObject(objectDomain);
					assignPlanCriteria.setStatusType(toStatusType);
					List<AssignPlanDomain> assignPlanList = assignPlanDao.getList(assignPlanCriteria);

					List<Long> userIds = new ArrayList<Long>();
					for (AssignPlanDomain assignPlan : assignPlanList) {
						if (assignPlan.getOwner() != null) {
							userIds.add( Long.valueOf(assignPlan.getOwner().getId()) );
						}
					}
					List<EIMUser> assignPlanUserList = UserUtils.getUserByIds(sess, userIds);
					for (EIMUser assignPlanUser : assignPlanUserList) {
						// 無効ユーザチェックを行う
						if(assignPlanUser.getDisable() != 0) {
							continue;
						}
						// 承認権限のチェックを行う
						if (!SecurityUtils.authorized(sess, object, assignPlanUser, EIMAccessRole.STATUS_UP)) {
							continue;
						}
						// 返却値生成（,区切りで返却）
						if (userNameCSVBuf.length() != 0)
						{
							userNameCSVBuf.append(",");
							userIdCSVBuf.append(",");
						}
						userNameCSVBuf.append(assignPlanUser.getName());
						userIdCSVBuf.append("1:"+assignPlanUser.getId());
					}
					out.println(" approverName=\"" + StringUtils.xmlEncode(userNameCSVBuf.toString()) + "\"");
					out.println(" approverId=\"" + StringUtils.xmlEncode(userIdCSVBuf.toString()) + "\"");

					//表示・非表示のフラグ(デフォルトでtrue)
					out.println(" displayFlag=\"" + "true" + "\"");
					out.println(">");
					out.println("</status>");
				}
			out.println("</statusList>");
			out.println("</object>");
		}

		//End Root Node
		out.println("</objList>");

	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(user.getId(), eime.getMessage()), eime);
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
			//Remove Session from Thread Local Table
			if(sessPutflg == true){
				EIMThreadContext.removeEIMSession();
				sessPutflg = false;
			}
			if(jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext.getTransactionContext() != null)
			{
				jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext.removeTransactionContext();
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
