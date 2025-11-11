<%@page import="jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils"%>
<%@page import="jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum"%>
<%@page import="jp.co.ctc_g.eim.framework2.common.context.TransactionContext"%>
<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>

<%@ page import = "common.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "org.springframework.context.*" %>

<%@ page import = "app.document.approve.ApproveCommonUtil" %>
<%@ page import = "jp.co.ctc_g.eim.app.document.common.util.AppWorkFlowUtil" %>
<%@ page import = "jp.co.ctc_g.eim.framework.business.dao.AssignPlanDao" %>
<%@ page import = "jp.co.ctc_g.eim.framework.business.dao.StatusDao" %>
<%@ page import = "jp.co.ctc_g.eim.framework.business.domain.AssignDomain" %>
<%@ page import = "jp.co.ctc_g.eim.framework.business.domain.AssignPlanDomain" %>
<%@ page import = "jp.co.ctc_g.eim.framework.business.domain.EventLogDomain" %>
<%@ page import = "jp.co.ctc_g.eim.framework.business.domain.GuardConditionDomain" %>
<%@ page import = "jp.co.ctc_g.eim.framework.business.domain.entity.AttributeDomain" %>
<%@ page import = "jp.co.ctc_g.eim.framework.business.domain.entity.EventTypeDomain" %>
<%@ page import = "jp.co.ctc_g.eim.framework.business.domain.entity.ObjectDomain" %>
<%@ page import = "jp.co.ctc_g.eim.framework.business.domain.entity.StatusDomain" %>
<%@ page import = "jp.co.ctc_g.eim.framework.business.domain.entity.StatusTypeDomain" %>
<%@ page import = "jp.co.ctc_g.eim.framework.business.domain.entity.WorkFlowDomain" %>
<%@ page import = "jp.co.ctc_g.eim.framework.business.service.WorkFlowDefService" %>
<%@ page import = "jp.co.ctc_g.eim.framework.common.util.ApplicationContextLoader" %>
<%@ page import = "jp.co.ctc_g.eim.framework.common.util.EIMThreadContext" %>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	//Session
	EIMSession sess = null;
	EIMUser user = null;

	//Parameter
	String prmObjId = request.getParameter("objId");
	String prmFromView = request.getParameter("fromView"); // 開始、取り戻しを区別する。ステータスチェックのため。
	//Message
	String message = null;
	Object[] paramId = {
			"objId=" + prmObjId,
			"fromView=" + prmFromView
			};

	boolean sessPutflg = false;

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

		//前処理
		if(EIMThreadContext.getEIMSession() == null)
		{
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

		// Beanの取得
		AssignPlanDao assignPlanDao = (AssignPlanDao)ApplicationContextLoader.getContext().getBean("assignPlanDao");
		WorkFlowDefService workFlowDefService = (WorkFlowDefService)ApplicationContextLoader.getContext().getBean("workFlowDefService");

		//Object
		EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		if(object == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOTEXIST.DOCFOL");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOTEXIST.DOCFOL");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}

		//Check Document Type or Folder Type
		Map docObjTypeIdMap = AppObjectTypeUtil.getObjTypeMap(sess, EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT"));
		boolean isDocument = (docObjTypeIdMap.get(new Long(object.getType().getId())) != null);
		if (isDocument)
		{
			// Document Type
			long higherFolderId = AppObjectUtil.getIntAttr(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_HIGHER_WF_FOLDER"), -1);
			if (higherFolderId != -1)
			{
				//上位WFありDoc
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.EXIST.HIGHRANK.WFFOLDER", new Object[]{StringUtils.xmlEncode(object.getName())});
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.EXIST.HIGHRANK.WFFOLDER", new Object[]{StringUtils.xmlEncode(object.getName())});
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
				return;
			}
		}
		else
		{
			// Folder Type
			if (! AppObjectUtil.isWFFolder(sess, object))
			{
				//WFなしフォルダ
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOSELECT.DOC.OR.WFFOLDER", new Object[]{StringUtils.xmlEncode(object.getName())});
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOSELECT.DOC.OR.WFFOLDER", new Object[]{StringUtils.xmlEncode(object.getName())});
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
				return;
			}
		}
		//ステータスチェック
		if ("approve".equals(prmFromView)
			&& ( (object.getStatus() == null) || (object.getStatus() != null && object.getStatus().getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_EDITTING) )
		)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOTUPDATING", new Object[]{object.getName()});
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOTUPDATING", new Object[]{object.getName()});
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message));
			return;
		}

		if ("takeback".equals(prmFromView)
			&& ( (object.getStatus() == null) || (object.getStatus() != null && object.getStatus().getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_PROCESSING_PUBLIC) )
		)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOTPROCESSINGPUBLIC", new Object[]{object.getName()});
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOTPROCESSINGPUBLIC", new Object[]{object.getName()});
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message));
			return;
		}

		//WorkFlow
		EIMWorkFlow workflow = WorkFlowUtils.getWorkFlowByStatusType(sess, object.getStatus().getType());
		EIMObject wfSettingObj = null;
		WorkFlowDomain wfDomain = null;
		if(workflow != null)
		{
			//ワークフロー設定オブジェクト取得
			wfSettingObj = AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_WFSETTING"), "" + workflow.getId());

			//要承認WF/承認不要WFのチェック
			// 承認開始の際、WFが承認不要WFならばエラー
			// bossApprovalがnullになるのは、要承認WFのみなのでエラーにしないこと。
			String bossApproval = AppObjectUtil.getStrAttr(sess, wfSettingObj, EIMConfig.get("ATTR_NAME_WFSETTING_BOSS_APPROVAL_FLG"));
			if( !"takeback".equals(prmFromView) && !"publiccancel".equals(prmFromView) && bossApproval!=null && !bossApproval.equals("necessary") )
			{
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOT.NECESSARY.WORKFLOW", new Object[]{object.getName()});
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOT.NECESSARY.WORKFLOW", new Object[]{object.getName()});
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message));
				return;
			}

			// ワークフロー定義取得
			wfDomain = workFlowDefService.getDefById(workflow.getId());
		}

		//権限チェック
		if(object.getSecurity() != null)
		{
			// [開始]処理の場合
			if ("approve".equals(prmFromView))
			{
				if(SecurityUtils.authorized(sess, object, sess.getUser(), EIMAccessRole.UPDATE) != true)
				{
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOAPPRREQROLE");
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOAPPRREQROLE");
					log.warn(AppMessageUtils.makeLogMessage(user.getId(), message))	;
					return;
				}
			}
			// [公開取消]処理の場合
			else if ("publiccancel".equals(prmFromView))
			{
				if(SecurityUtils.authorized(sess, object, sess.getUser(), EIMAccessRole.CREATE) != true)
				{
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOPUBLICCANCELROLE");
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOPUBLICCANCELROLE");
					log.warn(AppMessageUtils.makeLogMessage(user.getId(), message))	;
					return;
				}
			}
			// [取戻し]処理の場合
			else
			{
				// システム管理権限を保有するかを取得
				Boolean isSystemSecurityAuth = AppSecurityUtils.authorizedBySystemSecurity(sess, EIMAccessRole.STATUS_UP);
				
				if(!isSystemSecurityAuth && !SecurityUtils.authorized(sess, object, sess.getUser(), EIMAccessRole.CREATE))
				{
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOCANAPPREQROLE");
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOCANAPPREQROLE");
					log.warn(AppMessageUtils.makeLogMessage(user.getId(), message))	;
					return;
				}
			}
		}
		
		//essential attribute
		EIMAttributeType attTypeOfModifyUser = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_MODIFY_USER"));
		EIMAttributeType attTypeOfModifyDate = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_MODIFY_DATE"));
		EIMAttributeType attTypeOfCreateDate = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_CREATE_DATE"));
		EIMAttributeType attTypeOfFizeSize = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_SIZE"));

		String modifyDate = DateUtils.getDBTzToCLTzDate(sess, object.getModifyDate());
		String createDate = DateUtils.getDBTzToCLTzDate(sess, object.getCreateDate());

		ObjectDomain objectDomain = new ObjectDomain(object);
		
		//XML
		out.println("<object");
			out.println(" objId=\"" + object.getId() + "\"");
			out.println(" objTypeId=\"" + object.getType().getId() + "\"");
			out.println(" objTypeName=\"" + StringUtils.xmlEncode(object.getType().getDefName()) + "\"");
			out.println(" objName=\"" + StringUtils.xmlEncode(object.getName()) + "\"");
			out.println(" rev=\"" + object.getRev() + "\"");
			out.println(" latest=\"" + object.getLatest() + "\"");
			out.println(" securityId=\"" + object.getSecurityId() + "\"");
			String createUserName = "";
			if (!isDocument)
			{
				createUserName = object.getCreateUser().getName();
			}
			if (object.getAttribute("作成者") != null)
			{
				createUserName = UserUtils.getUserById(sess, object.getAttribute("作成者").getInt()).getName();
			}
			out.println(" createUserName=\"" + StringUtils.xmlEncode(createUserName) + "\"");
			out.println(" createDate=\"" + createDate + "\"");
			out.println(" modifyUserName=\"" + StringUtils.xmlEncode(object.getModifyUser().getName()) + "\"");
			out.println(" modifyDate=\"" + modifyDate + "\"");
			String modifyDateTime = String.valueOf(object.getModifyDate().getTime() / 1000);
			out.println(" modifyDateTime=\"" + modifyDateTime + "\"");

			out.println(" attType_" + attTypeOfModifyUser.getId() + "=\"" + StringUtils.xmlEncode(object.getModifyUser().getName()) + "\"");
			out.println(" attType_" + attTypeOfModifyDate.getId() + "=\"" + modifyDate + "\"");
			out.println(" attType_" + attTypeOfCreateDate.getId() + "=\"" + createDate + "\"");
			if (isDocument) {
				EIMFile file = FileUtils.getFile(sess, object, FileUtils.getDefaultFormat(sess, object.getType()));
				out.println(" attType_" + attTypeOfFizeSize.getId() + "=\"" + file.getSize() + "\"");
			}

			out.println(" isDocument=\"" + Boolean.toString(isDocument) + "\"");
			out.println(" order=\"" + (isDocument ? "1" : "0") + "\"");

			//フルパス
			if(AppObjectUtil.getPath(object) != null)
			{
				out.println(" fullPath=\"" + StringUtils.xmlEncode(AppObjectUtil.getPath(object))
						+ StringUtils.xmlEncode(object.getName()) + "\"");
			}
			
			EventLogDomain eventLogDomain = null;
			long forcastStatusTypeId = 0;
			//Status
			if(object.getStatus() != null)
			{
				//Status
				EIMStatus status = object.getStatus();

				//Load Attribute
				StatusAttributeUtils.load(sess, status);

				out.println(" statusId=\"" + status.getId() + "\"");
				out.println(" statusTypeId=\"" + status.getType().getId() + "\"");
				out.println(" statusTypeName=\"" + StringUtils.xmlEncode(status.getType().getDefName()) + "\"");
				out.println(" step=\"" + status.getType().getStep() + "\"");

				//WorkFlow
				if(workflow != null)
				{
					out.println(" workFlowId=\"" + workflow.getId() + "\"");
					out.println(" workFlowName=\"" + StringUtils.xmlEncode(workflow.getName()) + "\"");
				}

				/*
				if(!"approve".equals(prmFromView)){
					//ワークフロー設定差戻し・取戻しメール通知フラグ
					EIMAttribute attrSendingBackAndRegainingMail = wfSettingObj.getAttribute(EIMConfig.get("ATTR_NAME_WFSETTING_MAIL_NOTICE_FLG"));
					out.println(" sendingBackAndRegainingMail=\"" + attrSendingBackAndRegainingMail.getInt() + "\"");
				}
				*/
				
				// 過去の承認依頼を取得する（差戻し、取り戻し以外の場合）
				if ( !"takeback".equals(prmFromView)){
					eventLogDomain = AppWorkFlowUtil.getLastExecEventLog(sess, object, object.getStatus());
				}
				AttributeDomain lastPublisherAttribute = null;
				AttributeDomain lastApproveTimingAttribute = null;
				AttributeDomain lastReplyAttribute = null;
				AttributeDomain lastCommentAttribute = null;
				
				// 遷移先予測
				forcastStatusTypeId = 0;
				if(eventLogDomain != null){
					// 過去自身が承認依頼したイベントが存在する場合
					out.println(" lastEventFlag=\"" + "true" + "\"");
					forcastStatusTypeId = eventLogDomain.getEvent().getToStatus().getStatusType().getId();
					for (AttributeDomain lastEvtAttr : eventLogDomain.getEvent().getAttrList()){
						// 過去のイベント属性を取得する
						if(lastEvtAttr.getAttrType().getDefName().equals(EIMConfig.getValue("ATTR_NAME_MAIL_NOTIFY_PUBLIC_NOTIFY_TO"))){
							// 公開通知先
							lastPublisherAttribute = lastEvtAttr;
						}
						if(lastEvtAttr.getAttrType().getDefName().equals(EIMConfig.getValue("ATTR_NAME_MAIL_NOTIFY_REQUEST_NOTIFY"))){
							// 承認依頼通知タイミング
							lastApproveTimingAttribute = lastEvtAttr;
						}
						if(lastEvtAttr.getAttrType().getDefName().equals(EIMConfig.getValue("ATTR_NAME_MAIL_NOTIFY_CONFIRM_RECEIVE"))){
							// 受信確認属性
							lastReplyAttribute = lastEvtAttr;
						}
						if(lastEvtAttr.getAttrType().getDefName().equals(EIMConfig.getValue("EVENT_ATTR_NAME_COMMENT"))){
							// コメント属性
							lastCommentAttribute = lastEvtAttr;
						}
					}
				}else{	
					EIMStatusType statusType = null;
					if ("approve".equals(prmFromView)){
						//ST遷移予測結果のステータスタイプID（承認依頼で遷移するベースイベントタイプ限定）
						statusType = AppWorkFlowUtil.getNextStatusType(object, AppConstant.BASE_EVENT_TYPE_ID_REQ_APPROVE);
						forcastStatusTypeId = statusType.getId();
					}else if("takeback".equals(prmFromView)){
						statusType = AppWorkFlowUtil.getNextStatusType(object, AppConstant.BASE_EVENT_TYPE_ID_TAKE_BACK);
						forcastStatusTypeId = statusType.getId();
					}else if("publiccancel".equals(prmFromView)){
						statusType = AppWorkFlowUtil.getNextStatusType(object, AppConstant.BASE_EVENT_TYPE_ID_PUBLIC_CANCEL);
						forcastStatusTypeId = statusType.getId();
					}else{
						statusType = AppWorkFlowUtil.getNextStatusType(object, AppConstant.BASE_EVENT_TYPE_ID_APPROVAL);
						forcastStatusTypeId = statusType.getId();
					}
				}
				out.println(" forcastStatusTypeId=\"" + (forcastStatusTypeId != 0 ? forcastStatusTypeId : "") + "\"");

				out.println(" statusMDateLong=\"" + object.getStatus().getMDate().getTime() + "\"");

				// 公開通知先
				EIMObject mailNotifyObj = null;
				if( "approve".equals(prmFromView) && workflow != null ) {
					
					long publicTiming = -1;
					String[] publisherIdList = null;
					String publicComment = null;
					
					// 過去自身が承認依頼したイベントが存在する場合
					if (eventLogDomain != null) {
						// メール通知オブジェクト取得
						mailNotifyObj = AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_MAIL_NOTIFY"), prmObjId);
					}
					
					String pubNotifCSVName = "";
					String pubNotifCSVID = "";
					
					// メール通知オブジェクトが存在する場合
					if (mailNotifyObj != null) {
						// 公開通知タイミング
						publicTiming = AppObjectUtil.getIntAttr(sess, mailNotifyObj, EIMConfig.get("ATTR_NAME_MAIL_NOTIFY_PUBLIC_TIMING"), -1);
						// 公開通知送信先
						publisherIdList = AppObjectUtil.getStrAttrs(sess, mailNotifyObj, EIMConfig.get("ATTR_NAME_MAIL_NOTIFY_PUBLIC_NOTIFY_TO"));
						// 公開通知コメント
						publicComment = AppObjectUtil.getTextAttr(sess, mailNotifyObj, EIMConfig.get("ATTR_NAME_PUBLIC_COMMENT"));
						
						// メール通知オブジェクトに公開通知先属性が存在する場合
						if (publisherIdList != null) {
							// 最後に設定された公開通知先取得
							for (String publisherId : publisherIdList) {
								String publisherName = ApproveCommonUtil.getNameFromCode(sess, publisherId).replaceAll("\"", "\"\"");
								if (publisherName.equals("")) {
									// 空文字列の場合は無効ユーザの場合のため除外
									continue;
								}
								if (pubNotifCSVName.length() < 1) {
									pubNotifCSVName = "\"" + publisherName;
									pubNotifCSVID = publisherId;
								} else {
									pubNotifCSVName = pubNotifCSVName + "\",\"" + publisherName;
									pubNotifCSVID = pubNotifCSVID + "," + publisherId;
								}
							}
							pubNotifCSVName = pubNotifCSVName + "\"";
						}
					} else {
						// デフォルト公開通知先を設定
						List<String> publisherIdNameList = AppWorkFlowUtil.getDefaultPublisher(sess, workflow);
						if (publisherIdNameList != null) {
							pubNotifCSVID = publisherIdNameList.get(0);
							pubNotifCSVName = publisherIdNameList.get(1);
						}
					}
					
					// メール通知タイミングを取得
					String immediatePublic = null;
					if (publicTiming != -1) {
						immediatePublic = AppWorkFlowUtil.getMailNoticeImmediate((int)publicTiming);
					} else {
						// ワークフローのデフォルト通知タイミングを設定
						immediatePublic = AppObjectUtil.getStrAttr(sess, wfSettingObj, EIMConfig.get("ATTR_NAME_WFSETTING_MAIL_NOTICE_DFLT"));
					}
					
					out.println(" publisherName=\"" + StringUtils.xmlEncode(pubNotifCSVName) + "\"");
					out.println(" publisherId=\"" + pubNotifCSVID + "\"");
					out.println(" publicComment=\"" + StringUtils.xmlEncode(StringUtils.nullToBlank(publicComment)) + "\"");
					out.println(" immediatePublic=\"" + StringUtils.nullToBlank(immediatePublic) + "\"");
				}
				
				// メール通知方法のデフォルト設定
				String immediateStr = "";
				if(lastApproveTimingAttribute != null){
					// 過去自身が承認依頼したイベントの属性に承認依頼通知タイミングが存在する場合
					int lastTiming = Integer.parseInt(lastApproveTimingAttribute.getValues()[0].toString());
					immediateStr = AppWorkFlowUtil.getMailNoticeImmediate(lastTiming);
				}else{
					immediateStr = AppObjectUtil.getStrAttr(sess, wfSettingObj, EIMConfig.get("ATTR_NAME_WFSETTING_MAIL_NOTICE_DFLT"));
				}
				out.println(" immediate=\"" + StringUtils.nullToBlank(immediateStr) + "\"");
				

				//受信確認、通信タイミング
				long reply = 0;
				if(lastReplyAttribute != null){
					// 過去自身が承認依頼したイベントの属性に受信確認が存在する場合
					reply = Integer.parseInt(lastReplyAttribute.getValues()[0].toString());
					
				}else{
					//メール通知オブジェクトを取得する
					if (mailNotifyObj == null) {
						mailNotifyObj = AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_MAIL_NOTIFY"), prmObjId);
					}
					if (mailNotifyObj != null) {
						reply = mailNotifyObj.getAttribute("受信確認").getInt();
					}
				}
				out.println(" reply=\"" + reply + "\"");
				
				// コメント
				String lastComment = "";
				if(lastCommentAttribute != null){
					lastComment = (String)lastCommentAttribute.getValues()[0];
				}
				out.println(" comment=\"" + StringUtils.xmlEncode(lastComment) + "\"");

				//現在のスタータスのステータスタイプ種別
				if (object.getStatus() != null)
				{
					out.println(" statusKind=\"" + object.getStatus().getType().getKind() + "\"");
					long docThrough = AppWorkFlowUtil.getDocThrough(wfDomain, object.getStatus().getType());
					out.println(" through=\"" + docThrough + "\"");
				}

				// 差戻し・取戻しメール通知
				if( !"approve".equals(prmFromView) && !"takeback".equals(prmFromView) && !"publiccancel".equals(prmFromView)) {
					EIMAttribute attrSendingBackAndRegainingMail = wfSettingObj.getAttribute(EIMConfig.get("ATTR_NAME_WFSETTING_MAIL_NOTICE_FLG"));
					out.println(" sendingBackAndRegainingMail=\"" + attrSendingBackAndRegainingMail.getInt() + "\"");
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
			}

			out.println(">");
			out.println("<statusList>");
			
			//ワークフローのイベントを取得
			List<EventTypeDomain> eventTypeList = wfDomain.getEventTypeList();
			//Fromがカレントステータスの承認依頼イベントリスト
			List<StatusTypeDomain> toStatusTypeList = new ArrayList<StatusTypeDomain>();
			
			//イベントリスト分回す
			for(int ii=0; ii < eventTypeList.size(); ii++){
				EventTypeDomain eventType = eventTypeList.get(ii);
				//ベースイベントタイプが承認依頼 かつ イベントタイプのfromがカレントのシーケンス
				if( (eventType.getBaseEventType().getId() == AppConstant.BASE_EVENT_TYPE_ID_REQ_APPROVE) &&
						(object.getStatus().getType().getStep() == eventType.getFromStatusType().getSeq() ) ){
					toStatusTypeList.add(eventType.getToStatusType());
				}
			}

			//ステータスタイプリスト分回す
			for(int ii=0 ; ii < toStatusTypeList.size() ;ii++){
				StatusTypeDomain toStatusType = toStatusTypeList.get(ii);
				//遷移先ステータスタイプの情報を取得
				if(toStatusType.getId() == forcastStatusTypeId){
				out.println("<status");
				out.println(" statusTypeId=\"" + toStatusType.getId() + "\"");
				out.println(" statusTypeName=\"" + StringUtils.xmlEncode(toStatusType.getDefName()) + "\"");
				out.println(" step=\"" + toStatusType.getSeq() + "\"");
				out.println(" statusKind=\"" + toStatusType.getStatusTypeKind().getId() + "\"");
				int docThrough = AppConstant.THROUGH_APPROVE_NONE;
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
				out.println(" functionType=\"" + "approve" + "\""); //<処理タイプ>承認
				
				
				//EventTypeDomainの値を詰める
				EIMStatusType nextStatusType = new EIMStatusType();
				nextStatusType.setId(toStatusType.getId());
				nextStatusType.setDefName(toStatusType.getDefName());
				
				//承認依頼先
				List<EIMUser> assignUserList = new ArrayList<EIMUser>();
					//ユーザリストからXML出力
					StringBuffer nameCSVBuf = new StringBuffer();
					StringBuffer idCSVBuf = new StringBuffer();

					if(eventLogDomain != null && toStatusType.getId() == eventLogDomain.getEvent().getToStatus().getStatusType().getId()){
	 					// 過去に自身が承認依頼したイベントが存在する場合、遷移先ステータスに設定されているアサイン予定を取得する
	 					out.println(" selectedFlag=\"" + "true" + "\"");
						AssignPlanDomain assignPlanCriteria = new AssignPlanDomain();
						assignPlanCriteria.setObject(objectDomain);
						assignPlanCriteria.setStatusType(toStatusType);
						List<AssignPlanDomain> assignPlanList = assignPlanDao.getList(assignPlanCriteria);
	 					HashMap<Long,Long> lastAssaignMap = new HashMap<Long,Long>();
						for (AssignPlanDomain assignPlan : assignPlanList) {
							lastAssaignMap.put((long)assignPlan.getOwner().getId(), (long)assignPlan.getOwner().getId());
						}

						// アサイン先として指定できるユーザを取得
						List<EIMUser> tempAssignUserList = AppWorkFlowUtil.getAssinEntryUserByStatusType(object, nextStatusType);
						for (EIMUser assignUser : tempAssignUserList){
							// 遷移先ステータスに設定されているアサイン予定(ユーザ)と、アサイン先として設定可能なユーザと比較する
							if(lastAssaignMap.get((long)assignUser.getId()) != null){
								assignUserList.add(assignUser);
							}
						}

						// SQL条件用にユーザIDを1000件ずつに分割
						List<List<Long>> userIdsList = new ArrayList<>();
						List<Long> userIdList= new ArrayList<>();
						int cnt = 0;
						for(EIMUser assignUser: assignUserList) {
							++cnt;

							userIdList.add(Long.valueOf(assignUser.getId()));

							// IDを1000件ずつに分割
							if(cnt == 1000) {
								cnt = 0;
								userIdsList.add(userIdList);
								userIdList = new ArrayList<>();
							}
						}

						if(userIdList.size() != 0) {
							userIdsList.add(userIdList);
						}

						Set<Long> authorizedUserIdList = AppWorkFlowUtil.getAuthorizedUserIds(object, userIdsList, EIMAccessRole.STATUS_UP);

						for (EIMUser assignUser : assignUserList){
							// 無効ユーザチェックを行う
							if(assignUser.getDisable() != 0){
								continue;
							}

							//承認権限のチェックを行う
							if (authorizedUserIdList.contains(Long.valueOf(assignUser.getId())))
							{
								if (nameCSVBuf.length() != 0)
								{
									nameCSVBuf.append(",");
									idCSVBuf.append(",");
								}
								nameCSVBuf.append(assignUser.getName());
								idCSVBuf.append("1:"+assignUser.getId());
							}
						}
						
				}else{
					// 承認依頼先デフォルト設定している場合次ステータスのエントリリストを全て取得して処理
					if ("approve".equals(prmFromView) && 
						AppObjectUtil.getIntAttr(sess, wfSettingObj, EIMConfig.get("ATTR_NAME_WFSETTING_SETTING_FLG"), AppConstant.FLAG_OFF) == AppConstant.FLAG_ON)
					{
						//ユーザリストをユーザ名称でソート
						assignUserList = AppWorkFlowUtil.getAssinEntryUserByStatusType(object, nextStatusType);

						// SQL条件用にユーザIDを1000件ずつに分割
						List<List<Long>> userIdsList = new ArrayList<>();
						List<Long> userIdList= new ArrayList<>();
						int cnt = 0;
						for(EIMUser assignUser: assignUserList) {
							++cnt;

							userIdList.add(Long.valueOf(assignUser.getId()));

							// IDを1000件ずつに分割
							if(cnt == 1000) {
								cnt = 0;
								userIdsList.add(userIdList);
								userIdList = new ArrayList<>();
							}
						}

						if(userIdList.size() != 0) {
							userIdsList.add(userIdList);
						}

						// 対象ユーザのロールマップを取得します
						Map<Long, List<EIMRole>> userRoleMap = AppWorkFlowUtil.getUserRoleMap(userIdsList);
						// 対象ユーザのグループマップを取得します
						Map<Long, List<EIMGroup>> userGroupMap = AppWorkFlowUtil.getUserGroupMap(userIdsList);

						Set<Long> authorizedUserIdList = AppWorkFlowUtil.getAuthorizedUserIds(object, userIdsList, EIMAccessRole.STATUS_UP);

						for (EIMUser assignUser : assignUserList){
							// 無効ユーザチェックを行う
							if(assignUser.getDisable() != 0){
								continue;
							}
							// デフォルト表示時、上長で絞り込む場合は上長チェックを行う
							if( AppWorkFlowUtil.checkDefaultBossOnly(wfSettingObj , toStatusType.getId()) ){
								EIMUser loginUser = (EIMUser)sess.getAttribute("USER");
								BossGroupUtils bossGroups = new BossGroupUtils(sess, loginUser);
								if(!userGroupMap.containsKey(Long.valueOf(assignUser.getId())) || !bossGroups.isInGroup(userGroupMap.get(Long.valueOf(assignUser.getId())))) {
									// 上長ではない
									continue;
								}

								BossRoleUtils bossRoles = new BossRoleUtils(sess, loginUser);
								if(!userRoleMap.containsKey(Long.valueOf(assignUser.getId())) || !bossRoles.isInRole(userRoleMap.get(Long.valueOf(assignUser.getId())))){
									// 上長ではない
									continue;
								}
							}

							//承認権限のチェックを行う
							if (authorizedUserIdList.contains(Long.valueOf(assignUser.getId())))
							{
								if (nameCSVBuf.length() != 0)
								{
									nameCSVBuf.append(",");
									idCSVBuf.append(",");
								}
								nameCSVBuf.append(assignUser.getName());
								idCSVBuf.append("1:"+assignUser.getId());
							}
						}
					}
				}
				out.println(" approverName=\"" + StringUtils.xmlEncode(nameCSVBuf.toString()) + "\"");
				out.println(" approverId=\"" + StringUtils.xmlEncode(idCSVBuf.toString()) + "\"");

				//レンダラーで表示するかどうかのdisplayFlag(デフォルトでtrue)
				out.println(" displayFlag=\"" + "true" + "\"");
				
				out.println(">");	
				out.println("</status>");
			}
		}
		out.println("</statusList>");
		out.println("</object>");
	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(user.getId(), eime.getMessage(), paramId), eime);
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
			if(sess != null){
				sess.close();
			}
			if(jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext.getTransactionContext() != null)
			{
				jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext.removeTransactionContext();
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
