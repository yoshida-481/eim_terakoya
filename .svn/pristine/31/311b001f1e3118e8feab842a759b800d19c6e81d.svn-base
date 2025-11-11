<%@page import="jp.co.ctc_g.eim.framework.business.domain.entity.OtherNameDomain"%>
<%@page import="eim.util.ObjectUtils"%>
<%@page import="eim.bo.EIMObjectType"%>
<%@page import="jp.co.ctc_g.eim.app.document.common.util.AppWorkFlowUtil"%>
<%@page import="eim.bo.EIMAccessRole"%>
<%@page import="eim.util.SecurityUtils"%>
<%@page import="jp.co.ctc_g.eim.framework.business.domain.entity.AttributeDomain"%>
<%@page import="eim.bo.EIMRole"%>
<%@page import="eim.bo.EIMGroup"%>
<%@page import="common.util.BossRoleUtils"%>
<%@page import="common.util.BossGroupUtils"%>
<%@page import="eim.bo.EIMAttribute"%>
<%@page import="eim.bo.EIMObject"%>
<%@page import="common.util.AppObjectUtil"%>
<%@page import="jp.co.ctc_g.eim.app.document.common.util.AppWorkFlowUtil2"%>
<%@ page language="java" contentType="text/xml; charset=UTF-8" pageEncoding="UTF-8"%>

<%@page import="jp.co.ctc_g.eim.framework.common.util.EIMThreadContext"%>
<%@page import="jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils"%>
<%@page import="jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum"%>
<%@page import="jp.co.ctc_g.eim.framework2.common.context.TransactionContext"%>
<%@page import="common.util.AppMessageUtils"%>
<%@page import="jp.co.ctc_g.eim.framework.business.domain.EventHistoryDomain"%>
<%@page import="jp.co.ctc_g.eim.framework.business.service.EventHistoryService"%>

<%@page import="jp.co.ctc_g.eim.framework.business.service.StatusService"%>
<%@page import="eim.bo.EIMUser"%>
<%@page import="org.apache.commons.logging.LogFactory"%>
<%@page import="org.apache.commons.logging.Log"%>

<%@page import="java.util.List"%>
<%@page import="java.util.ArrayList"%>
<%@page import="java.util.Map"%>
<%@page import="java.util.HashMap"%>
<%@page import="java.util.Collections"%>
<%@page import="jp.co.ctc_g.eim.app.document.business.domain.WorkFlowHistoryDomain"%>
<%@page import="jp.co.ctc_g.eim.framework.business.domain.entity.WorkFlowDomain"%>
<%@page import="jp.co.ctc_g.eim.framework.business.domain.entity.StatusDomain"%>
<%@page import="jp.co.ctc_g.eim.framework.business.domain.entity.ObjectDomain"%>
<%@page import="jp.co.ctc_g.eim.framework.business.domain.entity.EventDomain"%>
<%@page import="jp.co.ctc_g.eim.framework.business.domain.AssignDomain"%>
<%@page import="jp.co.ctc_g.eim.framework.business.domain.EventLogDomain"%>
<%@page import="jp.co.ctc_g.eim.framework.business.domain.entity.EventTypeDomain"%>
<%@page import="jp.co.ctc_g.eim.framework.business.domain.BaseEventTypeDomain"%>
<%@page import="jp.co.ctc_g.eim.framework.business.domain.BelongType"%>
<%@page import="eim.util.StringUtils"%>
<%@page import="eim.util.DateUtils"%>
<%@page import="eim.util.EIMUtils"%>
<%@page import="eim.util.EIMConfig"%>
<%@page import="eim.bo.EIMResource"%>
<%@page import="eim.bo.EIMWorkFlow"%>
<%@page import="eim.bo.EIMStatusType"%>
<%@page import="eim.net.EIMSession"%>
<%@page import="common.util.AppConstant"%>
<%@page import="jp.co.ctc_g.eim.framework.business.domain.BelongDomain"%>
<%@page import="jp.co.ctc_g.eim.framework.common.exception.EIMAppException"%>
<%@page import="jp.co.ctc_g.eim.framework.business.dao.UserDefGroupDao"%>
<%@page import="jp.co.ctc_g.eim.framework.business.dao.SysFuncDao"%>
<%@page import="org.springframework.context.ApplicationContext"%>
<%@page import="jp.co.ctc_g.eim.framework.common.util.ApplicationContextLoader"%>
<%@page import="eim.bo.EIMException"%>
<result>

<%

	class DataUtil
	{
		UserDefGroupDao userDefGroupDao;
		SysFuncDao sysFuncDao;

		// コンストラクタ
		DataUtil(ApplicationContext contxt) {

			userDefGroupDao = (UserDefGroupDao)contxt.getBean("userDefGroupDao");
			sysFuncDao =  (SysFuncDao)contxt.getBean("sysFuncDao");
		}

		// アサイン先から、所属元の文字列を生成する（画面表示用）
		public String getBelongString(AssignDomain assign) throws Exception
		{
			StringBuffer sb = new StringBuffer();
			for (BelongDomain belong : assign.getBelongList()) {

				// ユーザの場合は出力しない
				if (belong.getBelongType().getSymbol().equals(BelongType.USER.getSymbol()) ) {
					continue;
				}

				// 複数の所属元を出力する場合はパイプで区切る
				if (sb.length() > 0) {sb.append(" | ");}

				// 所属元の名称を取得して追加
				if (belong.getBelongType().getSymbol().equals(BelongType.USERDEFGROUP.getSymbol()) ) {

					sb.append(userDefGroupDao.getById(belong.getBelonging().getId()).getName());
				}
				else if (belong.getBelongType().getSymbol().equals(BelongType.SYSFUNC.getSymbol())) {

					sb.append(sysFuncDao.getById(belong.getBelonging().getId()).getName());
				}
				else {
					sb.append(belong.getBelonging().getName());
				}
			}

			return (sb.length() > 0)? sb.toString():null;
		}

		// ベースイベントタイプ名をアプリケーションに適した名称にして返却する
		public String getBaseEventTypeName(EIMSession sess, EventDomain event) throws Exception
		{
			String ret;
		if(event.getEventType().getBaseEventType().getId()== AppConstant.BASE_EVENT_TYPE_ID_REQ_APPROVE){
			ret = EIMResource.getMessage(sess, "EIM.ACCESS.TYPE.APPROVALREQUEST");
		}else if(event.getEventType().getBaseEventType().getId()== AppConstant.BASE_EVENT_TYPE_ID_APPROVAL){
			ret = EIMResource.getMessage(sess, "EIM.ACCESS.TYPE.APPROVAL");
		}else if(event.getEventType().getBaseEventType().getId()== AppConstant.BASE_EVENT_TYPE_ID_CANCEL_REQ_APPROVE){
			ret = EIMResource.getMessage(sess, "EIM.ACCESS.TYPE.APPROVALREQUESTCANCEL");
		}else if(event.getEventType().getBaseEventType().getId()== AppConstant.BASE_EVENT_TYPE_ID_TAKE_BACK){
			ret = EIMResource.getMessage(sess, "EIM.ACCESS.TYPE.APPROVALTAKEBACK");
		}else if(event.getEventType().getBaseEventType().getId()== AppConstant.BASE_EVENT_TYPE_ID_SEND_BACK){
			ret = EIMResource.getMessage(sess, "EIM.ACCESS.TYPE.SENDBACK");
		}else if(event.getEventType().getBaseEventType().getId()== AppConstant.BASE_EVENT_TYPE_ID_PUBLIC){
			ret = EIMResource.getMessage(sess, "EIM.ACCESS.TYPE.PUBLIC");
		}else{
			ret = event.getEventType().getBaseEventType().getName();
			switch((int)event.getEventType().getBaseEventType().getId()) {

			case AppConstant.BASE_EVENT_TYPE_ID_REQ_APPROVE :
				ret = EIMResource.getMessage(sess, "EIM.ACCESS.TYPE.APPROVALREQUEST");
				break;
			case AppConstant.BASE_EVENT_TYPE_ID_APPROVAL :
				ret = EIMResource.getMessage(sess, "EIM.ACCESS.TYPE.APPROVAL");
				break;
			case AppConstant.BASE_EVENT_TYPE_ID_CANCEL_REQ_APPROVE :
				ret = EIMResource.getMessage(sess, "EIM.ACCESS.TYPE.APPROVALREQUESTCANCEL");
				break;
			case AppConstant.BASE_EVENT_TYPE_ID_TAKE_BACK :
				ret = EIMResource.getMessage(sess, "EIM.ACCESS.TYPE.APPROVALTAKEBACK");
				break;
			case AppConstant.BASE_EVENT_TYPE_ID_SEND_BACK :
				ret = EIMResource.getMessage(sess, "EIM.ACCESS.TYPE.SENDBACK");
				break;
			case AppConstant.BASE_EVENT_TYPE_ID_PUBLIC :
				ret = EIMResource.getMessage(sess, "EIM.ACCESS.TYPE.PUBLIC");
				break;
			default :

				// イベントタイプの名称を返却する（タスク管理用）
				ret = null;
				for (OtherNameDomain otherName : event.getEventType().getNameList()) {

					// セッション言語のイベントタイプ多言語名称をマッピング
					if (otherName.getLangId().equals(sess.getLangId())) {
						ret = otherName.getName();
						break;
					}
				}

				if (ret == null) {

					ret = event.getEventType().getBaseEventType().getName();
				}
				break;
			}
		}
			return ret;
		}
	}

		// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	//Parameter
	String prmObjId = request.getParameter("objId");

	//Message
	String message = null;
	Object[] paramId = {
			"objId=" + prmObjId,
			};

	// Session
	EIMSession sess = null;
	EIMUser loginUser = null;

	boolean sessPutflg = false;

try {

	//Session
	sess = EIMUtils.getSession(request);
	if (sess == null) {
		message = EIMResource.getMessage(request, "EIM.ERROR.SESSIONTIMEOUT");
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		message = EIMResource.getMessage("EIM.ERROR.SESSIONTIMEOUT");
		log.warn(AppMessageUtils.makeLogMessage(message));
		return;
	}

	loginUser = (EIMUser) sess.getAttribute("USER");

	if(!SecurityUtils.authorized(sess, ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId)), sess.getUser(), AppConstant.ACCESS_ROLE_ALWAYS_READ))
	{
		out.clear();
		message = EIMResource.getMessageValue(sess, "EIM.ERROR.LOGIC.NOACCESS");
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		message = EIMResource.getMessageValue("EIM.ERROR.LOGIC.NOACCESS");
		log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
		return;
	}

	//前処理
	EIMThreadContext.removeEIMSession();
	if(EIMThreadContext.getEIMSession() == null)
	{
		//Service、Dao呼出に必要
		EIMThreadContext.putEIMSession(sess);
		sessPutflg = true;
	}

	// TransactionContextの作成、設定
	if(EIMThreadContext.getTransactionContext() != null)
	{
		EIMThreadContext.removeTransactionContext();
	}
	TransactionContext context = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
	EIMThreadContext.putTransactionContext(context);
	context.setLangId(sess.getLangId());
	context.setDBConnection(sess.getDBConnection());
	context.setUser(ConvertUtils.toUserDomain(loginUser));

	long objId = Long.valueOf(prmObjId).longValue();


	ApplicationContext appContext = ApplicationContextLoader.getContext();
	StatusService statusService = (StatusService)appContext.getBean("statusService");
	StatusDomain statusDomain = statusService.getByObjId(objId);
	objId = statusDomain.getObject().getId();

	EventHistoryService eventHistoryService = (EventHistoryService)appContext.getBean("workflowHistoryService");

	EventHistoryDomain eventHistoryDomain = eventHistoryService.getByObjId(objId);

	WorkFlowHistoryDomain wfHistoryDomain = (WorkFlowHistoryDomain)eventHistoryDomain;

	// ステータスからオブジェクトを取得する
	// WFつきフォルダ以下のドキュメントを選択した場合、ここで取得されるオブジェクトは上位WFつきフォルダ
	List<StatusDomain> stDomainList = wfHistoryDomain.getStatusList();
	ObjectDomain objDomain = stDomainList.get(stDomainList.size() - 1).getObject();

	// ワークフロー
	WorkFlowDomain wfDomain = wfHistoryDomain.getWorkflow();
	EIMWorkFlow workFlow = wfDomain.createEIMWorkFlow();

	//ステータスタイプ一覧
	List statusTypeList = workFlow.getStatusTypeList();

	//脆弱性対応のため、アクセス権限のチェック処理を追加			
	if(SecurityUtils.authorized(sess, objDomain.createEIMObject(), loginUser, EIMAccessRole.READ) != true)			
	{			
		out.clear();		
		message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOACCESS");		
		out.println(AppMessageUtils.makeErrorTagByMessage(message));		
		message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOACCESS");		
		log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message));		
		return;		
	}			

	//try {
	// ステータスタイプ一覧を出力
	out.println("<statusTypeList");
		out.println(" workFlowId=\"" + workFlow.getId() + "\"");
		out.println(" workFlowName=\"" + StringUtils.xmlEncode(workFlow.getName()) + "\"");
		out.println(" wfFolderName=\"" + StringUtils.xmlEncode(objDomain.getName()) + "\"");
		out.println(" objId=\"" + objDomain.getId() + "\"");
	out.println(">");

	// 「キー：ステータスタイプID、値：ステータス(同ステータスタイプ中で最新のもの)」のマップ
	Map<Long, StatusDomain> statusMap = new HashMap<Long, StatusDomain>();
	for (StatusDomain stDomain : wfHistoryDomain.getStatusList()) {

		long stTypeId = stDomain.getStatusType().getId();
		if (statusMap.containsKey(stTypeId)) {

			if (stDomain.getMDate().getTime() < statusMap.get(stTypeId).getMDate().getTime()) {
				continue;
			}
		}

		// 同ドキュメント同ステータスタイプのステータスで最新の可能性がある場合、マップに追加
		statusMap.put(stTypeId , stDomain);
	}

	// 「キー：ステータスID、値：ステータスを遷移元/遷移先ステータスとするイベントログのリスト」のマップ
	Map<Long, List<EventLogDomain>> eventMap = new HashMap<Long, List<EventLogDomain>>();
	// 空のリストでマッピングを作成
	for (StatusDomain stDomain : new ArrayList<StatusDomain>(statusMap.values())) {

		long statusId = stDomain.getId();
		eventMap.put(statusId, new ArrayList<EventLogDomain>());
	}
	// 遷移元/遷移先ステータスIDのマッピングが存在する場合、それぞれリストに追加
	EventDomain lastRequestEvent = null;
	for (EventLogDomain eventLogDomain : wfHistoryDomain.getEventLogList()) {

		EventDomain eventDomain = eventLogDomain.getEvent();

		long fromStatusId = eventDomain.getFromStatus().getId();
		if (eventMap.containsKey(fromStatusId) ) {
			eventMap.get(fromStatusId).add(eventLogDomain);
		}
		// 遷移元ステータス・遷移先ステータスが同じ場合に二重に登録するのを防ぐ
		long toStatusId = eventDomain.getToStatus().getId();
		if (eventMap.containsKey(toStatusId) && fromStatusId != toStatusId) {
			eventMap.get(toStatusId).add(eventLogDomain);
		}

		// 最後の承認依頼イベントを保管する (アサイン予定を取得するかどうかを判断するため)
		if (eventDomain.getFromStatus().getStatusType().getStatusTypeKind().getId() == AppConstant.STATUS_TYPE_KIND_ID_EDITTING) {
			if (lastRequestEvent == null || lastRequestEvent.getCDate().getTime() < eventDomain.getCDate().getTime()) {
				lastRequestEvent = eventDomain;
			}
		}
	}

	// 現在ステータが編集中以外か、直前の承認依頼を実行したのがログインユーザの場合、設定済みのアサイン予定を取得する
	Map<Long, List<EIMUser>> assignmentPlanUserListMap = new HashMap<>();
	if (objDomain.getStatus().getStatusType().getStatusTypeKind().getId() != AppConstant.STATUS_TYPE_KIND_ID_EDITTING ||
			(lastRequestEvent != null && lastRequestEvent.getCUser().getId() == loginUser.getId())) {
		// ステータスタイプ毎のアサイン予定を取得しておく
		assignmentPlanUserListMap.putAll(AppWorkFlowUtil2.getAssignmentPlanUserListMap(objDomain.getId()));
	}

	// Workflow設定オブジェクトから承認依頼先デフォルト設定の有無を取得する
	EIMObject wfSettingObj = AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_WFSETTING"), String.valueOf(workFlow.getId()));
	Map<Long, List<EIMUser>> assignmentEntryUserListMap = new HashMap<>();
	if (wfSettingObj != null) {
		EIMAttribute wfSetting = wfSettingObj.getAttribute(EIMConfig.get("ATTR_NAME_WFSETTING_SETTING_FLG"));

		// 承認依頼先デフォルト設定を行う場合、アサイン予定の設定がないステータスタイプ毎のアサインエントリをユーザに展開して取得しておく

		if (wfSetting != null && wfSetting.getInt() == AppConstant.FLAG_ON) {
			List<Long> statusTypeIds = new ArrayList<>();
			// 現在ステータスより先のステータスタイプを対象とする
			int currentStep = objDomain.getStatus().getStatusType().getSeq();
			for (int i = currentStep; i < statusTypeList.size(); i ++) {
				EIMStatusType statusType = (EIMStatusType)statusTypeList.get(i);
				if (!assignmentPlanUserListMap.containsKey((long)statusType.getId())) {
					statusTypeIds.add((long)statusType.getId());
				}
			}
			// ステータスタイプ毎のアサインエントリをユーザに展開して取得する
			Map<Long, List<EIMUser>> assinmentEntryUserListMap = AppWorkFlowUtil2.getAssinmentEntryUserListMapByStatusTypeIds(statusTypeIds, objDomain.getId());
			if ( assinmentEntryUserListMap != null) {
				assignmentEntryUserListMap.putAll( assinmentEntryUserListMap );
			}
		}
	}


	ApplicationContext contxt = ApplicationContextLoader.getContext();
	DataUtil util = new DataUtil(contxt);
	BossGroupUtils bossGroups = null;
	BossRoleUtils bossRoles = null;
	Map<Long, List<EIMGroup>> groupListMap = new HashMap<Long, List<EIMGroup>>();
	Map<Long, List<EIMRole>> roleListMap =  new HashMap<Long, List<EIMRole>>();

	// ステータスタイプ毎の情報を出力
	for (int i = 0; i < statusTypeList.size(); i++) {
		EIMStatusType statusType = (EIMStatusType)statusTypeList.get(i);

		out.println("<statusType");
		out.println(" statusTypeId=\"" + statusType.getId() + "\"");
		out.println(" statusTypeName=\"" + StringUtils.xmlEncode(statusType.getName()) + "\"");
		out.println(" statusKindId=\"" + statusType.getKind() + "\"");
		out.println(" step=\"" + statusType.getStep() + "\"");
		out.println(" currentStatus=\"" + ((statusType.getId() == objDomain.getStatus().getStatusType().getId())? "true":"false") + "\"");

		long statusTypeId = statusType.getId();

		//スキップ承認が可能なステータスか返却
		boolean canSkip = false;

			for (EventTypeDomain eventTypeDomain : wfDomain.getEventTypeList()) {
				//ベースイベントタイプが承認依頼または承認の場合のみ取得
				if(eventTypeDomain.getBaseEventType().getId() == AppConstant.BASE_EVENT_TYPE_ID_REQ_APPROVE
					|| eventTypeDomain.getBaseEventType().getId() == AppConstant.BASE_EVENT_TYPE_ID_APPROVAL
				){
					//遷移元ステータスと遷移先ステータスの並び順の差が1より大きい場合、スキップ承認フラグをtrueにする
					int skipCount = eventTypeDomain.getToStatusType().getSeq() - eventTypeDomain.getFromStatusType().getSeq();
					if(skipCount > 1){
						for(int j = 0; j < skipCount-1; j++){
							if(i == eventTypeDomain.getFromStatusType().getSeq()+j
								&& i < statusTypeList.size()-2 //ステータスが公開処理中または公開済の場合はスキップ承認フラグはfalseのまま
							){
								canSkip = true;
							}
						}
					}

				}

			}
		out.println(" canSkip=\"" + canSkip + "\"");

		// スキップステータスタイプIDとステータスタイプIDを比較した結果を返却
		boolean skip = false;

		// ドキュメント属性スキップステータスタイプIDを取得
		List<AttributeDomain> attrList = objDomain.getAttrList();
		// 過去自身が承認依頼したイベントが存在しない かつ 現在ステータスが「編集中」の場合、スキップステータスタイプIDを取得しない
		if((lastRequestEvent == null || lastRequestEvent.getCUser().getId() != loginUser.getId()) &&
			objDomain.getStatus().getStatusType().getStatusTypeKind().getId() == AppConstant.STATUS_TYPE_KIND_ID_EDITTING ){
			skip = false;
		}else{
			for (int j= 0; j < attrList.size(); j++) {
				if(attrList.get(j).getAttrType().getDefName().equals("スキップステータスタイプID")){
					Object[] skipStatusTypeIdAttr = attrList.get(j).getValues();
					for(Object skipStatusTypeId : skipStatusTypeIdAttr){
						if(statusType.getId() == (long)skipStatusTypeId){
							skip = true;
						}
					}
				}
			}
		}

		out.println(" skip=\"" + skip + "\"");

		out.println(">");

		// 「全員承認」ステータスの場合"true"
		String isMust = "false";
		// 「承認」ステータスフラグ
		boolean isApproveStatus = false;
		if(statusType.getKind() == AppConstant.STATUS_TYPE_KIND_ID_APPROVE
			|| statusType.getKind() == AppConstant.STATUS_TYPE_KIND_ID_NOT_STARTED
			|| statusType.getKind() == AppConstant.STATUS_TYPE_KIND_ID_ONGOING
			|| statusType.getKind() == AppConstant.STATUS_TYPE_KIND_ID_APPROVING) {

			isApproveStatus = true;

			// 遷移元ステータスタイプが一致しガード条件が「最後の承認者でない」のものがある場合、全員承認
			for (EventTypeDomain eventType : wfDomain.getEventTypeList()) {

				if (eventType.getGuardCondition().getId() == AppConstant.GUARD_COND_ID_FINAL_APPROVED_IS_NOT
					&& eventType.getFromStatusType().getId() == statusType.getId()) {

					isMust = "true";
					break;
				}
			}
		}

		if (statusType.getStep() <= objDomain.getStatus().getStatusType().getSeq() && statusMap.get(statusTypeId) != null)
		{
		// 現在のステータスよりも後の段階でない場合、情報を出力する
		// また、公開処理を行わず「公開処理中」ステータスへ遷移しなかった場合はステータスが存在しないのでスキップ

			// 該当ステータスタイプのステータス・ステータスのアサイン先・ステータスが遷移元or遷移先のイベントログ
			StatusDomain status = statusMap.get(statusTypeId);
			List<AssignDomain> assignList = status.getAssignList();
			long statusId = status.getId();
			List<EventLogDomain> eventLogList = eventMap.get(statusId);

			/* --- アサイン先情報を出力 --- */
			out.println("<userList>");
			if (assignList.size() > 0 || eventLogList.size() > 0) {

				// アサイン先情報
				for (AssignDomain stAssign : assignList) {

					String isApproved = (stAssign.getEvent() != null && isApproveStatus)?"true":"false";
					out.println("<user");
						out.println(" id=\"" + stAssign.getOwner().getId() + "\"");
						out.println(" approve=\"" + isApproved + "\"");
						out.println(" entryType=\"" + stAssign.getType().getSymbol() + "\"");
						out.println(" must=\"" + isMust + "\"");
						out.println(" name=\"" + StringUtils.xmlEncode(stAssign.getOwner().getName()) + "\"");
					out.println("/>");
				}
				// 実行者情報
				for (EventLogDomain eventLog : eventLogList) {

					EventDomain execEvent = eventLog.getEvent();
					// 遷移元ステータスが一致しない場合、出力しない
					if (execEvent.getFromStatus().getId() != status.getId()) {
						continue;
					}
					// イベントが「承認依頼取消」の場合、出力しない
					if (execEvent.getEventType().getBaseEventType().getId() == AppConstant.BASE_EVENT_TYPE_ID_CANCEL_REQ_APPROVE) {
						continue;
					}
					// ステータスのアサイン先に含まれる場合、出力しない
					boolean isAssign = false;
					for (AssignDomain stAssign : assignList) {
						if (stAssign.getOwner().getId() == execEvent.getCUser().getId()) {
							isAssign = true;
							break;
						}
					}
					if (isAssign) {continue;}

					out.println("<user");
						out.println(" approve=\"false\" entryType=\"user\" must=\"false\"");
						out.println(" name=\"" + StringUtils.xmlEncode(execEvent.getCUser().createEIMUser().getName()) + "\"");
					out.println("/>");
				}
			}
			out.println("</userList>");

			/* --- イベント実行情報を出力 --- */
			out.println("<eventList>");
			if (eventLogList.size() > 0) {

				// ベースイベントタイプ毎、遷移先ステータス別にまとめて出力する
				long baseEvtTypeId = eventLogList.get(0).getEvent().getEventType().getBaseEventType().getId();
				long fromStatusId = eventLogList.get(0).getEvent().getFromStatus().getId();
				out.println("<event baseEvtType=\"" + util.getBaseEventTypeName(sess, eventLogList.get(0).getEvent()) + "\">");
				for (EventLogDomain eventLog: eventLogList) {

					BaseEventTypeDomain baseEventType = eventLog.getEvent().getEventType().getBaseEventType();
					EventDomain event = eventLog.getEvent();
					if (baseEvtTypeId != baseEventType.getId() || fromStatusId != event.getFromStatus().getId()) {

						out.println("</event>");
						out.println("<event baseEvtType=\"" + util.getBaseEventTypeName(sess, event) + "\">");
						baseEvtTypeId = baseEventType.getId();
						fromStatusId = event.getFromStatus().getId();
					}

					// イベント実行者情報
					out.println("<user");
						out.println(" date=\"" + DateUtils.getDBTzToCLTzDate(sess, event.getCDate(), "EIM.FORMAT.DATETIME") + "\"");
						out.println(" userName=\"" + StringUtils.xmlEncode(eventLog.getEvent().getCUser().createEIMUser().getName()) + "\"");
						if (eventLog.getAssign() != null) {
							String belongStr = util.getBelongString(eventLog.getAssign());
							if (belongStr != null) {
								out.println(" belong=\"" + StringUtils.xmlEncode(belongStr) + "\"");
							}
						}
						String noCommentEvent = (baseEvtTypeId == AppConstant.BASE_EVENT_TYPE_ID_CANCEL_REQ_APPROVE
															|| baseEvtTypeId == AppConstant.BASE_EVENT_TYPE_ID_PUBLIC)?"true":"false";
						String comment = (event.getAttribute(EIMConfig.getValue("EVENT_ATTR_NAME_COMMENT")) != null)?(String)event.getAttribute(EIMConfig.getValue("EVENT_ATTR_NAME_COMMENT")).getValues()[0]:"";
						if (comment == "") {
							// タスク管理アプリのWF時のコメント
							comment = (event.getAttribute(EIMConfig.getValue("ATTRIBUTE_TYPE_NAME_TASK_COMMENT")) != null)?(String)event.getAttribute(EIMConfig.getValue("ATTRIBUTE_TYPE_NAME_TASK_COMMENT")).getValues()[0]:"";
						}
						out.println(" comment=\"" + StringUtils.xmlEncode(comment) + "\" noCommentEvent=\"" + noCommentEvent + "\"");
						String noPublicCommentEvent = (baseEvtTypeId == AppConstant.BASE_EVENT_TYPE_ID_REQ_APPROVE || baseEvtTypeId == AppConstant.BASE_EVENT_TYPE_ID_APPROVAL)?"false":"true";
						String publicCommentLog = (event.getAttribute(EIMConfig.getValue("ATTR_NAME_PUBLIC_COMMENT_LOG")) != null)?(String)event.getAttribute(EIMConfig.getValue("ATTR_NAME_PUBLIC_COMMENT_LOG")).getValues()[0]:"";
						out.println(" publicCommentLog=\"" + StringUtils.xmlEncode(publicCommentLog) + "\" noPublicCommentEvent=\"" + noPublicCommentEvent + "\"");
					out.println("/>");
				}
				out.println("</event>");
			}
			out.println("</eventList>");
		} else {
			// 現在のステータスよりも後の段階の情報を出力する
			out.println("<userList>");

			List<EIMUser> userList = null;
			List<EIMUser> assignmentPlanUserList = assignmentPlanUserListMap.get((long)statusType.getId());
			if (assignmentPlanUserList != null && assignmentPlanUserList.size() > 0) {
				// アサイン予定が設定されている場合、そこから承認依頼先情報を出力
				userList = assignmentPlanUserList;

				// 承認依頼先情報
				for (EIMUser user : userList) {
					// 無効ユーザチェックを行う
					if(user.getDisable() != 0) {
						continue;
					}

					//承認権限のチェックを行う
					if (!SecurityUtils.authorized(sess, objDomain.createEIMObject(), user, EIMAccessRole.STATUS_UP)) {
						continue;
					}

					boolean isApproved = false;
					String type = BelongType.USER.getSymbol();
					out.println("<user");
						out.println(" id=\"" + user.getId() + "\"");
						out.println(" approve=\"" + isApproved + "\"");
						out.println(" entryType=\"" + type + "\"");
						out.println(" must=\"" + isMust + "\"");
						out.println(" name=\"" + StringUtils.xmlEncode(user.getName()) + "\"");
					out.println("/>");
				}
			} else {
				// アサイン予定がない
				// ★TODO★
				// 呼び元画面が承認画面、もしくは履歴画面の場合（※依頼画面以外の場合） ★TODO★
				// 再現手順：承認依頼時に未来のステータスを空にして依頼→承認画面開く・・・アサインエントリーがでちゃう
				/**********
				if ( 呼び元が依頼画面以外の場合 ) {
					continue;
				}
				*********/

				// アサイン予定が設定されていない場合、アサインエントリから承認依頼先情報を出力
				List<EIMUser> assignmentEntryUserList = assignmentEntryUserListMap.get((long)statusType.getId());
				userList = assignmentEntryUserList;

				if (userList != null && userList.size() > 0) {
					// デフォルト表示時、上長で絞り込む場合は上長チェックを行う
					boolean isBossCheck = AppWorkFlowUtil.checkDefaultBossOnly(wfSettingObj , statusType.getId());
					if( isBossCheck ) {
						groupListMap = AppWorkFlowUtil2.getGroupListMapUserIds(userList, groupListMap);
						roleListMap = AppWorkFlowUtil2.getRoleListMapUserIds(userList, roleListMap);
					}

					// 承認依頼先情報
					for (EIMUser user : userList) {
						// 無効ユーザチェックを行う
						if(user.getDisable() != 0) {
							continue;
						}
						//承認権限のチェックを行う
						if (!SecurityUtils.authorized(sess, objDomain.createEIMObject(), user, EIMAccessRole.STATUS_UP)) {
							continue;
						}

						// デフォルト表示時、上長で絞り込む場合は上長チェックを行う
						if( isBossCheck ) {
							// 承認依頼先ユーザの所属グループが自身の所属グループツリーと一致しているか
							if (bossGroups == null) {
								bossGroups = new BossGroupUtils(sess, loginUser);
							}
							List<EIMGroup> groupList = groupListMap.get((long)user.getId());
							if(!bossGroups.isInGroup(groupList)) {
								// 上長ではない
								continue;
							}

							// 承認依頼先ユーザのロールが自身のロールの上位に該当するか
							if (bossRoles == null) {
								bossRoles = new BossRoleUtils(sess, loginUser);
							}
							List<EIMRole> roleList = roleListMap.get((long)user.getId());
							if(!bossRoles.isInRole(roleList)) {
								// 上長ではない
								continue;
							}
						}

						boolean isApproved = false;
						String type = BelongType.USER.getSymbol();
						out.println("<user");
							out.println(" id=\"" + user.getId() + "\"");
							out.println(" approve=\"" + isApproved + "\"");
							out.println(" entryType=\"" + type + "\"");
							out.println(" must=\"" + isMust + "\"");
							out.println(" name=\"" + StringUtils.xmlEncode(user.getName()) + "\"");
						out.println("/>");
					}
				}
			}
			out.println("</userList>");
		}
		out.println("</statusType>");
	}

	out.println("</statusTypeList>");

	// イベントタイプリストを返却
	out.println("<eventTypeList>");
		for (EventTypeDomain eventTypeDomain : wfDomain.getEventTypeList()) {
			out.println("<eventType");
				out.println(" id=\"" + eventTypeDomain.getId() + "\"");
				out.println(" seq=\"" + eventTypeDomain.getSeq() + "\"");
				out.println(" fromStatusTypeSeq=\"" + eventTypeDomain.getFromStatusType().getSeq() + "\"");
				out.println(" toStatusTypeSeq=\"" + eventTypeDomain.getToStatusType().getSeq() + "\"");
				out.println(" baseEventTypeId=\"" + eventTypeDomain.getBaseEventType().getId() + "\"");
				out.println(" guardConditionId=\"" + eventTypeDomain.getGuardCondition().getId() + "\"");
			out.println(">");
			out.println("</eventType>");
		}
	out.println("</eventTypeList>");

%>
	<eventList>
<%
	/* --- イベント履歴の出力 --- */
	for (EventLogDomain eventLog: wfHistoryDomain.getEventLogList()) {

		EventDomain event = eventLog.getEvent();
		long baseEvtTypeId = event.getEventType().getBaseEventType().getId();

		out.println("<event");
			out.println(" date=\"" + DateUtils.getDBTzToCLTzDate(sess, event.getCDate(), "EIM.FORMAT.DATETIME") + "\"");
			out.println(" userName=\"" + StringUtils.xmlEncode(eventLog.getEvent().getCUser().createEIMUser().getName()) + "\"");
			if (eventLog.getAssign() != null) {
				String belongStr = util.getBelongString(eventLog.getAssign());
				if (belongStr != null) {
					out.println(" belong=\"" + StringUtils.xmlEncode(belongStr) + "\"");
				}
			}
			out.println(" baseEvtType=\"" + util.getBaseEventTypeName(sess, event) + "\"");
			out.println(" baseEvtTypeId=\"" + event.getEventType().getBaseEventType().getId() + "\"");
			out.println(" toStatus=\"" + StringUtils.xmlEncode(event.getToStatus().getStatusType().createEIMStatusType().getName()) + "\"");
			out.println(" toStatusId=\"" + event.getToStatus().getStatusType().createEIMStatusType().getId() + "\"");
			out.println(" toStatusStep=\"" + event.getToStatus().getStatusType().createEIMStatusType().getStep() + "\"");
			out.println(" fromStatus=\"" + StringUtils.xmlEncode(event.getFromStatus().getStatusType().createEIMStatusType().getName()) + "\"");
			out.println(" fromStatusId=\"" + event.getFromStatus().getStatusType().createEIMStatusType().getId() + "\"");
			String noCommentEvent = (baseEvtTypeId == AppConstant.BASE_EVENT_TYPE_ID_CANCEL_REQ_APPROVE
												|| baseEvtTypeId == AppConstant.BASE_EVENT_TYPE_ID_PUBLIC)?"true":"false";
			String comment = (event.getAttribute(EIMConfig.getValue("EVENT_ATTR_NAME_COMMENT")) != null)?(String)event.getAttribute(EIMConfig.getValue("EVENT_ATTR_NAME_COMMENT")).getValues()[0]:"";
			if (comment == "") {
				// タスク管理アプリのWF時のコメント
				comment = (event.getAttribute(EIMConfig.getValue("ATTRIBUTE_TYPE_NAME_TASK_COMMENT")) != null)?(String)event.getAttribute(EIMConfig.getValue("ATTRIBUTE_TYPE_NAME_TASK_COMMENT")).getValues()[0]:"";
			}
			out.println(" comment=\"" + StringUtils.xmlEncode(comment) + "\" noCommentEvent=\"" + noCommentEvent + "\"");
			String noPublicCommentEvent = (baseEvtTypeId == AppConstant.BASE_EVENT_TYPE_ID_REQ_APPROVE || baseEvtTypeId == AppConstant.BASE_EVENT_TYPE_ID_APPROVAL)?"false":"true";
			String publicCommentLog = (event.getAttribute(EIMConfig.getValue("ATTR_NAME_PUBLIC_COMMENT_LOG")) != null)?(String)event.getAttribute(EIMConfig.getValue("ATTR_NAME_PUBLIC_COMMENT_LOG")).getValues()[0]:"";
			out.println(" publicCommentLog=\"" + StringUtils.xmlEncode(publicCommentLog) + "\" noPublicCommentEvent=\"" + noPublicCommentEvent + "\"");
		out.println("/>");
	}
%>
	</eventList>
<%
	}
	catch(EIMAppException e) {
		out.clear();
		message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
	}
	catch(EIMException e) {
		out.clear();
		message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
	}
	catch(Exception e)
	{
		out.clear();
		message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
	}
	finally {
		try {
			//Remove Session from Thread Local Table
			if(sessPutflg == true){
				EIMThreadContext.removeEIMSession();
				sessPutflg = false;
			}
			if (sess != null) {
				sess.close();
			}
			if(jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext.getTransactionContext() != null)
			{
				jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext.removeTransactionContext();
			}
		} catch (Exception se) {
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
	}
%>
</result>