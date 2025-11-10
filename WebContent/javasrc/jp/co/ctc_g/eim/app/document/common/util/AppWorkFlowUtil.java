package jp.co.ctc_g.eim.app.document.common.util;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Types;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.springframework.context.ApplicationContext;

import app.document.approve.ApproveCommonUtil;
import common.util.AppConstant;
import common.util.AppObjectConditionHelper;
import common.util.AppObjectTypeUtil;
import common.util.AppObjectUtil;
import common.util.PublishAddonUtils;
import eim.bo.EIMAccessEntryType;
import eim.bo.EIMAttribute;
import eim.bo.EIMComp;
import eim.bo.EIMException;
import eim.bo.EIMFile;
import eim.bo.EIMFormat;
import eim.bo.EIMGroup;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMRole;
import eim.bo.EIMStatus;
import eim.bo.EIMStatusType;
import eim.bo.EIMUser;
import eim.bo.EIMWorkFlow;
import eim.net.EIMSession;
import eim.util.CompUtils;
import eim.util.EIMConfig;
import eim.util.FileUtils;
import eim.util.GroupUtils;
import eim.util.ObjectUtils;
import eim.util.RoleUtils;
import eim.util.TypeConvertUtils;
import eim.util.UserUtils;
import eim.util.WorkFlowUtils;
import jp.co.ctc_g.eim.app.document.business.domain.ApprovalReqInfoDomain;
import jp.co.ctc_g.eim.app.document.business.domain.PDFSettingDomain;
import jp.co.ctc_g.eim.framework.business.dao.AssignDao;
import jp.co.ctc_g.eim.framework.business.dao.AssignEntryDao;
import jp.co.ctc_g.eim.framework.business.dao.EventHistoryDao;
import jp.co.ctc_g.eim.framework.business.dao.StatusDao;
import jp.co.ctc_g.eim.framework.business.domain.AssignDomain;
import jp.co.ctc_g.eim.framework.business.domain.AssignEntryDomain;
import jp.co.ctc_g.eim.framework.business.domain.BaseEventTypeExecDomain;
import jp.co.ctc_g.eim.framework.business.domain.BelongDomain;
import jp.co.ctc_g.eim.framework.business.domain.BelongType;
import jp.co.ctc_g.eim.framework.business.domain.EventHistoryDomain;
import jp.co.ctc_g.eim.framework.business.domain.EventLogDomain;
import jp.co.ctc_g.eim.framework.business.domain.ForcastStatusTypeDomain;
import jp.co.ctc_g.eim.framework.business.domain.GuardConditionDomain;
import jp.co.ctc_g.eim.framework.business.domain.NoticeMailDomain;
import jp.co.ctc_g.eim.framework.business.domain.UserDefGroupDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.EventDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.EventTypeDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.StatusDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.StatusTypeDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.WorkFlowDomain;
import jp.co.ctc_g.eim.framework.business.service.EventExecService;
import jp.co.ctc_g.eim.framework.business.service.EventHistoryService;
import jp.co.ctc_g.eim.framework.business.service.UserDefGroupConfService;
import jp.co.ctc_g.eim.framework.business.service.UserDefGroupPlugIn;
import jp.co.ctc_g.eim.framework.business.service.WorkFlowDefService;
import jp.co.ctc_g.eim.framework.common.exception.EIMAppException;
import jp.co.ctc_g.eim.framework.common.util.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.AssignmentCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AssignmentDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ComplexDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.GroupDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.RoleDomain;
import jp.co.ctc_g.eim.framework2.business.service.AssignmentService;
import jp.co.ctc_g.eim.framework2.business.service.ComplexService;
import jp.co.ctc_g.eim.framework2.business.service.GroupService;
import jp.co.ctc_g.eim.framework2.business.service.RoleService;
import jp.co.ctc_g.eim.framework2.common.enumeration.EntryTypeEnum;
import jp.co.ctc_g.eim.framework2.integration.dao.plugin.DatabasePlugInLoader;

public class AppWorkFlowUtil {

	/**
	 * 承認依頼中ステータスタイプの通過条件を取得する。
	 *
	 * @param statusType 識別対象のEIMStatusType
	 * @return
	 *  全員承認　⇒「2」
	 *  一人承認　⇒「3」
	 *  指定のステータスタイプは承認依頼中ステータスタイプではない場合、「0」を取得する
	 * @throws Exception
	 */
	public static int getDocThrough(EIMStatusType statusType)  throws Exception {
		EIMSession sess = EIMThreadContext.getEIMSession();
		if(statusType == null) return AppConstant.THROUGH_APPROVE_NONE;
		if(statusType.getKind() != AppConstant.STATUS_TYPE_KIND_ID_APPROVE) return AppConstant.THROUGH_APPROVE_NONE;

		//「承認依頼中」ステータスタイプの場合のみ「全員承認」か「一人承認」を判定する必要が有るので以下で判定する
		EIMWorkFlow workflow = WorkFlowUtils.getWorkFlowByStatusType(sess, statusType);

		WorkFlowDefService workFlowDefService = (WorkFlowDefService)ApplicationContextLoader.getContext().getBean("workFlowDefService");
		WorkFlowDomain workFlowDefDomain = workFlowDefService.getDefById(workflow.getId());

		int result = getDocThrough(workFlowDefDomain, statusType);
		return result;
	}

	/**
	 * 承認依頼中ステータスタイプの通過条件を取得する。<br>
	 * ※引数で与えられた情報を元にするためDBアクセスは行いません。
	 * @param workFlowDefDomain 識別対象のワークフロー定義
	 * @param statusType 識別対象のEIMStatusType
	 * @return
	 *  全員承認　⇒「2」
	 *  一人承認　⇒「3」
	 *  指定のステータスタイプは承認依頼中ステータスタイプではない場合、「0」を取得する
	 * @throws Exception
	 */
	public static int getDocThrough(WorkFlowDomain workFlowDefDomain, EIMStatusType statusType)  throws Exception {
		if(statusType == null) return AppConstant.THROUGH_APPROVE_NONE;
		long statusTypeId = statusType.getId();
		if(statusType.getKind() != AppConstant.STATUS_TYPE_KIND_ID_APPROVE) return AppConstant.THROUGH_APPROVE_NONE;

		// 自己遷移するイベントの存在を確認する
		List<EventTypeDomain> eventTypeList = workFlowDefDomain.getEventTypeList();
		for(EventTypeDomain eventTypeDomain :  eventTypeList){
			StatusTypeDomain fromStatusTypeDomain = eventTypeDomain.getFromStatusType();
			long fromStatusTypeId =  fromStatusTypeDomain.getId();
			if(statusTypeId == fromStatusTypeId){
				GuardConditionDomain gcDomain = eventTypeDomain.getGuardCondition();
				//ガード条件「最後の承認者ではない」
				if(gcDomain.getId() == AppConstant.GUARD_COND_ID_FINAL_APPROVED_IS_NOT){
					//FromステータスタイプIDとToステータスタイプIDが同じイベントタイプ
					if(eventTypeDomain.getFromStatusType().getId() == eventTypeDomain.getToStatusType().getId()){
						//全員承認
						return AppConstant.THROUGH_APPROVE_ALL;
					}
				}
			}
		}
		//該当しない場合は一人承認
		return AppConstant.THROUGH_APPROVE_ONE;
	}

	/**
	 * 指定したステータスタイプのアサイン先エントリーをユーザ単位で取得します。
	 * グループ・ロール・複合グループ・ユーザ定義グループはユーザに展開されて
	 * 重複ユーザが削除されます。
	 *
	 * @param obj EIMObject
	 * @param statusType EIMStatusType
	 * @return List<EIMUser>
	 */
	public static List<EIMUser> getAssinEntryUserByStatusType(EIMObject obj, EIMStatusType statusType) throws Exception
	{
		EIMSession sess = EIMThreadContext.getEIMSession();

		//アサイン先エントリーDAO
		AssignEntryDao assignEntryDao = (AssignEntryDao)ApplicationContextLoader.getContext().getBean("assignEntryDao");

		ObjectDomain objectDomain = new ObjectDomain(obj);

		List userList = new ArrayList();
		List groupList = new ArrayList();
		List roleList = new ArrayList();
		List compList = new ArrayList();
		List userGroupList = new ArrayList();

		AssignEntryDomain assignEntryDomain = new AssignEntryDomain();
		assignEntryDomain.setSttid(statusType.getId());
		List<BelongDomain> belongDomainList = assignEntryDao.getList(assignEntryDomain);

		for (int i = 0 ; i < belongDomainList.size() ; i++)
		{
			BelongDomain belongDomain = belongDomainList.get(i);

			//ユーザ
			if(belongDomain.getBelongType() == BelongType.USER)
			{
				EIMUser aprUser = UserUtils.getUserById(sess, belongDomain.getBelonging().getId());
				userList.add(aprUser);
			}
			//ロール
			else if(belongDomain.getBelongType() == BelongType.ROLE)
			{
				EIMRole role = RoleUtils.getRoleById(sess, belongDomain.getBelonging().getId());
				userList.addAll(RoleUtils.getUserList(sess, role));
			}
			//複合グループ
			else if(belongDomain.getBelongType() == BelongType.COMPGROUP)
			{
				EIMComp comp = CompUtils.getCompById(sess, belongDomain.getBelonging().getId());
				userList.addAll(CompUtils.getUserList(sess, comp));
			}
			//グループ
			else if(belongDomain.getBelongType() == BelongType.GROUP)
			{
				EIMGroup group = GroupUtils.getGroupById(sess, belongDomain.getBelonging().getId());
				userList.addAll(GroupUtils.getUserListRecurrently(sess, group));
			}
			//ユーザ定義グループ
			else if(belongDomain.getBelongType() == BelongType.USERDEFGROUP)
			{
				//ユーザ定義グループのサービス
				UserDefGroupConfService uds = (UserDefGroupConfService)ApplicationContextLoader.getContext().getBean("UserDefGroupConfService");
				UserDefGroupDomain userDefGroupDomain = uds.getUserDefGroup(belongDomain.getBelonging().getId());
				//該当するユーザ定義グループを取得
				String key = userDefGroupDomain.getKey();
				UserDefGroupPlugIn userDefGroup = (UserDefGroupPlugIn)ApplicationContextLoader.getContext().getBean(key);
				List<UserDomain> userDomainList = userDefGroup.getUserListByObject(objectDomain);

				for(UserDomain ud : userDomainList)
				{
					EIMUser eimUser = ud.createEIMUser();
					userList.add(eimUser);
				}

			}
		}

		//グループ等のユーザが含まれる可能性があるため
		//重複ユーザは削除する
		int i = 0;
		Set userSet = new HashSet();
		while (i < userList.size())
		{
			EIMUser nUser = (EIMUser)userList.get(i);

			if (userSet.add(""+nUser.getId()))
				i++;
			else
				userList.remove(i);
		}

		//ユーザリストをユーザ名称でソート
		userList = AppObjectUtil.getStrSortedList(userList, "getName", true);

		return userList;
	}

	/**
	 * 指定したオブジェクトとベースイベントタイプで次に遷移するステータスタイプを取得します。
	 *
	 * @param object EIMObject
	 * @param baseEventTypeId ベースイベントタイプID
	 * @return EIMStatusType
	 */
	public static EIMStatusType getNextStatusType(EIMObject object, long baseEventTypeId) throws Exception
	{
		//イベント実行サービス(ST遷移予測)
		EventExecService eventExecService = (EventExecService)ApplicationContextLoader.getContext().getBean("eventExecService");

		ForcastStatusTypeDomain forcastStatusType = new ForcastStatusTypeDomain();
		StatusDomain statusDomain = new StatusDomain(object.getStatus());
		forcastStatusType.setObject(new ObjectDomain(object));
		forcastStatusType.getObject().setStatus(statusDomain);
		forcastStatusType.getBaseEventType().setId(baseEventTypeId);
		Map<String,Object> map = new HashMap<String,Object>();

		if(baseEventTypeId == AppConstant.BASE_EVENT_TYPE_ID_REQ_APPROVE){
			map.put(AppConstant.PARAM_KEY_REFER_TO_APPROVAL,AppConstant.PARAM_VALUE_REFER_TO_APPROVAL);
		}else if(baseEventTypeId == AppConstant.BASE_EVENT_TYPE_ID_TAKE_BACK){
			map.put(AppConstant.PARAM_KEY_REFER_TO_APPROVAL,AppConstant.PARAM_VALUE_REFER_TO_APPROVAL);
		}else{
			map.put(AppConstant.PARAM_KEY_REFER_TO_APPROVAL,AppConstant.PARAM_VALUE_REFER_TO_CANCEL_APPROVAL);
		}

		forcastStatusType.setParamMap(map);

		ForcastStatusTypeDomain resultForcastStatusType = eventExecService.forcastNextStatusType(forcastStatusType);

		EventTypeDomain evtTypeDomain = resultForcastStatusType.getEventType();
		StatusTypeDomain toStatusTypeDomain = evtTypeDomain.getToStatusType();
		//forcastStatusTypeId = toStatusTypeDomain.getId();

		return(toStatusTypeDomain.createEIMStatusType());
	}

	/**
	 * 指定したオブジェクトで次に遷移するステータスタイプを取得します。
	 *
	 *
	 *
	 * @param object EIMObject
	 * @return EIMStatusType
	 */
	public static EIMStatusType getNextStatusTypeDoc(EIMObject object, EIMWorkFlow workflow) throws Exception
	{
		EIMStatusType currentStatusType = object.getStatus().getType();		// 現在のステータスタイプ

		List<EIMStatusType> statusTypeList = workflow.getStatusTypeList();					// WFから取得したステータスタイプリスト
		statusTypeList = AppObjectUtil.getIntSortedList(statusTypeList, "getStep", true);	// step順に並べる

		int currentSeq = currentStatusType.getStep();

		EIMStatusType nextStatusType = null;

		// 現在ステータスが最終承認の場合、公開済ステータスを返却する
		if ( currentSeq == statusTypeList.size()-2 ) {
			nextStatusType = statusTypeList.get( statusTypeList.size() -1 );
		} else {
			// スキップステータスタイプIDを取得しMapに詰める
			Map<Long, Long> skipStatusTypeMap = new HashMap<Long, Long>();
			EIMAttribute skipStatusTypeIdAttr = object.getAttribute("スキップステータスタイプID");
			if(skipStatusTypeIdAttr != null){
				long[] skipStatusTypeIds = TypeConvertUtils.convertToLongArray(skipStatusTypeIdAttr.getInts());
				for (long skipId : skipStatusTypeIds) {
					skipStatusTypeMap.put(skipId, skipId);
				}
			}

			// スキップを考慮し、遷移先のステータスタイプを返却する
			for (int i=0; i < statusTypeList.size(); i++){
				EIMStatusType statusType = (EIMStatusType) statusTypeList.get(i);
				if ( statusType.getStep() > currentSeq) {
					// カレントステータスタイプ以降で判定する
					if ( !skipStatusTypeMap.containsKey(Long.valueOf(statusType.getId())) ) {
						// step順にfor文を回し、はじめて取得できたステータスタイプがnextになる
						// 但し自身より先のステータスが全てスキップの場合、はじめて取得できるステータスタイプは公開処理中となる、その場合は公開済みを返却する
						if ( statusType.getKind() == AppConstant.STATUS_TYPE_KIND_ID_PROCESSING_PUBLIC) {
							// 公開処理中の場合、公開済を返却
							nextStatusType =  statusTypeList.get( statusTypeList.size() -1 ); // 公開済ステータスを返却
						} else {
							nextStatusType = statusType;
							break;
						}

					}
				}
			}
		}

		return nextStatusType;
	}

	/**
	 * 現在のステータスに遷移するために実行された承認イベント、または承認依頼イベントを取得する。
	 * 一つ前のステータスタイプが全員承認の場合は部分承認を含んだ全てのイベントをリストで返却する。
	 * 一人承認の場合は遷移するために実行された１つのイベントをリストで返却する。
	 *
	 * @param object EIMObject
	 * @return List<EventDomain>
	 */
	public static List<EventDomain> getOneAheadApprovalRequestEvent(EIMObject object) throws Exception
	{
		EventHistoryDao eventHistoryDao = (EventHistoryDao)ApplicationContextLoader.getContext().getBean("eventHistoryDaoWithoutAttribute");
		StatusDao statusDao = (StatusDao)ApplicationContextLoader.getContext().getBean("statusDao");
		List<EventLogDomain> eventLogList = eventHistoryDao.getByObjId(object.getId()).getEventLogList();

		//現在のステータス
		StatusDomain currentStatus = statusDao.getById(object.getStatus().getId());
		int currentSequence = currentStatus.getStatusType().getSeq();
		if (currentSequence == 1) {
			// 最初に作成されたシーケンスの場合、空を返す
			return new ArrayList<EventDomain>();
		}

		// Workflow定義を取得する
		EIMSession sess = EIMThreadContext.getEIMSession();
		EIMWorkFlow workflow = WorkFlowUtils.getWorkFlowByStatusType(sess, currentStatus.getStatusType().createEIMStatusType());

		WorkFlowDefService workFlowDefService = (WorkFlowDefService)ApplicationContextLoader.getContext().getBean("workFlowDefService");
		WorkFlowDomain workFlowDefDomain = workFlowDefService.getDefById(workflow.getId());

		List<EventDomain> resultEventList = getOneAheadApprovalRequestEvent(eventLogList, workFlowDefDomain, currentSequence);
		return resultEventList;
	}

	/**
	 * 現在のステータスに遷移するために実行された承認イベント、または承認依頼イベントを取得する。
	 * 一つ前のステータスタイプが全員承認の場合は部分承認を含んだ全てのイベントをリストで返却する。
	 * 一人承認の場合は遷移するために実行された１つのイベントをリストで返却する。<br>
	 * ※引数で与えられた情報を元にするためDBアクセスは行いません。
	 * @param eventLogList 対象オブジェクトのイベント履歴
	 * @param workFlowDefDomain 対象オブジェクトに設定されているワークフロー定義
	 * @param currentSequence 現在ステータスのシーケンス番号
	 * @return  List<EventDomain>
	 * @throws Exception
	 */
	public static List<EventDomain> getOneAheadApprovalRequestEvent(List<EventLogDomain> eventLogList, WorkFlowDomain workFlowDefDomain, int currentSequence) throws Exception
	{
		List<EventDomain> resultEventList = new ArrayList<EventDomain>();

		//現在のステータス
		if (currentSequence == 1) {
			// 最初に作成されたシーケンスの場合、空を返す
			return resultEventList;
		}

		//１つ前のイベント
		EventDomain oneAheadEvent = null;
		//１つ前のステータス
		StatusDomain beforeStatus = null;

		//シーケンスが現在のステータスより前になっている最も古いイベントを探す
		for (int i = eventLogList.size()-1; 0 <= i; i-- ) {
			EventDomain event = eventLogList.get(i).getEvent();
			if(
				event.getEventType().getBaseEventType().getId() == AppConstant.BASE_EVENT_TYPE_ID_REQ_APPROVE
				|| event.getEventType().getBaseEventType().getId() == AppConstant.BASE_EVENT_TYPE_ID_APPROVAL
			)
			{
				//承認依頼・承認のイベントの場合
				if(event.getFromStatus().getId() == event.getToStatus().getId()){
					//一部承認
					continue;
				}else{
					// シーケンスが現在のものより前でなければならない
					int diffSeq = currentSequence - event.getFromStatus().getStatusType().getSeq();
					if (1 <= diffSeq) {
						oneAheadEvent = event;
						beforeStatus = event.getFromStatus();
						break;
					}
				}
			}
		}

		EIMStatusType statusType = beforeStatus.getStatusType().createEIMStatusType();
		//１つ前のステータスのドキュメント管理用ステータスタイプ種別
		long docStatusTypeKind = statusType.getKind();

		if(docStatusTypeKind == AppConstant.STATUS_TYPE_KIND_ID_EDITTING ||
				(docStatusTypeKind == AppConstant.STATUS_TYPE_KIND_ID_APPROVE &&
					getDocThrough(workFlowDefDomain, statusType) == AppConstant.THROUGH_APPROVE_ONE)){
			//１つ前のステータスタイプが「編集中」、または一人承認の場合は
			//一部承認イベントを除いた直前のイベントが承認依頼者となる
			resultEventList.add(oneAheadEvent);
		}else{
			//１つ前のステータスのアサイン先一覧を取得する
			AssignDao assignDao = (AssignDao)ApplicationContextLoader.getContext().getBean("assignDao");
			AssignDomain assign = new AssignDomain();
			assign.setStatus(beforeStatus);
			List<AssignDomain> assignDomainList = assignDao.getList(assign);
			for(AssignDomain assignDomain : assignDomainList){
				if(assignDomain.getEvent() != null){
					resultEventList.add(assignDomain.getEvent());
				}
			}
		}

		return resultEventList;
	}

	/**
	 * WF付きフォルダかどうかの判定をします。
	 * WF付きフォルダの場合はtrue、ドキュメント・フォルダの場合はfalseを返却します。
	 *
	 * @param object EIMObject
	 * @return boolean
	 */
	public static boolean isWorkFlowFolder(EIMObject object) throws Exception
	{
		EIMSession sess = EIMThreadContext.getEIMSession();

		Map docObjTypeIdMap = AppObjectTypeUtil.getObjTypeMap(sess, EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT"));

		boolean isDocument = (docObjTypeIdMap.get(new Long(object.getType().getId())) != null);
		if (isDocument){
			return false;
		}else if(! AppObjectUtil.isWFFolder(sess, object)){
			//WFなしフォルダ
			return false;
		}

		return true;
	}

	/**
	 * 指定したオブジェクトとベースイベントタイプで次に実行されるイベントタイプのメールドメインを
	 * リストで取得します
	 *
	 * @param object EIMオブジェクト
	 * @param baseEventTypeId ベースイベントタイプID
	 * @return mailIdMap (key,value)=(メール種別ID,NoticeMailDomain)
	 */
	public static Map<String,List<NoticeMailDomain>> getMailIdMap(EIMObject object, long[] baseEventTypeIds) throws Exception
	{
		Map<String,List<NoticeMailDomain>> mailIdMap = new HashMap<String,List<NoticeMailDomain>>();

		ForcastStatusTypeDomain forcastStatusType = new ForcastStatusTypeDomain();
		StatusDomain statusDomain = new StatusDomain(object.getStatus());
		forcastStatusType.setObject(new ObjectDomain(object));
		forcastStatusType.getObject().setStatus(statusDomain);
		//↓実行するベースイベントタイプは後で指定する
		//forcastStatusType.getBaseEventType().setId(baseEventTypeId);
		Map<String,Object> map = new HashMap<String,Object>();
		map.put(AppConstant.PARAM_KEY_REFER_TO_APPROVAL,AppConstant.PARAM_VALUE_REFER_TO_APPROVAL);
		forcastStatusType.setParamMap(map);

		//イベント実行サービス(ST遷移予測)
		EventExecService eventExecService = (EventExecService)ApplicationContextLoader.getContext().getBean("eventExecService");

		ForcastStatusTypeDomain resultForcastStatusType = null;

		for(int i = 0; i < baseEventTypeIds.length; i++){
			try{
				forcastStatusType.getBaseEventType().setId(baseEventTypeIds[i]);
				resultForcastStatusType = eventExecService.forcastNextStatusType(forcastStatusType);
				if(resultForcastStatusType.getEventType().getMailList().size() > 0){
					mailIdMap.put(Long.toString(baseEventTypeIds[i]),resultForcastStatusType.getEventType().getMailList());
				}
			}catch(EIMException ee){
				//ガード条件にマッチしない・権限チェックエラーの場合なにもしない。
			}catch(Exception e){
				throw e;
			}
		}

		return mailIdMap;
	}

	/**
	 * 承認依頼、公開処理の共通処理(メール関連、PDF関連)
	 *  公開処理でも承認依頼と同じ処理を行うため共通化。
	 * @param sess
	 * @param object
	 * @param approvalReqInfoDomain
	 * @param baseEventTypeExecDomain
	 * @throws Exception
	 */
	public static void approvalRequestMailPDFProcess(EIMSession sess, EIMObject object,
		ApprovalReqInfoDomain approvalReqInfoDomain, BaseEventTypeExecDomain baseEventTypeExecDomain) throws Exception
	{
		/*--- 「メール通知」関連処理 ---*/

		// 「メール通知」オブジェクトを取得
		EIMObjectType objTypeMailNotify = ObjectUtils.getObjectTypeByName(sess, EIMConfig.getValue("OBJECT_TYPE_NAME_MAIL_NOTIFY"));
		EIMObject mailNotifyObj = ObjectUtils.getObjectByTypeAndName(sess, objTypeMailNotify, String.valueOf(object.getId()));
		if (mailNotifyObj == null) {
			// 取得できない場合は作成
			mailNotifyObj = ObjectUtils.createObject(sess, objTypeMailNotify, String.valueOf(object.getId()));
		}

		// 「承認依頼通知タイミング」属性
		if(approvalReqInfoDomain.getTiming() != null && !approvalReqInfoDomain.getTiming().equals("")){
			AppObjectUtil.setAttr(sess, mailNotifyObj, EIMConfig.getValue("ATTR_NAME_MAIL_NOTIFY_REQUEST_NOTIFY"), Long.parseLong(approvalReqInfoDomain.getTiming()));
		}

		// 「受信確認」属性
		long reply = approvalReqInfoDomain.getReply();
		AppObjectUtil.setAttr(sess, mailNotifyObj, EIMConfig.getValue("ATTR_NAME_MAIL_NOTIFY_CONFIRM_RECEIVE"), reply);

		// 「公開通知送信先」に設定する値を生成
		List<String> publisherList = approvalReqInfoDomain.getPublisherList();
		String[] publisherStrs = new String[publisherList.size()];

		long objId = object.getId();
		EIMObjectType objTypeReceive = ObjectUtils.getObjectTypeByName(sess, EIMConfig.getValue("OBJECT_TYPE_NAME_RECEIVE"));

		for(int i = 0; i < publisherList.size(); i++)
		{
			String[] tokensColon = publisherList.get(i).split(":");
			//エントリータイプID
			String entryTypeIdStr = tokensColon[0];
			//エントリー対象ID
			String entryIdStr = tokensColon[1];

			publisherStrs[i] = publisherList.get(i);

			// 「受信確認」オブジェクトを作成
			if (reply == AppConstant.FLAG_ON) {
				List<List> publishUserList = ApproveCommonUtil.getUserFromCode(sess, publisherList.get(i));
				for(int j = 0; j < publishUserList.size(); j++)
				{
					EIMUser user = (EIMUser)publishUserList.get(j);
					String publishName = String.valueOf(objId) + "." + String.valueOf(user.getId());
					if (ObjectUtils.getObjectByTypeAndName(sess, objTypeReceive, publishName) == null) {

						ObjectUtils.createObject(sess, objTypeReceive, publishName);
					}
				}
			}
		}

		// 「公開通知タイミング」属性・「公開通知送信先」属性・「公開通知コメント」属性を設定する
		if(approvalReqInfoDomain.getSendNotifyMailTiming() != null && !approvalReqInfoDomain.getSendNotifyMailTiming().equals("")){
			// 「公開通知タイミング」属性を設定
			AppObjectUtil.setAttr(sess, mailNotifyObj, EIMConfig.getValue("ATTR_NAME_MAIL_NOTIFY_PUBLIC_TIMING")
					,Long.parseLong(approvalReqInfoDomain.getSendNotifyMailTiming())
					);
			// 「公開通知送信先」属性を設定
			AppObjectUtil.setAttr(sess, mailNotifyObj, EIMConfig.getValue("ATTR_NAME_MAIL_NOTIFY_PUBLIC_NOTIFY_TO"), publisherStrs);

			// 「公開通知コメント」属性を設定
			AppObjectUtil.setAttr(sess, mailNotifyObj, EIMConfig.getValue("ATTR_NAME_PUBLIC_COMMENT"), approvalReqInfoDomain.getPublicComment());
		}

		/*--- PDF出力設定 ---*/

		// PDF出力設定ドメイン
		PDFSettingDomain pdfSettingDomain = approvalReqInfoDomain.getPdfDomain();

		boolean isPDFSign = false;		// PDF署名すべきか否か
		boolean isInsertURL = false;	// URL挿入すべきか否か
		boolean isLocalConfSet = false;	// PDF出力設定で設定された個別設定を使用すべきか否か
		EIMObject wfPubObj = null;		// ワークフロー公開処理オブジェクト
		EIMObject pdfSignObj = null;	// PDF署名オブジェクト
		EIMObject insertURLobj = null;  // URL挿入オブジェクト

		//署名時
		if(pdfSettingDomain.isLocalPDFOutputSet()) {
			// PDF出力設定で「電子署名／セキュリティ設定を行う」を設定
			if (pdfSettingDomain.isDoSignPDFAndSetSecurity()) {

				// PDF出力設定で設定された引数を使用
				isLocalConfSet = true;

				// PDF または PDF に変換されるドキュメントかをチェック
				if( PublishAddonUtils.isPDFDocument(sess, object) == false ) {

					throw new EIMAppException(sess, "EIM.ERROR.LOGIC.PDFSIG.SETTING.OUTOFTARGET");
				}
				isPDFSign = true;
			}
		}
		else {

			// ワークフロー公開処理オブジェクトのデフォルト設定を使用
			wfPubObj = PublishAddonUtils.getWfPubObjFromDocObj(sess, object);

			// ワークフロー公開処理で署名・セキュリティ設定をする場合 かつ
			// PDF 変換対象・PDF ファイルそのものである場合
			if( PublishAddonUtils.getDoSignAndSetSecurityConfig( sess, wfPubObj ) == 1 &&
			    PublishAddonUtils.isPDFDocument(sess, object) == true ) {
				isPDFSign = true;
			}
		}
		// PDF署名オブジェクト
		pdfSignObj = AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_SIGNPDF"), String.valueOf(object.getId()));

		if( isPDFSign == true || pdfSignObj != null ) {

			if( pdfSignObj == null ) {

				// PDF 署名オブジェクトが存在しない場合新規作成
				// → LocalConfSet = TRUE ならパラメータから, FALSE ならWF公開処理オブジェクトから属性設定
				pdfSignObj = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_SIGNPDF"), String.valueOf(object.getId()));
				if( isLocalConfSet == true ) {

					// 引数で与えられた属性を設定
					PublishAddonUtils.setSignAndSetSecurityConfig(sess, pdfSignObj, pdfSettingDomain);
				} else {

					// PDF 署名オブジェクトの属性値はWF公開処理オブジェクトとおなじ
					PublishAddonUtils.copyPDFSignAndSecAttrs2Obj(sess, wfPubObj, pdfSignObj);
				}
			} else if(isPDFSign == true){

				// PDF 署名オブジェクトが既に存在する場合
				if( isLocalConfSet == true ) {

					// 引数で与えられた属性を設定
					PublishAddonUtils.setSignAndSetSecurityConfig(sess, pdfSignObj, pdfSettingDomain);
				} else {

					// PDF 署名オブジェクトの属性値はWF公開処理オブジェクトと同じとする
					// (差戻・取戻後の再設定)
					PublishAddonUtils.copyPDFSignAndSecAttrs2Obj(sess, wfPubObj, pdfSignObj);
				}
			} else {

				// 署名しない設定を指定され, PDF署名オブジェクトが存在する場合は消す
				if( pdfSignObj != null ) {

					ObjectUtils.deleteObject(sess, pdfSignObj);
					pdfSignObj = null;
				}
			}
			// PDF 署名オブジェクト「PDF署名ステータス」属性を設定
			if( pdfSignObj != null ) {

				AppObjectUtil.setAttr(sess, pdfSignObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_STATUS"), 0);
			}
		}
		return;
	}

	/**
	 * 承認不要WF判定
	 *
	 * @param sess セッション
	 * @param object 対象ドキュメント
	 * @return true:承認不要WF、false:要承認WF
	 * @throws Exception
	 */
	public static boolean isUnnecessaryWF(EIMSession sess, EIMObject object) throws Exception
	{

		//ステータスチェック 編集中のみ通過
		EIMStatus status = object.getStatus();
		if ( status == null || (status.getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_EDITTING))
		{
			return (false);
		}

		//ワークフローとワークフロー設定オブジェクト取得
		EIMWorkFlow workflow = WorkFlowUtils.getWorkFlowByStatusType(sess, status.getType());
		EIMObject wfSettingObj = AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_WFSETTING"), String.valueOf(workflow.getId()));

		// 承認不要WF/要承認WFのチェック
		String bossApproval = AppObjectUtil.getStrAttr(sess, wfSettingObj, EIMConfig.get("ATTR_NAME_WFSETTING_BOSS_APPROVAL_FLG"));
		if( bossApproval.equals("necessary") )
		{
			return(false);
		}
		return(true);
	}

	/**
	 * PDF署名実施判定
	 *
	 * @param sess セッション
	 * @param object 対象ドキュメント
	 * @return true:実施、false:実施しない
	 * @throws Exception
	 */
	public static boolean isPDFSignDocument(EIMSession sess, EIMObject object) throws Exception
	{
		EIMObject pdfSignObj = AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_SIGNPDF"), String.valueOf(object.getId()));
		if( pdfSignObj != null ) {
			return(true);
		}
		else
		{
			// ワークフロー公開処理オブジェクトの取得
			EIMObject wfpubObj = AppObjectUtil.getWorkFlowProcessing(sess, object);
			if(wfpubObj == null){
				return(false);
			}
			EIMAttribute att = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_FLG"));
			if(att == null){
				return(false);
			}

			//PDF署名チェック
			long flagVal = att.getInts()[0];
			if( flagVal == 1)
			{
				return(true);
			}
			else
			{
				return(false);
			}
		}
	}

	/**
	 * 署名・URL挿入対象判定(PDFファイルか、PDFファイルを含むか)
	 *
	 * @param sess セッション
	 * @param object 対象ドキュメント
	 * @return true:対象、false:対象外
	 * @throws Exception
	 */
	public static boolean isPDFFile(EIMSession sess, EIMObject object) throws Exception
	{
		EIMFormat format = FileUtils.getDefaultFormat(sess, object.getType());
		EIMFile file = null;

		boolean isFolder = false;
		boolean ret = false;

		if (format == null) {
			//フォルダ
			isFolder = true;
		} else {
			// 拡張子のチェック
			file = FileUtils.getFile(sess, object, format);
			if (file == null) {
				//フォルダ
				isFolder = true;
			} else {
				//PDF変換対象か判定
				if( file.getExt().equals(".pdf") ) {
					ret = true;
				}
			}
		}
		if( isFolder == true ) {
			// フォルダの場合, 配下にPDFファイルがあれば true
			List chldObjectList = AppObjectUtil.getChildEIMObjectRecurrently(sess,
				object, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));
			AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

			for (Iterator i = chldObjectList.iterator(); i.hasNext();) {
				EIMObject childObj = (EIMObject) i.next();
				// フォルダの場合は読み飛ばす
				if(helper.isTypeOfFolder(childObj.getType())){
					continue;
				}

				EIMFormat childFormat = FileUtils.getDefaultFormat(sess, childObj.getType());
				EIMFile childFile = FileUtils.getFile(sess, childObj, childFormat);
				if( childFile.getExt().equals(".pdf") ) {
					ret = true;
					break;
				}
			}
		}
		return ret;
	}

	/**
	 * URL挿入実施判定
	 *
	 * @param sess セッション
	 * @param object 対象ドキュメント
	 * @return true:実施、false:実施しない
	 * @throws Exception
	 */
	public static boolean isPDFInserURLDocument(EIMSession sess, EIMObject object) throws Exception
	{
		EIMObject pdfSignObj = AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_INSERTURL"), String.valueOf(object.getId()));
		if( pdfSignObj != null ) {
			return(true);
		}
		else
		{
			// ワークフロー公開処理オブジェクトの取得
			EIMObject wfpubObj = AppObjectUtil.getWorkFlowProcessing(sess, object);
			if(wfpubObj == null){
				return(false);
			}
			EIMAttribute att = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_INSERT_URL_FLG"));
			if(att == null){
				return(false);
			}

			//URL挿入チェック
			long flagVal = att.getInts()[0];
			if( flagVal == 1)
			{
				return(true);
			}
			else
			{
				return(false);
			}
		}
	}

	/**
	 * デフォルト公開通知先取得
	 *
	 * @param sess セッション
	 * @param workflow ワークフロー
	 * @return
	 * @throws Exception
	 */
	public static List<String> getDefaultPublisher(EIMSession sess, EIMWorkFlow workflow) throws Exception
	 {
		List<String> publisherIdName = new ArrayList<String>();
		// 公開通知先エントリー取得
		EIMObjectType objType = ObjectUtils.getObjectTypeByName(sess,EIMConfig.get("OBJECT_TYPE_NAME_WFPUBNOTIFY"));
		EIMObject entry = ObjectUtils.getObjectByTypeAndName(sess,objType,Long.toString(workflow.getId()));
		// ワークフロー設定取得
		EIMObject entry2 = AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_WFSETTING"), String.valueOf(workflow.getId()));
		// 公開通知先デフォルト設定フラグ取得
		boolean pubNotifyFlag = false;
		if( entry2 != null ) {
			EIMAttribute attr2 = entry2.getAttribute(EIMConfig.get("ATTR_NAME_WFSETTING_PUBLISHNOTIFY_FLG"));
			if( attr2 != null ) {
				if( attr2.getInts()[0] == AppConstant.FLAG_ON ) {
					pubNotifyFlag = true;
				}
			}
		}
		// 公開通知先エントリーが存在し, 公開通知先デフォルト設定フラグON
		if( entry != null && pubNotifyFlag == true ) {
			String pubNotifCSVName = "";
			String pubNotifCSVID = "";
			EIMAttribute attrTypeId = entry.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_NOTIFY_ENTRY_TYPE"));
			EIMAttribute attrTypeObj = entry.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_NOTIFY_ENTRY_OBJ"));
			// エントリタイプID, 対象IDが存在
			if( attrTypeId != null && attrTypeObj != null ) {
				// 属性を取得
				long[] ids = TypeConvertUtils.convertToLongArray(attrTypeId.getInts());
				long[] types = TypeConvertUtils.convertToLongArray(attrTypeObj.getInts());
				// エントリリスト (後にソートの上 XML 化)
				List<EIMUser> userList = new ArrayList<EIMUser>();
				List<EIMGroup> groupList = new ArrayList<EIMGroup>();
				List<EIMRole> roleList = new ArrayList<EIMRole>();
				List<EIMComp> compList = new ArrayList<EIMComp>();
				for( int j = 0; j < ids.length; j++ ) {
					// 「公開読取権」のチェックは行わない。
					// (公開通知メール送信時にチェックする)
					if( ids[j] == EIMAccessEntryType.USER ) {
						EIMUser entUser = UserUtils.getUserById(sess,types[j]);
						userList.add(entUser);
					} else if( ids[j] == EIMAccessEntryType.GROUP ) {
						EIMGroup entGroup = GroupUtils.getGroupById(sess,types[j]);
						groupList.add(entGroup);
					} else if( ids[j] == EIMAccessEntryType.ROLE ) {
						EIMRole entRole = RoleUtils.getRoleById(sess,types[j]);
						roleList.add(entRole);
					} else if( ids[j] == EIMAccessEntryType.COMP ) {
						EIMComp entComp = CompUtils.getCompById(sess,types[j]);
						compList.add(entComp);
					}
				}
				if( groupList.size() > 0 ) {
					groupList = AppObjectUtil.getStrSortedList(groupList, "getName", true);
					for( Iterator ite = groupList.iterator(); ite.hasNext(); ) {
						if( pubNotifCSVID.length() > 0 ) {
							pubNotifCSVName = pubNotifCSVName + ",";
							pubNotifCSVID = pubNotifCSVID + ",";
						}
						EIMGroup grp = (EIMGroup)ite.next();
						pubNotifCSVName = pubNotifCSVName + "\"" + grp.getName().replaceAll("\"","\"\"") + "\"";
						pubNotifCSVID = pubNotifCSVID + "2:" + grp.getId();
					}
				}
				if( roleList.size() > 0 ) {
					roleList = AppObjectUtil.getStrSortedList(roleList, "getName", true);
					for( Iterator ite = roleList.iterator(); ite.hasNext(); ) {
						if( pubNotifCSVID.length() > 0 ) {
							pubNotifCSVName = pubNotifCSVName + ",";
							pubNotifCSVID = pubNotifCSVID + ",";
						}
						EIMRole role = (EIMRole)ite.next();
						pubNotifCSVName = pubNotifCSVName + "\"" + role.getName().replaceAll("\"","\"\"") + "\"";
						pubNotifCSVID = pubNotifCSVID + "3:" + role.getId();
					}
				}
				if( compList.size() > 0 ) {
					compList = AppObjectUtil.getStrSortedList(compList, "getName", true);
					for( Iterator ite = compList.iterator(); ite.hasNext(); ) {
						if( pubNotifCSVID.length() > 0 ) {
							pubNotifCSVName = pubNotifCSVName + ",";
							pubNotifCSVID = pubNotifCSVID + ",";
						}
						EIMComp comp = (EIMComp)ite.next();
						pubNotifCSVName = pubNotifCSVName + "\"" + comp.getName().replaceAll("\"","\"\"") + "\"";
						pubNotifCSVID = pubNotifCSVID + "4:" + comp.getId();
					}
				}
				if( userList.size() > 0 ) {
					userList = AppObjectUtil.getStrSortedList(userList, "getName", true);
					for( Iterator ite = userList.iterator(); ite.hasNext(); ) {
						if( pubNotifCSVID.length() > 0 ) {
							pubNotifCSVName = pubNotifCSVName + ",";
							pubNotifCSVID = pubNotifCSVID + ",";
						}
						EIMUser eUser = (EIMUser)ite.next();
						pubNotifCSVName = pubNotifCSVName + "\"" + eUser.getName().replaceAll("\"","\"\"") + "\"";
						pubNotifCSVID = pubNotifCSVID + "1:" + eUser.getId();
					}
				}
				publisherIdName.add(pubNotifCSVID);
				publisherIdName.add(pubNotifCSVName);
				return publisherIdName;
			}
		}
		return null;
	 }

	/**
	 * 過去、自身が承認依頼・承認したイベントを取得する、詳細な取得条件は下記の通り
	 * <li>承認依頼もしくは承認イベントの場合
	 * <li>イベントのFromステータスタイプが引数「fromStatus」のタイプと一致する場合
	 * <li>イベント実行者が自身に限る
	 * @param sess セッション
	 * @param object 対象オブジェクト
	 * @param fromStatus 遷移元ステータス
	 * @return eventLogDomain イベント履歴
	 * @throws Exception
	 */
	public static EventLogDomain getLastExecEventLog(EIMSession sess, EIMObject object, EIMStatus fromStatus) throws Exception
	{
		if(fromStatus == null){
			return null;
		}

		// イベントリストを取得
		EventHistoryService eventHistoryService = (EventHistoryService)ApplicationContextLoader.getContext().getBean("eventHistoryServiceWithoutAttribute");
		EventHistoryDomain evtHistoryDomain = eventHistoryService.getByObjId(object.getId());
		List<EventLogDomain> eventLogList = evtHistoryDomain.getEventLogList();

		EventLogDomain eventLogDomain = getLastExecEventLog(eventLogList, fromStatus, sess.getUser());
		return eventLogDomain;
	}

	/**
	 * 過去、自身が承認依頼・承認したイベントを取得する、詳細な取得条件は下記の通り
	 * <li>承認依頼もしくは承認イベントの場合
	 * <li>イベントのFromステータスタイプが引数「fromStatus」のタイプと一致する場合
	 * <li>イベント実行者が自身に限る<br>
	 * ※引数で与えられた情報を元にするためDBアクセスは行いません。
	 * @param eventLogList 対象オブジェクトのイベント履歴
	 * @param fromStatus 遷移元ステータス
	 * @param user 対象ユーザー
	 * @return eventLogDomain イベント履歴
	 * @throws Exception
	 */
	public static EventLogDomain getLastExecEventLog(List<EventLogDomain> eventLogList, EIMStatus fromStatus, EIMUser user) throws Exception
	{
		if(fromStatus == null){
			return null;
		}

		if(eventLogList == null || eventLogList.size() < 1) {
			// イベント履歴がない場合はnullを返却
			return null;
		}

		for (int i = eventLogList.size() - 1; i >= 0; i--) {
			EventLogDomain eventLogDomain = eventLogList.get(i);
			long baseEventTypeId = eventLogDomain.getEvent().getEventType().getBaseEventType().getId();
			// 承認依頼もしくは承認イベントの場合
			if(baseEventTypeId == AppConstant.BASE_EVENT_TYPE_ID_APPROVAL || baseEventTypeId == AppConstant.BASE_EVENT_TYPE_ID_REQ_APPROVE){
				// fromステータスが現在ステータスのイベントを取得
				if(eventLogDomain.getEvent().getFromStatus().getStatusType().getId() == fromStatus.getType().getId()) {
					// イベント実行者が自分の場合
					if(eventLogDomain.getEvent().getCUser().getId() == user.getId()){
						return eventLogDomain;
					}
				}
			}
		}
		return null;
	}

	 /**
	  * メール通知タイミングを返却する
	  * @param timing	メール通知タイミング(0,1,3)
	  * @return immediate メール通知タイミング("immediate","scheduled","off")
	  * @throws Exception
	  */
	 public static String getMailNoticeImmediate(int timing) throws Exception
	 {
		 String immediate = "";
		 switch (timing){
			// 0:即時
			case AppConstant.MAILNOTICE_TIMING_IMMEDIATE:
				immediate = AppConstant.MAILNOTICE_TIMING_IMMEDIATE_STR;
				break;
			// 1:定時
			case AppConstant.MAILNOTICE_TIMING_SCHEDULED:
				immediate = AppConstant.MAILNOTICE_TIMING_SCHEDULED_STR;
				break;
			// 3:なし
			case AppConstant.MAILNOTICE_TIMING_OFF:
				immediate = AppConstant.MAILNOTICE_TIMING_OFF_STR;
				break;
			default:
				immediate = "";
				break;
		}
		return immediate;
	 }

	 /**
	  * メール通知タイミングを返却する
	  * @param immediate	メール通知タイミング("immediate","scheduled","off")
	  * @return timing メール通知タイミング(0,1,3)
	  * @throws Exception
	  */
	 public static int getMailNoticeTiming(String immediate) throws Exception
	 {
		int timing = -1;
		// 0:即時
		if(immediate.equals(AppConstant.MAILNOTICE_TIMING_IMMEDIATE_STR)){
			timing = AppConstant.MAILNOTICE_TIMING_IMMEDIATE;
		}
		// 1:定時
		if(immediate.equals(AppConstant.MAILNOTICE_TIMING_SCHEDULED_STR)){
			timing = AppConstant.MAILNOTICE_TIMING_SCHEDULED;
		}
		// 3:なし
		if(immediate.equals(AppConstant.MAILNOTICE_TIMING_OFF_STR)){
			timing = AppConstant.MAILNOTICE_TIMING_OFF;
		}
		return timing;
	}

	/**
	 * 指定のユーザが指定のステータスの承認者に設定されているかのチェックを行います。
	 *
	 * @param statusId チェック対象ステータスID
	 * @param userId ユーザID
	 * @return true:承認者、false:承認者でない
	 * @throws Exception
	 */
	public static boolean isUserEntriedApproverCheck(long statusId, long userId) throws Exception
	{
		boolean rtn = false;

		// StatusTypeServiceクラスのインスタンス化
		ApplicationContext contxt = ApplicationContextLoader.getContext();
		AssignmentService  assignmentService = (AssignmentService)contxt.getBean("assignmentService2");

		AssignmentCriteria assCriteria = new AssignmentCriteria();
		assCriteria.setStatusId(statusId);

		// StatusType取得
		List<AssignmentDomain> assignmentDomainList = assignmentService.getList(assCriteria);

		List<jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain> entryUserList = getEntryUserListByAssignmentList(assignmentDomainList);

		for (jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain entryUser : entryUserList){

			if (entryUser.getId() == userId) {
				rtn = true;
				break;
			}
		}

		return rtn;
	}

	/**
	 * エントリのリストをユーザのリストに変換します。
	 *
	 * @param assignmentList アサインリスト
	 * @return アサイン先に含まれるユーザのリスト
	 * @throws Exception
	 */
	private static List<jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain> getEntryUserListByAssignmentList(List<AssignmentDomain> assignmentList) throws Exception {

		// エントリユーザリスト
		List<jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain> entryUserList = new ArrayList<jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain>();

		for (AssignmentDomain entry : assignmentList){
			// ユーザの場合
			if (entry.getEntryType() == EntryTypeEnum.USER) {
				// 対象オブジェクトへアクセス可能ユーザリストに追加
				entryUserList.add((jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain)entry.getEntryElement());

			// グループの場合
			} else if (entry.getEntryType() == EntryTypeEnum.GROUP) {
				GroupService groupService  = (GroupService)jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader.getApplicationContext().getBean("groupServiceForSelectEntry");

				// グループに所属するユーザリスト取得
				GroupDomain group = groupService.getById(entry.getEntryElement().getId());

				// 子グループに所属するユーザも再帰的に取得
				entryUserList = getRecursiveUserList(group, entryUserList);

			// ロールの場合
			} else if (entry.getEntryType() == EntryTypeEnum.ROLE) {
				RoleService roleService  = (RoleService)jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader.getApplicationContext().getBean("roleServiceForSelectEntry");
				// ロールに所属するユーザリスト取得
				RoleDomain role = roleService.getById(entry.getEntryElement().getId());

				for (jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain roleUser: role.getUserList()) {
					// 対象オブジェクトへアクセス可能ユーザリストに追加
					entryUserList.add(roleUser);
				}

			// 複合グループの場合
			} else if (entry.getEntryType() == EntryTypeEnum.COMPLEX) {
				ComplexService complexService = (ComplexService)jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader.getApplicationContext().getBean("complexServiceForSelectEntry");
				// 複合グループに所属するユーザリスト取得
				ComplexDomain complex = complexService.getById(entry.getEntryElement().getId());

				for (jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain compUser: complex.getUserList()) {
					// 対象オブジェクトへアクセス可能ユーザリストに追加
					entryUserList.add(compUser);
				}
			}
		}
		return entryUserList;
	}

	/**
	 * 親グループのユーザ情報を再帰的に取得します。
	 *
	 * @param group 対象グループ
	 * @param userList ユーザリスト
	 * @return 子グループを含めたユーザリスト
	 */
	private static List<jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain> getRecursiveUserList(GroupDomain group,
			List<jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain> userList) {

		for(jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain user: group.getUserList()){

			// ユーザ情報追加
			userList.add(user);
		}

		if(group.getChildList().size() > 0) {

			// 子グループが存在する場合、再帰的にユーザ情報取得/追加
			for(GroupDomain childGroup: group.getChildList()) {

				getRecursiveUserList(childGroup, userList);
			}
		}
		return userList;
	}

	/**
	 * 公開通知先変更有無のチェック
	 *
	 * @param srcPublisherStrs 変更前の公開通知先配列
	 * @param destPublisherStrs 変更後の公開通知先配列
	 * @return 公開通知先変更有無のチェック結果(true:変更有、false:変更無)
	 */
	public static boolean checkPublisherChange(String[] srcPublisherStrs, String[] destPublisherStrs) throws Exception {
		boolean changePublisher = false;
		if(srcPublisherStrs == null || srcPublisherStrs.length < 1){
			if(destPublisherStrs == null || destPublisherStrs.length < 1){
				// 変更なし
				return false;
			}else{
				// 変更あり
				return true;
			}
		}

		// それぞれ配列をソートする
		Arrays.sort(srcPublisherStrs);
		Arrays.sort(destPublisherStrs);
		// それぞれ文字列に変換
		String srcPublisherStr = Arrays.toString(srcPublisherStrs);
		String destPublisherStr = Arrays.toString(destPublisherStrs);
		if( !srcPublisherStr.equals(destPublisherStr) ){
			// 変更あり
			changePublisher = true;
		}
		return changePublisher;
	}

	/**
	 * 上長のみ表示デフォルト設定のチェック
	 *
	 * @param wfSettingObj ワークフロー設定オブジェクト
	 * @param statusTypeId 現在ステータスタイプID
	 * @return 上長のみ表示デフォルトのチェック結果(true:上長のみ表示をデフォルトで設定する、false:上長のみ表示をデフォルトで設定しない)
	 * */
	public static boolean checkDefaultBossOnly( EIMObject wfSettingObj,long statusTypeId) throws Exception {
		boolean defBossOnly = false;
		if( wfSettingObj != null ) {
			EIMAttribute bossOnlyFlagAttr = wfSettingObj.getAttribute( EIMConfig.get("ATTR_NAME_BOSS_ONLY_DEFAULT_STATUS"));
			if(bossOnlyFlagAttr != null) {
				for(long bossOnlyStatysTypeId : bossOnlyFlagAttr.getInts()) {
					if(bossOnlyStatysTypeId == statusTypeId) {
						defBossOnly = true;
						break;
					}
				}
			}
		}
		return defBossOnly;

	}

	/**
	 * 対象のユーザがオブジェクトに対し権限を保持しているかどうかを返却する
	 * 保持しているユーザのユーザIDをSetで返却する
	 * 実行するSQLはPL/SQLのSecurityUtils.enabledを複数ユーザIDに対応したもの
	 * @param object ドキュメントオブジェクト
	 * @param userIdsList ユーザIDを1000件ずつ分割したリスト
	 * @param roleId アクセス権限ID
	 * @return 権限を保持しているユーザIDSet
	 */
	public static Set<Long> getAuthorizedUserIds(EIMObject object, List<List<Long>> userIdsList, int roleId) throws Exception	{
		EIMSession sess = EIMThreadContext.getEIMSession();
		// Connection
		Connection conn = sess.getDBConnection();
		// Statement
		PreparedStatement pstmt = null;
		ResultSet rset = null;

		Set<Long> authorizedUserIdList = new HashSet<>();

		try{
			for(List<Long> userIds: userIdsList) {
				String sql = String.format("select TMP.entry as userId from" +
						"(" +
						"select entry, permit, rank() over (partition by sid, entry order by priority) as ranks" +
						" from EIMACR ACR inner join EIMACU ACU on ACR.id = ACU.eid" +
						" where ACU.sid = ?" +
						" and ACU.entry in (%s)" +
						" and ACR.role = ?" +
						" and ACR.permit != 2" +
						") TMP" +
						" where TMP.ranks = 1" +
						" and TMP.permit = 1",
						DatabasePlugInLoader.getPlugIn().getQueryStringSelectArray(Types.BIGINT));

				pstmt = conn.prepareStatement(sql);

				pstmt.setLong(1, object.getSecurity().getId());
				pstmt.setArray(2, DatabasePlugInLoader.getPlugIn().createArray(conn, userIds.toArray(new Long[0])));
				pstmt.setLong(3, roleId);

				rset = pstmt.executeQuery();

				while (rset.next()) {
					authorizedUserIdList.add(rset.getLong("userId"));
				}
			}
		} finally {
			if(rset != null){
				// Close ResultSet
				rset.close();
			}
			if(pstmt != null){
				// Close Statement
				pstmt.close();
			}
		}

		return authorizedUserIdList;
	}

	/**
	 * 対象のユーザのロールを取得しユーザIDをキーとしたロールリストのマップを返却する
	 * @param userIdsList ユーザIDを1000件ずつ分割したリスト
	 * @return ユーザIDをキーとしたユーザが属するEIMRoleリストマップ
	 */
	public static Map<Long, List<EIMRole>> getUserRoleMap(List<List<Long>> userIdsList) throws Exception {
		EIMSession sess = EIMThreadContext.getEIMSession();
		// Connection
		Connection conn = sess.getDBConnection();
		// Statement
		PreparedStatement pstmt = null;
		ResultSet rset = null;

		Map<Long, List<EIMRole>> userRoleMap = new HashMap<>();

		try{
			for(List<Long> userIds: userIdsList) {
				String sql = String.format("select id as userId, rid as roleId from eimrasgn where id in (%s)",
						DatabasePlugInLoader.getPlugIn().getQueryStringSelectArray(Types.BIGINT));

				pstmt = conn.prepareStatement(sql);
				pstmt.setArray(1, DatabasePlugInLoader.getPlugIn().createArray(conn, userIds.toArray(new Long[0])));

				rset = pstmt.executeQuery();

				while (rset.next()) {
					Long userId = rset.getLong("userId");
					Long roleId = rset.getLong("roleId");

					EIMRole role = new EIMRole(roleId, null, null, 0);
					if(userRoleMap.containsKey(userId)) {
						userRoleMap.get(userId).add(role);
					} else {
						List<EIMRole> roleList = new ArrayList<>();
						roleList.add(role);
						userRoleMap.put(userId, roleList);
					}
				}
			}
		} finally {
			if(rset != null){
				// Close ResultSet
				rset.close();
			}
			if(pstmt != null){
				// Close Statement
				pstmt.close();
			}
		}

		return userRoleMap;
	}

	/**
	 * 対象のユーザのグループを取得しユーザIDをキーとしたグループリストのマップを返却する
	 * @param userIdsList ユーザIDを1000件ずつ分割したリスト
	 * @return ユーザIDをキーとしたユーザが属するEIMGroupリストマップ
	 */
	public static Map<Long, List<EIMGroup>> getUserGroupMap(List<List<Long>> userIdsList) throws Exception {
		EIMSession sess = EIMThreadContext.getEIMSession();
		// Connection
		Connection conn = sess.getDBConnection();
		// Statement
		PreparedStatement pstmt = null;
		ResultSet rset = null;

		Map<Long, List<EIMGroup>> userGroupMap = new HashMap<>();

		try {
			for(List<Long> userIds: userIdsList) {
				String sql = String.format("select id as userId, gid as groupId from eimgasgn where id in (%s)",
						DatabasePlugInLoader.getPlugIn().getQueryStringSelectArray(Types.BIGINT));

				pstmt = conn.prepareStatement(sql);
				pstmt.setArray(1, DatabasePlugInLoader.getPlugIn().createArray(conn, userIds.toArray(new Long[0])));

				rset = pstmt.executeQuery();

				while (rset.next()) {
					Long userId = rset.getLong("userId");
					Long groupId = rset.getLong("groupId");

					EIMGroup group = new EIMGroup(groupId, null, null, null);
					if(userGroupMap.containsKey(userId)) {
						userGroupMap.get(userId).add(group);
					} else {
						List<EIMGroup> groupList = new ArrayList<>();
						groupList.add(group);
						userGroupMap.put(userId, groupList);
					}
				}
			}
		} finally {
			if(rset != null){
				// Close ResultSet
				rset.close();
			}
			if(pstmt != null){
				// Close Statement
				pstmt.close();
			}
		}
		return userGroupMap;
	}
}
