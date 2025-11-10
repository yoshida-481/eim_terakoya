package jp.co.ctc_g.eim.admin.business.service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import common.util.AppConstant;
import common.util.NamespaceUtil;
import eim.util.EIMConfig;
import jp.co.ctc_g.eim.admin.business.domain.WorkflowAdminDomain;
import jp.co.ctc_g.eim.admin.business.service.WorkFlowDefAdminService;
import jp.co.ctc_g.eim.app.document.business.domain.WorkFlowDocDomain;
import jp.co.ctc_g.eim.app.document.business.service.WorkFlowDefDocService;
import jp.co.ctc_g.eim.app.form.business.domain.AttributeTypeLayoutDomain;
import jp.co.ctc_g.eim.app.form.business.domain.StatusTypeLayoutDomain;
import jp.co.ctc_g.eim.app.form.business.service.AttributeTypeLayoutService;
import jp.co.ctc_g.eim.app.form.business.service.StatusTypeLayoutService;
import jp.co.ctc_g.eim.framework.business.domain.BelongDomain;
import jp.co.ctc_g.eim.framework.business.domain.BelongType;
import jp.co.ctc_g.eim.framework.business.domain.EntryDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.WorkFlowDomain;
import jp.co.ctc_g.eim.framework.business.service.WorkFlowDefService;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.AttributeTypeCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.EventTypeCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.StatusTypeCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.EventTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.SecurityDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.StatusSecurityDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.StatusTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.WorkflowDomain;
import jp.co.ctc_g.eim.framework2.business.service.EventTypeService;
import jp.co.ctc_g.eim.framework2.business.service.ObjectService;
import jp.co.ctc_g.eim.framework2.business.service.ObjectTypeService;
import jp.co.ctc_g.eim.framework2.business.service.SecurityService;
import jp.co.ctc_g.eim.framework2.business.service.StatusTypeService;
import jp.co.ctc_g.eim.framework2.business.service.WorkflowService;
import jp.co.ctc_g.eim.framework2.common.enumeration.DuplicateCheckModeEnum;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;


/**
 * WorkFlowDefAdminService実装クラス
 * @see jp.co.ctc_g.eim.admin.business.service.WorkFlowDefAdminService
 * @since Ver6.0
 */
public class WorkFlowDefAdminServiceImpl implements WorkFlowDefAdminService {

	/** ワークフローService(framework) */
	private WorkFlowDefService workFlowDefService;

	/** ワークフローService(framework2) */
	private WorkflowService workflowService;

	/** ワークフローService(ドキュメント管理) */
	private WorkFlowDefDocService workFlowDefDocService;
	
	/** ステータスタイプService(framework2) */
	private StatusTypeService statusTypeService;

	/** ステータスタイプ(レイアウト情報)Service */
	private StatusTypeLayoutService statusTypeLayoutService;
	
	/** イベントタイプService */
	private EventTypeService eventTypeService;
	
	/** 属性タイプ(レイアウト情報)Service */
	private AttributeTypeLayoutService attributeTypeLayoutService;
	
	/** オブジェクトタイプService */
	private ObjectTypeService objectTypeService;
	
	/** オブジェクトService */
	private ObjectService objectService;
	
	/** セキュリティService(framework2) */
	private SecurityService securityService;

	/**
     * @see jp.co.ctc_g.eim.admin.business.service.WorkFlowDefAdminService#getDefById(String,String,long)
	 * @since Ver6.0
	 */
	public WorkflowAdminDomain getDefById(String adminAppId, String namespace, long workFlowId) throws Exception {
		
		// ワークフロー定義を取得
		WorkFlowDomain workFlowDomain = workFlowDefService.getDefById(workFlowId);
		
		List<StatusTypeLayoutDomain> sttypeLayoutList = new ArrayList<StatusTypeLayoutDomain>();
		
		// ステータスタイプのレイアウト情報を取得
		StatusTypeCriteria sttypeCriteria = new StatusTypeCriteria();
		sttypeCriteria.setWorkflowId(new Long(workFlowDomain.getId()));
		
		// 汎用の場合
		if (adminAppId.equals(AppConstant.ADMIN_APP_ID_GENERAL)) {
			
			// レイアウト情報を含まないステータスタイプ一覧を取得
			List<StatusTypeDomain> statusTypeList = statusTypeService.getList(sttypeCriteria);
			
			// 取得したステータス情報をStatusTypeLayoutDomainに変換する
			for (StatusTypeDomain statusType : statusTypeList) {
				
				StatusTypeLayoutDomain stTypeLayout = new StatusTypeLayoutDomain();
				
				stTypeLayout.setId(statusType.getId());
				stTypeLayout.setSequence(statusType.getSequence());
				
				List<AttributeTypeLayoutDomain> attTypeLayoutList = new ArrayList<AttributeTypeLayoutDomain>();
				
				for (AttributeTypeDomain attType : statusType.getAttributeTypeList()) {
					
					AttributeTypeLayoutDomain attTypeLayout = new AttributeTypeLayoutDomain();
					
					attTypeLayout.setId(attType.getId());
					attTypeLayout.setDefinitionName(attType.getDefinitionName());
					attTypeLayout.setValueType(attType.getValueType());
					attTypeLayout.setNameList(attType.getNameList());
					attTypeLayout.setMultiple(attType.isMultiple());
					attTypeLayout.setDefaultLongValueList(attType.getDefaultLongValueList());
					attTypeLayout.setDefaultStringValueList(attType.getDefaultStringValueList());
					attTypeLayout.setDefaultTextValueList(attType.getDefaultTextValueList());
					attTypeLayout.setDefaultDateValueList(attType.getDefaultDateValueList());
					attTypeLayout.setDefaultDoubleValueList(attType.getDefaultDoubleValueList());
					attTypeLayout.setCodeType(attType.getCodeType());
					
					attTypeLayoutList.add(attTypeLayout);
				}
				stTypeLayout.setAttributeLayoutList(attTypeLayoutList);
				
				sttypeLayoutList.add(stTypeLayout);
			}
			
		// 帳票管理の場合
		} else {
			
			// レイアウト情報を含むステータスタイプ一覧を取得
			sttypeLayoutList = statusTypeLayoutService.getList(sttypeCriteria);
			
			// 検索対象の属性タイプID
			Set<Long> attTypeIdSet = new HashSet<Long>();
			
			// 帳票管理のネームスペースに合致する属性タイプだけ返却
			for (StatusTypeLayoutDomain sttypeLayout : sttypeLayoutList) {
				
				// 対象の属性タイプIDを重複無しで追加
				for (AttributeTypeLayoutDomain attTypeLayout : sttypeLayout.getAttributeLayoutList()) {
					attTypeIdSet.add(attTypeLayout.getId());
				}
			}
			
			if (attTypeIdSet.size() > 0) {
				
				List<Long> attTypeList = new ArrayList<Long>(attTypeIdSet);
				
				AttributeTypeCriteria attTypeCriteria = new AttributeTypeCriteria();
				attTypeCriteria.setIds(attTypeList);
				// 検索条件にネームスペースを追加する(曖昧検索)
				String defSearchName = NamespaceUtil.concatenate(namespace, null) + "*";
				attTypeCriteria.setDefinitionName(defSearchName);
				// 検索実行
				List<AttributeTypeLayoutDomain> attTypeLayoutList = attributeTypeLayoutService.getList(attTypeCriteria);
				
				// 表示する属性タイプのMap
				Set<Long> dispAttTypeSet = new HashSet<Long>();
				for (AttributeTypeLayoutDomain attTypeLayout : attTypeLayoutList) {
					dispAttTypeSet.add(attTypeLayout.getId());
				}
				
				// 帳票管理のネームスペースに合致する属性タイプだけ返却
				for (int i = 0; i < sttypeLayoutList.size(); i++) {
					
					List<AttributeTypeLayoutDomain> dispAttTypeLayoutList = new ArrayList<AttributeTypeLayoutDomain>();
					
					for (AttributeTypeLayoutDomain attTypeLayout : sttypeLayoutList.get(i).getAttributeLayoutList()) {
						
						if (dispAttTypeSet.contains(attTypeLayout.getId())) {
							// 表示すべき属性タイプとして追加
							dispAttTypeLayoutList.add(attTypeLayout);
						}
					}
					
					sttypeLayoutList.get(i).setAttributeLayoutList(dispAttTypeLayoutList);
				}
			}
		}
		
		// 返却用の属性タイプ情報を含むステータスタイプのマップ生成
		Map<Integer, StatusTypeLayoutDomain> statusTypeLayoutMap = new HashMap<Integer, StatusTypeLayoutDomain>();
		for (StatusTypeLayoutDomain stTypeLayout : sttypeLayoutList) {
			statusTypeLayoutMap.put(stTypeLayout.getSequence(), stTypeLayout);
		}
		
		// ワークフロー定義を返却用のドメインに変換
		WorkflowAdminDomain workFlowAdmin = new WorkflowAdminDomain(workFlowDomain);
		// ワークフロー定義にステータスタイプ情報を設定
		workFlowAdmin.setStatusTypeLayoutMap(statusTypeLayoutMap);
		
		return workFlowAdmin;
	}
	
	/**
     * @see jp.co.ctc_g.eim.admin.business.service.WorkFlowDefAdminService#updateDef(String,WorkflowAdminDomain)
	 * @since Ver6.0
	 */
	public void updateDef(String adminAppId, WorkflowAdminDomain workFlowAdmin) throws Exception {
		
		// 更新前のステータスタイプIDを保持
		Set<Long> deleteStTypeIds = new HashSet<Long>();
		
		// ステータスを取得
		// ※クライアントからは削除情報は渡されないため取得情報にて比較する
		StatusTypeCriteria stTypeCriteria = new StatusTypeCriteria();
		stTypeCriteria.setWorkflowId(new Long(workFlowAdmin.getId()));
		List<StatusTypeDomain> beforeStTypeList = statusTypeService.getList(stTypeCriteria);
		
		EventTypeCriteria evtTypeCriteria = new EventTypeCriteria();
		evtTypeCriteria.setWorkflowId(new Long(workFlowAdmin.getId()));
		List<EventTypeDomain> beforeEvtTypeList = eventTypeService.getList(evtTypeCriteria);
		
		// ステータス別セキュリティを取得
		// key: New StatusType sequence, value:beforeStTypeId
		Map<Integer, Long> sequenceStTypeMap = new HashMap<Integer, Long>();
		// key: beforeStTypeId value: afterStTypeId
		Map<Long, Long> beforeAfterMap = new HashMap<Long, Long>();
		List<Long> stIds = getStatusTypeIds(workFlowAdmin.getStatusTypeList(),beforeStTypeList, sequenceStTypeMap);
		Map<SecurityDomain, List<StatusSecurityDomain>> statusSecurityListMap = getStatusSecurity(stIds);
		
		// 更新前のステータスタイプIDを保持
		for (StatusTypeDomain deleteStType : beforeStTypeList) {
			deleteStTypeIds.add(deleteStType.getId());
		}
		
		// 帳票管理における初回ワークフロー定義(ステータスタイプ未定義)時、
		// 第一ステータスのアサイン先エントリーにユーザ定義グループ『作成者(ID：-11000)』を割り当てる
		if (adminAppId.equals(AppConstant.ADMIN_APP_ID_FORM) && beforeStTypeList.size() == 0) {
			BelongDomain belong = new BelongDomain();
			EntryDomain entry = new EntryDomain();
			entry.setId(-11000);
			belong.setBelonging(entry);
			belong.setBelongType(BelongType.USERDEFGROUP);
			// 第一ステータスタイプに追加
			List<jp.co.ctc_g.eim.framework.business.domain.entity.StatusTypeDomain> statusTypeList = workFlowAdmin.getStatusTypeList();
			if (statusTypeList.size() != 0) {
				boolean canAssign = true;
				List<BelongDomain> firstStatusTypeBelongList = statusTypeList.get(0).getAssignEntry().getBelongList();
				for (BelongDomain firstStatusTypeBelong : firstStatusTypeBelongList) {
					// 既にアサイン先エントリーの対象となっている場合は追加しない
					if (firstStatusTypeBelong.getBelonging().getId() == -11000) {
						canAssign = false;
						break;
					}
				}
				if (canAssign) {
					firstStatusTypeBelongList.add(belong);
				}
			}
		}
		
		// ワークフロー定義を更新
		// frameworkの仕様
		// ※ステータスタイプに紐つくステータスが存在すると更新できない
		// ※更新時、ステータスタイプ、イベントタイプを全て削除する
		// ※属性タイプの適用は実施しない
		// ※更新後の情報が設定される
		workFlowDefService.updateDef(workFlowAdmin);
		
		// 更新前のステータスタイプオブジェクトを削除
		Iterator<Long> ite = deleteStTypeIds.iterator();
		while (ite.hasNext()) {

			long stTypeId = ite.next();

			// 帳票管理の場合
			if (adminAppId.equals(AppConstant.ADMIN_APP_ID_FORM)) {

				StatusTypeLayoutDomain beforeStTypeLayout = new StatusTypeLayoutDomain();
				beforeStTypeLayout.setId(stTypeId);
				statusTypeLayoutService.deleteStatusTypeObject(beforeStTypeLayout);

			}
		}
		
		// 更新後のワークフロー定義を取得
		WorkflowDomain afterWorkflow = 
			workflowService.getById(workFlowAdmin.getId());
		
		// 属性タイプの適用
		for (int i = 0; i < afterWorkflow.getStatusTypeList().size(); i++) {

			// 更新前のステータスタイプを取得
			jp.co.ctc_g.eim.framework.business.domain.entity.StatusTypeDomain beforeStType = workFlowAdmin.getStatusTypeList().get(i);
			
			// 更新後のステータスタイプを取得
			StatusTypeDomain afterStType = afterWorkflow.getStatusTypeList().get(i);
			
			// 属性タイプを適用
			for (jp.co.ctc_g.eim.framework.business.domain.entity.AttributeTypeDomain targetAttType : beforeStType.getAttrTypeList()) {
				
				AttributeTypeDomain applyAttType = new AttributeTypeDomain();
				applyAttType.setId(targetAttType.getId());
				statusTypeService.addAttributeType(afterStType, applyAttType);
			}
		
			// 帳票管理の場合は属性タイプの表示順を再作成
			if (adminAppId.equals(AppConstant.ADMIN_APP_ID_FORM)) {
				
				// 表示順情報が存在する場合
				if (workFlowAdmin.getStatusTypeLayoutMap().containsKey(afterStType.getSequence())) {
					
					// 表示順情報を取得
					StatusTypeLayoutDomain stTypeLayout =
						workFlowAdmin.getStatusTypeLayoutMap().get(afterStType.getSequence());
					
					// 複写したステータスタイプIDを設定
					stTypeLayout.setId(afterStType.getId());
					
					// 表示順情報を含むステータスタイプオブジェクトを生成
					statusTypeLayoutService.createStatusTypeObject(stTypeLayout);
				}
			}
		}
		
		// 文書管理用WFに対してワークフロー更新によって削除された属性等を再設定する
		if (isWorkflowForDoc(beforeEvtTypeList)) {
			updateWorkflowPublishObject(beforeStTypeList, afterWorkflow);
			registConfigurationStatusInfoForAdmin(afterWorkflow);
		}
		
		// 帳票管理の場合は全てのイベントタイプに「コメント」属性・「追加のメール通知先」属性・「通知方法」属性を適用
		if (adminAppId.equals(AppConstant.ADMIN_APP_ID_FORM)) {
			addEventAttributeType(afterWorkflow.getEventTypeList());
		}

		// タスク管理の場合は全てのイベントタイプに「コメント」属性・「追加のメール通知先」属性・「通知方法」属性を適用
		if (adminAppId.equals(AppConstant.ADMIN_APP_ID_TASK)) {
			addEventAttributeTypeForTask(afterWorkflow.getEventTypeList());
		}
		
		// 新しくなったステータスタイプにステータス別セキュリティを設定する。
		for(int seq=0; seq<afterWorkflow.getStatusTypeList().size(); seq++) {
			StatusTypeDomain stType = afterWorkflow.getStatusTypeList().get(seq);
			if(sequenceStTypeMap.get(seq) != null) {
				beforeAfterMap.put(sequenceStTypeMap.get(seq), stType.getId());
			}
		}
		setStatusSecurity(statusSecurityListMap, beforeAfterMap);
		
	}

	/**
	 * 文書管理用のワークフローかどうかをイベントタイプの属性から判別する
	 *
	 * @param eventTypeList	
	 * @return boolean
	 * @throws Exception 
	 */
	private boolean isWorkflowForDoc(List<EventTypeDomain> eventTypeList) throws Exception {
		// 文書管理用イベントタイプに登録される属性リスト
		List<String> reqApproveAttrList = new ArrayList<String>(){
            {
                add(ConfigUtils.getByKey("ATTR_NAME_MAIL_NOTIFY_REQUEST_NOTIFY"));	// 承認依頼通知タイミング
                add(ConfigUtils.getByKey("ATTR_NAME_MAIL_NOTIFY_PUBLIC_TIMING"));	// 公開通知タイミング
                add(ConfigUtils.getByKey("ATTR_NAME_MAIL_NOTIFY_PUBLIC_NOTIFY_TO"));	// 公開通知先
                add(ConfigUtils.getByKey("ATTR_NAME_PUBLIC_COMMENT_LOG"));	// 公開通知コメントログ
                add(ConfigUtils.getByKey("ATTR_NAME_MAIL_NOTIFY_CONFIRM_RECEIVE"));	// 受信確認
                add(ConfigUtils.getByKey("EVENT_ATTR_NAME_COMMENT"));	// コメント
                
            }
        };
		List<String> approvalAttrList = new ArrayList<String>(){
            {
                add(ConfigUtils.getByKey("ATTR_NAME_MAIL_NOTIFY_REQUEST_NOTIFY"));	// 承認依頼通知タイミング
                add(ConfigUtils.getByKey("ATTR_NAME_PUBLIC_COMMENT_LOG"));	// 公開通知コメントログ
                add(ConfigUtils.getByKey("EVENT_ATTR_NAME_COMMENT"));	// コメント
            }
        };
        
		for (EventTypeDomain eventTypeDomain : eventTypeList) {
			// 承認依頼イベントの場合
			if (eventTypeDomain.getBase().getId() == AppConstant.BASE_EVENT_TYPE_ID_REQ_APPROVE) {
				long count = eventTypeDomain.getAttributeTypeList().stream()
								.filter(v -> reqApproveAttrList.contains(v.getName()))
								.count();
				// 文書管理用の属性が全て存在するか
				if (count == reqApproveAttrList.size()) {
					// イベントタイプのうちいずれか1つが合致すれば文書管理で作成されたものとみなしてtrueを返す
					return true;
				}
				
			}
			// 承認イベントの場合
			if(eventTypeDomain.getBase().getId() == AppConstant.BASE_EVENT_TYPE_ID_APPROVAL){
				long count = eventTypeDomain.getAttributeTypeList().stream()
						.filter(v -> approvalAttrList.contains(v.getName()))
						.count();
				// 文書管理用の属性が全て存在するか
				if (count == approvalAttrList.size()) {
					// イベントタイプのうちいずれか1つが合致すれば文書管理で作成されたものとみなしてtrueを返す
					return true;
				}
			}
		}
		
		return false;
	}

	/**
	 * ワークフローの更新によって削除された「公開処理中」ステータスタイプが保持していた「ワークフロー公開処理」オブジェクト
	 * を新規登録されたステータスタイプに対して関連付けるよう更新する。
	 *
	 * @param List<StatusTypeDomain>
	 * @param WorkflowDomain
	 * @return
	 * @throws Exception
	 */
	private void updateWorkflowPublishObject(List<StatusTypeDomain> beforeStTypeList, WorkflowDomain afterWorkflow) throws Exception {
		// 更新前の「公開処理中」ステータスタイプを抽出
		StatusTypeDomain oldPublishingStatusType = beforeStTypeList.stream()
				.filter(stt -> stt.getBase().getId() == AppConstant.STATUS_TYPE_KIND_ID_PROCESSING_PUBLIC)
				.findFirst().orElse(null);
		long oldPublishingStatusTypeId = oldPublishingStatusType != null ? oldPublishingStatusType.getId() : 0;

		// 「ワークフロー公開処理」オブジェクトの名称を更新されたステータスタイプIDで更新
		if (oldPublishingStatusTypeId > 0) {
			// 更新後の「公開処理中」ステータスタイプを抽出
			StatusTypeDomain newPublishingStatusType = afterWorkflow.getStatusTypeList().stream()
					.filter(stt -> stt.getBase().getId() == AppConstant.STATUS_TYPE_KIND_ID_PROCESSING_PUBLIC)
					.findFirst().orElse(null);

			// 「ワークフロー公開処理」オブジェクトの名称を更新されたステータスタイプIDで更新
			if(newPublishingStatusType != null) {
				ObjectDomain publishingObject = objectService.getByTypeAndName(
						new ObjectTypeDomain(EIMConfig.get("OBJECT_TYPE_NAME_WFPUB")),
						String.valueOf(oldPublishingStatusTypeId));
				if (publishingObject != null) {
					objectService.updateName(publishingObject, String.valueOf(newPublishingStatusType.getId()), DuplicateCheckModeEnum.NONE);
				}
			}
		}
	}
	
	/**
	 * ワークフローの更新によって削除されたイベントタイプの属性やワークフロー公開処理設定に関係する属性を再設定する（文書管理WFのみ）
	 *
	 * @param WorkflowAdminDomain
	 * @param WorkflowDomain
	 * @return
	 * @throws Exception
	 */
	private void registConfigurationStatusInfoForAdmin(WorkflowDomain workflowDomain) throws Exception {

		WorkFlowDocDomain workflowDoc = workFlowDefDocService.getDefById(workflowDomain.getId());

		// 文書管理ワークフローのみが対象
		for (EventTypeDomain eventTypeDomain : workflowDomain.getEventTypeList()){
			// 承認依頼イベントの場合
			if(eventTypeDomain.getBase().getId() == AppConstant.BASE_EVENT_TYPE_ID_REQ_APPROVE){
				// 承認依頼通知タイミング
				AttributeTypeDomain attTypeApproveTiming = new AttributeTypeDomain(ConfigUtils.getByKey("ATTR_NAME_MAIL_NOTIFY_REQUEST_NOTIFY"));
				eventTypeService.addAttributeType(eventTypeDomain, attTypeApproveTiming);
				
				// 公開通知タイミング
				AttributeTypeDomain attTypePublicTiming = new AttributeTypeDomain(ConfigUtils.getByKey("ATTR_NAME_MAIL_NOTIFY_PUBLIC_TIMING"));
				eventTypeService.addAttributeType(eventTypeDomain, attTypePublicTiming);
				
				// 公開通知先
				AttributeTypeDomain attTypePublicTo = new AttributeTypeDomain(ConfigUtils.getByKey("ATTR_NAME_MAIL_NOTIFY_PUBLIC_NOTIFY_TO"));
				eventTypeService.addAttributeType(eventTypeDomain, attTypePublicTo);

				// 公開通知コメントログ
				AttributeTypeDomain attTypePublicCommentLog = new AttributeTypeDomain(ConfigUtils.getByKey("ATTR_NAME_PUBLIC_COMMENT_LOG"));
				eventTypeService.addAttributeType(eventTypeDomain, attTypePublicCommentLog);
				
				// 受信確認
				AttributeTypeDomain attTypeReply = new AttributeTypeDomain(ConfigUtils.getByKey("ATTR_NAME_MAIL_NOTIFY_CONFIRM_RECEIVE"));
				eventTypeService.addAttributeType(eventTypeDomain, attTypeReply);
				
			}
			// 承認イベントの場合
			if(eventTypeDomain.getBase().getId() == AppConstant.BASE_EVENT_TYPE_ID_APPROVAL){
				// 承認依頼通知タイミング
				AttributeTypeDomain attTypeApproveTiming = new AttributeTypeDomain(ConfigUtils.getByKey("ATTR_NAME_MAIL_NOTIFY_REQUEST_NOTIFY"));
				eventTypeService.addAttributeType(eventTypeDomain, attTypeApproveTiming);
				
				// 公開通知コメントログ
				AttributeTypeDomain attTypePublicCommentLog = new AttributeTypeDomain(ConfigUtils.getByKey("ATTR_NAME_PUBLIC_COMMENT_LOG"));
				eventTypeService.addAttributeType(eventTypeDomain, attTypePublicCommentLog);
				
			}
			// ※ 全てのイベントタイプにコメント属性タイプを紐付けているが、イベントタイプ｢公開｣では使用していません。
			AttributeTypeDomain applyAttributeType = new AttributeTypeDomain(ConfigUtils.getByKey("EVENT_ATTR_NAME_COMMENT"));
			eventTypeService.addAttributeType(eventTypeDomain, applyAttributeType);
		}
	}

	/**
     * @see jp.co.ctc_g.eim.admin.business.service.WorkFlowDefAdminService#copyDef(String,WorkflowAdminDomain)
	 * @since Ver6.0
	 */
	public WorkflowAdminDomain copyDef(String adminAppId, WorkflowAdminDomain workFlow) throws Exception {
		
		// 複写元ワークフローIDを保持
		long srcWorkflowId = workFlow.getId();
		
		// ワークフロー定義を複写
		// ※workFlow.idが複写したワークフローのidに書き換わる
		workFlowDefService.copyDef(workFlow);
		
		// 複写したワークフロー定義を取得
		// ※IDは分からないため定義名称で取得
		WorkflowDomain destWorkflow = 
			workflowService.getById(workFlow.getId());
		
		// 返却値
		// ※ワークフローIDのみ設定されていれば問題ない
		WorkflowAdminDomain workFlowAdmin = new WorkflowAdminDomain();
		workFlowAdmin.setId(destWorkflow.getId());
		
		// 複写元のワークフローを取得
		WorkflowDomain srcWorkflow = workflowService.getById(srcWorkflowId);
		
		// 複写元ステータスタイプの属性タイプ表示順情報格納用
		StatusTypeLayoutDomain srcStTypeLayout= new StatusTypeLayoutDomain();
		// 複写ステータスタイプの属性タイプ表示順情報格納用
		StatusTypeLayoutDomain destStTypeLayout= new StatusTypeLayoutDomain();
		
		// オブジェクトタイプ「ステータスタイプオブジェクト」を取得
		ObjectTypeDomain objTypeSttype = 
			objectTypeService.getByDefinitionName(EIMConfig.getValue("OBJECT_TYPE_NAME_STATUSTYPE"));
		
		// 属性タイプの適用状態を複写
		for (int i = 0; i < destWorkflow.getStatusTypeList().size(); i++) {
			
			// 複写元のステータスタイプを取得
			StatusTypeDomain srcStType = srcWorkflow.getStatusTypeList().get(i);
			// 複写したステータスタイプを取得
			StatusTypeDomain destStType = destWorkflow.getStatusTypeList().get(i);
			
			// 属性タイプを適用
			for (AttributeTypeDomain srcAttType : srcStType.getAttributeTypeList()) {
				
				statusTypeService.addAttributeType(destStType, srcAttType);
			}
			
			// オブジェクトタイプ「ステータスタイプオブジェクト」が存在しない場合
			if (objTypeSttype == null) {
				continue;
			}
			
			// 複写元ステータスタイプのステータスタイプオブジェクトを取得
			ObjectDomain sttypeObj = 
				objectService.getByTypeAndName(objTypeSttype, String.valueOf(srcStType.getId()));

			// 帳票管理の場合
			// and
			// 複写元ステータスタイプのステータスタイプオブジェクトが存在する(=表示順が設定)場合
			// →表示順を複写
			if (adminAppId.equals(AppConstant.ADMIN_APP_ID_FORM)
					&& sttypeObj != null ) {
				
				// 複写元のステータスタイプIDを設定
				srcStTypeLayout.setId(srcStType.getId());
				// 複写したステータスタイプIDを設定
				destStTypeLayout.setId(destStType.getId());
				
				// 属性タイプの表示順を複写
				statusTypeLayoutService.copyStatusTypeObject(srcStTypeLayout, destStTypeLayout);
			}
		}
		
		// 帳票管理の場合は全てのイベントタイプに「コメント」属性・「追加のメール通知先」属性・「通知方法」属性を適用
		if (adminAppId.equals(AppConstant.ADMIN_APP_ID_FORM)) {
			addEventAttributeType(destWorkflow.getEventTypeList());
		}
		
		// タスク管理の場合は全てのイベントタイプに「コメント」属性・「追加のメール通知先」属性・「通知方法」属性を適用
		if (adminAppId.equals(AppConstant.ADMIN_APP_ID_TASK)) {
			addEventAttributeTypeForTask(destWorkflow.getEventTypeList());
		}
		
		return workFlowAdmin;
	}
	
	//「コメント」属性・「追加のメール通知先」属性・「通知方法」属性を適用するメソッド
	private void addEventAttributeType(List<EventTypeDomain> eventTypeList) throws Exception {
		AttributeTypeDomain commentAttrType = new AttributeTypeDomain(ConfigUtils.getByKey("ATTRIBUTE_TYPE_NAME_COMMENT"));
		AttributeTypeDomain addmailNotificationAttrType = new AttributeTypeDomain(ConfigUtils.getByKey("ATTRIBUTE_TYPE_NAME_ADDMAIL_NOTIFICATION"));
		AttributeTypeDomain notificationMethodAttrType = new AttributeTypeDomain(ConfigUtils.getByKey("ATTRIBUTE_TYPE_NAME_NOTIFICATION_METHOD"));

		for (EventTypeDomain eventType : eventTypeList){
			eventTypeService.addAttributeType(eventType, commentAttrType);
			eventTypeService.addAttributeType(eventType, addmailNotificationAttrType);
			eventTypeService.addAttributeType(eventType, notificationMethodAttrType);
		}
	}
	
	// 【タスク管理用】「コメント」属性・「追加のメール通知先」属性・「通知方法」属性を適用するメソッド
	private void addEventAttributeTypeForTask(List<EventTypeDomain> eventTypeList) throws Exception {
		AttributeTypeDomain commentAttrType = new AttributeTypeDomain(ConfigUtils.getByKey("ATTRIBUTE_TYPE_NAME_TASK_COMMENT"));
		AttributeTypeDomain addmailNotificationAttrType = new AttributeTypeDomain(ConfigUtils.getByKey("ATTRIBUTE_TYPE_NAME_TASK_ADDMAIL_NOTIFICATION"));
		AttributeTypeDomain notificationMethodAttrType = new AttributeTypeDomain(ConfigUtils.getByKey("ATTRIBUTE_TYPE_NAME_TASK_NOTIFICATION_METHOD"));

		for (EventTypeDomain eventType : eventTypeList){
			eventTypeService.addAttributeType(eventType, commentAttrType);
			eventTypeService.addAttributeType(eventType, addmailNotificationAttrType);
			eventTypeService.addAttributeType(eventType, notificationMethodAttrType);
		}
	}
	
	/**
	 * 
	 * @param afterStatusTypeList	変更後のワークフローのステータスタイプリスト
	 * @param beforeStatusTypeList	変更前のワークフローのステータスタイプリスト
	 */
	private List<Long> getStatusTypeIds(
			List<jp.co.ctc_g.eim.framework.business.domain.entity.StatusTypeDomain> afterStatusTypeList,
			List<StatusTypeDomain> beforeStatusTypeList,
			Map<Integer, Long> beforeAfterMap) throws Exception{
		
		List<Long> afterStIds = new ArrayList<Long>();
		int sequence = 0;
		for(jp.co.ctc_g.eim.framework.business.domain.entity.StatusTypeDomain afterStType:afterStatusTypeList) {
			for(StatusTypeDomain beforeStType:beforeStatusTypeList) {
				if(afterStType.getId()==beforeStType.getId()) {
					afterStIds.add(beforeStType.getId());
					beforeAfterMap.put(sequence, beforeStType.getId());
					break;
				}
			}
			sequence++;
		}
		return afterStIds;
	}

	/**
	 * 新しいステータスタイプとステータス別セキュリティリストのマップを取得する。(FW2使用)
	 *
	 * @param afterStatusTypeList	変更後のワークフローのステータスタイプリスト
	 * @param beforeStatusTypeList	変更前のワークフローのステータスタイプリスト
	 * @return Map<EIMSecurity, List<StatusSecurityDomain>>
	 */
	private Map<SecurityDomain, List<StatusSecurityDomain>> getStatusSecurity(List<Long> stIds) throws Exception {

		// key:セキュリティ、value:ステータス別セキュリティリスト
		Map<SecurityDomain, List<StatusSecurityDomain>> stSecListMap = new HashMap<SecurityDomain, List<StatusSecurityDomain>>();
		
		// セキュリティごとにステータスタイプリストを取得する。
		List<SecurityDomain> securityList = securityService.getList();
		for(SecurityDomain sec: securityList) {

			// 全ステータス別セキュリティから、対象のステータスタイプのみ抽出する。
			List<StatusSecurityDomain> allStatusSecurityList = sec.getStatusSecurityList();
			List<StatusSecurityDomain> statusSecurityList = new ArrayList<StatusSecurityDomain>();
			
			for(StatusSecurityDomain statusSecurity : allStatusSecurityList) {
				if(stIds.contains(statusSecurity.getStatusType().getId())) {
					statusSecurityList.add(statusSecurity);
				}
			}
			stSecListMap.put(sec, statusSecurityList);
			
		}
		return stSecListMap;
	}
	
	/**
	 * ステータス別セキュリティを設定する。
	 * 
	 * @param stSecListMap ステータスタイプ順序とステータス別セキュリティのマップ
	 * @return なし
	 */
	private void setStatusSecurity(Map<SecurityDomain, List<StatusSecurityDomain>> stSecListMap, Map<Long, Long> stTypeIdMap) throws Exception {

		for(SecurityDomain security :stSecListMap.keySet()) {
			List<StatusSecurityDomain> stSecList = stSecListMap.get(security);
			// stSecListは更新前のステータスタイプを保持しているため、
			// 更新後のステータスタイプに変更してステータスセキュリティを作成する。
			for(StatusSecurityDomain statusSecurity : stSecList) {
				
				StatusTypeDomain newStatusTypeDomain = new StatusTypeDomain(stTypeIdMap.get(statusSecurity.getStatusType().getId()));
				StatusSecurityDomain newStatusSecurity = new StatusSecurityDomain(statusSecurity.getId());
				newStatusSecurity.setAccessEntryList(statusSecurity.getAccessEntryList());
				newStatusSecurity.setStatusType(newStatusTypeDomain);
				
				securityService.createStatusSecurity(security, newStatusSecurity);
			}
		}
	}
	
	/**
	 * ワークフローService(framework)を取得します。
	 * @return ワークフローService(framework)
	 * @since Ver6.0
	 */
	public WorkFlowDefService getWorkFlowDefService() {
		return workFlowDefService;
	}

	/**
	 * ワークフローService(framework)を設定します。
	 * @param workflowService ワークフローService(framework)
	 * @since Ver6.0
	 */
	public void setWorkFlowDefService(WorkFlowDefService workFlowDefService) {
		this.workFlowDefService = workFlowDefService;
	}

	/**
	 * ワークフローService(framework2)を取得します。
	 * @return ワークフローService(framework2)
	 * @since Ver6.0
	 */
	public WorkflowService getWorkflowService() {
		return workflowService;
	}

	/**
	 * ワークフローService(framework2)を設定します。
	 * @param workflowService ワークフローService(framework2)
	 * @since Ver6.0
	 */
	public void setWorkflowService(WorkflowService workflowService) {
		this.workflowService = workflowService;
	}

	/**
	 * ワークフローService(ドキュメント管理)を取得します。
	 * @return ワークフローService(ドキュメント管理)
	 * @since Ver6.0
	 */
	public WorkFlowDefDocService getWorkflowDefDocService() {
		return workFlowDefDocService;
	}

	/**
	 * ワークフローService(ドキュメント管理)を設定します。
	 * @param workflowService ワークフローService(ドキュメント管理)
	 * @since Ver6.0
	 */
	public void setWorkFlowDefDocService(WorkFlowDefDocService workFlowDefDocService) {
		this.workFlowDefDocService = workFlowDefDocService;
	}

	/**
	 * ステータスタイプServiceを取得します。
	 * @return ステータスタイプService
	 * @since Ver6.0
	 */
	public StatusTypeService getStatusTypeService() {
		return statusTypeService;
	}

	/**
	 * ステータスタイプServiceを設定します。
	 * @param statusLayoutService ステータスタイプService
	 * @since Ver6.0
	 */
	public void setStatusTypeService(StatusTypeService statusTypeService) {
		this.statusTypeService = statusTypeService;
	}

	/**
	 * ステータスタイプ(レイアウト情報)Serviceを取得します。
	 * @return ステータスタイプ(レイアウト情報)Service
	 * @since Ver6.0
	 */
	public StatusTypeLayoutService getStatusTypeLayoutService() {
		return statusTypeLayoutService;
	}

	/**
	 * ステータスタイプ(レイアウト情報)Serviceを設定します。
	 * @param statusLayoutService ステータスタイプ(レイアウト情報)Service
	 * @since Ver6.0
	 */
	public void setStatusTypeLayoutService(StatusTypeLayoutService statusTypeLayoutService) {
		this.statusTypeLayoutService = statusTypeLayoutService;
	}

	/**
	 * イベントタイプServiceを取得します。
	 * @return イベントタイプService
	 * @since Ver6.0
	 */
	public EventTypeService getEventTypeService() {
		return eventTypeService;
	}

	/**
	 * イベントタイプServiceを設定します。
	 * @param eventTypeService イベントタイプService
	 * @since Ver6.0
	 */
	public void setEventTypeService(EventTypeService eventTypeService) {
		this.eventTypeService = eventTypeService;
	}

	/**
	 * 属性タイプ(レイアウト情報)Serviceを取得します。
	 * @return 属性タイプ(レイアウト情報)Service
	 * @since Ver6.0
	 */
	public AttributeTypeLayoutService getAttributeTypeLayoutService() {
		return attributeTypeLayoutService;
	}

	/**
	 * 属性タイプ(レイアウト情報)Serviceを設定します。
	 * @param attributeLayoutService 属性タイプ(レイアウト情報)Service
	 * @since Ver6.0
	 */
	public void setAttributeTypeLayoutService(
			AttributeTypeLayoutService attributeTypeLayoutService) {
		this.attributeTypeLayoutService = attributeTypeLayoutService;
	}

	/**
	 * オブジェクトタイプServiceを取得します。
	 * @return オブジェクトタイプService
	 * @since Ver6.0
	 */
	public ObjectTypeService getObjectTypeService() {
		return objectTypeService;
	}

	/**
	 * オブジェクトタイプServiceを設定します。
	 * @param objectTypeService オブジェクトタイプService
	 * @since Ver6.0
	 */
	public void setObjectTypeService(ObjectTypeService objectTypeService) {
		this.objectTypeService = objectTypeService;
	}

	/**
	 * オブジェクトServiceを取得します。
	 * @return オブジェクトService
	 * @since Ver6.0
	 */
	public ObjectService getObjectService() {
		return objectService;
	}

	/**
	 * オブジェクトServiceを設定します。
	 * @param objectService オブジェクトService
	 * @since Ver6.0
	 */
	public void setObjectService(ObjectService objectService) {
		this.objectService = objectService;
	}

	/**
	 * セキュリティServiceを取得します。
	 * @return セキュリティService
	 * @since Ver6.21
	 */
	public SecurityService getSecurityService() {
		return securityService;
	}

	/**
	 * セキュリティServiceを設定します。
	 * @param securityService セキュリティService
	 * @since Ver6.21
	 */
	public void setSecurityService(SecurityService securityService) {
		this.securityService = securityService;
	}
}