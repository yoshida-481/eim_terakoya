package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;

import addon.PublishCommandAddOn;
import common.util.AdminAuthUtil;
import common.util.AppConstant;
import common.util.AppObjectUtil;
import common.util.AppSecurityUtils;
import common.util.AppUpdateNoticeUtils;
import common.util.OptionConfData;
import common.util.PublishAddonUtils;
import eim.bo.EIMAccessEntryType;
import eim.bo.EIMAttribute;
import eim.bo.EIMAttributeType;
import eim.bo.EIMComp;
import eim.bo.EIMEventType;
import eim.bo.EIMGroup;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMOtherName;
import eim.bo.EIMRole;
import eim.bo.EIMStatusType;
import eim.bo.EIMUser;
import eim.bo.EIMWorkFlow;
import eim.net.EIMSession;
import eim.util.AttributeUtils;
import eim.util.CompUtils;
import eim.util.EIMConfig;
import eim.util.EIMConstant;
import eim.util.EventAttributeUtils;
import eim.util.GroupUtils;
import eim.util.ObjectAttributeUtils;
import eim.util.ObjectUtils;
import eim.util.OperationHistoryUtils;
import eim.util.RoleUtils;
import eim.util.StringUtils;
import eim.util.TypeConvertUtils;
import eim.util.UserUtils;
import eim.util.WorkFlowUtils;
import jp.co.ctc_g.eim.app.document.business.domain.WorkFlowDocDomain;
import jp.co.ctc_g.eim.app.document.business.domain.WorkFlowDocPDFSignatureDomain;
import jp.co.ctc_g.eim.app.document.business.service.WorkFlowDefDocService;
import jp.co.ctc_g.eim.framework.business.dao.AssignEntryDao;
import jp.co.ctc_g.eim.framework.business.dao.EventTypeDao;
import jp.co.ctc_g.eim.framework.business.dao.StatusTypeDao;
import jp.co.ctc_g.eim.framework.business.domain.AssignEntryDomain;
import jp.co.ctc_g.eim.framework.business.domain.BelongDomain;
import jp.co.ctc_g.eim.framework.business.domain.BelongType;
import jp.co.ctc_g.eim.framework.business.domain.entity.EventTypeDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.OtherNameDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.StatusTypeDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.WorkFlowDomain;
import jp.co.ctc_g.eim.framework.business.service.impl.WorkFlowDefServiceImpl;
import jp.co.ctc_g.eim.framework.common.exception.EIMAppException;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.business.dao.ObjectDao;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.WorkflowCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.WorkflowDomain;
import jp.co.ctc_g.eim.framework2.business.service.WorkflowService;
import jp.co.ctc_g.eim.framework2.common.enumeration.DuplicateCheckModeEnum;

/*************************************************************************************************************************
 *
 * ワークフロー定義(ドキュメント用)サービスクラス
 *
 *************************************************************************************************************************/
public class WorkFlowDefDocServiceImpl extends WorkFlowDefServiceImpl implements WorkFlowDefDocService
{

	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//	parameter
	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//	Service(メイン)
	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	/**
	 * ワークフロー定義　取得
	 *
	 */
	public WorkFlowDocDomain getDefById(long workFlowId) throws Exception
	{
		if(!AdminAuthUtil.hasSpecifiedOrGeneralAuth(EIMThreadContext.getEIMSession().getUser(), AppConstant.ADMIN_AUTH_ID_WORKFLOW)){
			throw new EIMAppException("EIM.ERROR.NOTADMINISTRATOR");
		}
		//===================================================
		// ワークロー情報(アプリ専用機能)の取得
		//===================================================
		EIMWorkFlow workFlow = WorkFlowUtils.getWorkFlowById(EIMThreadContext.getEIMSession(), workFlowId);
		if(workFlow == null){
			return null;
		}
		WorkFlowDocDomain workFlowDocDomain = new WorkFlowDocDomain(workFlow);

		List<EIMOtherName> otherList = WorkFlowUtils.getOtherWorkFlowNameList(EIMThreadContext.getEIMSession(), workFlowId);
		workFlowDocDomain.setOtherList(otherList);

		//===================================================
		// ステータス構成の取得(FW機能)
		//===================================================
		WorkFlowDomain workFlowDomain = super.getDefById(workFlowId);
		workFlowDocDomain.setWorkFlowDomain(workFlowDomain);
		
		List<EventTypeDomain> eventTypeList = workFlowDocDomain.getEventTypeList();
		// イベントタイプID順にソートする
		List<EventTypeDomain> sortedEventTypeList = AppObjectUtil.getIntSortedList(eventTypeList, "getId", true);
		workFlowDocDomain.setEventTypeList(sortedEventTypeList);

		//===================================================
		// ワークフロー 全般(アプリ専用機能)の取得
		//===================================================
		EIMObject workFlowSettingObj = AppObjectUtil.getObject(EIMThreadContext.getEIMSession(), EIMConfig.get("OBJECT_TYPE_NAME_WFSETTING"), String.valueOf(workFlowId));

		if(workFlowSettingObj != null) {
			// ワークフロー設定オブジェクト情報をドメインに格納
			workFlowDocDomain.setWorkFlowSetting(workFlowSettingObj);
		}

		//===================================================
		// 公開処理設定情報の取得
		//===================================================
		StatusTypeDomain statusTypeDomain = null;
		for(StatusTypeDomain stDomain:workFlowDocDomain.getStatusTypeList()) {
			if(stDomain.getStatusTypeKind().getId() == AppConstant.STATUS_TYPE_KIND_ID_PROCESSING_PUBLIC) {
				statusTypeDomain = stDomain;
				break;
			}
		}
		// 公開処理設定情報
		if (statusTypeDomain != null) {
			EIMObject wfpubObj = AppObjectUtil.getObject(EIMThreadContext.getEIMSession(), EIMConfig.get("OBJECT_TYPE_NAME_WFPUB"), String.valueOf(statusTypeDomain.getId()));

			Map<String, Map<String, String>> map = new HashMap<String, Map<String, String>>();
			List classList = PublishAddonUtils.getAddonClasses(EIMThreadContext.getEIMSession());		// アドオンクラスのインスタンスリストを取得
			for(int i = 0; i < classList.size() ; i++){
				PublishCommandAddOn addon = (PublishCommandAddOn)classList.get(i);
				//アドオンオプションが無効の場合、無視するように変更　by lin.chen at 2010/03/18
				Map<String, Map<String, String>> addOnSettingMap = addon.getPublishCommandSettingList(EIMThreadContext.getEIMSession(), wfpubObj);
				if(addOnSettingMap == null || addOnSettingMap.isEmpty()) continue;
				map.putAll(addOnSettingMap);	// 各アドオンクラスの公開処理設定情報を取得
			}

			Map<String, String> paramMap = new HashMap<String, String>();									// 公開処理設定情報の格納領域
			Iterator<String> iter = map.keySet().iterator();
			while (iter.hasNext()) {
				String key = (String) iter.next();
				paramMap.putAll(map.get(key));
			}
			
			//公開処理設定のPDF変換部分の情報を追加する
			
			workFlowDocDomain.setDoPDFConvert(Boolean.valueOf(paramMap.get("doConvertToPDF")));
			workFlowDocDomain.setDoPDFURL(Boolean.valueOf(paramMap.get("doinsertURLPDF")));
			workFlowDocDomain.setDoSetTerm(Boolean.valueOf(paramMap.get("enable")));
		
			if(workFlowDocDomain.isDoSetTerm() == true){
				workFlowDocDomain.setTermNumParam(Integer.parseInt(paramMap.get("term")));
				workFlowDocDomain.setTermUnitParam(paramMap.get("termUnit"));
			}

			// 電子署名／セキュリティ設定
			WorkFlowDocPDFSignatureDomain pdfSignatureDomain = new WorkFlowDocPDFSignatureDomain(
					paramMap.get("doSignPDFAndSetSecurity"),
					paramMap.get("doSignPDF"),
					paramMap.get("insertApproveDate"),
					paramMap.get("insertApproveUser"),
					paramMap.get("insertPage"),
					paramMap.get("insertPlace"),
					paramMap.get("insertPlaceX"),
					paramMap.get("insertPlaceY"),
					paramMap.get("approveNamelang"),
					paramMap.get("signJobName"),
					paramMap.get("doSetSecurity"),
					paramMap.get("doSetSecurityPassword"),
					paramMap.get("securityPassword"),
					paramMap.get("doSetReferencePassword"),
					paramMap.get("referencePassword"),
					paramMap.get("forbidPrint"),
					paramMap.get("forbidEdit"),
					paramMap.get("forbidAnnotate"),
					paramMap.get("forbidReproduce")
			);
			workFlowDocDomain.setSignatureCommand(pdfSignatureDomain);
		}

		//===================================================
		// デフォルト公開通知先参照情報の取得
		//===================================================
		workFlowDocDomain.setPublishNotyfyAssignEntryDomain(getPublishNotify(workFlowDocDomain));
		
		return workFlowDocDomain;
	}

	/**
	 * ワークフロー定義　新規作成
	 *
	 */
	public void create(WorkFlowDocDomain workFlowDocDomain) throws Exception {
		if(!AdminAuthUtil.hasSpecifiedOrGeneralAuth(EIMThreadContext.getEIMSession().getUser(), AppConstant.ADMIN_AUTH_ID_WORKFLOW)){
			throw new EIMAppException("EIM.ERROR.NOTADMINISTRATOR");
		}

		//===================================================
		// ワークフロー 作成
		//===================================================
		EIMWorkFlow workFlow = createWorkFlow(workFlowDocDomain);

		//===================================================
		// ステータス構成 登録
		//===================================================
		createConfigurationStatusInfo(workFlowDocDomain, workFlow);
		
		//===================================================
		// 公開通知先参照 更新
		//===================================================
		updatePublishNotify(workFlowDocDomain, workFlow);
		
		//===================================================
		// 複写の場合 属性:チェックイン可能ステータス/上長のみ表示設定 更新
		//===================================================
		if( workFlowDocDomain.isCreateCopy() ) {
			
			// 複写元のワークフロー情報を取得
			WorkFlowDomain srcWorkFlow = super.getDefById(workFlowDocDomain.getOriginWorkflowId());
			WorkFlowDocDomain srcWorkFlowDocDomain = new WorkFlowDocDomain(srcWorkFlow);
			
			EIMObject destWorkFlowSettingObj = AppObjectUtil.getObject(EIMThreadContext.getEIMSession(),
					EIMConfig.get("OBJECT_TYPE_NAME_WFSETTING"),
					String.valueOf(workFlow.getId()));
			EIMObject srcWorkFlowSettingObj = AppObjectUtil.getObject(EIMThreadContext.getEIMSession(),
					EIMConfig.get("OBJECT_TYPE_NAME_WFSETTING"),
					String.valueOf(srcWorkFlowDocDomain.getId()));
			
			// ワークフロー設定情報をドメインにセット
			srcWorkFlowDocDomain.setWorkFlowSetting(srcWorkFlowSettingObj);
			
			// ワークフロー設定情報(ステータス毎設定)をコピー
			copyWorkFlowSettingForStatusType(srcWorkFlowDocDomain, workFlowDocDomain);
			
			// ワークフロー設定情報(ステータス毎設定)を更新
			updateWorkFlowSettingForStatusType(workFlowDocDomain, destWorkFlowSettingObj);
		}
	}

	/**
	 * ワークフロー定義　更新
	 *
	 */
	public void updateWorkFlowDef(WorkFlowDocDomain workFlowDocDomain) throws Exception {
		if(!AdminAuthUtil.hasSpecifiedOrGeneralAuth(EIMThreadContext.getEIMSession().getUser(), AppConstant.ADMIN_AUTH_ID_WORKFLOW)){
			throw new EIMAppException("EIM.ERROR.NOTADMINISTRATOR");
		}

		if(workFlowDocDomain == null || WorkFlowUtils.getWorkFlowById(EIMThreadContext.getEIMSession(), workFlowDocDomain.getId()) == null){
			throw new EIMAppException("EIM.ERROR.LOGIC.WORKFLOW.NOTFOUND");
		}
		//===================================================
		// ワークフロー(全般) 更新
		//===================================================
		EIMWorkFlow workFlow = updateWorkFlow(workFlowDocDomain);

		//===================================================
		// ワークフロー(公開処理設定) 更新
		//===================================================
		setPublishCommand(workFlowDocDomain, workFlow);

		//===================================================
		// ワークフロー(公開通知先参照) 更新
		//===================================================
		updatePublishNotify(workFlowDocDomain, workFlow);
		
		// SearchFramework 検索FW更新通知 対象：ワークフロー
		AppUpdateNoticeUtils.updateNoticeInsert(workFlow.getId(), "SEARCHFW_WORKFLOW_EDIT_WORKFLOW");

	}

	/**
	 * ステータス構成　更新
	 *
	 */
	public void updateConfigurationStatus(WorkFlowDocDomain workFlowDocDomain) throws Exception {
		if(!AdminAuthUtil.hasSpecifiedOrGeneralAuth(EIMThreadContext.getEIMSession().getUser(), AppConstant.ADMIN_AUTH_ID_WORKFLOW)){
			throw new EIMAppException("EIM.ERROR.NOTADMINISTRATOR");
		}
		if(workFlowDocDomain == null || WorkFlowUtils.getWorkFlowById(EIMThreadContext.getEIMSession(), workFlowDocDomain.getId()) == null){
			throw new EIMAppException("EIM.ERROR.LOGIC.WORKFLOW.NOTFOUND");
		}

		//===================================================
		// ワークロー情報の取得
		//===================================================
		EIMWorkFlow workFlow = WorkFlowUtils.getWorkFlowById(EIMThreadContext.getEIMSession(), workFlowDocDomain.getId());

		//===================================================
		// ステータス構成 登録
		//===================================================
		updateConfigurationStatusInfo(workFlowDocDomain, workFlow);
		
		EIMObject workFlowSettingObj = AppObjectUtil.getObject(
				  EIMThreadContext.getEIMSession()
				, EIMConfig.get("OBJECT_TYPE_NAME_WFSETTING")
				, String.valueOf(workFlowDocDomain.getOriginWorkflowId())
				);
		// ワークフロー設定情報(ステータス毎設定)を更新
		updateWorkFlowSettingForStatusType(workFlowDocDomain, workFlowSettingObj);
	}

	/**
	 * ステータス構成　削除
	 *
	 */
	public void delete(WorkFlowDocDomain workFlowDocDomain) throws Exception {
		if(!AdminAuthUtil.hasSpecifiedOrGeneralAuth(EIMThreadContext.getEIMSession().getUser(), AppConstant.ADMIN_AUTH_ID_WORKFLOW)){
			throw new EIMAppException("EIM.ERROR.NOTADMINISTRATOR");
		}
		
		EIMSession sess = EIMThreadContext.getEIMSession();
		//ワークフローオブジェクトを取得する
		EIMWorkFlow workFlow = WorkFlowUtils.getWorkFlowById(EIMThreadContext.getEIMSession(), workFlowDocDomain.getId());
		
		if(workFlowDocDomain == null || workFlow == null){
			throw new EIMAppException("EIM.ERROR.LOGIC.WORKFLOW.NOTFOUND");
		}

		List statusTypeList = workFlow.getStatusTypeList();
		for(int i = 0; i < statusTypeList.size(); i++)
		{
			//Status Type
			EIMStatusType statusType = (EIMStatusType)statusTypeList.get(i);
			
			//Create Operation History
			OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.DELETE_STATUS, 
					EIMConstant.TARGET_PARENT_WORKFLOW, EIMConstant.WORKFLOW, workFlow,
					 EIMConstant.TARGET_DELETE, EIMConstant.STATUS_TYPE, statusType, null);
			
			// 公開処理中ステータス削除の場合
			if(statusType.getKind() == AppConstant.STATUS_TYPE_KIND_ID_PROCESSING_PUBLIC){
				// 公開処理オブジェクトを削除
				AppObjectUtil.deleteObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_WFPUB"), String.valueOf(statusType.getId()));
			}

			//Delete
			WorkFlowUtils.deleteStatusType(sess, statusType);
		}
		
		//Create Operation History
		OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.DELETE_WORKFLOW, 
				EIMConstant.TARGET_DELETE, EIMConstant.WORKFLOW, workFlow,
				null, null, null, null);

		//Delete
		WorkFlowUtils.deleteWorkFlow(sess, workFlow);
		
		AppObjectUtil.deleteObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_WFSETTING"), String.valueOf(workFlow.getId()));
		
		// 公開通知先エントリーオブジェクトを削除
		AppObjectUtil.deleteObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_WFPUBNOTIFY"), String.valueOf(workFlow.getId()));
		
	}

	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//	private
	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	/**
	 * ワークフロー新規作成
	 *
	 * ワークフローを作成する（イベントとステータス付いていません）
	 * @param workFlowDocDomain
	 * @return
	 * @throws Exception
	 */
	private EIMWorkFlow createWorkFlow(WorkFlowDocDomain workFlowDocDomain) throws Exception {

		EIMSession sess = EIMThreadContext.getEIMSession();
		EIMWorkFlow workFlow = null;

		// 複写の場合、以下の判定を行う
		if (workFlowDocDomain.isCreateCopy())
		{
			// ワークフロー設定オブジェクトを取得
			EIMObject workFlowSettingObj = AppObjectUtil.getObject(
											  EIMThreadContext.getEIMSession()
											, EIMConfig.get("OBJECT_TYPE_NAME_WFSETTING")
											, String.valueOf(workFlowDocDomain.getOriginWorkflowId())
											);
			// 未設定の場合(汎用で作られたワークフローをコピー)エラーとする
			if(workFlowSettingObj == null)
			{
				throw new EIMAppException("EIM.ERROR.INPUT.NOTDOCUMENTWORKFLOW");
			}

			// セッションの回復(2つ目のサービスのため)
			sess = EIMThreadContext.getEIMSession();
			sess.setConnection(null);
		}

		/*
		 * Create WorkFlow
		 */
		workFlow = WorkFlowUtils.createWorkFlow(sess, workFlowDocDomain.getDefName());

		/*
		 * Create WorkFlow Other
		 */
		for(OtherNameDomain otherName:workFlowDocDomain.getNameList())
		{
			String prmOtherName = otherName.getName();
			String prmOtherLId = otherName.getLangId();

			// Create Object Type
			WorkFlowUtils.addOtherWorkFlowName(sess, workFlow.getId(), prmOtherLId, prmOtherName);
		}

		/*
		 * Update Work Flow Setting Object
		 */
		EIMObject workFlowSettingObj = AppObjectUtil.createObject(sess,
																EIMConfig.get("OBJECT_TYPE_NAME_WFSETTING"),
																String.valueOf(workFlow.getId()));

		updateWorkFlowSetting(sess, workFlowDocDomain, workFlowSettingObj);

		/*
		 * Create Operation History
		 */
		// 操作タイプ設定
		String opeType;
		String opeTarget;
		if (!workFlowDocDomain.isCreateCopy())
		{
			// 新規作成の場合
			opeType = EIMConstant.CREATE_WORKFLOW;
			opeTarget = EIMConstant.TARGET_CREATE;
		}
		else
		{
			// 複写の場合
			opeType = EIMConstant.COPY_WORKFLOW;
			opeTarget = EIMConstant.TARGET_COPY;
		}
		OperationHistoryUtils.create(sess,
							AppConstant.SYSTEM,
							opeType,
							opeTarget,
							EIMConstant.WORKFLOW,
							workFlow,
							null,
							null,
							null,
							null);

		return workFlow;

	}

	/**
	 * ワークフロー更新
	 *
	 * ワークフローを更新する（イベントとステータス付いていません）
	 * @param workFlowDocDomain
	 * @return
	 * @throws Exception
	 */
	private EIMWorkFlow updateWorkFlow(WorkFlowDocDomain workFlowDocDomain) throws Exception {

		EIMSession sess = EIMThreadContext.getEIMSession();
		EIMWorkFlow workFlow = null;

		//=====================================================================
		// ワークフロー設定オブジェクトの存在チェックを行う
		//   ※「汎用」で作成されたワークフローを編集しようとするとエラーが発生するため、そのハンドリング
		//=====================================================================
		// ワークフロー設定オブジェクトを取得
		EIMObject workFlowSettingObjCheck = AppObjectUtil.getObject(
										  EIMThreadContext.getEIMSession()
										, EIMConfig.get("OBJECT_TYPE_NAME_WFSETTING")
										, String.valueOf(workFlowDocDomain.getId())
										);
		// 未設定の場合(汎用で作られたワークフローをコピー)エラーとする
		if(workFlowSettingObjCheck == null)
		{
			throw new EIMAppException("EIM.ERROR.INPUT.NOTDOCUMENTWORKFLOW");
		}

		// セッションの回復(2つ目のサービスのため)
		sess = EIMThreadContext.getEIMSession();
		sess.setConnection(null);

		/*
		 * Update WorkFlow
		 */
		workFlow = WorkFlowUtils.updateWorkFlow(sess, workFlowDocDomain.createEIMWorkFlow(), workFlowDocDomain.getDefName());

		/*
		 * Update WorkFlow Other
		 */
		for(OtherNameDomain otherName:workFlowDocDomain.getNameList())
		{
			String prmOtherName = otherName.getName();
			String prmOtherLId = otherName.getLangId();

			// Update Object Type
			WorkFlowUtils.updateOtherWorkFlowName(sess, workFlow.getId(), prmOtherLId, prmOtherName);
		}

		/*
		 * Update Work Flow Setting Object
		 */
		EIMObject workFlowSettingObj = AppObjectUtil.getObject(sess,
																EIMConfig.get("OBJECT_TYPE_NAME_WFSETTING"),
																String.valueOf(workFlow.getId()));

		updateWorkFlowSetting(sess, workFlowDocDomain, workFlowSettingObj);

		/*
		 * Create Operation History
		 */
		OperationHistoryUtils.create(sess,
							AppConstant.SYSTEM,
							EIMConstant.UPDATE_WORKFLOW,
							EIMConstant.TARGET_UPDATE,
							EIMConstant.WORKFLOW,
							workFlow,
							null,
							null,
							null,
							null);

		return workFlow;

	}

	/**
	 * ワークフロー設定オブジェクト属性更新
	 *
	 * @param sess
	 * @param workFlow
	 * @throws Exception
	 */
	private void updateWorkFlowSetting(EIMSession sess, WorkFlowDocDomain workFlowDocDomain, EIMObject workFlowSettingObj) throws Exception {

		// 上長承認設定フラグ
		AppObjectUtil.setAttr(sess,
								workFlowSettingObj,
								EIMConfig.get("ATTR_NAME_WFSETTING_BOSS_APPROVAL_FLG"),
								workFlowDocDomain.getDefBossApproval());
		
		// メール通知方法のデフォルト設定
		AppObjectUtil.setAttr(sess,
								workFlowSettingObj,
								EIMConfig.get("ATTR_NAME_WFSETTING_MAIL_NOTICE_DFLT"),
								workFlowDocDomain.getDefNotifyMail());

		// 承認依頼先デフォルト設定フラグ
		AppObjectUtil.setAttr(sess,
								workFlowSettingObj,
								EIMConfig.get("ATTR_NAME_WFSETTING_SETTING_FLG"),
								workFlowDocDomain.getDefApproveRequest());

		// 処理待ちポップアップ通知フラグ
		AppObjectUtil.setAttr(sess,
								workFlowSettingObj,
								EIMConfig.get("ATTR_NAME_WFSETTING_POPUP_NOTICE_FLG"),
								workFlowDocDomain.getProcessWaitPopup());

		// 差戻し・取戻しメール通知フラグ
		AppObjectUtil.setAttr(sess,
								workFlowSettingObj,
								EIMConfig.get("ATTR_NAME_WFSETTING_MAIL_NOTICE_FLG"),
								workFlowDocDomain.getBackMail());

		// 公開通知先デフォルト設定フラグ
		AppObjectUtil.setAttr(sess,
								workFlowSettingObj,
								EIMConfig.get("ATTR_NAME_WFSETTING_PUBLISHNOTIFY_FLG"),
								workFlowDocDomain.getPublishNotifyMail());
		
		// OCR処理のデフォルト設定
		AppObjectUtil.setAttr(sess,
								workFlowSettingObj,
								EIMConfig.get("ATTR_NAME_OCR_SETTING_EXISTENCE"),
								workFlowDocDomain.getDefOcr());
		
		
	}
	
	/**
	 * ワークフロー設定を更新（ステータスタイプ毎に設定されている内容）
	 * ・承認依頼中チェックイン可否（オプションがONの場合のみ処理）
	 * ・上長のみ表示デフォルト設定
	 * @param workFlowDocDomain ワークフロー
	 * @param workFlowSettingObj ワークフロー設定オブジェクト
	 * @throws Exception
	 */
	private void updateWorkFlowSettingForStatusType(WorkFlowDocDomain workFlowDocDomain, EIMObject workFlowSettingObj) throws Exception {
		EIMSession sess = EIMThreadContext.getEIMSession();
		// 承認依頼中チェックイン可否
		if (OptionConfData.getInstance().enableApproverCheckin && workFlowDocDomain.getEnableCheckinStatusTypeArr() != null) {
			// チェックイン可能ステータスタイプ
			AppObjectUtil.setAttr(sess, 
					workFlowSettingObj, 
					EIMConfig.get("ATTR_NAME_ENABLE_CHECKIN_STATUS"), 
					workFlowDocDomain.getEnableCheckinStatusTypeArr());
		}
		
		// 上長のみ表示デフォルト設定
		if(workFlowDocDomain.getDefBossOnlyStatusTypeArr() != null) {
			// 上長のみ表示デフォルト設定
			AppObjectUtil.setAttr(sess, 
					workFlowSettingObj, 
					EIMConfig.get("ATTR_NAME_BOSS_ONLY_DEFAULT_STATUS"), 
					workFlowDocDomain.getDefBossOnlyStatusTypeArr());
		}
		
	}
	
	/**
	 * ワークフロー設定をコピー（ステータスタイプ毎に設定されている内容）
	 * ・承認依頼中チェックイン可否（オプションがONの場合のみ処理）
	 * ・上長のみ表示デフォルト設定
	 * @param srcWorkFlowDocDomain コピー元ワークフロー
	 * @param destWorkFlowDocDomain コピー先ワークフロー
	 * @throws Exception
	 */
	private void copyWorkFlowSettingForStatusType(WorkFlowDocDomain srcWorkFlowDocDomain ,WorkFlowDocDomain destWorkFlowDocDomain) throws Exception {
		
		HashSet<Long> srcEnableCheckinStatusTypeSet = new HashSet<Long>();
		HashSet<Long> srcBossOnlyStatusTypeSet = new HashSet<Long>();
		
		long[] srcEnableCheckinStatusTypeArr = srcWorkFlowDocDomain.getEnableCheckinStatusTypeArr();
		long[] srcBossOnlyStatusTypeArr = srcWorkFlowDocDomain.getDefBossOnlyStatusTypeArr();
		
		// 承認依頼中チェックイン可否情報取得
		long[] newEnableCheckinStatusTypeArr = null;
		if( OptionConfData.getInstance().enableApproverCheckin && srcEnableCheckinStatusTypeArr != null && srcEnableCheckinStatusTypeArr.length > 0) {
			newEnableCheckinStatusTypeArr = new long[srcEnableCheckinStatusTypeArr.length];
			
			for(long statusTypeId : srcEnableCheckinStatusTypeArr) {
				srcEnableCheckinStatusTypeSet.add(statusTypeId);
			}
		}
		// 上長のみ表示設定情報取得
		long[] newBossOnlyStatusTypeArr = null;
		if ( srcBossOnlyStatusTypeArr != null && srcBossOnlyStatusTypeArr.length > 0) {
			newBossOnlyStatusTypeArr = new long[srcBossOnlyStatusTypeArr.length];
			
			for(long statusTypeId : srcBossOnlyStatusTypeArr) {
				srcBossOnlyStatusTypeSet.add(statusTypeId);
			}
		}
		
		// コピー先のステータスタイプIDに変換
		int newEnableCheckinCnt = 0;
		int newBossOnlyCnt = 0;
		for(int i = 0; i < srcWorkFlowDocDomain.getStatusTypeList().size(); i++ ) {
			StatusTypeDomain statusTypeDomain = srcWorkFlowDocDomain.getStatusTypeList().get(i);
			// 承認依頼中チェックイン可否
			if( srcEnableCheckinStatusTypeSet.contains((long)statusTypeDomain.getId()) ) {
				newEnableCheckinStatusTypeArr[newEnableCheckinCnt] = destWorkFlowDocDomain.getStatusTypeList().get(i).getId();
				newEnableCheckinCnt++;
			}
			
			// 上長のみ表示設定
			if( srcBossOnlyStatusTypeSet.contains((long)statusTypeDomain.getId()) ) {
				newBossOnlyStatusTypeArr[newBossOnlyCnt] = destWorkFlowDocDomain.getStatusTypeList().get(i).getId();
				newBossOnlyCnt++;
			}
		}
		
		// 複製先ワークフローに設定
		destWorkFlowDocDomain.setEnableCheckinStatusTypeArr(newEnableCheckinStatusTypeArr);
		destWorkFlowDocDomain.setDefBossOnlyStatusTypeArr(newBossOnlyStatusTypeArr);
	}

	/**
	 * ワークフロー構成の新規作成処理
	 *
	 * @param workFlowDocDomain
	 * @param eimWorkFlow
	 * @return
	 * @throws Exception
	 */
	private void createConfigurationStatusInfo(WorkFlowDocDomain workFlowDocDomain, EIMWorkFlow workFlow) throws Exception {

		// ステータス構成 作成(INSERTのみ)
		createStatusAndEvents(workFlowDocDomain, workFlow);

		regstConfigurationStatusInfo(workFlowDocDomain, workFlow);

		//Create Operation History
		for (StatusTypeDomain statusTypeDomain : workFlowDocDomain.getStatusTypeList()){
			OperationHistoryUtils.create(EIMThreadContext.getEIMSession(), 
									AppConstant.SYSTEM, 
									EIMConstant.CREATE_STATUS, 
									EIMConstant.TARGET_PARENT_WORKFLOW, 
									EIMConstant.WORKFLOW, 
									workFlow,
									EIMConstant.TARGET_CREATE, 
									EIMConstant.STATUS_TYPE, 
									statusTypeDomain.createEIMStatusType(),
									null
			);
		}
	}

	/**
	 * ワークフロー構成の更新処理
	 *
	 * @param workFlowDocDomain
	 * @param eimWorkFlow
	 * @return
	 * @throws Exception
	 */
	private void updateConfigurationStatusInfo(WorkFlowDocDomain workFlowDocDomain, EIMWorkFlow workFlow) throws Exception {

		// ステータス構成 作成(DELETE-INSERT)
		updateStatusAndEvents(workFlowDocDomain, workFlow);

		regstConfigurationStatusInfo(workFlowDocDomain, workFlow);

		//Create Operation History
		for (StatusTypeDomain statusTypeDomain : workFlowDocDomain.getStatusTypeList()){
			OperationHistoryUtils.create(EIMThreadContext.getEIMSession(), 
									AppConstant.SYSTEM, 
									EIMConstant.UPDATE_STATUS, 
									EIMConstant.TARGET_PARENT_WORKFLOW, 
									EIMConstant.WORKFLOW, 
									workFlow,
									EIMConstant.TARGET_UPDATE, 
									EIMConstant.STATUS_TYPE, 
									statusTypeDomain.createEIMStatusType(),
									null
			);
		}
	}

	/**
	 * ワークフロー構成の登録(DELETE-INSERT)処理
	 *
	 * [使用箇所]
	 * ・ワークフロー構成の新規作成処理
	 * ・ワークフロー構成の更新処理
	 * [特記事項]
	 * ステータス構成作成処理を実施してから本メソッドを呼び出すこと。ステータスタイプ、イベントタイプがDelete・Insertされた場合のため、関連する情報も再登録する必要がある。
	 * ※ステータス構成に関連する情報
	 * 	・ステータス別セキュリティ設定
	 * 	・公開処理設定
	 * また、ステータス構成作成処理では｢公開通知先参照｣情報も登録している。
	 *
	 * @param workFlowDocDomain
	 * @param eimWorkFlow
	 * @return
	 * @throws Exception
	 */
	private void regstConfigurationStatusInfo(WorkFlowDocDomain workFlowDocDomain, EIMWorkFlow workFlow) throws Exception {
		
		//ステータス別セキュリティ設定
		AppSecurityUtils.updateStatusSecurityForDocByWf(EIMThreadContext.getEIMSession(), workFlow);

		// 公開処理設定
		setPublishCommand(workFlowDocDomain, workFlow);

		// イベントタイプに属性タイプ｢コメント｣「承認依頼通知タイミング」「受信確認」「公開通知タイミング」「公開通知送信先」を紐付ける
		
		for (EventTypeDomain eventTypeDomain : workFlowDocDomain.getEventTypeList()){
			EIMEventType eimeventType = eventTypeDomain.createEIMEventType();
			// 承認依頼イベントの場合
			if(eventTypeDomain.getBaseEventType().getId() == AppConstant.BASE_EVENT_TYPE_ID_REQ_APPROVE){
				// 承認依頼通知タイミング
				EIMAttributeType attTypeApproveTiming = AttributeUtils.getAttributeTypeByName(EIMThreadContext.getEIMSession(), EIMConfig.getValue("ATTR_NAME_MAIL_NOTIFY_REQUEST_NOTIFY"));
				EventAttributeUtils.applyAttributeType(EIMThreadContext.getEIMSession(), eimeventType, attTypeApproveTiming);
				// 公開通知タイミング
				EIMAttributeType attTypePublicTiming = AttributeUtils.getAttributeTypeByName(EIMThreadContext.getEIMSession(), EIMConfig.getValue("ATTR_NAME_MAIL_NOTIFY_PUBLIC_TIMING"));
				EventAttributeUtils.applyAttributeType(EIMThreadContext.getEIMSession(), eimeventType, attTypePublicTiming);
				// 公開通知先
				EIMAttributeType attTypePublicTo = AttributeUtils.getAttributeTypeByName(EIMThreadContext.getEIMSession(), EIMConfig.getValue("ATTR_NAME_MAIL_NOTIFY_PUBLIC_NOTIFY_TO"));
				EventAttributeUtils.applyAttributeType(EIMThreadContext.getEIMSession(), eimeventType, attTypePublicTo);
				// 公開通知コメントログ
				EIMAttributeType attTypePublicCommentLog = AttributeUtils.getAttributeTypeByName(EIMThreadContext.getEIMSession(), EIMConfig.getValue("ATTR_NAME_PUBLIC_COMMENT_LOG"));
				EventAttributeUtils.applyAttributeType(EIMThreadContext.getEIMSession(), eimeventType, attTypePublicCommentLog);
				// 受信確認
				EIMAttributeType attTypeReply = AttributeUtils.getAttributeTypeByName(EIMThreadContext.getEIMSession(), EIMConfig.getValue("ATTR_NAME_MAIL_NOTIFY_CONFIRM_RECEIVE"));
				EventAttributeUtils.applyAttributeType(EIMThreadContext.getEIMSession(), eimeventType, attTypeReply);
			}
			// 承認イベントの場合
			if(eventTypeDomain.getBaseEventType().getId() == AppConstant.BASE_EVENT_TYPE_ID_APPROVAL){
				// 承認依頼通知タイミング
				EIMAttributeType attTypeApproveTiming = AttributeUtils.getAttributeTypeByName(EIMThreadContext.getEIMSession(), EIMConfig.getValue("ATTR_NAME_MAIL_NOTIFY_REQUEST_NOTIFY"));
				EventAttributeUtils.applyAttributeType(EIMThreadContext.getEIMSession(), eimeventType, attTypeApproveTiming);
				// 公開通知コメントログ
				EIMAttributeType attTypePublicCommentLog = AttributeUtils.getAttributeTypeByName(EIMThreadContext.getEIMSession(), EIMConfig.getValue("ATTR_NAME_PUBLIC_COMMENT_LOG"));
				EventAttributeUtils.applyAttributeType(EIMThreadContext.getEIMSession(), eimeventType, attTypePublicCommentLog);
			}
			// ※ 全てのイベントタイプにコメント属性タイプを紐付けているが、イベントタイプ｢公開｣では使用していません。
			EIMAttributeType attTypeComment = AttributeUtils.getAttributeTypeByName(EIMThreadContext.getEIMSession(), EIMConfig.getValue("EVENT_ATTR_NAME_COMMENT"));
			EventAttributeUtils.applyAttributeType(EIMThreadContext.getEIMSession(), eimeventType, attTypeComment);
		}
		
	}

	/**
	 * ワークフローのステータスとイベントをセットする
	 * @param workFlowDocDomain
	 * @param eimWorkFlow
	 * @return
	 * @throws Exception
	 */
	private void createStatusAndEvents(WorkFlowDocDomain workFlowDocDomain, EIMWorkFlow workFlow) throws Exception {
		if(workFlow == null)
			throw new Exception("Not Found workflow at updateStatusStruct");

		// ワークフローの更新
		workFlowDocDomain.setId(workFlow.getId());
		workFlowDocDomain.setDefName(workFlow.getDefName());
		getWorkFlowDefDao().updateDef((WorkFlowDomain)workFlowDocDomain);

	}

	/**
	 * ワークフローのステータスとイベントを更新する
	 * @param workFlowDocDomain
	 * @param eimWorkFlow
	 * @return
	 * @throws Exception
	 */
	private void updateStatusAndEvents(WorkFlowDocDomain workFlowDocDomain, EIMWorkFlow workFlow) throws Exception {
		if(workFlow == null)
			throw new Exception("Not Found workflow at updateStatusStruct");

		// 更新前の「公開処理中」ステータスタイプを抽出
		StatusTypeDomain oldPublishingStatusType = workFlowDocDomain.getStatusTypeList().stream()
				.filter(stt -> stt.getStatusTypeKind().getId() == AppConstant.STATUS_TYPE_KIND_ID_PROCESSING_PUBLIC)
				.findFirst().orElse(null);
		long oldPublishingStatusTypeId = oldPublishingStatusType != null ? oldPublishingStatusType.getId() : 0;

		// ワークフローの更新
		workFlowDocDomain.setId(workFlow.getId());
		workFlowDocDomain.setDefName(workFlow.getDefName());
		getWorkFlowDefDao().updateDef((WorkFlowDomain)workFlowDocDomain);

		// 「ワークフロー公開処理」オブジェクトの名称を更新されたステータスタイプIDで更新
		if (oldPublishingStatusTypeId > 0) {
			updatePublishObject(oldPublishingStatusTypeId, workFlowDocDomain.getStatusTypeList());
		}
	}

	/**
	 * ワークフローの更新によって削除された「公開処理中」ステータスタイプが保持していた「ワークフロー公開処理」オブジェクト
	 * を新規登録されたステータスタイプに対して関連付けるよう更新する。
	 * @throws Exception
	 */
	private void updatePublishObject(long oldStatusTypeId, List<StatusTypeDomain> newStatusTypeList) throws Exception {

		// 更新後の「公開処理中」ステータスタイプを抽出
		StatusTypeDomain newPublishingStatusType = newStatusTypeList.stream()
				.filter(stt -> stt.getStatusTypeKind().getId() == AppConstant.STATUS_TYPE_KIND_ID_PROCESSING_PUBLIC)
				.findFirst().orElse(null);

		// 「ワークフロー公開処理」オブジェクトの名称を更新されたステータスタイプIDで更新
		if(newPublishingStatusType != null) {
			ObjectDomain publishingObject = objectDao2.getByTypeAndName(
					new ObjectTypeDomain(EIMConfig.get("OBJECT_TYPE_NAME_WFPUB")),
					String.valueOf(oldStatusTypeId));
			if (publishingObject != null) {
				objectDao2.updateName(publishingObject, String.valueOf(newPublishingStatusType.getId()), DuplicateCheckModeEnum.NONE);
			}
		}
	}

	/**
	 * 公開処理設定に関係の情報設定する
	 * [特記事項]
	 * 　
	 *
	 * @param workFlowDocDomain
	 * @param workFlow
	 * @return
	 * @throws Exception
	 */
	private void setPublishCommand(WorkFlowDocDomain workFlowDocDomain, EIMWorkFlow workFlow) throws Exception {
		EIMSession sess = EIMThreadContext.getEIMSession();

		EIMWorkFlow wf = WorkFlowUtils.getWorkFlowById(sess, workFlow.getId());
		@SuppressWarnings("unchecked")
		List<EIMStatusType> boStatusTypeList = (List<EIMStatusType>)wf.getStatusTypeList();
		//StatusType
		EIMStatusType stType = null;

		// ステータスタイプ｢公開処理中｣を取得
		for(EIMStatusType stty:boStatusTypeList) {
			if(stty.getKind() == AppConstant.STATUS_TYPE_KIND_ID_PROCESSING_PUBLIC) {
				stType = stty;
				break;
			}
		}
		
		if(stType == null) {
			return ;
		}
		
		// ワークフロー公開処理オブジェクトの取得
		EIMObject wfpubObj = AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_WFPUB"), String.valueOf(stType.getId()));
		if(wfpubObj == null){
			wfpubObj = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_WFPUB"), String.valueOf(stType.getId()));
		}

		long[] doPDFConvert = {0};
		long[] doPDFURL ={0};
		if(workFlowDocDomain.isDoPDFConvert()) {
			doPDFConvert[0] = 1;
		}
		if(workFlowDocDomain.isDoPDFURL()) {
			doPDFURL[0] = 1;
		}
		WorkFlowDocPDFSignatureDomain signatureCommand = workFlowDocDomain.getSignatureCommand();
		if(signatureCommand != null) {

			/* 公開処理設定の電子署名 */
			PublishAddonUtils.setDoSignAndSetSecurityConfig(sess, wfpubObj, signatureCommand.getDoSignAndSetSecurity());
			PublishAddonUtils.setSignAndSetSecurityConfig(sess, wfpubObj, signatureCommand.getDoSignPDF(),
					signatureCommand.getInsertApproveDate(), signatureCommand.getInsertApproveUser(),
					signatureCommand.getInsertPage(), signatureCommand.getInsertPlace(),
					signatureCommand.getInsertPlaceX(), signatureCommand.getInsertPlaceY(),
					signatureCommand.getApproveNamelang(), signatureCommand.getSignJobName(),
					signatureCommand.getDoSetSecurity(), signatureCommand.getDoSetSecurityPassword(),
					signatureCommand.getSecurityPassword(), signatureCommand.getDoSetReferencePassword(),
					signatureCommand.getReferencePassword(), signatureCommand.getForbidPrint(),
					signatureCommand.getForbidEdit(), signatureCommand.getForbidAnnotate(), signatureCommand.getForbidReproduce());
		}


		// 属性「PDF変換実施フラグ」の設定
		AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_FLG"),  doPDFConvert);

		// 属性「URL挿入フラグ」の設定
		AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_INSERT_URL_FLG"), doPDFURL);

		/*公開処理設定のPDF化有効期限*/
		if(workFlowDocDomain.isDoSetTerm()){
			// パラメータの取得

			// 配列型に変換
			long[] termNumParam = {workFlowDocDomain.getTermNumParam()};
			String[] termUnitParam = {workFlowDocDomain.getTermUnitParam()};

			// 属性「有効期限設定期間数字」の設定
			AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_TERM_NUM"), termNumParam);
			// 属性「有効期限設定期間単位」の設定
			AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_TERM_UNIT"), termUnitParam);
		}
		// 有効期限設定を行わない場合
		else{
			// 属性「有効期限設定期間数字」の削除
			AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_TERM_NUM"), (long[])null);
			// 属性「有効期限設定期間単位」の削除
			AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_TERM_UNIT"), (String[])null);
		}
		return;
	}

	/**
	 * ステータス名称とアサインエントリを更新します
	 *
	 * @param workFlowDefDomain ワークフロー
	 * @throws Exception
	 */
	public void updateStatusNameAndAssigns(WorkFlowDocDomain workFlowDocDomain, boolean updateFlg) throws Exception {

		if(!AdminAuthUtil.hasSpecifiedOrGeneralAuth(EIMThreadContext.getEIMSession().getUser(), AppConstant.ADMIN_AUTH_ID_WORKFLOW)){
			throw new EIMAppException("EIM.ERROR.NOTADMINISTRATOR");
		}
		
		EIMWorkFlow workFlow = WorkFlowUtils.getWorkFlowById(EIMThreadContext.getEIMSession(), workFlowDocDomain.getId());
		
		if(workFlowDocDomain == null || workFlow == null){
			throw new EIMAppException("EIM.ERROR.LOGIC.WORKFLOW.NOTFOUND");
		}

		// ステータスタイプリスト取得
		List<StatusTypeDomain> statusTypeDomainList = workFlowDocDomain.getStatusTypeList();

		long workFlowId = (long)workFlowDocDomain.getId();
		if (updateFlg) {
			// イベントタイプリスト取得
			List<EventTypeDomain> eventTypeDomainList = workFlowDocDomain.getEventTypeList();
			// イベントタイプ->ステータスタイプの順に更新する(ステータスタイプを更新するとイベントタイプがdelete/insertされるため)
			for (EventTypeDomain eventTypeDomain : eventTypeDomainList) {
				// イベント名称更新
				eventTypeDao.update(eventTypeDomain);
			}

			// 公開処理中のステータスタイプ
			StatusTypeDomain processingPublicStatusTypeDomain = null;

			for (StatusTypeDomain statusTypeDomain : statusTypeDomainList) {

				// ステータス名称・アサインエントリ更新
				statusTypeDao.update(statusTypeDomain);

				// 公開処理中のステータスタイプ抽出
				if (statusTypeDomain.getStatusTypeKind().getId() == AppConstant.STATUS_TYPE_KIND_ID_PROCESSING_PUBLIC) {
					processingPublicStatusTypeDomain = statusTypeDomain;
				}

				// SearchFramework 検索FW更新通知 対象：全ステータスタイプ
				AppUpdateNoticeUtils.updateNoticeInsert(statusTypeDomain.getId(), "SEARCHFW_WORKFLOW_RELEASE_STATUSTYPE");
			}
			
			// 更新したステータスタイプを再取得
			List<StatusTypeDomain> newStatusTypeList = statusTypeDaoLight.getListByWorkFlowId(workFlowId);

			// 「ワークフロー公開処理」オブジェクトの名称を更新されたステータスタイプIDで更新
			updatePublishObject(processingPublicStatusTypeDomain.getId(), newStatusTypeList);

			// workFlowDocDomainの承認依頼中チェックイン、上長のみ表示設定は更新前のステータスタイプIDが投入されているので詰め替える
			workFlowDocDomain.setEnableCheckinStatusTypeArr(
					convertUpdatedStatusTypeIdArray(workFlowDocDomain.getEnableCheckinStatusTypeArr(), statusTypeDomainList, newStatusTypeList));

			workFlowDocDomain.setDefBossOnlyStatusTypeArr(
					convertUpdatedStatusTypeIdArray(workFlowDocDomain.getDefBossOnlyStatusTypeArr(), statusTypeDomainList, newStatusTypeList));

		} else {
			// ===================================================
			// ステータス構成 登録
			// ===================================================
			createConfigurationStatusInfo(workFlowDocDomain, workFlow);
			
			// リビジョンアップ時のみ通る
			// ワークフロー公開処理オブジェクトを前リビジョンのオブジェクトからコピー
			WorkflowDomain newWorkflow = workflowService2.getById(workFlow.getId());
			
			// 直前の版のワークフローを取得
			long prevWorkflowId = getPrevWorkflowId(newWorkflow);
			if (prevWorkflowId != -1) {
				// 公開処理オブジェクトを複製
				AppObjectUtil.copyDocWorkflowPublishObject(EIMThreadContext.getEIMSession(), prevWorkflowId, workFlow.getId(), false);
				
				// 複写元のワークフロー情報を取得
				WorkFlowDomain srcWorkFlow = super.getDefById(prevWorkflowId);
				WorkFlowDocDomain srcWorkFlowDocDomain = new WorkFlowDocDomain(srcWorkFlow);
				
				// workFlowDocDomainの承認依頼中チェックイン、上長のみ表示設定は古いステータスタイプIDが投入されているので詰め替える
				srcWorkFlowDocDomain.setEnableCheckinStatusTypeArr(workFlowDocDomain.getEnableCheckinStatusTypeArr());
				srcWorkFlowDocDomain.setDefBossOnlyStatusTypeArr(workFlowDocDomain.getDefBossOnlyStatusTypeArr());
				// ワークフロー設定情報(ステータス毎設定)をコピー
				copyWorkFlowSettingForStatusType(srcWorkFlowDocDomain, workFlowDocDomain);
			}
		}
		
		// ワークフロー設定を更新（ステータスタイプ毎）
		EIMObject workFlowSettingObj = AppObjectUtil.getObject(EIMThreadContext.getEIMSession(), 	EIMConfig.get("OBJECT_TYPE_NAME_WFSETTING"), String.valueOf(workFlow.getId()));
		updateWorkFlowSettingForStatusType(workFlowDocDomain, workFlowSettingObj);
		
		// SearchFramework 検索FW更新通知 対象：ワークフロー
		AppUpdateNoticeUtils.updateNoticeInsert(workFlowId, "SEARCHFW_WORKFLOW_RELEASE_WORKFLOW");
		
		//Create Operation History
		OperationHistoryUtils.create(EIMThreadContext.getEIMSession(), 
				AppConstant.SYSTEM, 
				EIMConstant.UPDATE_WORKFLOW,
				EIMConstant.TARGET_UPDATE, 
				EIMConstant.WORKFLOW, 
				workFlowDocDomain.createEIMWorkFlow(),
				null, 
				null, 
				null, 
				null
		);
	}

	/**
	 * 直前の版のワークフローIDを取得します<br/>
	 * 
	 * @param newWorkflow ワークフロー
	 * @return 直前の版のワークフローID（存在しなかった場合は-1）
	 * @throws Exception
	 */
	private long getPrevWorkflowId(WorkflowDomain newWorkflow) throws Exception {

		long prevWorkflowId = -1;

		WorkflowCriteria workflowCriteria = new WorkflowCriteria();
		workflowCriteria.setRevisionGroupId(newWorkflow.getRevisionGroupId());
		// ワークフロー履歴一覧を取得
		List<WorkflowDomain> workflowList = workflowService2.getList(workflowCriteria);
		for (WorkflowDomain workflow : workflowList) {
			// 直前のリビジョン
			if (workflow.getRevision() != newWorkflow.getRevision()-1 ) {
				continue;
			}
			prevWorkflowId = workflow.getId();
			break;
		}
		return prevWorkflowId;
	}

	/**
	 * 旧ステータスタイプIDの配列を新ステータスタイプIDの配列に変換します
	 * 
	 * @param oldStatusTypeIdArray 旧ステータスタイプIDの配列
	 * @param oldStatusTypeList 旧ステータスタイプのリスト
	 * @param newStatusTypeList 新ステータスタイプのリスト
	 * @return 新ステータスタイプIDの配列
	 * @throws Exception
	 */
	private long[] convertUpdatedStatusTypeIdArray(
			long[] oldStatusTypeIdArray, List<StatusTypeDomain> oldStatusTypeList, List<StatusTypeDomain> newStatusTypeList) throws Exception {

		if (oldStatusTypeIdArray == null) {
			return null;
		}

		if (oldStatusTypeList.size() != newStatusTypeList.size()) {

			throw new Exception("No corresponding status type");
			
		}

		long[] newStatusTypeIdArray = new long[oldStatusTypeIdArray.length];
		try {
			for (int i = 0; i < oldStatusTypeIdArray.length; i++) {
				long oldStatusTypeId = oldStatusTypeIdArray[i];
				
				StatusTypeDomain oldStatusType = oldStatusTypeList.stream()
						.filter(statusType -> statusType.getId() == oldStatusTypeId)
						.findFirst().get();
	
				StatusTypeDomain newStatusType = newStatusTypeList.stream()
						.filter(statusType -> statusType.getSeq() == oldStatusType.getSeq())
						.findFirst().get();
	
				newStatusTypeIdArray[i] = newStatusType.getId();
			}
		} catch (NoSuchElementException e) {

			// 対応するステータスタイプが存在しない場合
			throw new Exception("No corresponding status type", e);

		}
		return newStatusTypeIdArray;
	}

	/**
	 * デフォルト公開通知先参照 登録(新規追加・更新)
	 * 
	 * ステータスタイプ｢公開処理中｣のオブジェクトに紐付くオブジェクトタイプ｢公開通知先エントリー｣へ</br>
	 * デフォルト公開通知先参照情報を登録する。
	 * 
	 * @param workFlowDocDomain
	 * @param workFlow
	 * @throws Exception
	 */
	private void updatePublishNotify(WorkFlowDocDomain workFlowDocDomain, EIMWorkFlow workFlow) throws Exception {
		
		//=================================================================
		// オブジェクトタイプ｢公開通知先エントリー｣情報取得
		//=================================================================
		EIMObjectType objType = ObjectUtils.getObjectTypeByName(EIMThreadContext.getEIMSession(),EIMConfig.get("OBJECT_TYPE_NAME_WFPUBNOTIFY"));

		// オブジェクトタイプ｢公開通知先エントリー｣のオブジェクトを取得
		EIMObject pubNotifObj = ObjectUtils.getObjectByTypeAndName(EIMThreadContext.getEIMSession(),objType,Long.toString(workFlow.getId()));
		if( pubNotifObj == null ) {
			pubNotifObj = ObjectUtils.createObject( EIMThreadContext.getEIMSession(), objType, Long.toString(workFlow.getId()));
		}

		//=================================================================
		// オブジェクトタイプ｢公開通知先エントリー｣の属性情報取得
		//=================================================================
		EIMAttributeType attrTypeId = AttributeUtils.getAttributeTypeByName( EIMThreadContext.getEIMSession(), EIMConfig.get("ATTR_NAME_WFPUB_NOTIFY_ENTRY_TYPE") );
		EIMAttributeType attrTypeObj = AttributeUtils.getAttributeTypeByName( EIMThreadContext.getEIMSession(), EIMConfig.get("ATTR_NAME_WFPUB_NOTIFY_ENTRY_OBJ") );

		//=================================================================
		// オブジェクトタイプ｢公開通知先エントリー｣のオブジェクトに
		// エントリー情報を追加
		//=================================================================
		ArrayList<Long> entryTypeIdList = new ArrayList<Long>();
		ArrayList<Long> entryObjIdList = new ArrayList<Long>();
		AssignEntryDomain publishNotyfyAssignEntryDomain = workFlowDocDomain.getPublishNotyfyAssignEntryDomain();	// デフォルト公開通知先 情報

		// ステータスタイプの取得(操作履歴 登録用)
		List<StatusTypeDomain> statusTypeDomainList = workFlowDocDomain.getStatusTypeList();
		EIMStatusType eimStatusType = null;
		for (StatusTypeDomain statusTypeDomain : statusTypeDomainList) {
			if(statusTypeDomain.getStatusTypeKind().getId() == AppConstant.STATUS_TYPE_KIND_ID_PROCESSING_PUBLIC) {
				eimStatusType = statusTypeDomain.createEIMStatusType();
				break;
			}
		}
		
		// 属性を追加した 新 ArrayList を作成
		for( BelongDomain belongDomain : publishNotyfyAssignEntryDomain.getBelongList()){
			//=================================================================
			// 公開通知先エントリー 登録用データ格納
			//=================================================================
			long entryTypeId = convBelongTypeToEntryTypeID(belongDomain.getBelongType());
			long entryObjId = belongDomain.getBelonging().getId();

			entryTypeIdList.add(entryTypeIdList.size(), entryTypeId);
			entryObjIdList.add(entryObjIdList.size(), entryObjId);

			//=================================================================
			// 操作履歴登録 Create Operation History
			//=================================================================
			//User
			if( entryTypeId == EIMAccessEntryType.USER ) {
				EIMUser entUser = UserUtils.getUserById(EIMThreadContext.getEIMSession(), entryObjId);
				if( entUser == null ) {
					throw new EIMAppException("EIM.ERROR.LOGIC.NOACCESSENTRY");
				}
				if( workFlow != null ) {
					OperationHistoryUtils.create(EIMThreadContext.getEIMSession(), AppConstant.SYSTEM, EIMConstant.UPDATE_STATUS_INFO, 
						EIMConstant.TARGET_PARENT_WORKFLOW, EIMConstant.WORKFLOW, workFlow,
						EIMConstant.TARGET_UPDATE, EIMConstant.STATUS_TYPE, eimStatusType, null);

					OperationHistoryUtils.create(EIMThreadContext.getEIMSession(), AppConstant.SYSTEM, AppConstant.REGIST_USER_ENTRY_FOR_PUBLIC,
							EIMConstant.TARGET_TO_REGIST_ENTRY, EIMConstant.WORKFLOW, workFlow,
							EIMConstant.TARGET_REGISTED_ENTRY, EIMConstant.USER, entUser, null);
				}
			}
			//Group
			if( entryTypeId == EIMAccessEntryType.GROUP ) {
				EIMGroup entGroup = GroupUtils.getGroupById(EIMThreadContext.getEIMSession(), entryObjId);
				if( entGroup == null ) {
					throw new EIMAppException("EIM.ERROR.LOGIC.NOACCESSENTRY");
				}
				if( workFlow != null ) {
					OperationHistoryUtils.create(EIMThreadContext.getEIMSession(), AppConstant.SYSTEM, EIMConstant.UPDATE_STATUS_INFO, 
						EIMConstant.TARGET_PARENT_WORKFLOW, EIMConstant.WORKFLOW, workFlow,
						EIMConstant.TARGET_UPDATE, EIMConstant.STATUS_TYPE, eimStatusType, null);

					OperationHistoryUtils.create(EIMThreadContext.getEIMSession(), AppConstant.SYSTEM, AppConstant.REGIST_GROUP_ENTRY_FOR_PUBLIC,
							EIMConstant.TARGET_TO_REGIST_ENTRY, EIMConstant.WORKFLOW, workFlow,
							EIMConstant.TARGET_REGISTED_ENTRY, EIMConstant.GROUP, entGroup, null);
				}
			}
			//Role
			if( entryTypeId == EIMAccessEntryType.ROLE ) {
				EIMRole entRole = RoleUtils.getRoleById(EIMThreadContext.getEIMSession(), entryObjId);
				if( entRole == null ) {
					throw new EIMAppException("EIM.ERROR.LOGIC.NOACCESSENTRY");
				}
				if( workFlow != null ) {
					OperationHistoryUtils.create(EIMThreadContext.getEIMSession(), AppConstant.SYSTEM, EIMConstant.UPDATE_STATUS_INFO, 
						EIMConstant.TARGET_PARENT_WORKFLOW, EIMConstant.WORKFLOW, workFlow,
						EIMConstant.TARGET_UPDATE, EIMConstant.STATUS_TYPE, eimStatusType, null);

					OperationHistoryUtils.create(EIMThreadContext.getEIMSession(), AppConstant.SYSTEM, AppConstant.REGIST_ROLE_ENTRY_FOR_PUBLIC,
							EIMConstant.TARGET_TO_REGIST_ENTRY, EIMConstant.WORKFLOW, workFlow,
							EIMConstant.TARGET_REGISTED_ENTRY, EIMConstant.ROLE, entRole, null);
				}
			}
			//Comp
			if( entryTypeId == EIMAccessEntryType.COMP ) {
				EIMComp entComp = CompUtils.getCompById(EIMThreadContext.getEIMSession(), entryObjId);
				if( entComp == null ) {
					throw new EIMAppException("EIM.ERROR.LOGIC.NOACCESSENTRY");
				}
				if( workFlow != null ) {
					OperationHistoryUtils.create(EIMThreadContext.getEIMSession(), AppConstant.SYSTEM, EIMConstant.UPDATE_STATUS_INFO, 
						EIMConstant.TARGET_PARENT_WORKFLOW, EIMConstant.WORKFLOW, workFlow,
						EIMConstant.TARGET_UPDATE, EIMConstant.STATUS_TYPE, eimStatusType, null);

					OperationHistoryUtils.create(EIMThreadContext.getEIMSession(), AppConstant.SYSTEM, AppConstant.REGIST_COMP_ENTRY_FOR_PUBLIC,
							EIMConstant.TARGET_TO_REGIST_ENTRY, EIMConstant.WORKFLOW, workFlow,
							EIMConstant.TARGET_REGISTED_ENTRY, EIMConstant.COMP, entComp, null);
				}
			}

		}
		
		//=================================================================
		// 公開通知先エントリー 登録
		//=================================================================
		// 新属性の設定
		long[] newIds = new long[entryTypeIdList.size()];
		for (int i = 0; i < entryTypeIdList.size() ; i++ ){
			newIds[i] = ((Long)entryTypeIdList.get(i)).longValue();
		} 
		long[] newTypes = new long[entryObjIdList.size()];
		for (int i = 0; i < entryObjIdList.size() ; i++ ){
			newTypes[i] = ((Long)entryObjIdList.get(i)).longValue();
		}
		ObjectAttributeUtils.setAttribute(EIMThreadContext.getEIMSession(), pubNotifObj, attrTypeId
				, TypeConvertUtils.convertToBuildTypeArray(newIds));
		ObjectAttributeUtils.setAttribute(EIMThreadContext.getEIMSession(), pubNotifObj, attrTypeObj
				, TypeConvertUtils.convertToBuildTypeArray(newTypes));
		
	}

	/**
	 * 属性タイプ(BelongType) ==> エントリータイプID変換(int)
	 * 
	 * @param belongType 属性タイプ(BelongType)
	 * @return int エントリータイプID
	 */
	private int convBelongTypeToEntryTypeID(BelongType belongType){
		int id = -1;

		if( belongType.equals(BelongType.USER)) {
			id = EIMAccessEntryType.USER;
		}
		else if( belongType.equals(BelongType.GROUP)) {
			id = EIMAccessEntryType.GROUP;
		}
		else if( belongType.equals(BelongType.ROLE)) {
			id = EIMAccessEntryType.ROLE;
		}
		else if( belongType.equals(BelongType.COMPGROUP)) {
			id = EIMAccessEntryType.COMP;
		}

		return id;
	}
	
	/**
	 * デフォルト公開通知先参照 取得
	 * 
	 * 
	 * @param workFlowDocDomain
	 * @return AssignEntryDomain
	 * @throws Exception
	 */
	private AssignEntryDomain getPublishNotify(WorkFlowDocDomain workFlowDocDomain) throws Exception {

		AssignEntryDomain assignEntryDomain = null;
		
		EIMObjectType objType = ObjectUtils.getObjectTypeByName(EIMThreadContext.getEIMSession(), EIMConfig.get("OBJECT_TYPE_NAME_WFPUBNOTIFY"));
		EIMObject entry = ObjectUtils.getObjectByTypeAndName(EIMThreadContext.getEIMSession(), objType,Long.toString(workFlowDocDomain.getId()));
		if (entry == null){				// オブジェクトタイプ｢公開通知先エントリー｣に紐付くオブジェクトがない場合、処理終了
			return assignEntryDomain;
		}
		
		EIMAttribute attrTypeId = entry.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_NOTIFY_ENTRY_TYPE"));
		EIMAttribute attrTypeObj = entry.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_NOTIFY_ENTRY_OBJ"));
		if( attrTypeId == null || attrTypeObj == null ) {		// エントリタイプID, 対象IDが存在しない場合、処理終了
			return assignEntryDomain;
		}

		assignEntryDomain = new AssignEntryDomain();

		// 属性を取得
		long[] ids = TypeConvertUtils.convertToLongArray(attrTypeId.getInts());
		long[] types = TypeConvertUtils.convertToLongArray(attrTypeObj.getInts());
		// エントリリスト (後にソートの上 XML 化)
		List<EIMUser> userList = new ArrayList<EIMUser>();		// ユーザー     リスト
		List<EIMGroup> groupList = new ArrayList<EIMGroup>();	// グループ     リスト
		List<EIMRole> roleList = new ArrayList<EIMRole>();		// ロール       リスト
		List<EIMComp> compList = new ArrayList<EIMComp>();		// 複合グループ リスト

		// 
		for( int j = 0; j < ids.length; j++ ) {
			if( ids[j] == EIMAccessEntryType.USER ) {
				EIMUser entUser = UserUtils.getUserById(EIMThreadContext.getEIMSession(), types[j]);
				userList.add(entUser);
			} else if( ids[j] == EIMAccessEntryType.GROUP ) {
				EIMGroup entGroup = GroupUtils.getGroupById(EIMThreadContext.getEIMSession(), types[j]);
				groupList.add(entGroup);
			} else if( ids[j] == EIMAccessEntryType.ROLE ) {
				EIMRole entRole = RoleUtils.getRoleById(EIMThreadContext.getEIMSession(), types[j]);
				roleList.add(entRole);
			} else if( ids[j] == EIMAccessEntryType.COMP ) {
				EIMComp entComp = CompUtils.getCompById(EIMThreadContext.getEIMSession(), types[j]);
				compList.add(entComp);
			}
		}

		// 並び替え
		BelongDomain belongDomain;
		if( groupList.size() > 0 ) {
			groupList = AppObjectUtil.getStrSortedList(groupList, "getName", true);
			for( Iterator ite = groupList.iterator(); ite.hasNext(); ) {
				EIMGroup grp = (EIMGroup)ite.next();

				belongDomain = new BelongDomain();
				belongDomain.setBelongType(BelongType.GROUP);								// 所属タイプ
				belongDomain.getBelonging().setId(grp.getId());								// 所属ID
				belongDomain.getBelonging().setName(StringUtils.xmlEncode(grp.getName()));	// 所属名
				assignEntryDomain.getBelongList().add(belongDomain);
			}
		}
		if( roleList.size() > 0 ) {
			roleList = AppObjectUtil.getStrSortedList(roleList, "getName", true);
			for( Iterator ite = roleList.iterator(); ite.hasNext(); ) {
				EIMRole role = (EIMRole)ite.next();

				belongDomain = new BelongDomain();
				belongDomain.setBelongType(BelongType.ROLE);								// 所属タイプ
				belongDomain.getBelonging().setId(role.getId());							// 所属ID
				belongDomain.getBelonging().setName(StringUtils.xmlEncode(role.getName()));	// 所属名
				assignEntryDomain.getBelongList().add(belongDomain);
			}
		}
		if( compList.size() > 0 ) {
			compList = AppObjectUtil.getStrSortedList(compList, "getName", true);
			for( Iterator ite = compList.iterator(); ite.hasNext(); ) {
				EIMComp comp = (EIMComp)ite.next();

				belongDomain = new BelongDomain();
				belongDomain.setBelongType(BelongType.COMPGROUP);							// 所属タイプ
				belongDomain.getBelonging().setId(comp.getId());							// 所属ID
				belongDomain.getBelonging().setName(StringUtils.xmlEncode(comp.getName()));	// 所属名
				assignEntryDomain.getBelongList().add(belongDomain);

			}
		}
		
		if( userList.size() > 0 ) {
			userList = AppObjectUtil.getStrSortedList(userList, "getName", true);
			for( Iterator ite = userList.iterator(); ite.hasNext(); ) {
				EIMUser eUser = (EIMUser)ite.next();

				belongDomain = new BelongDomain();
				belongDomain.setBelongType(BelongType.USER);								// 所属タイプ
				belongDomain.getBelonging().setId(eUser.getId());							// 所属ID
				belongDomain.getBelonging().setName(StringUtils.xmlEncode(eUser.getName()));// 所属名
				assignEntryDomain.getBelongList().add(belongDomain);

			}
		}

		return assignEntryDomain;
	}
	
	private StatusTypeDao statusTypeDao;
	private StatusTypeDao statusTypeDaoLight;
	private AssignEntryDao assignEntryDao;
	private EventTypeDao eventTypeDao;
	private WorkflowService workflowService2;
	private ObjectDao objectDao2;

	/**
	 * statusTypeDaoを取得します。
	 * @return statusTypeDao
	 */
	public StatusTypeDao getStatusTypeDao() {
	    return statusTypeDao;
	}

	/**
	 * statusTypeDaoを設定します。
	 * @param statusTypeDao statusTypeDao
	 */
	public void setStatusTypeDao(StatusTypeDao statusTypeDao) {
	    this.statusTypeDao = statusTypeDao;
	}

	/**
	 * statusTypeDaoLightを取得します。
	 * @return statusTypeDaoLight
	 */
	public StatusTypeDao getStatusTypeDaoLight() {
		return statusTypeDaoLight;
	}

	/**
	 * statusTypeDaoLightを設定します。
	 * @param statusTypeDaoLight statusTypeDaoLight
	 */
	public void setStatusTypeDaoLight(StatusTypeDao statusTypeDaoLight) {
		this.statusTypeDaoLight = statusTypeDaoLight;
	}

	/**
	 * assignEntryDaoを取得します。
	 * @return assignEntryDao
	 */
	public AssignEntryDao getAssignEntryDao() {
	    return assignEntryDao;
	}

	/**
	 * assignEntryDaoを設定します。
	 * @param assignEntryDao assignEntryDao
	 */
	public void setAssignEntryDao(AssignEntryDao assignEntryDao) {
	    this.assignEntryDao = assignEntryDao;
	}
	
	/**
	 * eventTypeDaoを取得します。
	 * @return eventTypeDao
	 */
	public EventTypeDao getEventTypeDao() {
	    return eventTypeDao;
	}

	/**
	 * eventTypeDaoを設定します。
	 * @param eventTypeDao eventTypeDao
	 */
	public void setEventTypeDao(EventTypeDao eventTypeDao) {
	    this.eventTypeDao = eventTypeDao;
	}

	/**
	 * workflowService2を取得します。
	 * @return the workflowService2
	 */
	public WorkflowService getWorkflowService2() {
		return workflowService2;
	}

	/**
	 * workflowService2を設定します。
	 * @param workflowService2 workflowService2
	 */
	public void setWorkflowService2(WorkflowService workflowService2) {
		this.workflowService2 = workflowService2;
	}

	/**
	 * objectDao2を取得します。
	 * @return objectDao2
	 */
	public ObjectDao getObjectDao2() {
		return objectDao2;
	}

	/**
	 * objectDao2を設定します。
	 * @param objectDao2 objectDao2
	 */
	public void setObjectDao2(ObjectDao objectDao2) {
		this.objectDao2 = objectDao2;
	}

}