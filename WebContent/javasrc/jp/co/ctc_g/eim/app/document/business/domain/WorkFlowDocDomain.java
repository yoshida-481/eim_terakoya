package jp.co.ctc_g.eim.app.document.business.domain;

import java.util.ArrayList;
import java.util.List;

import eim.bo.EIMAttribute;
import eim.bo.EIMObject;
import eim.bo.EIMOtherName;
import eim.bo.EIMWorkFlow;
import eim.util.EIMConfig;
import eim.util.TypeConvertUtils;
import jp.co.ctc_g.eim.framework.business.domain.AssignEntryDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.OtherNameDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.WorkFlowDomain;


public class WorkFlowDocDomain extends WorkFlowDomain {

	/* メール通知方法のデフォルト設定 */
	String defNotifyMail = "";

	/* 処理依頼先のデフォルト設定 */
	long defApproveRequest;

	/* 公開通知デフォルト設定 */
	long publishNotifyMail;

	/* 処理待ちポップアップ通知 */
	long processWaitPopup;

	/* 差戻し・取戻しメール通知 */
	long backMail;
	
	/* OCR処理のデフォルト設定 */
	long defOcr;
	
	/* PDF変換実施フラグ */
	boolean doPDFConvert = false;

	/* PDF分割実施フラグ */
	boolean doPDFDivide = false;

	/* URL挿入フラグ */
	boolean doPDFURL = false;

	/* 有効期限設定フラグ */
	boolean doSetTerm = false;

	/* 有効期限設定期間数字 */
	int termNumParam;

	/* 有効期限設定期間単位 */
	String termUnitParam;

	/* 公開処理設定電子署名 */
	WorkFlowDocPDFSignatureDomain signatureCommand;

	/* デフォルト公開通知先参照 */
	AssignEntryDomain publishNotyfyAssignEntryDomain = new AssignEntryDomain();

	/* 公開機能用：上長承認   necessary：要承認  unnecessary：承認不要 */
	String bossApproval = "";

	/* ワークフロー複写作成設定フラグ */
	boolean isCreateCopy = false;

	/* ワークフロー複写元ID */
	long originWorkflowId;

	/* 承認中チェックイン可能ステータスタイプのID配列 */
	long[] enableCheckinStatusTypeArr = null;
	
	/* 上長のみ表示デフォルト設定ステータスタイプのID配列 */
	long[] defBossOnlyStatusTypeArr = null;
	
	public WorkFlowDocDomain() {
		super();
	}

	/**
	 * コンストラクタ<br>
	 * EIMWorkFlowが保持しているプロパティ値を設定します。<br>
	 * <li>イベントタイプ一覧は空です。
	 * <li>ワークフロー名称一覧には、セッション言語分しか設定されていません。
	 *
	 * @param workFlow ワークフロー
	 * @throws Exception
	 */
	public WorkFlowDocDomain(EIMWorkFlow workFlow) throws Exception {
		super();
		super.setId(workFlow.getId());
		super.setDefName(workFlow.getDefName());
	}

	/**
	 * コンストラクタ<br>
	 * WorkFlowDomainが保持しているプロパティ値を設定します。<br>
	 * <li>イベントタイプ一覧は空です。
	 * <li>ワークフロー名称一覧には、セッション言語分しか設定されていません。
	 *
	 * @param workFlow ワークフロー
	 * @throws Exception
	 */
	public WorkFlowDocDomain(WorkFlowDomain workFlow) throws Exception {
		super();
		super.setId(workFlow.getId());
		super.setNameList(workFlow.getNameList());
		super.setDefName(workFlow.getDefName());
		super.setStatusTypeList(workFlow.getStatusTypeList());
		super.setEventTypeList(workFlow.getEventTypeList());
	}

	/**
	 * defNotifyMailを取得します。
	 * @return defNotifyMail
	 */
	public String getDefNotifyMail() {
	    return defNotifyMail;
	}

	/**
	 * defNotifyMailを設定します。
	 * @param defNotifyMail defNotifyMail
	 */
	public void setDefNotifyMail(String defNotifyMail) {
	    this.defNotifyMail = defNotifyMail;
	}

	/**
	 * defApproveRequestを取得します。
	 * @return defApproveRequest
	 */
	public long getDefApproveRequest() {
	    return defApproveRequest;
	}

	/**
	 * defApproveRequestを設定します。
	 * @param defApproveRequest defApproveRequest
	 */
	public void setDefApproveRequest(long defApproveRequest) {
	    this.defApproveRequest = defApproveRequest;
	}

	/**
	 * publishNotifyMailを取得します。
	 * @return publishNotifyMail
	 */
	public long getPublishNotifyMail() {
	    return publishNotifyMail;
	}

	/**
	 * publishNotifyMailを設定します。
	 * @param publishNotifyMail publishNotifyMail
	 */
	public void setPublishNotifyMail(long publishNotifyMail) {
	    this.publishNotifyMail = publishNotifyMail;
	}

	/**
	 * processWaitPopupを取得します。
	 * @return processWaitPopup
	 */
	public long getProcessWaitPopup() {
	    return processWaitPopup;
	}

	/**
	 * processWaitPopupを設定します。
	 * @param processWaitPopup processWaitPopup
	 */
	public void setProcessWaitPopup(long processWaitPopup) {
	    this.processWaitPopup = processWaitPopup;
	}

	/**
	 * backMailを取得します。
	 * @return backMail
	 */
	public long getBackMail() {
	    return backMail;
	}

	/**
	 * backMailを設定します。
	 * @param backMail backMail
	 */
	public void setBackMail(long backMail) {
	    this.backMail = backMail;
	}


	/**
	 * doPDFConvertを取得します。
	 * @return doPDFConvert
	 */
	public boolean isDoPDFConvert() {
	    return doPDFConvert;
	}

	/**
	 * doPDFConvertを設定します。
	 * @param doPDFConvert doPDFConvert
	 */
	public void setDoPDFConvert(boolean doPDFConvert) {
	    this.doPDFConvert = doPDFConvert;
	}
	
	/**
	 * doPDFDivideを取得します。
	 * @return doPDFDivide
	 */
	public boolean isDoPDFDivide() {
	    return doPDFDivide;
	}

	/**
	 * doPDFDivideを設定します。
	 * @param doPDFDivide doPDFDivide
	 */
	public void setDoPDFDivide(boolean doPDFDivide) {
	    this.doPDFDivide = doPDFDivide;
	}
	
	/**
	 * doPDFURLを取得します。
	 * @return doPDFInsertURL
	 */
	public boolean isDoPDFURL() {
	    return doPDFURL;
	}

	/**
	 * doPDFURLを設定します。
	 * @param doPDFURL 
	 */
	public void setDoPDFURL(boolean doPDFURL) {
	    this.doPDFURL = doPDFURL;
	}

	/**
	 * doSetTermを取得します。
	 * @return doSetTerm
	 */
	public boolean isDoSetTerm() {
	    return doSetTerm;
	}

	/**
	 * doSetTermを設定します。
	 * @param doSetTerm doSetTerm
	 */
	public void setDoSetTerm(boolean doSetTerm) {
	    this.doSetTerm = doSetTerm;
	}

	/**
	 * termNumParamを取得します。
	 * @return termNumParam
	 */
	public int getTermNumParam() {
	    return termNumParam;
	}

	/**
	 * termNumParamを設定します。
	 * @param termNumParam termNumParam
	 */
	public void setTermNumParam(int termNumParam) {
	    this.termNumParam = termNumParam;
	}

	/**
	 * termUnitParamを取得します。
	 * @return termUnitParam
	 */
	public String getTermUnitParam() {
	    return termUnitParam;
	}

	/**
	 * termUnitParamを設定します。
	 * @param termUnitParam termUnitParam
	 */
	public void setTermUnitParam(String termUnitParam) {
	    this.termUnitParam = termUnitParam;
	}

	/**
	 * signatureCommandを取得します。
	 * @return signatureCommand
	 */
	public WorkFlowDocPDFSignatureDomain getSignatureCommand() {
	    return signatureCommand;
	}

	/**
	 * signatureCommandを設定します。
	 * @param signatureCommand signatureCommand
	 */
	public void setSignatureCommand(WorkFlowDocPDFSignatureDomain signatureCommand) {
	    this.signatureCommand = signatureCommand;
	}

	/**
	 * publishNotyfyAssignEntryDomainを取得します。
	 * @return publishNotyfyAssignEntryDomain
	 */
	public AssignEntryDomain getPublishNotyfyAssignEntryDomain() {
	    return publishNotyfyAssignEntryDomain;
	}

	/**
	 * publishNotyfyAssignEntryDomainを設定します。
	 * @param AssignEntryDomain publishNotyfyAssignEntryDomain
	 */
	public void setPublishNotyfyAssignEntryDomain(AssignEntryDomain val) {
	    this.publishNotyfyAssignEntryDomain = val;
	}


	/**
	 * ワークフロー 多言語名称 設定
	 * 
	 * @param otherList
	 * @throws Exception
	 */
	public void setOtherList(List<EIMOtherName> otherList) throws Exception{
		
		List<OtherNameDomain> otherNameDomainList = new ArrayList<OtherNameDomain>();
		for (int i=0; i < otherList.size(); i++){
			OtherNameDomain otherNameDomain = new OtherNameDomain(otherList.get(i));
			otherNameDomainList.add(otherNameDomain);
		}

		super.setNameList(otherNameDomainList);
	}

	/**
	 * WorkFlowDomain 設定<br>
	 * 
	 * 
	 * <li>ステータスタイプリスト、イベントタイプリスト、を設定します。
	 * <li>ワークフロー名称(デフォルト)を設定します。
	 *
	 * @param workFlow ワークフロー
	 * @throws Exception
	 */
	public void setWorkFlowDomain(WorkFlowDomain workFlow) throws Exception {
		super.setId(workFlow.getId());
		super.setDefName(workFlow.getDefName());
		super.setStatusTypeList(workFlow.getStatusTypeList());
		super.setEventTypeList(workFlow.getEventTypeList());
	}
	
	/**
	 * WorkFlowDomain ワークフロー設定オブジェクト情報を設定します。
	 *
	 * @param workFlowSettingObj ワークフロー設定オブジェクト
	 * @throws Exception
	 */
	public void setWorkFlowSetting(EIMObject workFlowSettingObj) throws Exception {
		
		if(workFlowSettingObj != null) {
			this.setDefNotifyMail(workFlowSettingObj.getAttribute(EIMConfig.get("ATTR_NAME_WFSETTING_MAIL_NOTICE_DFLT")).getStrings()[0]);
			this.setDefApproveRequest(workFlowSettingObj.getAttribute(EIMConfig.get("ATTR_NAME_WFSETTING_SETTING_FLG")).getInts()[0]);
			this.setProcessWaitPopup(workFlowSettingObj.getAttribute(EIMConfig.get("ATTR_NAME_WFSETTING_POPUP_NOTICE_FLG")).getInts()[0]);
			this.setBackMail(workFlowSettingObj.getAttribute(EIMConfig.get("ATTR_NAME_WFSETTING_MAIL_NOTICE_FLG")).getInts()[0]);
			EIMAttribute attributeOcr = workFlowSettingObj.getAttribute(EIMConfig.get("ATTR_NAME_OCR_SETTING_EXISTENCE"));
			if(attributeOcr != null){
				this.setDefOcr(attributeOcr.getInts()[0]);
			}else{
				this.setDefOcr(0);
			}
			EIMAttribute attr = workFlowSettingObj.getAttribute(EIMConfig.get("ATTR_NAME_WFSETTING_PUBLISHNOTIFY_FLG"));
			if( attr == null ) {
				this.setPublishNotifyMail(0);
			}
			else {
				this.setPublishNotifyMail(attr.getInts()[0]);
			}
			attr = workFlowSettingObj.getAttribute(EIMConfig.get("ATTR_NAME_WFSETTING_BOSS_APPROVAL_FLG"));
			if( attr == null ) {
				this.setDefBossApproval("necessary");
			}
			else {
				this.setDefBossApproval(attr.getStrings()[0]);
			}

			EIMAttribute enableCheckinStatusAttr = workFlowSettingObj.getAttribute(EIMConfig.get("ATTR_NAME_ENABLE_CHECKIN_STATUS"));
			if (enableCheckinStatusAttr != null) {
				this.setEnableCheckinStatusTypeArr(TypeConvertUtils.convertToLongArray(enableCheckinStatusAttr.getInts()));
			}
			
			EIMAttribute defBossOnlyStatusAttr = workFlowSettingObj.getAttribute(EIMConfig.get("ATTR_NAME_BOSS_ONLY_DEFAULT_STATUS"));
			if (defBossOnlyStatusAttr != null) {
				this.setDefBossOnlyStatusTypeArr(TypeConvertUtils.convertToLongArray(defBossOnlyStatusAttr.getInts()));
			}
		}
		
	}

	/**
	 * bossApprovalを取得します。
	 * @return defApproveRequest
	 */
	public String getDefBossApproval() {
	    return bossApproval;
	}

	/**
	 * bossApprovalを設定します。
	 * @param bossApproval bossApproval
	 */
	public void setDefBossApproval(String bossApproval) {
	    this.bossApproval = bossApproval;
	}

	/**
	 * isCreateCopyを設定します。
	 * @param isCreateCopy
	 */
	public void setCreateCopy(boolean isCreateCopy) {
		this.isCreateCopy = isCreateCopy;
	}

	/**
	 * isCreateCopyを取得します。
	 * @return isCreateCopy
	 */
	public boolean isCreateCopy() {
		return isCreateCopy;
	}

	/**
	 * originWorkflowIdを設定します。
	 * @param originWorkflowId
	 */
	public void setOriginWorkflowId(long originWorkflowId) {
	    this.originWorkflowId = originWorkflowId;
	}

	/**
	 * originWorkflowIdを取得します。
	 * @return originWorkflowId
	 */
	public long getOriginWorkflowId() {
	    return originWorkflowId;
	}

	/**
	 * defOcrを取得します。
	 * @return defOcr
	 */
	public long getDefOcr() {
		return defOcr;
	}

	/**
	 * defOcrを設定します。
	 * @param defOcr
	 */
	public void setDefOcr(long defOcr) {
		this.defOcr = defOcr;
	}
	
	/**
	 * enableCheckinStatusTypeArrを設定します。
	 * @param enableCheckinStatusTypeArr
	 */
	public void setEnableCheckinStatusTypeArr(long[] enableCheckinStatusTypeArr) {
	    this.enableCheckinStatusTypeArr = enableCheckinStatusTypeArr;
	}

	/**
	 * enableCheckinStatusTypeArrを取得します。
	 * @return enableCheckinStatusTypeArr
	 */
	public long[] getEnableCheckinStatusTypeArr() {
	    return enableCheckinStatusTypeArr;
	}
	
	/**
	 * defBossOnlyStatusTypeArrを設定します。
	 * @param defBossOnlyStatusTypeArr
	 */
	public void setDefBossOnlyStatusTypeArr(long[] defBossOnlyStatusTypeArr) {
		this.defBossOnlyStatusTypeArr = defBossOnlyStatusTypeArr;
	}
	
	/**
	 * defBossOnlyStatusTypeArrを取得します。
	 * @return defBossOnlyStatusTypeArr
	 */
	public long[] getDefBossOnlyStatusTypeArr() {
		return defBossOnlyStatusTypeArr;
	}
}
