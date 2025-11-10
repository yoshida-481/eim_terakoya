package jp.co.ctc_g.eim.common.presentation.web.dto;

import jp.co.ctc_g.eim.common.presentation.web.dto.BaseEventTypeDTO.BaseEventTypeList;
import jp.co.ctc_g.eim.common.presentation.web.dto.GuardConditionConfDTO.GuardConditionList;
import jp.co.ctc_g.eim.common.presentation.web.dto.StatusTypeKindDTO.StatusTypeKindList;
import jp.co.ctc_g.eim.common.presentation.web.dto.SysFuncDTO.SysFuncList;
import jp.co.ctc_g.eim.framework.business.domain.WorkFlowConfDomain;

/**
 * ワークフロー定義DTO
 * <p>
 * Struts->SpringMVCへの移行に際して、クライアントへ送信するXMLデータを再現したデータ転送用オブジェクトです。
 */
public class WorkFlowConfDTO {

	/** ベースイベントタイプリスト */
	private BaseEventTypeList baseEventTypeList = null;
	/** ガード条件リスト */
	private GuardConditionList guardConditionList = null;
	/** ステータスタイプ種別リスト */
	private StatusTypeKindList statusTypeKindList = null;
	/** システム処理リスト */
	private SysFuncList sysFuncList = null;
	
	/**
	 * コンストラクタ
	 * 
	 * @param workFlowConfDomain
	 */
	public WorkFlowConfDTO(WorkFlowConfDomain workFlowConfDomain) {

		setBaseEventTypeList(new BaseEventTypeList(workFlowConfDomain.getBaseEventTypeList()));
		setGuardConditionList(new GuardConditionList(workFlowConfDomain.getGuardConditionList()));
		setStatusTypeKindList(new StatusTypeKindList(workFlowConfDomain.getStatusTypeKindList()));
		setSysFuncList(new SysFuncList(workFlowConfDomain.getSysFuncList()));
	}

	/**
	 * @return baseEventTypeListを取得します。
	 */
	public BaseEventTypeList getBaseEventTypeList() {
		return baseEventTypeList;
	}

	/**
	 * @param baseEventTypeListを設定します。
	 */
	public void setBaseEventTypeList(BaseEventTypeList baseEventTypeList) {
		this.baseEventTypeList = baseEventTypeList;
	}

	/**
	 * @return guardConditionListを取得します。
	 */
	public GuardConditionList getGuardConditionList() {
		return guardConditionList;
	}

	/**
	 * @param guardConditionListを設定します。
	 */
	public void setGuardConditionList(GuardConditionList guardConditionList) {
		this.guardConditionList = guardConditionList;
	}

	/**
	 * @return statusTypeKindListを取得します。
	 */
	public StatusTypeKindList getStatusTypeKindList() {
		return statusTypeKindList;
	}

	/**
	 * @param statusTypeKindListを設定します。
	 */
	public void setStatusTypeKindList(StatusTypeKindList statusTypeKindList) {
		this.statusTypeKindList = statusTypeKindList;
	}

	/**
	 * @return sysFuncListを取得します。
	 */
	public SysFuncList getSysFuncList() {
		return sysFuncList;
	}

	/**
	 * @param sysFuncListを設定します。
	 */
	public void setSysFuncList(SysFuncList sysFuncList) {
		this.sysFuncList = sysFuncList;
	}
}
