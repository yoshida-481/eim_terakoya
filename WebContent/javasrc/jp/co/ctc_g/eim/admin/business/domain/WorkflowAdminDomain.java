package jp.co.ctc_g.eim.admin.business.domain;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import jp.co.ctc_g.eim.app.form.business.domain.StatusTypeLayoutDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.OtherNameDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.WorkFlowDomain;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * カスタム属性(レイアウト情報)を含むワークフロー定義ドメイン
 * @since Ver6.0
 */
public class WorkflowAdminDomain extends WorkFlowDomain {
	
	/** レイアウト情報を保持したステータスタイプのマップ(key:ステータスタイプのsequence) */
	private Map<Integer, StatusTypeLayoutDomain> statusTypeLayoutMap = new HashMap<Integer, StatusTypeLayoutDomain>();
	
	/**
	 * コンストラクタ。<br>
	 * プロパティ値は全てデフォルト値となります。
	 * @since Ver6.0
	 */
	public WorkflowAdminDomain() {
		super();
	}
	
	/**
	 * コンストラクタ<br>
	 * WorkFlowDomainが保持しているプロパティ値を設定します。<br>
	 *
	 * @param workFlow ワークフロー
	 * @throws Exception
	 * @since Ver6.0
	 */
	public WorkflowAdminDomain(WorkFlowDomain workFlow) throws Exception {
		super();
		super.setId(workFlow.getId());
		super.setNameList(workFlow.getNameList());
		super.setDefName(workFlow.getDefName());
		super.setStatusTypeList(workFlow.getStatusTypeList());
		super.setEventTypeList(workFlow.getEventTypeList());
	}

	/**
	 * statusTypeLayoutMapを取得します。
	 * @return statusTypeLayoutMap
	 * @since Ver6.0
	 */
	public Map<Integer, StatusTypeLayoutDomain> getStatusTypeLayoutMap() {
		return statusTypeLayoutMap;
	}

	/**
	 * statusTypeLayoutMapを設定します。
	 * @param statusTypeLayoutList レイアウト情報を保持したステータスタイプのマップ
	 * @since Ver6.0
	 */
	public void setStatusTypeLayoutMap(
			Map<Integer, StatusTypeLayoutDomain> statusTypeLayoutMap) {
			this.statusTypeLayoutMap = statusTypeLayoutMap;
	}
	
	/**
	 * ワークフローのセッション言語名称を取得します。
	 * @return ワークフローのセッション言語名称
	 * @since Ver6.0
	 */
	public String getName() throws Exception {
		
		for (OtherNameDomain otherName : super.getNameList()) {
			
			if (otherName.getLangId().equals(EIMThreadContext.getEIMSession().getLangId())) {
				
				return otherName.getName();
			}
		}
		
		return "";
	}
}