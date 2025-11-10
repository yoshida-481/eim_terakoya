package jp.co.ctc_g.eim.admin.business.service;

import jp.co.ctc_g.eim.admin.business.domain.WorkflowAdminDomain;

/**
 * 汎用、帳票用システム管理のワークフローに関する機能を提供するサービスです。
 * @since Ver6.0
 */
public interface WorkFlowDefAdminService {

	/**
	 * IDを指定して、ワークフロー定義を取得します。<br>
     * 帳票用システム管理の場合はステータスに割り当てられた属性の表示順情報も取得します。<br>
     *
     * @param adminAppId システム管理種別ID
     * @param namespace ネームスペース
     * @param workFlowId ワークフローID
     * @return レイアウト属性を含むワークフロー定義情報
	 * @since Ver6.0
	 */
	WorkflowAdminDomain getDefById(String adminAppId, String namespace, long workFlowId) throws Exception;
	
	/**
	 * ワークフロー定義を更新します。<br>
     * 帳票用システム管理の場合はステータスに割り当てられた属性の表示順情報も更新します。<br>
     *
     * @param adminAppId システム管理種別ID
     * @param workFlowAdmin ワークフロー定義更新情報
	 * @since Ver6.0
	 */
	void updateDef(String adminAppId, WorkflowAdminDomain workFlowAdmin) throws Exception;
	
	/**
	 * ワークフロー定義を複写します。<br>
     * 帳票用システム管理の場合はステータスに割り当てられた属性の表示順情報も複写します。<br>
     *
     * @param adminAppId システム管理種別ID
     * @param srcWorkFlow 複写元のワークフロー定義情報
     * @return レイアウト属性を含む複写したワークフロー定義情報
	 * @since Ver6.0
	 */
	WorkflowAdminDomain copyDef(String adminAppId, WorkflowAdminDomain workFlow) throws Exception;
}