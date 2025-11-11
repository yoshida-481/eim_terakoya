package jp.co.ctc_g.eim.app.document.business.service;

import jp.co.ctc_g.eim.app.document.business.domain.WorkFlowDocDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.WorkFlowDomain;
import jp.co.ctc_g.eim.framework.business.service.WorkFlowDefService;

/**
*
* ワークフロー定義サービスクラス
*
*/
public interface WorkFlowDefDocService extends WorkFlowDefService
{

	/**
	 * ワークフローIDからワークフロー定義を取得します。
	 *
	 * @param workFlowId ワークフローID
	 * @return ワークフロー定義
	 * @throws Exception
	 */
	public WorkFlowDocDomain getDefById(long workFlowId) throws Exception;

	/**
	 * ワークフロー定義(ステータス構成を含む)を新規作成します
	 *
	 * @param workFlowDefDomain ワークフロー
	 * @throws Exception
	 */
	public void create(WorkFlowDocDomain workFlowDocDomain) throws Exception;

	/**
	 * ワークフロー定義を編集します
	 *
	 * @param workFlowDefDomain ワークフロー
	 * @throws Exception
	 */
	public void updateWorkFlowDef(WorkFlowDocDomain workFlowDocDomain) throws Exception;

	/**
	 * ステータス構成を編集します
	 *
	 * @param workFlowDefDomain ワークフロー
	 * @throws Exception
	 */
	public void updateConfigurationStatus(WorkFlowDocDomain workFlowDocDomain) throws Exception;

	/**
	 * ワークフロー定義(ステータス構成を含む)を削除します
	 *
	 * @param workFlowDefDomain ワークフロー
	 * @throws Exception
	 */
	public void delete(WorkFlowDocDomain workFlowDocDomain) throws Exception;

	/**
	 * ステータス名称とアサインエントリを編集します
	 *
	 * @param workFlowDefDomain ワークフロー
	 * @throws Exception
	 */
	public void updateStatusNameAndAssigns(WorkFlowDocDomain workFlowDocDomain, boolean updateFlg) throws Exception;

}