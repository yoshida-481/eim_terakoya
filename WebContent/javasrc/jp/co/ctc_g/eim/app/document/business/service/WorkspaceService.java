package jp.co.ctc_g.eim.app.document.business.service;

import java.util.List;

import jp.co.ctc_g.eim.app.document.business.domain.WorkspaceDomain;
import jp.co.ctc_g.eim.app.document.business.domain.criteria.WorkspaceCriteria;



/**
* 【ドキュメントAPI】
* ワークスペースサービスクラス
*
*/
public interface WorkspaceService
{

	/**
	 * ワークスペースIDを指定してワークスペースを取得します。
	 *
	 * @param id ワークスペースID
	 * @return ワークスペースドメイン
	 * @throws Exception
	 */
	public WorkspaceDomain getById(long id) throws Exception;

	/**
	 * ワークスペース名称を指定してワークスペースを取得します。
	 *
	 * @param workspaceName ワークスペース名
	 * @return ワークスペースドメイン
	 * @throws Exception
	 */
	public WorkspaceDomain getByName(String workspaceName) throws Exception;

	/**
	 * ワークスペース名称とワークスペースIDを指定してワークスペースを取得します。
	 *
	 * @param criteria ワークスペースクライテリア
	 * @return ワークスペースドメインリスト
	 * @throws Exception
	 */
	public List<WorkspaceDomain> getList(WorkspaceCriteria criteria) throws Exception;

	/**
	 * ワークスペースを作成します。
	 *
	 * @param workspaceDomain ワークスペースドメイン
	 * @return ワークスペースドメイン
	 * @throws Exception
	 */
	public WorkspaceDomain create(WorkspaceDomain workspaceDomain) throws Exception;

	/**
	 * ワークスペースの名称と属性、セキュリティを更新します。
	 *
	 * @param workspaceDomain ワークスペースドメイン
	 * @throws Exception
	 */
	public void update(WorkspaceDomain workspaceDomain) throws Exception;

	/**
	 * ワークスペースを削除します。ワークスペース名称もしくはオブジェクトIDを指定します。
	 *
	 * @param worksapceDomainList ワークスペースドメインリスト
	 * @throws Exception
	 */
	public void delete(List<WorkspaceDomain> worksapceDomainList) throws Exception;

}