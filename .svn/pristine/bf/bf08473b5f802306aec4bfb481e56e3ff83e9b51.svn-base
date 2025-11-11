package jp.co.ctc_g.eim.app.document.business.service;

import java.util.List;

import jp.co.ctc_g.eim.app.document.business.domain.FolderDomain;
import jp.co.ctc_g.eim.app.document.business.domain.PlaceDomain;
import jp.co.ctc_g.eim.app.document.business.domain.criteria.FolderCriteria;

/**
* 【ドキュメントAPI】
* フォルダサービスクラス
*
*/
public interface FolderService
{

	/**
	 * フォルダIDを指定してフォルダを取得します。
	 *
	 * @param id フォルダID
	 * @return フォルダドメイン
	 * @throws Exception
	 */
	public FolderDomain getById(long id) throws Exception;

	/**
	 * パスを指定してフォルダを取得します。
	 *
	 * @param path パス
	 * @return フォルダドメイン
	 * @throws Exception
	 */
	public FolderDomain getByPath(String path) throws Exception;

	/**
	 * 指定した条件のフォルダを取得します。
	 *
	 * @param criteria フォルダクライテリア
	 * @return フォルダドメインリスト
	 * @throws Exception
	 */
	public List<FolderDomain> getList(FolderCriteria criteria) throws Exception;

	/**
	 * フォルダを作成します。
	 *
	 * @param folderDomain フォルダドメイン
	 * @param placeDomain ワークスペースドメインまたはフォルダドメイン
	 * @return フォルダドメイン
	 * @throws Exception
	 */
	public FolderDomain create(FolderDomain folderDomain, PlaceDomain placeDomain) throws Exception;

	/**
	 * フォルダの名称と属性、セキュリティを更新します。
	 *
	 * @param folderDomain フォルダドメイン
	 * @throws Exception
	 */
	public void update(FolderDomain folderDomain) throws Exception;

	/**
	 * 指定したフォルダをごみ箱に移動します。
	 *
	 * @param folderDomainList フォルダドメインリスト
	 * @throws Exception
	 */
	public void dispose(List<FolderDomain> folderDomainList) throws Exception;

	/**
	 * 指定したフォルダを削除します。
	 *
	 * @param folderDomainList フォルダドメインリスト
	 * @throws Exception
	 */
	public void delete(List<FolderDomain> folderDomainList) throws Exception;

	/**
	 * 指定したフォルダをPlaceDomainの下に移動します。
	 *
	 * @param folderDomain フォルダドメイン
	 * @param placeDomain ワークスペースドメインまたはフォルダドメイン
	 * @throws Exception
	 */
	public void move(FolderDomain folderDomain, PlaceDomain placeDomain) throws Exception;

}