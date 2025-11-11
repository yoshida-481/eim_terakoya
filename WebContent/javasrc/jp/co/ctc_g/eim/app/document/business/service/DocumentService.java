package jp.co.ctc_g.eim.app.document.business.service;

import java.io.InputStream;
import java.util.List;

import jp.co.ctc_g.eim.app.document.business.domain.DocumentDomain;
import jp.co.ctc_g.eim.app.document.business.domain.PlaceDomain;
import jp.co.ctc_g.eim.app.document.business.domain.criteria.DocumentCriteria;

/**
* 【ドキュメントAPI】
* ドキュメントサービスクラス
*
*/
public interface DocumentService
{

	/**
	 * ドキュメントIDを指定してドキュメントを取得します。
	 *
	 * @param id ドキュメントID
	 * @return ドキュメント
	 * @throws Exception
	 */
	public DocumentDomain getById(long id) throws Exception;

	/**
	 * パスを指定してドキュメントを取得します。
	 *
	 * @param path パス
	 * @return ドキュメントドメイン
	 * @throws Exception
	 */
	public DocumentDomain getByPath(String path) throws Exception;

	/**
	 * 指定した条件のドキュメントを取得します。
	 *
	 * @param docCriteria ドキュメントクライテリア
	 * @return ドキュメントドメインリスト
	 * @throws Exception
	 */
	public List<DocumentDomain> getList(DocumentCriteria docCriteria) throws Exception;

	/**
	 * ドキュメントを作成します。
	 *
	 * @param documentDomain ドキュメントドメイン
	 * @param placeDomain ワークスペースドメイン又はフォルダドメイン
	 * @param inputStream InputStream
	 * @return ドキュメントドメイン
	 * @throws Exception
	 */
	public DocumentDomain create(DocumentDomain documentDomain, PlaceDomain placeDomain, InputStream inputStream) throws Exception;

	/**
	 * 原本ファイルを取得します。
	 *
	 * @param documentDomain ドキュメントドメイン
	 * @return InputStream
	 * @throws Exception
	 */
	public InputStream getOriginalFile(DocumentDomain documentDomain) throws Exception;

	/**
	 * 公開ファイルを取得します。
	 *
	 * @param documentDomain ドキュメントドメイン
	 * @return InputStream
	 * @throws Exception
	 */
	public InputStream getPublicFile(DocumentDomain documentDomain) throws Exception;

	/**
	 * ドキュメントの名称と属性、セキュリティを更新します。
	 *
	 * @param documentDomain ドキュメントドメイン
	 * @throws Exception
	 */
	public void update(DocumentDomain documentDomain) throws Exception;

	/**
	 * ドキュメントを改訂します。
	 *
	 * @param documentDomain ドキュメントドメイン
	 * @param inputStream InputStream
	 * @throws Exception
	 */
	public void checkin(DocumentDomain documentDomain, InputStream inputStream) throws Exception;

	/**
	 * ドキュメントを改訂中に変更します。
	 *
	 * @param documentDomain ドキュメントドメイン
	 * @return DocumentDomain
	 * @throws Exception
	 */
	public DocumentDomain checkout(DocumentDomain documentDomain) throws Exception;

	/**
	 * ドキュメントの改訂中を取消しします。
	 *
	 * @param documentDomain ドキュメントドメイン
	 * @throws Exception
	 */
	public void cancelCheckout(DocumentDomain documentDomain) throws Exception;

	/**
	 * ドキュメントをごみ箱に移動します。
	 *
	 * @param documentDomainList ドキュメントドメインリスト
	 * @throws Exception
	 */
	public void dispose(List<DocumentDomain> documentDomainList) throws Exception;


	/**
	 * ドキュメントを削除します。
	 *
	 * @param documentDomainList ドキュメントドメインリスト
	 * @throws Exception
	 */
	public void delete(List<DocumentDomain> documentDomainList) throws Exception;

	/**
	 * 指定したドキュメントの全バージョンのドキュメントを取得します。<br/>
	 *
	 * @param document ドキュメント
	 * @return ドキュメントリスト
	 * @throws Exception
	 */
	public List<DocumentDomain> getRevisionList(DocumentDomain document) throws Exception;


	/**
	 * ドキュメントリストの対象バージョンを削除します。
	 *
	 * @param documentDomainList ドキュメントドメイン
	 * @throws Exception
	 */
	public void deleteRevision(List<DocumentDomain> documentDomainList) throws Exception;

	/**
	 * 指定したドキュメントをPlaceDomainの下に移動します。
	 *
	 * @param documentDomain ドキュメントドメイン
	 * @param placeDomain ワークスペースドメイン又はフォルダドメイン
	 * @throws Exception
	 */
	public void move(DocumentDomain documentDomain, PlaceDomain placeDomain) throws Exception;

	/**
	 * チェックアウト中のファイル原本を差し替えます。
	 *
	 * @param documentDomain ドキュメントドメイン
	 * @param inputStream InputStream
	 * @throws Exception
	 */
	public void replaceOriginalFile(DocumentDomain documentDomain, InputStream inputStream) throws Exception;

	/**
	 * 公開ファイルを差し替えます。
	 *
	 * @param documentDomain ドキュメントドメイン
	 * @param inputStream InputStream
	 * @throws Exception
	 */
	public void replacePublicFile(DocumentDomain documentDomain, InputStream inputStream) throws Exception;

	/**
	 * 指定したドキュメントをPlaceDomainの下にコピーします。
	 *
	 * @param documentDomain ドキュメントドメイン
	 * @param placeDomain ワークスペースドメイン又はフォルダドメイン
	 * @throws Exception
	 */
	//public void copy(DocumentDomain documentDomain, PlaceDomain placeDomain) throws Exception;
}