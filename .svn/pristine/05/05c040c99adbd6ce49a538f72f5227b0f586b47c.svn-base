package jp.co.ctc_g.eim.admin.business.dao;

import java.util.List;

import jp.co.ctc_g.eim.admin.business.domain.CacheNodeDomain;
import jp.co.ctc_g.eim.admin.business.domain.criteria.CacheEntrySearchCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.cache.CacheEntrySearchDomain;
import jp.co.ctc_g.eim.framework2.business.domain.cache.CacheSpaceDomain;

/**
*
* キャッシュモニタービューDAO
*
*/
public interface CacheMonitorViewDao {

	/**
	 * ノードの一覧を取得します。
	 *
	 * @return ノードの一覧
	 * @throws Exception  処理中にエラーが発生した場合、例外を通知します。
	 */
	List<CacheNodeDomain> getNodes() throws Exception;

	/**
	 * ノードIDを検索条件に、キャッシュ領域の一覧を取得します。
	 *
	 * @param nodeId キャッシュ領域を取得するノードのID
	 * @return キャッシュ領域の一覧
	 * @throws Exception  処理中にエラーが発生した場合、例外を通知します。
	 */
	List<CacheSpaceDomain> getCaches(long nodeId) throws Exception;

	/**
	 * 指定された検索条件でキャッシュエントリー検索結果を取得します。
	 *
	 * @param cacheEntrySearchCriteria 取得するキャッシュエントリーの検索条件
	 * @return キャッシュエントリー検索結果
	 * @throws Exception  処理中にエラーが発生した場合、例外を通知します。
	 */
	CacheEntrySearchDomain searchEntries(CacheEntrySearchCriteria cacheEntrySearchCriteria) throws Exception;

	/**
	 * 指定されたPKのキャッシュエントリー(ドメイン)を更新ログに書き込みます。
	 * 操作履歴をpkList数分出力しているため、引数を変更する場合は注意してください。
	 * @param cacheSpaceKey キャッシュ領域キー
	 * @param pkList リロードするキャッシュエントリーのPK一覧
	 * @throws Exception  処理中にエラーが発生した場合、例外を通知します。
	 */
	void reload(String cacheSpaceKey, List<String> pkList) throws Exception;

}
