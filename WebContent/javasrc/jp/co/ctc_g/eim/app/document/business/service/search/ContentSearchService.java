/**
 *
 */
package jp.co.ctc_g.eim.app.document.business.service.search;

import java.util.List;

import jp.co.ctc_g.eim.app.document.business.domain.search.criteria.ContentSearchCriteria;
import jp.co.ctc_g.eim.search.core.searchApi.business.domain.SearchResultsDomain;

/**
 * 検索エンジンに登録されているコンテンツ情報(ドキュメント/フォルダ/タグ)を条件に従い検索取得するためのサービスインターフェイスです。
 */
public interface ContentSearchService {

	/**
	 * 検索エンジンに登録されているコンテンツ情報(ドキュメント/フォルダ/タグ)を条件に従い検索取得します。
	 * @param criteria 検索条件
	 * @return 検索結果
	 * @throws Exception
	 */
	SearchResultsDomain search(ContentSearchCriteria criteria) throws Exception;

	/**
	 * キーワード検索においてキーワード文字列を補完して提案します。
	 * @param letters キーワード文字列
	 * @param parentCriteria オブジェクトスキーマに対する検索条件
	 * @return サジェストキーワードリスト
	 * @throws Exception
	 */
	List<String> suggest(String letters, ContentSearchCriteria parentCriteria) throws Exception;

	/**
	 * キーワード検索においてPDFファイルの該当ページ番号を特定し取得します。
	 * @param keyword キーワード文字列
	 * @param objectId 検索対象のPDFファイルのオブジェクトID
	 * @return ページ番号リスト
	 * @throws Exception
	 */
	List<Integer> searchPages(String keyword, long objectId) throws Exception;

}
