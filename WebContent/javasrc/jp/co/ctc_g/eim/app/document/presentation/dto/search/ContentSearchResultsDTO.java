/**
 *
 */
package jp.co.ctc_g.eim.app.document.presentation.dto.search;

import java.util.ArrayList;
import java.util.List;

import jp.co.ctc_g.eim.search.core.searchApi.business.domain.FacetFieldDomain;

/**
 * コンテンツ検索結果返却用DTOです。
 */
public class ContentSearchResultsDTO {

	/** ヒット件数 */
	private long numFounds = 0;

	/** 検索結果コンテンツリスト */
	private List<ContentSearchRecordDTO> contentList = new ArrayList<>();

	/** ファセットフィールドリスト */
	private List<FacetFieldDomain> facetFieldList = new ArrayList<>();

	/**
	 * ヒット件数を取得します。
	 * @return ヒット件数
	 */
	public long getNumFounds() {
		return numFounds;
	}

	/**
	 * ヒット件数を設定します。
	 * @param numFounds ヒット件数
	 */
	public void setNumFounds(long numFounds) {
		this.numFounds = numFounds;
	}

	/**
	 * 検索結果コンテンツリストを取得します。
	 * @return 検索結果コンテンツリスト
	 */
	public List<ContentSearchRecordDTO> getContentList() {
		return contentList;
	}

	/**
	 * 検索結果コンテンツリストを設定します。
	 * @param contentList 検索結果コンテンツリスト
	 */
	public void setContentList(List<ContentSearchRecordDTO> contentList) {
		this.contentList = contentList;
	}

	/**
	 * ファセットフィールドリストを取得します。
	 * @return ファセットフィールドリスト
	 */
	public List<FacetFieldDomain> getFacetFieldList() {
		return facetFieldList;
	}

	/**
	 * ファセットフィールドリストを設定します。
	 * @param facetFieldList ファセットフィールドリスト
	 */
	public void setFacetFieldList(List<FacetFieldDomain> facetFieldList) {
		this.facetFieldList = facetFieldList;
	}

}
