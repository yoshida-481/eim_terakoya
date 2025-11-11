package jp.co.ctc_g.eim.app.document.business.domain.search.criteria;

import java.util.List;

import jp.co.ctc_g.eim.framework2.business.domain.criteria.RangeCriteria;

/**
 * 検索エンジンフィールドに対する範囲指定検索条件です。
 * @param <T>
 *
 */
public class ContentSearchFieldRangeCriteria<T> extends ContentSearchFieldCriteria {

	private List<RangeCriteria<T>> rangeList = null;

	public List<RangeCriteria<T>> getRangeList() {
		return rangeList;
	}

	public void setRangeList(List<RangeCriteria<T>> rangeList) {
		this.rangeList = rangeList;
	}

}