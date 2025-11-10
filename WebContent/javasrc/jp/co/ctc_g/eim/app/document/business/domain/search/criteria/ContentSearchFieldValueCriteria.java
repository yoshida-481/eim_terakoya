package jp.co.ctc_g.eim.app.document.business.domain.search.criteria;

import java.util.List;

/**
 * 検索エンジンフィールドに対する値比較検索条件です。
 */
public class ContentSearchFieldValueCriteria extends ContentSearchFieldCriteria {

	private List<String> valueList = null;

	public List<String> getValueList() {
		return valueList;
	}

	public void setValueList(List<String> valueList) {
		this.valueList = valueList;
	}

}