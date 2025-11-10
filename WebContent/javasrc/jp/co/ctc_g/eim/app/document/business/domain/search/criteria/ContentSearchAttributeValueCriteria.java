package jp.co.ctc_g.eim.app.document.business.domain.search.criteria;

/**
 * オブジェクト検索における属性値条件クラスです。
 * @param <T>
 */
public class ContentSearchAttributeValueCriteria extends ContentSearchAttributeCriteria {

	/** 等価条件 */
	private String value = null;

	public String getValue() {
		return value;
	}

	public void setValue(String value) {
		this.value = value;
	}

}
