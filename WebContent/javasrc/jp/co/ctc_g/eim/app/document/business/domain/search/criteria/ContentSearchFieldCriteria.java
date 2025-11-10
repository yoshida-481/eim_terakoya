package jp.co.ctc_g.eim.app.document.business.domain.search.criteria;

/**
 * 検索エンジンフィールドに対する検索条件です。
 */
public class ContentSearchFieldCriteria {

	/**
	 * フィールド名です。
	 */
	private String fieldName = null;

	/**
	 * @return
	 */
	public String getFieldName() {
		return fieldName;
	}

	/**
	 * @param fieldName
	 */
	public void setFieldName(String fieldName) {
		this.fieldName = fieldName;
	}

}