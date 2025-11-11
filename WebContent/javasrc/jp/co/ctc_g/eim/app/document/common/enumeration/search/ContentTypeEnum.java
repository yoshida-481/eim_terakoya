/**
 *
 */
package jp.co.ctc_g.eim.app.document.common.enumeration.search;

/**
 * コンテンツの種別を表す列挙型です。
 */
public enum ContentTypeEnum {

	DOCUMENT("ドキュメント"),
	FOLDER("フォルダ"),
	TAG("タグ");


	/** コンテンツタイプ名 */
	private final String value;

	/**
	 * コンストラクタです。
	 * @param value コンテンツタイプ名
	 */
	private ContentTypeEnum(String value) {
		this.value = value;
	}

	/**
	 * データタイプ名を取得します。
	 * @return コンテンツタイプ列挙型
	 */
	public String getValue() {
		return value;
	}

}
