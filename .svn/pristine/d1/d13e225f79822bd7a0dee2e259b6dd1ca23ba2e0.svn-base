package jp.co.ctc_g.eim.app.document.common.enumeration.search;

import java.util.Arrays;

import jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext;

/**
 * コンテンツ(オブジェクト)スキーマフィールドを表す列挙型です。
 */
public enum ContentSearchFieldEnum {

	// EIMANAGER 標準フィールド

	KEY(""),
	ID("文書ID"),
	SITE_CODE(""),
	SYSTEM_KIND(""),
	DATA_KIND(""),
	DATA_TYPE(""),
	APP_KIND(""),
	ACL_ACCEPT(""),
	ACL_REJECT(""),
	OBJECT_NAME(""),
	OBJECTTYPE_LANG_ID_NAME(""),
	MODIFY_USER_LANG_ID_NAME("更新者"),
	MODIFY_DATE("更新日"),
	CREATE_USER_LANG_ID_NAME("作成者"),
	CREATE_DATE("作成日"),
	LOCK_USER_LANG_ID_NAME(""),
	LOCK_DATE(""),
	STATUSTYPE_KIND(""),
	STATUSTYPE_LANG_ID_NAME(""),
	REVISION(""),
	LATEST(""),
	DOC_FULL_TEXT(""),
	FILE_SIZE("サイズ"),
	EXTENSION(""),
	SEQ(""),
	KEYWORD_ATTRS(""),
	KEYWORD_FULL_TEXTS(""),

	// EIMANAGER ドキュメント管理用追加フィールド

	EFFECTIVE_TERM("有効期限"),
	PUBLIC_PROC_FAILURE("公開処理失敗"),
	FAILED_TO_PDF_MERGE("PDF結合処理失敗"),
	SIGNENCR("署名・暗号化状態"),
	SIGNATURE_AND_ENCRYPTION_VER("署名・暗号化バージョン"),
	PATH("パス"),
	EMPTY_FOLDER(""),
	PUBLIC(""),
	PROPERTY("プロパティ"),
	REVISED_CONTENT("改訂内容"),
	WF_ATTACHED_FOLDER_ID("上位WFフォルダ"),
	LINK_PARENT_ID("リンク先"),
	LINK_UPDATE_TIMING("リンク更新タイミング"),
	PDF_CONV_EXEC_DATE("PDF変換処理実行日時"),
	PDF_PRE_REGIST_DATE("公開PDF事前登録日時"),
	OCR_PROC_STATUS("OCR処理ステータス"),
	OCR_RESULT_STATUS("OCR結果ステータス"),
	AUTO_NUMBER("番号"),
	ATTACHMENT_NAMES(""),
	ATTACHMENT_IDS(""),
	ATTACHMENT_FULL_TEXTS(""),

	// JAVA内での受け渡しに使用する疑似フィールド
	MATCH_PAGES("");


	/** 属性タイプ名 */
	private final String attribteTypeName;

	/** フィールド名称置換文字列 */
	private static final String REPLACEMENT_STRING_LANG_ID_NAME = "_LANG_ID_NAME";

	/** フィールド名称置換文字列 */
	private static final String REPLACEMENT_STRING_NAME = "_NAME";

	/**
	 * コンストラクタです。
	 * @param attribteTypeName 属性タイプ名
	 */
	private ContentSearchFieldEnum(String attribteTypeName) {
		this.attribteTypeName = attribteTypeName;
	}

	/**
	 * 属性タイプ名を取得します。
	 * @return 属性タイプ名
	 */
	public String getAttributeTypeName() {
		return attribteTypeName;
	}

	/**
	 * 多言語フィールドの場合"LANG_ID"をセッション言語IDに置換して返却します。<br>
	 * これによって、検索条件、または取得対象のフィールドを特定します。
	 * @return フィールド名
	 */
	@Override
	public String toString () {

		// 多言語フィールドでない場合は文字列をそのまま返却
		if (!name().contains(REPLACEMENT_STRING_LANG_ID_NAME)) {
			return name();
		}

		// セッション言語
		String langId = EIMThreadContext.getTransactionContext().getLangId();
		CharSequence replacement = "_" + langId + REPLACEMENT_STRING_NAME;

		// 多言語フィールドの場合は"LANG_ID" -> 言語IDに置換して返却
		return name().replace(REPLACEMENT_STRING_LANG_ID_NAME, replacement);
	}

	/**
	 * 属性タイプ定義名称をキーに列挙型を取得します。
	 * @param attribteTypeName 属性タイプ名
	 * @return 検索フィールド列挙型
	 */
	public static ContentSearchFieldEnum getByAttribuetTypeName(String attribteTypeName) {
		return Arrays.stream(ContentSearchFieldEnum.values())
                .filter(value -> value.attribteTypeName.equals(attribteTypeName))
                .findFirst()
                .orElse(null);
	}

	/**
	 * フィールド名称をキーに列挙型を取得します。
	 * 多言語IDを"LANG_ID"に置換します。
	 * @param fieldName フィールド名
	 * @return 検索フィールド列挙型
	 */
	public static ContentSearchFieldEnum getByFieldName(String fieldName) {

		// セッション言語
		String langId = EIMThreadContext.getTransactionContext().getLangId();
		CharSequence target = "_" + langId + REPLACEMENT_STRING_NAME;

		// セッション言語IDを"LANG_ID"に置換 (多言語フィールドでない場合はそのまま)
		String enumName = fieldName.replace(target, REPLACEMENT_STRING_LANG_ID_NAME);

		return Arrays.stream(ContentSearchFieldEnum.values())
                .filter(value -> value.name().equals(enumName))
                .findFirst()
                .orElse(null);
	}

}
