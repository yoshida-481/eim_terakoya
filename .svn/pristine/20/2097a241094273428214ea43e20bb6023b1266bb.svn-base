package common.tools.internal;

/**
 * ドキュメント管理リレーションパスタイプ列挙型
 * 0:DOCUMENT =ドキュメントリレーション（ノードがドキュメント、フォルダ場合）
 * 1:LINK =リンクリレーション（ノードがリンクの場合）
 * 2:ROOT =ルート（ノードの場合）
 *
 */
public enum PathTypeEnum {

	/** ドキュメント */
	DOCUMENT(0,"DOCUMENT"),
	/** リンク */
	LINK(1,"LINK"),
	/** マイドキュメント */
	MYDOCUMENT(2,"MYDOCUMENT"),
	
	/**イレギュラー リレーションなし */
	IRREGULAR_NORELATION(3,"IRREGULAR_NORELATION"),

	/**イレギュラー リレーションなし */
	IRREGULAR_LATEST_NOTFOUND(4,"IRREGULAR_LATEST_NOTFOUND"),
	
	/**
	 * ルート
	 */
	ROOT(5,"ROOT");
	
	private final String symbol;
	private final int value;
	
	/**
	 * コンストラクタ<br>
	 * @param value v
	 * @param symbol シンボル
	 */
	private PathTypeEnum(int value, String symbol) {
		this.value = value;
		this.symbol = symbol;
	}
	
	/**
	 * 種別ID
	 * @return 種別IDを返します。
	 */
	public int getValue() {
		return value;
	}

	/**
	 * シンボルを返します。
	 * @return シンボル
	 */
    public String getSymbol() {
    	return symbol;
    }
}
