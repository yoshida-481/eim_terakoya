package jp.co.ctc_g.eim.admin.business.domain;


/**
*
* OCR処理ドメイン
*
*/
public class OcrProcessorDomain{
	
	/** オブジェクトID */
	private String objectId = "";
	
	/** 入力ファイル名 */
	private String inFileName = "";
	
	/** 出力ファイル名 */
	private String outFileName = "";

	/** オブジェクト名 */
	private String objectName = "";
	
	
	/**
	 * オブジェクトIDを取得します。
	 * @return オブジェクトID
	 */
	public String getObjectId() {
		return objectId;
	}

	/**
	 * オブジェクトIDを設定します。
	 * @param objectId オブジェクトID
	 */
	public void setObjectId(String objectId) {
		this.objectId = objectId;
	}

	/**
	 * 入力ファイル名を取得します。
	 * @return 入力ファイル名
	 */
	public String getInFileName() {
		return inFileName;
	}

	/**
	 * 入力ファイル名を設定します。
	 * @param dateValue 入力ファイル名
	 */
	public void setInFileName(String inFileName) {
		this.inFileName = inFileName;
	}

	/**
	 * 出力ファイル名を取得します。
	 * @return 入力ファイル名
	 */
	public String getOutFileName() {
		return outFileName;
	}

	/**
	 * 出力ファイル名を設定します。
	 * @param dateValue 入力ファイル名
	 */
	public void setOutFileName(String outFileName) {
		this.outFileName = outFileName;
	}

	/**
	 * オブジェクト名を取得します。
	 * @return objectName
	 */
	public String getObjectName() {
		return objectName;
	}

	/**
	 * オブジェクト名を設定します。
	 * @param objectName オブジェクト名
	 */
	public void setObjectName(String objectName) {
		this.objectName = objectName;
	}
}