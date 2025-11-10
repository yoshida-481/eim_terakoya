package jp.co.ctc_g.eim.admin.presentation.web.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;



/**
 * インポート用DTO
 * @since Ver3.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class ImportDTO implements Cloneable{

	/** メッセージ */
	private String message = null;

	/**
	 * メッセージを取得します。
	 * @return メッセージ
	 * @since Ver3.0
	 */
	public String getMessage() {
	    return message;
	}


	/**
	 * メッセージを設定します。
	 * @param name セキュリティ名称
	 * @since Ver3.0
	 */
	public void setMessage(String message) {
	    this.message = message;
	}

}