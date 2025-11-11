package jp.co.ctc_g.eim.admin.presentation.web.form;



import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;


/**
 * コード作成フォーム
 * @since Ver3.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class CodeCreateForm {

	/** コードタイプフォーム */
	public CodeTypeForm codeType;
	/** コードフォーム */
	public CodeForm code;
	/** コード名称1 */
	public String otherName1;
	/** コード名称2 */
	public String otherName2;
	/** コードリスト */
	public List<CodeForm> codeList;

	/**
	 * コードタイプフォームを取得します。
	 * @return コードタイプフォーム
	 * @since Ver3.0
	 */
	public CodeTypeForm getCodeType() {
	    return codeType;
	}

	/**
	 * コードタイプフォームを設定します。
	 * @param id コードタイプフォーム
	 * @since Ver3.0
	 */
	public void setCodeType(CodeTypeForm codeType) {
	    this.codeType = codeType;
	}

	/**
	 * コードタイプフォームを取得します。
	 * @return コードタイプフォーム
	 * @since Ver3.0
	 */
	public CodeForm getCode() {
	    return code;
	}

	/**
	 * コードタイプフォームを設定します。
	 * @param id コードタイプフォーム
	 * @since Ver3.0
	 */
	public void setCode(CodeForm code) {
	    this.code = code;
	}

	/**
	 * 一つ目のコード名称を取得します。
	 * @return 一つ目のコード名称
	 * @since Ver3.0
	 */
	public String getOtherName1() {
	    return otherName1;
	}

	/**
	 * 一つ目のコード名称を設定します。
	 * @param otherName1 一つ目のコード名称
	 * @since Ver3.0
	 */
	public void setOtherName1(String otherName1) {
	    this.otherName1 = otherName1;
	}

	/**
	 * 二つ目のコード名称を取得します。
	 * @return 二つ目のコード名称
	 * @since Ver3.0
	 */
	public String getOtherName2() {
	    return otherName2;
	}

	/**
	 * 二つ目のコード名称を設定します。
	 * @param otherName2 二つ目のコード名称
	 * @since Ver3.0
	 */
	public void setOtherName2(String otherName2) {
	    this.otherName1 = otherName2;
	}

	/**
	 * コードリストを取得します。
	 * @return コードリスト
	 * @since Ver3.0
	 */
	public List<CodeForm> getCodeList() {
	    return codeList;
	}

	/**
	 * コードリストを設定します。
	 * @param codeList コードリスト
	 * @since Ver3.0
	 */
	public void setCodeList(List<CodeForm> codeList) {
	    this.codeList = codeList;
	}

}
