package jp.co.ctc_g.eim.admin.presentation.web.dto;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
/**
 * 帳票タイプDTO(リスト返却用)
 *
 * @since Ver1.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class FormTypeFolderDTO extends ObjectTypeForm {


	/** 帳票タイプフォルダセキュリティ名称 */
	private String securityName;

	/** 帳票タイプフォルダ一覧 */
	private List<FormTypeFolderDTO> children = new ArrayList<FormTypeFolderDTO>();

	/** 親帳票タイプフォルダ */
	private FormTypeFolderDTO parentFolder;

	/** 言語リスト */
	private List<String> languageList = new ArrayList<String>();

	/**
	 * ワークフロー名称を取得します。
	 * @return ワークフロー名称
	 * @since Ver1.0
	 */
	public String getSecurityName() {
	    return securityName;
	}


	/**
	 * セキュリティ名称を設定します。
	 * @param securityName セキュリティ名称
	 * @since Ver1.0
	 */
	public void setSecurityName(String securityName) {
	    this.securityName = securityName;
	}


	/**
	 * 帳票タイプフォルダリストを取得します。
	 * @return children
	 * @since Ver1.0
	 */
	public List<FormTypeFolderDTO> getChildren() {
		return children;
	}

	/**
	 * 帳票タイプフォルダリストを設定します。
	 * @param formTypeFolderList 帳票タイプフォルダリスト
	 * @since Ver1.0
	 */
	public void setChildren(List<FormTypeFolderDTO> children) {
		this.children = children;
	}

	/**
	 * 親帳票タイプフォルダ を取得します。
	 * @return parentFolder
	 * @since Ver1.0
	 */
	public FormTypeFolderDTO getParentFolder() {
		return parentFolder;
	}

	/**
	 * 親帳票タイプフォルダ を設定します。
	 * @param parentFolder 親帳票タイプフォルダ
	 * @since Ver1.0
	 */
	public void setParentFolder(FormTypeFolderDTO parentFolder) {
		this.parentFolder = parentFolder;
	}

	/**
	 * 言語リストを取得します。
	 * @return languageList
	 * @since Ver3.0
	 */
	public List<String> getLanguageList() {
		return languageList;
	}

	/**
	 * 言語リストを設定します。
	 * @param languageList 言語リスト
	 * @since Ver3.0
	 */
	public void setLanguageList(List<String> languageList) {
		this.languageList = languageList;
	}
}

