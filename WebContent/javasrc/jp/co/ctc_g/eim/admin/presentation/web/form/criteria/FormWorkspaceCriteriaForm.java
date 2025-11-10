package jp.co.ctc_g.eim.admin.presentation.web.form.criteria;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

/**
 * 帳票タイプの検索条件を保持します。
 * @since Ver 1.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class FormWorkspaceCriteriaForm {

	/**
	 * 帳票ワークスペースID<br>
	 */
	private Long formWorkspaceId;

	/**
	 * 帳票フォルダID<br>
	 */
	private Long formFolderId;


	/**
	 * 帳票ワークスペース日本語名称<br>
	 */
	private String name;


	/** 言語リスト */
	private List<String>  nameList;


	/**
	 * 帳票ワークスペースIDを取得します。<br>
	 * @return 帳票ワークスペースID
	 * @since Ver 1.0
	 */
	public Long getFormWorkspaceId() {
	    return formWorkspaceId;
	}


	/**
	 * 帳票ワークスペースIDを設定します。<br>
	 * @param formWorkspaceId 帳票ワークスペースID
	 * @since Ver 1.0
	 */
	public void setFormWorkspaceId(Long formWorkspaceId) {
		this.formWorkspaceId = formWorkspaceId;
	}

	/**
	 * 帳票フォルダIDを取得します。<br>
	 * @return 帳票フォルダID
	 * @since Ver 1.0
	 */
	public Long getFormFolderId() {
	    return formFolderId;
	}


	/**
	 * 帳票フォルダIDを設定します。<br>
	 * @param formFolderId 帳票フォルダID
	 * @since Ver 1.0
	 */
	public void setFormFolderId(Long formFolderId) {
		this.formFolderId = formFolderId;
	}


	/**
	 * 帳票ワークスペース日本語名称を取得します。<br>
	 * @return 帳票ワークスペース日本語名称
	 * @since Ver 1.0
	 */
	public String getName() {
		return name;
	}

	/**
	 * 帳票ワークスペース日本語名称を設定します。<br>
	 * @param name 帳票ワークスペース日本語名称
	 * @since Ver 1.0
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * 言語リストを取得します。<br>
	 * @return 言語リスト
	 * @since Ver 1.0
	 */
	public List<String> getNameList() {
		return nameList;
	}

	/**
	 * 言語リストを設定します。<br>
	 * @param nameList 言語リスト
	 * @since Ver 1.0
	 */
	public void setNameList(List<String> nameList) {
		this.nameList = nameList;
	}

}