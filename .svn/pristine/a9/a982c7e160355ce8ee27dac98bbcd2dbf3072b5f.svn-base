package jp.co.ctc_g.eim.admin.presentation.web.form.criteria;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

/**
 * 帳票タイプの検索条件を保持します。
 * @since Ver 1.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class FormTypeFolderCriteriaForm {

	/**
	 * 帳票ワークスペースIDに対する検索条件<br>
	 */
	private Long formWorkspaceId;


	/** 帳票タイプIDに対する検索条件 */
	private Long formTypeId;

	/**
	 * 親帳票フォルダID<br>
	 */
	private Long parentFormFolderId;

	/**
	 * 帳票フォルダID<br>
	 */
	private Long formFolderId;


	/** 言語リスト */
	private List<String>  nameList;


	/**
	 * 帳票ワークスペースIDに対する検索条件を取得します。<br>
	 * @return 帳票ワークスペースIDに対する検索条件
	 * @since Ver 1.0
	 */
	public Long getFormWorkspaceId() {
	    return formWorkspaceId;
	}


	/**
	 * 帳票ワークスペースIDに対する検索条件を設定します。<br>
	 * @param ids 帳票ワークスペースIDに対する検索条件
	 * @since Ver 1.0
	 */
	public void setFormWorkspaceId(Long formWorkspaceId) {
		this.formWorkspaceId = formWorkspaceId;
	}


	/**
	 * 帳票タイプIDに対する検索条件を取得します。<br>
	 * @return 帳票タイプIDに対する検索条件
	 * @since Ver 1.0
	 */
	public Long getFormTypeId() {
		return formTypeId;
	}


	/**
	 * 帳票タイプIDに対する検索条件を設定します。<br>
	 * @param ids 帳票タイプIDに対する検索条件
	 * @since Ver 1.0
	 */
	public void setFormTypeId(Long formTypeId) {
		this.formTypeId = formTypeId;
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
	 * 親帳票フォルダIDを取得します。<br>
	 * @return 親帳票フォルダID
	 */
	public Long getParentFormFolderId() {
		return parentFormFolderId;
	}


	/**
	 * 親帳票フォルダIDを設定します。<br>
	 * @param parentFormFolderId 親帳票フォルダID
	 */
	public void setParentFormFolderId(Long parentFormFolderId) {
		this.parentFormFolderId = parentFormFolderId;
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