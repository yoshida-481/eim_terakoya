package jp.co.ctc_g.eim.admin.presentation.web.form.criteria;

import java.util.ArrayList;
import java.util.List;

/**
 * 帳票タイプの検索条件を保持します。
 * @since Ver 1.0
 */
public class FormTypeCriteriaForm {

	/**
	 * 帳票ワークスペースID<br>
	 */
	private Long formWorkspaceId;

	/**
	 * 定義名称<br>
	 */
	private String definitionName;

	/**
	 * 帳票タイプIDに対する検索条件<br>
	 */
	private List<Long> ids = new ArrayList<Long>();

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
	 * 定義名称を取得します。<br>
	 * @return 定義名称
	 * @since Ver 1.0
	 */
	public String getDefinitionName() {
		return definitionName;
	}

	/**
	 * 定義名称を設定します。<br>
	 * @param definitionName 定義名称
	 * @since Ver 1.0
	 */
	public void setDefinitionName(String definitionName) {
		this.definitionName = definitionName;
	}

	/**
	 * 帳票タイプIDに対する検索条件を取得します。<br>
	 * @return 帳票タイプIDに対する検索条件
	 * @since Ver 1.0
	 */
	public List<Long> getIds() {
	    return ids;
	}


	/**
	 * 帳票タイプIDに対する検索条件を設定します。<br>
	 * @param ids 帳票タイプIDに対する検索条件
	 * @since Ver 1.0
	 */
	public void setIds(List<Long> ids) {
		this.ids = ids;
	}

}