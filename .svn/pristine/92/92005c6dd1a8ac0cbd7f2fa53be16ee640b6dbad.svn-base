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
public class FormTypeDTO extends ObjectTypeForm {

	/** ワークフローID */
	private long workflowId;

	/** ワークフロー名称 */
	private String workflowName;

	/** ルートタイプフラグ */
	private boolean isRootType;

	/** ルートタイプ名称 */
	private String rootTypeDefName;

	/** 帳票タイプフォルダ一覧 */
	private List<FormTypeFolderDTO> children = new ArrayList<FormTypeFolderDTO>();

	/** 帳票タイプ一覧 */
	private List<FormTypeDTO> childrenType = new ArrayList<FormTypeDTO>();


	/**
	 * ワークフローIDを取得します。
	 * @return ワークフローID
	 * @since Ver1.0
	 */
	public long getWorkflowId() {
	    return workflowId;
	}


	/**
	 * ワークフローIDを設定します。
	 * @param workflowId ワークフローID
	 * @since Ver1.0
	 */
	public void setWorkflowId(long workflowId) {
	    this.workflowId = workflowId;
	}


	/**
	 * ワークフロー名称を取得します。
	 * @return ワークフロー名称
	 * @since Ver1.0
	 */
	public String getWorkflowName() {
	    return workflowName;
	}


	/**
	 * ワークフロー名称を設定します。
	 * @param name ワークフロー名称
	 * @since Ver1.0
	 */
	public void setWorkflowName(String workflowName) {
	    this.workflowName = workflowName;
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
	 * 帳票タイプリストを取得します。
	 * @return childrenType
	 * @since Ver1.0
	 */
	public List<FormTypeDTO> getChildrenType() {
		return childrenType;
	}

	/**
	 * 帳票タイプリストを設定します。
	 * @param formTypeFolderList 帳票タイプリスト
	 * @since Ver1.0
	 */
	public void setChildrenType(List<FormTypeDTO> childrenType) {
		this.childrenType = childrenType;
	}

	/**
	 * ルートタイプフラグを取得します。
	 * @return isRootType
	 * @since Ver1.0
	 */
	public boolean isRootType() {
		return isRootType;
	}

	/**
	 * ルートタイプフラグを設定します。
	 * @param isRootType ルートタイプフラグ
	 * @since Ver1.0
	 */
	public void setRootType(boolean isRootType) {
		this.isRootType = isRootType;
	}


	/**
	 * ルートタイプ名称を取得します。
	 * @return rootTypeDefName
	 * @since Ver1.0
	 */
	public String getRootTypeDefName() {
		return rootTypeDefName;
	}

	/**
	 * ルートタイプ名称を取得します。
	 * @return rootTypeDefName
	 * @since Ver1.0
	 */
	public void setRootTypeDefName(String rootTypeDefName) {
		this.rootTypeDefName = rootTypeDefName;
	}

}

