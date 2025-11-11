package jp.co.ctc_g.eim.admin.presentation.web.dto;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

/**
 * 帳票ワークスペースDTO(リスト返却用)
 *
 * @since Ver1.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class FormWorkspaceDTO {

	/** 帳票ワークフローID */
	private long id;

	/** 帳票ワークフロー名称 */
	private String name;

	/** 帳票ワークスペースのセキュリティ */
	private SecurityDTO security;

	/** 言語リスト */
	private List<String> languageList = new ArrayList<String>();

	/** 帳票タイプリスト */
	private List<FormTypeDTO> children = new ArrayList<FormTypeDTO>();

	/**
	 * ワークフローIDを取得します。
	 * @return ワークフローID
	 * @since Ver1.0
	 */
	public long getId() {
	    return id;
	}


	/**
	 * 帳票ワークフローIDを設定します。
	 * @param id ワークフローID
	 * @since Ver1.0
	 */
	public void setId(long id) {
	    this.id = id;
	}


	/**
	 * 帳票ワークフロー名称を取得します。
	 * @return 帳票ワークフロー名称
	 * @since Ver1.0
	 */
	public String getName() {
	    return name;
	}


	/**
	 * 帳票ワークフロー名称を設定します。
	 * @param name 帳票ワークフロー名称
	 * @since Ver1.0
	 */
	public void setName(String name) {
	    this.name = name;
	}


	/**
	 * 帳票タイプリストを取得します。
	 * @return children
	 * @since Ver1.0
	 */
	public List<FormTypeDTO> getChildren() {
		return children;
	}

	/**
	 * 帳票タイプリストを設定します。
	 * @param formTypeFolderList 帳票タイプリスト
	 * @since Ver1.0
	 */
	public void setChildren(List<FormTypeDTO> children) {
		this.children = children;
	}

	/**
	 * 言語リストを取得します。
	 * @return languageList
	 * @since Ver1.0
	 */
	public List<String> getLanguageList() {
		return languageList;
	}

	/**
	 * 言語リストを設定します。
	 * @param securityId 言語リスト
	 * @since Ver1.0
	 */
	public void setLanguageList(List<String> languageList) {
		this.languageList = languageList;
	}

	/**
	 * 帳票ワークスペースのセキュリティを取得します。
	 * @return securityId
	 * @since Ver1.0
	 */
	public SecurityDTO getSecurity() {
		return security;
	}

	/**
	 * 帳票ワークスペースのセキュリティを設定します。
	 * @param securityName 帳票ワークスペースのセキュリティ
	 * @since Ver1.0
	 */
	public void setSecurity(SecurityDTO security) {
		this.security = security;
	}

}

