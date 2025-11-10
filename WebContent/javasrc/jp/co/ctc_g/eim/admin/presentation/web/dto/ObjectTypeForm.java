package jp.co.ctc_g.eim.admin.presentation.web.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;



/**
 * オブジェクトタイプフォーム
 * ObjectTypeDomainはFrameworkにあり、@JsonIgnorePropertiesが追加できないためDTOとして追加している。
 * Domainに@JsonIgnorePropertiesを追加した後はDomainを使用するため不要となる。
 * @since Ver3.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class ObjectTypeForm implements Cloneable{

	/** 属性タイプ一覧 */
//	private List<AttributeTypeDomain> attributeTypeList = new ArrayList<AttributeTypeDomain>();

	/** オブジェクトタイプID */
	private long id = 0;

	/** オブジェクトタイプ名称 */
	private String name = null;

	/** オブジェクトタイプ定義名称 */
	private String definitionName = null;

	/**
	 * オブジェクトタイプIDを取得します。
	 * @return オブジェクトタイプID
	 * @since Ver3.0
	 */
	public long getId() {
	    return id;
	}


	/**
	 * オブジェクトタイプIDを設定します。
	 * @param id オブジェクトタイプID
	 * @since Ver3.0
	 */
	public void setId(long id) {
	    this.id = id;
	}


	/**
	 * オブジェクトタイプ名称を取得します。
	 * @return オブジェクトタイプ名称
	 * @since Ver3.0
	 */
	public String getName() {
	    return name;
	}


	/**
	 * オブジェクトタイプ名称を設定します。
	 * @param name オブジェクトタイプ名称
	 * @since Ver3.0
	 */
	public void setName(String name) {
	    this.name = name;
	}


	/**
	 * オブジェクトタイプ定義名称を取得します。
	 * @return オブジェクトタイプ定義名称
	 * @since Ver3.0
	 */
	public String getDefinitionName() {
	    return definitionName;
	}


	/**
	 * オブジェクトタイプ定義名称を設定します。
	 * @param definitionName オブジェクトタイプ定義名称
	 * @since Ver3.0
	 */
	public void setDefinitionName(String definitionName) {
	    this.definitionName = definitionName;
	}


}