package jp.co.ctc_g.eim.admin.presentation.web.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;



/**
 * セキュリティフォーム
 * ObjectTypeDomainはFrameworkにあり、@JsonIgnorePropertiesが追加できないためDTOとして追加している。
 * Domainに@JsonIgnorePropertiesを追加した後はDomainを使用するため不要となる。
 * @since Ver3.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class SecurityDTO implements Cloneable{

	/** セキュリティID */
	private long id = 0;

	/** セキュリティ名称 */
	private String name = null;

	/** セキュリティ定義名称 */
	private String definitionName = null;

	/**
	 * セキュリティIDを取得します。
	 * @return セキュリティID
	 * @since Ver3.0
	 */
	public long getId() {
	    return id;
	}


	/**
	 * セキュリティIDを設定します。
	 * @param id セキュリティID
	 * @since Ver3.0
	 */
	public void setId(long id) {
	    this.id = id;
	}


	/**
	 * セキュリティ名称を取得します。
	 * @return セキュリティ名称
	 * @since Ver3.0
	 */
	public String getName() {
	    return name;
	}


	/**
	 * セキュリティ名称を設定します。
	 * @param name セキュリティ名称
	 * @since Ver3.0
	 */
	public void setName(String name) {
	    this.name = name;
	}


	/**
	 * セキュリティ定義名称を取得します。
	 * @return セキュリティ定義名称
	 * @since Ver3.0
	 */
	public String getDefinitionName() {
	    return definitionName;
	}


	/**
	 * セキュリティ定義名称を設定します。
	 * @param definitionName セキュリティ定義名称
	 * @since Ver3.0
	 */
	public void setDefinitionName(String definitionName) {
	    this.definitionName = definitionName;
	}


}