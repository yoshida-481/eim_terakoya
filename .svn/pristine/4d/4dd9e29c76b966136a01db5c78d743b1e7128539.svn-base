package jp.co.ctc_g.eim.admin.presentation.web.form;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import jp.co.ctc_g.eim.framework2.business.domain.entity.OtherNameDomain;

/**
 * コードタイプフォーム
 * CodeTypeDomainはFrameworkにあり、@JsonIgnorePropertiesが追加できないためDTOとして追加している。
 * Domainに@JsonIgnorePropertiesを追加した後はDomainを使用するため不要となる。
 * @since Ver3.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class OtherNameForm {

	/** 言語ID */
	private String langId = null;

	/** 名称 */
	private String name = null;


	/**
	 * 言語IDを取得します。
	 * @return 言語ID
	 * @since Ver5.0
	 */
	public String getLangId() {
	    return langId;
	}

	/**
	 * 言語IDを設定します。
	 * @param langId 言語ID
	 * @since Ver5.0
	 */
	public void setLangId(String langId) {
	    this.langId = langId;
	}

	/**
	 * 名称を取得します。
	 * @since Ver5.0
	 * @return 名称
	 */
	public String getName() {
	    return name;
	}

	/**
	 * 名称を設定します。
	 * @since Ver5.0
	 * @param name 名称
	 */
	public void setName(String name) {
	    this.name = name;
	}


	/**
	 * ドメインに変換します。
	 * @param domainObject ドメイン
	 * @since Ver3.0
	 */
	public OtherNameDomain convert(Object domainObject) {
		OtherNameDomain domain = (OtherNameDomain)domainObject;

		domain.setName(name);
		domain.setLangId(langId);
		return domain;
	}

}
