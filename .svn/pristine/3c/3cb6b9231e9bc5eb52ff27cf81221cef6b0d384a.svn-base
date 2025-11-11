package jp.co.ctc_g.eim.admin.presentation.web.form;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import jp.co.ctc_g.eim.framework2.business.domain.entity.CodeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.CodeTypeDomain;

/**
 * コードタイプフォーム
 * CodeTypeDomainはFrameworkにあり、@JsonIgnorePropertiesが追加できないためDTOとして追加している。
 * Domainに@JsonIgnorePropertiesを追加した後はDomainを使用するため不要となる。
 * @since Ver3.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class CodeTypeForm {

	/** コードID */
	private long id = 0;
	/** 定義名称 */
	private String definitionName = null;
	/** コードリスト */
	private List<CodeForm> codeList = new ArrayList<CodeForm>();

	/**
	 * コードタイプIDを取得します。
	 * @return コードタイプID
	 * @since Ver3.0
	 */
	public long getId() {
	    return id;
	}

	/**
	 * コードタイプIDを設定します。
	 * @param id コードタイプID
	 * @since Ver3.0
	 */
	public void setId(long id) {
	    this.id = id;
	}

	/**
	 * 定義名称を取得します。
	 * @return 定義名称
	 * @since Ver3.0
	 */
	public String getDefinitionName() {
	    return definitionName;
	}


	/**
	 * 定義名称を設定します。
	 * @param name 定義名称
	 * @since Ver3.0
	 */
	public void setDefinitionName(String definitionName) {
	    this.definitionName = definitionName;
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
	 * @param code コードリスト
	 * @since Ver3.0
	 */
	public void setCodeList(List<CodeForm> codeList) {
	    this.codeList = codeList;
	}

	/**
	 * ドメインに変換します。
	 * @param domainObject ドメイン
	 * @since Ver3.0
	 */
	public CodeTypeDomain convert(Object domainObject) {
		CodeTypeDomain domain = (CodeTypeDomain)domainObject;

		domain.setId(id);
		domain.setDefinitionName(definitionName);
		if (codeList != null) {
			List<CodeDomain> codeDomainList = new ArrayList<CodeDomain>();
			for (CodeForm code : codeList) {
				codeDomainList.add(code.convert(new CodeDomain()));
			}
			domain.setCodeList(codeDomainList);
		}
		return domain;
	}
}
