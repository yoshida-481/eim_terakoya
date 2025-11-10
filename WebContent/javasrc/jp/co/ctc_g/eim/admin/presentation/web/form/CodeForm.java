package jp.co.ctc_g.eim.admin.presentation.web.form;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import jp.co.ctc_g.eim.framework2.business.domain.entity.CodeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.OtherNameDomain;

/**
 * コードフォーム
 * CodeDomainはFrameworkにあり、@JsonIgnorePropertiesが追加できないためDTOとして追加している。
 * Domainに@JsonIgnorePropertiesを追加した後はDomainを使用するため不要となる。
 * @since Ver3.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class CodeForm {

	/** コードID */
	private long id = 0;

	/** コード名称 */
	private String name = null;

	/** コード値 */
	private String code = null;

	/** 並び順序 */
	private int sequence = 0;

	/** 他言語名称 */
	private List<OtherNameForm> nameList = new ArrayList<OtherNameForm>();

	/** 無効フラグ */
	private boolean disable = false;

	/**
	 * コードIDを取得します。
	 * @return コードID
	 * @since Ver3.0
	 */
	public long getId() {
	    return id;
	}

	/**
	 * コードIDを設定します。
	 * @param id コードID
	 * @since Ver3.0
	 */
	public void setId(long id) {
	    this.id = id;
	}

	/**
	 * コード名称を取得します。
	 * @return コード名称
	 * @since Ver3.0
	 */
	public String getName() {
	    return name;
	}


	/**
	 * コード名称を設定します。
	 * @param name コード名称
	 * @since Ver3.0
	 */
	public void setName(String name) {
	    this.name = name;
	}

		/**
	 * コード名称一覧を取得します。
	 * @return コード名称
	 * @since Ver3.0
	 */
	public List<OtherNameForm> getNameList() {
	    return nameList;
	}


	/**
	 * コード名称一覧を設定します。
	 * @param name コード名称
	 * @since Ver3.0
	 */
	public void setNameList(List<OtherNameForm> nameList) {
	    this.nameList = nameList;
	}

	/**
	 * コード値を取得します。
	 * @return コード値
	 * @since Ver3.0
	 */
	public String getCode() {
	    return code;
	}

	/**
	 * コード値を設定します。
	 * @param code コード値
	 * @since Ver3.0
	 */
	public void setCode(String code) {
	    this.code = code;
	}

	/**
	 * 並び順序を取得します。
	 * @return 並び順序
	 * @since Ver3.0
	 */
	public int getSequence() {
	    return sequence;
	}

	/**
	 * 並び順序を設定します。
	 * @param sequence 並び順序
	 * @since Ver3.0
	 */
	public void setSequence(int sequence) {
	    this.sequence = sequence;
	}

	/**
	 * 無効フラグを取得します。
	 * @return 無効フラグ
	 * @since Ver3.0
	 */
	public boolean isDisable() {
	    return disable;
	}

	/**
	 * 無効フラグを設定します。
	 * @param disable 無効フラグ
	 * @since Ver3.0
	 */
	public void setDisable(boolean disable) {
	    this.disable = disable;
	}


	/**
	 * ドメインに変換します。
	 * @param domainObject ドメイン
	 * @since Ver3.0
	 */
	public CodeDomain convert(Object domainObject) {
		CodeDomain domain = (CodeDomain)domainObject;

		domain.setId(id);
		domain.setCode(code);
		domain.setSequence(sequence);
		domain.setDisable(disable);

		if (nameList != null) {
			List<OtherNameDomain> otherNameDomainList = new ArrayList<OtherNameDomain>();
			for (OtherNameForm otherName : nameList) {
				otherNameDomainList.add(otherName.convert(new OtherNameDomain()));
			}
			domain.setNameList(otherNameDomainList);
		}

		return domain;
	}
}
