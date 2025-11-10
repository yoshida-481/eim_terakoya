package jp.co.ctc_g.eim.common.presentation.web.dto;

import java.util.ArrayList;
import java.util.List;

import jp.co.ctc_g.eim.framework.business.domain.entity.OtherNameDomain;

/**
 * 他言語のIDと名前を管理するDTO
 * <p>
 * Struts->SpringMVCへの移行に際して、クライアントへ送信するXMLデータを再現したデータ転送用オブジェクトです。
 */
public class OtherNameDTO {

	/**
	 * Strutsで生成したXMLデータにおける配列要素を再現するためのクラスです。
	 */
	public static class NameList {
		/** 名称リスト */
		private List<OtherNameDTO> name = new ArrayList<OtherNameDTO>();

		/**
		 * コンストラクタ
		 * 
		 * @param nameList
		 */
		public NameList(List<OtherNameDomain> nameList) {
			for (OtherNameDomain otherNameDomain : nameList) {
				name.add(new OtherNameDTO(otherNameDomain));
			}
		}

		public List<OtherNameDTO> getName() {
			return name;
		}

		public void setName(List<OtherNameDTO> name) {
			this.name = name;
		}
	}

	/**
	 * クライアントへ送信していたXMLデータにおいて、単数値のプロパティを保持する親要素を再現するためのクラスです。
	 */
	public class Attr {
		/** 言語ID */
		private String lang = null;

		/** 名称 */
		private String value = null;

		/**
		 * コンストラクタ
		 * 
		 * @param otherName
		 */
		private Attr(OtherNameDomain otherName) {
			lang = otherName.getLangId();
			value = otherName.getName();
		}

		public String getLang() {
			return lang;
		}

		public void setLang(String lang) {
			this.lang = lang;
		}

		public String getValue() {
			return value;
		}

		public void setValue(String value) {
			this.value = value;
		}
	}

	/**
	 * Strutsで生成したXMLデータにおいて単数値のプロパティを保持する親要素です。
	 */
	private Attr attr = null;

	/**
	 * コンストラクタ<br>
	 * EIMOtherNameが保持しているプロパティ値を設定します。<br>
	 *
	 * @param otherName 他言語名称
	 */
	public OtherNameDTO(OtherNameDomain otherName) {
		setAttr(new Attr(otherName));
	}

	/**
	 * @return attrを取得します。
	 */
	public Attr getAttr() {
		return attr;
	}

	/**
	 * @param attrを設定します。
	 */
	public void setAttr(Attr attr) {
		this.attr = attr;
	}

}
