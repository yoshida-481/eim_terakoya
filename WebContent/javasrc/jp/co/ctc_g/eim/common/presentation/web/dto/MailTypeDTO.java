package jp.co.ctc_g.eim.common.presentation.web.dto;

import java.util.ArrayList;
import java.util.List;

import jp.co.ctc_g.eim.framework.business.domain.MailTypeDomain;

/**
 * MailTypeConfDTO
 * <p>
 * Struts->SpringMVCへの移行に際して、クライアントへ送信するXMLデータを再現したデータ転送用オブジェクトです。
 */
public class MailTypeDTO {

	/**
	 * Strutsで生成したXMLデータにおける配列要素を再現するためのクラスです。
	 */
	public static class MailTypeList {
		/** 名称リスト */
		private List<MailTypeDTO> mailType = new ArrayList<MailTypeDTO>();

		/**
		 * コンストラクタ
		 * 
		 * @param mailTypeList
		 */
		public MailTypeList(List<MailTypeDomain> mailTypeList) {
			for (MailTypeDomain mailTypeDomain : mailTypeList) {
				mailType.add(new MailTypeDTO(mailTypeDomain));
			}
		}

		public List<MailTypeDTO> getMailType() {
			return mailType;
		}

		public void setMailType(List<MailTypeDTO> mailType) {
			this.mailType = mailType;
		}

	}

	/**
	 * クライアントへ送信していたXMLデータにおいて、単数値のプロパティを保持する親要素を再現するためのクラスです。
	 */
	public class Attr {
		/** ID */
		private String id = null;

		/** 名称 */
		private String name = null;

		/**
		 * コンストラクタ
		 * 
		 * @param mailTypeDomain
		 */
		private Attr(MailTypeDomain mailTypeDomain) {
			id = String.valueOf(mailTypeDomain.getId());
			name = mailTypeDomain.getName();
		}

		public String getId() {
			return id;
		}

		public void setId(String id) {
			this.id = id;
		}

		public String getName() {
			return name;
		}

		public void setName(String name) {
			this.name = name;
		}
	}

	/**
	 * Strutsで生成したXMLデータにおいて単数値のプロパティを保持する親要素です。
	 */
	private Attr attr = null;

	/**
	 * コンストラクタ<br>
	 * MailTypeDomainが保持しているプロパティ値を設定します。<br>
	 *
	 * @param mailTypeDomain
	 * @throws Exception
	 */
	public MailTypeDTO(MailTypeDomain mailTypeDomain) {
		setAttr(new Attr(mailTypeDomain));
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
