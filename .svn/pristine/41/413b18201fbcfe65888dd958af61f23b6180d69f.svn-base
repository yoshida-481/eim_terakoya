package jp.co.ctc_g.eim.common.presentation.web.dto;

import java.util.ArrayList;
import java.util.List;

import jp.co.ctc_g.eim.framework.business.domain.NoticeMailDomain;


/**
 * NoticeMailDTO
 * <p>
 * Struts->SpringMVCへの移行に際して、クライアントへ送信するXMLデータを再現したデータ転送用オブジェクトです。
 */
public class NoticeMailDTO {

	/**
	 * Strutsで生成したXMLデータにおける配列要素を再現するためのクラスです。
	 */
	public static class MailList {

		private List<NoticeMailDTO> mail = new ArrayList<NoticeMailDTO>();

		/**
		 * コンストラクタ
		 * 
		 * @param mailList
		 */
		public MailList(List<NoticeMailDomain> mailList) {
			for (NoticeMailDomain noticeMailDomain : mailList) {
				mail.add(new NoticeMailDTO(noticeMailDomain));
			}
		}

		public List<NoticeMailDTO> getMail() {
			return mail;
		}

		public void setMail(List<NoticeMailDTO> mail) {
			this.mail = mail;
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

		/** メソッド */
		private String method = null;

		/**
		 * コンストラクタ
		 * 
		 * @param noticeMail
		 */
		private Attr(NoticeMailDomain noticeMail) {
			id = String.valueOf(noticeMail.getMailType().getId());
			name = noticeMail.getMailType().getName();
			method = noticeMail.getMailMethod().getSymbol();
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

		public String getMethod() {
			return method;
		}

		public void setMethod(String method) {
			this.method = method;
		}
	}

	/**
	 * Strutsで生成したXMLデータにおいて単数値のプロパティを保持する親要素です。
	 */
	private Attr attr = null;

	/**
	 * コンストラクタ<br>
	 * NoticeMailDomainが保持しているプロパティ値を設定します。<br>
	 *
	 * @param noticeMail 
	 */
	public NoticeMailDTO(NoticeMailDomain noticeMail) {
		setAttr(new Attr(noticeMail));
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
