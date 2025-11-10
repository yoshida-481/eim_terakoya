package jp.co.ctc_g.eim.common.presentation.web.dto;

import java.util.List;

import jp.co.ctc_g.eim.common.presentation.web.dto.MailTypeDTO.MailTypeList;
import jp.co.ctc_g.eim.framework.business.domain.MailTypeDomain;

/**
 * MailTypeListDTO
 * <p>
 * Struts->SpringMVCへの移行に際して、クライアントへ送信するXMLデータを再現したデータ転送用オブジェクトです。
 */
public class MailTypeListDTO {

	
	/** ベースイベントタイプリスト */
	private MailTypeList mailTypeList = null;
	
	/**
	 * コンストラクタ<br>
	 * mailTypeListが保持しているプロパティ値を設定します。<br>
	 *
	 * @param mailTypeList
	 */
	public MailTypeListDTO(List<MailTypeDomain> mailTypeList) {
		setMailTypeList(new MailTypeList(mailTypeList));

	}

	/**
	 * @return mailTypeListを取得します。
	 */
	public MailTypeList getMailTypeList() {
		return mailTypeList;
	}

	/**
	 * @param mailTypeListを設定します。
	 */
	public void setMailTypeList(MailTypeList mailTypeList) {
		this.mailTypeList = mailTypeList;
	}
}
