package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.util.ArrayList;
import java.util.List;

import jp.co.ctc_g.eim.framework2.business.domain.entity.MailDomain;
import jp.co.ctc_g.eim.framework2.business.plugin.impl.DefaultMailElementPlugInImpl;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;

/**
 * 有効期限事前通知メッセージ要素編集クラス  本文　アイテム部：事前通知日数
 *
 */
public class MailElementBodyItemExpirationDateNoticeDatePlugInImpl extends DefaultMailElementPlugInImpl {

	/**
	 * 事前通知日数取得<br>
	 *
	 * @param mailDomain メールドメイン
	 * @param LangId 言語ID
	 *
	 * @return 事前通知日数文字列リスト
	 *
	 * @throws Exception
	 */
	@Override
	public List<Object> getParams(MailDomain mailDomain, String langId) throws Exception {

		List<Object> returnArray = new ArrayList<>();

		// 事前通知日数取得
		String advanceNoticeDays = ConfigUtils.getByKey("EXPIRY_NOTICE_DAYS");
		if (advanceNoticeDays == null) {
			advanceNoticeDays = "";
		}
		returnArray.add(advanceNoticeDays);
		return returnArray;
	}

}
