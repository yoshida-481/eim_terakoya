package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import jp.co.ctc_g.eim.framework2.business.domain.entity.MailDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.plugin.impl.DefaultMailElementPlugInImpl;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;

/**
 * 有効期限切れドキュメント削除通知メッセージ要素編集クラス  本文　アイテム部：有効期限
 *
 */
public class MailElementBodyItemDisposeExpiredDocsExpirationDatePlugInImpl extends DefaultMailElementPlugInImpl {

	/**
	 * 有効期限取得<br>
	 *
	 * @param mailDomain メールドメイン
	 * @param LangId 言語ID
	 *
	 * @return 有効期限文字列リスト
	 *
	 * @throws Exception
	 */
	@Override
	public List<Object> getParams(MailDomain mailDomain, String langId) throws Exception {

		List<Object> returnArray = new ArrayList<>();

		// 有効期限取得
		ObjectDomain object = mailDomain.getObject();
		Date date =object.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_EFFECT_DATE")).getDate();

		String dateFmt ="yyyy-MM-dd";
		String effectDate = new SimpleDateFormat(dateFmt).format(date);

		returnArray.add(effectDate);
		return returnArray;
	}

}
