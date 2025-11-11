package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.text.SimpleDateFormat;
import java.util.Date;

import eim.bo.EIMResource;
import eim.util.StringUtils;

import jp.co.ctc_g.eim.framework.business.domain.MailDomain;
import jp.co.ctc_g.eim.framework.business.service.impl.MailElementPlugInImpl;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * メッセージ要素編集クラス  本文　アイテム部：参照日
 *
 */
public class MailElementBodyItemReferenceDatePlugInImpl extends MailElementPlugInImpl {

	/**
	 * 置換文字列リスト取得<br>
	 *
	 * @param mailDomain メールドメイン
	 * @param LangId 言語ID
	 *
	 * @return 置換文字列リスト
	 *
	 * @throws Exception
	 */
	@Override
	protected Object[] getParams(MailDomain mailDomain, String langId) throws Exception {

		String[] returnArray = new String[1];
		returnArray[0] = StringUtils.getDateStringByFormat(new Date(),EIMResource.getMessage(langId,"EIM.FORMAT.DATETIME"));
		return returnArray;

	}

}
