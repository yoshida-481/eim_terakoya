package jp.co.ctc_g.eim.app.document.business.service.impl;

import common.util.AppObjectUtil;

import eim.bo.EIMObject;
import eim.util.EIMConfig;
import eim.util.ObjectUtils;
import jp.co.ctc_g.eim.framework.business.domain.MailDomain;
import jp.co.ctc_g.eim.framework.business.service.impl.MailElementPlugInImpl;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * メッセージ要素編集クラス  本文　アイテム部：パス
 *
 */
public class MailElementBodyItemPathPlugInImpl extends MailElementPlugInImpl {

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
		String	pass = AppObjectUtil.getStrAttrs(
				EIMThreadContext.getEIMSession(),
				mailDomain.getObjectDomain().createEIMObject(), 
				EIMConfig.get("ATTR_NAME_DOCUMENT_PASS"))[0];
		returnArray[0] = pass;
		return returnArray;

	}

}
