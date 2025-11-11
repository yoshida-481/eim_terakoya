package jp.co.ctc_g.eim.app.document.business.service.impl;

import common.util.AppObjectUtil;

import eim.util.EIMConfig;
import jp.co.ctc_g.eim.framework.business.domain.MailDomain;
import jp.co.ctc_g.eim.framework.business.service.impl.MailElementPlugInImpl;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * メッセージ要素編集クラス  本文　アイテム部：プロパティ
 *
 */
public class MailElementBodyItemPropertiePlugInImpl extends MailElementPlugInImpl {

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
		String	properties = AppObjectUtil.getStrAttr(
				EIMThreadContext.getEIMSession(),
				mailDomain.getObjectDomain().createEIMObject(), 
				EIMConfig.get("ATTR_NAME_DOCUMENT_PROP"));
		returnArray[0] = properties == null ? "" : properties;
		return returnArray;

	}

}
