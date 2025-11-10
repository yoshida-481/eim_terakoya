package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.util.ArrayList;
import java.util.List;

import jp.co.ctc_g.eim.framework2.business.domain.entity.MailDomain;
import jp.co.ctc_g.eim.framework2.business.plugin.impl.DefaultMailElementPlugInImpl;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;

/**
 * 有効期限通知メッセージ要素編集クラス  本文　アイテム部：パス
 *
 */
public class MailElementBodyItemDisposeExpiredDocsPathPlugInImpl extends DefaultMailElementPlugInImpl {

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
	public List<Object> getParams(MailDomain mailDomain, String langId) throws Exception {

		List<Object> returnArray = new ArrayList<>();
		String path = mailDomain.getObject().getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PASS")).getString();
		returnArray.add(path);
		return returnArray;

	}

}
