package jp.co.ctc_g.eim.app.document.business.service.impl;

import eim.bo.EIMObject;
import eim.util.ObjectUtils;
import jp.co.ctc_g.eim.framework.business.domain.MailDomain;
import jp.co.ctc_g.eim.framework.business.service.impl.MailElementPlugInImpl;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * メッセージ要素編集クラス  本文　アイテム部：名称
 *
 */
public class MailElementBodyItemNamePlugInImpl extends MailElementPlugInImpl {

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
		EIMObject object = mailDomain.getObjectDomain().createEIMObject();
		if(object == null){
			returnArray[0] = "";
		}
		else{
			returnArray[0] = object.getName();
		}
		return returnArray;

	}

}
