package jp.co.ctc_g.eim.app.document.business.service.impl;

import jp.co.ctc_g.eim.framework.business.domain.MailDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework.business.service.impl.MailElementPlugInImpl;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

import common.util.MailUtil;

import eim.net.EIMSession;
import eim.util.ObjectUtils;
import eim.util.UserUtils;
/**
/**	
 * メッセージ要素編集クラス  本文　アイテム部：依頼者
 *
 */
public class MailElementBodyItemRequestUserPlugInImpl extends MailElementPlugInImpl {
	
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
		returnArray[0] = "";
		
		long objId = mailDomain.getObjectDomain().getId();
		EIMSession sess = EIMThreadContext.getEIMSession();
		UserDomain user = MailUtil.getLatestRequester(sess, ObjectUtils.getObjectById(sess, objId));
		returnArray[0] = UserUtils.getOtherUserName(sess, user.getId(), langId);
		return returnArray;
	}
}

