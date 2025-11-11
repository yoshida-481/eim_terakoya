package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.util.List;

import org.springframework.context.ApplicationContext;

import jp.co.ctc_g.eim.framework.business.domain.EventHistoryDomain;
import jp.co.ctc_g.eim.framework.business.domain.EventLogDomain;
import jp.co.ctc_g.eim.framework.business.domain.MailDomain;
import jp.co.ctc_g.eim.framework.business.service.EventHistoryService;
import jp.co.ctc_g.eim.framework.business.service.MailService;
import jp.co.ctc_g.eim.framework.business.service.impl.MailElementPlugInImpl;
import jp.co.ctc_g.eim.framework.common.util.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

import eim.net.EIMSession;
import eim.util.ObjectUtils;
import eim.bo.EIMUser;
import eim.util.UserUtils;
import common.util.MailUtil;
/**
 * メッセージ要素編集クラス  本文　アイテム部：ユーザー
 *
 */
public class MailElementBodyItemCreateUserPlugInImpl extends MailElementPlugInImpl {

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
		EIMUser user = MailUtil.getCreater(sess,
				ObjectUtils.getObjectById(sess, objId));
		returnArray[0] = UserUtils.getOtherUserName(sess, user.getId(), langId);
		return returnArray;
	}

}
