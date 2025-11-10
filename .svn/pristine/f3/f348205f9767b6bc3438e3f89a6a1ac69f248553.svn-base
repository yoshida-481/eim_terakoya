package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.util.List;

import org.springframework.context.ApplicationContext;

import eim.bo.EIMUser;
import eim.util.EIMConfig;
import eim.util.ObjectUtils;
import eim.util.UserUtils;
import jp.co.ctc_g.eim.framework.business.domain.EventHistoryDomain;
import jp.co.ctc_g.eim.framework.business.domain.EventLogDomain;
import jp.co.ctc_g.eim.framework.business.domain.MailDomain;
import jp.co.ctc_g.eim.framework.business.service.EventHistoryService;
import jp.co.ctc_g.eim.framework.business.service.impl.MailElementPlugInImpl;
import jp.co.ctc_g.eim.framework.common.util.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * メッセージ要素編集クラス  本文　冒頭
 *
 */
public class MailElementBodySessionUserMsgPlugInImpl extends MailElementPlugInImpl {

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
		EIMUser user = EIMThreadContext.getEIMSession().getUser();
		returnArray[0] = UserUtils.getOtherUserName(EIMThreadContext.getEIMSession(), user.getId(), langId);
		return returnArray;

	}

}
