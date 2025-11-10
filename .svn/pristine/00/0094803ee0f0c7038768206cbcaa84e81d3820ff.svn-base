package jp.co.ctc_g.eim.app.document.business.service.impl;

import eim.bo.EIMAccessRole;
import eim.bo.EIMAttribute;
import eim.bo.EIMObject;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.ObjectUtils;
import eim.util.SecurityUtils;
import jp.co.ctc_g.eim.framework.business.domain.MailDomain;
import jp.co.ctc_g.eim.framework.business.service.impl.MailElementPlugInImpl;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

public class MailElementBodyItemJoinPathPlugInImpl extends
		MailElementPlugInImpl {
	
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
		
		String[] path = new String[1];
		EIMSession sess = EIMThreadContext.getEIMSession();
		EIMObject joinObj = (EIMObject)EIMThreadContext.get("JOIN.OBJ");
		
		// 親オブジェクトの名称を「出力先」として指定
		EIMAttribute parentId = joinObj.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_PARENT_ID"));
		EIMObject parentObj = ObjectUtils.getObjectById(sess, parentId.getInt());
		
		String objId = String.valueOf(parentObj.getId());
		if (SecurityUtils.authorized(sess, parentObj, sess.getUser(), EIMAccessRole.READ)) {
			path[0] = parentObj.getName() + "(" + objId + ")";
		} else {
			path[0] = " -(" + objId + ")";
		}
		
		return path;
	}

}
