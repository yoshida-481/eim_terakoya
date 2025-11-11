package jp.co.ctc_g.eim.app.document.business.service.impl;


import common.util.AppObjectUtil;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.util.EIMConfig;
import eim.util.ObjectUtils;
import jp.co.ctc_g.eim.framework.business.domain.MailDomain;
import jp.co.ctc_g.eim.framework.business.service.impl.MailElementPlugInImpl;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * メッセージ要素編集クラス  本文　アイテム部：公開通知コメント
 *
 */
public class MailElementBodyItemPublicCommentPlugInImpl extends MailElementPlugInImpl {

	/**
	 * 置換文字列リスト取得<br>
	 *
	 * @param mailDomain メールドメイン
	 * @param langId 言語ID
	 *
	 * @return 置換文字列リスト
	 *
	 * @throws Exception
	 */
	@Override
	protected Object[] getParams(MailDomain mailDomain, String langId) throws Exception {
		
		// メール通知オブジェクト取得
		String[] returnArray = new String[1];
		EIMObjectType objTypeMailNotify = ObjectUtils.getObjectTypeByName(EIMThreadContext.getEIMSession(), EIMConfig.getValue("OBJECT_TYPE_NAME_MAIL_NOTIFY"));
		EIMObject mailNotifyObj = ObjectUtils.getObjectByTypeAndName(
				EIMThreadContext.getEIMSession(),
				objTypeMailNotify,
				Long.toString( mailDomain.getObjectDomain().getId())
				);
				
		String	publicComment = AppObjectUtil.getTextAttr(
				EIMThreadContext.getEIMSession(), mailNotifyObj,
				EIMConfig.get("ATTR_NAME_PUBLIC_COMMENT"));
		returnArray[0] = publicComment == null ? "" : publicComment;
		return returnArray;
	}

}
