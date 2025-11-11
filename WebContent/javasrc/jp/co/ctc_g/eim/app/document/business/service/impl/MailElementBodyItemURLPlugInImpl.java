package jp.co.ctc_g.eim.app.document.business.service.impl;

import common.util.AppObjectUtil;

import eim.util.EIMConfig;
import jp.co.ctc_g.eim.framework.business.domain.MailDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework.business.service.impl.MailElementPlugInImpl;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * メッセージ要素編集クラス  本文　アイテム部：URL
 *
 */
public class MailElementBodyItemURLPlugInImpl extends MailElementPlugInImpl {


	/**
	 * objId使用可否フラグ 設定
	 * <p>
	 * URLパラメータ｢objId｣を設定するかの判定用フラグ<br>
	 * ※ applicationContextのプロパティ値自動設定(spring標準機能)
	 *
	 * @param objId使用可否 : true:使用する/false:使用しない
	 */
	private Boolean objIdFlg = false;
	public void setObjIdFlg(Boolean val) {
		this.objIdFlg = val;
	}

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

		ObjectDomain objectDomain = mailDomain.getObjectDomain();	// オブジェクトID取得
		returnArray[0] = EIMConfig.get("DOCUMENT_URL") +  EIMConfig.get("QUERY_STRING");

		if(objIdFlg){
			returnArray[0] += "objId=" + objectDomain.getId();
		}
		
		return returnArray;

	}

}
