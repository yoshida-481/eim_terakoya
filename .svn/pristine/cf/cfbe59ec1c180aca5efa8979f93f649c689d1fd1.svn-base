package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.util.ArrayList;
import java.util.List;

import jp.co.ctc_g.eim.framework2.business.domain.entity.MailDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.plugin.impl.DefaultMailElementPlugInImpl;

/**
 * 有効期限切れドキュメント削除通知メッセージ要素編集クラス  本文　アイテム部：名称
 *
 */
public class MailElementBodyItemDisposeExpiredDocsNamePlugInImpl extends DefaultMailElementPlugInImpl {

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
		ObjectDomain object = mailDomain.getObject();
		if(object == null){
			returnArray.add("");
		}
		else{
			returnArray.add(object.getName());
		}
		return returnArray;

	}

}
