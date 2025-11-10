package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.util.List;

import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.ObjectUtils;
import jp.co.ctc_g.eim.framework.business.domain.MailDomain;
import jp.co.ctc_g.eim.framework.business.service.impl.MailElementPlugInImpl;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * メッセージ要素編集クラス  本文　アイテム部：比較結果出力先ファイル名
 *
 */
public class MailElementBodyItemCompareResultFileNamePlugInImpl extends MailElementPlugInImpl {

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
		EIMObject object = (EIMObject)EIMThreadContext.get("PDF.COMP.OBJ");
		if(object == null){
			returnArray[0] = "";
		}
		else{
			returnArray[0] = object.getName();
		}
		return returnArray;

	}

}
