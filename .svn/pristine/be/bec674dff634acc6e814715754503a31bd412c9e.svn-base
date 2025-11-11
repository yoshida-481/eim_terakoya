package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.util.ArrayList;
import java.util.List;

import eim.bo.EIMObject;
import eim.bo.EIMUser;

import jp.co.ctc_g.eim.framework.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework.business.service.impl.AbstractUserDefGroupPlugInImpl;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * メール送信先設定用クラス  PDF結合オブジェクトの作成ユーザ
 *
 *[トリガ]
 *  ・公開ファイル比較 実行時
 *
 */
public class UserDefCUserOfPDFJoinObjectPlugInImpl extends AbstractUserDefGroupPlugInImpl {

	List<UserDomain> userList = new ArrayList<UserDomain>();

	/**
	 * メール送信先 取得 ログインユーザー
	 * 
	 * @param ObjectDomain 
	 * 
	 * @return メール送信先：ログインユーザー
	 */
	public List<UserDomain> getUserListByObject(ObjectDomain object)
	throws Exception {
		
		List<UserDomain> userList = new ArrayList<UserDomain>();
		
		// 公開ファイル比較オブジェクト取得
		EIMObject pdfCompObj = (EIMObject)EIMThreadContext.get("JOIN.OBJ");
		if(pdfCompObj == null) {
			return userList;
		}
		
		EIMUser user = pdfCompObj.getCreateUser();
		
		UserDomain userDomain = new UserDomain(user);
		userList.add(userDomain);
		return userList;
	}

}
