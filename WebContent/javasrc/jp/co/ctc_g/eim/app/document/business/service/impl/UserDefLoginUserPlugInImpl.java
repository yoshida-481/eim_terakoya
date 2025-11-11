package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.util.ArrayList;
import java.util.List;

import common.util.AppConstant;

import eim.bo.EIMUser;
import jp.co.ctc_g.eim.framework.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework.business.service.impl.AbstractUserDefGroupPlugInImpl;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * メール送信先設定用クラス  ログインユーザー
 *
 * ※ ユーザ定義グループではないが、メール送信先に設定する際には
 *    AbstractUserDefGroupPlugInImplを継承する。
 *    
 */
public class UserDefLoginUserPlugInImpl extends AbstractUserDefGroupPlugInImpl {

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
		
		EIMUser user = EIMThreadContext.getEIMSession().getUser();
		UserDomain userDomain = new UserDomain(user);
		if (userDomain.getDisable() == AppConstant.INVALID_FLAG_OFF) {	// 無効フラグ判定
			userList.add(userDomain);
		}
		return userList;
	}

}
