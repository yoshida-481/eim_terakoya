package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.util.ArrayList;
import java.util.List;

import common.util.AppConstant;

import eim.bo.EIMUser;
import eim.util.UserUtils;
import jp.co.ctc_g.eim.framework.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework.business.service.impl.AbstractUserDefGroupPlugInImpl;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * メール送信先設定用クラス  メールアドレスからユーザ情報取得
 *
 * ※ ユーザ定義グループではないが、メール送信先に設定する際には
 *    AbstractUserDefGroupPlugInImplを継承する。
 *    
 */
public class UserDefMailAddressFromUserPlugInImpl extends AbstractUserDefGroupPlugInImpl {

	List<UserDomain> userList = new ArrayList<UserDomain>();

	/**
	 * メール送信先 取得 ログインユーザー
	 * 
	 * [特記事項]
	 * 	本メソッド呼出前には、必ずメールアドレス(EIM.MAIL.ADDRESS)をEIMThreadContextに設定すること
	 * 
	 * @param ObjectDomain 
	 * 
	 * @return メール送信先：ログインユーザー
	 */
	public List<UserDomain> getUserListByObject(ObjectDomain object)
	throws Exception {
		
		List<UserDomain> userList = new ArrayList<UserDomain>();
		EIMUser user = null;
		String mailAddress = (String)EIMThreadContext.get("EIM.MAIL.ADDRESS");
		if(mailAddress != null){
			user = UserUtils.getUserByMail(EIMThreadContext.getEIMSession(), mailAddress);
		}
		
		if (user == null) {
			user = UserUtils.getUserById(EIMThreadContext.getEIMSession(), 1);
		}

		UserDomain userDomain = new UserDomain(user);
		if (userDomain.getDisable() == AppConstant.INVALID_FLAG_OFF) {	// 無効フラグ判定
			userList.add(userDomain);
		}

		return userList;
	}

}
