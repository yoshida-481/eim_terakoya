package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.util.ArrayList;
import java.util.List;

import common.util.AppConstant;

import jp.co.ctc_g.eim.framework.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework.business.service.impl.AbstractUserDefGroupPlugInImpl;

/**
 * メール送信先設定用クラス  ObjectDomain CUser(作成者)
 *
 * ※ ユーザ定義グループではないが、メール送信先に設定する際には
 *    AbstractUserDefGroupPlugInImplを継承する。
 *    
 */
public class UserDefObjectDomainCUserPlugInImpl extends AbstractUserDefGroupPlugInImpl {

	List<UserDomain> userList = new ArrayList<UserDomain>();

	/**
	 * メール送信先 取得
	 * 
	 * @param ObjectDomain 
	 * 
	 * @return メール送信先：作成者(CUSER)
	 */
	public List<UserDomain> getUserListByObject(ObjectDomain object)
	throws Exception
	{
		UserDomain userDomain = object.getCUser();
		if (userDomain.getDisable() == AppConstant.INVALID_FLAG_OFF) {	// 無効フラグ判定
			userList.add(userDomain);
		}

		return userList;
	}

}
