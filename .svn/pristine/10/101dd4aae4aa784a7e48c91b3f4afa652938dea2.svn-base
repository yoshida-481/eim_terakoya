package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.util.ArrayList;
import java.util.List;

import eim.bo.EIMUser;
import jp.co.ctc_g.eim.framework.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework.business.service.impl.AbstractUserDefGroupPlugInImpl;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * メール送信先設定用クラス  承認通知
 *
 * ※ ユーザ定義グループではないが、メール送信先に設定する際には
 *    AbstractUserDefGroupPlugInImplを継承する。
 */
public class UserDefDummyPlugInImpl extends AbstractUserDefGroupPlugInImpl {

	List<UserDomain> userList = new ArrayList<UserDomain>();

	public List<UserDomain> getUserListByObject(ObjectDomain object)
	throws Exception
	{

		// ToDo スタブ作成 2010.01.07 単体テスト用として、セッションのユーザ情報(ログインユーザ)からアドレス取得
		EIMUser user = EIMThreadContext.getEIMSession().getUser();
		UserDomain domain = new UserDomain(user);
		userList.add(domain);

		return userList;
	}

}
