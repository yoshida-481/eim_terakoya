package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.util.ArrayList;
import java.util.List;

import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework2.business.plugin.impl.AbstractUserDefGroupPlugInImpl;

/**
 * 有効期限切れドキュメント削除通知メール送信先設定用クラス
 *
 * ※ ユーザ定義グループではないが、メール送信先に設定する際には
 *    AbstractUserDefGroupPlugInImplを継承する。
 *
 */
public class UserDefAddressDisposeExpiredDocsPlugInImpl extends AbstractUserDefGroupPlugInImpl {

	List<UserDomain> userList = new ArrayList<UserDomain>();

	/**
	 * メール送信先 取得 更新者
	 *
	 * [特記事項]
	 * 	本メソッド呼出前には、必ずメールアドレス(EIM.MAIL.ADDRESS)をobject.getModificationUserに設定すること
	 *
	 * @param ObjectDomain
	 *
	 * @return メール送信先：更新ユーザ
	 */
	@Override
	public List<UserDomain> getUserListByObject(ObjectDomain object) throws Exception {

		List<UserDomain> userList = new ArrayList<UserDomain>();

		UserDomain destinationUser = object.getModificationUser();
		userList.add(destinationUser);

		return userList;
	}

}
