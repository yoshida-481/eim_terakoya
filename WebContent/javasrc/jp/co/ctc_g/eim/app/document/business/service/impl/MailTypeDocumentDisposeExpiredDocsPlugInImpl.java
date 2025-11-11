package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.util.ArrayList;
import java.util.List;

import jp.co.ctc_g.eim.framework2.business.dao.UserDao;
import jp.co.ctc_g.eim.framework2.business.domain.entity.MailDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework2.business.plugin.UserDefinitionGroupPlugIn;
import jp.co.ctc_g.eim.framework2.business.plugin.impl.DefaultMailTypePlugInImpl;
import jp.co.ctc_g.eim.framework2.common.exception.EIMException;

/**
 * 有効期限切れドキュメント削除通知メール実装クラス
 *
 */
public class MailTypeDocumentDisposeExpiredDocsPlugInImpl extends DefaultMailTypePlugInImpl {

	private UserDao userDao;

	/**
	 * @see jp.co.ctc_g.eim.framework2.business.plugin.MailTypePlugIn#getToUserList(jp.co.ctc_g.eim.framework2.business.domain.entity.MailDomain)
	 * @since Ver5.0
	 */
	@Override
	public List<UserDomain> getToUserList(MailDomain mail) throws Exception {

		if(mail == null){
			// 引数mailとしてnullが入力されました。
			throw new EIMException("EIM.ERROR.LOGIC.MAIL.VALUE.ILLEGAL");
		}

		// メール送信先(to)リストチェック
		if (getUserDefineEntryToList() == null) {
			Object[] param = {"userDefineEntryToList"};
			throw new EIMException("EIM.ERROR.MAIL.PARAM.NOTFOUND", param);
		}

		return getUserDomainList(mail, getUserDefineEntryToList());
	}


	private List<UserDomain> getUserDomainList(MailDomain mail, List<UserDefinitionGroupPlugIn> userDefineGroupList) throws Exception {

		// 返却値
		List<UserDomain> userDomainList = new ArrayList<UserDomain>();

		for (UserDefinitionGroupPlugIn userDefineGroupPlugIn : userDefineGroupList) {
			for (UserDomain userDomain : userDefineGroupPlugIn.getUserListByObject(mail.getObject())) {
				userDomainList.add(userDomain);
			}
		}

		return userDomainList;
	}

	/**
	 * ユーザDaoを取得します。
	 * @return ユーザDao
	 */
	public UserDao getUserDao() {
		return userDao;
	}

	/**
	 * ユーザDaoを設定します。
	 * @param userDao ユーザDao
	 */
	public void setUserDao(UserDao userDao) {
		this.userDao = userDao;
	}
}
