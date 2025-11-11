package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.util.ArrayList;
import java.util.List;

import common.util.AppConstant;

import eim.bo.EIMAttribute;
import eim.bo.EIMObject;
import eim.bo.EIMUser;
import eim.util.EIMConfig;
import eim.util.UserUtils;
import jp.co.ctc_g.eim.framework.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework.business.service.impl.AbstractUserDefGroupPlugInImpl;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * メール送信先設定用クラス  PDF結合オブジェクトの登録ユーザ
 *
 * ※ ユーザ定義グループではないが、メール送信先に設定する際には
 *    AbstractUserDefGroupPlugInImplを継承する。
 *    
 */
public class UserDefRegistUserOfPDFJoinUserAttributePlugInImpl extends AbstractUserDefGroupPlugInImpl {

	List<UserDomain> userList = new ArrayList<UserDomain>();

	/**
	 * メール送信先 取得
	 * 
	 * @param ObjectDomain 結合ドキュメントオブジェクト
	 * 
	 * @return メール送信先：PDF結合オブジェクトの登録者
	 */
	public List<UserDomain> getUserListByObject(ObjectDomain object)
	throws Exception
	{

		EIMObject eimObj = object.createEIMObject();
		EIMAttribute regUserAttr = eimObj.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_PDFJOIN_USER"));

		EIMUser regUser = UserUtils.getUserById(EIMThreadContext.getEIMSession(), regUserAttr.getInt());
		
		UserDomain userDomain = new UserDomain(regUser);
		if (userDomain.getDisable() == AppConstant.INVALID_FLAG_OFF) {	// 無効フラグ判定
			userList.add(userDomain);
		}

		return userList;
	}

}
