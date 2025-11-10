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
 * メール送信先設定用クラス  PDFセキュリティ設定オブジェクトの登録ユーザ
 *
 * ※ ユーザ定義グループではないが、メール送信先に設定する際には
 *    AbstractUserDefGroupPlugInImplを継承する。
 *    
 */
public class UserDefCUserOfPDFSecuritySettingObjectPlugInImpl extends AbstractUserDefGroupPlugInImpl {

	List<UserDomain> userList = new ArrayList<UserDomain>();

	/**
	 * メール送信先 取得
	 * 
	 * @param ObjectDomain PDFセキュリティ設定オブジェクト
	 * 
	 * @return メール送信先：PDFセキュリティ設定オブジェクトの登録ユーザ
	 */
	public List<UserDomain> getUserListByObject(ObjectDomain object)
	throws Exception
	{
		
		// 公開ファイル比較オブジェクト取得
		EIMObject secObj = (EIMObject)EIMThreadContext.get("PDF.SEQ.OBJ");
		if(secObj == null) {
			return userList;
		}

		EIMAttribute uid = secObj.getAttribute(EIMConfig.get("ATTR_NAME_WEPUB_PDF_PDFSIG_USER"));

		EIMUser regUser = UserUtils.getUserById(EIMThreadContext.getEIMSession(), uid.getInt());

		UserDomain userDomain = new UserDomain(regUser);
		if (userDomain.getDisable() == AppConstant.INVALID_FLAG_OFF) {	// 無効フラグ判定
			userList.add(userDomain);
		}

		return userList;
	}

}
