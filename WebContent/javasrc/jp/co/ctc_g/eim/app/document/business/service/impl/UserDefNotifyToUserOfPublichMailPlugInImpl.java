package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import common.util.AppConstant;
import common.util.AppObjectUtil;

import app.document.approve.ApproveCommonUtil;

import eim.bo.EIMAccessRole;
import eim.bo.EIMAttribute;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMUser;
import eim.util.EIMConfig;
import eim.util.ObjectUtils;
import eim.util.SecurityUtils;
import jp.co.ctc_g.eim.framework.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework.business.service.impl.AbstractUserDefGroupPlugInImpl;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * メール送信先設定用クラス  公開通知先
 *
 * ※ ユーザ定義グループではないが、メール送信先に設定する際には
 *    AbstractUserDefGroupPlugInImplを継承する。
 *    
 */
public class UserDefNotifyToUserOfPublichMailPlugInImpl extends AbstractUserDefGroupPlugInImpl {

	List<UserDomain> userList = new ArrayList<UserDomain>();

	/**
	 * メール送信先 取得
	 * 
	 * @param ObjectDomain 結合ドキュメントオブジェクト
	 * 
	 * @return メール送信先：公開通知オブジェクトの通知先
	 */
	public List<UserDomain> getUserListByObject(ObjectDomain object)
	throws Exception
	{
		// 「メール通知」オブジェクトを取得
		EIMObjectType objTypeMailNotify = ObjectUtils.getObjectTypeByName(EIMThreadContext.getEIMSession(), EIMConfig.getValue("OBJECT_TYPE_NAME_MAIL_NOTIFY"));
		EIMObject mailNotifyObj = ObjectUtils.getObjectByTypeAndName(EIMThreadContext.getEIMSession(), objTypeMailNotify, String.valueOf(object.getId()));
		// ｢公開通知送信先｣属性を取得
		List mailList = null;
		if(AppObjectUtil.getStrAttrs(EIMThreadContext.getEIMSession(), mailNotifyObj, EIMConfig.get("ATTR_NAME_MAIL_NOTIFY_PUBLIC_NOTIFY_TO")) != null){
			mailList = java.util.Arrays.asList(AppObjectUtil.getStrAttrs(EIMThreadContext.getEIMSession(), mailNotifyObj, EIMConfig.get("ATTR_NAME_MAIL_NOTIFY_PUBLIC_NOTIFY_TO")));
		}
		
		List<EIMUser> pblisherList = new ArrayList<EIMUser>();
		for(int i=0; i < mailList.size(); i++) {
			pblisherList.addAll(ApproveCommonUtil.getUserFromCode(EIMThreadContext.getEIMSession(), mailList.get(i).toString()));
		}
		
		for (Iterator iter = pblisherList.iterator(); iter.hasNext();) {
			EIMUser user = (EIMUser)iter.next();

			//ユーザーがドキュメントに対して公開読取権を持つかどうかチェックする
			//公開読取権を持たない場合はメールの送信を行わない
			if( !SecurityUtils.authorized(EIMThreadContext.getEIMSession(), object.createEIMObject(), user, EIMAccessRole.READ) ) {
				continue;
			}
			
			UserDomain userDomain = new UserDomain(user);
			if (userDomain.getDisable() == AppConstant.INVALID_FLAG_OFF) {	// 無効フラグ判定
				userList.add(userDomain);
			}
		}
		return userList;
	}

}
