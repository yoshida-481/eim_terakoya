package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.util.ArrayList;
import java.util.List;

import common.util.AppConstant;
import eim.bo.EIMAccessRole;
import eim.bo.EIMAttribute;
import eim.bo.EIMComp;
import eim.bo.EIMGroup;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMRole;
import eim.bo.EIMUser;
import eim.net.EIMSession;
import eim.util.CompUtils;
import eim.util.EIMConfig;
import eim.util.GroupUtils;
import eim.util.ObjectUtils;
import eim.util.RoleUtils;
import eim.util.SecurityUtils;
import eim.util.UserUtils;
import jp.co.ctc_g.eim.app.document.business.util.AppMailUtils;
import jp.co.ctc_g.eim.framework.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework.business.service.impl.AbstractUserDefGroupPlugInImpl;
import jp.co.ctc_g.eim.framework.common.exception.EIMAppException;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * 公開通知先に指定されたユーザ
 * ・メール通知．公開通知送信先
 * 
 * 公開通知の送信先としての使用を想定する。
 */

@SuppressWarnings("unchecked")
public class UserDefAddressPublishPlugInImpl extends
		AbstractUserDefGroupPlugInImpl {

	private static final int TYPE_USER = 1;
	private static final int TYPE_GROUP = 2;
	private static final int TYPE_ROLE = 3;
	private static final int TYPE_COMP = 4;
	private static final String SENDTO_DELIMITER = ":";
	private static final int SENDTO_ELEMENT_ORDER_TYPE = 0;
	private static final int SENDTO_ELEMENT_ORDER_ID = 1;
	
	
	@Override
	public List<UserDomain> getUserListByObject(ObjectDomain object)
			throws Exception {
		
		List<UserDomain> tmpUserList = new ArrayList<UserDomain>();
		List<UserDomain> sendUserList = new ArrayList<UserDomain>();
 		
		EIMSession sess = EIMThreadContext.getEIMSession();

		EIMObjectType objTypeNoticeMail = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_MAIL_NOTIFY"));
		EIMObject objNoticeMail = ObjectUtils.getObjectByTypeAndName(sess, objTypeNoticeMail, Long.toString(object.getId()));
		
		EIMAttribute attrSendTo = objNoticeMail.getAttribute(EIMConfig.get("ATTR_NAME_MAIL_NOTIFY_PUBLIC_NOTIFY_TO"));
		
		//公開通知送信先がnullの場合は公開通知先が指定されていないので空のユーザリストを返却する
		if(attrSendTo == null ) return sendUserList;
			
		String[] sendTos = attrSendTo.getStrings();
		
		// ユーザ一覧を取得する
		for (int i=0; i<sendTos.length; i++)
		{
			String sendTo = sendTos[i];
			String[] sendToElements = sendTo.split(SENDTO_DELIMITER);
			
			long id = Long.parseLong(sendToElements[SENDTO_ELEMENT_ORDER_ID]);
			int type = Integer.parseInt(sendToElements[SENDTO_ELEMENT_ORDER_TYPE]);
			
			if (TYPE_USER == type)
			{
				EIMUser user = UserUtils.getUserById(sess, id);
				UserDomain userDomain = new UserDomain(user);
				if (userDomain.getDisable() == AppConstant.INVALID_FLAG_OFF) {	// 無効フラグ判定
					tmpUserList.add(userDomain);
				}
			}
			else if (TYPE_GROUP == type)
			{
				EIMGroup group = GroupUtils.getGroupById(sess, id);
				List<EIMUser> grouptmpUserList = GroupUtils.getUserListRecurrently(sess, group, false);
				for (EIMUser user : grouptmpUserList)
				{
					UserDomain userDomain = new UserDomain(user);
					if (userDomain.getDisable() == AppConstant.INVALID_FLAG_OFF) {	// 無効フラグ判定
						tmpUserList.add(userDomain);
					}
				}
			}
			else if (TYPE_ROLE == type)
			{
				EIMRole role = RoleUtils.getRoleById(sess, id);
				List<EIMUser> roletmpUserList = RoleUtils.getUserList(sess, role);
				for (EIMUser user : roletmpUserList)
				{
					UserDomain userDomain = new UserDomain(user);
					if (userDomain.getDisable() == AppConstant.INVALID_FLAG_OFF) {	// 無効フラグ判定
						tmpUserList.add(userDomain);
					}
				}
			}
			else if (TYPE_COMP == type){
				
				EIMComp comp = CompUtils.getCompById(sess, id);
				List<EIMUser> comptmpUserList = CompUtils.getUserList(sess, comp);
				for (EIMUser user : comptmpUserList)
				{
					UserDomain userDomain = new UserDomain(user);
					if (userDomain.getDisable() == AppConstant.INVALID_FLAG_OFF) {	// 無効フラグ判定
						tmpUserList.add(userDomain);
					}
				}
			}
			else {
				
				throw new EIMAppException("EIM.ERROR.SYSTEMERROR");
			}
		}
		
		// 重複除去
		tmpUserList = AppMailUtils.removeDuplicateUser(tmpUserList, false);
		
		// オブジェクトに対して読み取り権限の無いユーザは対象とならない
		for (UserDomain user : tmpUserList) {
			if (SecurityUtils.authorized(sess, object.createEIMObject(), user.createEIMUser(), EIMAccessRole.READ)) {
				sendUserList.add(user);
			}
		}
	
		return sendUserList;
	}
}
