package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.util.ArrayList;
import java.util.List;

import jp.co.ctc_g.eim.app.document.business.util.AppMailUtils;
import jp.co.ctc_g.eim.framework.business.dao.EventHistoryDao;
import jp.co.ctc_g.eim.framework.business.domain.EventHistoryDomain;
import jp.co.ctc_g.eim.framework.business.domain.EventLogDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.EventDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework.business.service.UserDefGroupPlugIn;
import jp.co.ctc_g.eim.framework.business.service.impl.AbstractUserDefGroupPlugInImpl;
import jp.co.ctc_g.eim.framework.common.exception.EIMAppException;
import jp.co.ctc_g.eim.framework.common.exception.EIMSysException;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

import common.util.AppConstant;
import common.util.AppObjectUtil;

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

/**
 * 公開取消通知に設定するユーザ
 * 
 * 公開取消通知の送信先としての使用を想定する。
 */

public class UserDefAddressPublishCancelPlugInImpl extends
AbstractUserDefGroupPlugInImpl {

	protected EventHistoryDao eventHistoryDao;

	public void setEventHistoryDao(EventHistoryDao eventHistoryDao) {
		this.eventHistoryDao = eventHistoryDao;
	}

	private static final int TYPE_USER = 1;
	private static final int TYPE_GROUP = 2;
	private static final int TYPE_ROLE = 3;
	private static final int TYPE_COMP = 4;
	private static final String SENDTO_DELIMITER = ":";
	private static final int SENDTO_ELEMENT_ORDER_TYPE = 0;
	private static final int SENDTO_ELEMENT_ORDER_ID = 1;

	@SuppressWarnings("unchecked")
	@Override
	public List<UserDomain> getUserListByObject(ObjectDomain object)
			throws Exception {

		EventHistoryDomain eventHistory = eventHistoryDao.getByObjId(object.getId());
		List<EventLogDomain> eventLogList = eventHistory.getEventLogList();
		List<UserDomain> sentToUserList = new ArrayList<UserDomain>();
		List<UserDomain> tmpUserList = new ArrayList<UserDomain>();
		EIMSession sess = EIMThreadContext.getEIMSession();

		if (eventLogList.size() == 0) {
			throw new EIMSysException("EIM.ERROR.LOGIC.MAIL.NOSENDTO");
		}
		// 過去のイベント実行者
		for (int i=eventLogList.size()-2; 0<=i; i--) {
			// イベントから実行ユーザを取得
			EventDomain event = eventLogList.get(i).getEvent();
			if (event.getFromStatus().getStatusType().getStatusTypeKind().getId() == AppConstant.STATUS_TYPE_KIND_ID_PROCESSING_PUBLIC) {
				continue;
			}
			UserDomain userDomain = event.getCUser();
			// イベント実行者が無効ユーザでない場合、追加
			if (userDomain.getDisable() == AppConstant.INVALID_FLAG_OFF) {
				tmpUserList.add(userDomain);
			}
			// 直近の承認依頼イベントまでさかのぼったら、ループを抜ける
			if (event.getFromStatus().getStatusType().getSeq() == 1) {
				break;
			}
		}

		// 公開通知者
		EIMObjectType objTypeNoticeMail = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_MAIL_NOTIFY"));
		EIMObject objNoticeMail = ObjectUtils.getObjectByTypeAndName(sess, objTypeNoticeMail, String.valueOf(object.getId()));
		
		// 受信確認オブジェクトを削除する
		EIMObject eimObj = new EIMObject(new Long(object.getId()).longValue(), objTypeNoticeMail, object.getName(), object.getRev(), object.isLatest(), null, null, null, null, null, null, false, false, null);
		AppObjectUtil.deleteReceiveObject(sess, eimObj);
		
		EIMAttribute attrSendTo = null;

		if (objNoticeMail != null) {
			attrSendTo = objNoticeMail.getAttribute(EIMConfig.get("ATTR_NAME_MAIL_NOTIFY_PUBLIC_NOTIFY_TO"));
		}
		if (attrSendTo != null) {
			String[] sendTos = attrSendTo.getStrings();
			if (sendTos != null) {
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
			}
		}

		// 重複除去
		tmpUserList = AppMailUtils.removeDuplicateUser(tmpUserList, false);

		// オブジェクトに対して読み取り権限の無いユーザは対象とならない
		for (UserDomain user : tmpUserList) {
			if (SecurityUtils.enabled(sess, object.createEIMObject(), user.createEIMUser(), EIMAccessRole.READ)) {
				sentToUserList.add(user);
			}
		}
		return sentToUserList;
	}
}
