package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.util.List;

import common.util.AppConstant;
import eim.bo.EIMUser;
import eim.net.EIMSession;
import eim.util.UserUtils;
import jp.co.ctc_g.eim.app.document.business.util.AppMailUtils;
import jp.co.ctc_g.eim.framework.business.dao.EventHistoryDao;
import jp.co.ctc_g.eim.framework.business.dao.StatusDao;
import jp.co.ctc_g.eim.framework.business.domain.AssignDomain;
import jp.co.ctc_g.eim.framework.business.domain.EventHistoryDomain;
import jp.co.ctc_g.eim.framework.business.domain.EventLogDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.EventDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.StatusDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework.business.service.UserDefGroupPlugIn;
import jp.co.ctc_g.eim.framework.business.service.impl.AbstractUserDefGroupPlugInImpl;
import jp.co.ctc_g.eim.framework.common.exception.EIMSysException;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * 承認を依頼したユーザ、これまでに承認を依頼されたユーザ
 * <li>最後の「承認依頼」イベントの実行者
 * <li>前ステータスのアサイン先
 * <li>承認実施者
 * <li>上記より、セッションユーザを除く
 *
 * 差戻し通知の送信先としての仕様を想定する。
 */
public class UserDefAddressRejectionPlugInImpl extends
			AbstractUserDefGroupPlugInImpl {

	protected EventHistoryDao eventHistoryDao;
	protected StatusDao statusDao;
	protected UserDefGroupPlugIn userDefAddressApprovedPlugIn;
	
	@Override
	public List<UserDomain> getUserListByObject(ObjectDomain object)
			throws Exception {

		/*--------------------------------------------------------------------------------------------
		 * 最後の「承認依頼」イベントの実行者 */
		
		List<UserDomain> userList = userDefAddressApprovedPlugIn.getUserListByObject(object);
		
		
		/*--------------------------------------------------------------------------------------------
		 * 全ての「承認」実行者 */
		
		//全ての承認イベントの作成者=「承認者」を取得
		EventHistoryDomain eventHistory = eventHistoryDao.getByObjId(object.getId());
		List<EventLogDomain> eventLogList = eventHistory.getEventLogList();
		if (eventLogList.size() == 0) {
			throw new EIMSysException("EIM.ERROR.LOGIC.MAIL.NOSENDTO");
		}
		
		for (int i = eventLogList.size() - 1; 0 <= i; i--) {
			
			EventDomain event = eventLogList.get(i).getEvent();
			
			// 承認イベントのユーザを追加
			if (event.getEventType().getBaseEventType().getId() == 
				AppConstant.BASE_EVENT_TYPE_ID_APPROVAL) {
				UserDomain userDomain = event.getCUser();
				
				if (userDomain.getDisable() == AppConstant.INVALID_FLAG_OFF) {	// 無効フラグ判定
					userList.add(userDomain);
				}
			}
			
			// 承認依頼イベントがあれば終了
			if ( 1 == event.getFromStatus().getStatusType().getSeq() ) {
				break;
			}
		}
		
		/*--------------------------------------------------------------------------------------------
		 * 前ステータスの承認依頼先を取得 */
		
		StatusDomain statusDomain = eventLogList.get(eventLogList.size() - 1).getEvent().getFromStatus();
		List<AssignDomain> assignList = statusDao.getAssignTaskListByStatusId(statusDomain.getId());
		
		EIMSession sess = EIMThreadContext.getEIMSession();
		for (AssignDomain assign : assignList) {
			long taskUserId = assign.getOwner().getId();
			EIMUser user = UserUtils.getUserById(sess, taskUserId);
			UserDomain userDomain = new UserDomain(user);
			if (userDomain.getDisable() == AppConstant.INVALID_FLAG_OFF) {	// 無効フラグ判定
				userList.add(userDomain);
			}
		}
		
		/*--------------------------------------------------------------------------------------------
		 * 上記より、セッションユーザを除く */
		userList = AppMailUtils.removeDuplicateUser(userList, true);
		
		
		return userList;
	}
	
	public void setStatusDao(StatusDao statusDao) {
		this.statusDao = statusDao;
	}
		
	public void setEventHistoryDao(EventHistoryDao eventHistoryDao) {
		this.eventHistoryDao = eventHistoryDao;
	}

	public void setUserDefAddressApprovedPlugIn(
			UserDefGroupPlugIn userDefAddressApprovedPlugIn) {
		this.userDefAddressApprovedPlugIn = userDefAddressApprovedPlugIn;
	}

	
	
	
}
