package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.util.ArrayList;
import java.util.List;

import common.util.AppConstant;

import eim.bo.EIMUser;
import eim.net.EIMSession;
import eim.util.UserUtils;

import jp.co.ctc_g.eim.app.document.business.util.AppMailUtils;
import jp.co.ctc_g.eim.framework.business.dao.AssignDao;
import jp.co.ctc_g.eim.framework.business.dao.EventHistoryDao;
import jp.co.ctc_g.eim.framework.business.dao.StatusDao;
import jp.co.ctc_g.eim.framework.business.domain.AssignDomain;
import jp.co.ctc_g.eim.framework.business.domain.EventHistoryDomain;
import jp.co.ctc_g.eim.framework.business.domain.EventLogDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.EventDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.StatusDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework.business.service.impl.AbstractUserDefGroupPlugInImpl;
import jp.co.ctc_g.eim.framework.common.exception.EIMSysException;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * 【承認依頼取消実行によりステータスが遷移しなかった場合】
 *		・現ステータスで、承認依頼取消実行前に承認していたユーザ
 *		・上記より、セッションユーザを除く
 * 【承認依頼取消実行によりステータスが遷移した場合】
 * 		・承認依頼取消実行前ステータスでのアサイン先
 * 		・承認依頼取消実行後ステータスでのアサイン先
 * 		・上記より、セッションユーザを除く
 *
 * 承認依頼取消通知の送信先としての使用を想定する。
 */
public class UserDefAddressCancelApprovalPlugInImpl extends
		AbstractUserDefGroupPlugInImpl {

	private static final String BET_KEY_APPROVE = "BaseEvtApprovalPlugIn";
	
	protected EventHistoryDao eventHistoryDao;
	protected StatusDao statusDao;
	protected AssignDao assignDao;
	
	@Override
	public List<UserDomain> getUserListByObject(ObjectDomain object)
			throws Exception {

		EIMSession sess = EIMThreadContext.getEIMSession();
		List<UserDomain> userList = new ArrayList<UserDomain>();
		
		//イベント実行前後のステータスを取得
		EventHistoryDomain eventHistory = eventHistoryDao.getByObjId(object.getId());
		List<EventLogDomain> eventLogList = eventHistory.getEventLogList();
		if (eventLogList.size() == 0) {
			throw new EIMSysException("EIM.ERROR.LOGIC.MAIL.NOSENDTO");
		}
		EventLogDomain eventLog = eventLogList.get(eventLogList.size()-1);
		StatusDomain fromStatus = eventLog.getEvent().getFromStatus();
		StatusDomain toStatus = eventLog.getEvent().getToStatus();
		
		if (fromStatus.getStatusType().getSeq() == toStatus.getStatusType().getSeq()) {
			 /* 【承認依頼取消実行によりステータスが遷移しなかった場合】
			  *		・現ステータスで、承認依頼取消実行前に承認していたユーザ	*/
			for (int i=eventLogList.size()-2; 0<=i; i--)
			{
				//From/Toが等しい承認イベントなら、そのcUserをリストに追加する。
				//それ以外(承認依頼取消など)ならbreakする。
				EventDomain event = eventLogList.get(i).getEvent();
				if ( event.getFromStatus().getStatusType().getSeq() == event.getToStatus().getStatusType().getSeq()
				 &&  event.getEventType().getBaseEventType().getKey().equals(BET_KEY_APPROVE) )
				{
					UserDomain userDomain = event.getCUser();
					if (userDomain.getDisable() == AppConstant.INVALID_FLAG_OFF) {	// 無効フラグ判定
						userList.add(userDomain);
					}
				}
				else
				{
					break;
				}
			}
		}
		else {
			 /* 【承認依頼取消実行によりステータスが遷移した場合】
			  * 		・承認依頼取消実行前ステータスでのアサイン先
			  * 		・承認依頼取消実行後ステータスでのアサイン先	*/
			//実行前ステータスのStatusIdで、かつevid=0のタスクのownerをリストに追加
			AssignDomain fromAssignKey = new AssignDomain();
			fromAssignKey.setStatus(fromStatus);
			List<AssignDomain> fromAssignList = assignDao.getList(fromAssignKey);
			for (AssignDomain fromAssign : fromAssignList)
			{
				EIMUser user = UserUtils.getUserById(sess, fromAssign.getOwner().getId());
				UserDomain userDomain = new UserDomain(user);
				if (userDomain.getDisable() == AppConstant.INVALID_FLAG_OFF) {	// 無効フラグ判定
					userList.add(userDomain);
				}
			}
			
			//実行後ステータスのタスクユーザをリストに追加
			List<AssignDomain> toAssignList = statusDao.getAssignTaskListByStatusId(toStatus.getId());
			for (AssignDomain toAssign : toAssignList)
			{
				EIMUser user = UserUtils.getUserById(sess, toAssign.getOwner().getId());
				UserDomain userDomain = new UserDomain(user);
				if (userDomain.getDisable() == AppConstant.INVALID_FLAG_OFF) {	// 無効フラグ判定
					userList.add(userDomain);
				}
			}
		}
		
		AppMailUtils.removeDuplicateUser(userList, true);
		
		return userList;
	}

	public void setEventHistoryDao(EventHistoryDao eventHistoryDao) {
		this.eventHistoryDao = eventHistoryDao;
	}

	public void setStatusDao(StatusDao statusDao) {
		this.statusDao = statusDao;
	}

	public void setAssignDao(AssignDao assignDao) {
		this.assignDao = assignDao;
	}
}