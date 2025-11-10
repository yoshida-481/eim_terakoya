package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.util.ArrayList;
import java.util.List;

import common.util.AppConstant;
import common.util.AppObjectUtil;
import eim.bo.EIMObject;
import eim.net.EIMSession;
import eim.util.EIMConfig;

import jp.co.ctc_g.eim.framework.business.dao.EventHistoryDao;
import jp.co.ctc_g.eim.framework.business.domain.EventHistoryDomain;
import jp.co.ctc_g.eim.framework.business.domain.EventLogDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.EventDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.WorkFlowDomain;
import jp.co.ctc_g.eim.framework.business.service.impl.AbstractUserDefGroupPlugInImpl;
import jp.co.ctc_g.eim.framework.common.exception.EIMSysException;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * 承認を依頼したユーザ
 * ・最後の「承認依頼」イベントの実行者
 *
 * 承認通知の送信先としての使用を想定する。
 */
public class UserDefAddressApprovedPlugInImpl extends
		AbstractUserDefGroupPlugInImpl {
	
	protected EventHistoryDao eventHistoryDao;

	public void setEventHistoryDao(EventHistoryDao eventHistoryDao) {
		this.eventHistoryDao = eventHistoryDao;
	}

	@Override
	public List<UserDomain> getUserListByObject(ObjectDomain object)
			throws Exception {

		EventHistoryDomain eventHistory = eventHistoryDao.getByObjId(object.getId());
		List<EventLogDomain> eventLogList = eventHistory.getEventLogList();

		if (eventLogList.size() == 0) {
			throw new EIMSysException("EIM.ERROR.LOGIC.MAIL.NOSENDTO");
		}

		// 承認不要WF判定のための情報取得
		String bossApproval = AppConstant.BOSSAPPROVAL_NECESSARY;
		EIMSession sess = EIMThreadContext.getEIMSession();
		WorkFlowDomain workflow = eventHistory.getWorkflow();
		if( workflow != null )
		{
			EIMObject wfSettingObj = AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_WFSETTING"), String.valueOf(workflow.getId()));
			if( wfSettingObj != null )
			{
				// 承認不要WF/要承認WFのチェック
				String defBossApproval = AppObjectUtil.getStrAttr(sess, wfSettingObj, EIMConfig.get("ATTR_NAME_WFSETTING_BOSS_APPROVAL_FLG"));
				if( defBossApproval != null ) 
				{
					bossApproval = defBossApproval;
				}
			}
		}
		
		if( bossApproval.equals(AppConstant.BOSSAPPROVAL_NECESSARY) )
		{
			for (int i=eventLogList.size()-2; 0<=i; i--)
			{
				EventDomain event = eventLogList.get(i).getEvent();
				
				//イベントのFromStatusTypeのシーケンスが１であることと、
				//イベントが承認依頼イベントであることは、等価。
				if ( 1 == event.getFromStatus().getStatusType().getSeq() )
				{
					List<UserDomain> userDomainList = new ArrayList<UserDomain>();
					UserDomain userDomain = event.getCUser();
//					if (userDomain.getDisable() == AppConstant.INVALID_FLAG_OFF) {	// 無効フラグ判定
						userDomainList.add(userDomain);
//					}
					return userDomainList;
				}
			}
		}
		else
		{
			// PDF変換対象外のワークフローはイベントが1つのみ
			EventDomain eventDomain = new EventDomain();
			if( eventLogList.size() >= 2 )
			{
				// PDF変換対象
				eventDomain = eventLogList.get(eventLogList.size()-2).getEvent();
			}
			else
			{
				// PDF変換対象外
				eventDomain = eventLogList.get(0).getEvent();
			}
			
			// 承認不要WFの公開で発行されたイベント用
			List<UserDomain> userDomainList = new ArrayList<UserDomain>();
			UserDomain userDomain = eventDomain.getCUser();
			userDomainList.add(userDomain);
			return userDomainList;
		}
		
		//ありえないはず
		throw new EIMSysException("EIM.ERROR.LOGIC.MAIL.NOSENDTO");
	}

}
