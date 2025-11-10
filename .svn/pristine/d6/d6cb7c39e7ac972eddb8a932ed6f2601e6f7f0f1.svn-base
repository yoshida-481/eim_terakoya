package jp.co.ctc_g.eim.app.document.integration.dao.impl;

import common.util.AppObjectUtil;
import common.util.AppUpdateNoticeUtils;
import eim.bo.EIMObject;
import eim.net.EIMSession;

import jp.co.ctc_g.eim.app.document.common.util.AppWorkFlowUtil;
import jp.co.ctc_g.eim.framework.business.domain.EventExecDomain;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import jp.co.ctc_g.eim.framework.integration.dao.impl.EventExecDaoImpl;

public class EventExecDocDaoImpl extends EventExecDaoImpl {

	public void changeStatus(EventExecDomain eventExec) throws Exception {
		EIMSession sess = EIMThreadContext.getEIMSession();
		
		//継承元の処理を行う
		super.changeStatus(eventExec);
		
		//--以下、ドキュメント管理用の独自実装
		EIMObject object = eventExec.getObject().createEIMObject();
		//WF付きﾌｫﾙﾀﾞの場合、配下のドキュメントにWF付きﾌｫﾙﾀﾞと同じステータスをセットする
		//if(!AppObjectUtil.isWFFolder(sess, object)){
		//if(object.getStatus() != null && object.getStatus().getId() != 0){
		if(AppWorkFlowUtil.isWorkFlowFolder(object)){
			AppObjectUtil.updateStatusRecurrently(sess, object, eventExec.getToStatus().createEIMStatus());

		}

		// SearchFramework 検索FW更新通知 対象：WF付きフォルダ、ドキュメントと配下のフォルダ、ドキュメント
		AppUpdateNoticeUtils.updateNoticeInsertWFObject(sess, object,
				"SEARCHFW_STATUS_DOEVENT_DOCUMENT", "SEARCHFW_STATUS_DOEVENT_FOLDER",
				"SEARCHFW_STATUS_DOEVENT_FOLDER_CHILD_DOCUMENT", "SEARCHFW_STATUS_DOEVENT_FOLDER_CHILD_FOLDER");
		
	}
}
