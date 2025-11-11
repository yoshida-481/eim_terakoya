package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.util.List;

import common.util.AppConstant;
import common.util.AppObjectConditionHelper;
import common.util.AppObjectUtil;
import eim.bo.EIMObject;
import eim.bo.EIMResource;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.ObjectUtils;
import eim.util.StringUtils;
import jp.co.ctc_g.eim.app.document.business.domain.WorkFlowHistoryDomain;
import jp.co.ctc_g.eim.framework.business.dao.StatusDao;
import jp.co.ctc_g.eim.framework.business.domain.EventHistoryDomain;
import jp.co.ctc_g.eim.framework.business.domain.EventLogDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.StatusDomain;
import jp.co.ctc_g.eim.framework.business.service.impl.EventHistoryServiceImpl;
import jp.co.ctc_g.eim.framework.common.exception.EIMAppException;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

public class WorkFlowHistoryServiceImpl extends EventHistoryServiceImpl {

	private StatusDao statusDao;

	/**
	 * @see jp.co.ctc_g.eim.framework.business.service.EventHistoryService#getByObjId(int)
	 */
	public EventHistoryDomain getByObjId(long objectId) throws Exception
	{
		EIMSession sess = EIMThreadContext.getEIMSession();
		
		// 選択ドキュメント/フォルダ
		// WF付きフォルダ以下のドキュメントの場合、WF付きフォルダを対象として処理を行う
		EIMObject object = ObjectUtils.getObjectById(sess, objectId);
		if(object == null)
		{
			throw new EIMAppException(sess, EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOTEXIST.DOCFOL"));
		}
		
		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
		
		// ワークフローなしフォルダの場合
		if (!helper.isTypeOfDocument(object.getType()) && !AppObjectUtil.isWFFolder(sess, object)) 
		{
			//WFなしフォルダ
			throw new EIMAppException(sess,  "EIM.ERROR.LOGIC.NOSELECT.DOC.OR.WFFOLDER", new Object[]{StringUtils.xmlEncode(object.getName())});
		}
		
		// ユーザーが対象に対して公開読取権限しかなく、ステータスが公開済で無い場合
		if (helper.isReadOnlyAccess(object) 
				&& object.getStatus() != null 
				&& object.getStatus().getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_PUBLIC)
		{
			throw new EIMAppException(sess, "EIM.ERROR.LOGIC.NOTPUBLIC", new Object[]{StringUtils.xmlEncode(object.getName())});
		}
		
		// Document
		if (helper.isTypeOfDocument(object.getType())) 
		{
			// 上位WFフォルダ属性取得
			long higherWFFolderID = AppObjectUtil.getIntAttr(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_HIGHER_WF_FOLDER"), -1);

			if (higherWFFolderID != -1)
			{
				// ターゲットを上位WFフォルダに変更
				object = ObjectUtils.getObjectById(sess, higherWFFolderID);
				// 上位WFフォルダが取得できない場合
				if (object == null)
				{
					throw new EIMAppException(sess, EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOTEXIST.HIGHRANK.WFFOLDER"));
				}
			}
			else {
				if (object.getStatus() == null) {
					//WFフォルダ以下でなく、ステータスなし
					throw new EIMAppException(sess, "EIM.ERROR.LOGIC.NOSELECT.DOC.OR.WFFOLDER", new Object[]{StringUtils.xmlEncode(object.getName())});
				}
			}
		}

		// イベント履歴を取得
		EventHistoryDomain evtHistory = super.getByObjId(objectId);
		WorkFlowHistoryDomain wfHistory = new WorkFlowHistoryDomain();
		List<EventLogDomain> evntList = evtHistory.getEventLogList();
		
		List<EventLogDomain> sortAfterList = sortID(evntList);
		
		wfHistory.setEventLogList(sortAfterList);
		wfHistory.setWorkflow(evtHistory.getWorkflow());
		
		// ステータス一覧を取得
		StatusDomain status = new StatusDomain();
		status.setObject(new ObjectDomain(object));
		List<StatusDomain> statusList = statusDao.getList(status);
		//属性を取得
		for(int i = 0; i < statusList.size(); i++){
			StatusDomain st = statusList.get(i);
			st.setObject(new ObjectDomain(object));
		}
		wfHistory.setStatusList(statusList);
		
		return wfHistory;
	}
	
	/**
	 * 日時が全く同じ場合、正しくソートされないため
	 * イベントIDでもソートを行なう。
	 * 
	 * */
	public List<EventLogDomain> sortID(List<EventLogDomain> list){
		
		for(int i = 0 ; i < list.size()-1 ; i++){
			
			for(int j=list.size()-1;j>i;j--){
				
				EventLogDomain target = list.get(j);
				EventLogDomain comp = list.get(j-1);
				long evTargetId = list.get(j).getEvent().getId();
				long evComId = list.get(j-1).getEvent().getId();
				
				if(evTargetId < evComId){
					
					EventLogDomain tmp =  list.get(j);
					target = comp;
					comp = tmp;
					
					list.set(j, target);
					list.set(j-1, comp);
				}
			}
		}
		
		return list;
	}
	
	/**
	 * @return the statusDao
	 */
	public StatusDao getStatusDao() {
		return statusDao;
	}

	/**
	 * @param statusDao the statusDao to set
	 */
	public void setStatusDao(StatusDao statusDao) {
		this.statusDao = statusDao;
	}
	
}
