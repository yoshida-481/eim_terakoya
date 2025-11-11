package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.util.List;
import java.util.Map;

import app.document.approve.ApproveCommonUtil;
import eim.bo.EIMObject;
import eim.bo.EIMStatusType;
import eim.bo.EIMWorkFlow;
import eim.net.EIMSession;
import eim.util.WorkFlowUtils;
import jp.co.ctc_g.eim.app.document.common.util.AppWorkFlowUtil;
import jp.co.ctc_g.eim.framework.business.domain.GuardConditionExecDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework.business.service.impl.GuardConditionPlugInImpl;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * ガード条件「PDF変換対象」
 * 
 */
public class GuardCndPDFConversionPlugInImpl extends GuardConditionPlugInImpl {

	/**
	 * ガード条件を判定します。
	 * 
	 * @see jp.co.ctc_g.eim.framework.business.service.impl.GuardConditionPlugInImpl#judge(jp.co.ctc_g.eim.framework.business.domain.GuardConditionExecDomain)
	 */
	public boolean judge(GuardConditionExecDomain guardConditionExecDomain) throws Exception
	{
		// 承認不要WFでは、署名オブジェクト作成タイミングが要承認WFと異なる。
		// PDF署名にて不都合があるため、
		// 意図するガード条件判定をするために、特殊な処理を行う

		EIMSession sess = EIMThreadContext.getEIMSession();
		ObjectDomain objDomain = guardConditionExecDomain.getObject();
		EIMObject object = objDomain.createEIMObject();
		boolean unnecessaryWFFlag= AppWorkFlowUtil.isUnnecessaryWF(sess, object);
		
		Map<String, Object> paramMap = guardConditionExecDomain.getParamMap();
		// forcastStatusTypeIdが公開済ステータスタイプIDかチェック
		// forcastStatusTypeIdがnullの場合は即公開処理の為、ガード条件の判定を行う
		EIMStatusType lastStatus = getLastStatus(sess, object);
		String forcastStatusTypeId = (String)paramMap.get("forcastStatusTypeId");
		if(forcastStatusTypeId != null && !forcastStatusTypeId.equals("")){
			// forcastStatusTypeIdが公開済の場合
			if(lastStatus.getId() != Long.parseLong(forcastStatusTypeId)){
				return false;
			}
		}

		if( unnecessaryWFFlag == true)
		{
			// PDF変換／PDF分割／PDF署名／URL挿入を行う場合true
			return( ApproveCommonUtil.isNeedAsyncProcess(sess, object, guardConditionExecDomain) );
		}
		else
		{
			// PDF変換／PDF分割／PDF署名／URL挿入を行う場合true
			return( ApproveCommonUtil.isNeedAsyncProcess(sess, object, null) );
		}
	}
	
	/**
	 * ガード条件アクションを実行します。
	 * 
	 * @see jp.co.ctc_g.eim.framework.business.service.impl.GuardConditionPlugInImpl#doAction(jp.co.ctc_g.eim.framework.business.domain.GuardConditionExecDomain)
	 */
	public void doAction(GuardConditionExecDomain guardConditionExecDomain) throws Exception
	{
		EIMSession sess = EIMThreadContext.getEIMSession();
		EIMObject object = guardConditionExecDomain.getObject().createEIMObject();
		
		// PDF関連設定を行う
		ApproveCommonUtil.doPublishCommandOnline(sess, object);
	}
	
	/**
	 * 公開済みステータスを取得する
	 */
	private EIMStatusType getLastStatus(EIMSession sess, EIMObject object) throws Exception{
		EIMWorkFlow workflow = WorkFlowUtils.getWorkFlowByStatusType(sess, object.getStatus().getType());
		List<EIMStatusType> statusTypeList = workflow.getStatusTypeList();
		EIMStatusType lastStatus = statusTypeList.get(statusTypeList.size()-1);
		return lastStatus;
	}
}
