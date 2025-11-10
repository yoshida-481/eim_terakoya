package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.util.Map;

import common.util.AppConstant;

import jp.co.ctc_g.eim.framework.business.domain.GuardConditionExecDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.EventTypeDomain;
import jp.co.ctc_g.eim.framework.business.service.impl.GuardConditionPlugInImpl;

/**
 * ガード条件「なし」
 * 
 */
public class GuardCndAlwaysTruePlugInImpl extends GuardConditionPlugInImpl {

	/**
	 * ガード条件を判定します。
	 * 
	 * @see jp.co.ctc_g.eim.framework.business.service.impl.GuardConditionPlugInImpl#judge(jp.co.ctc_g.eim.framework.business.domain.GuardConditionExecDomain)
	 */
	@Override
	public boolean judge(GuardConditionExecDomain guardConditionExecDomain) throws Exception
	{
		
		// イベントタイプ取得
		EventTypeDomain eventType = guardConditionExecDomain.getEventType();
		
		if(eventType.getBaseEventType().getId() == AppConstant.BASE_EVENT_TYPE_ID_SEND_BACK){
			// 差戻しの場合無条件でtrue
			return true;
		}
		// パラメータ取得
		Map<String, Object> paramMap = guardConditionExecDomain.getParamMap();
		if(paramMap.get("forcastStatusTypeId") == null){
			return true;
		}else{
			// 遷移先ステータスが指定したforcastStatusTypeIdの場合trueを返却する
			String forcastStatusTypeId = (String)paramMap.get("forcastStatusTypeId");
			if(eventType.getToStatusType().getId() == Long.parseLong(forcastStatusTypeId)){
				return true;
			}
		}
		return false;
	}

}
