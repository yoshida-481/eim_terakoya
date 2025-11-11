/**
 * 
 */
package app.document.search.condition.impl;

import java.util.ArrayList;
import java.util.List;

import app.document.search.EIMDocSearchType;
import app.document.search.condition.EIMDocSearchConditionMaker;
import common.util.AppSearchUtils;
import eim.bo.EIMObject;
import eim.bo.EIMSearchConditionGroup;
import eim.bo.EIMSearchConditionIn;
import eim.bo.EIMSearchSelectEIMObject;
import eim.net.EIMSession;
import eim.util.TypeConvertUtils;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * @author z1J5161
 *
 */
public class DspApproveItemConditionMaker extends EIMDocSearchConditionMaker {
	
	/** 承認対象となるオブジェクト一覧 */
	private List<Long> approveObjId;

	/**
	 * @param type 検索種別
	 * @param userData 承認対象オブジェクト一覧
	 */
	public DspApproveItemConditionMaker(EIMDocSearchType type, Object userData) {
		super(type, userData);
	}

	/**
	 * @see app.document.search.condition.EIMDocSearchConditionMaker#getDefaultCondition()
	 */
	@Override
	public EIMSearchConditionGroup getDefaultCondition() throws Exception {
		
		EIMSearchSelectEIMObject.SearchConditionBuildHelper helper =
			new EIMSearchSelectEIMObject.SearchConditionBuildHelper();
		EIMSession sess = EIMThreadContext.getEIMSession();
		
		// 検索条件：オブジェクトタイプが「ドキュメント」か「フォルダ」
		EIMSearchConditionGroup conds = helper.group(helper.opAnd());

		// 自身に承認のあるオブジェクトがある時のみ検索を行う
		if (this.getApproveObjId().size() > 0) {
			conds.addCondition(AppSearchUtils.getApprovalItemObjTypeCondition(sess));
		
			// 検索条件：自身に承認のあるオブジェクトのID
			long[] approveIds = new long[this.getApproveObjId().size()];
			for (int i = 0 ; i < this.getApproveObjId().size() ; i++) {
				approveIds[i] = this.getApproveObjId().get(i).longValue();
			}
			conds.addCondition(new EIMSearchConditionIn(helper.opAnd(), 
					EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.ID, helper.opIn(), TypeConvertUtils.convertToBuildTypeArray(approveIds)));
		}
		
		return conds;
	}

	/**
	 * @see app.document.search.condition.EIMDocSearchConditionMaker#parseUserData(java.lang.Object)
	 */
	@Override
	public void parseUserData(Object userData) throws Exception {
		
		List<EIMObject> objList = (List<EIMObject>)userData;
		List<Long> objIdList = new ArrayList<Long>();
		
		for (EIMObject obj : objList) {
			objIdList.add(new Long(obj.getId()));
		}
		
		this.setApproveObjId(objIdList);
	}

	/**
	 * @return the approveObjId
	 */
	public List<Long> getApproveObjId() {
		return approveObjId;
	}

	/**
	 * @param approveObjId the approveObjId to set
	 */
	public void setApproveObjId(List<Long> approveObjId) {
		this.approveObjId = approveObjId;
	}
	
	
	
	
}
