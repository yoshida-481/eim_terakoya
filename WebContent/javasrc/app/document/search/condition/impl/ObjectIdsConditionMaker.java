/**
 * 
 */
package app.document.search.condition.impl;

import app.document.search.EIMDocSearchType;
import app.document.search.condition.EIMDocSearchConditionMaker;
import eim.bo.EIMSearchConditionGroup;
import eim.bo.EIMSearchSelectEIMObject;
import eim.util.TypeConvertUtils;

/**
 * 複数のオブジェクトID指定の検索条件定義クラス
 */
public class ObjectIdsConditionMaker extends EIMDocSearchConditionMaker {
	
	/** 対象オブジェクトIDの配列 */
	private long[] objectIds;

	/**
	 * @param type
	 * @param userData
	 */
	public ObjectIdsConditionMaker(EIMDocSearchType type, Object userData) {
		super(type, userData);
	}

	/**
	 * @see app.document.search.condition.EIMDocSearchConditionMaker#getDefaultCondition()
	 */
	@Override
	public EIMSearchConditionGroup getDefaultCondition() throws Exception {
		
		if (objectIds == null) {
			return null;
		}
		
		// 検索条件：オブジェクトIDがユーザデータで渡されたオブジェクトのIDの配列
		EIMSearchSelectEIMObject.SearchConditionBuildHelper helper = 
			new EIMSearchSelectEIMObject.SearchConditionBuildHelper();
		
		EIMSearchConditionGroup conds = helper.group(helper.opAnd());
		conds.addCondition(helper.in(helper.opAnd(), 
				EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.ID, helper.opIn(), TypeConvertUtils.convertToBuildTypeArray(this.getObjectIds())));
		
		return conds;
	}

	/**
	 * @see app.document.search.condition.EIMDocSearchConditionMaker#parseUserData(java.lang.Object)
	 */
	@Override
	public void parseUserData(Object userData) throws Exception {
		// 取得対象オブジェクトIDの配列をセット
		this.setObjectIds((long[])userData);
	}

	/**
	 * 対象オブジェクトIDの配列を取得します。
	 * @return 対象オブジェクトIDの配列
	 */
	public long[] getObjectIds() {
	    return objectIds;
	}

	/**
	 * 対象オブジェクトIDの配列を設定します。
	 * @param objectIds 対象オブジェクトIDの配列
	 */
	public void setObjectIds(long[] objectIds) {
	    this.objectIds = objectIds;
	}
	
}
