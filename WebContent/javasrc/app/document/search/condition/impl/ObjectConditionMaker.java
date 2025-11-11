/**
 * 
 */
package app.document.search.condition.impl;

import eim.bo.EIMObject;
import eim.bo.EIMSearchConditionGroup;
import eim.bo.EIMSearchSelectEIMObject;
import app.document.search.EIMDocSearchType;
import app.document.search.condition.EIMDocSearchConditionMaker;

/**
 * オブジェクトID指定の検索条件定義クラス
 *
 */
public class ObjectConditionMaker extends EIMDocSearchConditionMaker {
	
	/** 対象オブジェクト */
	private EIMObject object;

	/**
	 * @param type
	 * @param userData
	 */
	public ObjectConditionMaker(EIMDocSearchType type, Object userData) {
		super(type, userData);
	}

	/**
	 * @see app.document.search.condition.EIMDocSearchConditionMaker#getDefaultCondition()
	 */
	@Override
	public EIMSearchConditionGroup getDefaultCondition() throws Exception {
		
		if (object == null) {
			return null;
		}
		
		// 検索条件：オブジェクトIDがユーザデータで渡されたオブジェクトのID
		EIMSearchSelectEIMObject.SearchConditionBuildHelper helper = 
			new EIMSearchSelectEIMObject.SearchConditionBuildHelper();
		
		EIMSearchConditionGroup conds = helper.group(helper.opAnd());
		conds.addCondition(helper.eq(helper.opAnd(), 
				EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.ID, this.getObject().getId()));
		
		return conds;
	}

	/**
	 * @see app.document.search.condition.EIMDocSearchConditionMaker#parseUserData(java.lang.Object)
	 */
	@Override
	public void parseUserData(Object userData) throws Exception {
		// 取得対象EIMオブジェクトをセット
		this.setObject((EIMObject)userData);
	}

	/**
	 * @return the object
	 */
	public EIMObject getObject() {
		return object;
	}

	/**
	 * @param object the object to set
	 */
	public void setObject(EIMObject object) {
		this.object = object;
	}
	
}
