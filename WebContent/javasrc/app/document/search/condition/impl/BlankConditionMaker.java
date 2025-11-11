/**
 * 
 */
package app.document.search.condition.impl;

import eim.bo.EIMSearchConditionGroup;
import app.document.search.EIMDocSearchType;
import app.document.search.condition.EIMDocSearchConditionMaker;

/**
 * @author z1J5161
 *
 */
public class BlankConditionMaker extends EIMDocSearchConditionMaker {

	/**
	 * @param type
	 * @param userData
	 */
	public BlankConditionMaker(EIMDocSearchType type, Object userData) {
		super(type, userData);
	}

	/**
	 * @see app.document.search.condition.EIMDocSearchConditionMaker#getDefaultCondition()
	 */
	@Override
	public EIMSearchConditionGroup getDefaultCondition() throws Exception {
		// 何もせず、NULLを返す。
		return null;
	}

	/**
	 * @see app.document.search.condition.EIMDocSearchConditionMaker#parseUserData(java.lang.Object)
	 */
	@Override
	public void parseUserData(Object userData) throws Exception {
	}

}
