/**
 * 
 */
package app.document.search.condition.impl;

import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import eim.bo.EIMObjectType;
import eim.bo.EIMSearchConditionGroup;
import eim.bo.EIMSearchSelectEIMObject;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.ObjectUtils;
import app.document.search.EIMDocSearchType;
import app.document.search.condition.EIMDocSearchConditionMaker;

/**
 * 属性ツリー一覧表示の検索条件定義
 *
 */
public class DspAttributeTreeConditionMaker extends EIMDocSearchConditionMaker {

	/**
	 * @param type
	 * @param userData
	 */
	public DspAttributeTreeConditionMaker(EIMDocSearchType type, Object userData) {
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
		
		// 検索条件１：オブジェクトタイプ「属性ツリービュー」
		EIMSearchConditionGroup conds = helper.group(helper.opAnd());
		EIMObjectType type = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREE"));
		conds.addCondition(helper.eq(helper.opAnd(), EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.TYPE, type.getId()));
		
		return conds;
	}

	/**
	 * @see app.document.search.condition.EIMDocSearchConditionMaker#parseUserData(java.lang.Object)
	 */
	@Override
	public void parseUserData(Object userData) throws Exception {
		// パラメータ無し

	}

}
