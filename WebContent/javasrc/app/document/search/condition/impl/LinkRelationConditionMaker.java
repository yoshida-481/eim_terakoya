/**
 * 
 */
package app.document.search.condition.impl;

import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import eim.bo.EIMRelationType;
import eim.bo.EIMSearchConditionGroup;
import eim.bo.EIMSearchSelectEIMRelation;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.RelationUtils;
import app.document.search.EIMDocSearchType;
import app.document.search.condition.EIMDocSearchConditionMaker;

/**
 * @author z1J5161
 *
 */
public class LinkRelationConditionMaker extends EIMDocSearchConditionMaker {

	/**
	 * @param type
	 * @param userData
	 */
	public LinkRelationConditionMaker(EIMDocSearchType type, Object userData) {
		super(type, userData);
	}

	/**
	 * @see app.document.search.condition.EIMDocSearchConditionMaker#getDefaultCondition()
	 */
	@Override
	public EIMSearchConditionGroup getDefaultCondition() throws Exception {
		
		EIMSearchSelectEIMRelation.SearchConditionBuildHelper helper =
			new EIMSearchSelectEIMRelation.SearchConditionBuildHelper();
		EIMSession sess = EIMThreadContext.getEIMSession();
		
		// 検索条件１：リレーションタイプが「リンク」
		EIMRelationType type = RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_LINK"));
		
		EIMSearchConditionGroup conds = helper.group(helper.opAnd());
		conds.addCondition(helper.eq(helper.opAnd(), EIMSearchSelectEIMRelation.PsedoAttributeTypeEnum.TYPE, type.getId()));
		
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
