/**
 * 
 */
package app.document.search.condition.impl;

import app.document.search.EIMDocSearchType;
import app.document.search.condition.EIMDocSearchConditionMaker;
import eim.bo.EIMRelationType;
import eim.bo.EIMSearchConditionGroup;
import eim.bo.EIMSearchSelectEIMRelation;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.RelationUtils;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * ドキュメントリレーションを取得する際の検索条件定義
 */
public class DocumentRecycleRelationConditionMaker extends EIMDocSearchConditionMaker {

	/**
	 * @param type
	 * @param userData
	 */
	public DocumentRecycleRelationConditionMaker(EIMDocSearchType type, Object userData) {
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
		
		// 検索条件１：リレーションタイプが「ドキュメント」
		EIMRelationType type = RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));
		// 検索条件２：リレーションタイプが「ごみ箱」
		EIMRelationType recycle = RelationUtils.getRelationTypeByName(sess, EIMConfig.getValue("RELATION_TYPE_NAME_RECYCLE"));
		
		// OR条件
		EIMSearchConditionGroup conds = helper.group(helper.opAnd());
		conds.addCondition(helper.eq(helper.opOr(), EIMSearchSelectEIMRelation.PsedoAttributeTypeEnum.TYPE, type.getId()));
		conds.addCondition(helper.eq(helper.opOr(), EIMSearchSelectEIMRelation.PsedoAttributeTypeEnum.TYPE, recycle.getId()));
		
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
