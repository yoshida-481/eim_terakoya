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
 * ドキュメントリレーション、もしくはリンクリレーションを取得する際の検索条件定義
 */
public class DocumentOrLinkRelationConditionMaker extends EIMDocSearchConditionMaker {

	/**
	 * @param type
	 * @param userData
	 */
	public DocumentOrLinkRelationConditionMaker(EIMDocSearchType type, Object userData) {
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
		EIMRelationType relTypeDoc = RelationUtils.getRelationTypeByName(sess, EIMConfig.getValue("RELATION_TYPE_NAME_DOCUMENT"));
		// 検索条件２：リレーションタイプが「リンク」
		EIMRelationType relTypeLink = RelationUtils.getRelationTypeByName(sess, EIMConfig.getValue("RELATION_TYPE_NAME_LINK"));
		
		// OR条件を作成
		EIMSearchConditionGroup conds = helper.group(helper.opAnd());
		conds.addCondition(helper.eq(helper.opOr(), EIMSearchSelectEIMRelation.PsedoAttributeTypeEnum.TYPE, relTypeDoc.getId()));
		conds.addCondition(helper.eq(helper.opOr(), EIMSearchSelectEIMRelation.PsedoAttributeTypeEnum.TYPE, relTypeLink.getId()));
		
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
