package app.document.search.condition.impl;

import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import app.document.search.EIMDocSearchType;
import app.document.search.condition.EIMDocSearchConditionMaker;
import eim.bo.EIMObject;
import eim.bo.EIMRelationType;
import eim.bo.EIMSearchConditionGroup;
import eim.bo.EIMSearchSelectEIMRelation;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.RelationUtils;

public class ListViewRelationConditionMaker extends EIMDocSearchConditionMaker {

	/** 対象オブジェクト */
	private EIMObject object;

	/**
	 * @param type
	 * @param userData
	 */
	public ListViewRelationConditionMaker(EIMDocSearchType type, Object userData) {
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
		// 検索条件２：リレーションタイプが「ドキュメントリンク」
		EIMRelationType link = RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_LINK"));
		// 検索条件３：リレーションタイプが「ごみ箱」
		EIMRelationType recycle = RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_RECYCLE"));
		
		EIMSearchConditionGroup conds = helper.group(helper.opAnd());
		conds.addCondition(
				helper.eq(helper.opAnd(),
				EIMSearchSelectEIMRelation.PsedoAttributeTypeEnum.PARENT,
				object.getId())
		)
		.addCondition(helper.group(helper.opAnd())
				.addCondition(
						helper.eq(helper.opAnd(),
						EIMSearchSelectEIMRelation.PsedoAttributeTypeEnum.TYPE,
						type.getId())
				)
				.addCondition(
						helper.eq(helper.opOr(),
						EIMSearchSelectEIMRelation.PsedoAttributeTypeEnum.TYPE,
						link.getId())
				)
				.addCondition(
						helper.eq(helper.opOr(),
						EIMSearchSelectEIMRelation.PsedoAttributeTypeEnum.TYPE,
						recycle.getId())
				)
		);
		
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
