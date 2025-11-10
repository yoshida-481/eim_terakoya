package app.document.search.condition.impl;

import app.document.search.EIMDocSearchType;
import app.document.search.condition.EIMDocSearchConditionMaker;
import common.util.AppSearchUtils;
import eim.bo.EIMSearchConditionGroup;
import eim.bo.EIMSearchOperatorEnum;
import eim.bo.EIMSearchSelectEIMObject;
import eim.net.EIMSession;
import eim.util.TypeConvertUtils;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

public class SearchLinkParentObjectConditionMaker extends EIMDocSearchConditionMaker {
	
	private long[] parents;
	
	/**
	 * @param type 検索種別
	 * @param userData 承認対象オブジェクト一覧
	 */
	public SearchLinkParentObjectConditionMaker(EIMDocSearchType type, Object userData) {
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
		
		// リンクリレーションの親オブジェクトを検索する
		EIMSearchConditionGroup conds = helper.group(helper.opAnd());
		
		// 検索条件1：オブジェクトタイプが「フォルダ」。そのサブクラスも対象。
		conds.addCondition(AppSearchUtils.getSearchListViewItemFolderObjTypeCondition(sess));

		// 検索条件2：オブジェクトIDが指定のもの
		conds.addCondition(
			helper.in(helper.opAnd(),
			EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.ID,
			EIMSearchOperatorEnum.IN,
			TypeConvertUtils.convertToBuildTypeArray(parents))
		);
		
		return conds;
	}

	/**
	 * @see app.document.search.condition.EIMDocSearchConditionMaker#parseUserData(java.lang.Object)
	 */
	@Override
	public void parseUserData(Object userData) throws Exception {
		
		this.parents = (long[])userData;
	}
	
}