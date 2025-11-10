package app.document.search.condition.impl;

import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import common.util.AppSearchUtils;

import eim.bo.EIMObject;
import eim.bo.EIMSearchConditionGroup;
import eim.bo.EIMSearchOperatorEnum;
import eim.bo.EIMSearchSelectEIMObject;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import app.document.search.EIMDocSearchType;
import app.document.search.condition.EIMDocSearchConditionMaker;

public class DspParentObjectConditionMaker extends EIMDocSearchConditionMaker {
	
	/** 親オブジェクト */
	private EIMObject object;
	
	/**
	 * コンストラクタ
	 * @param type 検索種別
	 * @param userData
	 */
	public DspParentObjectConditionMaker(EIMDocSearchType type, Object userData) {
		super(type, userData);
	}
	
	/**
	 * @see app.document.search.condition.EIMDocSearchConditionMaker#parseUserData(Object)
	 */
	@Override
	public void parseUserData(Object userData) throws Exception{
		this.object = (EIMObject)userData;
	}
	
	@Override
	public EIMSearchConditionGroup getDefaultCondition() throws Exception{
		
		EIMSearchSelectEIMObject.SearchConditionBuildHelper helper = 
			new EIMSearchSelectEIMObject.SearchConditionBuildHelper();
		EIMSession sess = EIMThreadContext.getEIMSession();
		
		// 検索条件グループ作成
		EIMSearchConditionGroup conds = helper.group(helper.opAnd());
		
		// 検索条件1：オブジェクトタイプが「ワークスペース」か「ドキュメント」「フォルダ」「タグ」
		if(object.getType().getDefName().equals(EIMConfig.getValue("OBJECT_TYPE_NAME_WORKSPACE")))
		{
			// オブジェクトタイプが「ワークスペース」
			conds.addCondition(helper.eq(helper.opAnd(),
					EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.TYPE,
					object.getType().getId()));
		}
		else
		{
			// オブジェクトタイプが「ドキュメント」「フォルダ」「タグ」。そのサブクラスも対象。
			conds.addCondition(AppSearchUtils.getListViewItemObjTypeCondition(sess));
		}
		
		// 検索条件2：オブジェクトIDが指定のもの
		conds.addCondition(
				helper.compare(helper.opAnd(),
						EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.ID,
						EIMSearchOperatorEnum.EQ,
						object.getId())
		);
		
		return conds;
	}
}