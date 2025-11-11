/**
 * 
 */
package app.document.search.condition.impl;


import app.document.search.EIMDocSearchType;
import app.document.search.condition.EIMDocSearchConditionMaker;
import common.util.AppSearchUtils;
import eim.bo.EIMSearchConditionGroup;
import eim.bo.EIMSearchConditionIn;
import eim.bo.EIMSearchOperatorEnum;
import eim.bo.EIMSearchSelectEIMObject;
import eim.bo.EIMSearchSelectEIMObject.PsedoAttributeTypeEnum;
import eim.net.EIMSession;
import eim.util.TypeConvertUtils;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;


/**
 * オブジェクトに付与されたタグ一覧を取得する検索条件定義
 */
public class DspAddedTagListConditionMaker extends EIMDocSearchConditionMaker {
	
	/** タグのIDの配列 */
	private long[] object;

	/**
	 * @param type 検索種別
	 * @param userData ユーザデータ(選択されたタグ)
	 */
	public DspAddedTagListConditionMaker(EIMDocSearchType type, Object userData) {
		super(type, userData);
	}

	/**
	 * @see app.document.search.condition.EIMDocSearchConditionMaker#getDefaultCondition()
	 */
	@Override
	public EIMSearchConditionGroup getDefaultCondition() throws Exception {
		
		EIMSession sess = EIMThreadContext.getEIMSession();
		
		// 検索条件ヘルパー生成
		EIMSearchSelectEIMObject.SearchConditionBuildHelper h = new EIMSearchSelectEIMObject.SearchConditionBuildHelper();
		
		// 検索条件グループ作成
		EIMSearchConditionGroup conds = new EIMSearchConditionGroup(EIMSearchOperatorEnum.AND);
		
		// 検索条件1：オブジェクトタイプが「タグ」。そのサブクラスも含む。
		conds.addCondition(AppSearchUtils.getTreeViewItemTagObjTypeCondition(sess));
		
		// 検索条件2：IDが指定されたID配列のいずれか
		EIMSearchConditionIn idCond = h.in(h.opAnd(), PsedoAttributeTypeEnum.ID, h.opIn(), TypeConvertUtils.convertToBuildTypeArray(this.getObject()));
		conds.addCondition(idCond);
		
		return conds;
	}

	/**
	 * @see app.document.search.condition.EIMDocSearchConditionMaker#parseUserData(java.lang.Object)
	 */
	@Override
	public void parseUserData(Object userData) throws Exception {
		
		// 呼び出し元から渡されたパラメータを格納
		this.setObject((long[])userData);

	}

	/**
	 * @return the object
	 */
	public long[] getObject() {
		return object;
	}

	/**
	 * @param object the object to set
	 */
	public void setObject(long[] object) {
		this.object = object;
	}
	

}
