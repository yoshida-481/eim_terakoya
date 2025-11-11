/**
 * 
 */
package app.document.search.condition.impl;

import app.document.search.EIMDocSearchType;
import app.document.search.condition.EIMDocSearchConditionMaker;
import common.util.AppSearchUtils;
import eim.bo.EIMSearchConditionGroup;
import eim.bo.EIMSearchSelectEIMObject;
import eim.net.EIMSession;
import eim.util.TypeConvertUtils;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * お気に入り表示検索条件定義クラス
 *
 */
public class DspFavoriteItemConditionMaker extends EIMDocSearchConditionMaker {
	
	/** お気に入りとして登録されているオブジェクトのID */
	private long[] favoriteItems;

	/**
	 * @param type 検索種別
	 * @param userData マイドキュメントとして登録されているオブジェクトのID
	 */
	public DspFavoriteItemConditionMaker(EIMDocSearchType type, Object userData) {
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
		EIMSearchConditionGroup conds = helper.group(helper.opAnd());
		
		// 検索条件１：オブジェクトタイプ(=フォルダとそのサブクラス)
		conds.addCondition(AppSearchUtils.getMyFavoriteItemObjTypeCondition(sess));
		
		// 検索条件２：お気に入りとして登録されているオブジェクトのIDを持つ
		conds.addCondition(helper.in(helper.opAnd(), 
				EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.ID, 
				helper.opIn(), TypeConvertUtils.convertToBuildTypeArray(this.getFavoriteItems())));
		
		return conds;
	}

	/**
	 * @see app.document.search.condition.EIMDocSearchConditionMaker#parseUserData(java.lang.Object)
	 */
	@Override
	public void parseUserData(Object userData) throws Exception {
		this.setFavoriteItems((long[])userData);
	}

	/**
	 * @return the favoriteItems
	 */
	public long[] getFavoriteItems() {
		return favoriteItems;
	}

	/**
	 * @param favoriteItems the favoriteItems to set
	 */
	public void setFavoriteItems(long[] favoriteItems) {
		this.favoriteItems = favoriteItems;
	}
	
	

}
