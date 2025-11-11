package app.document.search.condition;

import app.document.search.EIMDocSearchType;
import app.document.search.condition.impl.DspAddedTagListConditionMaker;
import app.document.search.condition.impl.DspApproveItemConditionMaker;
import app.document.search.condition.impl.DspAttrObjTreeConditionMaker;
import app.document.search.condition.impl.DspAttributeTreeConditionMaker;
import app.document.search.condition.impl.DspCheckoutListConditionMaker;
import app.document.search.condition.impl.DspFavoriteItemConditionMaker;
import app.document.search.condition.impl.DspFixedFormConditionMaker;
import app.document.search.condition.impl.DspParentObjectConditionMaker;
import app.document.search.condition.impl.DspTagTreeConditionMaker;
import app.document.search.condition.impl.DspWorkspaceConditionMaker;
import app.document.search.condition.impl.SearchLinkParentObjectConditionMaker;


/**
 * 検索条件の定義を行うクラスを提供するファクトリクラス
 *
 */
public class EIMDocObjSearchConditionFactory {
	
	
	/**
	 * 検索条件定義クラスを取得
	 * @param type 検索の操作種別
	 * @param userData 検索条件定義時の入力で使用する任意のデータ
	 * @return 検索種別に応じた検索条件定義クラスを返す。未定義の操作種別であった場合はNULLを返す
	 */
	static public EIMDocSearchConditionMaker getConditionMaker(EIMDocSearchType type, Object userData)
	{
		EIMDocSearchConditionMaker condMaker = null;
		
		if (type == EIMDocSearchType.DISPLAY_WORKSPACE) {
			condMaker = new DspWorkspaceConditionMaker(type, userData);
		} else if (type == EIMDocSearchType.DISPLAY_APPROVEITEM) {
			condMaker = new DspApproveItemConditionMaker(type, userData);
		} else if (type == EIMDocSearchType.DIPSLAY_TAGLIST) {
			condMaker = new DspTagTreeConditionMaker(type, userData);
		} else if (type == EIMDocSearchType.DISPLAY_OBJECTTYPE) {
			condMaker = new DspFixedFormConditionMaker(type, userData);
		} else if (type == EIMDocSearchType.DISPLAY_ATTRTREE) {
			condMaker = new DspAttributeTreeConditionMaker(type, userData);
		} else if (type == EIMDocSearchType.DISPLAY_ATTRTREE_OBJLIST) {
			condMaker = new DspAttrObjTreeConditionMaker(type, userData);
		} else if (type == EIMDocSearchType.DISPLAY_FAVORITEITEM) {
			condMaker = new DspFavoriteItemConditionMaker(type, userData);
		} else if (type == EIMDocSearchType.DISPLAY_CHECKOUTITEM) {
			condMaker = new DspCheckoutListConditionMaker(type, userData);
		} else if (type == EIMDocSearchType.DISPLAY_ADDED_TAGLIST) {
			condMaker = new DspAddedTagListConditionMaker(type, userData);
		} else if (type == EIMDocSearchType.SEARCH_LINK_PARENT) {
			condMaker = new SearchLinkParentObjectConditionMaker(type, userData);
		} else if (type == EIMDocSearchType.SEARCH_OBJECT_BY_ID) {
			condMaker = new DspParentObjectConditionMaker(type, userData);
		}
		
		
		return condMaker;
	}

}
