package app.document.search.condition;

import app.document.search.EIMDocSearchType;
import app.document.search.condition.impl.BlankConditionMaker;
import app.document.search.condition.impl.DocAndLinkRelationConditionMaker;
import app.document.search.condition.impl.DocumentOrLinkRelationConditionMaker;
import app.document.search.condition.impl.DocumentRecycleRelationConditionMaker;
import app.document.search.condition.impl.DocumentRelationConditionMaker;
import app.document.search.condition.impl.DspAttrValueTreeConditionMaker;
import app.document.search.condition.impl.ListViewRelationConditionMaker;
import app.document.search.condition.impl.ObjTypeFolderOrWsRecycleConditionMaker;
import app.document.search.condition.impl.ObjTypeTagConditionMaker;
import app.document.search.condition.impl.ObjectConditionMaker;
import app.document.search.condition.impl.ObjectIdsConditionMaker;


/**
 * 標準品リレーション検索条件定義を行うクラスを提供するファクトリ
 *
 */
public class EIMDocRelSearchConditionFactory {
	
	/** 検索条件：リレーション */
	static private final int RELATION = 1;
	/** 検索条件：親オブジェクト */
	static private final int PARENT = 2;
	/** 検索条件：子オブジェクト */
	static private final int CHILD = 3;
	
	/**
	 * リレーション検索条件定義クラスを取得
	 * @param key 検索対象リレーションタイプのキー
	 * @return 検索条件、ファクトリ内で定義されていなければNULLが返る
	 */
	static public EIMDocSearchConditionMaker getRelConditionMaker(EIMDocSearchType type, Object userData)
	{
		return getConditionMaker(type, RELATION, userData);
	}
	
	/**
	 * 親オブジェクト検索条件定義クラスを取得
	 * @param type
	 * @return
	 */
	static public EIMDocSearchConditionMaker getParentConditionMaker(EIMDocSearchType type, Object userData)
	{
		return getConditionMaker(type, PARENT, userData);
	}
	
	/**
	 * 子オブジェクト検索条件定義クラスを取得
	 * @param type
	 * @return
	 */
	static public EIMDocSearchConditionMaker getChildConditionMaker(EIMDocSearchType type, Object userData)
	{
		return getConditionMaker(type, CHILD, userData);
	}
	
	/**
	 * リレーション検索条件定義クラスを取得
	 * @param type
	 * @param destType
	 * @return
	 */
	static private EIMDocSearchConditionMaker getConditionMaker(EIMDocSearchType type, int destType, Object userData)
	{
		EIMDocSearchConditionMaker condMaker = null;
		
		if (type == EIMDocSearchType.DISPLAY_LISTVIEW) {
			
			// ツリービュー、リストビューのフォルダ表示
			if (destType == RELATION) {
				condMaker = new ListViewRelationConditionMaker(type, userData);
			} else if (destType == PARENT) {
				condMaker = new BlankConditionMaker(type, userData);
			} else if (destType == CHILD) {
				condMaker = new BlankConditionMaker(type, userData);
			}

		} else if (type == EIMDocSearchType.DISPLAY_MYDOCUMENT) {
			
			// マイドキュメント表示
			if (destType == RELATION) {
				condMaker = new DocumentRelationConditionMaker(type, userData);
			} else if (destType == PARENT) {
				condMaker = new ObjectConditionMaker(type, userData);
			} else if (destType == CHILD) {
				condMaker = new BlankConditionMaker(type, userData);
			}
		} else if (type == EIMDocSearchType.SEARCH_PARENT_FOR_LISTVIEW) {
			
			// リストビュー表示時に親オブジェクトが取得可能かで自身が表示可能かチェック
			if (destType == RELATION) {
				condMaker = new DocAndLinkRelationConditionMaker(type, userData);
			} else if (destType == PARENT) {
				condMaker = new BlankConditionMaker(type, userData);
			} else if (destType == CHILD) {
				condMaker = new ObjectConditionMaker(type, userData);
			}
		} else if (type == EIMDocSearchType.DISPLAY_CHILDFOLDER) {
			
			// ツリービューにて子フォルダの一覧表示
			if (destType == RELATION) {
				condMaker = new DocumentRecycleRelationConditionMaker(type, userData);
			} else if (destType == PARENT) {
				condMaker = new ObjectConditionMaker(type, userData);
			} else if (destType == CHILD) {
				condMaker = new ObjTypeFolderOrWsRecycleConditionMaker(type, userData);
			}
		} else if (type == EIMDocSearchType.DISPLAY_ALL_CHILD_OBJECTS) {
			
			// フォルダツリーにてすべての子オブジェクトを取得する
			if (destType == RELATION) {
				condMaker = new DocumentRecycleRelationConditionMaker(type, userData);
			} else if (destType == PARENT) {
				condMaker = new ObjectIdsConditionMaker(type, userData);
			} else if (destType == CHILD) {
				condMaker = new BlankConditionMaker(type, userData);
			}
		} else if (type == EIMDocSearchType.DISPLAY_CHILDTAG) {
			
			// ツリービューにて配下のタグ一覧表示
			if (destType == RELATION) {
				condMaker = new DocumentRelationConditionMaker(type, userData);
			} else if (destType == PARENT) {
				condMaker = new ObjectConditionMaker(type, userData);
			} else if (destType == CHILD) {
				condMaker = new ObjTypeTagConditionMaker(type, userData);
			}
		} else if (type == EIMDocSearchType.DISPLAY_ATTRTREE_LIST) {
			
			// 属性ツリービューにて配下の属性値一覧取得
			if (destType == RELATION) {
				condMaker = new DocumentOrLinkRelationConditionMaker(type, userData);
			} else if (destType == PARENT) {
				condMaker = new BlankConditionMaker(type, userData);
			} else if (destType == CHILD) {
				condMaker = new DspAttrValueTreeConditionMaker(type, userData);
			}
		} else if (type == EIMDocSearchType.SEARCH_ALL_CHILD) {
			
			// すべての子オブジェクトを取得する(リンク含む)
			if (destType == RELATION) {
				condMaker = new DocumentOrLinkRelationConditionMaker(type, userData);
			} else if (destType == PARENT) {
				condMaker = new ObjectConditionMaker(type, userData);
			} else if (destType == CHILD) {
				condMaker = new BlankConditionMaker(type, userData);
			}
		} else if (type == EIMDocSearchType.SEARCH_CHILD) {
			
			// すべての子オブジェクトを取得する(リンク含まない)
			if (destType == RELATION) {
				condMaker = new DocumentRelationConditionMaker(type, userData);
			} else if (destType == PARENT) {
				condMaker = new ObjectConditionMaker(type, userData);
			} else if (destType == CHILD) {
				condMaker = new BlankConditionMaker(type, userData);
			}
		}else if (type == EIMDocSearchType.DISPLAY_ASPERADOCUMENT) {
			
			// Asperaドキュメント表示
			if (destType == RELATION) {
				condMaker = new DocumentRelationConditionMaker(type, userData);
			} else if (destType == PARENT) {
				condMaker = new ObjectConditionMaker(type, userData);
			} else if (destType == CHILD) {
				condMaker = new BlankConditionMaker(type, userData);
			}
		}

		return condMaker;
	}

}
