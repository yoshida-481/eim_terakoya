package app.document.search;

/**
 * 標準品の検索種別を操作別に纏めた列挙体
 *
 */
public enum EIMDocSearchType {
	
	// ワークスペース一覧表示
	DISPLAY_WORKSPACE,
	// ツリー一覧表示
	DISPLAY_TREEVIEW,
	// ツリービューにて子フォルダ表示
	DISPLAY_CHILDFOLDER,
	// ツリービューにて子タグ表示
	DISPLAY_CHILDTAG,
	// ツリービューにて全ての子オブジェクト取得
	DISPLAY_ALL_CHILD_OBJECTS,
	// リスト一覧表示
	DISPLAY_LISTVIEW,
	// 属性ツリービュー一覧表示
	DISPLAY_ATTRTREE,
	// 属性ツリービューのリスト表示
	DISPLAY_ATTRTREE_LIST,
	// 属性ツリービューのオブジェクト表示
	DISPLAY_ATTRTREE_OBJLIST,
	// 検索
	SEARCH_OBJECT,
	// 承認
	DISPLAY_APPROVEITEM,
	// お気に入り一覧表示
	DISPLAY_FAVORITEITEM,
	// マイドキュメント表示
	DISPLAY_MYDOCUMENT,
	// チェックアウト一覧表示
	DISPLAY_CHECKOUTITEM,
	// タグに割当てられたオブジェクト一覧表示
	DIPSLAY_TAGLIST,
	// オブジェクトに付与されたタグ一覧表示
	DISPLAY_ADDED_TAGLIST,
	// 定型ドキュメント取得
	DISPLAY_OBJECTTYPE,
	// リストビューにて表示可能か親を取得して確認
	SEARCH_PARENT_FOR_LISTVIEW,
	// ドキュメントリンクの親オブジェクト検索
	SEARCH_LINK_PARENT,
	// 親オブジェクト配下の子オブジェクト全て(リンクも含む)
	SEARCH_ALL_CHILD,
	// 親オブジェクト配下の子オブジェクト全て(リンクを含まない)
	SEARCH_CHILD,
	// IDのよるオブジェクトの取得
	SEARCH_OBJECT_BY_ID,
	// Asperaドキュメント表示
	DISPLAY_ASPERADOCUMENT
}
