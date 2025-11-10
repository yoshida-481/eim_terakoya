/**
 * unapproved_scr_Model
 */
var unapproved_scr_Model = function () {
	/*****************************************************************
	 * 宣言
	 ****************************************************************/
	function F() {};
	F.prototype = prototypeModel;
	var f = new F();
	
	// ビュー
	const viewName = "unapproved_scr_View";
	const viewId = "#" + viewName;
	var view = null;
	
	/*****************************************************************
	 * 初期処理
	 ****************************************************************/
	jQuery(document).on('pageinit', viewId, function(ev) {
		
		/**
		 * 変数とセレクタの関連付け
		 */
		// ビュー
		view = $(viewId);
		
	});
	
	/*****************************************************************
	 * 関数定義
	 ****************************************************************/

	/**
	 * 未承認一覧情報を取得
	 * post_dspApproveDocumentList ()
	 */
	function post_dspApproveDocumentList () {
		// 共通モデルメソッドを呼び出し
		f.post_dspApproveDocumentList (view);
	}
	
	/**
	 * 履歴情報の取得
	 * post_WorkFlowHistory (punapproved_scr_list_itemId)
	 */
	function post_WorkFlowHistory (punapproved_scr_list_itemId) {
		// 共通モデルメソッドを呼び出し
		f.post_WorkFlowHistory (punapproved_scr_list_itemId, view);
	}
	
	/**
	 * メールを送る
	 * post_actSendReplyMail (punapproved_scr_list_itemId)
	 */
	function post_actSendReplyMail (punapproved_scr_list_itemId) {
		// 共通モデルメソッドを呼び出し
		f.post_actSendReplyMail (punapproved_scr_list_itemId, view);
	}
	
	/*****************************************************************
	 * サーバ呼び出し結果
	 ****************************************************************/
	
	return {
		post_dspApproveDocumentList : post_dspApproveDocumentList,
		post_WorkFlowHistory : post_WorkFlowHistory,
		post_actSendReplyMail : post_actSendReplyMail,
	};
	
}();

