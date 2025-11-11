/**
 * properties_scr_Model
 */
var properties_scr_Model = function () {
	/*****************************************************************
	 * 宣言
	 ****************************************************************/
	function F() {};
	F.prototype = prototypeModel;
	var f = new F();
	
	// ビュー
	const viewName = "properties_scr_View";
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
	
	/**
	 * 未承認一覧情報を取得
	 * post_dspApproveDocumentList ()
	 */
	function post_dspApproveDocumentList () {
		// 共通モデルメソッドを呼び出し
		f.post_dspApproveDocumentList (view);
	}
	
	/*****************************************************************
	 * 関数定義
	 ****************************************************************/

	/**
	 * アトリビュート情報の取得
	 * post_dspProperty (pfolderList_scr_list_itemId)
	 */
	function post_dspProperty (pfolderList_scr_list_itemId) {
		// 共通モデルメソッドを呼び出し
		f.post_dspProperty (pfolderList_scr_list_itemId, view);
	}
		
	/**
	 * メールを送る
	 * post_actSendReplyMail (pfolderList_scr_list_itemId)
	 */
	function post_actSendReplyMail (pfolderList_scr_list_itemId) {
		// 共通モデルメソッドを呼び出し
		f.post_actSendReplyMail (pfolderList_scr_list_itemId, view);
	}
	
	/**
	 * アトリビュート情報の取得
	 * post_dspAttribute (pfolderList_scr_list_itemId, objectTypeId)
	 */
	function post_dspAttribute (pfolderList_scr_list_itemId, objTypeId) {
		// しばらくお待ち下さい
		f.show_wait();
		
		var url = CONST_URL_PATH + cmnJSPAPI['JSPAPI_00019'];

		var post_data = {objId : pfolderList_scr_list_itemId , objTypeId : objTypeId, }; 
		
		$.ajax({
			url : url, 
			type : 'POST',
			data : post_data, 
			dataType : 'xml',
			cache : false,
			success : function(xml){
				post_dspAttributeSuccessHandler(xml);
			},
			statusCode : { 
				// on('timeout')
				401: function(){ f.raiseSessionTimeoutError();},
				// on('not_found')
				404: function(){ f.raiseNotFoundError();},
				// on('internal_error')
				500: function(){ f.raiseInternalServerError();},
			},
			error : function(xhr, status, error){
				post_dspAttributeErrorHandler();
			},
		});
	}
	
	/*****************************************************************
	 * サーバ呼び出し結果
	 ****************************************************************/
	/**
	 * アトリビュート情報の取得
	 * 成功
	 */
	function post_dspAttributeSuccessHandler(xml) {
		f.postProcess();
		
		// エラーメッセージを確認
		var f_error_found = false;
		$(xml).find("error").each(function(){
			view.trigger('post_dspAttributeError', xml);
			f_error_found = true;
		});
		
		if (!f_error_found){
			view.trigger('post_dspAttributeSuccess', xml);
		};
	}
	
	/**
	 * アトリビュート情報の取得
	 * エラー
	 */
	function post_dspAttributeErrorHandler() {
		f.postProcess();
		f.showServerDown();
	}
	
	return {
		post_dspProperty : post_dspProperty,
		post_actSendReplyMail : post_actSendReplyMail,
		post_dspApproveDocumentList : post_dspApproveDocumentList,
		post_dspAttribute : post_dspAttribute,
	};
	
}();

