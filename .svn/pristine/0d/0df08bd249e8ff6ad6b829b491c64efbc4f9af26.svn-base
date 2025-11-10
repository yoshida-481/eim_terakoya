/**
 * main_scr_Model
 */
var main_scr_Model = function () {
	/*****************************************************************
	 * 宣言
	 ****************************************************************/
	function F() {};
	F.prototype = prototypeModel;
	var f = new F();
	
	// ビュー
	const viewName = "main_scr_View";
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
	 * 未承認文書情報の取得
	 * post_dspApproveDocumentList ()
	 */
	function post_dspApproveDocumentList () {
		// 共通モデルメソッドを呼び出し
		f.post_dspApproveDocumentListCount (view);
	}
	
	/**
	 * ログアウトの情報を取得
	 * get_actLogout ()
	 */
	function get_actLogout () {
		// しばらくお待ち下さい
		f.show_wait();
		
		var url = CONST_URL_PATH + cmnJSPAPI['JSPAPI_00006'];

		$.ajax({
			url : url, 
			dataType : 'xml',
			type : 'GET',
			cache : false,
			success : function(xml){
				get_actLogoutSuccessHandler(xml);
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
				get_actLogoutErrorHandler();
			},
		});
	}
	
	/*****************************************************************
	 * サーバ呼び出し結果
	 ****************************************************************/
	
	/**
	 * ログアウトの情報を取得
	 * 成功
	 */
	function get_actLogoutSuccessHandler(xml) {
		f.postProcess();
		
		// エラーメッセージを確認
		var f_error_found = false;
		$(xml).find("error").each(function(){
			view.trigger('get_actLogoutError', xml);
			f_error_found = true;
		});
		if (!f_error_found){
			view.trigger('get_actLogoutSuccess', xml);
		};
	}
	
	/**
	 * ログアウトの情報を取得
	 * エラー
	 */
	function get_actLogoutErrorHandler() {
		f.postProcess();
		f.showServerDown();
	}
	
	return {
		post_dspApproveDocumentList : post_dspApproveDocumentList,
		get_actLogout : get_actLogout,
	};
	
}();

