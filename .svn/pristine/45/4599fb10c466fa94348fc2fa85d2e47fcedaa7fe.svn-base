/**
 * favorites_scr_Model
 */
var favorites_scr_Model = function () {
	/*****************************************************************
	 * 宣言
	 ****************************************************************/
	function F() {};
	F.prototype = prototypeModel;
	var f = new F();
	
	// ビュー
	const viewName = "favorites_scr_View";
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
	 * お気に入り一覧情報の取得
	 * get_dspFavoriteList ()
	 */
	function get_dspFavoriteList () {
		// しばらくお待ち下さい
		f.show_wait();
		
		var url = CONST_URL_PATH + cmnJSPAPI['JSPAPI_00014'];

		$.ajax({
			url : url, 
			type : 'GET',
			dataType : 'xml',
			cache : false,
			contentType : 'application/x-www-form-urlencoded' ,
			success : function(xml){
				get_dspFavoriteListSuccessHandler(xml);
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
				get_dspFavoriteListErrorHandler();
			},
		});
	}
	
	/**
	 * 未承認一覧情報を取得
	 * post_dspApproveDocumentList ()
	 */
	function post_dspApproveDocumentList () {
		// 共通モデルメソッドを呼び出し
		f.post_dspApproveDocumentList (view);
	}
	
	/*****************************************************************
	 * サーバ呼び出し結果
	 ****************************************************************/
	
	/**
	 * お気に入り一覧情報の取得
	 * 成功
	 */
	function get_dspFavoriteListSuccessHandler(xml) {
		f.postProcess();
		
		// エラーメッセージを確認
		var f_error_found = false;
		$(xml).find("error").each(function(){
			var ferror_content = "";
			
			ferror_content = $(this).attr('message');
			// 特別な場合、下記のメッセージはフォルダ一覧に表示したので、要らない
			if (!(ferror_content.indexOf("削除されているものがあります") >= 0))
			{	
				f_error_found = true;
			};
		});
		
		if (!f_error_found){
			view.trigger('get_dspFavoriteListSuccess', xml);
		};
	}
	
	/**
	 * お気に入り一覧情報の取得
	 * エラー
	 */
	function get_dspFavoriteListErrorHandler(xml) {
		f.postProcess();
		f.showServerDown();
	}
	
	return {
		get_dspFavoriteList : get_dspFavoriteList,
		post_dspApproveDocumentList : post_dspApproveDocumentList,
	};
	
}();

