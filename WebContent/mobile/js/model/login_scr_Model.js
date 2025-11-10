/**
 * login_scr_Model
 */
var login_scr_Model = function () {
	/*****************************************************************
	 * 宣言
	 ****************************************************************/
	function F() {};
	F.prototype = prototypeModel;
	var f = new F();
	
	// ビュー
	const viewName = "login_scr_View";
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
	 * 言語一覧を取得する
	 * get_dspLanguageXML ()
	 */
	function get_dspLanguageXML () {
		// 共通モデルメソッドを呼び出し
		f.get_dspLanguageXML (view);
	}
	
	/**
	 * 言語の設定
	 * get_actCookie ()
	 */
	function get_actCookie () {
		// しばらくお待ち下さい
		f.show_wait();
		
		var url = CONST_URL_PATH + cmnJSPAPI['JSPAPI_00002'];

		$.ajax({
			url : url, 
			dataType : 'xml',
			type : 'GET',
			cache : false,
			success : function(xml){
				get_actCookieSuccessHandler(xml);
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
				get_actCookieErrorHandler();
			},
		});
	}

	/**
	 * ログイン確認
	 * post_login ()
	 */
	function post_login (pLogin_scr_ID, pLogin_scr_Password, pLogin_scr_Language) {
		// しばらくお待ち下さい
		window.localStorage.setItem(EIM_KEY_CURRENT_CHECK_FORCE_WAIT_LANGUAGE, JSON.stringify(pLogin_scr_Language));
		f.show_wait();

		// pLogin_scr_Languag = "JA@ja_JP"又は"EN@"
		var f_locale = pLogin_scr_Language.substring(pLogin_scr_Language.indexOf("@")+1);
		var f_Login_scr_Language = pLogin_scr_Language.substring(0,pLogin_scr_Language.indexOf("@"));
		
		var url = CONST_URL_PATH + cmnJSPAPI['JSPAPI_00003'];

		var post_data = {
			userCode : pLogin_scr_ID , 
			langId : f_Login_scr_Language , 
			userPass : pLogin_scr_Password , 
			userTzOffset : "-32400000" , 
			locale : f_locale,
		}; 
		
		$.ajax({
			url : url, 
			type : 'POST',
			data : post_data, 
			dataType : 'xml',
			cache : false,
			success : function(xml){
				post_loginSuccessHandler(xml);
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
				post_loginErrorHandler();
			},
		});
	}
	
	/**
	 * ログイン情報の取得
	 * get_dspSession ()
	 */
	function get_dspSession () {
		// しばらくお待ち下さい
		f.show_wait();
		
		var url = CONST_URL_PATH + cmnJSPAPI['JSPAPI_00004'];

		$.ajax({
			url : url, 
			dataType : 'xml',
			type : 'GET',
			cache : false,
			success : function(xml){
				get_dspSessionSuccessHandler(xml);
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
				get_dspSessionErrorHandler();
			},
		});
	}
	
	/*****************************************************************
	 * サーバ呼び出し結果
	 ****************************************************************/
	
	/**
	 * 言語情報の取得
	 * 成功
	 */
	function get_actCookieSuccessHandler(xml) {
		f.postProcess();
		
		// エラーメッセージを確認
		var f_error_found = false;
		$(xml).find("error").each(function(){
			view.trigger('get_actCookieError', xml);
			f_error_found = true;
		});
		
		if (!f_error_found){
			view.trigger('get_actCookieSuccess', xml);
		};
	}
	
	/**
	 * 言語情報の取得
	 * エラー
	 */
	function get_actCookieErrorHandler() {
		f.postProcess();
		f.showServerDown();
	}

	/**
	 * ログイン確認
	 * 成功
	 */
	function post_loginSuccessHandler(xml) {
		f.postProcess();
		
		// エラーメッセージを確認
		var f_error_found = false;
		$(xml).find("error").each(function(){
			view.trigger('post_loginError', xml);
			f_error_found = true;
		});
		
		if (!f_error_found){
			view.trigger('post_loginSuccess', xml);
		};
	}
	
	/**
	 * ログイン確認
	 * エラー
	 */
	function post_loginErrorHandler() {
		f.postProcess();
		f.showServerDown();
	}

	/**
	 * セッション情報の取得
	 * 成功
	 */
	function get_dspSessionSuccessHandler(xml) {
		f.postProcess();
		
		// エラーメッセージを確認
		var f_error_found = false;
		$(xml).find("error").each(function(){
			view.trigger('get_dspSessionsError', xml);
			f_error_found = true;
		});
		
		if (!f_error_found){
			view.trigger('get_dspSessionSuccess', xml);
		};
	}
	
	/**
	 * セッション情報の取得
	 * エラー
	 */
	function get_dspSessionErrorHandler() {
		f.postProcess();
		f.showServerDown();
	}
	
	return {
		get_dspLanguageXML : get_dspLanguageXML,
		get_actCookie : get_actCookie,
		post_login : post_login,
		get_dspSession : get_dspSession,
	};
	
}();

