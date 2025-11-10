/**
 * prototypeModel
 */
var prototypeModel = function () {
	
	function test () {
		return null;
	}
	
	/***********************************************************************************
	 * [機能]	しばらくお待ちくださいメッセージを表示する
	 * [引数]	無し
	 * [戻値]	無し
	 ***********************************************************************************/
	function show_wait(){
		var flanguage = JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_CHECK_FORCE_WAIT_LANGUAGE));
		if (flanguage == null)
		{
				flanguage = "JA";
		}
		$.mobile.loading('show',{text:cmnLanguage['LAN_'+flanguage.substring(0,2)+'_00062']});
	}
	
	/**
	 * タイムアウトエラー
	 */
	function raiseSessionTimeoutError() {
		// Sorryページにリダイレクト
		window.location.href="../html/server_down.html";
	}
	
	/**
	 * 未検出４０４エラー
	 */
	function raiseNotFoundError() {
		// Sorryページにリダイレクト
		window.location.href="../html/server_down.html";
	}
	
	/**
	 * サーバ内部エラー５００エラー
	 */
	function raiseInternalServerError() {
		// Sorryページにリダイレクト
		window.location.href="../html/server_down.html";
	}
	
	/**
	 * サーバーダウンを表示
	 */
	function showServerDown() {
		// Sorryページにリダイレクト
		window.location.href="../html/server_down.html";
	}
	
	/**
	 * 後処理
	 */
	function postProcess() {
		$.mobile.loading('hide');
	}
	
	/**
	 * 言語一覧を取得する
	 * get_dspLanguageXML ()
	 */
	function get_dspLanguageXML (view) {
		// しばらくお待ち下さい
		//show_wait();
		
		var url = CONST_URL_PATH + cmnJSPAPI['JSPAPI_00001'];

		$.ajax({
			url : url, 
			dataType : 'xml',
			type : 'GET',
			cache : false,
			success : function(xml){
				get_dspLanguageXMLSuccessHandler(xml, view);
			},
			statusCode : { 
				// on('timeout')
				401: function(){ raiseSessionTimeoutError();},
				// on('not_found')
				404: function(){ raiseNotFoundError();},
				// on('internal_error')
				500: function(){ raiseInternalServerError();},
			},
			error : function(xhr, status, error){
				get_dspLanguageXMLErrorHandler(view);
			},
		});
	}
	
	
	/**
	 * ワークスペース情報の取得
	 * get_dspFolderTree (view)
	 */
	function get_dspFolderTree (view) {
		// しばらくお待ち下さい
		//show_wait();
		
		var url = CONST_URL_PATH + cmnJSPAPI['JSPAPI_00007'];
		
		$.ajax({
			url : url, 
			dataType : 'xml',
			type : 'GET',
			cache : false,
			success : function(xml){
				get_dspFolderTreeSuccessHandler(xml, view);
			},
			statusCode : { 
				// on('timeout')
				401: function(){ raiseSessionTimeoutError();},
				// on('not_found')
				404: function(){ raiseNotFoundError();},
				// on('internal_error')
				500: function(){ raiseInternalServerError();},
			},
			error : function(xhr, status, error){
				get_dspFolderTreeErrorHandler();
			},
		});
	}

	/**
	 * フォルダ／ドキュメント情報の取得
	 * post_dspChildObject (pfolderList_scr_workspace_infoId, view)
	 */
	function post_dspChildObject (pfolderList_scr_workspace_infoId, view) {
		// しばらくお待ち下さい
		//show_wait();
		
		var url = CONST_URL_PATH + cmnJSPAPI['JSPAPI_00009'];

		var post_data = {objId : pfolderList_scr_workspace_infoId ,}; 
		
		$.ajax({
			url : url, 
			type : 'POST',
			data : post_data, 
			dataType : 'xml',
			cache : false,
			success : function(xml){
				post_dspChildObjectSuccessHandler(xml, view);
			},
			statusCode : { 
				// on('timeout')
				401: function(){ raiseSessionTimeoutError();},
				// on('not_found')
				404: function(){ raiseNotFoundError();},
				// on('internal_error')
				500: function(){ raiseInternalServerError();},
			},
			error : function(xhr, status, error){
				post_dspChildObjectErrorHandler();
			},
		});
	}
	
	/**
	 * アトリビュート情報の取得
	 * post_dspProperty (pfolderList_scr_list_itemId, view)
	 */
	function post_dspProperty (pfolderList_scr_list_itemId, view) {
		// しばらくお待ち下さい
		//show_wait();
		
		var url = CONST_URL_PATH + cmnJSPAPI['JSPAPI_00012'];

		var post_data = {objId : pfolderList_scr_list_itemId ,}; 
		
		$.ajax({
			url : url, 
			type : 'POST',
			data : post_data, 
			dataType : 'xml',
			cache : false,
			success : function(xml){
				post_dspPropertySuccessHandler(xml, view);
			},
			statusCode : { 
				// on('timeout')
				401: function(){ raiseSessionTimeoutError();},
				// on('not_found')
				404: function(){ raiseNotFoundError();},
				// on('internal_error')
				500: function(){ raiseInternalServerError();},
			},
			error : function(xhr, status, error){
				post_dspPropertyErrorHandler();
			},
		});
	}
		
	/**
	 * メールを送る
	 * post_actSendReplyMail (pfolderList_scr_list_itemId, view)
	 */
	function post_actSendReplyMail (pfolderList_scr_list_itemId, view) {
		// 特別な場合、データーを送りますが、ＧＥＴのコールを使用する。
		var url = CONST_URL_PATH + cmnJSPAPI['JSPAPI_00013'] + pfolderList_scr_list_itemId;

		$.ajax({
			url : url, 
			type : 'POST',
			dataType : 'xml',
			cache : false,
			success : function(xml){
				post_actSendReplyMailSuccessHandler(xml, view);
			},
			statusCode : { 
				// on('timeout')
				401: function(){ raiseSessionTimeoutError();},
				// on('not_found')
				404: function(){ raiseNotFoundError();},
				// on('internal_error')
				500: function(){ raiseInternalServerError();},
			},
			error : function(xhr, status, error){
				post_actSendReplyMailErrorHandler();
			},
		});
	}
	
	/**
	 * 未承認文書情報の簡易取得
	 * post_dspApproveDocumentList (view)
	 */
	function post_dspApproveDocumentListCount (view) {
		// しばらくお待ち下さい
		//show_wait();
		
		var url = CONST_URL_PATH + cmnJSPAPI['JSPAPI_00005C'];

		var post_data = {objId : "",}; 

		$.ajax({
			url : url, 
			type : 'POST',
			data : post_data, 
			dataType : 'xml',
			cache : false,
			success : function(xml){
				post_dspApproveDocumentListSuccessHandler(xml, view);
			},
			statusCode : { 
				// on('timeout')
				401: function(){ raiseSessionTimeoutError();},
				// on('not_found')
				404: function(){ raiseNotFoundError();},
				// on('internal_error')
				500: function(){ raiseInternalServerError();},
			},
			error : function(xhr, status, error){
				post_dspApproveDocumentListErrorHandler();
			},
		});
	}
	
	/**
	 * 未承認文書情報の取得
	 * post_dspApproveDocumentList (view)
	 */
	function post_dspApproveDocumentList (view) {
		// しばらくお待ち下さい
		//show_wait();
		
		var url = CONST_URL_PATH + cmnJSPAPI['JSPAPI_00005'];

		var post_data = {objId : "",}; 

		$.ajax({
			url : url, 
			type : 'POST',
			data : post_data, 
			dataType : 'xml',
			cache : false,
			success : function(xml){
				post_dspApproveDocumentListSuccessHandler(xml, view);
			},
			statusCode : { 
				// on('timeout')
				401: function(){ raiseSessionTimeoutError();},
				// on('not_found')
				404: function(){ raiseNotFoundError();},
				// on('internal_error')
				500: function(){ raiseInternalServerError();},
			},
			error : function(xhr, status, error){
				post_dspApproveDocumentListErrorHandler();
			},
		});
	}
	
	/**
	 * 履歴情報の取得
	 * post_WorkFlowHistory (punapproved_scr_list_itemId, view)
	 */
	function post_WorkFlowHistory (punapproved_scr_list_itemId, view) {
		// しばらくお待ち下さい
		//show_wait();
		
		var url = CONST_URL_PATH + cmnJSPAPI['JSPAPI_00016'];

		var post_data = {
			objId : punapproved_scr_list_itemId,
		}; 
		
		$.ajax({
			url : url, 
			type : 'POST',
			dataType : 'xml',
			data : post_data, 
			cache : false,
			contentType : 'application/x-www-form-urlencoded' ,
			success : function(xml){
				post_WorkFlowHistorySuccessHandler(xml, view);
			},
			statusCode : { 
				// on('timeout')
				401: function(){ raiseSessionTimeoutError();},
				// on('not_found')
				404: function(){ raiseNotFoundError();},
				// on('internal_error')
				500: function(){ raiseInternalServerError();},
			},
			error : function(xhr, status, error){
				post_WorkFlowHistoryErrorHandler();
			},
		});
	}
	
	/*****************************************************************
	 * サーバ呼び出し結果
	 ****************************************************************/
	/**
	 * 言語一覧の取得
	 * 成功
	 */
	function get_dspLanguageXMLSuccessHandler(xml, view) {
		postProcess();
		view.trigger('get_dspLanguageXMLSuccess', xml);
	}
	
	/**
	 * 言語一覧の取得
	 * エラー
	 */
	function get_dspLanguageXMLErrorHandler(view) {
		postProcess();
		view.trigger('get_dspLanguageXMLError', null);
	}

	/**
	 * ワークスペース情報の取得
	 * 成功
	 */
	function get_dspFolderTreeSuccessHandler(xml, view) {
		postProcess();
		
		// エラーメッセージを確認
		var f_error_found = false;
		$(xml).find("error").each(function(){
			view.trigger('get_dspFolderTreeError', xml);
			f_error_found = true;
		});
		
		if (!f_error_found){
			view.trigger('get_dspFolderTreeSuccess', xml);
		};
	}
	
	/**
	 * ワークスペース情報の取得
	 * エラー
	 */
	function get_dspFolderTreeErrorHandler() {
		postProcess();
		showServerDown();
	}
	
	/**
	 * フォルダ／ドキュメント情報の取得
	 * 成功
	 */
	function post_dspChildObjectSuccessHandler(xml, view) {
		postProcess();
		
		// エラーメッセージを確認
		var f_error_found = false;
		$(xml).find("error").each(function(){
			view.trigger('post_dspChildObjectError', xml);
			f_error_found = true;
		});
		
		if (!f_error_found){
			view.trigger('post_dspChildObjectSuccess', xml);
		};
	}
	
	/**
	 * フォルダ／ドキュメント情報の取得
	 * エラー
	 */
	function post_dspChildObjectErrorHandler() {
		postProcess();
		showServerDown();
	}

	/**
	 * アトリビュート情報の取得
	 * 成功
	 */
	function post_dspPropertySuccessHandler(xml, view) {
		postProcess();
		
		// エラーメッセージを確認
		var f_error_found = false;
		$(xml).find("error").each(function(){
			view.trigger('post_dspPropertyError', xml);
			f_error_found = true;
		});
		
		if (!f_error_found){
			view.trigger('post_dspPropertySuccess', xml);
		};
	}
	
	/**
	 * アトリビュート情報の取得
	 * エラー
	 */
	function post_dspPropertyErrorHandler() {
		postProcess();
		showServerDown();
	}

	/**
	 * メールを送る
	 * 成功
	 */
	function post_actSendReplyMailSuccessHandler(xml, view) {
		postProcess();
		// エラーメッセージを確認
		var f_error_found = false;
		$(xml).find("error").each(function(){
			view.trigger('post_actSendReplyMailError', xml);
			f_error_found = true;
		});
		
		if (!f_error_found){
			view.trigger('post_actSendReplyMailSuccess', xml);
		};
	}
	
	/**
	 * メールを送る
	 * エラー
	 */
	function post_actSendReplyMailErrorHandler() {
		postProcess();
		showServerDown();
	}
	
	
	/**
	 * 未承認文書情報の取得
	 * 成功
	 */
	function post_dspApproveDocumentListSuccessHandler(xml, view) {
		postProcess();
		
		// エラーメッセージを確認
		var f_error_found = false;
		$(xml).find("error").each(function(){
			view.trigger('post_dspApproveDocumentListError', xml);
			f_error_found = true;
		});

		if (!f_error_found){
			view.trigger('post_dspApproveDocumentListSuccess', xml);
		};
	}
	
	/**
	 * 未承認文書情報の取得
	 * エラー
	 */
	function post_dspApproveDocumentListErrorHandler() {
		postProcess();
		showServerDown();
	}
	
	/**
	 * 履歴情報の取得
	 * 成功
	 */
	function post_WorkFlowHistorySuccessHandler(xml, view) {
		postProcess();
		
		// エラーメッセージを確認
		var f_error_found = false;
		$(xml).find("error").each(function(){
			view.trigger('post_WorkFlowHistoryError', xml);
			f_error_found = true;
		});
		
		if (!f_error_found){
			view.trigger('post_WorkFlowHistorySuccess', xml);
		};
	}
	
	/**
	 * 履歴情報の取得
	 * エラー
	 */
	function post_WorkFlowHistoryErrorHandler(xml) {
		postProcess();
		showServerDown();
	}
	
	return {
		postProcess : postProcess,
		raiseSessionTimeoutError : raiseSessionTimeoutError,
		raiseNotFoundError : raiseNotFoundError,
		raiseInternalServerError : raiseInternalServerError,
		get_dspLanguageXML : get_dspLanguageXML,
		get_dspFolderTree : get_dspFolderTree,
		post_dspChildObject : post_dspChildObject,
		post_dspProperty : post_dspProperty,
		showServerDown : showServerDown,
		post_actSendReplyMail : post_actSendReplyMail,
		post_dspApproveDocumentList: post_dspApproveDocumentList,
		post_dspApproveDocumentListCount: post_dspApproveDocumentListCount,
		post_WorkFlowHistory: post_WorkFlowHistory,
		show_wait: show_wait,
	};
	
}();

