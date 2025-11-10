/**
 * folderList_scr_Model
 */
var folderList_scr_Model = function () {
	/*****************************************************************
	 * 宣言
	 ****************************************************************/
	function F() {};
	F.prototype = prototypeModel;
	var f = new F();
	
	// ビュー
	const viewName = "folderList_scr_View";
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
	 * ワークスペース情報の取得
	 * get_dspFolderTree ()
	 */
	function get_dspFolderTree () {
		// 共通モデルメソッドを呼び出し
		f.get_dspFolderTree (view);
	}
	
	/**
	 * アトリビュート情報の取得
	 * post_dspProperty (pfolderList_scr_list_itemId)
	 */
	function post_dspProperty (pfolderList_scr_list_itemId) {
		// 共通モデルメソッドを呼び出し
		f.post_dspProperty (pfolderList_scr_list_itemId, view);
	}
	
	/**
	 * フォルダ／ドキュメント情報の取得
	 * post_dspChildObject (pfolderList_scr_workspace_infoId)
	 */
	function post_dspChildObject (pfolderList_scr_workspace_infoId) {
		// 共通モデルメソッドを呼び出し
		f.post_dspChildObject (pfolderList_scr_workspace_infoId, view);
	}
	
	/**
	 * キーワード情報の取得
	 * post_actSearch (pfolderList_scr_search_keyword, psearch_path)
	 */
	function post_actSearch (pfolderList_scr_search_keyword, psearch_path) {
		var fpathCondition = 'true';
		var fsearchpath = psearch_path;
		
		// しばらくお待ち下さい
		f.show_wait();
		
		if (fsearchpath=="/")
		{
			fsearchpath = "";
			fpathCondition = 'false';
		}
		
		var url = CONST_URL_PATH + cmnJSPAPI['JSPAPI_00008'];

		var post_data = {
				searchType : 'document', 
				pathCondition : fpathCondition, 
				history : false, 
				modifyDateTo : '', 
				modifyUserName : '', 
				property : '', 
				keyword : pfolderList_scr_search_keyword , 
				modifyDateFrom : '', 
				contents: 'false', 
				tableId : 'default', 
				status: 'all', 
				emptyFolder : 'false', 
				searchPath : fsearchpath,
		}; 
		
		$.ajax({
			url : url, 
			type : 'POST',
			data : post_data, 
			dataType : 'xml',
			cache : false,
			contentType : 'application/x-www-form-urlencoded' ,
			success : function(xml){
				post_actSearchSuccessHandler(xml);
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
				post_actSearchErrorHandler();
			},
		});
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
	 * お気に入りフォルダＩＤのフォルダツリー情報の取得
	 * post_dspFolderTreeForTarget (pfolderList_scr_search_keyword)
	 */
	function post_dspFolderTreeForTarget (pfolderList_scr_list_itemId) {
		// しばらくお待ち下さい
		f.show_wait();
		
		var url = CONST_URL_PATH + cmnJSPAPI['JSPAPI_00015'];

		var post_data = {
				tagObjectsIds : '', 
				ObjectId : pfolderList_scr_list_itemId, 
		}; 
		
		$.ajax({
			url : url, 
			type : 'POST',
			data : post_data, 
			dataType : 'xml',
			cache : false,
			contentType : 'application/x-www-form-urlencoded' ,
			success : function(xml){
				post_dspFolderTreeForTargetSuccessHandler(xml);
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
				post_dspFolderTreeForTargetErrorHandler();
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
	 * キーワード情報の取得
	 * 成功
	 */
	function post_actSearchSuccessHandler(xml) {
		f.postProcess();
		
		// エラーメッセージを確認
		var f_error_found = false;
		$(xml).find("error").each(function(){
			view.trigger('post_actSearchError', xml);
			f_error_found = true;
		});
		
		if (!f_error_found){
			view.trigger('post_actSearchSuccess', xml);
		};
	}
	
	/**
	 * キーワード情報の取得
	 * エラー
	 */
	function post_actSearchErrorHandler(xml) {
		f.postProcess();
		f.showServerDown;
	}

	/**
	 * お気に入りフォルダＩＤのフォルダツリー情報の取得
	 * 成功
	 */
	function post_dspFolderTreeForTargetSuccessHandler(xml) {
		f.postProcess();
		
		// エラーメッセージを確認
		var f_error_found = false;
		$(xml).find("error").each(function(){
			view.trigger('post_dspFolderTreeForTargetError', xml);
			f_error_found = true;
		});
		
		if (!f_error_found){
			view.trigger('post_dspFolderTreeForTargetSuccess', xml);
		};
	}
	
	/**
	 * お気に入りフォルダＩＤのフォルダツリー情報の取得
	 * エラー
	 */
	function post_dspFolderTreeForTargetErrorHandler(xml) {
		f.postProcess();
		f.showServerDown;
	}
	
	return {
		get_dspFolderTree : get_dspFolderTree,
		post_dspChildObject : post_dspChildObject,
		post_dspProperty : post_dspProperty,
		post_actSearch : post_actSearch,
		post_actSendReplyMail : post_actSendReplyMail,
		post_dspFolderTreeForTarget: post_dspFolderTreeForTarget,
		post_dspApproveDocumentList : post_dspApproveDocumentList,
	};
	
}();

