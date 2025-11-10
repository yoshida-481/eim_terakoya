/**
 * favorites_scr_Controller
 */
var favorites_scr_Controller = function () {
	/*****************************************************************
	 * 宣言
	 ****************************************************************/
	function F() {};
	F.prototype = prototypeController;
	var f = new F();
	
	// ビュー
	const viewName = "favorites_scr_View";
	const viewId = "#" + viewName;
	var view = null;
	// モデル
	const model = favorites_scr_Model;
	
	// コンポーネント
	var pfavorites_scr_fileInfo = null;
	
	// 変数
	var pCurrentFolder = new Object();
	var planguage = null;
	var pfolderList_scr_workspace_infoId = "";
	
	
	/*****************************************************************
	 * 初期処理
	 ****************************************************************/
	jQuery(document).on('pageinit', viewId, function(ev) {
		
		/**
		 * 変数とセレクタの関連付け
		 */
		// ビュー
		view = $(viewId);
		
		// ＪＱＵＥＲＹのコンポーネント、ボタン変数
		pfavorites_scr_fileInfo = $(viewId+'-'+'favorites_scr_fileInfo');
		
		// ボタン
		
		/**
		 * initialize
		 */
		initialize();
		
		
		/*****************************************************************
		 * イベントハンドラ
		 ****************************************************************/
		/**
		 * 「orientationChange_Portrait」イベントハンドラ
		 * 縦向きになった場合
		 * このイベントハンドラは全ての画面の回転イベントで呼び出されてしまう。
		 * 現在ページの判定処理を入れて対応する
		 */
		$(this).on('orientationChange_Portrait', function(e) {
			resize_screen_portrait();
		});
		
		
		/**
		 * 「orientationChange_Landscape」イベントハンドラ
		 * 横向きになった場合
		 * このイベントハンドラは全ての画面の回転イベントで呼び出されてしまう。
		 * 現在ページの判定処理を入れて対応する
		 */
		$(this).on('orientationChange_Landscape', function(e) {
			resize_screen_landscape();
		});

		
		/**
		 * お気に入りボタンクリックイベントハンドラ（情報をクリック）
		 */
		jQuery(ev.target).on('click', $('.favorites_src_folder_info').selector, function() {
			//フォルダ／ドキュメント情報の取得
			pfolderList_scr_workspace_infoId = (this.id).substring(26);
			// ヘッダーメニューバックスを閉じる
			$('#header_menuBox').hide();
			// フォルダ一覧コンとろらーにフォルダＩＤを設定
			folderList_scr_Controller.setParameter(pfolderList_scr_workspace_infoId);
			// フォルダ一覧画面の遷移
			$.mobile.changePage(folderList_scr_Controller.getViewId(), {'transition' : 'fade', });
		});
		
		/**
		 * お気に入りボタンクリックイベントハンドラ（アイコンをクリック）
		 */
		jQuery(ev.target).on('click', $('.favorites_src_folder_icon').selector, function() {
			//フォルダ／ドキュメント情報の取得
			pfolderList_scr_workspace_infoId = (this.id).substring(26);
			// ヘッダーメニューバックスを閉じる
			$('#header_menuBox').hide();
			// フォルダ一覧コンとろらーにフォルダＩＤを設定
			folderList_scr_Controller.setParameter(pfolderList_scr_workspace_infoId);
			// フォルダ一覧画面の遷移
			$.mobile.changePage(folderList_scr_Controller.getViewId(), {'transition' : 'fade', });
		});
		
		/**
		 * お気に入り一覧オブジェクトクリックイベントハンドラ
		 */
		jQuery(ev.target).on('click', $('.favorites_scr_list_item').selector, function() {
			// クーキーをアップデート
			pfolderList_scr_workspace_infoId = (this.id).substring(26);
			// ヘッダーメニューバックスを閉じる
			$('#header_menuBox').hide();
			// フォルダ一覧コンとろらーにフォルダＩＤを設定
			folderList_scr_Controller.setParameter(pfolderList_scr_workspace_infoId);
			// フォルダ一覧画面の遷移
			$.mobile.changePage(folderList_scr_Controller.getViewId(), {'transition' : 'fade', });
		});
		
		/**
		 * トップへ戻るボタンクリックイベントハンドラ
		 */
		jQuery(ev.target).on('click', $('#favorites_scr_View-list_scr_top_icon').selector, function() {
			// スクロールの1行目を最上行に表示する。
			// 画面の縦/横の初期設定
			if ($(document.body).height()>800)
			{
				resize_screen_portrait();
			}
			else
			{
				resize_screen_landscape();
			}
		});
		
		/**
		 * エラーメッセージＯＫボタンクリックイベントハンドラ
		 */
		jQuery(ev.target).on('click', $('#favorites_scr_View-message_popup_ok').selector, function() {
			// クーキーをアップデート
			window.localStorage.setItem(EIM_KEY_CURRENT_ERROR_MESSAGE, JSON.stringify(cmnGetmsg['MSG_ERR_'+planguage+'_00006']));
			// ヘッダーメニューバックスを閉じる
			$('#header_menuBox').hide();
			// フォルダ一覧コンとろらーにフォルダＩＤを設定
			folderList_scr_Controller.setParameter("000");
			// フォルダ一覧画面の遷移
			$.mobile.changePage(folderList_scr_Controller.getViewId(), {'transition' : 'fade', });
			
		});
		
		/**
		 * 「success」イベントハンドラテンプレート
		 */
		view.on('success', function(e, data) {
			;
		});
		
		
		/**
		 * 「error」イベントハンドラテンプレート
		 */
		view.on('error', function(e, data) {
			;
		});
		
		
		/**
		 * 「get_dspFavoriteListSuccess」イベントハンドラ
		 */
		view.on('get_dspFavoriteListSuccess', function(e, xml) {
			var fcounter = 0;
			
			// 変数にXMLのお気に入り情報をバインド
			
			$(xml).find("objList").each(function(){
				$(xml).find("object").each(function(){
					var fobjId = "";
					var fobjTypeId ="";
					var fobjName ="";
					var fpath = "";
					
					// objIdタブ
					fobjId = $(this).attr('objId');
					// objTypeIdタブ
					fobjTypeId = $(this).attr('objTypeId');
					// objNameタブ
					fobjName = $(this).attr('objName');
					// pathタブ
					fpath = $(this).attr('path');
			    
					pCurrentFolder[fobjId]=new Object();
					pCurrentFolder[fobjId]['objTypeId']=fobjTypeId;
					pCurrentFolder[fobjId]['objName']=fobjName;
					pCurrentFolder[fobjId]['path']=fpath;
			    	
					fcounter = fcounter + 1;
				});
			});
			
			if (fcounter == 0)
			{
				
				f.cmnPopup(cmnGetmsg['MSG_ERR_'+planguage+'_00006'],"ERR",viewName);
				return;
			}
			
			// コンポーネントに変数をバインド
			if ($(document.body).height()>800)
			{	
				resize_screen_portrait();
			}
			else 
			{
				resize_screen_landscape();
			};
		});

		/**
		 * 「get_dspFavoriteListError」イベントハンドラ
		 */
		view.on('get_dspFavoriteListError', function(e, xml) {
			// エラーメッセージを表示する。
			$(xml).find("error").each(function(){
				f.cmnPopup($(this).attr('message'),"ERR",viewName);
				if (($(this).attr('message').indexOf('タイムアウト')>=0)||($(this).attr('message').indexOf('time-out')>=0))
				{
					// ログアウトの場合、ブラウザーのメモリとクーキーを削除
					f.memory_clean();
					// フラッグ
					coming_from_logout = true;
					// ログイン画面の遷移
					$.mobile.changePage(login_scr_Controller.getViewId(), {'transition' : 'fade', });	
				}
			});
		});
		
		/**
		 * 「post_dspApproveDocumentListSuccess」イベントハンドラ
		 */
		view.on('post_dspApproveDocumentListSuccess', function(e, xml) {
			pmain_scr_unapproved_count = 0;
			$(xml).find("object").each(function(){
			    // 変数にXMLの未承認件数情報をバインド
				if ($(this).attr('objId')!=""){
					pmain_scr_unapproved_count = pmain_scr_unapproved_count+1;	
				}
			});
			// 未承認件数クーキーを登録する
			try {
				window.localStorage.setItem(EIM_KEY_APPROVEDOCUMENT_COUNT, JSON.stringify(pmain_scr_unapproved_count));
			}
			catch (e){;
			}
			// ヘッダーの未承認件数をアップデート
			if (pmain_scr_unapproved_count>0){
				$('.header_unapprovedText').empty().append(cmnLanguage['LAN_'+planguage+'_00008']+"("+pmain_scr_unapproved_count+")");
			}
			else
			{
				$('.header_unapprovedText').empty().append(cmnLanguage['LAN_'+planguage+'_00008']);
			}
		});
		
		/**
		 * 「post_dspApproveDocumentListError」イベントハンドラ
		 */
		view.on('post_dspApproveDocumentListError', function(e, xml) {
			// エラーメッセージを表示する。
			$(xml).find("error").each(function(){
				f.cmnPopup($(this).attr('message'),"ERR",viewName);
				if (($(this).attr('message').indexOf('タイムアウト')>=0)||($(this).attr('message').indexOf('time-out')>=0))
				{
					// ログアウトの場合、ブラウザーのメモリとクーキーを削除
					f.memory_clean();
					// フラッグ
					coming_from_logout = true;
					// ログイン画面の遷移
					$.mobile.changePage(login_scr_Controller.getViewId(), {'transition' : 'fade', });
				}
			});
		});

	});
	
	/*****************************************************************
	 * ページ表示処理
	 ****************************************************************/
	jQuery(document).on('pageinit', viewId, function(ev) {
		// トップへ戻るボタンを設定
		$('.favorites_scr_View-list_scr_top_icon_div').hide();
		$('#favorites_scr_View-list_scr_top_icon').empty().append('<a id="favorites_scr_View-list_scr_top_icon" href="#">' + 
																	cmnGetimg['list_scr_top_icon'] + '</a>');
		
		// 閉じる
		$('.header_menuBox').hide();
		
		model.post_dspApproveDocumentList();
	});
	
	/**
	 * 「pagebeforeshow」イベントハンドラテンプレート
	 */
	jQuery(document).on('pagebeforeshow', viewId, function(ev) {
		
		// パンクずと検索を非表示
		$('.folderList_scr_View-folderList_scr_order_combo_div').hide();
		$('.folderList_scr_View-folderList_scr_bc').hide();
		
		pcurrentView = viewName;
		// トップへ戻るボタンを非表示にする。
		$('.folderList_scr_View-list_scr_top_icon_div').hide();
		$('.unapproved_scr_View-list_scr_top_icon_div').hide();
		$('.favorites_scr_View-list_scr_top_icon_div').hide();
		// ボタンハイライト
		cmn_header_button_highlight (3);	
	});
	
	/**
	 * 「pageshow」イベントハンドラ
	 */
	jQuery(document).on('pageshow', viewId, function(ev) {
		
		// ヘッダーのタイトル
		$('.header_title').empty();
		
		// ヘッダーのタイトルラベルを設定
		planguage = window.localStorage.getItem(EIM_KEY_LANGUAGE);
		planguage = JSON.parse(planguage);
		if(planguage == null){
			planguage = "JA";
		}
		else
		{
			planguage = planguage.substring(0,2);
		}
		$('.header_title').empty().append(cmnLanguage['LAN_'+planguage+'_00028']);
		
		// クーキーを確認
		if (JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_LIST_TOP_ELEMENT, JSON.stringify(ptop_locator))) != null){
			ptop_locator = JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_LIST_TOP_ELEMENT, JSON.stringify(ptop_locator)));
		}
		else 
		{
			ptop_locator = "";
		}
		
		ptop_locator = "";
		// クーキーをアップデート
		window.localStorage.setItem(EIM_KEY_CURRENT_LIST_TOP_ELEMENT, JSON.stringify(ptop_locator));
		
		// ワークスペース情報の取得
		model.get_dspFavoriteList();	
	});
	
	/*****************************************************************
	 * 関数定義
	 ****************************************************************/
	/**
	 * initialize
	 */
	function initialize () {
		;
	}

	
	/********************************************************************************
	 * [機能]	縦向きになった場合
	 * [引数]	無し
	 * [戻値]	無し
	 ********************************************************************************/
	function resize_screen_portrait() {
		var ffavorites_scr_list_item_html = "";
		var fpath = "";
		
		// 並び替え
		var fsorted_list =null;
		var i = "";
		
		// ＯＳによって、アイコンのサイズを変わる
		cmn_change_image_sizes (0, planguage);
		
		// トップへ戻るボタンを非表示にする。
		$('.favorites_scr_View-list_scr_top_icon_div').hide();	

		// コンポーネントリドロー
		if ((navigator.userAgent.indexOf("iPad")>0)&&((navigator.userAgent.indexOf("OS 8")>0)||(navigator.userAgent.indexOf("OS 7")>0)))
		{
			ffavorites_scr_list_item_html = '<ul style="height: 747px;" class="scrollable" data-role="listview" data-split-icon="grid" data-split-theme="d">';
		}
		else if (navigator.userAgent.indexOf("iPad")>0)
		{
			ffavorites_scr_list_item_html = '<ul style="height: 705px;" class="scrollable" data-role="listview" data-split-icon="grid" data-split-theme="d">';	
		}
		else if (navigator.userAgent.indexOf("iPhone")>0)
		{
			ffavorites_scr_list_item_html = '<ul style="height: 1330px;" class="scrollable" data-role="listview" data-split-icon="grid" data-split-theme="d">';
		}
		else 
		{
			ffavorites_scr_list_item_html = '<ul style="height: 705px;" class="scrollable" data-role="listview" data-split-icon="grid" data-split-theme="d">';
		}
		
		// 並び替え
		fsorted_list = create_sorted_list(0);
		
		ptop_locator = "";
		// クーキーをアップデート
		window.localStorage.setItem(EIM_KEY_CURRENT_LIST_TOP_ELEMENT, JSON.stringify(ptop_locator));
		
		for (var key in fsorted_list){
			i = String(fsorted_list[key][0]);
			
			if (ptop_locator == "") 
			{
				ptop_locator = 'favorites_src_folder_icon_'+i;
				// クーキーをアップデート
				window.localStorage.setItem(EIM_KEY_CURRENT_LIST_TOP_ELEMENT, JSON.stringify(ptop_locator));
			} 
			
			fpath = pCurrentFolder[i]['path'];
			fpath = resizeString(fpath, viewName, 'portrait');
			
			ffavorites_scr_list_item_html = ffavorites_scr_list_item_html + 
			'<li class="favorites_scr_list_item" id="favorites_src_folder_info_'+i+'">'+
				'<table style="vertical-align: top;-webkit-border-horizontal-spacing: 0px; -webkit-border-vertical-spacing: 0px;' +
				' border: inset 0pt;border-style: hidden;border-collapse: collapse;border-spacing:0;width:100%;"><tr width="100%">' +
				'<td width="15%" valign="top"><a class="favorites_src_folder_icon" id="favorites_src_folder_icon_'+i+
				'" href="#" data-transition="slidefade">' + cmnGetimg['favorites_scr_btn_Favourites'] + '</a></td>' +
				'<td width="75%" align="left" style="vertical-align: middle;"><a class="favorites_src_folder_info" id="favorites_src_folder_info_'+
				i+'" href="#"><font style="font-size: 150%;font-weight: bold;">'+pCurrentFolder[i]['objName']+'</font><br>'+
				cmnLanguage['LAN_'+planguage+'_00027'] + ': '+fpath+
				'</a></td><td width="5%" ></td></tr></table>'+
			'</li>';
		};
		
		ffavorites_scr_list_item_html = ffavorites_scr_list_item_html + '</ul>';
		pfavorites_scr_fileInfo.empty().append(ffavorites_scr_list_item_html);
		
	}
	
	/********************************************************************************
	 * [機能]	横向きになった場合
	 * [引数]	無し
	 * [戻値]	無し
	 ********************************************************************************/
	function resize_screen_landscape() {
		var ffavorites_scr_list_item_html="";
		var fpath = "";
		
		// 並び替え
		var fsorted_list =null;
		var i = "";
		
		// ＯＳによって、アイコンのサイズを変わる
		cmn_change_image_sizes (1, planguage);
		
		// トップへ戻るボタンを非表示にする。
		$('.favorites_scr_View-list_scr_top_icon_div').hide();	
		
		// コンポーネントリドロー
		if ((navigator.userAgent.indexOf("iPad")>0)&&((navigator.userAgent.indexOf("OS 8")>0)||(navigator.userAgent.indexOf("OS 7")>0)))
		{
			ffavorites_scr_list_item_html = '<ul style="height: 490px;" class="scrollable" data-role="listview" data-split-icon="grid" data-split-theme="d">';
		}
		else if (navigator.userAgent.indexOf("iPad")>0)
		{
			ffavorites_scr_list_item_html = '<ul style="height: 455px;" class="scrollable" data-role="listview" data-split-icon="grid" data-split-theme="d">';	
		}
		else 
		{
			ffavorites_scr_list_item_html = '<ul style="height: 475px;" class="scrollable" data-role="listview" data-split-icon="grid" data-split-theme="d">';	
		}
		
		// 並び替え
		fsorted_list = create_sorted_list(0);
		
		ptop_locator = "";
		// クーキーをアップデート
		window.localStorage.setItem(EIM_KEY_CURRENT_LIST_TOP_ELEMENT, JSON.stringify(ptop_locator));
		
		for (key in fsorted_list){
			i = String(fsorted_list[key][0]);
		
			if (ptop_locator == "") 
			{
				ptop_locator = 'favorites_src_folder_icon_'+i;
				// クーキーをアップデート
				window.localStorage.setItem(EIM_KEY_CURRENT_LIST_TOP_ELEMENT, JSON.stringify(ptop_locator));
			} 
			
			fpath = pCurrentFolder[i]['path'];
			fpath = resizeString(fpath, viewName, 'landscape');
			
			ffavorites_scr_list_item_html = ffavorites_scr_list_item_html + 
			'<li class="favorites_scr_list_item" id="favorites_src_folder_info_'+i+'">'+
				'<table style="vertical-align: top;-webkit-border-horizontal-spacing: 0px; -webkit-border-vertical-spacing: 0px; ' + 
				'border: inset 0pt;border-style: hidden;border-collapse: collapse;border-spacing:0;width:100%;"><tr width="100%">' + 
				'<td width="15%" valign="top"><a class="favorites_src_folder_icon" id="favorites_src_folder_icon_'+i+
				'" href="#" data-transition="slidefade">' + cmnGetimg['favorites_scr_btn_Favourites'] + '</a></td>' +
				'<td width="75%" align="left" style="vertical-align: middle;"><a class="favorites_src_folder_info" id="favorites_src_folder_info_'+i+'" href="#">' + 
				'<font style="font-size: 150%;font-weight: bold;">'+pCurrentFolder[i]['objName']+'</font><br>'+
				cmnLanguage['LAN_'+planguage+'_00027']+': ' + fpath+
				'</a></td><td width="5%"></td></tr></table>'+
			'</li>';
		};

		ffavorites_scr_list_item_html = ffavorites_scr_list_item_html + '</ul>';
		pfavorites_scr_fileInfo.empty().append(ffavorites_scr_list_item_html);
	}
	
	
	/**
	 * 並び替え：無し
	 */
	function create_sorted_list(screen_type) {
		var sortable = [];
		var forigin;
		
		forigin = pCurrentFolder;
		
		for (var key in forigin)
		{
			// 名称
			sortable.push([key,forigin[key]['objName']]);
		};
		//sortable.sort(function(a, b) {return String(a[1].localeCompare(String(b[1])));});
		return sortable;
	}
	
	/**
	 * getParameterテンプレート
	 */
	function getParameter () {
		;
	}
	
	/**
	 * setParameter
	 */
	function setParameter (data) {
		// ログアウトする場合、マインメニューコントローラー
		// はこの機能でメモリクリーンをリクエストする。
		if (data == null)
		{
			if (pfavorites_scr_fileInfo != null)
			{
				pfavorites_scr_fileInfo.empty();
				pCurrentFolder = new Object();
			}
		}
	}
	
	/**
	 * getViewId
	 */
	function getViewId (controller) {
		return viewId;
	}
	
	/*****************************************************************
	 * 公開設定
	 ****************************************************************/
	return {
		getParameter : getParameter,
		setParameter : setParameter,
		getViewId : getViewId,
	};
	
}();
