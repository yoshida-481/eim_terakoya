/************************************************************
 * jQuery ヘッダー・アプリケーション全体イベント
 ************************************************************
 */
/**
 * 画面回転イベントカスタマイズ
 */
$(document).ready(function(){
	$(window).bind("orientationchange",function(e){
		if (e.orientation=="portrait"){
			$(login_scr_Controller.getViewId()).trigger('orientationChange_Portrait');
			$(main_scr_Controller.getViewId()).trigger('orientationChange_Portrait');
			$(folderList_scr_Controller.getViewId()).trigger('orientationChange_Portrait');
			$(favorites_scr_Controller.getViewId()).trigger('orientationChange_Portrait');
			$(unapproved_scr_Controller.getViewId()).trigger('orientationChange_Portrait');
			$(properties_scr_Controller.getViewId()).trigger('orientationChange_Portrait');
			$(check_scr_Controller.getViewId()).trigger('orientationChange_Portrait');
		} 
		else 
		{
			$(login_scr_Controller.getViewId()).trigger('orientationChange_Landscape');
			$(main_scr_Controller.getViewId()).trigger('orientationChange_Landscape');
			$(folderList_scr_Controller.getViewId()).trigger('orientationChange_Landscape');
			$(favorites_scr_Controller.getViewId()).trigger('orientationChange_Landscape');
			$(unapproved_scr_Controller.getViewId()).trigger('orientationChange_Landscape');
			$(properties_scr_Controller.getViewId()).trigger('orientationChange_Landscape');
			$(check_scr_Controller.getViewId()).trigger('orientationChange_Landscape');
		}
	});
});


/************************************************************
 * jQuery Mobile 設定変更
 ************************************************************
 */
$(window).on('pagecontainercreate', function () {
	$.event.special.swipe.durationThreshold = 800;
	$.event.special.swipe.horizontalDistanceThreshold = 200;
});

/************************************************************
 * 全ページのヘッダー・フッター内要素を生成
 ************************************************************
 */


/**
 * 「pagebeforecreate」イベントハンドラ
 */
$(document).on('pagebeforecreate', ':jqmData(role="page")',
	function(e, ui){
		create_header($(this));
	}
);

/**
 * 「pageshow」イベントハンドラ
 */
jQuery(document).on('pageshow', ':jqmData(role="page")', 
	function(e, ui){
		// ヘッダーを生成
		var url = $(this).jqmData('url');
		
		if ((url != "login_scr_View") && (url != "main_scr_View"))
		{
			// ローカルストレージからユーザー名称を取り出す
			set_header_labels();
		}
	}
);

/**
 * ヘッダーのＨＴＭＬを追加
 */
function create_header(doc_jquery)
{
	var language = "";

	if (pheader_created == true)
	{
		return;
	}
	
	// ヘッダーを生成
	var url = doc_jquery.jqmData('url');
	
	if ((url != "login_scr_View") && (url != "main_scr_View"))
	{
		// ヘッダーを生成
		
		// ローカルストレージからユーザー名称を取り出す
		var header_userName = window.localStorage.getItem(EIM_KEY_USERNAME);
		header_userName = JSON.parse(header_userName);
		
		if(header_userName == null){
			header_userName = "";
		}
			
		/**
		 * スクロールイベントハンドラ
		 */
		document.addEventListener("touchmove", ScrollStart, false);
		
		// ヘッダーを設定 data-mini="true"
		var previous = $(document).find(':jqmData(id="menu-bar")').html();
		$(document).find(':jqmData(id="menu-bar")').html("");
		$(document).find(':jqmData(id="menu-bar")').append(
			'<center>'+
			'<table width="100%" height="45" align="center">'+
				'<tr>'+
					'<td width="1%">'+
						'<a data-shadow="false" data-theme="a" class="class-btn-return-page header_navigation_buttons header_btn_Back" id="header_btn_Back" href="#" data-icon="header_btn_Back" data-role="button" data-iconpos="top" data-rel="back" '+
						'data-direction="reverse" class="jqm-home"></a></td>'+
					'<td width="29%">'+
					'<td align="center" width="40%"><font style="font-size: 150%;"><label class="header_title"></label></font></td>'+
					'<td width="29%" align="left"><table><tr><td valign="bottom"><label class="main_scr_user_icon_"></label></td><td valign="top"><font style="font-size: 125%;"><label class="header_user_name">'+header_userName+'</label></font></td></tr></table></td>'+
					'<td width="1%" class="combo_container" align="right"><select data-icon="header_btn_Menu" id="header_btn_Combo" class="header_btn_Combo" data-theme="c"></select></td>'+
			'</tr>'+
			'</table>'+
			'</center>'+
			'<div data-role="navbar">'+
			'<ul>'+
					'<li><a class="header_buttons" data-role="button" data-iconpos="top" id="header-btn_FolderList" data-icon="main_scr_btn_folderList" data-theme="b"><label class="header_listText"></label></a></li>'+
					'<li><a class="header_buttons" data-role="button" data-iconpos="top" id="header-btn_Unapproved" data-icon="main_scr_btn_Unapproved" data-theme="b"><label class="header_unapprovedText"></label></a></li>'+
					'<li><a class="header_buttons" data-role="button" data-iconpos="top" id="header-btn_Favorites" data-icon="main_scr_btn_Favorites" data-theme="b"><label class="header_favoritesText"></label></a></div></li>'+
			'</ul>'+
			'</div>'+
			previous
		);
		
		// パンクずと検索を設定
		if (url=="folderList_scr_View")
		{
			$('.folderList_scr_View-folderList_scr_order_combo_div').show();
			$('.folderList_scr_View-folderList_scr_bc').show();
		}
		else 
		{
			$('.folderList_scr_View-folderList_scr_order_combo_div').hide();
			$('.folderList_scr_View-folderList_scr_bc').hide();
		}
		
		// ヘッダーのメニューコンボを設定
		language = window.localStorage.getItem(EIM_KEY_LANGUAGE);
		language = JSON.parse(language);
		if(language == null){
			language = "JA";
		}
		else
		{
			language = language.substring(0,2);
		}
		
		$('.header_btn_Combo').append('<option value="'+cmnLanguage['LAN_'+language+'_00012']+'">' + 
				cmnLanguage['LAN_'+language+'_00012'] + '</option>');
		$('.header_btn_Combo').append('<option value="'+cmnLanguage['LAN_'+language+'_00013']+'">' + 
				cmnLanguage['LAN_'+language+'_00013'] + '</option>');
		$('.header_btn_Combo').append('<option selected value="'+cmnLanguage['LAN_'+language+'_00035']+'">' + 
				cmnLanguage['LAN_'+language+'_00035'] + '</option>');
		
		// ボタンハイライト
		cmn_header_button_highlight (0);
		var style3 = document.createElement('style');
		style3.type = 'text/css';
		style3.innerHTML = '#header_btn_Back {background: -webkit-linear-gradient(top, #FFFFFF 0%, #FFFFFF 100%) !important;}';
		document.getElementsByTagName('head')[0].appendChild(style3);
		var style4 = document.createElement('style');
		style4.type = 'text/css';
		style4.innerHTML = '#header_btn_Menu {background: -webkit-linear-gradient(top, #FFFFFF 0%, #FFFFFF 100%) !important;}';
		document.getElementsByTagName('head')[0].appendChild(style4);
		
		// アイコンをバインド
		$('.main_scr_user_icon_').empty().append(cmnGetimg['main_scr_user_icon_']);
		
		// デフォルト
		$('#folderList_scr_View-folderList_scr_breadcrumb').empty().append('<ul id="breadcrumbs-one"><li><a class="folderList_scr_breadcrumb" id="Ｔｏｐ" href="#">　Ｔｏｐ　</a></li></ul>');
		
		pheader_created = true;
	}
}

/**
 * スクロールイベントハンドラ
 */
function ScrollStart() {
	var ftop_value_id ="";
	try
	{
		if (pcurrentView == "login_scr_View")
		{
			return;
		}
		
		if ((navigator.userAgent.indexOf("iPad")>0)&&((navigator.userAgent.indexOf("OS 8")>0)||(navigator.userAgent.indexOf("OS 7")>0)))
		{
			ftop_value_id = pcurrentView + "_OS_8";
		}
		else 
		{
			ftop_value_id = pcurrentView;
		}
		// 一階層上フォルダアイコン
		var offset = $('#'+ptop_locator).offset();
		
		//テスト
		//$('.header_title').empty().append(offset['top']+":"+offset['left']+ptop_locator+"@"+pcurrentView);
		
		if (offset['top'] < ptop_value[ftop_value_id]){
			// 1行目が画面から見えなくなった際、トップへ戻るボタンを表示する。
			$('#'+pcurrentView+'-list_scr_top_icon_div').show();
		}
		else 
		{
			// 1行目が画面に表示された場合、トップへ戻るボタンを非表示にする。
			$('#'+pcurrentView+'-list_scr_top_icon_div').hide();
		};	
	}
	catch (e)
	{
		;
	}
}


/**
 * アプリケーション全体
 * ヘッダーのフォルダ一覧ボタンクリックイベントハンドラ
 */
jQuery(document).on('click', $('#header-btn_FolderList').selector, function(ev) {
	// 上に戻るリセット
	window.localStorage.removeItem(EIM_KEY_CURRENT_LIST_TOP_ELEMENT);
	// フォルダ一覧画面の遷移
	$.mobile.changePage(folderList_scr_Controller.getViewId(), {'transition' : 'fade', });
});


/**
 * アプリケーション全体
 * ヘッダーの未承認一覧ボタンクリックイベントハンドラ
 */
jQuery(document).on('click', $('#header-btn_Unapproved').selector, function(ev) {
	// 上に戻るリセット
	window.localStorage.removeItem(EIM_KEY_CURRENT_LIST_TOP_ELEMENT);
	// 未承認一覧画面の遷移
	$.mobile.changePage(unapproved_scr_Controller.getViewId(), {'transition' : 'fade', });
});


/**
 * アプリケーション全体
 * ヘッダーのお気に入りボタンクリックイベントハンドラ
 */
jQuery(document).on('click', $('#header-btn_Favorites').selector, function(ev) {
	clicked_on_header = true;
	// 上に戻るリセット
	window.localStorage.removeItem(EIM_KEY_CURRENT_LIST_TOP_ELEMENT);
	// お気に入り画面の遷移
	$.mobile.changePage(favorites_scr_Controller.getViewId(), {'transition' : 'fade', });
});


/**
 * アプリケーション全体
 * メニューボックス「最新に更新」クリック
 */
jQuery(document).on('click', '.header_menuBox_reload', function(ev) {
	// サーバーからページを読む
	location.reload(true);
});

/**
 * アプリケーション全体
 * メニューボックス「ホームに戻る」クリック
 */
jQuery(document).on('click', '.header_menuBox_home', function(ev) {
	// 上に戻るリセット
	window.localStorage.removeItem(EIM_KEY_CURRENT_LIST_TOP_ELEMENT);
	// マインメニュー画面の遷移
	$.mobile.changePage(main_scr_Controller.getViewId(), {'transition' : 'fade', });
});

/**
 * アプリケーション全体
 * メニューボックス「ホームに戻る」クリック
 */
jQuery(document).on('change', '.header_btn_Combo', function(ev) {
	var language = "";
	
	if (($(this).val() == cmnLanguage['LAN_JA_00012'])||($(this).val() == cmnLanguage['LAN_EN_00012']))
	{
		// ヘッダーのメニューコンボを設定
		language = window.localStorage.getItem(EIM_KEY_LANGUAGE);
		language = JSON.parse(language);
		if(language == null){
			language = "JA";
		}
		else
		{
			language = language.substring(0,2);
		}
		$('.header_btn_Combo').empty();
		$('.header_btn_Combo').append('<option value="'+cmnLanguage['LAN_'+language+'_00012']+'">' + 
				cmnLanguage['LAN_'+language+'_00012'] + '</option>');
		$('.header_btn_Combo').append('<option value="'+cmnLanguage['LAN_'+language+'_00013']+'">' + 
				cmnLanguage['LAN_'+language+'_00013'] + '</option>');
		$('.header_btn_Combo').append('<option selected value="'+cmnLanguage['LAN_'+language+'_00035']+'">' + 
				cmnLanguage['LAN_'+language+'_00035'] + '</option>');
		// 上に戻るリセット
		window.localStorage.removeItem(EIM_KEY_CURRENT_LIST_TOP_ELEMENT);
		// マインメニュー画面の遷移
		$.mobile.changePage(main_scr_Controller.getViewId(), {'transition' : 'fade', });
	}
	else if (($(this).val() == cmnLanguage['LAN_JA_00013'])||($(this).val() == cmnLanguage['LAN_EN_00013']))
	{
		// サーバーからページを読む
		location.reload(true);
	}
});

/**
 * アプリケーション全体
 * 「戻る」ボタン・「キャンセル」ボタンの遷移先制御
 */
jQuery(document).on('click', '.class-btn-return-page', function(ev) {
	if (pcurrentView == "properties_scr_View")
	{
		properties_scr_Controller.setParameter(null);
	}
	menu_history_back = true;
	window.history.back();
});

