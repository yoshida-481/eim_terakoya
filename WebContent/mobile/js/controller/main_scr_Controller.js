/**
 * main_scr_Controller
 */
var main_scr_Controller = function () {
	/*****************************************************************
	 * 宣言
	 ****************************************************************/
	function F() {};
	F.prototype = prototypeController;
	var f = new F();
	
	// ビュー
	const viewName = "main_scr_View";
	const viewId = "#" + viewName;
	var view = null;
	// モデル
	const model = main_scr_Model;
	
	// コンポーネント
	var pmain_scr_title = null;
	var pmain_scr_folder_list_text = null;
	var pmain_scr_unapproved_text = null;
	var pmain_scr_favorites_text = null;
	var pmain_scr_userName = null;
	var pmain_scr_logout_popup = null;
	var pmain_scr_btn_Logout_l = null;
	var pmain_scr_btn_Logout = null;
	var pmain_scr_btn_Logout_no = null;
	// main_scr_unapproved_count → 「eim_key_language.js」pmain_scr_unapproved_count
	
	// 変数
	var pmain_scr_title_image = null;
	var pmain_scr_btn_FolderList = null;
	var pmain_scr_btn_Unapproved = null;
	var pmain_scr_btn_Favorites = null;
	var pmain_scr_user_icon = null;
	var pmain_scr_btn_Logout_ = null;
	var pmain_scr_logout_text = null;
	var pmain_scr_img_confirm_dialog = null;
	
	// 「dspSession.jsp」のメモリにあるセッションをバインドする。
	var pSession = new Object();
	var page_was_initialized = false;


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
		pmain_scr_title_image = $(viewId+'-'+'main_scr_title_image');
		pmain_scr_title = $(viewId+'-'+'main_scr_title');
		pmain_scr_btn_FolderList = $(viewId+'-'+'main_scr_btn_FolderList');
		pmain_scr_folder_list_text = $(viewId+'-'+'main_scr_folder_list_text');
		pmain_scr_btn_Unapproved = $(viewId+'-'+'main_scr_btn_Unapproved');
		pmain_scr_unapproved_text = $(viewId+'-'+'main_scr_unapproved_text');
		pmain_scr_btn_Favorites = $(viewId+'-'+'main_scr_btn_Favorites');
		pmain_scr_favorites_text = $(viewId+'-'+'main_scr_favorites_text');
		pmain_scr_user_icon = $(viewId+'-'+'main_scr_user_icon');
		pmain_scr_userName = $(viewId+'-'+'main_scr_userName');
		pmain_scr_btn_Logout = $(viewId+'-'+'main_scr_btn_Logout');
		pmain_scr_btn_Logout_l = $(viewId+'-'+'main_scr_btn_Logout_l');
		pmain_scr_logout_text = $(viewId+'-'+'main_scr_logout_text');
		pmain_scr_btn_Logout_ = $(viewId+'-'+'main_scr_btn_Logout_');
		pmain_scr_btn_Logout_no = $(viewId+'-'+'main_scr_btn_Logout_no');
		pmain_scr_logout_popup = $(viewId+'-'+'main_scr_logout_popup');
		pmain_scr_img_confirm_dialog = $(viewId+'-'+'main_scr_img_confirm_dialog');
		// ボタンをＤＩＳＡＢＬＥ
		pmain_scr_btn_FolderList.addClass('ui-disabled');
		pmain_scr_btn_Unapproved.addClass('ui-disabled');
		pmain_scr_btn_Favorites.addClass('ui-disabled');
		pmain_scr_btn_Logout.addClass('ui-disabled');
		
		// イメージ・アイコン
		pmain_scr_title_image.empty().append(cmnGetimg['login_scr_title_image']);
		pmain_scr_user_icon.empty().append(cmnGetimg['main_scr_user_icon']);
		pmain_scr_img_confirm_dialog.empty().append(cmnGetimg['main_scr_img_confirm_dialog']);
		
		document.getElementById("main_scr_View-main_scr_btn_FolderList").style.background='-webkit-linear-gradient(top, #257EFF 0%, #257EFF 100%)';
		document.getElementById("main_scr_View-main_scr_btn_Unapproved").style.background='-webkit-linear-gradient(top, #42DF48 0%, #42DF48 100%)';
		document.getElementById("main_scr_View-main_scr_btn_Favorites").style.background='-webkit-linear-gradient(top, #00D9EC 0%, #00D9EC 100%)';
		document.getElementById("main_scr_View-main_scr_btn_Logout").style.background='-webkit-linear-gradient(top, #E6E6E6 0%, #E6E6E6 100%)';
		
		pmain_scr_btn_FolderList.children().children().css('font-size','20px');
		pmain_scr_btn_Unapproved.children().children().css('font-size','20px');
		pmain_scr_btn_Favorites.children().children().css('font-size','20px');
		pmain_scr_btn_Logout.children().children().css('font-size','45px');
		
		// 画面の縦/横の初期設定
		if ($(document.body).height()>800)
		{
			resize_screen_portrait();
		}
		else
		{
			resize_screen_landscape();
		};
		
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
		 * Logoutボタンクリックイベントハンドラ
		 */
		jQuery(ev.target).on('click', pmain_scr_btn_Logout.selector, function() {
			// ボタンをＤＩＳＡＢＬＥ
			pmain_scr_btn_FolderList.addClass('ui-disabled');
			pmain_scr_btn_Unapproved.addClass('ui-disabled');
			pmain_scr_btn_Favorites.addClass('ui-disabled');
			// ログアウトコンファームボタンをＥＮＡＢＬＥ
			pmain_scr_btn_Logout_.removeClass('ui-disabled');
			pmain_scr_logout_popup.popup('open', {'transition' : 'pop' , 'position-to' : 'window'});
		});
		
		/**
		 * Logoutボタンクリックイベントハンドラ
		 * ポップアップログアウトコンファームボタン クリックイベントハンドラ
		 * [Yes]をクリックと、ログアウトが行う
		 */
		jQuery(ev.target).on('click', pmain_scr_btn_Logout_.selector, function() {
			// ログアウトコンファームボタンをＤＩＳＡＢＬＥ
			pmain_scr_btn_Logout_.addClass('ui-disabled');
			// ログアウトの情報を取得
			model.get_actLogout();
		});
		
		/**
		 * Logoutボタンクリックイベントハンドラ
		 * ポップアップログアウトコンファームボタン クリックイベントハンドラ
		 * [No]をクリックと、ログアウトが行い
		 */
		jQuery(ev.target).on('click', pmain_scr_btn_Logout_no.selector, function() {
			// ボタンをＥＮＡＢＬＥ
			pmain_scr_btn_FolderList.removeClass('ui-disabled');
			pmain_scr_btn_Unapproved.removeClass('ui-disabled');
			pmain_scr_btn_Favorites.removeClass('ui-disabled');
		});
		
		/**
		 * フォルダ一覧ボタンクリックイベントハンドラ
		 */
		jQuery(ev.target).on('click', pmain_scr_btn_FolderList.selector, function() {
			// フォルダ一覧画面の遷移
			$.mobile.changePage(folderList_scr_Controller.getViewId(), {'transition' : 'fade', });
		});
		
		/**
		 * 未承認ボタンクリックイベントハンドラ
		 */
		jQuery(ev.target).on('click', pmain_scr_btn_Unapproved.selector, function() {
			// フラッグリセット
			coming_from_logout = false;
			// 未承認画面の遷移
			$.mobile.changePage(unapproved_scr_Controller.getViewId(), {'transition' : 'fade', });
		});
		
		/**
		 * お気に入りボタンクリックイベントハンドラ
		 */
		jQuery(ev.target).on('click', pmain_scr_btn_Favorites.selector, function() {
			// フラッグリセット
			coming_from_logout = false;
			// フォルダ一覧画面の遷移
			$.mobile.changePage(favorites_scr_Controller.getViewId(), {'transition' : 'fade', });
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
			// コンポーネントに変数をバインド
			if (pmain_scr_unapproved_count>0){
				pmain_scr_unapproved_text.empty().append(cmnLanguage['LAN_'+pSession['language'].substring(0,2)+'_00008']+"("+pmain_scr_unapproved_count+")");
			}
			else 
			{
				if ($('.header_unapprovedText') != null)
				{
					$('.header_unapprovedText').empty().append(cmnLanguage['LAN_'+pSession['language']+'_00008']);
				}
			};
			// 未承認件数クーキーを登録する
			try {
				window.localStorage.setItem(EIM_KEY_APPROVEDOCUMENT_COUNT, JSON.stringify(pmain_scr_unapproved_count));
			}
			catch (e){;
			}
			
			// ボタンをＥＮＡＢＬＥ
			pmain_scr_btn_FolderList.removeClass('ui-disabled');
			pmain_scr_btn_Unapproved.removeClass('ui-disabled');
			pmain_scr_btn_Favorites.removeClass('ui-disabled');
			pmain_scr_btn_Logout.removeClass('ui-disabled');
			
			page_was_initialized=true;
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
		
		/**
		 * 「get_actLogoutSuccess」イベントハンドラ
		 */
		view.on('get_actLogoutSuccess', function(e, xml) {
			$(xml).find("windowAction").each(function(){
				if ($(this).attr('windowClose')=="off")
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
		 * 「get_actLogoutError」イベントハンドラ
		 */
		view.on('get_actLogoutError', function(e, xml) {
			// エラーメッセージを表示する。
			$(xml).find("error").each(function(){
				f.cmnPopup($(this).attr('message'),"ERR",viewName);
			});
		});			
		
	});
	
	/*****************************************************************
	 * ページ表示処理
	 ****************************************************************/
	
	/**
	 * 「pageinit」イベントハンドラ
	 */
	jQuery(document).on('pageinit', viewId, function(ev) {
		set_labels();
	});
	
	/**
	 * 「pagebeforeshow」イベントハンドラ
	 */
	jQuery(document).on('pagebeforeshow', viewId, function(ev) {
		
		if (page_was_initialized){
			pmain_scr_userName.empty();
			// ボタンをＤＩＳＡＢＬＥ
			pmain_scr_btn_FolderList.addClass('ui-disabled');
			pmain_scr_btn_Unapproved.addClass('ui-disabled');
			pmain_scr_btn_Favorites.addClass('ui-disabled');
			pmain_scr_btn_Logout.addClass('ui-disabled');
			
			set_labels();
		}
		
		// ボタンハイライトリセット
		cmn_header_button_highlight (0);
	});
	
	/*****************************************************************
	 * 関数定義
	 ****************************************************************/
	/**
	 * initializeテンプレート
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
		
		if (navigator.userAgent.indexOf("iPad")>0)
		{
			$("#main_scr_View-filler1").empty().append("<br><br><br><br><br><br><br>");
			$("#main_scr_View-filler2").empty().append("<br><br><br><br><br><br><br>");
			var style = document.createElement('style');
			style.type = 'text/css';
			style.innerHTML = '.ui-icon-main_scr_btn_Logout {margin-right: 99px !important;}';
			document.getElementsByTagName('head')[0].appendChild(style);
			$('.main_logout_button').css({
	            'width' : '450px',
	        });
			$('.logout_button_confirm').css({
	            'width' : '200px',
	        });
		}
		else if (navigator.userAgent.indexOf("iPhone")>0)
		{
			$("#main_scr_View-filler1").empty().append("<br><br><br><br><br><br><br>");
			$("#main_scr_View-filler2").empty().append("<br><br><br><br><br><br><br>");
			var style = document.createElement('style');
			style.type = 'text/css';
			style.innerHTML = '.ui-icon-main_scr_btn_Logout {margin-right: 99px !important;}';
			document.getElementsByTagName('head')[0].appendChild(style);
			$('.main_logout_button').css({
	            'width' : '550px',
	        });
			$('.logout_button_confirm').css({
	            'width' : '200px',
	        });
			$("#main_scr_View-filler3").empty().append("<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>");	
		}
		else 
		{
			$("#main_scr_View-filler1").empty().append("<br><br><br><br><br><br><br>");
			$("#main_scr_View-filler2").empty().append("<br><br><br><br><br><br><br>");
			var style = document.createElement('style');
			style.type = 'text/css';
			style.innerHTML = '.ui-icon-main_scr_btn_Logout {margin-right: 99px !important;}';
			document.getElementsByTagName('head')[0].appendChild(style);
			$('.main_logout_button').css({
	            'width' : '450px',
	        });
			$('.logout_button_confirm').css({
	            'width' : '200px',
	        });
		}
	}
	
	/********************************************************************************
	 * [機能]	横向きになった場合
	 * [引数]	無し
	 * [戻値]	無し
	 ********************************************************************************/
	function resize_screen_landscape() {
		$("#main_scr_View-filler1").empty().append("<br><br>");
		$("#main_scr_View-filler2").empty().append("<br><br>");
		var style = document.createElement('style');
		style.type = 'text/css';
		style.innerHTML = '.ui-icon-main_scr_btn_Logout {margin-right: 179px !important;}';
		document.getElementsByTagName('head')[0].appendChild(style);
		$('.main_logout_button').css({
            'width' : '620px',
        });
		$('.logout_button_confirm').css({
            'width' : '270px',
        });
	}
	
	/********************************************************************************
	 * [機能]	メニュー画面初期処理の「ステップ①未承認文書件数の確認」を行う
	 * [引数]	無し
	 * [戻値]	無し
	 ********************************************************************************/
	function set_labels(){
		// 共通関数「cmnSessionCheck（pSession＝エンプティーレコード）」にてセッションの有無を確認
		pSession['userId']="";
		pSession['userCode']="";
		pSession['userName']="";
		pSession['userKana']="";
		pSession['userMail']="";
		pSession['userAdmin']="";
		pSession['groupPath']="";
		pSession['approveDocument']="";
		pSession['isPopupExists']="";
		pSession['viceApprove']="";
		pSession['systemSecurity']="";
		pSession['textAttrMaxChars']="";
		pSession['language']="";
		
		// check
		f.cmnSessionCheck(pSession);
		
		// ラベルを設定
		if (cmnGetEnvironment=="ドキュメント管理")
		{
			pmain_scr_title.empty().append(cmnLanguage['LAN_'+pSession['language'].substring(0,2)+'_00001']);
		}
		else 
		{
			pmain_scr_title.empty().append(cmnLanguage['LAN_'+pSession['language'].substring(0,2)+'_00002']);
		};
		pmain_scr_folder_list_text.empty().append(cmnLanguage['LAN_'+pSession['language'].substring(0,2)+'_00007']);
		pmain_scr_unapproved_text.empty().append(cmnLanguage['LAN_'+pSession['language'].substring(0,2)+'_00008']);
		pmain_scr_favorites_text.empty().append(cmnLanguage['LAN_'+pSession['language'].substring(0,2)+'_00009']);
		pmain_scr_userName.empty().append(pSession['userName']);
		pmain_scr_btn_Logout_l.empty().append(cmnLanguage['LAN_'+pSession['language'].substring(0,2)+'_00010']);
		pmain_scr_logout_text.empty().append(cmnLanguage['LAN_'+pSession['language'].substring(0,2)+'_00011']);
		
		// 未承認文書情報の取得
		model.post_dspApproveDocumentList();
	}
	
	/**
	 * getParameterテンプレート
	 */
	function getParameter () {
		;
	}
	
	/**
	 * setParameterテンプレート
	 */
	function setParameter (data) {
		;
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
