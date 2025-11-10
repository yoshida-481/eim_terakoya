/**
 * login_scr_Controller
 */
var login_scr_Controller = function () {
	/*****************************************************************
	 * 宣言
	 ****************************************************************/
	function F() {};
	F.prototype = prototypeController;
	var f = new F();
	
	// ビュー
	const viewName = "login_scr_View";
	const viewId = "#" + viewName;
	var view = null;
	// モデル
	const model = login_scr_Model;
		
	// コンポーネント
	var pLogin_scr_title = null;
	var pLogin_scr_ID = null;
	var pLogin_scr_ID_label = null;
	var pLogin_scr_Password = null;
	var pLogin_scr_Password_label = null;
	var pLogin_scr_Language = null;
	var pLogin_scr_Language_label = null;
	var plogin_scr_title_image = null;
	var plogin_scr_slashBackground = null;
	var pLogin_scr_btn_Login = null;
	
	// 変数
	var pLogin_scr_btn_Login_l = null;
	var ppage_init = false;
	var pcalling_cookie_jsp = false;
	
	// 「dspSession.jsp」のJSPコールの内容をバインドする。そして、言語コードを保存する。
	var pSession = new Object();
	
	// 「dspLanguageXML.jsp」のJSPコールの内容をバインドする。
	var pLanguage = new Object();
	var pfirst_init = true;
	
	
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
		plogin_scr_title_image = $(viewId+'-'+'login_scr_title_image');
		pLogin_scr_title = $(viewId+'-'+'Login_scr_title');
		plogin_scr_slashBackground = $(viewId+'-'+'login_scr_slashBackground');
		pLogin_scr_ID = $(viewId+'-'+'Login_scr_ID');
		pLogin_scr_ID_label = $(viewId+'-'+'Login_scr_ID_label');
		pLogin_scr_Password = $(viewId+'-'+'Login_scr_Password');
		pLogin_scr_Password_label = $(viewId+'-'+'Login_scr_Password_label');
		pLogin_scr_Language = $(viewId+'-'+'Login_scr_Language');
		pLogin_scr_Language_label = $(viewId+'-'+'Login_scr_Language_label');
		pLogin_scr_btn_Login = $(viewId+'-'+'login_scr_btn_Login');
		pLogin_scr_btn_Login_l = $(viewId+'-'+'login_scr_btn_Login_l');
		// ログインボタンをＤＩＳＡＢＬＥ
		pLogin_scr_btn_Login.addClass('ui-disabled');
		
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
			
			if ((navigator.userAgent.indexOf("iPad")>0)&&((navigator.userAgent.indexOf("OS 8")>0)||(navigator.userAgent.indexOf("OS 7")>0)))
			{
				$("#login_scr_View-filler").empty().append("<br><br><br><br><br><br>");
				$("#login_scr_View-filler2").empty().append("<br>");
				$(".login_scr_View-rotate").css("width","100%");
				$("#login_scr_View-table_whole").height($(document.body).height());
				var style1 = document.createElement('style');
				style1.type = 'text/css';
				style1.innerHTML = '.ui-icon-login_scr_btn_Login {margin-left: 79px !important;}';
				document.getElementsByTagName('head')[0].appendChild(style1);
				$('.main_login_button').css({
					'width' : '404px',
		        });
			}
			else if (navigator.userAgent.indexOf("iPad")>0)
			{
				$("#login_scr_View-filler").empty().append("<br><br><br><br><br><br>");
				$("#login_scr_View-filler2").empty().append("<br>");
				$(".login_scr_View-rotate").css("width","100%");
				$("#login_scr_View-table_whole").height($(document.body).height());
				var style1 = document.createElement('style');
				style1.type = 'text/css';
				style1.innerHTML = '.ui-icon-login_scr_btn_Login {margin-left: 79px !important;}';
				document.getElementsByTagName('head')[0].appendChild(style1);
				$('.main_login_button').css({
					'width' : '404px',
		        });
			}
			else if (navigator.userAgent.indexOf("iPhone")>0)
			{
				$("#login_scr_View-filler").empty().append("<br><br><br><br><br><br>");
				$("#login_scr_View-filler2").empty().append("<br>");
				$(".login_scr_View-rotate").css("width","100%");
				$("#login_scr_View-table_whole").height($(document.body).height());
				var style1 = document.createElement('style');
				style1.type = 'text/css';
				style1.innerHTML = '.ui-icon-login_scr_btn_Login {margin-left: 79px !important;}';
				document.getElementsByTagName('head')[0].appendChild(style1);
				$('.main_login_button').css({
					'width' : '550px',
		        });
			}
			else
			{
				$("#login_scr_View-filler").empty().append("<br><br><br><br><br><br>");
				$("#login_scr_View-filler2").empty().append("<br>");
				$(".login_scr_View-rotate").css("width","100%");
				$("#login_scr_View-table_whole").height($(document.body).height());
				var style1 = document.createElement('style');
				style1.type = 'text/css';
				style1.innerHTML = '.ui-icon-login_scr_btn_Login {margin-left: 79px !important;}';
				document.getElementsByTagName('head')[0].appendChild(style1);
				$('.main_login_button').css({
					'width' : '404px',
		        });
			};
		});
		
		/**
		 * 「orientationChange_Landscape」イベントハンドラ
		 * 横向きになった場合
		 * このイベントハンドラは全ての画面の回転イベントで呼び出されてしまう。
		 * 現在ページの判定処理を入れて対応する
		 */
		$(this).on('orientationChange_Landscape', function(e) {
			
			if ((navigator.userAgent.indexOf("iPad")>0)&&((navigator.userAgent.indexOf("OS 8")>0)||(navigator.userAgent.indexOf("OS 7")>0)))
			{
				$("#login_scr_View-filler").empty().append("");
				$("#login_scr_View-filler2").empty().append("");
				$(".login_scr_View-rotate").css("width","70%");
				$("#login_scr_View-table_whole").height($(document.body).height());
				var style1 = document.createElement('style');
				style1.type = 'text/css';
				style1.innerHTML = '.ui-icon-login_scr_btn_Login {margin-left: 69px !important;}';
				document.getElementsByTagName('head')[0].appendChild(style1);
				$('.main_login_button').css({
					'width' : '367px',
		        });
			}
			else if (navigator.userAgent.indexOf("iPad")>0)
			{
				$("#login_scr_View-filler").empty().append("");
				$("#login_scr_View-filler2").empty().append("");
				$(".login_scr_View-rotate").css("width","70%");
				$("#login_scr_View-table_whole").height($(document.body).height());
				var style1 = document.createElement('style');
				style1.type = 'text/css';
				style1.innerHTML = '.ui-icon-login_scr_btn_Login {margin-left: 69px !important;}';
				document.getElementsByTagName('head')[0].appendChild(style1);
				$('.main_login_button').css({
					'width' : '367px',
		        });
			}
			else if (navigator.userAgent.indexOf("iPhone")>0)
			{
				$("#login_scr_View-filler").empty().append("");
				$("#login_scr_View-filler2").empty().append("");
				$(".login_scr_View-rotate").css("width","70%");
				$("#login_scr_View-table_whole").height($(document.body).height());
				var style1 = document.createElement('style');
				style1.type = 'text/css';
				style1.innerHTML = '.ui-icon-login_scr_btn_Login {margin-left: 69px !important;}';
				document.getElementsByTagName('head')[0].appendChild(style1);
				$('.main_login_button').css({
					'width' : '347px',
		        });
			}
			else
			{
				$("#login_scr_View-filler").empty().append("");
				$("#login_scr_View-filler2").empty().append("");
				$(".login_scr_View-rotate").css("width","70%");
				$("#login_scr_View-table_whole").height($(document.body).height());
				var style1 = document.createElement('style');
				style1.type = 'text/css';
				style1.innerHTML = '.ui-icon-login_scr_btn_Login {margin-left: 69px !important;}';
				document.getElementsByTagName('head')[0].appendChild(style1);
				$('.main_login_button').css({
					'width' : '367px',
		        });
			};
		});

		/**
		 * Loginボタンクリックイベントハンドラ
		 */
		jQuery(ev.target).on('vclick', pLogin_scr_btn_Login.selector, function() {
			model.post_login(pLogin_scr_ID.val(),pLogin_scr_Password.val(),pLogin_scr_Language.val());
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
		 * 「get_dspLanguageXMLSuccess」イベントハンドラ。言語情報の取得
		 */
		view.on('get_dspLanguageXMLSuccess', function(e, xml) {
			// バインドエラー管理
			var f_bind_error = false;
			var f_bind_error0 = true;
			var f_bind_error1 = true;
			var f_bind_error2 = true;
			var f_bind_error3 = true;
			var f_bind_error4 = true;
			
			// 変数にXMLの情報をバインド
			$(xml).find("language").each(function(){
				
				var fcurrent_id = $(this).attr('id');
				var fcurrent_locale = "";
				var fcurrent_label = "";
				
				// バインドエラー管理
				f_bind_error1 = false;
				
				// idタブ = dataタブ
				pLanguage[fcurrent_id] = new Object();
				// dataタブ
				pLanguage[fcurrent_id][fcurrent_id]= new Object();
				
				$(this).find("label").each(function(){
					fcurrent_label = $(this).text();
					// バインドエラー管理
					f_bind_error0 = false;
					// localeタブ
					pLanguage[fcurrent_id][fcurrent_id][fcurrent_label]= new Object();
				});
				
				$(this).find("locale").each(function(){
					fcurrent_locale = $(this).text();
					// バインドエラー管理
					f_bind_error2 = false;
					// localeタブ
					pLanguage[fcurrent_id][fcurrent_id][fcurrent_label][fcurrent_locale]= new Object();
				});
				
				$(this).find("item").each(function(){
					
					var fcurrent_lang = "";
					var fcurrent_name = "";
					
					$(this).find("lang").each(function(){
						fcurrent_lang=$(this).text();
						// バインドエラー管理
						f_bind_error3 = false;
						// langタブ
						pLanguage[fcurrent_id][fcurrent_id][fcurrent_label][fcurrent_locale][fcurrent_lang]= new Object();
					});
					$(this).find("name").each(function(){
						// バインドエラー管理
						f_bind_error4 = false;
						fcurrent_name=$(this).text();
						// nameタブ
						pLanguage[fcurrent_id][fcurrent_id][fcurrent_label][fcurrent_locale][fcurrent_lang][fcurrent_name]=new Object();
					});
				});
			});
			// バインドエラー管理
			f_bind_error = f_bind_error0 && f_bind_error1 && f_bind_error2&& f_bind_error3&& f_bind_error4; 
			if (f_bind_error)
			{
				// Sorryページにリダイレクト
				window.location.href="../html/server_down.html";
			}
			else 
			{
				// 端末の言語クッキーの取得
				model.get_actCookie();
			};
		});
		
		/**
		 * 「get_dspLanguageXMLError」イベントハンドラ
		 */
		view.on('get_dspLanguageXMLError', function(e, xml) {;
			// エラーメッセージを表示する。
			$(xml).find("error").each(function(){
				f.cmnPopup($(this).attr('message'),"ERR",viewName);
			});
			// Sorryページにリダイレクト
			window.location.href="../html/server_down.html";
		});				

		/**
		 * 「get_actCookieSuccess」イベントハンドラ
		 */
		view.on('get_actCookieSuccess', function(e, xml) {
			// 変数にXMLの情報をバインド
			var fCookieLanguage = "";
			
			if (pcalling_cookie_jsp == false)
			{
				pcalling_cookie_jsp = true;
			}
			else 
			{
				// 何もしない
				return;
			}
			
			$(xml).find("user").each(function(){
				// 変数にXMLの情報をバインド（’ＪＡ’、’ＥＮ’）
				pSession['userId'] = $(this).attr('userId');
				fCookieLanguage  = $(this).attr('langId');
			});
			
			// サーバー側の言語クーキーがエンプティーの場合、”ＪＡ”を設定
			if (fCookieLanguage == "")
			{
				// ブラウザークーキーを使う
				fCookieLanguage = JSON.parse(window.localStorage.getItem(EIM_KEY_LANGUAGE));
				if (fCookieLanguage==null)
				{
					fCookieLanguage = "JA";
				}
				else 
				{
					fCookieLanguage = fCookieLanguage.substring(0,2);
					pfirst_init = false;
				};
			}
			else 
			{
				pfirst_init = false;
			}
			
			// ブラウザーに設定されている言語を取得する。
			// ja＝日本語 en＝英語
			var fBrowserLanguage = navigator.language || navigator.userLanguage;
			fBrowserLanguage =  fBrowserLanguage.toUpperCase(); 
			
			// サーバー側の言語が存在ある場合
			fBrowserLanguage = fCookieLanguage;
			
			var f_language_found = false;
			var f_first_key = "";
			for (var key in pLanguage){
				if (f_first_key == "") 
				{ 
					f_first_key = key;
				}
				if (key ==  fBrowserLanguage)
				{
					// [fBrowserLanguage]が取得したリストに存在する場合
					f_language_found = true;
					break;
				};
			}
			
			if (!f_language_found)
			{
				// [fBrowserLanguage]が取得したリストに存在しない場合
				fBrowserLanguage = f_first_key;
			}
			
			// 言語一覧に「日本語」、「英語」を表示する。デフォルト：「日本語」
			pLogin_scr_Language.empty();

			if (pfirst_init == true)
			{
				for (var key_label in pLanguage[fBrowserLanguage][fBrowserLanguage]){
					for (var key_locale in pLanguage[fBrowserLanguage][fBrowserLanguage][key_label]){
						for (var key_lang in pLanguage[fBrowserLanguage][fBrowserLanguage][key_label][key_locale]){
							for (var key_name in pLanguage[fBrowserLanguage][fBrowserLanguage][key_label][key_locale][key_lang]){
								// 言語一覧に「日本語」、「英語」を表示する。デフォルト：「日本語」
								pLogin_scr_Language.append('<option value=' + key_lang + "@" + key_locale + '>' + key_name + '</option>');
								pfirst_init = false;
							};
						};
					};
				}
				
			}
			else 
			{
				for (var key_id1 in pLanguage){
					for (var key_id2 in pLanguage[key_id1]){
						for (var key_label in pLanguage[key_id1][key_id2]){
							// 言語一覧にＸＭＬ「label」タグの内容を表示する。「Japanese」、「English」。デフォルト：ユーザーのクーキー
							if (key_label=="Japanese")
							{
								if (fBrowserLanguage=="JA")
								{
									pLogin_scr_Language.append('<option selected value="JA@ja_JP">' + key_label + '</option>');	
								}
								else 
								{
									pLogin_scr_Language.append('<option value="JA@ja_JP">' + key_label + '</option>');
								};
							}
							else 
							{
								if (fBrowserLanguage=="EN")
								{
									pLogin_scr_Language.append('<option selected value="EN@ja_JP">' + key_label + '</option>');	
								}
								else 
								{
									pLogin_scr_Language.append('<option value="EN@ja_JP">' + key_label + '</option>');
								};
							};
						};
					};
				};
			};
			
			
			// リフレッシュ
			pLogin_scr_Language.selectmenu("refresh", true);
			
			// ラベル
			if (cmnGetEnvironment=="ドキュメント管理")
			{
				pLogin_scr_title.empty().append(cmnLanguage['LAN_'+fBrowserLanguage+'_00001']);
			}
			else 
			{
				pLogin_scr_title.empty().append(cmnLanguage['LAN_'+fBrowserLanguage+'_00002']);
			};
			
			pLogin_scr_ID_label.empty().append(cmnLanguage['LAN_'+fBrowserLanguage+'_00003']);
			pLogin_scr_Password_label.empty().append(cmnLanguage['LAN_'+fBrowserLanguage+'_00004']);
			pLogin_scr_Language_label.empty().append(cmnLanguage['LAN_'+fBrowserLanguage+'_00005']);
			pLogin_scr_btn_Login_l.empty().append(cmnLanguage['LAN_'+fBrowserLanguage+'_00006']);
			
			// イメージ・アイコン
			plogin_scr_title_image.empty().append(cmnGetimg['login_scr_title_image']);
			plogin_scr_slashBackground.empty().append(cmnGetimg['login_scr_slashBackground']);
			document.getElementById("login_scr_View-login_scr_btn_Login").style.background='-webkit-linear-gradient(top, #00D0E5 0%, #00D0E5 100%)';
			pLogin_scr_btn_Login.children().children().css('font-size','45px');
			
			// 画面の縦/横の初期設定
			if ($(document.body).height()>800)
			{
				$(this).trigger('orientationChange_Portrait');
			}
			else
			{
				$(this).trigger('orientationChange_Landscape');
			};
			
			// ログインボタンをＥＮＡＢＬＥ
			pLogin_scr_btn_Login.removeClass('ui-disabled');
		});
		
		/**
		 * 「get_actCookieError」イベントハンドラ
		 */
		view.on('get_actCookieError', function(e, xml) {
			// エラーメッセージを表示する。
			$(xml).find("error").each(function(){
				f.cmnPopup($(this).attr('message'),"ERR",viewName);
			});
		});				

		/**
		 * 「post_loginSuccess」イベントハンドラ
		 */
		view.on('post_loginSuccess', function(e, xml) {
			$(xml).find("user").each(function(){
			    // 変数にXMLの情報をバインド
				pSession['userId'] = $(this).attr('userId');
				pSession['userCode'] = $(this).attr('userCode');
				pSession['userName'] = $(this).attr('userName');
				pSession['userKana'] = $(this).attr('userKana');
				pSession['userMail'] = $(this).attr('userMail');
				
				// メモリクリーン
				f.cmnSessionCheck(null);
				// 新しいセッションを登録
				f.cmnSessionCheck(pSession);
				
				// セッション情報の取得
				model.get_dspSession(); 
			});
		});
		
		/**
		 * 「post_loginError」イベントハンドラ
		 */
		view.on('post_loginError', function(e, xml) {
			// エラーメッセージを表示する。
			$(xml).find("error").each(function(){
				f.cmnPopup($(this).attr('message'),"ERR",viewName);
			});
		});				

		/**
		 * 「get_dspSessionSuccess」イベントハンドラ
		 */
		view.on('get_dspSessionSuccess', function(e, xml) {
			//insert_maintenance_values(JSON.parse(data));
			$(xml).find("user").each(function(){
			    // 変数にXMLの情報をバインド
				pSession['userId'] = $(this).attr('userId');
				pSession['userCode'] = $(this).attr('userCode');
				pSession['userName'] = $(this).attr('userName');
				pSession['userKana'] = $(this).attr('userKana');
				pSession['userMail'] = $(this).attr('userMail');
				pSession['userAdmin']= $(this).attr('userAdmin');
				pSession['groupPath']= $(this).attr('groupPath');
				pSession['approveDocument']= $(this).attr('approveDocument');
				pSession['isPopupExists']= $(this).attr('isPopupExists');
				pSession['viceApprove']= $(this).attr('viceApprove');
				pSession['systemSecurity']= $(this).attr('systemSecurity');
				pSession['textAttrMaxChars']= $(this).attr('textAttrMaxChars');
				
				// there is something wrong maybe check
				pSession['language']= pLogin_scr_Language.val();
				
				// メモリクリーン
				f.cmnSessionCheck(null);
				// セッションクッキーを登録
				f.cmnSessionCheck(pSession);
				
				// ユーザーとパスワードのコントローラーをクリーン
				pLogin_scr_ID.val("");
				pLogin_scr_Password.val("");
				
				// ログインボタンをＤＩＳＡＢＬＥ
				pLogin_scr_btn_Login.addClass('ui-disabled');

				// メイン画面の遷移
				$.mobile.changePage(main_scr_Controller.getViewId(), {'transition' : 'fade', });
			});
		});
		
		/**
		 * 「get_dspSessionError」イベントハンドラ
		 */
		view.on('get_dspSessionError', function(e, xml) {
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
		
		f.cmnSessionCheck(pSession);
		
		if ((pSession['userId']!=null) && (pSession['userCode']!=null) && 
			(pSession['userName']!=null) && (pSession['userKana']!=null) && 
			(pSession['userMail']!=null) && (pSession['userAdmin']!=null) && 
			(pSession['groupPath']!=null) && (pSession['approveDocument']!=null) && 
			(pSession['isPopupExists']!=null) && (pSession['viceApprove']!=null) && 
			(pSession['systemSecurity']!=null) && (pSession['textAttrMaxChars']!=null) && 
			(pSession['language']!=null))
		{
			if ((pSession['userId']!="") && (pSession['userCode']!="") && 
				(pSession['userName']!="") && (pSession['userKana']!="") && 
				(pSession['userMail']!="") && (pSession['userAdmin']!="") && 
				(pSession['groupPath']!="") && (pSession['approveDocument']!="") && 
				(pSession['isPopupExists']!="") && (pSession['viceApprove']!="") && 
				(pSession['systemSecurity']!="") && (pSession['textAttrMaxChars']!="") && 
				(pSession['language']!=""))
			{
				// 返却値：true、メインコントローラーにセッション情報をセット、メイン画面の遷移
				$.mobile.changePage(main_scr_Controller.getViewId(), {'transition' : 'fade', });
			}	
		}
		ppage_init = true;
		// 返却値：false。言語情報の取得
		model.get_dspLanguageXML();
	});
	
	/**
	 * 「pagebeforeshow」イベントハンドラ
	 */
	jQuery(document).on('pagebeforeshow', viewId, function(ev) {
		pcurrentView = viewName;
		
		// ログインボタンをＥＮＡＢＬＥ
		pLogin_scr_btn_Login.removeClass('ui-disabled');
		
		if (ppage_init != true)
		{
			pcalling_cookie_jsp = false;
			
			// 端末の言語クッキーの取得
			model.get_actCookie();
		}
		else {
			ppage_init = false;
		}
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
