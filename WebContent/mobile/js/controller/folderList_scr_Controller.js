/**
 * folderList_scr_Controller
 */
var folderList_scr_Controller = function () {
	/*****************************************************************
	 * 宣言
	 ****************************************************************/
	function F() {};
	F.prototype = prototypeController;
	var f = new F();
	
	// ビュー
	const viewName = "folderList_scr_View";
	const viewId = "#" + viewName;
	var view = null;
	// モデル
	const model = folderList_scr_Model;
	
	// コンポーネント
	// コメントしているコンポーネントはダイナミックＨＴＭＬになります。
	var pfolderList_scr_search_keyword = "";
	var pfolderList_scr_order_text = null;
	var pfolderList_scr_order_combo = null;
	var pfolderList_scr_list_item = null;
	// folderList_scr_workspace_info → ダイナミックHTML「list_src_workspace_info」になります。
	var pfolderList_scr_breadcrumb = null;
	var pfolderList_scr_back_list = null;
	// folderList_scr_search_results → ダイナミックなHTML「list_src_workspace_info」になります。
	// folderList_scr_document_item → ダイナミックなHTML「list_src_workspace_info」になります。
	// folderList_scr_document_info → ダイナミックなHTML「list_src_workspace_info」になります。
	// folderList_scr_properties_text → ダイナミックなHTML「list_scr_btn_Properties_[i]」になります。 
	
	// 変数
	var pNodes = new Object();
	var pCurrentFolder = new Object();
	var pCurrentFolderSearchBackup = "";
	var planguage = null;
	var pbreadcrumb = new Object();
	var pbreadcrumb_names = new Object();
	var pfolderList_scr_workspace_infoId = "";
	var plist_scr_btn_Search = null;
	var pfolderName = "";
	var pfolderId = "";
	var psearch = false;
	var pcoming_from_favorites = false;
	var pwas_initialized = false;
	var pis_favorite_node = false;
	var pdownloaded_document = "";
	// 属性フラッグ ０＝確認なし、１＝確認あり、
	var pchecking_before_properties = 0;
	var pchecking_before_properties_element_type = "";
	var presults_counter = 0;
	var psending_email = false;
	
	
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
		pfolderList_scr_list_item = $(viewId+'-'+'folderList_scr_list_item');
		pfolderList_scr_breadcrumb = $(viewId+'-'+'folderList_scr_breadcrumb');
		pfolderList_scr_order_text = $(viewId+'-'+'folderList_scr_order_text');
		pfolderList_scr_order_combo = $(viewId+'-'+'folderList_scr_order_combo');
		plist_scr_btn_Search = $(viewId+'-'+'list_scr_btn_Search');
		pfolderList_scr_search_keyword = $(viewId+'-'+'folderList_scr_search_keyword');
		pfolderList_scr_back_list = $(viewId+'-'+'folderList_scr_back_list');
		pfolderList_scr_list_item.empty();
		
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
			//$('#folderList_scr_View-divfiller').hide();
			resize_screen_portrait();
		});
		
		/**
		 * 「orientationChange_Landscape」イベントハンドラ
		 * 横向きになった場合
		 * このイベントハンドラは全ての画面の回転イベントで呼び出されてしまう。
		 * 現在ページの判定処理を入れて対応する
		 */
		$(this).on('orientationChange_Landscape', function(e) {
			//$('#folderList_scr_View-divfiller').hide();
			resize_screen_landscape();
		});
		
		/**
		 * パンくずボタンクリッククリックイベントハンドラ
		 */
		jQuery(ev.target).on('click', $('.folderList_scr_breadcrumb').selector, function() {
			//ワークスペース／フォルダ／ドキュメント情報の取得
			var fobjId = this.id;
			var fbreadcrumb_position = 0;
			var fbreadcrumb_length = 0;
			
			// パンクずが長い場合、「＞...＞」を表示する
			if (fobjId=="...") 
			{
				// 何もしない
				return;
			}
			
			// 検索結果の場合、リセット
			psearch = false;
			// クーキーをアップデート
			window.localStorage.setItem(EIM_KEY_CURRENT_LIST_SEARCH, JSON.stringify(psearch));
			if (pfolderList_scr_search_keyword.val() != null){
				window.localStorage.setItem(EIM_KEY_CURRENT_LIST_SEARCH_KEYWORD, JSON.stringify(pfolderList_scr_search_keyword.val()));
			};
			
			if (fobjId == "Ｔｏｐ")
			{
				fobjId = 0;
			}
			
			// パンくず変数をアップデート
			for (var key in pbreadcrumb){
				fbreadcrumb_length = fbreadcrumb_length + 1;
				if (pbreadcrumb[key]==pbreadcrumb[fobjId])
				{
					// will remove all under this point
					fbreadcrumb_position = key;
				};
			}
			
			// 現在のフォルダのパンくずボタンクリックの場合、何もしない
			if (fobjId != (fbreadcrumb_length-1)) 
			{
				for (var i = fbreadcrumb_length-1; i>fbreadcrumb_position; i--){
					delete pbreadcrumb[i];
					delete pbreadcrumb_names[i];	
				};
			}
			
			// クーキーをリセット
			window.localStorage.setItem(EIM_KEY_CURRENT_LIST_ORDER, JSON.stringify(''));

			// 情報の取得
			if (fobjId == 0)
			{	
				// ワークスペース情報の取得
				model.get_dspFolderTree();
			}
			else
			{
				pfolderList_scr_workspace_infoId = pbreadcrumb[fobjId];
				// クーキーをアップデート
				window.localStorage.setItem(EIM_KEY_CURRENT_LIST_FOLDERID, JSON.stringify(pfolderList_scr_workspace_infoId));
				model.post_dspChildObject (pfolderList_scr_workspace_infoId);
			};
		});
		
		/**
		 * ワークスペースクリックイベントハンドラ
		 */
		jQuery(ev.target).on('click', $('.list_src_workspace_info').selector, function() {
			//フォルダ／ドキュメント情報の取得
			pfolderList_scr_workspace_infoId = (this.id).substring(24);
			// クーキーをアップデート
			window.localStorage.setItem(EIM_KEY_CURRENT_LIST_FOLDERID, JSON.stringify(pfolderList_scr_workspace_infoId));
			
			// クーキーをリセット
			window.localStorage.setItem(EIM_KEY_CURRENT_LIST_ORDER, JSON.stringify(''));

			model.post_dspChildObject (pfolderList_scr_workspace_infoId)
		});
		
		/**
		 * ワークスペースアイコンクリックイベントハンドラ
		 */
		jQuery(ev.target).on('click', $('.list_src_workspace_icon').selector, function() {
			//フォルダ／ドキュメント情報の取得
			pfolderList_scr_workspace_infoId = (this.id).substring(24);
			// クーキーをアップデート
			window.localStorage.setItem(EIM_KEY_CURRENT_LIST_FOLDERID, JSON.stringify(pfolderList_scr_workspace_infoId));
			
			// クーキーをリセット
			window.localStorage.setItem(EIM_KEY_CURRENT_LIST_ORDER, JSON.stringify(''));

			model.post_dspChildObject (pfolderList_scr_workspace_infoId)
		});
		
		/**
		 * 一階層上を表示ボタンクリック
		 */
		jQuery(ev.target).on('click', $('.list_src_folder_back_icon').selector, function() {
			//フォルダ／ドキュメント情報の取得
			var fbreadcrumb_lenght = 0;
			
			// パンくず変数をアップデート
			fbreadcrumb_lenght = calculate_breadcrumb_lenght();
			
			delete pbreadcrumb[fbreadcrumb_lenght-1];
			delete pbreadcrumb_names[fbreadcrumb_lenght-1];
			
			// クーキーをリセット
			window.localStorage.setItem(EIM_KEY_CURRENT_LIST_ORDER, JSON.stringify(''));

			if (fbreadcrumb_lenght==2)
			{
				// ワークスペース情報の取得
				model.get_dspFolderTree();
			}
			else
			{
				// フォルダ情報の取得
				pfolderList_scr_workspace_infoId = pbreadcrumb[fbreadcrumb_lenght-2];
				// クーキーをアップデート
				window.localStorage.setItem(EIM_KEY_CURRENT_LIST_FOLDERID, JSON.stringify(pfolderList_scr_workspace_infoId));
				
				// ファイル/フォルダ情報の取得
				model.post_dspChildObject (pfolderList_scr_workspace_infoId); 
			};
		});
		
		/**
		 * トップへ戻るボタンクリックイベントハンドラ
		 */
		jQuery(ev.target).on('click', $('#folderList_scr_View-list_scr_top_icon_').selector, function() {
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
		 * 検索ボタンクリックイベントハンドラ
		 */
		jQuery(ev.target).on('click', plist_scr_btn_Search.selector, function() {
			var ffolderList_scr_search_keyword;
			
			if (plist_scr_btn_Search.data('disabled')!=null)
			{
				if (plist_scr_btn_Search.data('disabled')==true)
				{
					// まだ前の検索した結果を表示しない場合、何もしない
					return;
				};
			};
			
			// 変数にfolderList_scr_View情報をバインド
			ffolderList_scr_search_keyword = pfolderList_scr_search_keyword.val();
			
			// ボタンをＤＩＳＡＢＬＥする
			plist_scr_btn_Search.data('disabled',true);
			
			// クーキーをリセット
			window.localStorage.setItem(EIM_KEY_CURRENT_LIST_ORDER, JSON.stringify(''));

			//モデルメソッドの呼び出し
			model.post_actSearch(ffolderList_scr_search_keyword, build_path());
		});
		
		/**
		 * ファイルをクリックイベントハンドラ
		 */
		jQuery(ev.target).on('click', $('.post_actSendReplyMail').selector, function() {
			var ffolderList_scr_ObjId = "";
			
			// 変数にfolderList_scr_View情報をバインド
			ffolderList_scr_ObjId = $(this).attr('id');
			ffolderList_scr_ObjId = ffolderList_scr_ObjId.substring(24);
			
			//モデルメソッドの呼び出し
			pdownloaded_document = ffolderList_scr_ObjId;
			
			if (psending_email == false)
			{
				psending_email = true;
				model.post_actSendReplyMail(ffolderList_scr_ObjId);
			};
		});
		
		/**
		 * 一覧に戻るボタンクリックイベントハンドラ
		 */
		jQuery(ev.target).on('click', $('#folderList_scr_back_list_link').selector, function() {
			var fbreadcrumb_lenght;
			var fCurrentFolderSearchBackup;
			
			// パンくず変数をアップデート
			fbreadcrumb_lenght = 0;
			fbreadcrumb_lenght = calculate_breadcrumb_lenght();
			
			psearch = false;
			// クーキーをアップデート
			window.localStorage.setItem(EIM_KEY_CURRENT_LIST_SEARCH, JSON.stringify(""));
			if (pfolderList_scr_search_keyword.val() != null){
				window.localStorage.setItem(EIM_KEY_CURRENT_LIST_SEARCH_KEYWORD, JSON.stringify(""));
			};
			pfolderList_scr_search_keyword.val("");
			
			// クーキーをリセット
			window.localStorage.setItem(EIM_KEY_CURRENT_LIST_ORDER, JSON.stringify(''));

			if (fbreadcrumb_lenght == 1)
			{
				// ワークスペース情報の取得
				model.get_dspFolderTree();
			}
			else
			{
				fCurrentFolderSearchBackup = pCurrentFolderSearchBackup;
				// 変数をリセット
				pCurrentFolderSearchBackup = "";
				// フォルダ情報の取得
				model.post_dspChildObject (fCurrentFolderSearchBackup); 
			};
		});
		
		/**
		 * 属性ボタンクリックイベントハンドラ
		 */
		jQuery(ev.target).on('click', $('.list_scr_btn_Properties').selector, function() {
			var ffolderList_scr_ObjId = "";
			//フォルダ／ドキュメント情報の取得
			var fbreadcrumb_lenght = 0;
			
			// パンくず変数をアップデート
			fbreadcrumb_lenght = calculate_breadcrumb_lenght();
			
			// フォルダ一覧クリーン
			pfolderList_scr_list_item.empty();
			ffolderList_scr_ObjId = $(this).attr('id');
			ffolderList_scr_ObjId = ffolderList_scr_ObjId.substring(24);	

			// クーキーを登録
			window.localStorage.setItem(EIM_KEY_CURRENT_PROPERTY_ELEMENT, JSON.stringify(ffolderList_scr_ObjId));
			
			// ファイルまだ存在する確認
			if (pchecking_before_properties == 0)
			{
				// 確認有り
				pchecking_before_properties = 1;
				
				if (psearch == true)
				{
					//モデルメソッドの呼び出し
					model.post_actSearch(pfolderList_scr_search_keyword.val(), build_path());
				}
				else if (fbreadcrumb_lenght == 1)
				{
					// ワークスペース情報の取得
					model.get_dspFolderTree();
					return;
				}
				else if (fbreadcrumb_lenght > 1)
				{
					pchecking_before_properties_element_type = pCurrentFolder[ffolderList_scr_ObjId]['objTypeName'];
					
					// リロードのフォルダ情報の取得
					pfolderList_scr_workspace_infoId = JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_LIST_FOLDERID));
					model.post_dspChildObject (pfolderList_scr_workspace_infoId);
					return;
				};
			};
		});
		
		/**
		 * 並び替えチェンジイベントハンドラ
		 */
		jQuery(ev.target).on('change', pfolderList_scr_order_combo.selector, function() {
			// コンポーネントに変数をバインド
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
		 * 「post_actSendReplyMailSuccess」イベントハンドラ
		 */
		view.on('post_actSendReplyMailSuccess', function(e, xml) {
			//ドキュメントの存在を確認
			//モデルメソッドの呼び出し
			psending_email = false;
			
			if (psearch == true)
			{
				model.post_actSearch(pfolderList_scr_search_keyword.val(), build_path());
			}
			else
			{
				model.post_dspChildObject (pfolderList_scr_workspace_infoId);	
			}
		});		
		
		/**
		 * 「post_actSendReplyMailError」イベントハンドラ
		 */
		view.on('post_actSendReplyMailError', function(e, xml) {
			psending_email = false;
			
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
		 * 「get_dspFolderTreeSuccess」イベントハンドラ
		 */
		view.on('get_dspFolderTreeSuccess', function(e, xml) {
			var fnodes_counter = 0;
			var fnOrder = 0;
			
			pNodes = new Object();
			// 変数にXMLの未承認件数情報をバインド
			$(xml).find("nodes").each(function(){
			    $(xml).find("node").each(function(){
			    	var fobjId ="";
			    	var flabel ="";
			    	var fobjTypeId ="";
			    	var fobjTypeName ="";
			    	var fisWorkflowFolder = false;
			    	var fisWorkSpace = false;
			    	var fisSearch = false;
			    	var ftagListKind ="";
			    	
			    	// objIdタブ
			    	fobjId = $(this).attr('objId');
			    	
			    	// isWorkSpaceタブ
				    fisWorkSpace = $(this).attr('isWorkSpace') == "true" ? true : false ;
				    
			    	if ((fobjId!="")&&(fisWorkSpace == true))
				    {
				    	// labelタブ
				    	flabel = $(this).attr('label');
				    	// objTypeIdタブ
				    	fobjTypeId = $(this).attr('objTypeId');
				    	// isWorkflowFolderタブ
				    	fisWorkflowFolder = $(this).attr('isWorkflowFolder') == "true" ? true : false ;
				    	// isSearchタブ
				    	fisSearch = $(this).attr('isSearch') == "true" ? true : false ;
				    	// tagListKindタブ
				    	ftagListKind = $(this).attr('tagListKind');
				    	// objTypeNameタブ
				    	fobjTypeName = $(this).attr('objTypeName');
				    
				    	pNodes[fobjId] = new Object();
						pNodes[fobjId]['label'] = flabel;
						pNodes[fobjId]['objTypeId'] = fobjTypeId;
						pNodes[fobjId]['objTypeName'] = fobjTypeName;
						pNodes[fobjId]['isWorkflowFolder'] = fisWorkflowFolder;
						pNodes[fobjId]['isWorkSpace'] = fisWorkSpace;
						pNodes[fobjId]['isSearch'] = fisSearch;
						pNodes[fobjId]['tagListKind'] = ftagListKind;
						pNodes[fobjId]['nOrder']=fnOrder;
						fnOrder = fnOrder + 1;
						
						// 属性フラッグの場合
						if (pchecking_before_properties == 1){
							if (JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_PROPERTY_ELEMENT)) != null)
							{
								// 存在する場合
								if (fobjId==JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_PROPERTY_ELEMENT))){
									pchecking_before_properties = 2;
								};	
							};
						};
						
						fnodes_counter = fnodes_counter + 1;
					};
				});
			});
			
			// 属性フラッグの場合
			if (pchecking_before_properties == 1)
			{
				// 存在しない場合
				// フラッグリセット
				pchecking_before_properties = 0;
				// クーキーをアップデート
				window.localStorage.setItem(EIM_KEY_CURRENT_ERROR_MESSAGE, JSON.stringify(cmnGetmsg['MSG_ERR_'+planguage+'_00007']));
				// パンクずクーキーをクリーン
				f.cmnBreadcrumbsCheck(null, null, null);
				// ワークスペースを表示する
				location.reload(true);
				return;
			}
			else if (pchecking_before_properties == 2)
			{
				// 存在する場合
				// フラッグリセット
				pchecking_before_properties = 0;
				// クーキーをアップデート
				window.localStorage.setItem(EIM_KEY_CURRENT_PROPERTY_OBJTYPENAME, JSON.stringify(pNodes[JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_PROPERTY_ELEMENT))]['objTypeName']));
				// 属性情報画面の遷移
				$.mobile.changePage(properties_scr_Controller.getViewId(), {'transition' : 'fade', });
				return;
			}
			
			// アクセス可能なワークスペースが見つかりません
			if (fnodes_counter == 0)
			{
				f.cmnPopup(cmnGetmsg['MSG_ERR_'+planguage+'_00009'],"ERR",viewName); 
			}
			
			// 一つずつ、ワークスペースの更新者と更新日を取得
			for (var i in pNodes){
				if (pNodes[i]['modifyUserName']==undefined){
					model.post_dspProperty(i);
				}
				break;
			};
		});
		
		/**
		 * 「get_dspFolderTreeError」イベントハンドラ
		 */
		view.on('get_dspFolderTreeError', function(e, xml) {
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
		 * 「post_dspPropertySuccess」イベントハンドラ
		 */
		view.on('post_dspPropertySuccess', function(e, xml) {
			var fproperties_loaded = true;
			// 並び替え
			var fstored_order = null;
			
			// 変数にXMLの未承認件数情報をバインド
			$(xml).find("object").each(function(){
			    var fobjId ="";
			    var fcreateUserName ="";
			    var fcreateDate ="";
			    var fproperty = "";
			    
			    // objIdタブ
			    fobjId = $(this).attr('objId');
			    // createUserNameタブ
			    fcreateUserName = $(this).attr('createUserName');
			    // createDateタブ
			    fcreateDate = $(this).attr('createDate');
			    // propertyタブ
		    	fproperty = $(this).attr('property');

				if (pNodes[fobjId]){
					pNodes[fobjId]['modifyUserName'] = fcreateUserName;
					pNodes[fobjId]['modifyDate'] = fcreateDate;
					pNodes[fobjId]['property'] = fproperty;
				};
			});
			
			// 次のワークスペースの更新者と更新日を取得
			for (var i in pNodes){
				if (pNodes[i]['modifyUserName']==undefined){
					fproperties_loaded = false;
					model.post_dspProperty(i);
					return;
				}
				
			};
			
			// 全体のワークスペースデーターを取得した場合、ワークスペース情報を表示
			if (fproperties_loaded){

				// 並び替え
				// クーキーを設定
				if (JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_LIST_ORDER)) != null)
				{
					fstored_order = JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_LIST_ORDER));
				}
				
				pfolderList_scr_order_text.empty().append(cmnLanguage['LAN_'+planguage+'_00023']);
				pfolderList_scr_order_combo.empty();
				pfolderList_scr_order_combo.append('<option value="'+cmnLanguage['LAN_'+planguage+"_00025"]+'">' + 
													cmnLanguage['LAN_'+planguage+"_00025"] + '</option>');
				pfolderList_scr_order_combo.append('<option value="'+cmnLanguage['LAN_'+planguage+"_00018"]+'">' + 
													cmnLanguage['LAN_'+planguage+"_00018"] + '</option>');
				pfolderList_scr_order_combo.append('<option value="'+cmnLanguage['LAN_'+planguage+"_00016"]+'"">' + 
													cmnLanguage['LAN_'+planguage+"_00016"] + '</option>');
				pfolderList_scr_order_combo.append('<option value="'+cmnLanguage['LAN_'+planguage+"_00017"]+'">' + 
													cmnLanguage['LAN_'+planguage+"_00017"] + '</option>');
				pfolderList_scr_order_combo.append('<option value="'+cmnLanguage['LAN_'+planguage+"_00021"]+'">' + 
						cmnLanguage['LAN_'+planguage+"_00021"] + '</option>');
				
				if (fstored_order!=null)
				{
					if (fstored_order!="")
					{
						$('#folderList_scr_View-folderList_scr_order_combo option').filter(function () { 
							return $(this).html() == cmnLanguage['LAN_'+planguage+fstored_order]; }).prop('selected', true)
					};
				}
				else {
					$('#folderList_scr_View-folderList_scr_order_combo option').filter(function () { 
							return $(this).html() == cmnLanguage['LAN_'+planguage+"_00025"]; }).prop('selected', true)
				}
				// リフレッシュ
				pfolderList_scr_order_combo.selectmenu("refresh", true);
			
				// コンポーネントに変数をバインド
				if ($(document.body).height()>800)
				{	resize_screen_portrait();
				}
				else 
				{
					resize_screen_landscape();
				};
			};
		});
		
		/**
		 * 「post_dspPropertyError」イベントハンドラ
		 */
		view.on('post_dspPropertyError', function(e, xml) {
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
		 * 「post_dspChildObjectSuccess」イベントハンドラ
		 */
		view.on('post_dspChildObjectSuccess', function(e, xml) {
			// 並び替え
			var fstored_order = null;
			var fisdeleted = false;
			var download_file_check = false;
			var fnOrder = 0;
			
			// 変数にXMLの未承認件数情報をバインド
			$(xml).find("objList").each(function(){
				
				var fbreadcrumb_lenght;
				var fpCurrentFolder_lenght;
				
				// 特別場合：フォルダは削除しました
				// pathタブ="/ごみ箱”になります
				if (($(this).attr('path')).indexOf("ごみ箱")>=0) 
				{
					fisdeleted = true;
				}
				
				// objIdタブ
				pfolderId = $(this).attr('objId');

				// クーキーをアップデート
				pfolderList_scr_workspace_infoId = pfolderId;
				
				// pCurrentFolderをリセット
				// パンくず変数をアップデート
				fbreadcrumb_lenght = 0;
				fbreadcrumb_lenght = calculate_breadcrumb_lenght();
				
				// パンくず変数をアップデート
				if (fbreadcrumb_lenght == 1){
					pbreadcrumb[fbreadcrumb_lenght]= pfolderId;
					pbreadcrumb_names[fbreadcrumb_lenght] = pNodes[pfolderId]['label'];
				}
				else {
					fpCurrentFolder_lenght = 0;
					for (var key in pCurrentFolder){
						fpCurrentFolder_lenght = fpCurrentFolder_lenght + 1;
					}
					if (fpCurrentFolder_lenght > 0)
					{
						// フォルダをクリック
						try {
							pfolderName = pCurrentFolder[pfolderId]['objName'];
						}
						catch (e){
							// リロードの場合
							pfolderName = pbreadcrumb_names[fbreadcrumb_lenght-1];
						};
					}
					else 
					{  
						// 一階層上フォルダをクリック
						pfolderName = pbreadcrumb_names[fbreadcrumb_lenght-1];
					};
				}
				pCurrentFolder=new Object();
				
				$(xml).find("object").each(function(){
					var fobjId = "";
					var fobjTypeId ="";
					var fobjTypeName ="";
					var fobjName ="";
					var fstatusTypeName ="";
					var fmodifyUserName ="";
					var fmodifyDate ="";
					var fproperty ="";
					var fisPublished = false;
					var fhistory ="";
					var freadOnly = false;
					
					// objIdタブ
					fobjId = $(this).attr('objId');
					// objTypeIdタブ
					fobjTypeId = $(this).attr('objTypeId');
					// objTypeNameタブ
					fobjTypeName = $(this).attr('objTypeName');
					// objNameタブ
					fobjName = $(this).attr('objName');
					// statusTypeNameタブ
					fstatusTypeName = $(this).attr('statusTypeName');
					// modifyUserNameタブ
					fmodifyUserName = $(this).attr('modifyUserName');
					// modifyDateタブ
					fmodifyDate = $(this).attr('modifyDate');
					// objNameタブ
					fproperty = $(this).attr('property');
					// isPublishedタブ
					fisPublished = $(this).attr('isPublished') == "true" ? true : false ;
					// revタブ
					fhistory = $(this).attr('rev');
					// readOnlyタブ
					freadOnly = $(this).attr('readOnly') == "true" ? true : false ;
					
					if (fobjTypeName != cmn_objTypeName['タグ'])
					{
						pCurrentFolder[fobjId]=new Object();
						pCurrentFolder[fobjId]['objTypeId']=fobjTypeId;
						pCurrentFolder[fobjId]['objTypeName']=fobjTypeName;
						pCurrentFolder[fobjId]['objName']=fobjName;
						pCurrentFolder[fobjId]['statusTypeName']=fstatusTypeName;
						pCurrentFolder[fobjId]['modifyUserName']=fmodifyUserName;
						pCurrentFolder[fobjId]['modifyDate']=fmodifyDate;
						pCurrentFolder[fobjId]['property']=fproperty;
						pCurrentFolder[fobjId]['isPublished']=fisPublished;
						pCurrentFolder[fobjId]['history']=fhistory;
						pCurrentFolder[fobjId]['readOnly']=freadOnly;
						pCurrentFolder[fobjId]['nOrder']=fnOrder;
						fnOrder = fnOrder + 1;
					}
					
					// ダウンロードファイルの場合、確認する
					if (pdownloaded_document != ""){
						if (pdownloaded_document == fobjId)
						{
							download_file_check = true;
						};
					};
					
					// 属性フラッグの場合
					if (pchecking_before_properties == 1){
						if (JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_PROPERTY_ELEMENT)) != null)
						{
							// 存在する場合
							if (fobjId==JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_PROPERTY_ELEMENT))){
								pchecking_before_properties = 2;
							};	
						};
					};
				});
			});
			
			// 属性フラッグの場合
			if (pchecking_before_properties == 1)
			{
				// 存在しない場合
				// クーキーをアップデート
				if (pchecking_before_properties_element_type == cmn_objTypeName['フォルダ'])
				{
					// フォルダ
					window.localStorage.setItem(EIM_KEY_CURRENT_ERROR_MESSAGE, JSON.stringify(cmnGetmsg['MSG_ERR_'+planguage+'_00004']));
				}
				else 
				{
					window.localStorage.setItem(EIM_KEY_CURRENT_ERROR_MESSAGE, JSON.stringify(cmnGetmsg['MSG_ERR_'+planguage+'_00008']));		
				}
				
				// フラッグリセット
				pchecking_before_properties = 0;
				pchecking_before_properties_element_type = "";
				// パンクずクーキーをクリーン
				f.cmnBreadcrumbsCheck(null, null, null);
				// ワークスペースを表示する
				location.reload(true);
				return;
			}
			else if (pchecking_before_properties == 2)
			{
				// isPublishedクーキーを登録する
				// 属性情報画面はisPublishedタブを使用する
				window.localStorage.setItem(EIM_KEY_CURRENT_PROPERTY_ISPUBLISHED, JSON.stringify(pCurrentFolder[JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_PROPERTY_ELEMENT))]['isPublished']));
				window.localStorage.setItem(EIM_KEY_CURRENT_PROPERTY_STATUSTYPENAME, JSON.stringify(pCurrentFolder[JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_PROPERTY_ELEMENT))]['statusTypeName']));
				window.localStorage.setItem(EIM_KEY_CURRENT_PROPERTY_READONLY, JSON.stringify(pCurrentFolder[JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_PROPERTY_ELEMENT))]['readOnly']));
				// 存在する場合
				// フラッグリセット
				pchecking_before_properties = 0;
				// クーキーをアップデート
				window.localStorage.setItem(EIM_KEY_CURRENT_PROPERTY_OBJTYPENAME, JSON.stringify(pCurrentFolder[JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_PROPERTY_ELEMENT))]['objTypeName']));
				// 属性情報画面の遷移
				$.mobile.changePage(properties_scr_Controller.getViewId(), {'transition' : 'fade', });
				return;
			}
			
	    	// ダウンロードファイル確認
			if (pdownloaded_document != "")
			{
				// リセット
				pdownloaded_document = "";
				if (download_file_check == true){
					// ダウンロードしたドキュメントが存在する場合
					// 確認出来ましたので、何もしない
					return;
				}
				else 
				{
					// ダウンロードエラーの場合、ワークスペースを表示する
					// パンクずクーキーをクリーン
					f.cmnBreadcrumbsCheck(null, null, null);
					// ワークスペースを表示する
					location.reload(true);
					return;
				};
				// ダウンロードしたドキュメントが存在ない場合
				// フォルダの内容をリフレッシュ
			};
			
			// 特別場合：フォルダは削除しました
			// pathタブ="/ごみ箱”になります
			if (fisdeleted == true) 
			{
				fisdeleted = false;
				// クーキーをアップデート
				window.localStorage.setItem(EIM_KEY_CURRENT_ERROR_MESSAGE, JSON.stringify(cmnGetmsg['MSG_ERR_'+planguage+'_00004']));
				// パンクずクーキーをクリーン
				f.cmnBreadcrumbsCheck(null, null, null);
				// ワークスペースを表示する
				location.reload(true);
				return;
			}
			
			// 並び替え
			// クーキーを設定
			if (JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_LIST_ORDER)) != null)
			{
				fstored_order = JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_LIST_ORDER));
			}			
			pfolderList_scr_order_text.empty().append(cmnLanguage['LAN_'+planguage+'_00023']);
			pfolderList_scr_order_combo.empty();
			pfolderList_scr_order_combo.append('<option value="'+cmnLanguage['LAN_'+planguage+"_00025"]+'">' + 
												cmnLanguage['LAN_'+planguage+"_00025"] + '</option>');
			pfolderList_scr_order_combo.append('<option value="'+cmnLanguage['LAN_'+planguage+"_00018"]+'">' + 
												cmnLanguage['LAN_'+planguage+"_00018"] + '</option>');
			pfolderList_scr_order_combo.append('<option value="'+cmnLanguage['LAN_'+planguage+"_00016"]+'"">' + 
												cmnLanguage['LAN_'+planguage+"_00016"] + '</option>');
			pfolderList_scr_order_combo.append('<option value="'+cmnLanguage['LAN_'+planguage+"_00020"]+'">' + 
												cmnLanguage['LAN_'+planguage+"_00020"] + '</option>');
			pfolderList_scr_order_combo.append('<option value="'+cmnLanguage['LAN_'+planguage+"_00022"]+'">' + 
												cmnLanguage['LAN_'+planguage+"_00022"] + '</option>');
			pfolderList_scr_order_combo.append('<option value="'+cmnLanguage['LAN_'+planguage+"_00019"]+'">' + 
												cmnLanguage['LAN_'+planguage+"_00019"] + '</option>');
			pfolderList_scr_order_combo.append('<option value="'+cmnLanguage['LAN_'+planguage+"_00021"]+'">' + 
												cmnLanguage['LAN_'+planguage+"_00021"] + '</option>');	
			if (fstored_order!=null)
			{
				if (fstored_order!="")
				{
					if (fstored_order == "_00017"){
						fstored_order = "_00020";
					};
					$('#folderList_scr_View-folderList_scr_order_combo option').filter(function () { 
						return $(this).html() == cmnLanguage['LAN_'+planguage+fstored_order]; }).prop('selected', true)
				};
			}
			else {
				$('#folderList_scr_View-folderList_scr_order_combo option').filter(function () { 
					return $(this).html() == cmnLanguage['LAN_'+planguage+"_00025"]; }).prop('selected', true)
			}
			// リフレッシュ
			pfolderList_scr_order_combo.selectmenu("refresh", true);

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
		 * 「post_dspChildObjectError」イベントハンドラ
		 */
		view.on('post_dspChildObjectError', function(e, xml) {
			// エラーメッセージを表示する。
			$(xml).find("error").each(function(){
				if (($(this).attr('message').indexOf('タイムアウト')>=0)||($(this).attr('message').indexOf('time-out')>=0))
				{
					f.cmnPopup($(this).attr('message'),"ERR",viewName);
					// メモリクリーン
					f.cmnSessionCheck(null);
					// ログイン画面の遷移
					$.mobile.changePage(login_scr_Controller.getViewId(), {'transition' : 'fade', });
					return;
				}
				// クーキーをアップデート
				window.localStorage.setItem(EIM_KEY_CURRENT_ERROR_MESSAGE, JSON.stringify($(this).attr('message')));
			});
			
			// パンクずクーキーをクリーン
			f.cmnBreadcrumbsCheck(null, null, null);
			// ワークスペースを表示する
			location.reload(true);
		});
		
		/**
		 * 「post_actSearchSuccess」イベントハンドラ
		 */
		view.on('post_actSearchSuccess', function(e, xml) {
			// 並び替え
			var fstored_order = null;
			var ffound = false;
			var download_file_check = false;
			var fnOrder = 0;
			
			presults_counter = 0;
			
			// pCurrentFolder半数をバックアップ
			if ((pCurrentFolderSearchBackup == "")||(pCurrentFolderSearchBackup == undefined)){
				pCurrentFolderSearchBackup = pfolderId;
			}

			// 変数にXMLの未承認件数情報をバインド
			$(xml).find("objList").each(function(){
				
				pfolderId = $(this).attr('objId');
				pCurrentFolder=new Object();
				
				$(xml).find("object").each(function(){
					var fobjId = "";
					var fobjTypeId ="";
					var fobjName ="";
					var fstatusTypeName ="";
					var fmodifyUserName ="";
					var fmodifyDate ="";
					var fproperty ="";
					var fisPublished = false;
					var fhistory ="";
					var freadOnly = true;
			    
					presults_counter = presults_counter +1;
					
					ffound = true;
					
					// objIdタブ
					fobjId = $(this).attr('objId');
					// objTypeIdタブ
					fobjTypeId = $(this).attr('objTypeId');
					// objNameタブ
					fobjName = $(this).attr('objName');
					// statusTypeNameタブ
					fstatusTypeName = $(this).attr('statusTypeName');
					// modifyUserNameタブ
					fmodifyUserName = $(this).attr('modifyUserName');
					// modifyDateタブ
					fmodifyDate = $(this).attr('modifyDate');
					// objNameタブ
					fproperty = $(this).attr('property');
					// isPublishedタブ
					fisPublished = $(this).attr('isPublished') == "true" ? true : false ;
					// readOnlyタブ
					freadOnly = $(this).attr('readOnly') == "true" ? true : false ;
					
					// revタブ
					fhistory = $(this).attr('rev');
					
					pCurrentFolder[fobjId]=new Object();
					pCurrentFolder[fobjId]['objTypeId']=fobjTypeId;
					pCurrentFolder[fobjId]['objName']=fobjName;
					pCurrentFolder[fobjId]['statusTypeName']=fstatusTypeName;
			    	pCurrentFolder[fobjId]['modifyUserName']=fmodifyUserName;
			    	pCurrentFolder[fobjId]['modifyDate']=fmodifyDate;
			    	pCurrentFolder[fobjId]['property']=fproperty;
			    	pCurrentFolder[fobjId]['isPublished']=fisPublished;
			    	pCurrentFolder[fobjId]['history']=fhistory;
			    	pCurrentFolder[fobjId]['readOnly']=freadOnly;
			    	pCurrentFolder[fobjId]['nOrder']=fnOrder;
			    	fnOrder = fnOrder + 1;
			    	
			    	// ダウンロードファイルの場合、確認する
			    	if (pdownloaded_document != ""){
			    		if (pdownloaded_document == fobjId)
			    		{
			    			download_file_check = true;
			    		};
			    	};
			    	
			    	// 属性フラッグの場合
					if (pchecking_before_properties == 1){
						if (JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_PROPERTY_ELEMENT)) != null)
						{
							// 存在する場合
							if (fobjId==JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_PROPERTY_ELEMENT))){
								pchecking_before_properties = 2;
							};	
						};
					};
				});
			});
			
			// 属性フラッグの場合
			if (pchecking_before_properties == 1)
			{
				// 存在しない場合
				// クーキーをアップデート
				window.localStorage.setItem(EIM_KEY_CURRENT_ERROR_MESSAGE, JSON.stringify(cmnGetmsg['MSG_ERR_'+planguage+'_00008']));		
				// フラッグリセット
				pchecking_before_properties = 0;
				// パンクずクーキーをクリーン
				f.cmnBreadcrumbsCheck(null, null, null);
				// ワークスペースを表示する
				location.reload(true);
				return;
			}
			else if (pchecking_before_properties == 2)
			{
				// isPublishedクーキーを登録する
				// 属性情報画面はisPublishedタブを使用する
				window.localStorage.setItem(EIM_KEY_CURRENT_PROPERTY_ISPUBLISHED, JSON.stringify(pCurrentFolder[JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_PROPERTY_ELEMENT))]['isPublished']));
				window.localStorage.setItem(EIM_KEY_CURRENT_PROPERTY_STATUSTYPENAME, JSON.stringify(pCurrentFolder[JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_PROPERTY_ELEMENT))]['statusTypeName']));
				window.localStorage.setItem(EIM_KEY_CURRENT_PROPERTY_READONLY, JSON.stringify(pCurrentFolder[JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_PROPERTY_ELEMENT))]['readOnly']));
				
				// 存在する場合
				// フラッグリセット
				pchecking_before_properties = 0;
				// クーキーをアップデート
				window.localStorage.setItem(EIM_KEY_CURRENT_PROPERTY_OBJTYPENAME, JSON.stringify(''));
				// 属性情報画面の遷移
				$.mobile.changePage(properties_scr_Controller.getViewId(), {'transition' : 'fade', });
				return;
			}
			
	    	// ダウンロードファイル確認
			if (pdownloaded_document != "")
			{
				// リセット
				pdownloaded_document = "";
				if (download_file_check == true){
					// ダウンロードしたドキュメントが存在する場合
					// 確認出来ましたので、何もしない
					return;
				}
				else 
				{
					// ダウンロードエラーの場合、ワークスペースを表示する
					// パンクずクーキーをクリーン
					f.cmnBreadcrumbsCheck(null, null, null);
					psearch = false;
					// クーキーをアップデート
					window.localStorage.setItem(EIM_KEY_CURRENT_LIST_SEARCH, JSON.stringify(psearch));
					if (pfolderList_scr_search_keyword!="")
					{
						pfolderList_scr_search_keyword.val("");	
					}
					// ワークスペースを表示する
					location.reload(true);
					return;
				};
				// ダウンロードしたドキュメントが存在ない場合
				// フォルダの内容をリフレッシュ
			};
			
			// 並び替え
			// クーキーを設定
			if (JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_LIST_ORDER)) != null)
			{
				fstored_order = JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_LIST_ORDER));
			}			
			pfolderList_scr_order_text.empty().append(cmnLanguage['LAN_'+planguage+'_00023']);
			pfolderList_scr_order_combo.empty();
			pfolderList_scr_order_combo.append('<option value="'+cmnLanguage['LAN_'+planguage+"_00025"]+'">' + 
												cmnLanguage['LAN_'+planguage+"_00025"] + '</option>');
			pfolderList_scr_order_combo.append('<option value="'+cmnLanguage['LAN_'+planguage+"_00018"]+'">' + 
												cmnLanguage['LAN_'+planguage+"_00018"] + '</option>');
			pfolderList_scr_order_combo.append('<option value="'+cmnLanguage['LAN_'+planguage+"_00016"]+'"">' + 
												cmnLanguage['LAN_'+planguage+"_00016"] + '</option>');
			pfolderList_scr_order_combo.append('<option value="'+cmnLanguage['LAN_'+planguage+"_00020"]+'">' + 
												cmnLanguage['LAN_'+planguage+"_00020"] + '</option>');
			pfolderList_scr_order_combo.append('<option value="'+cmnLanguage['LAN_'+planguage+"_00022"]+'">' + 
												cmnLanguage['LAN_'+planguage+"_00022"] + '</option>');
			pfolderList_scr_order_combo.append('<option value="'+cmnLanguage['LAN_'+planguage+"_00019"]+'">' + 
												cmnLanguage['LAN_'+planguage+"_00019"] + '</option>');
			pfolderList_scr_order_combo.append('<option value="'+cmnLanguage['LAN_'+planguage+"_00021"]+'">' + 
												cmnLanguage['LAN_'+planguage+"_00021"] + '</option>');	
			if (fstored_order!=null)
			{
				if (fstored_order!="")
				{
					if (fstored_order == "_00017"){
						fstored_order = "_00020";
					};
					$('#folderList_scr_View-folderList_scr_order_combo option').filter(function () { 
						return $(this).html() == cmnLanguage['LAN_'+planguage+fstored_order]; }).prop('selected', true)
				};
			}
			else {
				$('#folderList_scr_View-folderList_scr_order_combo option').filter(function () { 
					return $(this).html() == cmnLanguage['LAN_'+planguage+"_00025"]; }).prop('selected', true)
			}
			// リフレッシュ
			pfolderList_scr_order_combo.selectmenu("refresh", true);

			// 検索結果
			psearch = true;
			// クーキーをアップデート
			window.localStorage.setItem(EIM_KEY_CURRENT_LIST_SEARCH, JSON.stringify(psearch));
			if (pfolderList_scr_search_keyword.val() != null){
				window.localStorage.setItem(EIM_KEY_CURRENT_LIST_SEARCH_KEYWORD, JSON.stringify(pfolderList_scr_search_keyword.val()));
			};
			
			pfolderList_scr_back_list.empty().append(cmnLanguage['LAN_'+planguage+"_00060"] + presults_counter + 
					cmnLanguage['LAN_'+planguage+"_00061"] +"<a id='folderList_scr_back_list_link' href='#'>"+cmnLanguage['LAN_'+planguage+"_00026"]+"</a>");


			if (ffound == false)
			{
				f.cmnPopup(cmnGetmsg['MSG_ERR_'+planguage+'_00003'],"ERR",viewName);
			}
			
			// コンポーネントに変数をバインド
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
		 * 「post_actSearchError」イベントハンドラ
		 */
		view.on('post_actSearchError', function(e, xml) {
			// エラーメッセージを表示する。
			$(xml).find("error").each(function(){
				f.cmnPopup($(this).attr('message'),"ERR",viewName);
				if (($(this).attr('message').indexOf('タイムアウト')>=0)||($(this).attr('message').indexOf('time-out')>=0))
				{
					// メモリクリーン
					f.cmnSessionCheck(null);
					// ログイン画面の遷移
					$.mobile.changePage(login_scr_Controller.getViewId(), {'transition' : 'fade', });
				}
			});
			
			// ボタンをＥＮＡＢＬＥする
			plist_scr_btn_Search.attr("disabled", false);
		});
		
		/**
		 * 「post_dspFolderTreeForTargetSuccess」イベントハンドラ
		 */
		view.on('post_dspFolderTreeForTargetSuccess', function(e, xml) {
			pbreadcrumb = new Object();
			pbreadcrumb_names = new Object();
			
			pbreadcrumb[0]="Top";
			
			// 変数にXMLの未承認件数情報をバインド
			pis_favorite_node = false;
			look_for_Folder($(xml),1);
			order_breadcrumbs();
			if (pis_favorite_node == false)
			{
				f.cmnPopup(cmnGetmsg['MSG_ERR_'+planguage+'_00004'],"ERR",viewName);
				// ワークスペース情報の取得
				model.get_dspFolderTree();
			}
			
			model.post_dspChildObject (pfolderList_scr_workspace_infoId);
		});
		
		/**
		 * 「post_dspFolderTreeForTargetError」イベントハンドラ
		 */
		view.on('post_dspFolderTreeForTargetError', function(e, xml) {
			// エラーメッセージを表示する。
			$(xml).find("error").each(function(){
				if (($(this).attr('message').indexOf('タイムアウト')>=0)||($(this).attr('message').indexOf('time-out')>=0))
				{
					f.cmnPopup($(this).attr('message'),"ERR",viewName);
					// メモリクリーン
					f.cmnSessionCheck(null);
					// ログイン画面の遷移
					$.mobile.changePage(login_scr_Controller.getViewId(), {'transition' : 'fade', });
					return;
				}
				// クーキーをアップデート
				window.localStorage.setItem(EIM_KEY_CURRENT_ERROR_MESSAGE, JSON.stringify($(this).attr('message')));
			});
			
			// パンクずクーキーをクリーン
			f.cmnBreadcrumbsCheck(null, null, null);
			// ワークスペースを表示する
			location.reload(true);
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
		$('.folderList_scr_View-list_scr_top_icon_div').hide();
		$('#folderList_scr_View-list_scr_top_icon').empty().append('<a id="folderList_scr_View-list_scr_top_icon_" href="#">' + 
																	cmnGetimg['list_scr_top_icon'] + '</a>');
		// 閉じる
		$('.header_menuBox').hide();
		
		pwas_initialized = true;
		
		model.post_dspApproveDocumentList();
	});
	
	/**
	 * 「pagebeforeshow」イベントハンドラテンプレート
	 */
	jQuery(document).on('pagebeforeshow', viewId, function(ev) {
		// クーキーを確認
		psearch = JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_LIST_SEARCH));
		
		if (psearch == null)
		{
			psearch = false;
		}
		else
		{
			// folderList_scr_Viewのキーワードコンポーネントに変数をバインド
			if (JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_LIST_SEARCH_KEYWORD)) == null)
			{
				pfolderList_scr_search_keyword.val("");
			}
			else 
			{
				pfolderList_scr_search_keyword.val(JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_LIST_SEARCH_KEYWORD)));
			};
		}
	
		// パンクずと検索を表示
		$('.folderList_scr_View-folderList_scr_order_combo_div').show();
		$('.folderList_scr_View-folderList_scr_bc').show();
		
		pcurrentView = viewName;
		pfolderList_scr_list_item.empty();
		// トップへ戻るボタンを非表示にする。
		$('.folderList_scr_View-list_scr_top_icon_div').hide();
		$('.unapproved_scr_View-list_scr_top_icon_div').hide();
		$('.favorites_scr_View-list_scr_top_icon_div').hide();
		// ボタンハイライト
		cmn_header_button_highlight (1);
		
		// 属性フラッグのクーキーをアップデート
		window.localStorage.setItem(EIM_KEY_CURRENT_FORMER_VIEW, JSON.stringify(viewName));
	});
	
	/**
	 * 「pageshow」イベントハンドラ
	 */
	jQuery(document).on('pageshow', viewId, function(ev) {
		var ferror_message = "";
		
		// お気に入りフラッグを確認
		if (pcoming_from_favorites != true)
		{
			// クーキーをアップデート
			pbreadcrumb = new Object();
			pbreadcrumb[0] = "";
			
			f.cmnBreadcrumbsCheck(pbreadcrumb, pbreadcrumb_names, pCurrentFolder);
			
			if (pbreadcrumb[0] == "")
			{
				pbreadcrumb[0]="Ｔｏｐ";
			};
		}
		
		// ヘッダーのタイトル
		$('.header_title').empty();
		
		var fbreadcrumb_lenght = 0;
		var ffolderList_scr_search_keyword = "";
		
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
		$('.header_title').empty().append(cmnLanguage['LAN_'+planguage+'_00007']);
		
		// 検索ボタンを設定
		plist_scr_btn_Search.empty().append(cmnLanguage['LAN_'+planguage+'_00024']);
		
		// パンくず変数をアップデート
		fbreadcrumb_lenght = calculate_breadcrumb_lenght();
		
		// クーキーを確認
		if (JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_LIST_TOP_ELEMENT, JSON.stringify(ptop_locator))) != null){
			ptop_locator = JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_LIST_TOP_ELEMENT, JSON.stringify(ptop_locator)));
		}
		else 
		{
			ptop_locator = "";
		}
		
		// エラーメッセージがある場合、表示する。
		// フォルダーが見つかりませんでした場合、等
		if (window.localStorage.getItem(EIM_KEY_CURRENT_ERROR_MESSAGE) != null)
		{
			// エラーメッセージを表示
			ferror_message = JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_ERROR_MESSAGE));
			
			if (pcoming_from_favorites == false){
				f.cmnPopup(ferror_message,"ERR",viewName);	
			}
			
			window.localStorage.removeItem(EIM_KEY_CURRENT_ERROR_MESSAGE);
			
			// クーキーをアップデート
			pbreadcrumb = new Object();
			pbreadcrumb[0]="Ｔｏｐ";
			pbreadcrumb_names = new Object();
			
			// お気に入りフラッグをリセット
			if (pcoming_from_favorites == true)
			{
				pcoming_from_favorites = false;
			}
			
			// ワークスペース情報の取得
			model.get_dspFolderTree();
			return;
		}
		
		if (psearch != true) {
			if ((fbreadcrumb_lenght > 1) || (pcoming_from_favorites == true))
			{
				// お気に入りフラッグを確認
				if (pcoming_from_favorites != true)
				{
					// リロードのフォルダ情報の取得
					pfolderList_scr_workspace_infoId = JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_LIST_FOLDERID));
					model.post_dspChildObject (pfolderList_scr_workspace_infoId);	
				}
				else 
				{
					// お気に入りフラッグをリセット
					pcoming_from_favorites = false;
					model.post_dspFolderTreeForTarget(pfolderList_scr_workspace_infoId);
				}
			}
			else 
			{
				// ワークスペース情報の取得
				model.get_dspFolderTree();
			};
		}
		else 
		{
			// 検索リロード
			// 変数にfolderList_scr_View情報をバインド
			ffolderList_scr_search_keyword = pfolderList_scr_search_keyword.val();
			
			// リロードのフォルダ情報の取得
			pfolderList_scr_workspace_infoId = JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_LIST_FOLDERID));
			pCurrentFolderSearchBackup = pfolderList_scr_workspace_infoId;
			pfolderId = pfolderList_scr_workspace_infoId;
				
			//モデルメソッドの呼び出し
			model.post_actSearch(ffolderList_scr_search_keyword, build_path());
		}
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
	 * [機能]	パンくずリストをアップデート
	 * [引数]	無し
	 * [戻値]	無し
	 ********************************************************************************/
	function make_breadcrumb_list() {
		var fbreadcrumb_list = "";
		var fbreadcrumb_lenght = 0;
		var fposition = 0;
		var fadded_points = false;
		var ftotallength = 0;
		var freduce = false;
		var fcurrentname = "";
		
		ftotallength = 6;
		for (var i in pbreadcrumb_names)
		{
			if (i!=0)
			{
				ftotallength = ftotallength + pbreadcrumb_names[i].length;
			};
		}
		
		// 画面の縦/横の初期設定
		if ($(document.body).height()>800)
		{
			if (ftotallength > 25)
			{
				freduce = true;
			};
		}
		else if (ftotallength > 50)
		{
			freduce = true;
		}

		fbreadcrumb_lenght = calculate_breadcrumb_lenght();
		
		fbreadcrumb_list ='<ul id="breadcrumbs-one">';	
		for (var i in pbreadcrumb){
			
			fposition = fposition + 1;
			
			if (i==0)
			{
				fbreadcrumb_list = fbreadcrumb_list + '<li><a class="folderList_scr_breadcrumb" id="Ｔｏｐ" href="#">　'+
									pbreadcrumb[i]+'　</a></li>';
			}
			else 
			{
				if ((fbreadcrumb_lenght > 4)&&(fposition > 2)&&(fposition < (fbreadcrumb_lenght-1)))
				{
					if (fadded_points == false){
						// パンクずが長い場合、「＞...＞」を表示する
						fbreadcrumb_list = fbreadcrumb_list + '<li><a class="folderList_scr_breadcrumb" id="..." href="#">　...　</a></li>';
						fadded_points = true;	
					}
				}
				else 
				{
					if (freduce == true)
					{
						fcurrentname = pbreadcrumb_names[i];
						if ($(document.body).height()>800)
						{
							if (fcurrentname.length > 5)
							{
								fbreadcrumb_list = fbreadcrumb_list + '<li><a class="folderList_scr_breadcrumb" id="'+i+'" href="#">　'+
								pbreadcrumb_names[i].substring(0,5)+'...　</a></li>';	
							}
							else
							{
								fbreadcrumb_list = fbreadcrumb_list + '<li><a class="folderList_scr_breadcrumb" id="'+i+'" href="#">　'+
								pbreadcrumb_names[i]+'　</a></li>';		
							}
						}
						else 
						{
							if (fcurrentname.length > 10)
							{
								fbreadcrumb_list = fbreadcrumb_list + '<li><a class="folderList_scr_breadcrumb" id="'+i+'" href="#">　'+
								pbreadcrumb_names[i].substring(0,10)+'...　</a></li>';
							}
							else
							{
								fbreadcrumb_list = fbreadcrumb_list + '<li><a class="folderList_scr_breadcrumb" id="'+i+'" href="#">　'+
								pbreadcrumb_names[i]+'　</a></li>';
							}
						}
					}
					else 
					{
						fbreadcrumb_list = fbreadcrumb_list + '<li><a class="folderList_scr_breadcrumb" id="'+i+'" href="#">　'+
						pbreadcrumb_names[i]+'　</a></li>';
					}
				}
			};
		}
		
		fbreadcrumb_list = fbreadcrumb_list+'</ul>';
		pfolderList_scr_breadcrumb.empty().append(fbreadcrumb_list);
		
		// クーキーをアップデート
		f.cmnBreadcrumbsCheck(null, null, null);
		f.cmnBreadcrumbsCheck(pbreadcrumb, pbreadcrumb_names, pCurrentFolder);
	}
	
	/********************************************************************************
	 * [機能]	縦向きになった場合
	 * [引数]	無し
	 * [戻値]	無し
	 ********************************************************************************/
	function resize_screen_portrait() {
		var ffolder_link_html1 = "";
		var ffolder_link_html1_icon = "";
		var ffolder_link_html2 = "";
		var ficon_to_show = "";
		var fbreadcrumb_lenght = 0;
		var flast_folder = "";
		var ffolderList_scr_list_item_html="";
		var ffile_extension = "";
		var fshorted_name = "";
		var fshorted_property = "";
		var flinker = "";
		// 並び替え
		var fsorted_list =null;
		var i = "";
		
		// ＯＳによって、アイコンのサイズを変わる
		cmn_change_image_sizes (0, planguage);
		
		// トップへ戻るボタンを非表示にする。
		$('.folderList_scr_View-list_scr_top_icon_div').hide();	
		// 検索結果「一覧に戻る」コンポーネント。
		if (psearch == true)
		{
			pfolderList_scr_back_list.show();	
		}
		else
		{
			pfolderList_scr_back_list.hide();
		};
		
		// コンポーネントリドロー
		if ((navigator.userAgent.indexOf("iPad")>0)&&((navigator.userAgent.indexOf("OS 8")>0)||(navigator.userAgent.indexOf("OS 7")>0))) 
		{
			ffolderList_scr_list_item_html = '<ul style="height: 590px;" class="scrollable" data-role="listview" data-split-icon="grid" data-split-theme="d">';
		}
		else if (navigator.userAgent.indexOf("iPad")>0)
		{
			ffolderList_scr_list_item_html = '<ul style="height: 560px;" class="scrollable" data-role="listview" data-split-icon="grid" data-split-theme="d">';
		}
		else if (navigator.userAgent.indexOf("iPhone")>0)
		{
			ffolderList_scr_list_item_html = '<ul style="height: 1330px;" class="scrollable" data-role="listview" data-split-icon="grid" data-split-theme="d">';
		}
		else 
		{
			ffolderList_scr_list_item_html = '<ul style="height: 560px;" class="scrollable" data-role="listview" data-split-icon="grid" data-split-theme="d">';
		}
		
		fbreadcrumb_lenght = calculate_breadcrumb_lenght();
		
		if ((fbreadcrumb_lenght == 1) && (psearch==false))
		{
			// ワークスペース
		
			// 並び替え
			fsorted_list = create_sorted_list(0);
			
			ptop_locator = "";
			// クーキーをアップデート
			window.localStorage.setItem(EIM_KEY_CURRENT_LIST_TOP_ELEMENT, JSON.stringify(ptop_locator));
			
			for (var key in fsorted_list){
				i = String(fsorted_list[key][0]);
				
				if (pNodes[i]['isWorkSpace']!=true)
				{
					continue;
				}
				
				if (ptop_locator == "") 
				{
					ptop_locator = 'list_src_workspace_icon_'+i;
					// クーキーをアップデート
					window.localStorage.setItem(EIM_KEY_CURRENT_LIST_TOP_ELEMENT, JSON.stringify(ptop_locator));
				} 
				
				fshorted_name = pNodes[i]['label'];
				fshorted_property = pNodes[i]['property'];
				fshorted_name = resizeString(fshorted_name, viewName, 'portrait_workspace_name');
				fshorted_property = resizeString(fshorted_property, viewName, 'portrait_workspace_property');
				
				ffolderList_scr_list_item_html = ffolderList_scr_list_item_html + 
				'<li class="folderList_scr_list_item">'+
					'<table style="vertical-align: top;-webkit-border-horizontal-spacing: 0px; -webkit-border-vertical-spacing: 0px;' +
					' border: inset 0pt;border-style: hidden;border-collapse: collapse;border-spacing:0;width:100%;"><tr width="100%">' +
					'<td width="15%" valign="top"><a class="list_src_workspace_icon" id="list_src_workspace_icon_'+i+
					'" href="#" data-transition="slidefade">' + cmnGetimg['list_src_workspace_icon'] + '</a></td>' +
					'<td width="75%" align="left" style="vertical-align: middle;"><div class="list_src_workspace_info" id="list_src_workspace_info_'+i+
					'"><a class="list_src_workspace_info" id="list_src_workspace_info_'+i+'" href="#"><font style="font-size: 150%;font-weight: bold;">'+fshorted_name+'</font><br>'+
					cmnLanguage['LAN_'+planguage+'_00016'] + ': '+pNodes[i]['modifyUserName']+'　　'+
					cmnLanguage['LAN_'+planguage+'_00017']+': '+pNodes[i]['modifyDate']+'<br>'+
					cmnLanguage['LAN_'+planguage+'_00021']+': '+fshorted_property+
					'</a></div></td><td width="5%" style="vertical-align: middle;">'+
					'<table style="-webkit-border-horizontal-spacing: 0px; -webkit-border-vertical-spacing: 0px; border: inset 0pt;' + 
					'border-style: hidden;border-collapse: collapse;border-spacing:0;width:100%;"><td align="middle">' + 
					'<p style="line-height: 400%;"><center><a class="list_scr_btn_Properties" id="list_scr_btn_Properties_'+i+'" href="#">'+
					cmnGetimg['list_scr_btn_Properties']+'<br><font color="black">'+
					cmnLanguage['LAN_'+planguage+'_00015']+'</font></a></center></p></td></tr></table>'+
					'</td><td width="5%" ></td></tr></table>'+
				'</li>';
			};
		}
		else
		{
			// フォルダ又は検索結果
		
			// パンくず変数をアップデート
			//  一階層上フォルダをクリックの場合、何もしない
			if ((pbreadcrumb[fbreadcrumb_lenght-1]!=pfolderId) && (psearch != true)){
				pbreadcrumb[fbreadcrumb_lenght]= pfolderId;
				pbreadcrumb_names[fbreadcrumb_lenght] = pfolderName;
				fbreadcrumb_lenght = fbreadcrumb_lenght + 1;	
			}
			flast_folder = pbreadcrumb[fbreadcrumb_lenght-2];
			
			ptop_locator = "";
			// クーキーをアップデート
			window.localStorage.setItem(EIM_KEY_CURRENT_LIST_TOP_ELEMENT, JSON.stringify(ptop_locator));
			
			// 検索結果の場合は一階層上フォルダを表示しない
			if (psearch != true){
				// 一階層上フォルダ
				ffolder_link_html1 = '<a class="list_src_folder_back_icon" id="list_src_workspace_info_'+flast_folder+'" href="#">';
				ffolder_link_html1_icon = '<a class="list_src_folder_back_icon" id="list_src_folder_back_icon" href="#">';
				ffolder_link_html2 = '</a>';
				ficon_to_show = cmnGetimg['list_scr_folder_icon'];
				ffolderList_scr_list_item_html = ffolderList_scr_list_item_html + 
				'<li class="folderList_scr_list_item">'+
					'<table style="vertical-align: top;-webkit-border-horizontal-spacing: 0px; -webkit-border-vertical-spacing: 0px;' + 
					' border: inset 0pt;border-style: hidden;border-collapse: collapse;border-spacing:0;width:100%;"><tr width="100%">' + 
					'<td width="15%" valign="top">' + ffolder_link_html1_icon + ficon_to_show + '</a></td>' +
					'<td width="75%" align="left" style="vertical-align: middle;"><div class="list_src_folder_back_icon">'+ffolder_link_html1+
					'<font style="font-size: 150%;font-weight: bold;">'+".. /"+'</font>'+
					ffolder_link_html2+'</div></td><td width="5%" style="vertical-align: middle;">'+
					'<table style="-webkit-border-horizontal-spacing: 0px; -webkit-border-vertical-spacing: 0px; ' + 
					'border: inset 0pt;border-style: hidden;border-collapse: collapse;border-spacing:0;width:100%;"><td align="middle"></td></tr></table>'+
					'</td><td width="5%" ></td></tr></table>'+
				'</li>';
				ptop_locator = "list_src_folder_back_icon";
				// クーキーをアップデート
				window.localStorage.setItem(EIM_KEY_CURRENT_LIST_TOP_ELEMENT, JSON.stringify(ptop_locator));
			}
			
			// 並び替え
			fsorted_list = create_sorted_list(1);

			for (var key in fsorted_list){
				i = String(fsorted_list[key][0]);
				
				if (ptop_locator == "") 
				{
					ptop_locator = 'list_src_workspace_icon_'+i;
					// クーキーをアップデート
					window.localStorage.setItem(EIM_KEY_CURRENT_LIST_TOP_ELEMENT, JSON.stringify(ptop_locator));
				};
				
				ffile_extension = (pCurrentFolder[i]['objName']).substring((pCurrentFolder[i]['objName']).lastIndexOf(".") + 1);
				
				if (pCurrentFolder[i]['objTypeName']==cmn_objTypeName['フォルダ'])
				{
					// フォルダ
					ffolder_link_html1 = '<a class="list_src_workspace_info" id="list_src_workspace_info_'+i+'" href="#">';
					ffolder_link_html1_icon = '<a class="list_src_workspace_info" id="list_src_workspace_icon_'+i+'" href="#">';
					ficon_to_show = cmnGetimg['list_scr_folder_icon'];
					flinker = 'class="list_src_workspace_info" id="list_src_workspace_info_'+i+'"';					
				}
				else 
				{
					// ファイル
					
					// ドキュメントをダウンロード
					if ((pCurrentFolder[i]['isPublished'] == true)&&(pCurrentFolder[i]['readOnly'] == true))
					{
						// ユーザーのセキュリチ―：パブリック（readOnly＝ＴＲＵＥ）
						// パブリックドキュメントが存在する場合
						// パブリックをダウンロード  '
						ffolder_link_html1 = '<a class="post_actSendReplyMail" target="_blank" id="list_src_workspace_icon_'+i+'" href="#">';
						ffolder_link_html1_icon = '<a class="post_actSendReplyMail" target="_blank" id="list_src_workspace_icon_'+i+'" href="'+cmnJSPAPI['JSPAPI_00010']+i+'">';
						flinker = 'onclick="var win = window.open(\''+cmnJSPAPI['JSPAPI_00010']+i+'\',\'_blank\');win.focus();$(\'#list_src_workspace_icon_'+i+'\').trigger(\'click\');"';
					}
					else if ((pCurrentFolder[i]['isPublished'] == false )&&(pCurrentFolder[i]['statusTypeName'] == "-"))
					{
						// 2014/12/15 CTC追記statusTypeName=- isPublished=false の場合は
						// public(公開文書)のダウンロードを可能 '
						ffolder_link_html1 = '<a class="post_actSendReplyMail" target="_blank" id="list_src_workspace_icon_'+i+'" href="#">';
						ffolder_link_html1_icon = '<a class="post_actSendReplyMail" target="_blank" id="list_src_workspace_icon_'+i+'" href="'+cmnJSPAPI['JSPAPI_00010']+i+'">';
						flinker = 'onclick="var win = window.open(\''+cmnJSPAPI['JSPAPI_00010']+i+'\',\'_blank\');win.focus();$(\'#list_src_workspace_icon_'+i+'\').trigger(\'click\');"';
					}
					else if ((pCurrentFolder[i]['isPublished'] == false )&&(pCurrentFolder[i]['readOnly'] == true))
					{
						// ユーザーのセキュリチ―：パブリック（readOnly＝ＴＲＵＥ）
						// パブリックドキュメントがまだ存在しない場合、パブリックダウンロードが出来ません
						ffolder_link_html1 = '<a class="" id="" href="#">';
						ffolder_link_html1_icon = '<a class="" id="" href="#">';
					}
					else if (pCurrentFolder[i]['readOnly'] == false)
					{
						// ユーザーのセキュリチ―：原本とパブリック（readOnly＝ＦＡＬＳＥ）
						// 原本をダウンロード '
						ffolder_link_html1 = '<a class="post_actSendReplyMail" target="_blank" id="list_src_workspace_icon_'+i+'" href="#">';
						ffolder_link_html1_icon = '<a class="post_actSendReplyMail" target="_blank" id="list_src_workspace_icon_'+i+'" href="'+cmnJSPAPI['JSPAPI_00011']+i+'">';
						flinker = 'onclick="var win = window.open(\''+cmnJSPAPI['JSPAPI_00011']+i+'\',\'_blank\');win.focus();$(\'#list_src_workspace_icon_'+i+'\').trigger(\'click\');"';
					}
							
					switch(ffile_extension.substring(0,3)) 
					{
						case "xls":
							ficon_to_show = cmnGetimg['list_scr_document_excel']; break;
						case "XLS":
							ficon_to_show = cmnGetimg['list_scr_document_excel']; break;
						case "pdf":
							ficon_to_show = cmnGetimg['list_scr_document_pdf']; break;
						case "PDF":
							ficon_to_show = cmnGetimg['list_scr_document_pdf']; break;
						case "ppt":
							ficon_to_show = cmnGetimg['list_scr_document_ppt']; break;
						case "PPT":
							ficon_to_show = cmnGetimg['list_scr_document_ppt']; break;
						case "doc":
							ficon_to_show = cmnGetimg['list_scr_document_doc']; break;
						case "DOC":
							ficon_to_show = cmnGetimg['list_scr_document_doc']; break;
						default:
							ficon_to_show = cmnGetimg['list_scr_document'];
					};
				}
				
				fshorted_name = pCurrentFolder[i]['objName'];
				fshorted_property = pCurrentFolder[i]['property'];
				fshorted_name = resizeString(fshorted_name, viewName, 'portrait_folder_name');
				fshorted_property = resizeString(fshorted_property, viewName, 'portrait_folder_property');
				
				ffolderList_scr_list_item_html = ffolderList_scr_list_item_html + 
					'<li class="folderList_scr_list_item">'+
						'<table style="vertical-align: top;-webkit-border-horizontal-spacing: 0px; -webkit-border-vertical-spacing: 0px;' + 
						' border: inset 0pt;border-style: hidden;border-collapse: collapse;border-spacing:0;width:100%;"><tr width="100%">' + 
						'<td width="15%" valign="top">' + ffolder_link_html1_icon + ficon_to_show + '</a></td>' +
						'<td width="75%" align="left" style="vertical-align: middle;"><div '+flinker+'>'+ffolder_link_html1+
						'<font style="font-size: 150%;font-weight: bold;">'+fshorted_name+'</font><br>'+
						cmnLanguage['LAN_'+planguage+'_00016'] + ': '+pCurrentFolder[i]['modifyUserName']+' 　　'+
						cmnLanguage['LAN_'+planguage+'_00020']+': '+pCurrentFolder[i]['modifyDate']+
						'<br>'+cmnLanguage['LAN_'+planguage+'_00022'] + ': '+pCurrentFolder[i]['history']+' 　　'+
						cmnLanguage['LAN_'+planguage+'_00019']+': '+pCurrentFolder[i]['statusTypeName']+' 　　'+
						cmnLanguage['LAN_'+planguage+'_00021']+': '+fshorted_property+
						ffolder_link_html2+'</div></td><td width="5%" style="vertical-align: middle;">'+
						'<table style="-webkit-border-horizontal-spacing: 0px; -webkit-border-vertical-spacing: 0px; ' + 
						'border: inset 0pt;border-style: hidden;border-collapse: collapse;border-spacing:0;width:100%;">' + 
						'<td align="middle"><p style="line-height: 400%;"><center><a class="list_scr_btn_Properties" id="list_scr_btn_Properties_'+i+'" href="#">' + 
						cmnGetimg['list_scr_btn_Properties']+'<br><font color="black">'+cmnLanguage['LAN_'+planguage+'_00015'] + 
						'</font></a></center></p></td></tr></table>'+
						'</td><td width="5%" ></td></tr></table>'+
					'</li>';
			};
		}
		
		ffolderList_scr_list_item_html = ffolderList_scr_list_item_html + '</ul>';
		pfolderList_scr_list_item.empty().append(ffolderList_scr_list_item_html);
		
		// パンくずリストをアップデート
		make_breadcrumb_list();
		
		if (psearch == true){
			// ボタンをＥＮＡＢＬＥする
			plist_scr_btn_Search.data('disabled',false);
		};
	}
	
	/********************************************************************************
	 * [機能]	横向きになった場合
	 * [引数]	無し
	 * [戻値]	無し
	 ********************************************************************************/
	function resize_screen_landscape() {
		var ffolder_link_html1 = "";
		var ffolder_link_html1_icon = "";
		var ffolder_link_html2 = "";
		var ficon_to_show = "";
		var flast_folder = "";
		var fbreadcrumb_lenght = 0;
		var ffolderList_scr_list_item_html="";
		var ffile_extension = "";
		var fshorted_name = "";
		var fshorted_property = "";
		var flinker = "";
		// 並び替え
		var fsorted_list =null;
		var i = "";
		
		// ＯＳによって、アイコンのサイズを変わる
		cmn_change_image_sizes (1, planguage);
		
		// トップへ戻るボタンを非表示にする。
		$('.folderList_scr_View-list_scr_top_icon_div').hide();	
		// 検索結果「一覧に戻る」コンポーネント。
		if (psearch == true)
		{
			pfolderList_scr_back_list.show();	
		}
		else
		{
			pfolderList_scr_back_list.hide();
		};
		
		// コンポーネントリドロー
		if ((navigator.userAgent.indexOf("iPad")>0)&&((navigator.userAgent.indexOf("OS 8")>0)||(navigator.userAgent.indexOf("OS 7")>0))) 
		{
			ffolderList_scr_list_item_html = '<ul style="height: 335px;" class="scrollable" data-role="listview" data-split-icon="grid" data-split-theme="d">';
		}
		else if (navigator.userAgent.indexOf("iPad")>0)
		{
			ffolderList_scr_list_item_html = '<ul style="height: 300px;" class="scrollable" data-role="listview" data-split-icon="grid" data-split-theme="d">';
		}
		else 
		{
			ffolderList_scr_list_item_html = '<ul style="height: 353px;" class="scrollable" data-role="listview" data-split-icon="grid" data-split-theme="d">';
		}
		
		fbreadcrumb_lenght = calculate_breadcrumb_lenght();
		
		if ((fbreadcrumb_lenght == 1) && (psearch==false))
		{
			// ワークスペース
			
			// 並び替え
			fsorted_list = create_sorted_list(0);
			
			ptop_locator = "";
			// クーキーをアップデート
			window.localStorage.setItem(EIM_KEY_CURRENT_LIST_TOP_ELEMENT, JSON.stringify(ptop_locator));
			
			for (key in fsorted_list){
				i = String(fsorted_list[key][0]);

				if (pNodes[i]['isWorkSpace']!=true)
				{
					continue;
				}
				
				if (ptop_locator == "") 
				{
					ptop_locator = 'list_src_workspace_icon_'+i;
					// クーキーをアップデート
					window.localStorage.setItem(EIM_KEY_CURRENT_LIST_TOP_ELEMENT, JSON.stringify(ptop_locator));
				}

				fshorted_name = pNodes[i]['label'];
				fshorted_property = pNodes[i]['property'];
				fshorted_name = resizeString(fshorted_name, viewName, 'landscape_workspace_name');
				fshorted_property = resizeString(fshorted_property, viewName, 'landscape_workspace_property');
				
				ffolderList_scr_list_item_html = ffolderList_scr_list_item_html + 
				'<li class="folderList_scr_list_item">'+
					'<table style="vertical-align: top;-webkit-border-horizontal-spacing: 0px; -webkit-border-vertical-spacing: 0px; ' + 
					'border: inset 0pt;border-style: hidden;border-collapse: collapse;border-spacing:0;width:100%;"><tr width="100%">' + 
					'<td width="8%" valign="top"><a class="list_src_workspace_icon" id="list_src_workspace_icon_'+i+
					'" href="#" data-transition="slidefade">' + cmnGetimg['list_src_workspace_icon'] + '</a></td>' +
					'<td width="45%" align="left" style="vertical-align: middle;"><div class="list_src_workspace_info" id="list_src_workspace_info_'+i+'">"'+
					'<a class="list_src_workspace_info" id="list_src_workspace_info_'+i+'" href="#">' + '<font style="font-size: 150%;font-weight: bold;">'+fshorted_name+'</font><br>'+cmnLanguage['LAN_'+planguage+'_00016']+': ' + 
					pNodes[i]['modifyUserName']+'　　'+cmnLanguage['LAN_'+planguage+'_00017']+': '+pNodes[i]['modifyDate']+'<br>'+
					cmnLanguage['LAN_'+planguage+'_00021']+': '+fshorted_property+
					'</a></div></td><td width="5%" style="vertical-align: middle;">'+
					'<table style="-webkit-border-horizontal-spacing: 0px; -webkit-border-vertical-spacing: 0px; ' + 
					'border: inset 0pt;border-style: hidden;border-collapse: collapse;border-spacing:0;width:100%;">' + 
					'<td align="middle"><p style="line-height: 400%;"><center><a class="list_scr_btn_Properties" id="list_scr_btn_Properties_'+i+'" href="#">' + 
					cmnGetimg['list_scr_btn_Properties']+'<br><font color="black">'+cmnLanguage['LAN_'+planguage+'_00015']+'</font></a></center></p></td></tr></table>'+
					'</td><td width="1%"></td></tr></table>'+
				'</li>';
			};
		}
		else
		{
			// フォルダ又は検索結果
			
			// パンくず変数をアップデート
			//  一階層上フォルダをクリックの場合、何もしない
			if ((pbreadcrumb[fbreadcrumb_lenght-1]!=pfolderId) && (psearch != true)){
				pbreadcrumb[fbreadcrumb_lenght]= pfolderId;
				pbreadcrumb_names[fbreadcrumb_lenght] = pfolderName;
				fbreadcrumb_lenght = fbreadcrumb_lenght + 1;	
			}
			flast_folder = pbreadcrumb[fbreadcrumb_lenght-2];
			
			ptop_locator = "";
			// クーキーをアップデート
			window.localStorage.setItem(EIM_KEY_CURRENT_LIST_TOP_ELEMENT, JSON.stringify(ptop_locator));
			
			// 検索結果の場合は一階層上フォルダを表示しない
			if (psearch != true){
				// 一階層上フォルダ
				ffolder_link_html1 = '<a class="list_src_folder_back_icon" id="list_src_workspace_info_'+flast_folder+'" href="#">';
				ffolder_link_html1_icon = '<a class="list_src_folder_back_icon" id="list_src_folder_back_icon" href="#">';
				ffolder_link_html2 = '</a>';
				ficon_to_show = cmnGetimg['list_scr_folder_icon'];
				ffolderList_scr_list_item_html = ffolderList_scr_list_item_html + 
				'<li class="folderList_scr_list_item">'+
					'<table style="vertical-align: top;-webkit-border-horizontal-spacing: 0px; -webkit-border-vertical-spacing: 0px;' + 
					' border: inset 0pt;border-style: hidden;border-collapse: collapse;border-spacing:0;width:100%;"><tr width="100%">' + 
					'<td width="15%" valign="top">' + ffolder_link_html1_icon + ficon_to_show + '</a></td>' +
					'<td width="75%" align="left" style="vertical-align: middle;"><div class="list_src_folder_back_icon">'+ffolder_link_html1+
					'<font style="font-size: 150%;font-weight: bold;">'+".. /"+'</font>'+
					ffolder_link_html2+'</div></td><td width="5%" style="vertical-align: middle;">'+
					'<table style="-webkit-border-horizontal-spacing: 0px; -webkit-border-vertical-spacing: 0px; ' + 
					'border: inset 0pt;border-style: hidden;border-collapse: collapse;border-spacing:0;width:100%;"><td align="middle"></td></tr></table>'+
					'</td><td width="5%" ></td></tr></table>'+
				'</li>';
				
				ptop_locator = "list_src_folder_back_icon";
				// クーキーをアップデート
				window.localStorage.setItem(EIM_KEY_CURRENT_LIST_TOP_ELEMENT, JSON.stringify(ptop_locator));
			}
			
			// 並び替え
			fsorted_list = create_sorted_list(1);

			for (var key in fsorted_list){
				i = String(fsorted_list[key][0]);
				
				if (ptop_locator == "") 
				{
					ptop_locator = 'list_src_workspace_icon_'+i;
					// クーキーをアップデート
					window.localStorage.setItem(EIM_KEY_CURRENT_LIST_TOP_ELEMENT, JSON.stringify(ptop_locator));
				};
				
				ffile_extension = (pCurrentFolder[i]['objName']).substring((pCurrentFolder[i]['objName']).indexOf(".")+1);
				
				if (pCurrentFolder[i]['objTypeName']==cmn_objTypeName['フォルダ'])
				{
					// フォルダ
					ffolder_link_html1 = '<a class="list_src_workspace_info" id="list_src_workspace_info_'+i+'" href="#">';
					ffolder_link_html1_icon = '<a class="list_src_workspace_info" id="list_src_workspace_icon_'+i+'" href="#">';
					ficon_to_show = cmnGetimg['list_scr_folder_icon'];
					flinker = 'class="list_src_workspace_info" id="list_src_workspace_info_'+i+'"';
				}
				else 
				{
					// ファイル

					// ドキュメントをダウンロード
					if ((pCurrentFolder[i]['isPublished'] == true)&&(pCurrentFolder[i]['readOnly'] == true))
					{
						// ユーザーのセキュリチ―：パブリック（readOnly＝ＴＲＵＥ）
						// パブリックドキュメントが存在する場合
						// パブリックをダウンロード
						ffolder_link_html1 = '<a class="post_actSendReplyMail" target="_blank" id="list_src_workspace_icon_'+i+'" href="#">';
						ffolder_link_html1_icon = '<a class="post_actSendReplyMail" target="_blank" id="list_src_workspace_icon_'+i+'" href="'+cmnJSPAPI['JSPAPI_00010']+i+'">';
						flinker = 'onclick="var win = window.open(\''+cmnJSPAPI['JSPAPI_00010']+i+'\',\'_blank\');win.focus();$(\'#list_src_workspace_icon_'+i+'\').trigger(\'click\');"';
					}
					else if ((pCurrentFolder[i]['isPublished'] == false )&&(pCurrentFolder[i]['statusTypeName'] == "-"))
					{
						// 2014/12/15 CTC追記statusTypeName=- isPublished=false の場合は
						// public(公開文書)のダウンロードを可能
						ffolder_link_html1 = '<a class="post_actSendReplyMail" target="_blank" id="list_src_workspace_icon_'+i+'" href="#">';
						ffolder_link_html1_icon = '<a class="post_actSendReplyMail" target="_blank" id="list_src_workspace_icon_'+i+'" href="'+cmnJSPAPI['JSPAPI_00010']+i+'">';
						flinker = 'onclick="var win = window.open(\''+cmnJSPAPI['JSPAPI_00010']+i+'\',\'_blank\');win.focus();$(\'#list_src_workspace_icon_'+i+'\').trigger(\'click\');"';
					}
					else if ((pCurrentFolder[i]['isPublished'] == false )&&(pCurrentFolder[i]['readOnly'] == true))
					{
						// ユーザーのセキュリチ―：パブリック（readOnly＝ＴＲＵＥ）
						// パブリックドキュメントがまだ存在しない場合、パブリックダウンロードが出来ません
						ffolder_link_html1 = '<a class="" id="" href="#">';
						ffolder_link_html1_icon = '<a class="" id="" href="#">';
					}
					else if (pCurrentFolder[i]['readOnly'] == false)
					{
						// ユーザーのセキュリチ―：原本とパブリック（readOnly＝ＦＡＬＳＥ）
						// 原本をダウンロード
						ffolder_link_html1 = '<a class="post_actSendReplyMail" target="_blank" id="list_src_workspace_icon_'+i+'" href="#">';
						ffolder_link_html1_icon = '<a class="post_actSendReplyMail" target="_blank" id="list_src_workspace_icon_'+i+'" href="'+cmnJSPAPI['JSPAPI_00011']+i+'">';
						flinker = 'onclick="var win = window.open(\''+cmnJSPAPI['JSPAPI_00011']+i+'\',\'_blank\');win.focus();$(\'#list_src_workspace_icon_'+i+'\').trigger(\'click\');"';
					}
						
					switch(ffile_extension.substring(0,3)) 
					{
						case "xls":
							ficon_to_show = cmnGetimg['list_scr_document_excel']; break;
						case "XLS":
							ficon_to_show = cmnGetimg['list_scr_document_excel']; break;
						case "pdf":
							ficon_to_show = cmnGetimg['list_scr_document_pdf']; break;
						case "PDF":
							ficon_to_show = cmnGetimg['list_scr_document_pdf']; break;
						case "ppt":
							ficon_to_show = cmnGetimg['list_scr_document_ppt']; break;
						case "PPT":
							ficon_to_show = cmnGetimg['list_scr_document_ppt']; break;
						case "doc":
							ficon_to_show = cmnGetimg['list_scr_document_doc']; break;
						case "DOC":
							ficon_to_show = cmnGetimg['list_scr_document_doc']; break;
						default:
							ficon_to_show = cmnGetimg['list_scr_document'];
					};
				}
				
				fshorted_name = pCurrentFolder[i]['objName'];
				fshorted_property = pCurrentFolder[i]['property'];
				fshorted_name = resizeString(fshorted_name, viewName, 'landscape_folder_name');
				fshorted_property = resizeString(fshorted_property, viewName, 'landscape_folder_property');
				
				ffolderList_scr_list_item_html = ffolderList_scr_list_item_html + 
					'<li class="folderList_scr_list_item">'+
						'<table style="vertical-align: top;-webkit-border-horizontal-spacing: 0px; -webkit-border-vertical-spacing: 0px; ' + 
						'border: inset 0pt;border-style: hidden;border-collapse: collapse;border-spacing:0;width:100%;"><tr width="100%">' + 
						'<td width="8%" valign="top">' + ffolder_link_html1_icon + ficon_to_show + '</a></td>' +
						'<td width="45%" align="left" style="vertical-align: middle;"><div '+flinker+'>'+ffolder_link_html1+'<font style="font-size: 150%;font-weight: bold;">' + 
						fshorted_name+'</font><br>'+cmnLanguage['LAN_'+planguage+'_00016'] + ': '+pCurrentFolder[i]['modifyUserName']+' 　　' + 
						cmnLanguage['LAN_'+planguage+'_00020']+': '+pCurrentFolder[i]['modifyDate']+
						'<br>'+cmnLanguage['LAN_'+planguage+'_00022'] + ': '+pCurrentFolder[i]['history']+' 　　'+cmnLanguage['LAN_'+planguage+'_00019']+': ' + 
						pCurrentFolder[i]['statusTypeName']+' 　　'+cmnLanguage['LAN_'+planguage+'_00021']+': '+fshorted_property+
						ffolder_link_html2+'</div></td><td width="5%" style="vertical-align: middle;">'+
						'<table style="-webkit-border-horizontal-spacing: 0px; -webkit-border-vertical-spacing: 0px; ' + 
						'border: inset 0pt;border-style: hidden;border-collapse: collapse;border-spacing:0;width:100%;">' + 
						'<td align="middle"><p style="line-height: 400%;"><center><a class="list_scr_btn_Properties" id="list_scr_btn_Properties_'+i+'" href="#">' + 
						cmnGetimg['list_scr_btn_Properties']+'<br><font color="black">'+cmnLanguage['LAN_'+planguage+'_00015'] + 
						'</font></a></center></p></td></tr></table>'+
						'</td><td width="1%"></td></tr></table>'+
					'</li>';
			};
		}
		
		ffolderList_scr_list_item_html = ffolderList_scr_list_item_html + '</ul>';
		pfolderList_scr_list_item.empty().append(ffolderList_scr_list_item_html);

		make_breadcrumb_list();	
		
		if (psearch == true){
			// ボタンをＥＮＡＢＬＥする
			plist_scr_btn_Search.data('disabled',false);
		};
	}
	
	/********************************************************************************
	 * [機能]   パンくずボ変数のサイズを取得
	 * [引数]	無し
	 * [戻値]	パンくずボ変数のサイズ
	 ********************************************************************************/
	function calculate_breadcrumb_lenght (){
		var fbreadcrumb_lenght = 0;

		// パンくず変数をアップデート
		for (var key in pbreadcrumb){
			if (pbreadcrumb[key] != undefined){
				fbreadcrumb_lenght = fbreadcrumb_lenght + 1;	
			};
		}
		return fbreadcrumb_lenght;
	}
	
	/********************************************************************************
	 * [機能]   ＳＴＲＩＮＧパスを作る
	 * [引数]	無し
	 * [戻値]	ＳＴＲＩＮＧパス
	 ********************************************************************************/
	function build_path (){
		var fbreadcrumb = "";

		// パンくず変数をアップデート
		for (var key in pbreadcrumb_names){
			
			if (pbreadcrumb_names[key]!="0")
			{
				fbreadcrumb = fbreadcrumb + "/" + pbreadcrumb_names[key];
			};
		}
		fbreadcrumb = fbreadcrumb + "/";
		return fbreadcrumb;
	}
	
	
	/**
	 * 並び替え：０＝ワークスペース、１＝フォルダ
	 */
	function create_sorted_list(screen_type) {
		var sortable = [];
		var forigin;
		var selected_val="";
		var fnatural_order = 0;
		
		if (screen_type == 0)
		{
			forigin = pNodes;
			switch (pfolderList_scr_order_combo.val())
			{
				case cmnLanguage['LAN_JA_00016']: selected_val="_00016";break;
				case cmnLanguage['LAN_EN_00016']: selected_val="_00016";break;
				case cmnLanguage['LAN_JA_00017']: selected_val="_00017";break;
				case cmnLanguage['LAN_EN_00017']: selected_val="_00017";break;
				case cmnLanguage['LAN_JA_00018']: selected_val="_00018";break;
				case cmnLanguage['LAN_EN_00018']: selected_val="_00018";break;
				case cmnLanguage['LAN_JA_00021']: selected_val="_00021";break;
				case cmnLanguage['LAN_EN_00021']: selected_val="_00021";break;
				case cmnLanguage['LAN_JA_00025']: selected_val="_00025";break;
				case cmnLanguage['LAN_EN_00025']: selected_val="_00025";break;
				default: break;
			};
		}
		else
		{
			forigin = pCurrentFolder;
			switch (pfolderList_scr_order_combo.val())
			{
				case cmnLanguage['LAN_JA_00016']: selected_val="_00016";break;
				case cmnLanguage['LAN_EN_00016']: selected_val="_00016";break;
				case cmnLanguage['LAN_JA_00020']: selected_val="_00017";break;
				case cmnLanguage['LAN_EN_00020']: selected_val="_00017";break;
				case cmnLanguage['LAN_JA_00018']: selected_val="_00018";break;
				case cmnLanguage['LAN_EN_00018']: selected_val="_00018";break;
				case cmnLanguage['LAN_JA_00019']: selected_val="_00019";break;
				case cmnLanguage['LAN_EN_00019']: selected_val="_00019";break;
				case cmnLanguage['LAN_JA_00021']: selected_val="_00021";break;
				case cmnLanguage['LAN_EN_00021']: selected_val="_00021";break;
				case cmnLanguage['LAN_JA_00022']: selected_val="_00022";break;
				case cmnLanguage['LAN_EN_00022']: selected_val="_00022";break;
				case cmnLanguage['LAN_JA_00025']: selected_val="_00025";break;
				case cmnLanguage['LAN_EN_00025']: selected_val="_00025";break;
				default: break;
			};
		}
		
		// クーキーをアップデート
		window.localStorage.setItem(EIM_KEY_CURRENT_LIST_ORDER, JSON.stringify(selected_val));
		
		if ((screen_type == 0)&&(selected_val=="_00018"))
		{
			selected_val = "_00000";
		}
		
		for (var key in forigin)
		{
			sortable.push([key, forigin[key][cmnOrderBy['ORD'+selected_val]]]);
		};
		
		if (selected_val != "_00025")
		{
			sortable.sort(function(a, b) {return String(a[1].localeCompare(String(b[1])));});	
		}
		else
		{
			sortable.sort(function(a, b) {
					if (a[1]<b[1]) 
					{ 
						return -1;
					}
					else if (a[1]==b[1])
					{
						return 0;
					}
					else
					{
						return 1;
					};});
		}
		
		return sortable;		
	}
		
	/**
	 * お気に入り画面から来た場合、ＸＭＬでパンクずを作る
	 */
	function look_for_Folder (xml_object, depth)
	{
		// 変数にXMLの未承認件数情報をバインド
		xml_object.find("node").each(function(){
			if (pis_favorite_node != true){
				if (pfolderList_scr_workspace_infoId == $(this).attr('objId'))
				{
					pbreadcrumb[depth] = $(this).attr('objId');
					pbreadcrumb_names[depth] = $(this).attr('label');
					pis_favorite_node = true;
				}
				else 
				{
					look_for_Folder($(this), depth+1);
					if (pis_favorite_node == true)
					{
						pbreadcrumb[depth] = $(this).attr('objId');
						pbreadcrumb_names[depth] = $(this).attr('label');
					}
				}
			}
		});
	}
	
	function order_breadcrumbs(){
		var fsortable = [];
		var new_pbreadcrumb = new Object();
		var new_pbreadcrumb_names = new Object();
		
		for (var key in pbreadcrumb)
		{
			fsortable.push([key]);
		}
		
		fsortable.sort(function(a, b) {return String(a[0].localeCompare(String(b[0])));});
		
		for (var key in fsortable)
		{
			new_pbreadcrumb[key] = pbreadcrumb[key];
			if (key in pbreadcrumb_names)
			{
				new_pbreadcrumb_names[key] = pbreadcrumb_names[key];
			};
		}
		
		pbreadcrumb = new_pbreadcrumb;
		pbreadcrumb_names = new_pbreadcrumb_names;
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
		if (data==null)
		{
			pbreadcrumb = new Object();
			pbreadcrumb_names = new Object();
			pNodes = new Object();
			pCurrentFolder = new Object();
			pfolderName = "";
			pfolderId = "";
			psearch = false;
			pCurrentFolderSearchBackup = "";
			planguage = null;
			pfolderList_scr_workspace_infoId = "";
			ptop_locator = "";
			pcoming_from_favorites = false;
			if (pfolderList_scr_search_keyword!="")
			{
				pfolderList_scr_search_keyword.val("");	
			}
			if (pfolderList_scr_back_list != null)
			{
				pfolderList_scr_back_list.hide();	
			}
			if (pfolderList_scr_order_combo != null)
			{
				pfolderList_scr_order_combo.empty();
				pfolderList_scr_order_combo.selectmenu("refresh", true);
			}
			// リフレッシュ
			if (pfolderList_scr_breadcrumb != null){
				make_breadcrumb_list();
			}
		}
		else {
			// お気に入り画面にフォルダをクリックとフォルダＩＤを設定
			pfolderList_scr_workspace_infoId = data;
			// クーキーをアップデート
			window.localStorage.setItem(EIM_KEY_CURRENT_LIST_FOLDERID, JSON.stringify(pfolderList_scr_workspace_infoId));
			// お気に入りのフラッグ
			pcoming_from_favorites = true;
			// 検索フラッグをリセット
			psearch = false;
			ptop_locator = "";
			// 検索クーキーをアップデート
			window.localStorage.setItem(EIM_KEY_CURRENT_LIST_SEARCH_KEYWORD, JSON.stringify(""));
			window.localStorage.setItem(EIM_KEY_CURRENT_LIST_SEARCH, JSON.stringify(""));
			window.localStorage.setItem(EIM_KEY_CURRENT_LIST_TOP_ELEMENT, JSON.stringify(""));
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
