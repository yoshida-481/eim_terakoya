/**
 * properties_scr_Controller
 */
var properties_scr_Controller = function () {
	/*****************************************************************
	 * 宣言
	 ****************************************************************/
	function F() {};
	F.prototype = prototypeController;
	var f = new F();
	
	// ビュー
	const viewName = "properties_scr_View";
	const viewId = "#" + viewName;
	var view = null;
	// モデル
	const model = properties_scr_Model;
	
	// コンポーネント
	// properties_scr_fileName
	var properties_scr_fileName_1 = null;
	var properties_scr_fileName_2 = null;
	var properties_scr_fileName_3 = null;
	var properties_scr_fileName_4 = null;
	var properties_scr_info = null;
	// properties_scr_original → ダイナミックＨＴＭＬになりました
	// properties_scr_public → ダイナミックＨＴＭＬになりました
	// properties_scr_private_button_label → ダイナミックＨＴＭＬになりました
	// properties_scr_public_button_label → ダイナミックＨＴＭＬになりました
	
	// 変数
	var pCurrentFolder = new Object();
	var properties_scr_info_div = null;
	var pfolderList_scr_ObjId = "";
	var pisPublished = false;
	var pstatusTypeName = "";
	var preadOnly = false;
	var planguage = null;
	var pcollapsible = null;
	var pobjId = "";
	
	
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
		properties_scr_fileName_1 = $(viewId+'-'+'properties_scr_fileName_1');
		properties_scr_fileName_2 = $(viewId+'-'+'properties_scr_fileName_2');
		properties_scr_fileName_3 = $(viewId+'-'+'properties_scr_fileName_3');
		properties_scr_fileName_4 = $(viewId+'-'+'properties_scr_fileName_4');
		properties_scr_info = $(viewId+'-'+'properties_scr_info');
		
		// 半数
		properties_scr_info_div = $(viewId+'-'+'properties_scr_info_div');
		pcollapsible = $(viewId+'-'+'collapsible');
		
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
		 * ファイルをクリックイベントハンドラ
		 */
		jQuery(ev.target).on('click', $('.post_actSendReplyMail').selector, function() {
			//モデルメソッドの呼び出し
			model.post_actSendReplyMail(pfolderList_scr_ObjId);
		});
		
		/**
		 * 属性情報プラスボタンリックイベントハンドラ
		 */
		jQuery(ev.target).on('click', $('#properties_scr_View-collapsible_open').selector, function() {
			pcollapsible.empty().append('<a href="#" id="properties_scr_View-collapsible_close">'+
					cmnGetimg['check_scr_btn_History_minus']+"</a>");
			properties_scr_info_div.show();
		});
		
		/**
		 * 属性情報マイナスボタンリックイベントハンドラ
		 */
		jQuery(ev.target).on('click', $('#properties_scr_View-collapsible_close').selector, function() {
			properties_scr_info_div.hide();
			pcollapsible.empty().append('<a href="#" id="properties_scr_View-collapsible_open">'+
					cmnGetimg['check_scr_btn_History_plus']+"</a>");
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
		 * 「post_dspPropertySuccess」イベントハンドラ
		 */
		view.on('post_dspPropertySuccess', function(e, xml) {
			var fobjId = "";
			var fobjTypeId = "";
			
			pCurrentFolder = new Object();
			
			// 変数にXMLの未承認件数情報をバインド
			$(xml).find("object").each(function(){
			    var furl ="";
			    var fpath = "";
			    var fobjName = "";
			    var fhistory = "";
			    var fcreateUserName = "";
			    var fcreateDate = "";
			    var fmodifyUserName ="";
			    var fmodifyDate ="";
			    var fsecurityName = "";
			    var fdspObjTypeName = "";
			    var fobjTypeName = "";
			    var ffileSize = "";
			    var fexpireDate = "";
			    var fproperty = "";
			    var freadOnly = true;
			    var fstatusTypeName = "";
			    
			    // objIdタブ
			    fobjId = $(this).attr('objId');
			    
			    // objTypeIdタブ
			    fobjTypeId = $(this).attr('objTypeId');
			    
			    pCurrentFolder[fobjId] = new Object();
			    
			    // urlタブ
			    furl = $(this).attr('url');
			    if (furl == undefined)
			    {
			    	furl ="";
			    }
			    // pathタブ
			    fpath = $(this).attr('path');
			    if (fpath == undefined)
			    {
			    	fpath ="";
			    }
			    // objNameタブ
			    fobjName = $(this).attr('objName');
			    if (fobjName == undefined)
			    {
			    	fobjName ="";
			    }
			    // revタブ
			    fhistory = $(this).attr('rev');
			    if (fhistory == undefined)
			    {
			    	fhistory ="";
			    }
			    // createUserNameタブ
			    fcreateUserName = $(this).attr('createUserName');
			    if (fcreateUserName == undefined)
			    {
			    	fcreateUserName ="";
			    }
			    // createDateタブ
			    fcreateDate = $(this).attr('createDate');
			    if (fcreateDate == undefined)
			    {
			    	fcreateDate ="";
			    }
			    // modifyUserNameタブ
			    fmodifyUserName = $(this).attr('modifyUserName');
			    if (fmodifyUserName == undefined)
			    {
			    	fmodifyUserName ="";
			    }
			    // modifyDateタブ
			    fmodifyDate = $(this).attr('modifyDate');
			    if (fmodifyDate == undefined)
			    {
			    	fmodifyDate ="";
			    }
			    // securityNameタブ
			    fsecurityName = $(this).attr('securityName');
			    if (fsecurityName == undefined)
			    {
			    	fsecurityName ="";
			    }
			    // propertyタブ
			    fproperty = $(this).attr('property');
			    if (fproperty == undefined)
			    {
			    	fproperty ="";
			    }
			    // dspObjTypeNameタブ
			    fdspObjTypeName = $(this).attr('dspObjTypeName');
			    if (fdspObjTypeName == undefined)
			    {
			    	fdspObjTypeName ="";
			    }
			    // dspObjTypeNameタブ
			    fobjTypeName = JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_PROPERTY_OBJTYPENAME));
			    
			    // fileSizeタブ
			    ffileSize = $(this).attr('fileSize');
			    if (ffileSize == undefined)
			    {
			    	ffileSize ="";
			    }
			    // expireDateタブ
			    fexpireDate = $(this).attr('expireDate');
			    if (fexpireDate == undefined)
			    {
			    	fexpireDate ="";
			    }
			    // statusTypeNameタブ
			    fstatusTypeName = $(this).attr('statusTypeName');
			    if (fstatusTypeName == undefined)
			    {
			    	fstatusTypeName ="";
			    }
			    
			    // readOnlyタブ
			    freadOnly = preadOnly;
			    
			    pCurrentFolder[fobjId]['url'] = furl;
			    pCurrentFolder[fobjId]['path'] = fpath;
			    pCurrentFolder[fobjId]['objName'] = fobjName;
			    pCurrentFolder[fobjId]['history'] = fhistory;
			    pCurrentFolder[fobjId]['createUserName'] = fcreateUserName;
			    pCurrentFolder[fobjId]['createDate'] = fcreateDate;
			    pCurrentFolder[fobjId]['modifyUserName'] = fmodifyUserName;
			    pCurrentFolder[fobjId]['modifyDate'] = fmodifyDate;
			    pCurrentFolder[fobjId]['securityName'] = fsecurityName;
			    pCurrentFolder[fobjId]['property'] = fproperty;
			    pCurrentFolder[fobjId]['dspObjTypeName'] = fdspObjTypeName;
			    pCurrentFolder[fobjId]['objTypeName'] = fobjTypeName;
			    pCurrentFolder[fobjId]['fileSize'] = ffileSize;
			    pCurrentFolder[fobjId]['expireDate'] = fexpireDate;
			    pCurrentFolder[fobjId]['readOnly'] = freadOnly;
			    pCurrentFolder[fobjId]['statusTypeName'] = fstatusTypeName;
			});
			
			pobjId = fobjId;
			
			// ドキュメントのダイナミックなアトリビュートを取得
			model.post_dspAttribute(fobjId,fobjTypeId);
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
		 * 「post_dspAttributeSuccess」イベントハンドラ
		 */
		view.on('post_dspAttributeSuccess', function(e, xml) {
			
			// ドキュメントのダイナミックなアトリビュートを取得
			pCurrentFolder['Attributes'] = new Object();
			
			// 変数にXMLの未承認件数情報をバインド
			$(xml).find("attribute").each(function(){
				
			    var fattTypeId ="";
			    
			    // attTypeIdタブ
			    fattTypeId = $(this).attr('attTypeId');
			    
			    // 管理者で作ったアトリビュートのＩＤは３桁以上
			    if (fattTypeId.length > 3)
			    {
			    	// 言語によって、attTypeNameかattTypeDefNameタブ
			    	var fattrMultiple = [];
			    	fattrMultiple.push($(this).attr('attValue'));
		    		
			    	if (planguage=='JA')
			    	{
			    		pCurrentFolder['Attributes'][$(this).attr('attTypeDefName')] = JSON.stringify(fattrMultiple);
			    	}
			    	else if (planguage=='EN')
			    	{
			    		pCurrentFolder['Attributes'][$(this).attr('attTypeName')] = JSON.stringify(fattrMultiple);
			    	};
			    	
			    	var ffound = false;
			    	$(this).find("attMultiple").each(function()
			    	{
			    		if (ffound == false)
			    		{
			    			ffound = true;
			    			fattrMultiple = [];
			    		}
			    		fattrMultiple.push($(this).attr('attValue'));
			    	});
			    	if (ffound == true)
			    	{
			    		pCurrentFolder['Attributes'][$(this).attr('attTypeName')] = JSON.stringify(fattrMultiple);
			    	};
			    }
			    else if ($(this).attr('attTypeDefName') == "改訂内容") 
			    {
			    	// 改訂内容タグの特別なケース
			    	var fattrMultiple = [];
			    	fattrMultiple.push(JSON.stringify($(this).attr('attValue')).replace('"','').replace('"',''));
		    		
			    	if (planguage=='JA')
			    	{
			    		pCurrentFolder['Attributes'][$(this).attr('attTypeDefName')] = JSON.stringify(fattrMultiple);
			    	}
			    	else if (planguage=='EN')
			    	{
			    		pCurrentFolder['Attributes'][$(this).attr('attTypeName')] = JSON.stringify(fattrMultiple);
			    	};
			    }
			    else if ($(this).attr('attTypeDefName') == "有効期限") 
			    {
			    	if (pCurrentFolder[pobjId]['objTypeName'].indexOf(cmn_objTypeName['フォルダ']) >= 0)
			    	{
			    		// 改訂内容タグの特別なケース
				    	var fattrMultiple = [];
				    	fattrMultiple.push(JSON.stringify($(this).attr('attValue')).replace('"','').replace('"',''));
			    		
				    	if (planguage=='JA')
				    	{
				    		pCurrentFolder['Attributes'][$(this).attr('attTypeDefName')] = JSON.stringify(fattrMultiple);
				    	}
				    	else if (planguage=='EN')
				    	{
				    		pCurrentFolder['Attributes'][$(this).attr('attTypeName')] = JSON.stringify(fattrMultiple);
				    	};	
			    	}
			    };
			});
			
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
		 * 「post_dspAttributeError」イベントハンドラ
		 */
		view.on('post_dspAttributeError', function(e, xml) {
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
		 * 「post_actSendReplyMailSuccess」イベントハンドラ
		 */
		view.on('post_actSendReplyMailSuccess', function(e, xml) {
			// 何もしない;
		});		
		
		/**
		 * 「post_actSendReplyMailError」イベントハンドラ
		 */
		view.on('post_actSendReplyMailError', function(e, xml) {
			// エラーメッセージを表示する。
			$(xml).find("error").each(function(){
				cmnPopup($(this).attr('message'),"ERR",viewName);
				if (($(this).attr('message').indexOf('タイムアウト')>=0)||($(this).attr('message').indexOf('timeout')>=0))
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
		model.post_dspApproveDocumentList();
	});
	
	jQuery(document).on('pagebeforeshow', viewId, function(ev) {
		
		// ボタンリセット
		$("#properties_scr_View-properties_scr_btn_Original").removeClass('ui-disabled');
		$("#properties_scr_View-properties_scr_btn_Public").removeClass('ui-disabled');

		// イメージ・アイコン
		document.getElementById("properties_scr_View-properties_scr_btn_Original").style.background='-webkit-linear-gradient(top, #effaff, #e0f5ff)';
		document.getElementById("properties_scr_View-properties_scr_btn_Public").style.background='-webkit-linear-gradient(top, #fff5eb, #fae8d6)';
		
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
		
		// ＯＳによって、アイコンのサイズを変わる
		if ($(document.body).height()>800)
		{
			cmn_change_image_sizes (0, planguage);
		}
		else
		{
			cmn_change_image_sizes (1, planguage);
		}
		
		// ヘッダーのタイトル
		$('.header_title').empty();
		
		// イメージ・アイコン
		$("#properties_scr_View-properties_scr_btn_Original").removeClass('properties_scr_View-scr_btn_set_blur'); 
		$("#properties_scr_View-properties_scr_btn_Original").removeClass('properties_scr_View-scr_btn_set_blur'); 
		
		// パンクずと検索を非表示
		$('.folderList_scr_View-folderList_scr_order_combo_div').hide();
		$('.folderList_scr_View-folderList_scr_bc').hide();
		
		$('.header_title').empty().append(cmnLanguage['LAN_'+planguage+'_00046']);
		
		$('#properties_scr_View-collapsible_text').empty().append(cmnLanguage['LAN_'+planguage+'_00046']);
		
		// 属性情報を表示
		properties_scr_info_div.show();
		pcollapsible.empty().append('<a href="#" id="properties_scr_View-collapsible_close">'+cmnGetimg['check_scr_btn_History_minus']+"</a>");
		
		// データーをクリア
		properties_scr_fileName_1.empty();
		properties_scr_fileName_2.empty();
		properties_scr_fileName_3.empty();
		properties_scr_fileName_4.empty();
		properties_scr_info.empty();
		$('#properties_scr_View-properties_scr_btn_Original_div').hide();
		$('#properties_scr_View-properties_scr_btn_Public_div').hide();
		
		// クーキーからisPublishedタグを取得
		if (JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_PROPERTY_ISPUBLISHED)) != null)
		{
			pisPublished = JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_PROPERTY_ISPUBLISHED));
		}
		else 
		{
			pisPublished = false;	
		}
		// クーキーからstatusTypeNameタグを取得
		if (JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_PROPERTY_STATUSTYPENAME)) != null)
		{
			pstatusTypeName = JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_PROPERTY_STATUSTYPENAME));
		}
		else 
		{
			pstatusTypeName = "";	
		}
		// クーキーからreadOnlyタグを取得
		if (JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_PROPERTY_READONLY)) != null)
		{
			preadOnly = JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_PROPERTY_READONLY));
		}
		else 
		{
			preadOnly = false;
		}
		if (JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_PROPERTY_ELEMENT)) != null)
		{
			pfolderList_scr_ObjId = JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_PROPERTY_ELEMENT));
		}
		
		model.post_dspProperty(pfolderList_scr_ObjId);
		
		pcurrentView = viewName;
		
		if (JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_FORMER_VIEW)) != null)
		{
			// ボタンハイライト
			if (JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_FORMER_VIEW)) == "check_scr_View")
			{
				cmn_header_button_highlight (2);
			}
			else if (JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_FORMER_VIEW)) == "folderList_scr_View")
			{
				cmn_header_button_highlight (1);	
			}
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
	 * [機能]	縦向きになった場合
	 * [引数]	無し
	 * [戻値]	無し
	 ********************************************************************************/
	function resize_screen_portrait() {
		var fhtml_info = "";
		var fhtml_type = "";
		var fhtml_file_data = "";
		var ffile_extension = "";
		var ficon_to_show = "";
		var flcounter = 0;
		var fname = "";
		
		// ＯＳによって、アイコンのサイズを変わる
		cmn_change_image_sizes (0, planguage);
		if (pcollapsible.html().indexOf('collapsible_close')>0)
		{	
			pcollapsible.empty().append('<a href="#" id="properties_scr_View-collapsible_close">'+
					cmnGetimg['check_scr_btn_History_minus']+"</a>");
		}
		else 
		{
			pcollapsible.empty().append('<a href="#" id="properties_scr_View-collapsible_open">'+
					cmnGetimg['check_scr_btn_History_plus']+"</a>");	
		}
		
		ffile_extension = (pCurrentFolder[pfolderList_scr_ObjId]['objName']).substring((pCurrentFolder[pfolderList_scr_ObjId]['objName']).indexOf(".")+1);
		
		if (pCurrentFolder[pfolderList_scr_ObjId]['objTypeName'].indexOf(cmn_objTypeName['ワークスペース']) >= 0)
		{
			ficon_to_show = '<a href="#">'+ cmnGetimg['list_src_workspace_icon']+'</a>';
		}
		else if ((pCurrentFolder[pfolderList_scr_ObjId]['objTypeName']).indexOf(cmn_objTypeName['フォルダ']) >= 0)
		{
			ficon_to_show = '<a href="#">'+ cmnGetimg['list_scr_folder_icon']+'</a>';
			// フォルダの「dspObjTypeName」タブを設定
			fhtml_type = '<tr style="height: 35px;"><td></td><td><font style="font-size: 100%;font-weight: bold;">'+
			cmnLanguage['LAN_'+planguage+'_00040']+'</font></td><td>'+
			pCurrentFolder[pfolderList_scr_ObjId]['dspObjTypeName']+'</td><td>'+'</td></tr>';
		}
		else 
		{
			switch(ffile_extension.substring(0,3)) 
			{
				case "xls":
					ficon_to_show = '<a href="#">'+cmnGetimg['list_scr_document_excel']+'</a>'; break;
				case "XLS":
					ficon_to_show = '<a href="#">'+cmnGetimg['list_scr_document_excel']+'</a>'; break;
				case "pdf":
					ficon_to_show = '<a href="#">'+cmnGetimg['list_scr_document_pdf']+'</a>'; break;
				case "PDF":
					ficon_to_show = '<a href="#">'+cmnGetimg['list_scr_document_pdf']+'</a>'; break;
				case "ppt":
					ficon_to_show = '<a href="#">'+cmnGetimg['list_scr_document_ppt']+'</a>'; break;
				case "PPT":
					ficon_to_show = '<a href="#">'+cmnGetimg['list_scr_document_ppt']+'</a>'; break;
				case "doc":
					ficon_to_show = '<a href="#">'+cmnGetimg['list_scr_document_doc']+'</a>'; break;
				case "DOC":
					ficon_to_show = '<a href="#">'+cmnGetimg['list_scr_document_doc']+'</a>'; break;
				default:
					ficon_to_show = '<a href="#">'+cmnGetimg['list_scr_document']+'</a>';
			};

			// ドキュメントをダウンロード
			if (((pisPublished == true)||((pisPublished == false )&&(pstatusTypeName == "-")))&&(pCurrentFolder[pfolderList_scr_ObjId]['readOnly'] == true))
			{
				// 2014/12/15 CTC追記statusTypeName=- isPublished=false の場合は
				// public(公開文書)のダウンロードを可能

				// ユーザーのセキュリチ―：パブリック（readOnly＝ＴＲＵＥ）
				// パブリックドキュメントが存在する場合
				// パブリックをダウンロード
				$("#properties_scr_View-properties_scr_btn_Original").attr("href", "");
				$("#properties_scr_View-properties_scr_btn_Original").addClass('ui-disabled');
				document.getElementById("properties_scr_View-properties_scr_btn_Original").style.background='-webkit-linear-gradient(top, #777777, #999999)';
				$("#properties_scr_View-properties_scr_btn_Public").attr("href", cmnJSPAPI['JSPAPI_00010'] + pfolderList_scr_ObjId);
			}
			else if ((pisPublished == false)&&(pCurrentFolder[pfolderList_scr_ObjId]['readOnly'] == true))
			{
				// ユーザーのセキュリチ―：パブリック（readOnly＝ＴＲＵＥ）
				// パブリックドキュメントがまだ存在しない場合、パブリックダウンロードが出来ません
				$("#properties_scr_View-properties_scr_btn_Original").attr("href", "");
				$("#properties_scr_View-properties_scr_btn_Original").addClass('ui-disabled');
				$("#properties_scr_View-properties_scr_btn_Public").attr("href", "");
				$("#properties_scr_View-properties_scr_btn_Public").addClass('ui-disabled');
				document.getElementById("properties_scr_View-properties_scr_btn_Original").style.background='-webkit-linear-gradient(top, #777777, #999999)';
				document.getElementById("properties_scr_View-properties_scr_btn_Public").style.background='-webkit-linear-gradient(top, #777777, #999999)';
			}
			else if (((pisPublished == true)||((pisPublished == false )&&(pstatusTypeName == "-")))&&(pCurrentFolder[pfolderList_scr_ObjId]['readOnly'] == false))
			{
				// 2014/12/15 CTC追記statusTypeName=- isPublished=false の場合は
				// public(公開文書)のダウンロードを可能

				// ユーザーのセキュリチ―：原本とパブリック（readOnly＝ＦＡＬＳＥ）
				// 原本とパブリックのダウンロードが出来る
				$("#properties_scr_View-properties_scr_btn_Original").attr("href", cmnJSPAPI['JSPAPI_00011'] + pfolderList_scr_ObjId);
				$("#properties_scr_View-properties_scr_btn_Public").attr("href", cmnJSPAPI['JSPAPI_00010'] + pfolderList_scr_ObjId);
			}
			else if ((pisPublished == false)&&(pCurrentFolder[pfolderList_scr_ObjId]['readOnly'] == false))
			{
				// ユーザーのセキュリチ―：原本とパブリック（readOnly＝ＦＡＬＳＥ）
				// パブリックドキュメントがまだ存在しない場合、パブリックのダウンロードが出来ない
				// 原本のダウンロードが出来る
				$("#properties_scr_View-properties_scr_btn_Original").attr("href", cmnJSPAPI['JSPAPI_00011'] + pfolderList_scr_ObjId);
				$("#properties_scr_View-properties_scr_btn_Public").attr("href", "");
				$("#properties_scr_View-properties_scr_btn_Public").addClass('ui-disabled');
				document.getElementById("properties_scr_View-properties_scr_btn_Public").style.background='-webkit-linear-gradient(top, #777777, #999999)';
			}

			// ファイルの「dspObjTypeName」と「expireDate」タブを設定
			fhtml_type = '<tr style="height: 35px;"><td></td><td><font style="font-size: 100%;font-weight: bold;">'+
			cmnLanguage['LAN_'+planguage+'_00041']+'</font></td><td>'+
			pCurrentFolder[pfolderList_scr_ObjId]['dspObjTypeName']+'</td><td>'+'</td></tr>';
			
			fhtml_file_data = '<tr style="height: 35px;"><td></td><td><font style="font-size: 100%;font-weight: bold;">'+
			cmnLanguage['LAN_'+planguage+'_00042']+'</font></td><td>'+
			pCurrentFolder[pfolderList_scr_ObjId]['fileSize']+'</td><td>'+'</td></tr>'+
			'<tr style="height: 35px;"><td></td><td><font style="font-size: 100%;font-weight: bold;">'+
			cmnLanguage['LAN_'+planguage+'_00043']+'</font></td><td>'+
			english_Date(timestamp_to_Date(pCurrentFolder[pfolderList_scr_ObjId]['expireDate']),planguage)+'</td><td>'+'</td></tr>';
			
			// ダウンロードボタンを表示
			$('#properties_scr_View-properties_scr_btn_Original_div').show();
			$('#properties_scr_View-properties_scr_btn_Public_div').show();
		}
		
		properties_scr_fileName_1.empty().append(ficon_to_show);

		fname = pCurrentFolder[pfolderList_scr_ObjId]['objName'];
		if (fname.length > cmnAddPoints['properties_scr_View_portrait'])
		{
			fname = fname.substring(0,cmnAddPoints['properties_scr_View_portrait'])+"...";
		}
		properties_scr_fileName_2.empty().append(fname);
		properties_scr_fileName_3.empty().append(cmnLanguage['LAN_'+planguage+'_00044']);
		properties_scr_fileName_4.empty().append(cmnLanguage['LAN_'+planguage+'_00045']);
		
		fhtml_info = fhtml_info + 
			'<table width="100%">'+
			'<tr style="height: 20px;"><td width="15%"></td><td width="30%"><font style="font-size: 100%;font-weight: bold;">'+
			'</font></td><td width="40%">'+'</td><td width="15%">'+'</td></tr>'+
			'<tr style="height: 35px;"><td></td><td><font style="font-size: 100%;font-weight: bold;">'+
			cmnLanguage['LAN_'+planguage+'_00018']+'</font></td><td>'+
			pCurrentFolder[pfolderList_scr_ObjId]['objName']+'</td><td>'+'</td></tr>'+
			'<tr style="height: 35px;"><td></td><td><font style="font-size: 100%;font-weight: bold;">'+
			cmnLanguage['LAN_'+planguage+'_00036']+'</font></td><td>'+
			pCurrentFolder[pfolderList_scr_ObjId]['url']+'</td><td>'+'</td></tr>'+
			'<tr style="height: 35px;"><td></td><td><font style="font-size: 100%;font-weight: bold;">'+
			cmnLanguage['LAN_'+planguage+'_00027']+'</font></td><td>'+
			pCurrentFolder[pfolderList_scr_ObjId]['path']+'</td><td>'+'</td></tr>'+
			fhtml_type +
			'<tr style="height: 35px;"><td></td><td><font style="font-size: 100%;font-weight: bold;">'+
			cmnLanguage['LAN_'+planguage+'_00022']+'</font></td><td>'+
			pCurrentFolder[pfolderList_scr_ObjId]['history']+'</td><td>'+'</td></tr>'+
			'<tr style="height: 35px;"><td></td><td><font style="font-size: 100%;font-weight: bold;">'+
			cmnLanguage['LAN_'+planguage+'_00037']+'</font></td><td>'+
			pCurrentFolder[pfolderList_scr_ObjId]['createUserName']+'</td><td>'+'</td></tr>'+
			'<tr style="height: 35px;"><td></td><td><font style="font-size: 100%;font-weight: bold;">'+
			cmnLanguage['LAN_'+planguage+'_00038']+'</font></td><td>'+
			pCurrentFolder[pfolderList_scr_ObjId]['createDate']+'</td><td>'+'</td></tr>'+
			'<tr style="height: 35px;"><td></td><td><font style="font-size: 100%;font-weight: bold;">'+
			cmnLanguage['LAN_'+planguage+'_00016']+'</font></td><td>'+
			pCurrentFolder[pfolderList_scr_ObjId]['modifyUserName']+'</td><td>'+'</td></tr>'+
			'<tr style="height: 35px;"><td></td><td><font style="font-size: 100%;font-weight: bold;">'+
			cmnLanguage['LAN_'+planguage+'_00017']+'</font></td><td>'+
			pCurrentFolder[pfolderList_scr_ObjId]['modifyDate']+'</td><td>'+'</td></tr>'+
			'<tr style="height: 35px;"><td></td><td><font style="font-size: 100%;font-weight: bold;">'+
			cmnLanguage['LAN_'+planguage+'_00019']+'</font></td><td>'+
			pCurrentFolder[pfolderList_scr_ObjId]['statusTypeName']+'</td><td>'+'</td></tr>'+
			'<tr style="height: 35px;"><td></td><td><font style="font-size: 100%;font-weight: bold;">'+
			cmnLanguage['LAN_'+planguage+'_00039']+'</font></td><td>'+
			pCurrentFolder[pfolderList_scr_ObjId]['securityName']+'</td><td>'+'</td></tr>'+
			'<tr style="height: 35px;"><td></td><td><font style="font-size: 100%;font-weight: bold;">'+
			cmnLanguage['LAN_'+planguage+'_00021']+'</font></td><td>'+
			pCurrentFolder[pfolderList_scr_ObjId]['property']+'</td><td>'+'</td></tr>';
		
		fhtml_info = fhtml_info + fhtml_file_data;

		for (var mykey in pCurrentFolder['Attributes'])
		{
			var fattr_list = [];
			fattr_list = JSON.parse(pCurrentFolder['Attributes'][mykey]);
			
			var fitems = 0;
			for (var mykey2 in fattr_list)
			{
				fitems = fitems + 1;
				flcounter = flcounter + 1;
				fhtml_info = fhtml_info + 
					'<tr style="height: 35px;"><td></td><td><font style="font-size: 100%;font-weight: bold;">';
				if (fitems == 1)
				{
					fhtml_info = fhtml_info +mykey;	
					fhtml_info = fhtml_info + '</font></td><td>'+fattr_list[mykey2]+'</td><td>'+'</td></tr>';
				}
				else 
				{
					fhtml_info = fhtml_info + '</font></td><td>'+fattr_list[mykey2]+'</td><td>'+'</td></tr>';	
				};
			};
		}
		
		fhtml_info = fhtml_info + '</table>';
		
		properties_scr_info.empty().append(fhtml_info);
		
		var style2 = document.createElement('style');
		style2.type = 'text/css';

		if ((navigator.userAgent.indexOf("iPad")>0)&&((navigator.userAgent.indexOf("OS 8")>0)||(navigator.userAgent.indexOf("OS 7")>0)))
		{
			flcounter = (flcounter * 35) + 635;
			style2.innerHTML = '#properties_scr_View-properties_scr_info_div {height: '+flcounter+'px;}';
		}
		else if (navigator.userAgent.indexOf("iPad")>0)
		{
			flcounter = (flcounter * 35) + 555;
			style2.innerHTML = '#properties_scr_View-properties_scr_info_div {height:'+flcounter+'px;}';
		}
		else if (navigator.userAgent.indexOf("iPhone")>0)
		{
			flcounter = (flcounter * 35) + 1045;
			style2.innerHTML = '#properties_scr_View-properties_scr_info_div {height:'+flcounter+'px;}';
		}
		else 
		{
			flcounter = (flcounter * 35) + 555;
			style2.innerHTML = '#properties_scr_View-properties_scr_info_div {height:'+flcounter+'px;}';
		}
		document.getElementsByTagName('head')[0].appendChild(style2);
	}
	
	/********************************************************************************
	 * [機能]	横向きになった場合
	 * [引数]	無し
	 * [戻値]	無し
	 ********************************************************************************/
	function resize_screen_landscape() {
		var fhtml_info = "";
		var fhtml_type = "";
		var fhtml_file_data = "";
		var ffile_extension = "";
		var ficon_to_show = "";
		var flcounter = 0;
		var fname = "";
		
		// ＯＳによって、アイコンのサイズを変わる
		cmn_change_image_sizes (1, planguage);
		if (pcollapsible.html().indexOf('collapsible_close')>0)
		{	
			pcollapsible.empty().append('<a href="#" id="properties_scr_View-collapsible_close">'+
					cmnGetimg['check_scr_btn_History_minus']+"</a>");
		}
		else 
		{
			pcollapsible.empty().append('<a href="#" id="properties_scr_View-collapsible_open">'+
					cmnGetimg['check_scr_btn_History_plus']+"</a>");	
		}
		
		ffile_extension = (pCurrentFolder[pfolderList_scr_ObjId]['objName']).substring((pCurrentFolder[pfolderList_scr_ObjId]['objName']).lastIndexOf(".")+1);
		
		if (pCurrentFolder[pfolderList_scr_ObjId]['objTypeName'].indexOf(cmn_objTypeName['ワークスペース']) >= 0)
		{
			ficon_to_show = '<a href="#">'+ cmnGetimg['list_src_workspace_icon']+'</a>';
		}
		else if (((pCurrentFolder[pfolderList_scr_ObjId]['objTypeName']).indexOf(cmn_objTypeName['フォルダ']) >= 0))
		{
			ficon_to_show = '<a href="#">'+ cmnGetimg['list_scr_folder_icon']+'</a>';
			// フォルダの「dspObjTypeName」タブを設定
			fhtml_type = '<tr style="height: 35px;"><td></td><td><font style="font-size: 100%;font-weight: bold;">'+
			cmnLanguage['LAN_'+planguage+'_00040']+'</font></td><td>'+
			pCurrentFolder[pfolderList_scr_ObjId]['dspObjTypeName']+'</td><td>'+'</td></tr>';
		}
		else 
		{
			switch(ffile_extension.substring(0,3)) 
			{
				case "xls":
					ficon_to_show = '<a href="#">'+cmnGetimg['list_scr_document_excel']+'</a>'; break;
				case "XLS":
					ficon_to_show = '<a href="#">'+cmnGetimg['list_scr_document_excel']+'</a>'; break;
				case "pdf":
					ficon_to_show = '<a href="#">'+cmnGetimg['list_scr_document_pdf']+'</a>'; break;
				case "PDF":
					ficon_to_show = '<a href="#">'+cmnGetimg['list_scr_document_pdf']+'</a>'; break;
				case "ppt":
					ficon_to_show = '<a href="#">'+cmnGetimg['list_scr_document_ppt']+'</a>'; break;
				case "PPT":
					ficon_to_show = '<a href="#">'+cmnGetimg['list_scr_document_ppt']+'</a>'; break;
				case "doc":
					ficon_to_show = '<a href="#">'+cmnGetimg['list_scr_document_doc']+'</a>'; break;
				case "DOC":
					ficon_to_show = '<a href="#">'+cmnGetimg['list_scr_document_doc']+'</a>'; break;
				default:
					ficon_to_show = '<a href="#">'+cmnGetimg['list_scr_document']+'</a>';
			};
			
			// ドキュメントをダウンロード
			if (((pisPublished == true)||((pisPublished == false )&&(pstatusTypeName == "-")))&&(pCurrentFolder[pfolderList_scr_ObjId]['readOnly'] == true))
			{
				// 2014/12/15 CTC追記statusTypeName=- isPublished=false の場合は
				// public(公開文書)のダウンロードを可能

				// ユーザーのセキュリチ―：パブリック（readOnly＝ＴＲＵＥ）
				// パブリックドキュメントが存在する場合
				// パブリックをダウンロード
				$("#properties_scr_View-properties_scr_btn_Original").attr("href", "");
				$("#properties_scr_View-properties_scr_btn_Original").addClass('ui-disabled');
				document.getElementById("properties_scr_View-properties_scr_btn_Original").style.background='-webkit-linear-gradient(top, #777777, #999999)';
				$("#properties_scr_View-properties_scr_btn_Public").attr("href", cmnJSPAPI['JSPAPI_00010'] + pfolderList_scr_ObjId);
			}
			else if ((pisPublished == false)&&(pCurrentFolder[pfolderList_scr_ObjId]['readOnly'] == true))
			{
				// ユーザーのセキュリチ―：パブリック（readOnly＝ＴＲＵＥ）
				// パブリックドキュメントがまだ存在しない場合、パブリックダウンロードが出来ません
				$("#properties_scr_View-properties_scr_btn_Original").attr("href", "");
				$("#properties_scr_View-properties_scr_btn_Original").addClass('ui-disabled');
				$("#properties_scr_View-properties_scr_btn_Public").attr("href", "");
				$("#properties_scr_View-properties_scr_btn_Public").addClass('ui-disabled');
				document.getElementById("properties_scr_View-properties_scr_btn_Original").style.background='-webkit-linear-gradient(top, #777777, #999999)';
				document.getElementById("properties_scr_View-properties_scr_btn_Public").style.background='-webkit-linear-gradient(top, #777777, #999999)';
			}
			else if (((pisPublished == true)||((pisPublished == false )&&(pstatusTypeName == "-")))&&(pCurrentFolder[pfolderList_scr_ObjId]['readOnly'] == false))
			{
				// 2014/12/15 CTC追記statusTypeName=- isPublished=false の場合は
				// public(公開文書)のダウンロードを可能

				// ユーザーのセキュリチ―：原本とパブリック（readOnly＝ＦＡＬＳＥ）
				// 原本とパブリックのダウンロードが出来る
				$("#properties_scr_View-properties_scr_btn_Original").attr("href", cmnJSPAPI['JSPAPI_00011'] + pfolderList_scr_ObjId);
				$("#properties_scr_View-properties_scr_btn_Public").attr("href", cmnJSPAPI['JSPAPI_00010'] + pfolderList_scr_ObjId);
			}
			else if ((pisPublished == false)&&(pCurrentFolder[pfolderList_scr_ObjId]['readOnly'] == false))
			{
				// ユーザーのセキュリチ―：原本とパブリック（readOnly＝ＦＡＬＳＥ）
				// パブリックドキュメントがまだ存在しない場合、パブリックのダウンロードが出来ない
				// 原本のダウンロードが出来る
				$("#properties_scr_View-properties_scr_btn_Original").attr("href", cmnJSPAPI['JSPAPI_00011'] + pfolderList_scr_ObjId);
				$("#properties_scr_View-properties_scr_btn_Public").attr("href", "");
				$("#properties_scr_View-properties_scr_btn_Public").addClass('ui-disabled');
				document.getElementById("properties_scr_View-properties_scr_btn_Public").style.background='-webkit-linear-gradient(top, #777777, #999999)';
			}
			
			// ファイルの「dspObjTypeName」と「expireDate」タブを設定
			fhtml_type = '<tr style="height: 35px;"><td></td><td><font style="font-size: 100%;font-weight: bold;">'+
			cmnLanguage['LAN_'+planguage+'_00041']+'</font></td><td>'+
			pCurrentFolder[pfolderList_scr_ObjId]['dspObjTypeName']+'</td><td>'+'</td></tr>';
			
			fhtml_file_data = '<tr style="height: 35px;"><td></td><td><font style="font-size: 100%;font-weight: bold;">'+
			cmnLanguage['LAN_'+planguage+'_00042']+'</font></td><td>'+
			pCurrentFolder[pfolderList_scr_ObjId]['fileSize']+'</td><td>'+'</td></tr>'+
			'<tr style="height: 35px;"><td></td><td><font style="font-size: 100%;font-weight: bold;">'+
			cmnLanguage['LAN_'+planguage+'_00043']+'</font></td><td>'+
			english_Date(timestamp_to_Date(pCurrentFolder[pfolderList_scr_ObjId]['expireDate']),planguage)+'</td><td>'+'</td></tr>';
			
			// ダウンロードボタンを表示
			$('#properties_scr_View-properties_scr_btn_Original_div').show();
			$('#properties_scr_View-properties_scr_btn_Public_div').show();
		}
		
		properties_scr_fileName_1.empty().append(ficon_to_show);
		fname = pCurrentFolder[pfolderList_scr_ObjId]['objName'];
		if (fname.length > cmnAddPoints['properties_scr_View_landscape'])
		{
			fname = fname.substring(0,cmnAddPoints['properties_scr_View_landscape'])+"...";
		}
		properties_scr_fileName_2.empty().append(fname);
		properties_scr_fileName_3.empty().append(cmnLanguage['LAN_'+planguage+'_00044']);
		properties_scr_fileName_4.empty().append(cmnLanguage['LAN_'+planguage+'_00045']);
	    
		fhtml_info = fhtml_info + 
			'<table width="100%">'+
			'<tr style="height: 20px;"><td width="15%"></td><td width="20%"><font style="font-size: 100%;font-weight: bold;">'+
			'</font></td><td width="50%">'+'</td><td width="15%">'+'</td></tr>'+
			'<tr style="height: 35px;"><td width="20%"></td><td width="20%"><font style="font-size: 100%;font-weight: bold;">'+
			cmnLanguage['LAN_'+planguage+'_00018']+'</font></td><td width="40%">'+
			pCurrentFolder[pfolderList_scr_ObjId]['objName']+'</td><td width="20%">'+'</td></tr>'+
			'<tr style="height: 35px;"><td></td><td><font style="font-size: 100%;font-weight: bold;">'+
			cmnLanguage['LAN_'+planguage+'_00036']+'</font></td><td>'+
			pCurrentFolder[pfolderList_scr_ObjId]['url']+'</td><td>'+'</td></tr>'+
			'<tr style="height: 35px;"><td></td><td><font style="font-size: 100%;font-weight: bold;">'+
			cmnLanguage['LAN_'+planguage+'_00027']+'</font></td><td>'+
			pCurrentFolder[pfolderList_scr_ObjId]['path']+'</td><td>'+'</td></tr>'+
			fhtml_type +
			'<tr style="height: 35px;"><td></td><td><font style="font-size: 100%;font-weight: bold;">'+
			cmnLanguage['LAN_'+planguage+'_00022']+'</font></td><td>'+
			pCurrentFolder[pfolderList_scr_ObjId]['history']+'</td><td>'+'</td></tr>'+
			'<tr style="height: 35px;"><td></td><td><font style="font-size: 100%;font-weight: bold;">'+
			cmnLanguage['LAN_'+planguage+'_00037']+'</font></td><td>'+
			pCurrentFolder[pfolderList_scr_ObjId]['createUserName']+'</td><td>'+'</td></tr>'+
			'<tr style="height: 35px;"><td></td><td><font style="font-size: 100%;font-weight: bold;">'+
			cmnLanguage['LAN_'+planguage+'_00038']+'</font></td><td>'+
			pCurrentFolder[pfolderList_scr_ObjId]['createDate']+'</td><td>'+'</td></tr>'+
			'<tr style="height: 35px;"><td></td><td><font style="font-size: 100%;font-weight: bold;">'+
			cmnLanguage['LAN_'+planguage+'_00016']+'</font></td><td>'+
			pCurrentFolder[pfolderList_scr_ObjId]['modifyUserName']+'</td><td>'+'</td></tr>'+
			'<tr style="height: 35px;"><td></td><td><font style="font-size: 100%;font-weight: bold;">'+
			cmnLanguage['LAN_'+planguage+'_00017']+'</font></td><td>'+
			pCurrentFolder[pfolderList_scr_ObjId]['modifyDate']+'</td><td>'+'</td></tr>'+
			'<tr style="height: 35px;"><td></td><td><font style="font-size: 100%;font-weight: bold;">'+
			cmnLanguage['LAN_'+planguage+'_00019']+'</font></td><td>'+
			pCurrentFolder[pfolderList_scr_ObjId]['statusTypeName']+'</td><td>'+'</td></tr>'+
			'<tr style="height: 35px;"><td></td><td><font style="font-size: 100%;font-weight: bold;">'+
			cmnLanguage['LAN_'+planguage+'_00039']+'</font></td><td>'+
			pCurrentFolder[pfolderList_scr_ObjId]['securityName']+'</td><td>'+'</td></tr>'+
			'<tr style="height: 35px;"><td></td><td><font style="font-size: 100%;font-weight: bold;">'+
			cmnLanguage['LAN_'+planguage+'_00021']+'</font></td><td>'+
			pCurrentFolder[pfolderList_scr_ObjId]['property']+'</td><td>'+'</td></tr>';

		fhtml_info = fhtml_info + fhtml_file_data;
		
		for (var mykey in pCurrentFolder['Attributes'])
		{
			var fattr_list = [];
			fattr_list = JSON.parse(pCurrentFolder['Attributes'][mykey]);
			
			var fitems = 0;
			for (var mykey2 in fattr_list)
			{
				fitems = fitems + 1;
				flcounter = flcounter + 1;
				fhtml_info = fhtml_info + 
					'<tr style="height: 35px;"><td></td><td><font style="font-size: 100%;font-weight: bold;">';
				if (fitems == 1)
				{
					fhtml_info = fhtml_info +mykey;	
					fhtml_info = fhtml_info + '</font></td><td>'+fattr_list[mykey2]+'</td><td>'+'</td></tr>';
				}
				else 
				{
					fhtml_info = fhtml_info + '</font></td><td>'+fattr_list[mykey2]+'</td><td>'+'</td></tr>';	
				};
			};
		}
		
		fhtml_info = fhtml_info + '</table>';
		
		properties_scr_info.empty().append(fhtml_info);
		
		var style2 = document.createElement('style');
		style2.type = 'text/css';

		if ((navigator.userAgent.indexOf("iPad")>0)&&((navigator.userAgent.indexOf("OS 8")>0)||(navigator.userAgent.indexOf("OS 7")>0)))
		{
			flcounter = (flcounter * 35) + 635;
			style2.innerHTML = '#properties_scr_View-properties_scr_info_div {height: '+flcounter+'px;}';
		}
		else if (navigator.userAgent.indexOf("iPad")>0)
		{
			flcounter = (flcounter * 35) + 555;
			style2.innerHTML = '#properties_scr_View-properties_scr_info_div {height:'+flcounter+'px;}';
		}
		else if (navigator.userAgent.indexOf("iPhone")>0)
		{
			flcounter = (flcounter * 35) + 1045;
			style2.innerHTML = '#properties_scr_View-properties_scr_info_div {height:'+flcounter+'px;}';
		}
		else 
		{
			flcounter = (flcounter * 35) + 555;
			style2.innerHTML = '#properties_scr_View-properties_scr_info_div {height:'+flcounter+'px;}';
		}
		document.getElementsByTagName('head')[0].appendChild(style2);
	}
	
	/**
	 * getParameterテンプレート
	 */
	function getParameter () {
		var param = window.localStorage.getItem(viewName);
		return JSON.parse(param);
	}
	
	/**
	 * setParameter
	 */
	function setParameter (data) {
		// ログアウトする場合、マインメニューコントローラー
		// はこの機能でメモリクリーンをリクエストする。
		if (data == null)
		{
			if (properties_scr_info_div!=null){
				properties_scr_info_div.hide();
				pcollapsible.empty().append('');
				properties_scr_info.empty();
				properties_scr_fileName_1.empty();
				properties_scr_fileName_2.empty();
				properties_scr_fileName_3.empty();
				properties_scr_fileName_4.empty();
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
