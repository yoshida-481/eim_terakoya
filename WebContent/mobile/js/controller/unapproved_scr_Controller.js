/**
 * unapproved_scr_Controller
 */
var unapproved_scr_Controller = function () {
	/*****************************************************************
	 * 宣言
	 ****************************************************************/
	function F() {};
	F.prototype = prototypeController;
	var f = new F();
	
	// ビュー
	const viewName = "unapproved_scr_View";
	const viewId = "#" + viewName;
	var view = null;
	// モデル
	const model = unapproved_scr_Model;
	
	// コンポーネント
	var punapproved_scr_list_item = null;
	// unapproved_scr_file_info → ダイナミックなHTML「list_src_unapproved_info」になります。
	// unapproved_scr_properties_text → ダイナミックなHTML「unapproved_scr_properties_text[i]」になります。 
	
	// 変数
	var pCurrentFolder = new Object();
	var planguage = null;
	
	
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
		punapproved_scr_list_item = $(viewId+'-'+'unapproved_scr_list_item');
		punapproved_scr_list_item.empty();
		
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
		 * ドキュメントクリックイベントハンドラ（情報をクリック）
		 */
		jQuery(ev.target).on('click', $('.list_src_unapproved_info').selector, function() {
			var funapproved_scr_ObjId = "";
			
			// 変数にunapproved_scr_View情報をバインド
			funapproved_scr_ObjId = $(this).attr('id');
			funapproved_scr_ObjId = funapproved_scr_ObjId.substring(25);

			//モデルメソッドの呼び出し
			model.post_actSendReplyMail(funapproved_scr_ObjId);
		});
		
		/**
		 * ドキュメントクリックイベントハンドラ（アイコンをクリック）
		 */
		jQuery(ev.target).on('click', $('.post_actSendReplyMail').selector, function() {
			var funapproved_scr_ObjId = "";
			
			// 変数にunapproved_scr_View情報をバインド
			funapproved_scr_ObjId = $(this).attr('id');
			funapproved_scr_ObjId = funapproved_scr_ObjId.substring(25);
			
			//モデルメソッドの呼び出し
			model.post_actSendReplApproveyMail(funapproved_scr_ObjId);
		});
		
		/**
		 * トップへ戻るボタンクリックイベントハンドラ
		 */
		jQuery(ev.target).on('click', $('#unapproved_scr_View-list_scr_top_icon').selector, function() {
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
		 * 承認ボタンクリックイベントハンドラ
		 */
		jQuery(ev.target).on('click', $(".unapproved_scr_properties_text").selector, function() {
			var ffolderList_scr_workspace_infoId;
			//ドキュメントＩＤを変数にバインド
			ffolderList_scr_workspace_infoId = (this.id).substring(30);
			// ローカルストレージにパラメータをセット
			window.localStorage.setItem(EIM_KEY_CURRENT_CHECK_ELEMENT, JSON.stringify(ffolderList_scr_workspace_infoId));
			window.localStorage.setItem(EIM_KEY_CURRENT_CHECK_ELEMENT_OBJNAME, JSON.stringify(pCurrentFolder[ffolderList_scr_workspace_infoId]['objName']));
			window.localStorage.setItem(EIM_KEY_CURRENT_CHECK_ELEMENT_REQUESTUSERNAME, JSON.stringify(pCurrentFolder[ffolderList_scr_workspace_infoId]['requestUserName']));
			window.localStorage.setItem(EIM_KEY_CURRENT_CHECK_ELEMENT_REQUESTDATE, JSON.stringify(pCurrentFolder[ffolderList_scr_workspace_infoId]['requestDate']));
			window.localStorage.setItem(EIM_KEY_CURRENT_CHECK_ELEMENT_COMMENT, JSON.stringify(pCurrentFolder[ffolderList_scr_workspace_infoId]['comment']));
			window.localStorage.setItem(EIM_KEY_CURRENT_CHECK_ELEMENT_TIMING, JSON.stringify(pCurrentFolder[ffolderList_scr_workspace_infoId]['timing']));
			window.localStorage.setItem(EIM_KEY_CURRENT_CHECK_ELEMENT_STATUSID, JSON.stringify(pCurrentFolder[ffolderList_scr_workspace_infoId]['statusId']));
			window.localStorage.setItem(EIM_KEY_CURRENT_CHECK_ELEMENT_STATUSMDATELONG, JSON.stringify(pCurrentFolder[ffolderList_scr_workspace_infoId]['statusMDateLong']));
			window.localStorage.setItem(EIM_KEY_CURRENT_CHECK_ELEMENT_FORCASTSTATUSTYPEID, JSON.stringify(pCurrentFolder[ffolderList_scr_workspace_infoId]['forcastStatusTypeId']));
			window.localStorage.setItem(EIM_KEY_CURRENT_CHECK_ELEMENT_APPROVERS, JSON.stringify(pCurrentFolder[ffolderList_scr_workspace_infoId]['approverId']));
			window.localStorage.setItem(EIM_KEY_CURRENT_CHECK_ELEMENT_APPROVERS_NAMES, JSON.stringify(pCurrentFolder[ffolderList_scr_workspace_infoId]['approverName']));
			window.localStorage.setItem(EIM_KEY_CURRENT_CHECK_ELEMENT_REQ_APPROVERS, JSON.stringify(pCurrentFolder[ffolderList_scr_workspace_infoId]['reApproveId']));
			window.localStorage.setItem(EIM_KEY_CURRENT_CHECK_ELEMENT_REQ_APPROVERS_NAMES, JSON.stringify(pCurrentFolder[ffolderList_scr_workspace_infoId]['reApproveName']));
			window.localStorage.setItem(EIM_KEY_CURRENT_CHECK_ELEMENT_ISDOCUMENT, JSON.stringify(pCurrentFolder[ffolderList_scr_workspace_infoId]['isDocument']));
			window.localStorage.setItem(EIM_KEY_CURRENT_CHECK_ELEMENT_STATUSLIST, JSON.stringify(pCurrentFolder[ffolderList_scr_workspace_infoId]['statusList']));
			
			// 承認画面の遷移
			$.mobile.changePage(check_scr_Controller.getViewId(), {'transition' : 'fade', });
		});
		
		/**
		 * 「success」イベントハンドラ
		 */
		view.on('success', function(e, data) {
			;
		});
		
		/**
		 * 「error」イベントハンドラ
		 */
		view.on('error', function(e, data) {
			;
		});
		
		/**
		 * 「post_dspApproveDocumentListSuccess」イベントハンドラ
		 */
		view.on('post_dspApproveDocumentListSuccess', function(e, xml) {
			var fmain_scr_unapproved_count = 0;
			
			pCurrentFolder = new Object();
	    	
			$(xml).find("object").each(function(){
			    // 変数にXMLの未承認件数情報をバインド
				if ($(this).attr('objId')!=""){
					fmain_scr_unapproved_count = fmain_scr_unapproved_count+1;	

					var fobjId ="";
			    	var fobjName ="";
			    	var fhistory ="";
			    	var fpath ="";
			    	var frequestUserName = "";
			    	var frequestDate = "";
			    	var ftiming = "";
			    	var fstatusId = "";
			    	var ffinalApprove = false;
			    	var fstatusMDateLong = "";
			    	var fforcastStatusTypeId = "";
			    	var fapproverId = "";
			    	var fapproverName = "";
			    	var freApproveId = "";
			    	var freApproveName = "";
			    	var fisDocument = "";
			    	
			    	// objIdタブ
			    	fobjId = $(this).attr('objId');
			    	// objNameタブ
			    	fobjName = $(this).attr('objName');
			    	// revタブ
			    	fhistory = $(this).attr('rev');
			    	// pathタブ
			    	fpath = $(this).attr('path');
			    	// requestUserNameタブ
			    	frequestUserName = $(this).attr('requestUserName');
			    	// frequestDateタブ
			    	frequestDate = $(this).attr('requestDate');
			    	// timingタブ
			    	ftiming = $(this).attr('timing');
			    	// statusIdタブ
			    	fstatusId = $(this).attr('statusId');
			    	// finalApproveタブ
			    	ffinalApprove = $(this).attr('finalApprove');
			    	// statusMDateLongタブ
			    	fstatusMDateLong = $(this).attr('statusMDateLong');
			    	// forcastStatusTypeIdタブ
			    	fforcastStatusTypeId = $(this).attr('forcastStatusTypeId');
			    	// approverIdタブ
			    	if ($(this).attr('approverId')!=null)
			    	{
			    		if ($(this).attr('approverId')!=undefined)
					    {
			    			fapproverId = $(this).attr('approverId');		
					    };
			    	}
			    	// approverNameタブ
			    	if ($(this).attr('approverName')!=null)
			    	{
			    		if ($(this).attr('approverName')!=undefined)
					    {
			    			fapproverName = $(this).attr('approverName');		
					    };
			    	}
			    	// reApproveIdタブ
			    	if ($(this).attr('reApproveId')!=null)
			    	{
			    		if ($(this).attr('reApproveId')!=undefined)
					    {
			    			freApproveId = $(this).attr('reApproveId');		
					    };
			    	}
			    	// reApproveNameタブ
			    	if ($(this).attr('reApproveName')!=null)
			    	{
			    		if ($(this).attr('reApproveName')!=undefined)
					    {
			    			freApproveName = $(this).attr('reApproveName');		
					    };
			    	}
			    	
			    	// isDocumentタブ
			    	if ($(this).attr('isDocument')!=null)
			    	{
			    		if ($(this).attr('isDocument')!=undefined)
					    {
			    			fisDocument = $(this).attr('isDocument') == "true" ? true : false ;
					    };
			    	}
			    	
			    	pCurrentFolder[fobjId] = new Object();
			    	pCurrentFolder[fobjId]['objName'] = fobjName;
			    	pCurrentFolder[fobjId]['history'] = fhistory;
			    	pCurrentFolder[fobjId]['path'] = fpath;
			    	pCurrentFolder[fobjId]['requestUserName'] = frequestUserName;
			    	pCurrentFolder[fobjId]['requestDate'] = frequestDate;
			    	pCurrentFolder[fobjId]['timing'] = ftiming;
			    	pCurrentFolder[fobjId]['statusId'] = fstatusId;
			    	pCurrentFolder[fobjId]['finalApprove'] = ffinalApprove;
			    	pCurrentFolder[fobjId]['statusMDateLong'] = fstatusMDateLong;
			    	pCurrentFolder[fobjId]['forcastStatusTypeId'] = fforcastStatusTypeId;
			    	pCurrentFolder[fobjId]['approverId'] = fapproverId;
			    	pCurrentFolder[fobjId]['approverName'] = fapproverName;
			    	pCurrentFolder[fobjId]['reApproveId'] = freApproveId;
			    	pCurrentFolder[fobjId]['reApproveName'] = freApproveName;
			    	pCurrentFolder[fobjId]['isDocument'] = fisDocument;
				};
				
				var statusTypeArray = new Array();
				$(this).find("status").each(function(i) {
					var statusType = new Object();
					var statusTypeId = $(this).attr('statusTypeId');
					statusType['approverName'] = $(this).attr('approverName');
					statusType['approverId'] = $(this).attr('approverId');
					statusType['statusTypeId'] = $(this).attr('statusTypeId');
					statusType['statusTypeName'] = $(this).attr('statusTypeName');
					statusType['statusKind'] = $(this).attr('statusKind');
					statusType['functionType'] = $(this).attr('functionType');
					statusType['displayFlag'] = $(this).attr('displayFlag');
					statusType['finalApprove'] = $(this).attr('finalApprove');
					statusTypeArray[i] = statusType;
				});
				
				pCurrentFolder[fobjId]['statusList'] = statusTypeArray;
				
			});
			
			
			// ヘッダーの未承認件数をアップデート
			if (fmain_scr_unapproved_count>0){
				$('.header_unapprovedText').empty().append(cmnLanguage['LAN_'+planguage+'_00008']+"("+fmain_scr_unapproved_count+")");
			}
			else
			{
				$('.header_unapprovedText').empty().append(cmnLanguage['LAN_'+planguage+'_00008']);
				f.cmnPopup(cmnGetmsg['MSG_ERR_'+planguage+'_00005'],"ERR",viewName);
			}
			
			// 未承認件数クーキーを登録する
			try {
				window.localStorage.setItem(EIM_KEY_APPROVEDOCUMENT_COUNT, JSON.stringify(fmain_scr_unapproved_count));
			}
			catch (e){;
			}

			// 一つずつ、に承認の更新者と更新日を取得
			for (var i in pCurrentFolder){
				if (pCurrentFolder[i]['comment']==undefined){
					model.post_WorkFlowHistory (i);
				}
				break;
			};
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
		 * 「post_WorkFlowHistorySuccess」イベントハンドラ
		 */
		view.on('post_WorkFlowHistorySuccess', function(e, xml) {
			var fcomments_loaded = true;
			var fobjId ="";
			
			// 変数にXMLの未承認履歴情報をバインド
			$(xml).find("statusTypeList").each(function(){
				// objIdタブ
				fobjId =$(this).attr('objId');
			});
			
			$(xml).find("eventList").each(function(){
				$(xml).find("event").each(function(){
					var fcomment ="";
					// commentタブ
					fcomment = $(this).attr('comment');
					
					if (fcomment != undefined)
					{
						// 最後のメッセージは正しいです。
						pCurrentFolder[fobjId]['comment'] = fcomment;
					}
				});	
			});
			
			// 次のドキュメントの未承認履歴情報を取得
			for (var i in pCurrentFolder){
				if (pCurrentFolder[i]['comment']==undefined)
				{
					fcomments_loaded = false;
					model.post_WorkFlowHistory (i);
					return;
				}
				
			};
			
			// 全体の未承認データーを取得した場合、未承認一覧を表示
			if (fcomments_loaded){
				// コンポーネントに変数をバインド
				if ($(document.body).height()>800)
				{	resize_screen_portrait();
				}
				else 
				{
					resize_screen_landscape();
				}
			};
		});		
		
		/**
		 * 「post_WorkFlowHistoryError」イベントハンドラ
		 */
		view.on('post_WorkFlowHistoryError', function(e, xml) {
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
				f.cmnPopup($(this).attr('message'),"ERR",viewName);
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
		
	});
	
	/*****************************************************************
	 * ページ表示処理
	 ****************************************************************/
	jQuery(document).on('pageinit', viewId, function(ev) {
		// トップへ戻るボタンを設定
		$('.unapproved_scr_View-list_scr_top_icon_div').hide();
		$('#unapproved_scr_View-list_scr_top_icon').empty().append('<a id="unapproved_scr_View-list_scr_top_icon" href="#">' + 
																	cmnGetimg['list_scr_top_icon'] + '</a>');
		// 閉じる
		$('.header_menuBox').hide();
	});
	
	jQuery(document).on('pagebeforeshow', viewId, function(ev) {
		
		// パンクずと検索を非表示
		$('.folderList_scr_View-folderList_scr_order_combo_div').hide();
		$('.folderList_scr_View-folderList_scr_bc').hide();
		
		pcurrentView = viewName;
		punapproved_scr_list_item.empty();
		// トップへ戻るボタンを非表示にする。
		$('.folderList_scr_View-list_scr_top_icon_div').hide();
		$('.unapproved_scr_View-list_scr_top_icon_div').hide();
		$('.favorites_scr_View-list_scr_top_icon_div').hide();
		// ボタンハイライト
		cmn_header_button_highlight (2);
	});
	
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
		$('.header_title').empty().append(cmnLanguage['LAN_'+planguage+'_00029']);
		
		// クーキーを確認
		if (JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_LIST_TOP_ELEMENT, JSON.stringify(ptop_locator))) != null){
			ptop_locator = JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_LIST_TOP_ELEMENT, JSON.stringify(ptop_locator)));
		}
		else 
		{
			ptop_locator = "";
		}
		
		// 未承認情報の取得
		model.post_dspApproveDocumentList();
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
		var ffolder_link_html1 = "";
		var ffolder_link_html1_icon = "";
		var ffolder_link_html2 = "";
		var ficon_to_show = "";
		var ffavorites_scr_list_item_html="";
		var ffile_extension = "";
		var fcomment = "";
		// 並び替え
		var fsorted_list =null;
		var i = "";
		
		// ＯＳによって、アイコンのサイズを変わる
		cmn_change_image_sizes (0, planguage);
		
		// トップへ戻るボタンを非表示にする。
		$('.unapproved_scr_View-list_scr_top_icon_div').hide();	
		
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
		
		ptop_locator = "";
		// クーキーをアップデート
		window.localStorage.setItem(EIM_KEY_CURRENT_LIST_TOP_ELEMENT, JSON.stringify(ptop_locator));
			
		// 並び替え
		fsorted_list = create_sorted_list(1);
			
		for (var key in fsorted_list){
			i = String(fsorted_list[key][0]);
			
			if (ptop_locator == "") 
			{
				ptop_locator = 'list_src_unapproved_icon_'+i;
				// クーキーをアップデート
				window.localStorage.setItem(EIM_KEY_CURRENT_LIST_TOP_ELEMENT, JSON.stringify(ptop_locator));
			};
				
			ffile_extension = (pCurrentFolder[i]['objName']).substring((pCurrentFolder[i]['objName']).lastIndexOf(".")+1);
			
			if (pCurrentFolder[i]['isDocument'] == false)
			{
				// フォルダ
				ffolder_link_html1 = '<a href="#">';
				ffolder_link_html1_icon = '<a href="#">';
				ficon_to_show = cmnGetimg['list_scr_folder_icon'];
			}
			else 
			{
				// ドキュメントをダウンロード
				// 原本
				ffolder_link_html1 = '<a class="list_src_unapproved_info" target="_blank" id="list_src_unapproved_info_'+i+'" href="#">';
				ffolder_link_html1_icon = '<a class="post_actSendReplyMail" target="_blank" id="list_src_unapproved_icon_'+i+'" href="'+cmnJSPAPI['JSPAPI_00011']+i+'">';
				
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

			// コメントタブの設定
			fcomment = pCurrentFolder[i]['comment'];
			fcomment = resizeString(fcomment, viewName, 'portrait');
			
			ffavorites_scr_list_item_html = ffavorites_scr_list_item_html + '<li class="folderList_scr_list_item">';
			
			ffavorites_scr_list_item_html = ffavorites_scr_list_item_html + 
				'<table style="vertical-align: top;-webkit-border-horizontal-spacing: 0px; -webkit-border-vertical-spacing: 0px;' + 
				' border: inset 0pt;border-style: hidden;border-collapse: collapse;border-spacing:0;width:100%;"><tr width="100%">' + 
				'<td width="15%" valign="top">' + ffolder_link_html1_icon + ficon_to_show + '</a></td>' +
				'<td width="75%" align="left" style="vertical-align: middle;">';
			
			if (pCurrentFolder[i]['isDocument'] == false)
			{
				ffavorites_scr_list_item_html = ffavorites_scr_list_item_html + '<div>';
			}
			else
			{
				ffavorites_scr_list_item_html = ffavorites_scr_list_item_html + '<div onclick="var win = window.open(\''+cmnJSPAPI['JSPAPI_00011']+i+'\',\'_blank\');win.focus();">';
			}

			ffavorites_scr_list_item_html = ffavorites_scr_list_item_html + ffolder_link_html1+
				'<font style="font-size: 150%;font-weight: bold;">'+pCurrentFolder[i]['objName']+'</font></br>'+
				cmnLanguage['LAN_'+planguage+'_00030'] + ': '+pCurrentFolder[i]['requestUserName']+' 　　'+
				cmnLanguage['LAN_'+planguage+'_00031']+': '+pCurrentFolder[i]['requestDate']+'<br>'+
				cmnLanguage['LAN_'+planguage+'_00033']+': '+fcomment+
				ffolder_link_html2+'</div></td><td width="5%" style="vertical-align: middle;">'+
				'<table style="-webkit-border-horizontal-spacing: 0px; -webkit-border-vertical-spacing: 0px; ' + 
				'border: inset 0pt;border-style: hidden;border-collapse: collapse;border-spacing:0;width:100%;">' + 
				'<td align="middle"><p style="line-height: 400%;"><center><a class="unapproved_scr_properties_text" id="unapproved_scr_properties_text'+i+'" href="#">' + 
				cmnGetimg['unapproved_scr_btn_Check']+'<br><font color="black">'+cmnLanguage['LAN_'+planguage+'_00034'] + 
				'</font></a></center></p></td></tr></table>'+
				'</td><td width="5%" ></td></tr></table>'+
			'</li>';
		};
		
		ffavorites_scr_list_item_html = ffavorites_scr_list_item_html + '</ul>';
		punapproved_scr_list_item.empty().append(ffavorites_scr_list_item_html);
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
		var ffavorites_scr_list_item_html="";
		var ffile_extension = "";
		var fcomment = "";
		// 並び替え
		var fsorted_list =null;
		var i = "";
		
		// ＯＳによって、アイコンのサイズを変わる
		cmn_change_image_sizes (1, planguage);
		
		// トップへ戻るボタンを非表示にする。
		$('.unapproved_scr_View-list_scr_top_icon_div').hide();	
		
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
			ffavorites_scr_list_item_html = '<ul style="height: 705px;" class="scrollable" data-role="listview" data-split-icon="grid" data-split-theme="d">';
		}
		
		ptop_locator = "";
		// クーキーをアップデート
		window.localStorage.setItem(EIM_KEY_CURRENT_LIST_TOP_ELEMENT, JSON.stringify(ptop_locator));
			
		// 並び替え
		fsorted_list = create_sorted_list(1);

		for (var key in fsorted_list){
			i = String(fsorted_list[key][0]);
			
			if (ptop_locator == "") 
			{
				ptop_locator = 'list_src_unapproved_icon_'+i;
				// クーキーをアップデート
				window.localStorage.setItem(EIM_KEY_CURRENT_LIST_TOP_ELEMENT, JSON.stringify(ptop_locator));
			};
			
			ffile_extension = (pCurrentFolder[i]['objName']).substring((pCurrentFolder[i]['objName']).indexOf(".")+1);
			
			if (pCurrentFolder[i]['isDocument'] == false)
			{
				// フォルダ
				ffolder_link_html1 = '<a href="#">';
				ffolder_link_html1_icon = '<a href="#">';
				ficon_to_show = cmnGetimg['list_scr_folder_icon'];
			}
			else 
			{
				// ドキュメントをダウンロード
				// 原本
				ffolder_link_html1 = '<a class="list_src_unapproved_info" target="_blank" id="list_src_unapproved_info_'+i+'" href="#">';
				ffolder_link_html1_icon = '<a class="post_actSendReplyMail" target="_blank" id="list_src_unapproved_icon_'+i+'" href="'+cmnJSPAPI['JSPAPI_00011']+i+'">';
				
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
			}
			
			// コメントタブの設定
			fcomment = pCurrentFolder[i]['comment'];
			fcomment = resizeString(fcomment, viewName, 'landscape');
			
			ffavorites_scr_list_item_html = ffavorites_scr_list_item_html + '<li class="folderList_scr_list_item">';

			ffavorites_scr_list_item_html = ffavorites_scr_list_item_html + 
			'<table style="vertical-align: top;-webkit-border-horizontal-spacing: 0px; -webkit-border-vertical-spacing: 0px; ' + 
			'border: inset 0pt;border-style: hidden;border-collapse: collapse;border-spacing:0;width:100%;"><tr width="100%">' + 
			'<td width="8%" valign="top">' + ffolder_link_html1_icon + ficon_to_show + '</a></td>' +
			'<td width="45%" align="left" style="vertical-align: middle;">';
			
			if (pCurrentFolder[i]['isDocument'] == false)
			{
				ffavorites_scr_list_item_html = ffavorites_scr_list_item_html + '<div>';
			}
			else
			{
				ffavorites_scr_list_item_html = ffavorites_scr_list_item_html + '<div onclick="var win = window.open(\''+cmnJSPAPI['JSPAPI_00011']+i+'\',\'_blank\');win.focus();">';
			}

			ffavorites_scr_list_item_html = ffavorites_scr_list_item_html + 
				ffolder_link_html1+
					'<font style="font-size: 150%;font-weight: bold;">' + pCurrentFolder[i]['objName']+'</font></br>'+
					cmnLanguage['LAN_'+planguage+'_00030'] + ': '+pCurrentFolder[i]['requestUserName']+' 　　'+
					cmnLanguage['LAN_'+planguage+'_00031']+': '+pCurrentFolder[i]['requestDate']+'</br>'+
					cmnLanguage['LAN_'+planguage+'_00033']+': '+fcomment+
					ffolder_link_html2+'</div></td><td width="5%" style="vertical-align: middle;">'+
					'<table style="-webkit-border-horizontal-spacing: 0px; -webkit-border-vertical-spacing: 0px; ' + 
					'border: inset 0pt;border-style: hidden;border-collapse: collapse;border-spacing:0;width:100%;">' + 
					'<td align="middle"><p style="line-height: 400%;"><center><a class="unapproved_scr_properties_text" id="unapproved_scr_properties_text'+i+'" href="#">' +  
					cmnGetimg['unapproved_scr_btn_Check']+'<br><font color="black">'+cmnLanguage['LAN_'+planguage+'_00034'] + 
					'</font></a></center></p></td></tr></table>'+
					'</td><td width="1%"></td></tr></table>'+
				'</li>';
		};
		
		ffavorites_scr_list_item_html  = ffavorites_scr_list_item_html  + '</ul>';
		punapproved_scr_list_item.empty().append(ffavorites_scr_list_item_html );
	}

	/**
	 * 並び替え：０＝ワークスペース、１＝フォルダ
	 */
	function create_sorted_list(screen_type) {
		var sortable = [];
		var forigin;
		var selected_val="";
		var selected_val2="";
		
		forigin = pCurrentFolder;
		
		// 日付
		selected_val="_00024";
		// ドキュメント名
		selected_val2="_00018";
		
		for (var key in forigin)
		{
			sortable.push([key, forigin[key][cmnOrderBy['ORD'+selected_val]], forigin[key][cmnOrderBy['ORD'+selected_val2]]]);
		};
		
		sortable.sort(function(a, b) 
			{
				if (String(a[1].localeCompare(String(b[1]))) == 0)
				{
					return String(a[2].localeCompare(String(b[2])));
				}
				else 
				{
					return String(a[1].localeCompare(String(b[1])));
				}
			});
		
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
			if (punapproved_scr_list_item != null)
			{
				punapproved_scr_list_item.empty();
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
