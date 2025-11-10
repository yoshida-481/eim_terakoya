/**
 * check_scr_Controller
 */
var check_scr_Controller = function () {
	/*****************************************************************
	 * 宣言
	 ****************************************************************/
	function F() {};
	F.prototype = prototypeController;
	var f = new F();
	
	// ビュー
	const viewName = "check_scr_View";
	const viewId = "#" + viewName;
	var view = null;
	// モデル
	const model = check_scr_Model;
	
	// コンポーネント
	var pcheck_scr_checkFile_Info_icon = null;
	var pcheck_scr_checkFile_Info_docinfo = null;
	var pcheck_scr_checkFile_Info_properties = null;
	var pcheck_scr_appReqDestination = null;
	var pcheck_scr_appStatusDestination_label = null;
	var pcheck_scr_appReqDestination_label = null;
	var pappReqDestination_button_label = null;
	var pcheck_scr_comment = "";
	var pcheck_scr_comment_label = null;
	// check_scr_history_desc → ダイナミックＨＴＭＬになりました
	// ポップアップコンポーネント（承認依頼先選択）
	var pcheckGroupList_scr_select = null;
	var pcheckGroupList_scr_select_label = null;
	var pcheckGroupList_scr_cancel = null;
	var pcheckGroupList_scr_cancel_label = null;
	var pcheckGroupList_scr_dispSup = null;
	var pcheckGroupList_scr_dispSup_label = null;
	var pcheckGroupList_scr_approverList = null;
	var pcheck_scr_btn_Set = null;
	var pcheck_scr_btn_Undo = null;
	// checkGroupList_scr_groupName → ダイナミックなHTMLになります。「post_dspApproverSuccess」イベントハンドラの「fuser_list」
	
	// 変数
	var check_scr_info_div = null;
	var planguage = null;
	var pcollapsible = null;
	var pcheck_scr_history_desc = null;
	var pfolderList_scr_timing = "";
	var pfolderList_scr_statusId = "";
	var pfolderList_scr_finalApprove = false;
	var pfolderList_scr_statusMDateLong = "";
	var pfolderList_scr_forcastStatusTypeId = "";
	var pfolderList_scr_ObjId = "";
	var pfolderList_scr_ObjName = "";
	var pfolderList_scr_RequestUserName = "";
	var pfolderList_scr_RequestDate = "";
	var pfolderList_scr_Comment = "";
	var pfolderList_scr_approverId = "";
	var pfolderList_scr_approverName = "";
	var pfolderList_scr_req_approverId = "";
	var pfolderList_scr_req_approverName = "";
	var papprovers = "2";
	var papprovers_list = [];
	var puserlist_was_empty = false;
	var patleastonechecked = false;
	var pCurrentFolder = new Object();
	var pisDocument = true;
	
	var pStatusList = null;
	var pStatusApproverMap = new Object();
	
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
		pcheck_scr_checkFile_Info_icon = $(viewId+'-'+'check_scr_checkFile_Info_icon');
		pcheck_scr_checkFile_Info_docinfo = $(viewId+'-'+'check_scr_checkFile_Info_docinfo');
		pcheck_scr_checkFile_Info_properties = $(viewId+'-'+'check_scr_checkFile_Info_properties');
		pcheck_scr_comment = $(viewId+'-'+'check_scr_comment');
		pcheck_scr_appReqDestination = $(viewId+'-'+'check_scr_appReqDestination');
		pcheck_scr_appStatusDestination_label = $(viewId+'-'+'check_scr_appStatusDestination_label');
		pcheck_scr_appReqDestination_label = $(viewId+'-'+'check_scr_appReqDestination_label');
		pcheck_scr_comment_label = $(viewId+'-'+'check_scr_comment_label');
		pappReqDestination_button_label = $(viewId+'-'+'check_scr_appReqDestination_button_label');
		pcheckGroupList_scr_dispSup = $(viewId+'-'+'checkGroupList_scr_dispSup');
		pcheckGroupList_scr_dispSup_label = $(viewId+'-'+'checkGroupList_scr_dispSup_label');
		pcheckGroupList_scr_select = $(viewId+'-'+'checkGroupList_scr_select');
		pcheckGroupList_scr_select_label = $(viewId+'-'+'checkGroupList_scr_select_label');
		pcheckGroupList_scr_cancel = $(viewId+'-'+'checkGroupList_scr_cancel');
		pcheckGroupList_scr_cancel_label = $(viewId+'-'+'checkGroupList_scr_cancel_label');
		pcheckGroupList_scr_approverList = $(viewId+'-'+'checkGroupList_scr_approverList');
		pcheck_scr_btn_Set = $(viewId+'-'+'check_scr_btn_Set');
		pcheck_scr_btn_Undo = $(viewId+'-'+'check_scr_btn_Undo');
		
		check_scr_info_div = $(viewId+'-'+'check_scr_info_div');
		pcollapsible = $(viewId+'-'+'collapsible');
		pcheck_scr_history_desc = $(viewId+'-'+'check_scr_history_desc');

		// イメージ・アイコン
		document.getElementById("check_scr_View-check_scr_btn_Set").style.background='-webkit-linear-gradient(top, #227cff, #0070ff)';
		document.getElementById("check_scr_View-check_scr_btn_Undo").style.background='-webkit-linear-gradient(top, #69d96d, #41cd66)';
		document.getElementById("check_scr_View-check_scr_btn_Cancel").style.background='-webkit-linear-gradient(top, #fc5600, #ee2320)';
		document.getElementById("check_scr_View-check_scr_appReqDestination").style.background='-webkit-linear-gradient(top, #fcfcfc, #f2f3f3)';
		document.getElementById('check_scr_View-checkGroupList_scr_select').style.background='-webkit-linear-gradient(top, #227cff, #0070ff)';
		document.getElementById('check_scr_View-checkGroupList_scr_cancel').style.background='-webkit-linear-gradient(top, #fc5600, #ee2320)';
		
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
		 * 承認履歴プラスボタンクリックイベントハンドラ
		 */
		jQuery(ev.target).on('click', $('#check_scr_View-collapsible_open').selector, function() {
			pcollapsible.empty().append('<a href="#" id="check_scr_View-collapsible_close">'+
					cmnGetimg['check_scr_btn_History_minus']+"</a>");
			check_scr_info_div.show();
		});
		
		/**
		 * 承認履歴マイナスボタンクリックイベントハンドラ
		 */
		jQuery(ev.target).on('click', $('#check_scr_View-collapsible_close').selector, function() {
			check_scr_info_div.hide();
			pcollapsible.empty().append('<a href="#" id="check_scr_View-collapsible_open">'+
					cmnGetimg['check_scr_btn_History_plus']+"</a>");
		});
		
		/**
		 * 遷移先セレクトメニューチェンジイベントハンドラ
		 */
		jQuery(ev.target).on('change', $('#check_scr_View-statusDestinationSelectMenu').selector, function() {
			console.log('セレクトメニューのチェンジ');
			// 選択されたステータスタイプ名のステータスタイプID
			var selectedStatusTypeId = $(this).val();
			
			// 遷移先ステータスタイプIDをセット
			pfolderList_scr_forcastStatusTypeId = selectedStatusTypeId;
			
			// 承認依頼先を特定する
			var approverList = pStatusApproverMap[selectedStatusTypeId];
			papprovers_list = approverList;
			
			// 遷移先ステータスタイプが最終承認かどうか
			jQuery.each(pStatusList, function(i, object) {
				if(object.statusTypeId == pfolderList_scr_forcastStatusTypeId){
					pfolderList_scr_finalApprove = object.finalApprove == "true" ? true : false;
				}
			});
			
			setApprover();
		});
		
		/**
		 * 属性ボタンクリックイベントハンドラ
		 */
		jQuery(ev.target).on('click', $('.check_scr_btn_Attr').selector, function() {
			var ffolderList_scr_ObjId = "";

			// 変数にfolderList_scr_View情報をバインド
			ffolderList_scr_ObjId = $(this).attr('id');
			ffolderList_scr_ObjId = ffolderList_scr_ObjId.substring(19);
			
			// クーキーを登録
			window.localStorage.setItem(EIM_KEY_CURRENT_PROPERTY_ELEMENT, JSON.stringify(ffolderList_scr_ObjId));
			window.localStorage.removeItem(EIM_KEY_CURRENT_PROPERTY_ISPUBLISHED);
			window.localStorage.removeItem(EIM_KEY_CURRENT_PROPERTY_READONLY);
			
			if (pisDocument == true)
			{
				window.localStorage.setItem(EIM_KEY_CURRENT_PROPERTY_OBJTYPENAME, JSON.stringify(''));	
			}
			
			// 属性情報画面の遷移
			$.mobile.changePage(properties_scr_Controller.getViewId(), {'transition' : 'fade', });
		});
		
		/**
		 * キャンセルボタンクリックイベントハンドラ
		 */
		jQuery(ev.target).on('click', $('#check_scr_View-check_scr_button_label_Cancel').selector, function() {
			// 未承認画面の遷移
			$.mobile.changePage(unapproved_scr_Controller.getViewId(), {'transition' : 'fade', });
		});
		
		/**
		 * 差戻しボタンクリックイベントハンドラ
		 */
		jQuery(ev.target).on('click', pcheck_scr_btn_Undo.selector, function() {
			
			// モデルを呼び出す。差戻し情報を取り得
			model.post_doEvent_back (pfolderList_scr_ObjId, 
									 pcheck_scr_comment.val(),
									 pfolderList_scr_timing,
									 pfolderList_scr_statusId,
									 pfolderList_scr_finalApprove,
									 pfolderList_scr_statusMDateLong,
									 pfolderList_scr_forcastStatusTypeId);
		});
		
		/**
		 * 承認ボタンクリックイベントハンドラ
		 */
		jQuery(ev.target).on('click', pcheck_scr_btn_Set.selector, function() {
			
			if ((pfolderList_scr_finalApprove!=true) && (papprovers_list.length == 0))
			{
				f.cmnPopup(cmnGetmsg['MSG_ERR_'+planguage+'_00011'],"WAR",viewName);
				return;
			};
			
			// モデルを呼び出す。承認イベント実行。
			model.post_doEvent_approve (pfolderList_scr_ObjId, 
										pcheck_scr_comment.val(),
										pfolderList_scr_timing,
										pfolderList_scr_statusId,
										pfolderList_scr_finalApprove,
										pfolderList_scr_statusMDateLong,
										pfolderList_scr_forcastStatusTypeId,
										papprovers_list);
		});
		
		/**
		 * 上長のみ表示チェックバックスリックイベントハンドラ
		 */
		jQuery(ev.target).on('click', pcheckGroupList_scr_dispSup.selector, function() {
			if (pcheckGroupList_scr_dispSup.prop('checked'))
			{
				papprovers = "2";
			}
			else 
			{
				papprovers = "0";
			}
			
			// モデルを呼び出す。承認者一覧の取得
			model.post_dspApprover (pfolderList_scr_ObjId, 
									pfolderList_scr_forcastStatusTypeId,
									papprovers);
		});
		
		/**
		 * 承認依頼先選択ボタンクリックイベントハンドラ
		 */
		jQuery(ev.target).on('click', pcheck_scr_appReqDestination.selector, function() {
			// finfalApproveタブ＝ＴＲＵＥの場合、再承認が有りません
			if (pfolderList_scr_finalApprove == true){
				// なにもしない
				return;
			}
			
			//  上長のみ表示チェックバックス
			pcheckGroupList_scr_dispSup.prop('checked', true);
			pcheckGroupList_scr_approverList.empty();
			
			papprovers = "2";
						
			// モデルを呼び出す。承認者一覧の取得
			model.post_dspApprover (pfolderList_scr_ObjId, 
									pfolderList_scr_forcastStatusTypeId,
									papprovers);
		});
		
		/**
		 * 承認依頼先選択ポップアップ 設定ボタンクリックイベントハンドラ
		 */
		jQuery(ev.target).on('click', pcheckGroupList_scr_select.selector, function() {

			var flg = false;
			var selectedApproverMap = new Object();
			
			$('#check_scr_View-checkGroupList_scr_approverList input[type=checkbox]').each(function () {
				if($(this).prop('checked')){
					var id = $(this).attr('id');
					id = id.split('_')[0];
					var name = $(this).attr('name');
					
					var approver = new Object();
					approver.approverId = id;
					approver.approverName = name;
					selectedApproverMap[id] = approver;
				}
			});
			
			papprovers_list = [];
			for(key in selectedApproverMap){
				console.log(selectedApproverMap[key]);
				papprovers_list.push(selectedApproverMap[key]);
			}
			
			if (papprovers_list.length > 0){
				$("#check-popup").popup("close");
			}else{
				f.cmnPopup2(cmnGetmsg['MSG_ERR_'+planguage+'_00011'],"WAR",viewName);
			};
			
			setApprover();
			
		});
		
		/**
		 * ポップアップのポップアップメッセージリックイベントハンドラ
		 */
		jQuery(ev.target).on('click', $("#check_scr_View-message_popup_ok_check").selector, function() {
			$("#check_scr_View-message_popup_check").hide();
		});
		
		
		/**
		 * ポップアップのキャンセルボタンクリックイベントハンドラ
		 */
		jQuery(ev.target).on('click', pcheckGroupList_scr_cancel.selector, function() {
			pcheckGroupList_scr_dispSup.prop('checked',true);
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
		 * 「post_WorkFlowHistorySuccess」イベントハンドラ
		 */
		view.on('post_WorkFlowHistorySuccess', function(e, xml) {
			var fapproval_history_html ="<br>";
			var fcounter = 0;
			
			// 変数にXMLの未承認履歴情報をバインド
			$(xml).find("event").each(function(){
				var fuserName = "";
				var fdate = "";
				var fcomment = "";
				var fbaseEvtType = "";
				
				// userNameタブ
				fuserName =$(this).attr('userName');
				// dateタブ
				fdate =$(this).attr('date');
				// commentタブ
				fcomment = $(this).attr('comment');
				// baseEvtTypeタブ
				fbaseEvtType =$(this).attr('baseEvtType');
				if (fbaseEvtType != null)
				{
					if (fbaseEvtType != undefined)
					{
						if ((fbaseEvtType == "承認")||(fbaseEvtType == "Approval")||(fbaseEvtType == "差戻し")||(fbaseEvtType == "Sending back"))
						{
							if (fuserName != null)
							{
								if (fuserName != undefined)
								{
									if (fdate == null)
									{
										fdate = "";
									}
									if (fdate == undefined)
									{
										fdate = "";
									}
									if (fcomment == null)
									{
										fcomment = "";
									}
									if (fcomment == undefined)
									{
										fcomment = "";
									}
									fcounter = fcounter + 1;
									fapproval_history_html = fapproval_history_html +
										'<table width="100%"><tr><td align="left" width="5%"></td><td align="left" width="30%">'+
										'<font style="font-weight: bold;">'+fuserName+'</font><td>'+
										'<td align="left" width="30%">'+fdate + '<td><td align="left" width="30%">'+
										'<font style="font-weight: bold;">'+
										fbaseEvtType +'</font></td><td align="left" width="5%"></td>'+
										'<tr><td></td><td colspan="4">'+
										fcomment+'</td></tr></table><hr>';
								};
							};	
						};
					};
				};
			});
			
			// 履歴がない場合
			if (fcounter == 0) 
			{
				fapproval_history_html = fapproval_history_html + 
					'<table width="100%"><tr><td align="left" width="5%"></td><td align="left" width="90%">'+
					cmnLanguage['LAN_'+planguage+'_00059']+'　　　'+
					'　　　<font style="font-weight: bold;"></font></td><td align="left" width="5%"></td>'+
					'<tr><td></td><td colspan="2"></td></tr></table><hr>';
			}
			
			pcheck_scr_history_desc.empty().append(fapproval_history_html);
			
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
		 * 「post_doEventSuccess」イベントハンドラ
		 */
		view.on('post_doEventSuccess', function(e, xml) {
			// 未承認画面の遷移
			$.mobile.changePage(unapproved_scr_Controller.getViewId(), {'transition' : 'fade', });
		});
		
		/**
		 * 「post_doEventError」イベントハンドラ
		 */
		view.on('post_doEventError', function(e, xml) {
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
		 * 「post_dspApproverSuccess」イベントハンドラ
		 */
		view.on('post_dspApproverSuccess', function(e, xml) {
			//上司フラグを取得
			var onlyBoss = false;
			//存在しない場合false
			$(xml).find("onlyBoss").each(function(){
				// 見つかった場合値をbool変換
				onlyBoss	 = $(this).attr('flag') === "true";
			});
			
			var fuser_listhead = "";
			var fgroup = "";
			var ffreegrouplistposition = 0;
			var faddtitle = true;

			puserlist_was_empty = true;
			
			pcheckGroupList_scr_approverList.empty();
			
			//テーブルの作成
			var tableHtml ="";
			tableHtml += '<table style="border-collapse:collapse;" width="100%">';
			//カラム作成関数
			var _columnNameFnc = function(_keyId){
				return	'<td bgcolor="#403f41">' +
							'<font style="color : white; text-shadow: none; font-weight: normal;">&nbsp;' +
								cmnLanguage['LAN_' + planguage + '_' + _keyId] +
							'</font>' +
						'</td>';
			};
			tableHtml += '<tr>';
			tableHtml += '<td bgcolor="#403f41">&nbsp;</td>';
			tableHtml += _columnNameFnc('00063');
			tableHtml += _columnNameFnc('00064');
			tableHtml += _columnNameFnc('00065');
			tableHtml += '</tr>';

			// HTMLにXMLの承認者一覧をバインド
			// レコード用カラムファンクション
			var _recordColumnFnc = function(id, label, width) {
				var result = 	'<td bgcolor="#F0F0F0" width="' + width + '" ' + 
									'style="border-right:1px solid white; border-bottom:1px solid white;"'+
									'onclick="var _check = $(\'#' + id + '\'); _check.prop(\'checked\', !_check.prop(\'checked\')); return false;" ' +
								'>' +
									label +
								'</td>';
				return result;
			}
			$(xml).find("user").each(function(){
				// 上司モードで上司ではないなら終了
				var bossFlag = $(this).attr('bossFlag') === "true";
				if (onlyBoss && !bossFlag) {
					return true;
				}
				
				//グループ分離用(空回しする必要があるかも)
				fgroup = ffreegrouplistposition;
				ffreegrouplistposition = ffreegrouplistposition + 1;
				
				// id
				var _id = $(this).attr('userId') + '_' + fgroup;
				
				// 1ユーザレコード作成
				tableHtml += '<tr>';
				// インプット
				tableHtml += '<td bgcolor="#F0F0F0" width="6%"  style="border-bottom:1px solid white;">';
				tableHtml += '<input style="width: 25px;height: 25px;" type="checkbox" ';
					tableHtml += ('id="'   + _id + '" ');
					tableHtml += ('name="' + $(this).attr('userName')  + '" ');
					tableHtml += ('value="value" />');
				tableHtml += '</td>';
				
				// 表示部
				tableHtml += _recordColumnFnc(_id, $(this).attr('userName'),  "auto");
				
				var _groupHtml = "";
				$(this).find("group").each(function(){
					_groupHtml += "<li>";
					_groupHtml += $(this).attr('name');
					_groupHtml += "</li>";
				});
				_groupHtml = (_groupHtml.length > 0) ? "<ul>"+ _groupHtml +"</ul>" : "&nbsp;";

				var _roleHtml = "";
				$(this).find("role").each(function(){
					_roleHtml += "<li>";
					_roleHtml += $(this).attr('name');
					_roleHtml += "</li>";
				});
				_roleHtml = (_roleHtml.length > 0) ? "<ul>"+ _roleHtml +"</ul>" : "&nbsp;";


				tableHtml += _recordColumnFnc(_id, _groupHtml, "30%");
				tableHtml += _recordColumnFnc(_id, _roleHtml,  "30%");
				
				tableHtml += '</tr>';
			});
			tableHtml += '</table>';
			pcheckGroupList_scr_approverList.append(tableHtml);
		});
		
		/**
		 * 「post_dspApproverError」イベントハンドラ
		 */
		view.on('post_dspApproverError', function(e, xml) {
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
					// statusTypeNameタブ
					pCurrentFolder[$(this).attr('objId')]=new Object();
					pCurrentFolder[$(this).attr('objId')]['statusTypeName']=$(this).attr('statusTypeName');
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
		//model.post_dspApproveDocumentList();
	});
	
	jQuery(document).on('pagebeforeshow', viewId, function(ev) {
		var ffile_extension = "";
		var ficon_to_show_html = "";
		var fdocument_info_to_show_html = "";
		var fproperties_to_show_html = "";
		var fcomment = "";
		var fone_id = "";
		var fnames = false;
		
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
		
		// 変数をリセット
		papprovers_list = [];
		pcheckGroupList_scr_approverList.empty();
		
		$('.header_title').empty().append(cmnLanguage['LAN_'+planguage+'_00047']);
		
		$('#check_scr_View-collapsible_text').empty().append(cmnLanguage['LAN_'+planguage+'_00048']);
		
		// パンクずと検索を非表示
		$('.folderList_scr_View-folderList_scr_order_combo_div').hide();
		$('.folderList_scr_View-folderList_scr_bc').hide();
		
		// 属性情報を表示
		check_scr_info_div.hide();
		pcollapsible.empty().append('<a href="#" id="check_scr_View-collapsible_open">'+cmnGetimg['check_scr_btn_History_plus']+"</a>");
		
		// データーをクリア
		pcheck_scr_checkFile_Info_icon.empty();
		pcheck_scr_checkFile_Info_docinfo.empty();
		pcheck_scr_checkFile_Info_properties.empty();
		
		pfolderList_scr_timing = "";
		pfolderList_scr_statusId = "";
		pfolderList_scr_finalApprove = false;
		pfolderList_scr_statusMDateLong = "";
		pfolderList_scr_forcastStatusTypeId = "";
		pfolderList_scr_ObjId = "";
		pfolderList_scr_ObjName = "";
		pfolderList_scr_RequestUserName = "";
		pfolderList_scr_RequestDate = "";
		pfolderList_scr_Comment = "";
		pfolderList_scr_approverId = "";
		pfolderList_scr_approverName = "";
		pfolderList_scr_req_approverId = "";
		pfolderList_scr_req_approverName = "";
		
		
		// ローカルストレージからパラメータを取得
		if (JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_CHECK_ELEMENT)) != null)
		{
			pfolderList_scr_ObjId = JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_CHECK_ELEMENT));
		}
		if (JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_CHECK_ELEMENT_OBJNAME)) != null)
		{
			pfolderList_scr_ObjName = JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_CHECK_ELEMENT_OBJNAME));
		}
		if (JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_CHECK_ELEMENT_REQUESTUSERNAME)) != null)
		{
			pfolderList_scr_RequestUserName = JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_CHECK_ELEMENT_REQUESTUSERNAME));
		}
		if (JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_CHECK_ELEMENT_REQUESTDATE)) != null)
		{
			pfolderList_scr_RequestDate = JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_CHECK_ELEMENT_REQUESTDATE));
		}
		if (JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_CHECK_ELEMENT_COMMENT)) != null)
		{
			pfolderList_scr_Comment = JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_CHECK_ELEMENT_COMMENT));
		}
		if (JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_CHECK_ELEMENT_TIMING)) != null)
		{
			pfolderList_scr_timing = JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_CHECK_ELEMENT_TIMING));
		}
		if (JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_CHECK_ELEMENT_STATUSID)) != null)
		{
			pfolderList_scr_statusId = JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_CHECK_ELEMENT_STATUSID));
		}
		if (JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_CHECK_ELEMENT_FINALAPPROVE)) != null)
		{
			pfolderList_scr_finalApprove = JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_CHECK_ELEMENT_FINALAPPROVE)) == "true" ? true : false ; 
		}
		if (JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_CHECK_ELEMENT_STATUSMDATELONG)) != null)
		{
			pfolderList_scr_statusMDateLong = JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_CHECK_ELEMENT_STATUSMDATELONG));
		}
		if (JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_CHECK_ELEMENT_FORCASTSTATUSTYPEID)) != null)
		{
			pfolderList_scr_forcastStatusTypeId = JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_CHECK_ELEMENT_FORCASTSTATUSTYPEID));
		}
		if (JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_CHECK_ELEMENT_APPROVERS)) != null)
		{
			pfolderList_scr_approverId = JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_CHECK_ELEMENT_APPROVERS));
		}
		if (JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_CHECK_ELEMENT_APPROVERS_NAMES)) != null)
		{
			pfolderList_scr_approverName = JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_CHECK_ELEMENT_APPROVERS_NAMES));
		}
		
		if (JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_CHECK_ELEMENT_ISDOCUMENT)) != null)
		{
			pisDocument = JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_CHECK_ELEMENT_ISDOCUMENT));
		}
		else 
		{
			pisDocument = true;
		}
		
		// 遷移先ステータスタイプのリスト
		pStatusList = JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_CHECK_ELEMENT_STATUSLIST));
		
		$('#check_scr_View-statusDestinationSelectMenu').empty();
		for(var i = 0; i < pStatusList.length; i++){
			
			// 遷移先セレクトメニューにアイテムを追加する
			$('#check_scr_View-statusDestinationSelectMenu').append(
				'<option value="' + pStatusList[i].statusTypeId + '">' + pStatusList[i].statusTypeName + '</option>'
			);
			
			var statusTypeId = pStatusList[i].statusTypeId;
			var finalApprove = pStatusList[i].finalApprove;
			var approverArray = createApproverObjectList(pStatusList[i].approverId, pStatusList[i].approverName);
			pStatusApproverMap[statusTypeId] = approverArray;
			
			if(i == 0){
				papprovers_list = approverArray;
				// 選択された遷移ステータスタイプのID
				pfolderList_scr_forcastStatusTypeId = statusTypeId;
				// 選択された遷移先ステータスタイプが最終承認かどうか
				pfolderList_scr_finalApprove = finalApprove == "true" ? true : false;
			}
		}
		// 遷移先セレクトメニューをリフレッシュ
		$('#check_scr_View-statusDestinationSelectMenu').selectmenu('refresh');
		
		// ドキュメントアイコン、インフォ、属性ボタンを表示
		ffile_extension = pfolderList_scr_ObjName.substring(pfolderList_scr_ObjName.lastIndexOf(".") + 1);

		if (pisDocument == true)
		{
			switch(ffile_extension.substring(0,3)) 
			{
				case "xls":
					ficon_to_show_html = '<a href="#">'+cmnGetimg['list_scr_document_excel']+'</a>'; break;
				case "XLS":
					ficon_to_show_html = '<a href="#">'+cmnGetimg['list_scr_document_excel']+'</a>'; break;
				case "pdf":
					ficon_to_show_html = '<a href="#">'+cmnGetimg['list_scr_document_pdf']+'</a>'; break;
				case "PDF":
					ficon_to_show_html = '<a href="#">'+cmnGetimg['list_scr_document_pdf']+'</a>'; break;
				case "ppt":
					ficon_to_show_html = '<a href="#">'+cmnGetimg['list_scr_document_ppt']+'</a>'; break;
				case "PPT":
					ficon_to_show_html = '<a href="#">'+cmnGetimg['list_scr_document_ppt']+'</a>'; break;
				case "doc":
					ficon_to_show_html = '<a href="#">'+cmnGetimg['list_scr_document_doc']+'</a>'; break;
				case "DOC":
					ficon_to_show_html = '<a href="#">'+cmnGetimg['list_scr_document_doc']+'</a>'; break;
				default:
					ficon_to_show_html = '<a href="#">'+cmnGetimg['list_scr_document']+'</a>';
			};	
		}
		else 
		{
			ficon_to_show_html = '<a href="#">'+cmnGetimg['list_scr_folder_icon']+'</a>';
		}
		
		pcheck_scr_checkFile_Info_icon.empty().append(ficon_to_show_html);
		
		fcomment = pfolderList_scr_Comment;
		if ($(document.body).height()>800)
		{
			fcomment = resizeString(fcomment, viewName, 'portrait');
		}
		else
		{
			fcomment = resizeString(fcomment, viewName, 'landscape');
		}
		
		fdocument_info_to_show_html = '<font style="font-size: 150%;font-weight: bold;">' + pfolderList_scr_ObjName +'</font></br>'+
			cmnLanguage['LAN_'+planguage+'_00030'] + ': '+pfolderList_scr_RequestUserName+' 　　'+
			cmnLanguage['LAN_'+planguage+'_00031']+': '+pfolderList_scr_RequestDate+'</br>'+
			cmnLanguage['LAN_'+planguage+'_00033']+': '+fcomment;
		
		pcheck_scr_checkFile_Info_docinfo.empty().append(fdocument_info_to_show_html);
		fproperties_to_show_html = '<p style="line-height: 400%;"><center><a class="check_scr_btn_Attr" id="check_scr_btn_Attr_'+
			pfolderList_scr_ObjId + '" href="#">' + 
			cmnGetimg['list_scr_btn_Properties']+'<br><font color="black">'+cmnLanguage['LAN_'+planguage+'_00015'] + 
			'</font></a></center></p>';
		pcheck_scr_checkFile_Info_properties.empty().append(fproperties_to_show_html);
		
		// 遷移先
		pcheck_scr_appStatusDestination_label.empty().append(cmnLanguage['LAN_'+planguage+'_00064']);
		
		// 承認依頼先選択
		pcheck_scr_appReqDestination_label.empty().append(cmnLanguage['LAN_'+planguage+'_00049']);
		pcheck_scr_comment_label.empty().append(cmnLanguage['LAN_'+planguage+'_00051']);
		pcheck_scr_comment.val("");
		
		setApprover();

		// ボタンラベル
		$('#check_scr_View-check_scr_button_label_Set').empty().append(cmnLanguage['LAN_'+planguage+'_00052']);
		$('#check_scr_View-check_scr_button_label_Undo').empty().append(cmnLanguage['LAN_'+planguage+'_00053']);
		$('#check_scr_View-check_scr_button_label_Cancel').empty().append(cmnLanguage['LAN_'+planguage+'_00054']);
		
		pcurrentView = viewName;
		// ポップアップレベルを設定
		$('#popup-select_next_approver_header_label').empty().append(cmnLanguage['LAN_'+planguage+'_00050']);
		pcheckGroupList_scr_cancel_label.empty().append(cmnLanguage['LAN_'+planguage+'_00054']);
		pcheckGroupList_scr_select_label.empty().append(cmnLanguage['LAN_'+planguage+'_00057']);
		pcheckGroupList_scr_dispSup_label.empty().append(cmnLanguage['LAN_'+planguage+'_00058']);
		
		// 承認履歴を取り得
		model.post_WorkFlowHistory(pfolderList_scr_ObjId);
		
		// 属性フラッグのクーキーをアップデート
		window.localStorage.setItem(EIM_KEY_CURRENT_FORMER_VIEW, JSON.stringify(viewName));
		
		// ボタンハイライト
		cmn_header_button_highlight (2);
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
		var fcomment = "";
		var fdocument_info_to_show_html = "";
		
		// ＯＳによって、アイコンのサイズを変わる
		cmn_change_image_sizes (0, planguage);
		if (pcollapsible.html().indexOf('collapsible_close')>0)
		{	
			pcollapsible.empty().append('<a href="#" id="check_scr_View-collapsible_close">'+
					cmnGetimg['check_scr_btn_History_minus']+"</a>");
		}
		else 
		{
			pcollapsible.empty().append('<a href="#" id="check_scr_View-collapsible_open">'+
					cmnGetimg['check_scr_btn_History_plus']+"</a>");
		}
		
		if ((navigator.userAgent.indexOf("iPad")>0)&&((navigator.userAgent.indexOf("OS 8")>0)||(navigator.userAgent.indexOf("OS 7")>0)))
		{
			$('#check_scr_View-filler').empty().append("<br><br><br><br><br><br><br>");
			$('#check_scr_View-check_content_div').css("height", "607px");
		}
		else if (navigator.userAgent.indexOf("iPad")>0)
		{
			$('#check_scr_View-filler').empty().append("<br><br><br><br><br><br><br>");
			$('#check_scr_View-check_content_div').css("height", "557px");
		}
		else if (navigator.userAgent.indexOf("iPhone")>0)
		{
			$('#check_scr_View-filler').empty().append("<br><br><br><br><br><br><br>");
			$('#check_scr_View-check_content_div').css("height", "1040px");		
		}
		else 
		{
			$('#check_scr_View-filler').empty().append("<br><br><br><br><br><br><br>");
			$('#check_scr_View-check_content_div').css("height", "557px");
		}
		
		fcomment = pfolderList_scr_Comment;
		if ($(document.body).height()>800)
		{
			fcomment = resizeString(fcomment, viewName, 'portrait');
		}
		else
		{
			fcomment = resizeString(fcomment, viewName, 'landscape');
		}
		
		fdocument_info_to_show_html = '<font style="font-size: 150%;font-weight: bold;">' + pfolderList_scr_ObjName +'</font></br>'+
		cmnLanguage['LAN_'+planguage+'_00030'] + ': '+pfolderList_scr_RequestUserName+' 　　'+
		cmnLanguage['LAN_'+planguage+'_00031']+': '+pfolderList_scr_RequestDate+'</br>'+
		cmnLanguage['LAN_'+planguage+'_00033']+': '+fcomment;
	
		pcheck_scr_checkFile_Info_docinfo.empty().append(fdocument_info_to_show_html);
	}
	
	/********************************************************************************
	 * [機能]	横向きになった場合
	 * [引数]	無し
	 * [戻値]	無し
	 ********************************************************************************/
	function resize_screen_landscape() {
		var fcomment = "";
		var fdocument_info_to_show_html = "";
		
		// ＯＳによって、アイコンのサイズを変わる
		cmn_change_image_sizes (1, planguage);
		if (pcollapsible.html().indexOf('collapsible_close')>0)
		{	
			pcollapsible.empty().append('<a href="#" id="check_scr_View-collapsible_close">'+
					cmnGetimg['check_scr_btn_History_minus']+"</a>");
		}
		else 
		{
			pcollapsible.empty().append('<a href="#" id="check_scr_View-collapsible_open">'+
					cmnGetimg['check_scr_btn_History_plus']+"</a>");
		}
		
		if (navigator.userAgent.indexOf("iPad")>0)
		{
			$('#check_scr_View-filler').empty();
			$('#check_scr_View-check_content_div').css("height", "400px");
		}
		else if (navigator.userAgent.indexOf("iPhone")>0)
		{
			$('#check_scr_View-filler').empty();
			$('#check_scr_View-check_content_div').css("height", "610px");
		}
		else 
		{
			$('#check_scr_View-filler').empty();
			$('#check_scr_View-check_content_div').css("height", "400px");
		}
		
		fcomment = pfolderList_scr_Comment;
		if ($(document.body).height()>800)
		{
			fcomment = resizeString(fcomment, viewName, 'portrait');
		}
		else
		{
			fcomment = resizeString(fcomment, viewName, 'landscape');
		}
		
		fdocument_info_to_show_html = '<font style="font-size: 150%;font-weight: bold;">' + pfolderList_scr_ObjName +'</font></br>'+
		cmnLanguage['LAN_'+planguage+'_00030'] + ': '+pfolderList_scr_RequestUserName+' 　　'+
		cmnLanguage['LAN_'+planguage+'_00031']+': '+pfolderList_scr_RequestDate+'</br>'+
		cmnLanguage['LAN_'+planguage+'_00033']+': '+fcomment;
	
		pcheck_scr_checkFile_Info_docinfo.empty().append(fdocument_info_to_show_html);
	}
	
	function createApproverObjectList(approverId, approverName) {
		var array = [];
		
		if(approverId == null || approverId == null){
			return array;
		}
		
		jQuery.each(approverId.split(','), function(i, val) {
			if(val == ""){
				return;
			}
			var tempArray = val.split(':');
			var approver = new Object();
			approver.approverId = tempArray[1];
			array.push(approver);
		});
		
		jQuery.each(approverName.split(','), function(i, val) {
			if(val == ""){
				return;
			}
			var approver = array[i];
			approver.approverName = val;
		});
		
		return array;
	}
	
	/********************************************************************************
	 * [機能]	承認依頼先の文字列をセット
	 * [引数]	無し
	 * [戻値]	無し
	 ********************************************************************************/
	function setApprover() {
		
		var str = "";
		var approverIds = "";
		var approverNames = "";
		
		if(pfolderList_scr_finalApprove){
			// 最終承認の場合
			
			pappReqDestination_button_label.empty().append(cmnLanguage['LAN_'+planguage+'_00056']);
			// 承認依頼先選択ボタンを非活性
			pcheck_scr_appReqDestination.attr("href", "");
		}else{
			
			jQuery.each(papprovers_list, function(i, object) {
				if(i == 0){
					str += object.approverName;
					approverIds += "1:" + object.approverId;
					approverNames += object.approverName;
				}else{
					str += "　" + object.approverName;
					approverIds += "," + "1:" + object.approverId;
					approverNames += "," + object.approverName;
				}
				
			});
			
			// ラベル変更
			if(papprovers_list.length > 0){
				pappReqDestination_button_label.empty().append(str);
			}else{
				// 承認依頼先に空をセットすると、コンポーネントのサイズが決まらないため
				// サイズがずれる。空の場合は全角空白文字をセットする。
				pappReqDestination_button_label.empty().append("　");
			}
			// 遷移先ステータスタイプで選択された承認依頼先をセット
			pStatusApproverMap[pfolderList_scr_forcastStatusTypeId] = $.extend(true, [], papprovers_list);
			// 承認依頼先選択ボタンを活性
			pcheck_scr_appReqDestination.attr("href", "#check-popup");
			
			// ローカルストレージを更新する
			// 承認依頼先(ユーザーID)
			window.localStorage.setItem(EIM_KEY_CURRENT_CHECK_ELEMENT_APPROVERS, JSON.stringify(approverIds));
			// 承認依頼先名称
			window.localStorage.setItem(EIM_KEY_CURRENT_CHECK_ELEMENT_APPROVERS_NAMES, JSON.stringify(approverNames));
			
		}
		
	}
	
	/**
	 * getParameter
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
			if (pcollapsible!=null){
				check_scr_info_div.hide();
				pcollapsible.empty();
				pcheck_scr_history_desc.empty();
				$('#check_scr_View-collapsible_text').empty();
				pcheck_scr_checkFile_Info_icon.empty();
				pcheck_scr_checkFile_Info_docinfo.empty();
				pcheck_scr_checkFile_Info_properties.empty();
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
