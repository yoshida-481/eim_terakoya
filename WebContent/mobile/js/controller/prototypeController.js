/**
 * prototypeController
 */
var prototypeController = function () {
	
	/**
	 * testテンプレート
	 */
	function test () {
		return null;
	}
	
	/**
	 * getParameterテンプレート
	 */
	function getParameter () {
		return null;
	}
	
	/**
	 * setParameterテンプレート
	 */
	function setParameter (data) {
		;
	}
	
	/***********************************************************************************
	 * [機能]	画面にポップアップメッセージを表示
	 * [引数]	message 伝えたいメッセージ
	 * 			type：”ＷＡＲ”＝ワーニング、”ＥＲＲ”＝エラー、”ＩＮＦ”＝インフォ
	 * 			viewName: ビュー名
	 * [戻値]	無し
	 ***********************************************************************************/
	function cmnPopup(message, type, viewName){
		switch (type){
			case "WAR": 
				$('#'+viewName+'-message_popup_header_text').empty().append("Warning");
				$('#'+viewName+'-message_popup_scr_img_confirm_dialog').empty().append(cmnGetimg['popup_warning']);
				break;
			case "ERR": 
				$('#'+viewName+'-message_popup_header_text').empty().append("Error");
				$('#'+viewName+'-message_popup_scr_img_confirm_dialog').empty().append(cmnGetimg['popup_error']);				
				break;
			case "INF": 
				$('#'+viewName+'-message_popup_header_text').empty().append("Information");
				$('#'+viewName+'-message_popup_scr_img_confirm_dialog').empty().append(cmnGetimg['popup_notice']);
				break;
		}
		
		$('#'+viewName+'-message_popup_text').empty().append(message);
		$('#'+viewName+'-message_popup').popup('open', {'transition' : 'pop' , 'position-to' : 'window'});
	}

	/***********************************************************************************
	 * [機能]	画面にポップアップメッセージを表示
	 * [引数]	message 伝えたいメッセージ
	 * 			type：”ＷＡＲ”＝ワーニング、”ＥＲＲ”＝エラー、”ＩＮＦ”＝インフォ
	 * 			viewName: ビュー名
	 * [戻値]	無し
	 ***********************************************************************************/
	function cmnPopup2(message, type, viewName){
		switch (type){
			case "WAR": 
				$('#'+viewName+'-message_popup_header_text_check').empty().append("Warning");
				$('#'+viewName+'-message_popup_scr_img_confirm_dialog_check').empty().append(cmnGetimg['popup_warning']);
				break;
			case "ERR": 
				$('#'+viewName+'-message_popup_header_text_check').empty().append("Error");
				$('#'+viewName+'-message_popup_scr_img_confirm_dialog_check').empty().append(cmnGetimg['popup_error']);				
				break;
			case "INF": 
				$('#'+viewName+'-message_popup_header_text_check').empty().append("Information");
				$('#'+viewName+'-message_popup_scr_img_confirm_dialog_check').empty().append(cmnGetimg['popup_notice']);
				break;
		}
		
		$('#'+viewName+'-message_popup_text_check').empty().append(message);
		$("#check_scr_View-message_popup_check").show();
	}
	
	/***********************************************************************************
	 * [機能]	ユーザーセッションデーターを確認、登録
	 * [引数]	sessionData	ユーザーセッションデーター
	 * [戻値]	容認＝true, 拒否＝false
	 ***********************************************************************************/
	function cmnSessionCheck(sessionData){
		if (sessionData == null){
			// クッキーを削除
			try {
				window.localStorage.removeItem(EIM_KEY_USERID);
				window.localStorage.removeItem(EIM_KEY_USERCODE);
				window.localStorage.removeItem(EIM_KEY_USERNAME);
				window.localStorage.removeItem(EIM_KEY_USERKANA);
				window.localStorage.removeItem(EIM_KEY_USERMAIL);
				window.localStorage.removeItem(EIM_KEY_USERADMIN);
				window.localStorage.removeItem(EIM_KEY_GROUPPATH);
				window.localStorage.removeItem(EIM_KEY_APPROVEDOCUMENT);
				window.localStorage.removeItem(EIM_KEY_ISPOPUPEXISTS);
				window.localStorage.removeItem(EIM_KEY_VICEAPPROVE);
				window.localStorage.removeItem(EIM_KEY_SYSTEMSECURITY);
				window.localStorage.removeItem(EIM_KEY_TEXTATTRMAXCHARS);
				return true;	
			}
			catch (e){
				return false;
			}
		}
		else if (sessionData['userId'] == ""){
			// クッキーを取得
			try {
				sessionData['userId']=JSON.parse(window.localStorage.getItem(EIM_KEY_USERID));
				sessionData['userCode']=JSON.parse(window.localStorage.getItem(EIM_KEY_USERCODE));
				sessionData['userName']=JSON.parse(window.localStorage.getItem(EIM_KEY_USERNAME));
				sessionData['userKana']=JSON.parse(window.localStorage.getItem(EIM_KEY_USERKANA));
				sessionData['userMail']=JSON.parse(window.localStorage.getItem(EIM_KEY_USERMAIL));
				sessionData['userAdmin']=JSON.parse(window.localStorage.getItem(EIM_KEY_USERADMIN));
				sessionData['groupPath']=JSON.parse(window.localStorage.getItem(EIM_KEY_GROUPPATH));
				sessionData['approveDocument']=JSON.parse(window.localStorage.getItem(EIM_KEY_APPROVEDOCUMENT));
				sessionData['isPopupExists']=JSON.parse(window.localStorage.getItem(EIM_KEY_ISPOPUPEXISTS));
				sessionData['viceApprove']=JSON.parse(window.localStorage.getItem(EIM_KEY_VICEAPPROVE));
				sessionData['systemSecurity']=JSON.parse(window.localStorage.getItem(EIM_KEY_SYSTEMSECURITY));
				sessionData['textAttrMaxChars']=JSON.parse(window.localStorage.getItem(EIM_KEY_TEXTATTRMAXCHARS));
				sessionData['language']=JSON.parse(window.localStorage.getItem(EIM_KEY_LANGUAGE));
				return true;	
			}
			catch (e){
				return false;
			}
		}
		else if ((JSON.parse(window.localStorage.getItem(EIM_KEY_USERID))==null)||(JSON.parse(window.localStorage.getItem(EIM_KEY_USERID))=="")){
			// クッキーと設定
			try {
				window.localStorage.setItem(EIM_KEY_USERID, JSON.stringify(sessionData['userId']));
				window.localStorage.setItem(EIM_KEY_USERCODE, JSON.stringify(sessionData['userCode']));
				window.localStorage.setItem(EIM_KEY_USERNAME, JSON.stringify(sessionData['userName']));
				window.localStorage.setItem(EIM_KEY_USERKANA, JSON.stringify(sessionData['userKana']));
				window.localStorage.setItem(EIM_KEY_USERMAIL, JSON.stringify(sessionData['userMail']));
				window.localStorage.setItem(EIM_KEY_USERADMIN, JSON.stringify(sessionData['userAdmin']));
				window.localStorage.setItem(EIM_KEY_GROUPPATH, JSON.stringify(sessionData['groupPath']));
				window.localStorage.setItem(EIM_KEY_APPROVEDOCUMENT, JSON.stringify(sessionData['approveDocument']));
				window.localStorage.setItem(EIM_KEY_ISPOPUPEXISTS, JSON.stringify(sessionData['isPopupExists']));
				window.localStorage.setItem(EIM_KEY_VICEAPPROVE, JSON.stringify(sessionData['viceApprove']));
				window.localStorage.setItem(EIM_KEY_SYSTEMSECURITY, JSON.stringify(sessionData['systemSecurity']));
				window.localStorage.setItem(EIM_KEY_TEXTATTRMAXCHARS, JSON.stringify(sessionData['textAttrMaxChars']));
				window.localStorage.setItem(EIM_KEY_LANGUAGE, JSON.stringify(sessionData['language']));
				return true;	
			}
			catch (e){
				return false;
			}
		}
		else if ((sessionData['userId']!=JSON.parse(window.localStorage.getItem(EIM_KEY_USERID))) &&
			(sessionData['userCode']!=JSON.parse(window.localStorage.getItem(EIM_KEY_USERCODE))) &&
			(sessionData['userName']!=JSON.parse(window.localStorage.getItem(EIM_KEY_USERNAME))) &&
			(sessionData['userKana']!=JSON.parse(window.localStorage.getItem(EIM_KEY_USERKANA))) &&
			(sessionData['userMail']!=JSON.parse(window.localStorage.getItem(EIM_KEY_USERMAIL))) &&
			(sessionData['userAdmin']!=JSON.parse(window.localStorage.getItem(EIM_KEY_USERADMIN))) &&
			(sessionData['groupPath']!=JSON.parse(window.localStorage.getItem(EIM_KEY_GROUPPATH))) &&
			(sessionData['approveDocument']!=JSON.parse(window.localStorage.getItem(EIM_KEY_APPROVEDOCUMENT))) &&
			(sessionData['isPopupExists']!=JSON.parse(window.localStorage.getItem(EIM_KEY_ISPOPUPEXISTS))) &&
			(sessionData['viceApprove']!=JSON.parse(window.localStorage.getItem(EIM_KEY_VICEAPPROVE))) &&
			(sessionData['systemSecurity']!=JSON.parse(window.localStorage.getItem(EIM_KEY_SYSTEMSECURITY))) &&
			(sessionData['textAttrMaxChars']!=JSON.parse(window.localStorage.getItem(EIM_KEY_TEXTATTRMAXCHARS))) &&
			(sessionData['language']!=JSON.parse(window.localStorage.getItem(EIM_KEY_LANGUAGE)))){
			// クッキーと比べる
			return false;
		}
		else {
			return true;
		};
	}

	/***********************************************************************************
	 * [機能]	パンくずリストを登録、取得
	 * [引数]	breadcrumbsData	ＩＤのパンくずリスト
	 *          breadcrumbs_namesData ワークスペースのフォルダ名のパンくずリスト
	 *          currentFolderData pCurrentFolderコンテイナー
	 * [戻値]	容認＝true, 拒否＝false
	 ***********************************************************************************/
	function cmnBreadcrumbsCheck(breadcrumbsData, breadcrumbs_namesData, currentFolderData){
		var fbreadcrumbsData;
		var fbreadcrumbs_namesData;
		var fcurrentFolderData;
		
		if (breadcrumbsData == null){
			// クッキーを削除
			try {
				window.localStorage.removeItem(EIM_KEY_CURRENT_BREADCRUMBS);
				window.localStorage.removeItem(EIM_KEY_CURRENT_BREADCRUMBS_NAMES);
				window.localStorage.removeItem(EIM_KEY_CURRENT_FOLDER);
				return true;	
			}
			catch (e){
				return false;
			}
		}
		else if (breadcrumbsData[0] == ""){
			// クッキーを取得
			try {
				fbreadcrumbsData = JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_BREADCRUMBS));
				fbreadcrumbs_namesData = JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_BREADCRUMBS_NAMES));
				fcurrentFolderData = JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_FOLDER));
				if (fbreadcrumbsData!=null)
				{
					for (var key in fbreadcrumbsData)
					{
						breadcrumbsData[parseInt(key)]=fbreadcrumbsData[key];
					}
						
				}
				if (fbreadcrumbs_namesData!=null)
				{
					for (var key in fbreadcrumbs_namesData)
					{
						breadcrumbs_namesData[parseInt(key)]=fbreadcrumbs_namesData[key];
					}	
				}
				if (fcurrentFolderData!=null)
				{
					for (var key in fcurrentFolderData)
					{
						currentFolderData[key]=fcurrentFolderData[key];
					}	
				}
				return true;
			}
			catch (e){
				return false;
			}
		}
		else if ((JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_BREADCRUMBS))==null)||
				 (JSON.parse(window.localStorage.getItem(EIM_KEY_CURRENT_BREADCRUMBS))=="")){
			// クッキーと設定
			try {
				window.localStorage.setItem(EIM_KEY_CURRENT_BREADCRUMBS, JSON.stringify(breadcrumbsData));
				window.localStorage.setItem(EIM_KEY_CURRENT_BREADCRUMBS_NAMES, JSON.stringify(breadcrumbs_namesData));
				window.localStorage.setItem(EIM_KEY_CURRENT_FOLDER, JSON.stringify(currentFolderData));
				return true;	
			}
			catch (e){
				return false;
			}
		}
	}
	
	/********************************************************************************
	 * [機能]	ログアウトの場合、ブラウザーのメモリとクーキーを削除
	 * [引数]	無し
	 * [戻値]	無し
	 ********************************************************************************/
	function memory_clean()
	{
		// セッション
		cmnSessionCheck(null);
		window.localStorage.removeItem(EIM_KEY_CURRENT_CHECK_FORCE_WAIT_LANGUAGE);
		// フォルダ一覧
		cmnBreadcrumbsCheck(null, null, null);
		window.localStorage.removeItem(EIM_KEY_APPROVEDOCUMENT_COUNT);
		window.localStorage.removeItem(EIM_KEY_CURRENT_FOLDER);
		window.localStorage.removeItem(EIM_KEY_CURRENT_BREADCRUMBS);
		window.localStorage.removeItem(EIM_KEY_CURRENT_BREADCRUMBS_NAMES);
		window.localStorage.removeItem(EIM_KEY_CURRENT_LIST_ORDER);
		window.localStorage.removeItem(EIM_KEY_CURRENT_LIST_SEARCH);
		window.localStorage.removeItem(EIM_KEY_CURRENT_LIST_SEARCH_KEYWORD);
		window.localStorage.removeItem(EIM_KEY_CURRENT_LIST_FOLDERID);
		window.localStorage.removeItem(EIM_KEY_CURRENT_LIST_TOP_ELEMENT);
		folderList_scr_Controller.setParameter(null);
		// 属性
		properties_scr_Controller.setParameter(null);
		window.localStorage.removeItem(EIM_KEY_CURRENT_FORMER_VIEW);
		window.localStorage.removeItem(EIM_KEY_CURRENT_PROPERTY_ELEMENT);
		window.localStorage.removeItem(EIM_KEY_CURRENT_PROPERTY_STATUSTYPENAME);
		window.localStorage.removeItem(EIM_KEY_CURRENT_PROPERTY_READONLY);
		window.localStorage.removeItem(EIM_KEY_CURRENT_PROPERTY_ISPUBLISHED);
		window.localStorage.removeItem(EIM_KEY_CURRENT_PROPERTY_OBJTYPENAME);
		// お気に入り
		favorites_scr_Controller.setParameter(null);
		// 未承認
		unapproved_scr_Controller.setParameter(null);
		// 承認
		check_scr_Controller.setParameter (null);
		window.localStorage.removeItem(EIM_KEY_CURRENT_CHECK_ELEMENT);
		window.localStorage.removeItem(EIM_KEY_CURRENT_CHECK_ELEMENT_OBJNAME);
		window.localStorage.removeItem(EIM_KEY_CURRENT_CHECK_ELEMENT_REQUESTUSERNAME);
		window.localStorage.removeItem(EIM_KEY_CURRENT_CHECK_ELEMENT_REQUESTDATE);
		window.localStorage.removeItem(EIM_KEY_CURRENT_CHECK_ELEMENT_COMMENT);
		window.localStorage.removeItem(EIM_KEY_CURRENT_CHECK_ELEMENT_TIMING);
		window.localStorage.removeItem(EIM_KEY_CURRENT_CHECK_ELEMENT_STATUSID);
		window.localStorage.removeItem(EIM_KEY_CURRENT_CHECK_ELEMENT_FINALAPPROVE);
		window.localStorage.removeItem(EIM_KEY_CURRENT_CHECK_ELEMENT_STATUSMDATELONG);
		window.localStorage.removeItem(EIM_KEY_CURRENT_CHECK_ELEMENT_FORCASTSTATUSTYPEID);
		window.localStorage.removeItem(EIM_KEY_CURRENT_CHECK_ELEMENT_APPROVERS);
		window.localStorage.removeItem(EIM_KEY_CURRENT_CHECK_ELEMENT_APPROVERS_NAMES);
		window.localStorage.removeItem(EIM_KEY_CURRENT_CHECK_ELEMENT_REQ_APPROVERS);
		window.localStorage.removeItem(EIM_KEY_CURRENT_CHECK_ELEMENT_REQ_APPROVERS_NAMES);
		window.localStorage.removeItem(EIM_KEY_CURRENT_CHECK_ELEMENT_ISDOCUMENT);
		// パンクずと検索を表示
		// デフォルト
		$('#folderList_scr_View-folderList_scr_breadcrumb').empty().append('<ul id="breadcrumbs-one"><li><a class="folderList_scr_breadcrumb" id="Ｔｏｐ" href="#">　Ｔｏｐ　</a></li></ul>');
	}
		
	return {
		getParameter : getParameter,
		setParameter : setParameter,
		test : test,
		cmnPopup : cmnPopup,
		cmnPopup2 : cmnPopup2,
		cmnSessionCheck : cmnSessionCheck,
		cmnBreadcrumbsCheck: cmnBreadcrumbsCheck,
		memory_clean: memory_clean,
	};
	
}();

