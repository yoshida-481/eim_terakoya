/*******************************************************************
 * [機能]	ヘッダーボタンをハイライト
 * レスポンシブデザイン・ＯＳによって、アイコンのサイズを変わる
 * [引数]	button_id	アイライトボタンＩＤ
 * 			１：フォルダ一覧 ２：未承認 ３：お気に入り
 * [戻値]	無し
 *******************************************************************/
function cmn_header_button_highlight (button_id){
	
	// デフォルト
	var style1 = document.createElement('style');
	style1.type = 'text/css';
	style1.innerHTML = '.ui-icon-main_scr_btn_folderList {background: url(../image/main_scr_btn_FolderList.png) 100% 100% no-repeat !important;}';
	document.getElementsByTagName('head')[0].appendChild(style1);
	
	var style2 = document.createElement('style');
	style2.type = 'text/css';
	style2.innerHTML = '#header-btn_FolderList {background: -webkit-linear-gradient(top, #0572ff 0%, #4789fd 100%) !important;}';
	document.getElementsByTagName('head')[0].appendChild(style2);
	
	var style3 = document.createElement('style');
	style3.type = 'text/css';
	style3.innerHTML = '#header-btn_FolderList .ui-btn-text {color: white !important;}';
	document.getElementsByTagName('head')[0].appendChild(style3);
	
	var style4 = document.createElement('style');
	style4.type = 'text/css';
	style4.innerHTML = '.ui-icon-main_scr_btn_Unapproved {background: url(../image/main_scr_btn_Unapproved.png) 100% 100% no-repeat !important;}';
	document.getElementsByTagName('head')[0].appendChild(style4);
	
	var style5 = document.createElement('style');
	style5.type = 'text/css';
	style5.innerHTML = '#header-btn_Unapproved {background: -webkit-linear-gradient(top, #7cec6f 0%, #34d661 100%) !important;}';
	document.getElementsByTagName('head')[0].appendChild(style5);
	
	var style6 = document.createElement('style');
	style6.type = 'text/css';
	style6.innerHTML = '#header-btn_Unapproved .ui-btn-text {color: white !important;}';
	document.getElementsByTagName('head')[0].appendChild(style6);
	
	var style7 = document.createElement('style');
	style7.type = 'text/css';
	style7.innerHTML = '.ui-icon-main_scr_btn_Favorites {background: url(../image/main_scr_btn_Favorites.png) 100% 100% no-repeat !important;}';
	document.getElementsByTagName('head')[0].appendChild(style7);
	
	var style8 = document.createElement('style');
	style8.type = 'text/css';
	style8.innerHTML = '#header-btn_Favorites {background: -webkit-linear-gradient(top, #1be1f4 0%, #11c3f1 100%) !important;}';
	document.getElementsByTagName('head')[0].appendChild(style8);
	
	var style9 = document.createElement('style');
	style9.type = 'text/css';
	style9.innerHTML = '#header-btn_Favorites .ui-btn-text {color: white !important;}';
	document.getElementsByTagName('head')[0].appendChild(style9);
	
	if (button_id == 1)
	{
		// フォルダ一覧
		var style10 = document.createElement('style');
		style10.type = 'text/css';
		if ((navigator.userAgent.indexOf("iPad")>0)&&((navigator.userAgent.indexOf("OS 8")>0)||(navigator.userAgent.indexOf("OS 7")>0)))
		{
			style10.innerHTML = '.ui-icon-main_scr_btn_folderList {background: url(../image/main_scr_btn_FolderList_i_iphone.png) 100% 100% no-repeat !important;}';
		}
		else if (navigator.userAgent.indexOf("iPad")>0)
		{
			style10.innerHTML = '.ui-icon-main_scr_btn_folderList {background: url(../image/main_scr_btn_FolderList_i.png) 100% 100% no-repeat !important;}';
		}
		else if (navigator.userAgent.indexOf("iPhone")>0)
		{
			style10.innerHTML = '.ui-icon-main_scr_btn_folderList {background: url(../image/main_scr_btn_FolderList_i_iphone.png) 100% 100% no-repeat !important;}';	
		}
		else 
		{
			style10.innerHTML = '.ui-icon-main_scr_btn_folderList {background: url(../image/main_scr_btn_FolderList_i.png) 100% 100% no-repeat !important;}';
		}
		document.getElementsByTagName('head')[0].appendChild(style10);
		
		var style11 = document.createElement('style');
		style11.type = 'text/css';
		style11.innerHTML = '#header-btn_FolderList {background: -webkit-linear-gradient(top, #E8E8E8 0%, white 100%) !important;}';
		document.getElementsByTagName('head')[0].appendChild(style11);
		
		var style12 = document.createElement('style');
		style12.type = 'text/css';
		style12.innerHTML = '#header-btn_FolderList .ui-btn-text {color: #4789fd !important;}';
		document.getElementsByTagName('head')[0].appendChild(style12);
	}
	else if (button_id == 2)
	{
		// 未承認
		var style13 = document.createElement('style');
		style13.type = 'text/css';
		if ((navigator.userAgent.indexOf("iPad")>0)&&((navigator.userAgent.indexOf("OS 8")>0)||(navigator.userAgent.indexOf("OS 7")>0)))
		{
			style13.innerHTML = '.ui-icon-main_scr_btn_Unapproved {background: url(../image/main_scr_btn_Unapproved_i_iphone.png) 100% 100% no-repeat !important;}';	
		}
		else if (navigator.userAgent.indexOf("iPad")>0)
		{
			style13.innerHTML = '.ui-icon-main_scr_btn_Unapproved {background: url(../image/main_scr_btn_Unapproved_i.png) 100% 100% no-repeat !important;}';
		}
		else if (navigator.userAgent.indexOf("iPhone")>0)
		{
			style13.innerHTML = '.ui-icon-main_scr_btn_Unapproved {background: url(../image/main_scr_btn_Unapproved_i_iphone.png) 100% 100% no-repeat !important;}';	
		}
		else 
		{
			style13.innerHTML = '.ui-icon-main_scr_btn_Unapproved {background: url(../image/main_scr_btn_Unapproved_i.png) 100% 100% no-repeat !important;}';
		}
		document.getElementsByTagName('head')[0].appendChild(style13);
		
		var style14 = document.createElement('style');
		style14.type = 'text/css';
		style14.innerHTML = '#header-btn_Unapproved {background: -webkit-linear-gradient(top, #E8E8E8 0%, white 100%) !important;}';
		document.getElementsByTagName('head')[0].appendChild(style14);
		
		var style15 = document.createElement('style');
		style15.type = 'text/css';
		style15.innerHTML = '#header-btn_Unapproved .ui-btn-text {color: #00a550 !important;}';
		document.getElementsByTagName('head')[0].appendChild(style15);		
	}
	else if (button_id == 3)
	{
		// お気に入り
		var style16 = document.createElement('style');
		style16.type = 'text/css';
		if ((navigator.userAgent.indexOf("iPad")>0)&&((navigator.userAgent.indexOf("OS 8")>0)||(navigator.userAgent.indexOf("OS 7")>0)))
		{
			style16.innerHTML = '.ui-icon-main_scr_btn_Favorites {background: url(../image/main_scr_btn_Favorites_i_iphone.png) 100% 100% no-repeat !important;}';	
		}
		else if (navigator.userAgent.indexOf("iPad")>0)
		{
			style16.innerHTML = '.ui-icon-main_scr_btn_Favorites {background: url(../image/main_scr_btn_Favorites_i.png) 100% 100% no-repeat !important;}';
		}
		else if (navigator.userAgent.indexOf("iPhone")>0)
		{
			style16.innerHTML = '.ui-icon-main_scr_btn_Favorites {background: url(../image/main_scr_btn_Favorites_i_iphone.png) 100% 100% no-repeat !important;}';
		}
		else
		{
			style16.innerHTML = '.ui-icon-main_scr_btn_Favorites {background: url(../image/main_scr_btn_Favorites_i.png) 100% 100% no-repeat !important;}';
		}
		
		document.getElementsByTagName('head')[0].appendChild(style16);
		
		var style17 = document.createElement('style');
		style17.type = 'text/css';
		style17.innerHTML = '#header-btn_Favorites {background: -webkit-linear-gradient(top, #E8E8E8 0%, white 100%) !important;}';
		document.getElementsByTagName('head')[0].appendChild(style17);
		
		var style18 = document.createElement('style');
		style18.type = 'text/css';
		style18.innerHTML = '#header-btn_Favorites .ui-btn-text {color: #11c3f1 !important;}';
		document.getElementsByTagName('head')[0].appendChild(style18);
	}
}

/********************************************************************************
 * [機能]	ヘッダーのレベルを設定する
 * [引数]	ビュー名
 * [戻値]	無し
 ********************************************************************************/
function set_header_labels(){
	var language = "";
	var funapproved_count = 0;
	
	var header_userName = window.localStorage.getItem(EIM_KEY_USERNAME);
		
	
	// ヘッダーのラベルを設定
	language = window.localStorage.getItem(EIM_KEY_LANGUAGE);
	language = JSON.parse(language);
	if(language == null){
		language = "JA";
	}
	else
	{
		language = language.substring(0,2);
	}
		
	// コンポーネントに変数をバインド
	funapproved_count = JSON.parse(window.localStorage.getItem(EIM_KEY_APPROVEDOCUMENT_COUNT));
	
	if (funapproved_count != null)
	{
		if (funapproved_count > 0){
			// 変数から未承認件数を取得
			$('.header_unapprovedText').empty().append(cmnLanguage['LAN_'+language+'_00008']+"("+funapproved_count+")");
		}
		else
		{		
			$('.header_unapprovedText').empty().append(cmnLanguage['LAN_'+language+'_00008']);
		}
	}
	else 
	{
		$('.header_unapprovedText').empty().append(cmnLanguage['LAN_'+language+'_00008']);
	}
		
	// メニューコンボバックス
	$('.header_btn_Combo').empty();
	$('.header_btn_Combo').append('<option value="'+cmnLanguage['LAN_'+language+'_00012']+'">' + 
			cmnLanguage['LAN_'+language+'_00012'] + '</option>');
	$('.header_btn_Combo').append('<option value="'+cmnLanguage['LAN_'+language+'_00013']+'">' + 
			cmnLanguage['LAN_'+language+'_00013'] + '</option>');
	$('.header_btn_Combo').append('<option selected value="'+cmnLanguage['LAN_'+language+'_00035']+'">' + 
			cmnLanguage['LAN_'+language+'_00035'] + '</option>');
	
	// ボタンをＥＮＡＢＬＥ
	$('.header_btn_Back').removeClass('ui-disabled');
	
	$('.header_listText').empty().append(cmnLanguage['LAN_'+language+'_00007']);
	$('.header_favoritesText').empty().append(cmnLanguage['LAN_'+language+'_00009']);
	
	header_userName = JSON.parse(header_userName);
	$('.header_user_name').empty().append(header_userName);
	
}

/*******************************************************************
 * [機能]	レスポンシブデザイン・ＯＳによって、アイコンのサイズを変わる
 * [引数]	is_yoko	横フラグ
 * 			０：縦 １：横
 * [戻値]	無し
 *******************************************************************/
function cmn_change_image_sizes (is_yoko, planguage){
	if ((navigator.userAgent.indexOf("iPad")>0)&&((navigator.userAgent.indexOf("OS 8")>0)||(navigator.userAgent.indexOf("OS 7")>0)))
	{
		switch (is_yoko)
		{
			case 0: 
				//cmnGetimg['login_scr_title_image'] = '<img style="width: 400px;" src="../image/login_scr_title_image.png">';
				//cmnGetimg['login_scr_slashBackground'] = '<img width="100%;" src="../image/login_scr_slash_image.png">';
				//cmnGetimg['main_scr_user_icon'] = '<img width="24px" src="../image/main_scr_user_icon.png">';
				//cmnGetimg['main_scr_user_icon_'] = '<img style="width: 60%;" src="../image/main_scr_user_icon.png">';
				//cmnGetimg['main_scr_img_confirm_dialog'] = '<img src="../image/main_scr_img_confirm_dialog.png">';
				//cmnGetimg['list_scr_btn_Properties'] = '<img src="../image/list_scr_btn_Properties.png">';
				//cmnGetimg['list_scr_top_icon'] = '<img style="width: 80%;" src="../image/list_scr_top_icon.png">';
				//cmnGetimg['list_scr_btn_combo'] = '<img style="width: 80%;" src="../image/list_scr_btn_combo.png">';
				//cmnGetimg['popup_error'] = '<img src="../image/popup_error.png">';
				//cmnGetimg['popup_notice'] = '<img src="../image/popup_notice.png">';
				//cmnGetimg['popup_success'] = '<img src="../image/popup_success.png">';
				//cmnGetimg['popup_warning'] = '<img src="../image/popup_warning.png">';
				cmnGetimg['favorites_scr_btn_Favourites'] = '<img style="width: 80%;" src="../image/favorites_scr_btn_Favourite.png">';
				cmnGetimg['list_src_workspace_icon'] = '<img style="width: 80%;" src="../image/list_scr_workspace_icon.png">';
				cmnGetimg['list_scr_folder_icon'] = '<img style="width: 80%;" src="../image/list_scr_folder_icon.png">';
				cmnGetimg['list_scr_document_doc'] = '<img style="width: 80%;" src="../image/list_scr_document_doc.png">';
				cmnGetimg['list_scr_document_excel'] = '<img style="width: 80%;" src="../image/list_scr_document_excel.png">';
				cmnGetimg['list_scr_document_pdf'] = '<img style="width: 80%;" src="../image/list_scr_document_pdf.png">';
				cmnGetimg['list_scr_document_ppt'] = '<img style="width: 80%;" src="../image/list_scr_document_ppt.png">';
				cmnGetimg['list_scr_document'] = '<img style="width: 80%;" src="../image/list_scr_document.png">';
				if (planguage == "JA")
				{
					cmnGetimg['unapproved_scr_btn_Check'] = '<img style="width: 100%;" src="../image/unapproved_scr_btn_Check.png">';
				}
				else if (planguage == "EN") 
				{
					cmnGetimg['unapproved_scr_btn_Check'] = '<img style="width: 75%;" src="../image/unapproved_scr_btn_Check.png">';
				}
				cmnGetimg['check_scr_btn_History_minus'] = '<img style="width: 75%;" src="../image/check_scr_btn_History_minus.png">';
				cmnGetimg['check_scr_btn_History_plus'] = '<img style="width: 75%;" src="../image/check_scr_btn_History_plus.png">';
				break;
			case 1: 
				//cmnGetimg['login_scr_title_image'] = '<img style="width: 400px;" src="../image/login_scr_title_image.png">';
				//cmnGetimg['login_scr_slashBackground'] = '<img width="100%;" src="../image/login_scr_slash_image.png">';
				//cmnGetimg['main_scr_user_icon'] = '<img width="24px" src="../image/main_scr_user_icon.png">';
				//cmnGetimg['main_scr_user_icon_'] = '<img style="width: 60%;" src="../image/main_scr_user_icon.png">';
				//cmnGetimg['main_scr_img_confirm_dialog'] = '<img src="../image/main_scr_img_confirm_dialog.png">';
				//cmnGetimg['list_scr_btn_Properties'] = '<img src="../image/list_scr_btn_Properties.png">';
				//cmnGetimg['list_scr_top_icon'] = '<img style="width: 80%;" src="../image/list_scr_top_icon.png">';
				//cmnGetimg['list_scr_btn_combo'] = '<img style="width: 80%;" src="../image/list_scr_btn_combo.png">';
				//cmnGetimg['popup_error'] = '<img src="../image/popup_error.png">';
				//cmnGetimg['popup_notice'] = '<img src="../image/popup_notice.png">';
				//cmnGetimg['popup_success'] = '<img src="../image/popup_success.png">';
				//cmnGetimg['popup_warning'] = '<img src="../image/popup_warning.png">';
				cmnGetimg['favorites_scr_btn_Favourites'] = '<img style="width: 60%;" src="../image/favorites_scr_btn_Favourite.png">';
				cmnGetimg['list_src_workspace_icon'] = '<img style="width: 70%;" src="../image/list_scr_workspace_icon.png">';
				cmnGetimg['list_scr_folder_icon'] = '<img style="width: 70%;" src="../image/list_scr_folder_icon.png">';
				cmnGetimg['list_scr_document_doc'] = '<img style="width: 70%;" src="../image/list_scr_document_doc.png">';
				cmnGetimg['list_scr_document_excel'] = '<img style="width: 70%;" src="../image/list_scr_document_excel.png">';
				cmnGetimg['list_scr_document_pdf'] = '<img style="width: 70%;" src="../image/list_scr_document_pdf.png">';
				cmnGetimg['list_scr_document_ppt'] = '<img style="width: 70%;" src="../image/list_scr_document_ppt.png">';
				cmnGetimg['list_scr_document'] = '<img style="width: 70%;" src="../image/list_scr_document.png">';
				cmnGetimg['unapproved_scr_btn_Check'] = '<img style="width: 50%;" src="../image/unapproved_scr_btn_Check.png">';
				cmnGetimg['check_scr_btn_History_minus'] = '<img style="width: 55%;" src="../image/check_scr_btn_History_minus.png">';
				cmnGetimg['check_scr_btn_History_plus'] = '<img style="width: 55%;" src="../image/check_scr_btn_History_plus.png">';
			break;
			default: break;
		}
	}
}

/*******************************************************************
 * [機能]	StringのByteCount
 * [引数]	str	String
 * 			idx index
 * [戻値]	code charCode （ＵＴＦの種類）
 *******************************************************************/
function fixedCharCodeAt(str, idx) {
    idx = idx || 0;
    var code = str.charCodeAt(idx);
    var hi, low;
    if (0xD800 <= code && code <= 0xDBFF) 
    {   // High surrogate (could change last hex to 0xDB7F to treat high private 
    	// surrogates as single characters)
        hi = code;
        low = str.charCodeAt(idx + 1);
        if (isNaN(low)) {
            throw '';
        }
        return ((hi - 0xD800) * 0x400) + (low - 0xDC00) + 0x10000;
    }
    if (0xDC00 <= code && code <= 0xDFFF) { // Low surrogate
        // We return false to allow loops to skip this iteration since should 
    	// have already handled high surrogate above in the previous iteration
        return false;
        /*hi = str.charCodeAt(idx-1);
        low = code;
        return ((hi - 0xD800) * 0x400) + (low - 0xDC00) + 0x10000;*/
    }
    return code;
}

/*******************************************************************
 * [機能]	StringのByteCount
 * [引数]	str	String
 * [戻値]	result 	ByteCount
 *******************************************************************/
function countUtf8(str) {
    var result = 0;
    for (var n = 0; n < str.length; n++) {
        var charCode = fixedCharCodeAt(str, n);
        if (typeof charCode === "number") {
            if (charCode < 128) {
                result = result + 1;
            } else if (charCode < 2048) {
                result = result + 2;
            } else if (charCode < 65536) {
                result = result + 3;
            } else if (charCode < 2097152) {
                result = result + 4;
            } else if (charCode < 67108864) {
                result = result + 5;
            } else {
                result = result + 6;
            }
        }
    }
    return result;
}

/*******************************************************************
 * [機能]	長いStringを切りまして、「...」を追加する
 * [引数]	sts				String
 *			view			ビュー
 *			type_of_screen	縦横の情報
 * [戻値]	result 	ByteCount
 *******************************************************************/
function resizeString(str, view, type_of_screen) {
	var fmax = cmnAddPoints[view+'_'+type_of_screen];
	var fstr = "";
	
	for (var n = 0; n < str.length; n++) 
    {
		if (fmax > 0)
		{
			fstr = fstr + str[n];
			fmax = fmax - countUtf8(str[n]);
		}
		else 
		{
			fstr = fstr + "...";
			break;
		}
    }
	return fstr;
}


/*******************************************************************
 * [機能]	タイムスタンプ（１３桁）　→　ＤＡＴＥ
 * [引数]	ts				String
 * [戻値]	fd	 			String
 *******************************************************************/
function timestamp_to_Date(ts) {
	
	if (ts.length < 13)
	{
		return "";
	}
			
	var fdate = new Date(ts/1);
	var fmonth = ''+(fdate.getMonth()+1);
	var fday = ''+fdate.getDate();
	
	var fd = '';
		
	if (fmonth.length == 1)
	{
		fmonth = '0'+fmonth;
	}
	if (fday.length == 1)
	{
		fday = '0'+fday;
	}
	
	fd = fdate.getFullYear() + '-' + fmonth + '-' + fday;
	
	return fd;
}

/*******************************************************************
 * [機能]	YYYY-MM-DD　→　MM-DD-YYYY
 * [引数]	ts				String
 * [戻値]	fd	 			String
 *******************************************************************/
function english_Date(ts, planguage) {
	if (planguage=="EN"){
		return ts.substring(5,7)+"-"+ts.substring(8)+"-"+ts.substring(0,4);
	}
	else 
	{
		return ts;
	}
}

