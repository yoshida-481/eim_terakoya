// jQuery
document.write("<script type='text/javascript' src='../lib/jquery/jquery-1.9.1.min.js'></script>");

// jQuery Mobile設定変更
document.write("<script type='text/javascript' src='../js/eim_setting_jquery-mobile.js'></script>");

// jQuery Mobile
document.write("<link rel='stylesheet' href='../lib/jquery.mobile/custom-theme.min.css' />");
document.write("<link rel='stylesheet' href='../lib/jquery.mobile/jquery.mobile-1.3.1.min.css' />");
document.write("<link rel='stylesheet' href='../lib/jquery.mobile/jquery.mobile.structure-1.3.1.min.css' />");
document.write("<link rel='stylesheet' href='../lib/jquery.mobile/jquery.mobile.theme-1.3.1.min.css' />");
document.write("<script type='text/javascript' src='../lib/jquery.mobile/jquery.mobile-1.3.1.min.js'></script>");

// アプリケーション
document.write("<script type='text/javascript' src='../resources/eim_key_language.js'></script>");
document.write("<script type='text/javascript' src='../resources/eim_resources.js'></script>");
document.write("<script type='text/javascript' src='../js/eim_application.js'></script>");

if ((navigator.userAgent.indexOf("iPad")>0)&&((navigator.userAgent.indexOf("OS 8")>0)||(navigator.userAgent.indexOf("OS 7")>0)))
{
	document.write("<link rel='stylesheet' href='../css/eim_application_OS_7_8.css' />");
	document.write('<meta id="viewport_tag" name="viewport" content="width=device-width, initial-scale=1, minimum-scale=1, maximum-scale=1">');
}
else if (navigator.userAgent.indexOf("iPad")>0) 
{
	document.write("<link rel='stylesheet' href='../css/eim_application.css' />");
	document.write('<meta name="viewport" content="width=device-width, initial-scale=1, minimum-scale=1, maximum-scale=1">');
}
else if (navigator.userAgent.indexOf("iPhone")>0)
{
	document.write("<link rel='stylesheet' href='../css/eim_application_iphone.css' />");
	document.write('<meta name="viewport" content="user-scalable=yes">');
}
else 
{
	document.write("<link rel='stylesheet' href='../css/eim_application.css' />");
	document.write('<meta name="viewport" content="width=device-width, initial-scale=1, minimum-scale=1, maximum-scale=1">');
}

// プロトタイプ
document.write("<script type='text/javascript' src='../js/model/prototypeModel.js'></script>");
document.write("<script type='text/javascript' src='../js/controller/prototypeController.js'></script>");

// ログインプレゼンテーションモデル
document.write("<script type='text/javascript' src='../js/model/login_scr_Model.js'></script>");
// ログインビューコントローラー
document.write("<script type='text/javascript' src='../js/controller/login_scr_Controller.js'></script>");

// メニュープレゼンテーションモデル
document.write("<script type='text/javascript' src='../js/model/main_scr_Model.js'></script>");
// メニュービューコントローラー
document.write("<script type='text/javascript' src='../js/controller/main_scr_Controller.js'></script>");

// ファルダ一覧プレゼンテーションモデル
document.write("<script type='text/javascript' src='../js/model/folderList_scr_Model.js'></script>");
// ファルダ一覧ビューコントローラー
document.write("<script type='text/javascript' src='../js/controller/folderList_scr_Controller.js'></script>");

// お気に入りプレゼンテーションモデル
document.write("<script type='text/javascript' src='../js/model/favorites_scr_Model.js'></script>");
// お気に入りビューコントローラー
document.write("<script type='text/javascript' src='../js/controller/favorites_scr_Controller.js'></script>");

//未承認一覧プレゼンテーションモデル
document.write("<script type='text/javascript' src='../js/model/unapproved_scr_Model.js'></script>");
// 未承認一覧ビューコントローラー
document.write("<script type='text/javascript' src='../js/controller/unapproved_scr_Controller.js'></script>");

//属性情報プレゼンテーションモデル
document.write("<script type='text/javascript' src='../js/model/properties_scr_Model.js'></script>");
// 属性情報ビューコントローラー
document.write("<script type='text/javascript' src='../js/controller/properties_scr_Controller.js'></script>");

// 承認プレゼンテーションモデル
document.write("<script type='text/javascript' src='../js/model/check_scr_Model.js'></script>");
// 承認ビューコントローラー
document.write("<script type='text/javascript' src='../js/controller/check_scr_Controller.js'></script>");
