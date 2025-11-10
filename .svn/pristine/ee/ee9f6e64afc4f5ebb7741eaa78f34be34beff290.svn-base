<%@ page contentType = "text/html; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "common.util.*" %>

<%@ page import = "jp.co.ctc_g.eim.admin.business.service.*" %>
<%@ page import = "jp.co.ctc_g.eim.admin.business.domain.*" %>
<%@ page import = "org.springframework.context.ApplicationContext" %>
<%@ page import = "jp.co.ctc_g.eim.framework.common.util.ApplicationContextLoader" %>

<%
	//------------------------------------------------
	//選択言語により遷移先SWFファイルの切替を実施
	//------------------------------------------------
	EIMSession sess = EIMUtils.getSession(request);
	String langId = null;
	String locale = null;
	String resourcePath = null;
	String topViewType = null;
	String adminAppId = null;
	String adminTitle = null;
	String namespace = null;
	String style = null;

	String contextPath = request.getContextPath();

	//Message
	String message = null;
	try {
		langId = (String)sess.getAttribute(EIMSession.LANG);
		locale = (String)sess.getAttribute("locale");
		resourcePath = request.getContextPath() + "/resource_eim_document" + EIMConfig.get("SWF_PREFIX") + ".swf";
		style = "style" + EIMConfig.get("SWF_PREFIX") + ".swf";

		topViewType = EIMConfig.get("SWF_MAIN") + EIMConfig.get("SWF_PREFIX");

		adminAppId = (String)session.getAttribute("ADMIN_APP_ID");
		namespace = (String)session.getAttribute("ADMIN_NAMESPACE");

		// セッションは有効だが、セッション変数が未設定の場合 (ドキュメント管理、帳票管理にログイン済みの場合など)
		if(adminAppId == null){
			String prmAppId = request.getParameter("appId");
			
			NamespaceDomain namespaceDomain = null;
			
			ApplicationContext context = ApplicationContextLoader.getContext();
			NamespaceService namespaceService = (NamespaceService)context.getBean("NamespaceService");
			
			if(prmAppId != null && prmAppId.equals("general"))
			{
				adminAppId = AppConstant.ADMIN_APP_ID_GENERAL;
				namespace = "";
			}
			else if(prmAppId != null && prmAppId.equals("form"))
			{
				adminAppId = AppConstant.ADMIN_APP_ID_FORM;
				namespace = EIMConfig.get("APP_FORM_NAMESPACE");
				namespaceDomain = namespaceService.getByNamespaceName(EIMConfig.get("APP_FORM_NAMESPACE"));
			}
			else
			{
				adminAppId = AppConstant.ADMIN_APP_ID_DOCUMENT;
				namespace = EIMConfig.get("APP_DOC_NAMESPACE");
				namespaceDomain = namespaceService.getByNamespaceName(EIMConfig.get("APP_DOC_NAMESPACE"));
			}
			
			if (namespaceDomain == null)
				namespaceDomain = new NamespaceDomain();
			
			// セッション変数を設定する
			session.setAttribute("ADMIN_APP_ID" , adminAppId);
			session.setAttribute("ADMIN_NAMESPACE", namespace);
			sess.setAttribute("namespace", namespaceDomain);
		}
		
		// ページタイトルを設定する
		if (AppConstant.ADMIN_APP_ID_GENERAL.equals(adminAppId))
		{
			adminTitle = EIMResource.getMessage(sess, "EIM.TITLE.SYSTEMMANAGEMENT.GENERAL");
		}
		else if (AppConstant.ADMIN_APP_ID_DOCUMENT.equals(adminAppId))
		{
			adminTitle = EIMResource.getMessage(sess, "EIM.TITLE.SYSTEMMANAGEMENT.DOCUMENT");
		}
		else if (AppConstant.ADMIN_APP_ID_FORM.equals(adminAppId))
		{
			adminTitle = EIMResource.getMessage(sess, "EIM.TITLE.SYSTEMMANAGEMENT.FORM");
		}
	} finally {
		if (sess != null) {
			sess.close();
		}
	}
	
	//------------------------------------------------
	//動的HTMLの変数をサニタイジングする
	//------------------------------------------------
	String s_lang = StringUtils.xmlEncode(langId);
	String s_RsPath = StringUtils.xmlEncode(resourcePath);
	String s_locale = StringUtils.xmlEncode(locale);
	String s_style = StringUtils.xmlEncode(style);
%>

<!-- saved from url=(0014)about:internet -->
<html lang="en">
<head>
	<title>EIMANAGER - <%=adminTitle%></title>
	<meta http-equiv="Content-Type" content="text/html;charset=UTF-8">
	<meta http-equiv="Pragma" content="no-cache">
	<meta http-equiv="cache-control" content="no-cache">
	<meta http-equiv="Expires" content="Thu,01 Dec 1994 16:00:00 GMT">
	<style type="text/css">
		<!--
		body
		{
			margin-top:			0px;
			margin-bottom:		0px;
			margin-left:		0px;
			margin-right:		0px;
		}
		-->
	</style>
<script src="AC_OETags.js" language="javascript"></script>
<style>
body { margin: 0px; overflow:hidden }
</style>
<script language="JavaScript" type="text/javascript">
<!--
// -----------------------------------------------------------------------------
// Globals
// Major version of Flash required
var requiredMajorVersion = 9;
// Minor version of Flash required
var requiredMinorVersion = 0;
// Minor version of Flash required
var requiredRevision = 0;
// -----------------------------------------------------------------------------
// -->
</script>
</head>

<body scroll="no">
<script language="JavaScript" type="text/javascript" src="history.js"></script>
<script language="JavaScript" type="text/javascript">
<!--
// Version check for the Flash Player that has the ability to start Player Product Install (6.0r65)
var hasProductInstall = DetectFlashVer(6, 0, 65);

// Version check based upon the values defined in globals
var hasRequestedVersion = DetectFlashVer(requiredMajorVersion, requiredMinorVersion, requiredRevision);


// Check to see if a player with Flash Product Install is available and the version does not meet the requirements for playback
if ( hasProductInstall && !hasRequestedVersion ) {
	// MMdoctitle is the stored document.title value used by the installation process to close the window that started the process
	// This is necessary in order to close browser windows that are still utilizing the older version of the player after installation has completed
	// DO NOT MODIFY THE FOLLOWING FOUR LINES
	// Location visited after installation is complete if installation is required
	var MMPlayerType = (isIE == true) ? "ActiveX" : "PlugIn";
	var MMredirectURL = encodeURI(window.location);
    document.title = document.title.slice(0, 47) + " - Flash Player Installation";
    var MMdoctitle = document.title;

	AC_FL_RunContent(
		"src", "playerProductInstall",
		"FlashVars", "MMredirectURL="+MMredirectURL+'&MMplayerType='+MMPlayerType+'&MMdoctitle='+MMdoctitle+"",
		"width", "100%",
		"height", "100%",
		"align", "middle",
		"id", "<%=topViewType%>",
		"quality", "high",
		"bgcolor", "#ccccff",
		"name", "<%=topViewType%>",
		"allowScriptAccess","sameDomain",
		"type", "application/x-shockwave-flash",
		"pluginspage", "http://www.adobe.com/go/getflashplayer"
	);
} else if (hasRequestedVersion) {
	// if we've detected an acceptable version
	// embed the Flash Content SWF when all tests are passed
	AC_FL_RunContent(
			"src", "<%=topViewType%>",
			"width", "100%",
			"height", "100%",
			"align", "middle",
			"id", "<%=topViewType%>",
			"quality", "high",
			"bgcolor", "#ccccff",
			"name", "<%=topViewType%>",
			"flashVars",'prmAdminAppId=<%=adminAppId%>&prmLangId=<%=s_lang%>&prmNamespace=<%=namespace%>&resourceModuleURLs=<%=s_RsPath%>&localeChain=<%=s_locale%>&prmContextPath=<%=contextPath%>&prmOPTION_ARRAY=<%=EIMConfig.getValue("OPTION_ARRAY")%>&langId=<%=s_lang%>&contextRoot=<%=contextPath%>&prmStyle=<%=s_style%>',
			"allowScriptAccess","sameDomain",
			"type", "application/x-shockwave-flash",
			"pluginspage", "http://www.adobe.com/go/getflashplayer"
	);
  } else {  // flash is too old or we can't detect the plugin
    var alternateContent = 'Alternate HTML content should be placed here. '
  	+ 'This content requires the Adobe Flash Player. '
   	+ '<a href=http://www.adobe.com/go/getflash/>Get Flash</a>';
    document.write(alternateContent);  // insert non-flash content
  }
// -->
</script>
<noscript>
  	<object classid="clsid:D27CDB6E-AE6D-11cf-96B8-444553540000"
			id="<%=topViewType%>" width="100%" height="100%"
			codebase="http://fpdownload.macromedia.com/get/flashplayer/current/swflash.cab">
			<param name="movie" value="<%=topViewType%>.swf" />
			<param name="quality" value="high" />
			<param name="bgcolor" value="#ccccff" />
			<param name="allowScriptAccess" value="sameDomain" />
			<embed src="<%=topViewType%>.swf" quality="high" bgcolor="#ccccff"
				width="100%" height="100%" name="<%=topViewType%>" align="middle"
				play="true"
				loop="false"
				quality="high"
				flashVars="prmAdminAppId=<%=adminAppId%>&prmLangId=<%=s_lang%>&prmNamespace=<%=namespace%>&resourceModuleURLs=<%=s_RsPath%>&localeChain=<%=s_locale%>&prmContextPath=<%=contextPath%>&prmOPTION_ARRAY=<%=EIMConfig.getValue("OPTION_ARRAY")%>&prmStyle=<%=s_style%>"
				allowScriptAccess="sameDomain"
				type="application/x-shockwave-flash"
				pluginspage="http://www.adobe.com/go/getflashplayer">
			</embed>
	</object>
</noscript>

<!-- ファイルダウンロード用フレーム -->
<iframe id="checkoutFrame" name="checkoutFrame" height="0" width="1" frameborder="0" vpsace="0" hspace="0" marginheight="0" marginwidth="0" scrolling="no" src="blank.html">
	Internet Explorer ERROR Inline Frame.
</iframe>
</body>
</html>
