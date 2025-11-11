<%@ page contentType = "text/html; charset=UTF-8" %>
<%@ page import = "java.util.*" %>
<%@ page import = "eim.util.*" %>

<%
	String prmObjId = request.getParameter("objId");

	if (prmObjId == null)
	{
		prmObjId = "";
	}

	String prmLinkParentObjId = request.getParameter("linkParentObjId");
	
	if (prmLinkParentObjId == null)
	{
		prmLinkParentObjId = "";
	}
	String prmPrivateFileDownloadObjId = request.getParameter("privateFileDownloadObjId");
	
	if(prmPrivateFileDownloadObjId == null)
	{
		prmPrivateFileDownloadObjId = "";
	}
	
	String prmPublicFileDownloadObjId = request.getParameter("publicFileDownloadObjId");
	if(prmPublicFileDownloadObjId == null)
	{
		prmPublicFileDownloadObjId = "";
	}
	
	String loginView;
	String resourcePath = request.getContextPath() + "/resource_eim_document" + EIMConfig.get("SWF_PREFIX") + ".swf";
	Date nowDate = new Date();
	long nowTime = nowDate.getTime();
	if(nowTime % 2 == 0)
	{
		loginView = "index1";
	}
	else
	{
		loginView = "index2";
	}
	
	//------------------------------------------------
	//動的HTMLの変数をサニタイジングする
	//------------------------------------------------
	String s_obj = StringUtils.xmlEncode(prmObjId);
	String s_lobj = StringUtils.xmlEncode(prmLinkParentObjId);
	String s_privateobj = StringUtils.xmlEncode(prmPrivateFileDownloadObjId);
	String s_publicobj = StringUtils.xmlEncode(prmPublicFileDownloadObjId);
%>

<%@page import="eim.util.EIMConfig"%>
<html lang="en">

<head>
	<title>EIMANAGER - Document Management</title>
	<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
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
		"id", "index1",
		"quality", "high",
		"bgcolor", "#ffffff",
		"name", "index1",
		"flashVars", "prmObjId=<%=s_obj%>&prmLinkParentObjId=<%=s_lobj%>&prmPrivateFileDownloadObjId=<%=s_privateobj%>&prmPublicFileDownloadObjId=<%=s_publicobj%>&resourceModuleURLs=<%=resourcePath%>&localeChain=ja_JP",
		"allowScriptAccess","sameDomain",
		"type", "application/x-shockwave-flash",
		"pluginspage", "http://www.adobe.com/go/getflashplayer"
	);
} else if (hasRequestedVersion) {
	// if we've detected an acceptable version
	// embed the Flash Content SWF when all tests are passed
	AC_FL_RunContent(
			"src", "<%=loginView%>",
			"width", "100%",
			"height", "100%",
			"align", "middle",
			"id", "<%=loginView%>",
			"quality", "high",
			"bgcolor", "#ffffff",
			"name", "<%=loginView%>",
			"flashVars",'prmObjId=<%=s_obj%>&prmLinkParentObjId=<%=s_lobj%>&prmPrivateFileDownloadObjId=<%=s_privateobj%>&prmPublicFileDownloadObjId=<%=s_publicobj%>&resourceModuleURLs=<%=resourcePath%>&localeChain=ja_JP',
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
			id="<%=loginView%>" width="100%" height="100%"
			codebase="http://fpdownload.macromedia.com/get/flashplayer/current/swflash.cab">
			<param name="movie" value="<%=loginView%>.swf" />
			<param name="quality" value="high" />
			<param name="bgcolor" value="#ffffff" />
			<param name="allowScriptAccess" value="sameDomain" />
			<embed src="<%=loginView%>.swf" quality="high" bgcolor="#ffffff"
				width="100%" height="100%" name="<%=loginView%>" align="middle"
				play="true"
				loop="false"
				quality="high"
				flashVars="prmObjId=<%=s_obj%>&prmLinkParentObjId=<%=s_lobj%>&resourceModuleURLs=<%=resourcePath%>&localeChain=ja_JP"
				allowScriptAccess="sameDomain"
				type="application/x-shockwave-flash"
				pluginspage="http://www.adobe.com/go/getflashplayer">
			</embed>
	</object>
</noscript>
</body>
</html>
