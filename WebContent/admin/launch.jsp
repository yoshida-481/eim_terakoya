<%@ page contentType = "text/html; charset=UTF-8" %>
<%@ page import = "eim.util.*" %>
<%
	String prmObjId = request.getParameter("objId");
	if(prmObjId == null)
	{
		prmObjId = "";
	}

	String prmAppId = request.getParameter("appId");
	if(prmAppId == null)
	{
		prmAppId = "";
	}

	String prmNamespace = request.getParameter("namespace");
	if(prmNamespace == null)
	{
		prmNamespace = "";
	}
	
	//------------------------------------------------
	//動的HTMLの変数をサニタイジングする
	//------------------------------------------------
	String s_obj = StringUtils.xmlEncode(prmObjId);
	String s_appId = StringUtils.xmlEncode(prmAppId);
%>

<html>
<head>
	<title>Now Launching System ........</title>
	<meta http-equiv="Content-Type" content="text/html;charset=UTF-8">
	<meta http-equiv="Pragma" content="no-cache">
	<meta http-equiv="cache-control" content="no-cache">
	<meta http-equiv="Expires" content="Thu,01 Dec 1994 16:00:00 GMT">
	<script language="JavaScript">
	<!--

		function launch()
		{
			window.open("index.jsp?objId=<%=s_obj%>&appId=<%=s_appId%>", '_self');
		}

	//-->
	</script>
</head>
<body onload="launch()">

<div id ="msg1"></div>

<noscript>
	Because JavaScript is off, it doesn't advance it any further. <br>
	Please access it again after switching JavaScript to on.
<!--
	JavaScriptがoffなため、これ以上は進めません。JavaScriptをonに切り替えた後、もう一度アクセスください。
-->
</noscript>

</body>
</html>