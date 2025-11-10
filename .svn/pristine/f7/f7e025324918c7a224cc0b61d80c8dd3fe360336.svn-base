<%@ page contentType = "text/html; charset=UTF-8" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "eim.db.DBUtils" %>
<%@ page import = "java.sql.Connection" %>
<%@ page import = "java.sql.PreparedStatement" %>
<%@ page import = "java.sql.ResultSet" %>
<%@ page import = "java.util.Arrays" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.integration.dao.plugin.DatabasePlugInLoader" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.integration.dao.plugin.DatabasePlugIn.RecursiveTableEnum" %>
<%
	String prmObjId = request.getParameter("objId");
	if(prmObjId == null)
	{
		prmObjId = "";
	}
	String prmLinkParentObjId = request.getParameter("linkParentObjId");
	if(prmLinkParentObjId == null)
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
	String prmIsFolder = request.getParameter("isFolder");
	if(prmIsFolder == null)
	{
		prmIsFolder = "";
	}

	//------------------------------------------------
	//動的HTMLの変数をサニタイジングする
	//------------------------------------------------
	String s_obj = StringUtils.xmlEncode(prmObjId);
	String s_lobj = StringUtils.xmlEncode(prmLinkParentObjId);
	String s_privateobj = StringUtils.xmlEncode(prmPrivateFileDownloadObjId);
	String s_publicobj = StringUtils.xmlEncode(prmPublicFileDownloadObjId);
	
	String s_isFolder = StringUtils.xmlEncode(prmIsFolder);
	if (!s_isFolder.equalsIgnoreCase("true") && !StringUtils.isBlank(s_obj)) {
		long objId = 0;
		try {
			objId = Long.parseLong(s_obj, 10);
		} catch (Exception e) {}
		if (objId > 0) {
			String startCondition = String.format("id in (select type from eimobj where id=%s)", objId);
			String whereCondition = String.format("parent is null and name=?");
			try (Connection conn = DBUtils.getDBConnection();
				PreparedStatement pstmt = conn.prepareStatement(
						DatabasePlugInLoader.getPlugIn()
							.getQueryStringWithRecursive(RecursiveTableEnum.EIMOBJTYPE_PARENT, new String[]{"id", "name", "parent"}, startCondition, false) +
						DatabasePlugInLoader.getPlugIn()
							.getQueryStringSelectRecursive(RecursiveTableEnum.EIMOBJTYPE_PARENT, new String[]{"count(*)"}, startCondition, null, whereCondition, false)
					)
			) {
				pstmt.setString(1, "ワークスペース");
				try (ResultSet rs = pstmt.executeQuery()) {
					rs.next();
					if (rs.getLong(1) == 1) {
						s_isFolder = "true";
					}
				}
			}
		}
	}
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
			window.open("index.jsp?objId=<%=s_obj%>&linkParentObjId=<%=s_lobj%>&isFolder=<%=s_isFolder%>&", '_self');
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