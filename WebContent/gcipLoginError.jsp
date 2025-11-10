<%@ page language="java" contentType="text/html; charset=UTF-8" %>
<%
	response.setHeader("Pragma","no-cache");
	response.setHeader("Cache-Control","no-cache");
	response.setDateHeader("Expires",0);
%>

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=windows-31j">
<title>Error</title>
</head>
<body>
<%
	String prmObjId = request.getParameter("objId");
	
	if (prmObjId.equals("cookienull")){

%>
<h3>G.CIP認証が使用できないかたはこちらからどうぞ</h3>
<p style="text-indent:1em;"><a href="./app/document/login.jsp">Sphinxログインページ</a></p>
<%
	}else if(prmObjId.equals("no_promo_user")){
%>
<h2>このページへのアクセス権がありません。</h2>
<p>申し訳ありません。参照しようとしているページへのアクセス権がありません。</p>
<p>アプリケーション管理者に連絡いただくよう、お願い致します。</p>
<%
	}
%>
</body>
</html>