<%@ page contentType = "text/html; charset=UTF-8" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "common.util.*" %>
<%

//システム管理(汎用)限定としてwarを作成する時は、このJSP(indexGeneral.jsp)を
//index.jspに上書きする。それによりシステム管理(ドキュメント、帳票)にアクセス出来ないようにする。

	if(session.getAttribute("USER") == null)
	{
//		String prmAppId = request.getParameter("appId");
//
//		if(prmAppId != null && prmAppId.equals("general"))
//		{
			session.setAttribute("ADMIN_APP_ID" , AppConstant.ADMIN_APP_ID_GENERAL);
			session.setAttribute("ADMIN_NAMESPACE", "");
//		}
//		else if(prmAppId != null && prmAppId.equals("form"))
//		{
//			session.setAttribute("ADMIN_APP_ID" , AppConstant.ADMIN_APP_ID_FORM);
//			session.setAttribute("ADMIN_NAMESPACE", EIMConfig.get("APP_FORM_NAMESPACE"));
//		}
//		else
//		{
//			session.setAttribute("ADMIN_APP_ID" , AppConstant.ADMIN_APP_ID_DOCUMENT);
//			session.setAttribute("ADMIN_NAMESPACE", EIMConfig.get("APP_DOC_NAMESPACE"));
//		}
%>
		<jsp:forward page="login.jsp" />	
<%
	}
	else
	{
%>
		<jsp:forward page="topView.jsp" />	
<%
	}
%>
