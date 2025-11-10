<%@ page contentType = "text/html; charset=UTF-8" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "common.util.*" %>
<%
	if(session.getAttribute("USER") == null)
	{
		String prmAppId = request.getParameter("appId");
		
		if(prmAppId != null && prmAppId.equals("general"))
		{
			session.setAttribute("ADMIN_APP_ID" , AppConstant.ADMIN_APP_ID_GENERAL);
			session.setAttribute("ADMIN_NAMESPACE", "");
		}
		else if(prmAppId != null && prmAppId.equals("form"))
		{
			session.setAttribute("ADMIN_APP_ID" , AppConstant.ADMIN_APP_ID_FORM);
			session.setAttribute("ADMIN_NAMESPACE", EIMConfig.get("APP_FORM_NAMESPACE"));
		}
		else
		{
			session.setAttribute("ADMIN_APP_ID" , AppConstant.ADMIN_APP_ID_DOCUMENT);
			session.setAttribute("ADMIN_NAMESPACE", EIMConfig.get("APP_DOC_NAMESPACE"));
		}
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
