<%@ page contentType="text/xml; charset=utf-8" %>
<%@ page import = "java.net.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "common.util.*" %>

<%
	//ContentType
	response.setContentType("text/xml; charset=utf-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");
	
	out.println("<result><option>" + EIMConfig.getValue("OPTION_ARRAY") + "</option></result>");
%>