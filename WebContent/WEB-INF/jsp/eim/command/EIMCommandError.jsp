<%@ page language="java" contentType="text/xml; charset=UTF-8" pageEncoding="UTF-8"
%><%@ include file="/WEB-INF/jsp/includes/jsp_header.jsp"

%><c:set var="resultData" value="${resultData}" 

/><?xml version="1.0" encoding="UTF-8"?>
<result>
	<type>
		<c:out value="${resultData.type}" escapeXml="true"/>
	</type>
	<code>
		<c:out value="${resultData.code}" escapeXml="true"/>
	</code>
	<message>
		<c:out value="${resultData.message}" escapeXml="true"/>
	</message>
</result>