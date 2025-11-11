<%@ page contentType = "text/xml; charset=UTF-8" %>

<%
	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");
	System.out.println("getBaseEventTypeList.jsp");
%>

<result>
		<baseEventType
				id="-201"
				name="ベースイベントタイプ１"
		/>
		<baseEventType
				id="-202"
				name="ベースイベントタイプ２"
		/>
		<baseEventType
				id="-203"
				name="ベースイベントタイプ３"
		/>
</result>
