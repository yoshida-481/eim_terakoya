<%@ page contentType = "text/xml; charset=UTF-8" %>

<%
	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");
	System.out.println("getMailTypeList.jsp");
%>

<result>
		<mailTypeList
				id="-901"
				name="メール種別１"
		/>
		<mailTypeList
				id="-902"
				name="メール種別２"
		/>
		<mailTypeList
				id="-903"
				name="メール種別３"
		/>
</result>
