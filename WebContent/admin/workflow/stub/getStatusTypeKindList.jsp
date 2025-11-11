<%@ page contentType = "text/xml; charset=UTF-8" %>

<%
	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");
	System.out.println("getStatusTypeKindList.jsp");
%>

<result>
		<statusTypeKind
				id="-301"
				name="ステータスタイプ種別１"
		/>
		<statusTypeKind
				id="-302"
				name="ステータスタイプ種別２"
		/>
		<statusTypeKind
				id="-303"
				name="ステータスタイプ種別３"
		/>
</result>
