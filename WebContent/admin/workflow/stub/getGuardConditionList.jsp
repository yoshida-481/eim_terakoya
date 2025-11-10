<%@ page contentType = "text/xml; charset=UTF-8" %>

<%
	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");
	System.out.println("getGuardConditionListList.jsp");
%>

<result>
		<guardCondition
				id="-101"
				name="ガード条件１"
		/>
		<guardCondition
				id="-102"
				name="ガード条件２"
		/>
		<guardCondition
				id="-103"
				name="ガード条件３"
		/>
</result>
