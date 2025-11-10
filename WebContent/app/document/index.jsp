<%@ page contentType = "text/html; charset=UTF-8" %>
<%
	String prmObjId = request.getParameter("objId");
	String prmLinkParentObjId = request.getParameter("linkParentObjId");
	String prmPrivateFileDownloadObjId = request.getParameter("privateFileDownloadObjId");
	String prmPublicFileDownloadObjId = request.getParameter("publicFileDownloadObjId");

	if(session.getAttribute("USER") == null)
	{
		if (prmObjId == null || prmObjId == "")
		{
%>
			<jsp:forward page="login.jsp" />	
<%
		}else{
			
			if(prmPrivateFileDownloadObjId != null && prmPrivateFileDownloadObjId != ""){
				// 直接ダウンロード(原本)の指定がある場合
				%>
				<jsp:forward page="login.jsp">
					<jsp:param name="objId" value='<%=prmObjId%>'/>
					<jsp:param name="privateFileDownloadObjId" value='<%=prmPrivateFileDownloadObjId%>'/>
				</jsp:forward>
				<%
			}else if(prmPublicFileDownloadObjId != null && prmPublicFileDownloadObjId != ""){
				// 直接ダウンロード(公開)の指定がある場合
				%>
				<jsp:forward page="login.jsp">
					<jsp:param name="objId" value='<%=prmObjId%>'/>
					<jsp:param name="publicFileDownloadObjId" value='<%=prmPublicFileDownloadObjId%>'/>
				</jsp:forward>
				<%
			}else{
				%>
				<jsp:forward page="login.jsp">
					<jsp:param name="objId" value='<%=prmObjId%>'/>
					<jsp:param name="linkParentObjId" value='<%=prmLinkParentObjId%>'/>
				</jsp:forward>
				<%
			}
		}
	}
	else
	{
		// ログイン済みの場合
		if (prmObjId == null || prmObjId == "")
		{
%>
			<jsp:forward page="topView.jsp" />	
<%
		}
		else
		{
%>
		<jsp:forward page="topView.jsp">	
			<jsp:param name="objId" value='<%=prmObjId%>'/>
			<jsp:param name="linkParentObjId" value='<%=prmLinkParentObjId%>'/>
			<jsp:param name="privateFileDownloadObjId" value='<%=prmPrivateFileDownloadObjId%>'/>
			<jsp:param name="publicFileDownloadObjId" value='<%=prmPublicFileDownloadObjId%>'/>
		</jsp:forward>
<%
		}
	}
%>
