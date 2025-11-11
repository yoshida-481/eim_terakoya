<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");
	
	EIMSession sess = null;
	EIMUser user = null;
	
	//Parameter
	String prmTableId = request.getParameter("tableId");
	//String prmTableDefName = request.getParameter("tableDefName");
	String prmTableDefName = new String(request.getParameter("tableDefName").getBytes("ISO_8859_1"), "UTF-8");
	
	//Message
	String message = null;
	Object[] paramId = {
			"tableId=" + prmTableId,
			"tableDefName=" + prmTableDefName
			};
	
	try
	{
		//Session
		sess = EIMUtils.getSession(request);
		if(sess == null)
		{
			message = EIMResource.getMessage(request, "EIM.ERROR.SESSIONTIMEOUT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.SESSIONTIMEOUT");
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}
		
		//User
		user = (EIMUser)sess.getAttribute("USER");
		
		//Table
		EIMTable table = null;	//デフォルトテーブル選択時はnullとなる
		
		if (prmTableId != null && prmTableId.length() > 0) {
			// ユーザテーブル選択時
			table = TableUtils.getTableById(sess, Long.parseLong(prmTableId));
			
			// EIMテーブルを選択にする
			TableUtils.updateSelected(sess, table);
			
			// カスタムデフォルトテーブルを未選択にする
			CustomDefaultTableUtils.updateSelected(sess, null);
			
		}else if(prmTableDefName != null && prmTableDefName.length() > 0){
			// カスタムデフォルトテーブル選択時
			
			// カスタムデフォルトテーブルを選択にする
			CustomDefaultTableUtils.updateSelected(sess, prmTableDefName);
			
			// EIMテーブルを未選択にする
			TableUtils.updateSelected(sess, null);
		}else{
			// デフォルトテーブル選択時
			
			// EIMテーブルを未選択にする
			TableUtils.updateSelected(sess, null);
			
			// カスタムデフォルトテーブルを未選択にする
			CustomDefaultTableUtils.updateSelected(sess, null);
		}
		
		sess.commit();
		
		out.println("<ok/>");

	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(user.getId(), eime.getMessage(), paramId), eime);
		try{
			if(sess != null){
				sess.rollback();
			}
		}
		catch (Exception se) {
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
	}
	catch(Exception e)
	{
		out.clear();
		message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
		try{
			if(sess != null){
				sess.rollback();
			}
		}
		catch (Exception se) {
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
	}
	finally
	{
		try{
			if(sess != null){
				sess.close();
			}
		}
		catch (Exception se) {
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
	}
%>
