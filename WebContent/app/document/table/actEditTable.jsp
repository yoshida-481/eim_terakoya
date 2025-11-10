<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>

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
	String prmTableName = EIMUtils.getParameter(request, "tableName");
	
	//Message
	String message = null;
	Object[] paramId = {
			"tableId=" + prmTableId,
			"tableName=" + prmTableName
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
		
		user = (EIMUser)sess.getAttribute("USER");
		
		//Table
		EIMTable table = TableUtils.getTableById(sess, Long.parseLong(prmTableId));
		
		//不正アクセスのチェック
		int count = 0;
		List tableList = TableUtils.getTableListByUser(sess, user);				
		
		for(int i = 0; i < tableList.size(); i++)
		{
			//Table	
			EIMTable table1 = (EIMTable)tableList.get(i);
			if(table1.getId() == table.getId()){
				count++;
				break;
			}
		}
		
		if(count == 0){
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.ILLEGAL.REQUEST");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}
		
		//Get old info
		String oldTableName = table.getName();
		
		//Table編集
		table = TableUtils.updateTable(sess, table, prmTableName);
		
		//Create Operation History
		OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.UPDATE_TABLE, 
				EIMConstant.TARGET_UPDATE, EIMConstant.TABLE, table,
				null, null, null, null);

		out.println("<table");
			out.println(" label=\"" + StringUtils.xmlEncode(table.getName()) + "\"");
			out.println(" isBranch=\"" + "false" + "\"");
			out.println(" tableId=\"" + table.getId() + "\"");
			out.println(" tableName=\"" + StringUtils.xmlEncode(table.getName()) + "\"");
			out.println(" selected=\"" + table.getSelected() + "\"");
		out.println(">");
		out.println("</table>");
		
		//Commit
		sess.commit();
		
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
