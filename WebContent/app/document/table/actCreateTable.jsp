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
	String prmTableName = EIMUtils.getParameter(request, "tableName");

	//Message
	String message = null;
	Object[] paramId = {
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
		
		//User
		user = (EIMUser)sess.getAttribute("USER");
		
		//Table作成
		//EIMTable table = (EIMTable)tableList.get(i);
		EIMTable table = TableUtils.createTable(sess, prmTableName, EIMTable.NOT_SELECTED);
		
		//Create Operation History
		OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.CREATE_TABLE, 
				EIMConstant.TARGET_CREATE, EIMConstant.TABLE, table,
				null, null, null, null);

		//XML作成（作成したテーブルID　etcを生成）
		//Root Node
		/*
		out.println("<tableList>");
			out.println("<table");
				out.println(" label=\"" + "test2" + "\"");
				out.println(" isBranch=\"" + "false" + "\"");
				out.println(" tableId=\"" + "test2" + "\"");
				out.println(" tableName=\"" + "test2" + "\"");
			out.println(">");
			out.println("</table>");
		out.println("</tableList>");
		*/
		
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
