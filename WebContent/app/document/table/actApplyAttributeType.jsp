<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "java.util.List"%>
<%@ page import = "java.util.ArrayList"%>
<%@ page import = "java.util.Iterator"%>
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
	String prmTableId = EIMUtils.getParameter(request, "tableId");
	String prmAttTypeIds = EIMUtils.getParameter(request, "attTypeIds");
	
	//Message
	String message = null;
	Object[] paramId = {
			"tableId=" + prmTableId,
			"attTypeIds=" + prmAttTypeIds
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
		
		//apply Attribute Types
		List attTypes = new ArrayList();
		for (Iterator i = StringUtils.getStringListFromCSV(prmAttTypeIds).iterator();i.hasNext();)
		{
			String attTypeId = (String)i.next();
			EIMAttributeType attType = AttributeUtils.getAttributeTypeById(sess, Long.parseLong(attTypeId));
			if(attType == null)
			{
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.ATTTYPE.NOTFOUND");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.ATTTYPE.NOTFOUND");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
				sess.rollback();//例外発生時の処理は、外側のtry-catchに任せる
				return;
			}

			//Apply Attribute Type
			TableUtils.applyAttributeType(sess, table, attType);
			
			//Create Operation History
			OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.APPLY_TABLE_ATTRIBUTE, 
					EIMConstant.TARGET_UPDATE, EIMConstant.TABLE, table,
					EIMConstant.TARGET_CHILD_ATTRIBUTE, EIMConstant.ATTRIBUTE_TYPE, attType, null);
			attTypes.add(attType);
		}

		//Commit
		sess.commit();
		
		//XML
		out.println("<attTypes>");
		for (Iterator i = attTypes.iterator();i.hasNext();) {
			EIMAttributeType attType = (EIMAttributeType)i.next();
			out.println("<attType");
				out.println(" attTypeId=\"" + attType.getId() + "\"");
				out.println(" attTypeName=\"" + StringUtils.xmlEncode(attType.getName()) + "\"");
				out.println(" valTypeId=\"" + attType.getValueType().getId() + "\"");
				out.println(" valTypeName=\"" + StringUtils.xmlEncode(attType.getValueType().getName()) + "\"");
				out.println(">");
			out.println("</attType>");
		}
		out.println("</attTypes>");
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
