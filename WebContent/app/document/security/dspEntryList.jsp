<%@page import="jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext"%>
<%@page import="jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils"%>
<%@page import="jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum"%>
<%@page import="jp.co.ctc_g.eim.framework2.common.context.TransactionContext"%>
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

	//Session
	EIMSession sess = null;
	EIMUser user = null;

	//Parameter
	String prmStatusTypeId = request.getParameter("statusTypeId");
	String prmSecId = request.getParameter("secId");

	//Message
	String message = null;
	Object[] paramId = {
			"statusTypeId=" + prmStatusTypeId,
			"secId=" + prmSecId
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

		//Security
		EIMSecurity sec = null;
		if(prmSecId != null && !prmSecId.equals(""))
		{
			sec = SecurityUtils.getSecurityById(sess, Long.parseLong(prmSecId));
			if(sec == null)
			{
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOSECURITY");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOSECURITY");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
				return;
			}
		}
		else
		{
			//Status
			EIMStatusType statusType = WorkFlowUtils.getStatusTypeById(sess, Long.parseLong(prmStatusTypeId));
			if(statusType == null)
			{
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.STTYPE.NOTFOUND");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.STTYPE.NOTFOUND");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
				return;
			}

			sec = (EIMSecurity)statusType;
		}

		//Entry List
		List entryList = SecurityUtils.getAccessEntryList(sess, sec);

		TransactionContext context = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
		EIMThreadContext.putTransactionContext(context);
		context.setLangId(sess.getLangId());
		context.setDBConnection(sess.getDBConnection());
		context.setUser(ConvertUtils.toUserDomain(user));

		//Root Node
		out.println("<entryList>");

		for(int i = 0; i < entryList.size(); i++)
		{
			//Entry
			EIMAccessEntry entry = (EIMAccessEntry)entryList.get(i);

			//XML
			out.println("<entry");
				out.println(" entryId=\"" + entry.getId() + "\"");
				out.println(" entryTypeId=\"" + entry.getType().getId() + "\"");
				out.println(" entryTypeName=\"" + StringUtils.xmlEncode(entry.getType().getName()) + "\"");
				if(entry.getType().getId() == EIMAccessEntryType.USER)
				{
					out.println(" entryObjName=\"" + StringUtils.xmlEncode(entry.getUser().getName()) + "\"");
					out.println(" entryObjId=\"" + entry.getUser().getId() + "\"");
				}
				else if(entry.getType().getId() == EIMAccessEntryType.GROUP)
				{
					out.println(" entryObjName=\"" + StringUtils.xmlEncode(entry.getGroup().getName()) + "\"");
					out.println(" entryObjId=\"" + entry.getGroup().getId() + "\"");
				}
				else if(entry.getType().getId() == EIMAccessEntryType.ROLE)
				{
					out.println(" entryObjName=\"" + StringUtils.xmlEncode(entry.getRole().getName()) + "\"");
					out.println(" entryObjId=\"" + entry.getRole().getId() + "\"");
				}
				else if(entry.getType().getId() == EIMAccessEntryType.COMP)
				{
					out.println(" entryObjName=\"" + StringUtils.xmlEncode(entry.getComp().getName()) + "\"");
					out.println(" entryObjId=\"" + entry.getComp().getId() + "\"");
				}
				else if(entry.getType().getId() == EIMAccessEntryType.USERDEF)
				{
					out.println(" entryObjName=\"" + StringUtils.xmlEncode(entry.getUserDefGroup().getName()) + "\"");
					out.println(" entryObjId=\"" + entry.getUserDefGroup().getId() + "\"");
				}
				else if(entry.getType().getId() == EIMAccessEntryType.OBJROLE)
				{
					out.println(" entryObjName=\"" + StringUtils.xmlEncode(entry.getObjectRole().getName()) + "\"");
					out.println(" entryObjId=\"" + entry.getObjectRole().getId() + "\"");
				}
				out.println(" priority=\"" + entry.getPriority() + "\"");
			out.println(">");
			out.println("</entry>");
		}

		//End Root Node
		out.println("</entryList>");
	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(user.getId(), eime.getMessage(), paramId), eime);
	}
	catch(Exception e)
	{
		out.clear();
		message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
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
