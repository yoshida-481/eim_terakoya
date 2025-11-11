<%@page import="jp.co.ctc_g.eim.framework.integration.dao.impl.xml.SysFuncDaoImpl"%>
<%@page import="jp.co.ctc_g.eim.framework.integration.dao.impl.xml.StatusTypeKindDaoImpl"%>
<%@page import="jp.co.ctc_g.eim.framework.integration.dao.impl.xml.GuardConditionDaoImpl"%>
<%@page import="jp.co.ctc_g.eim.framework.integration.dao.impl.xml.BaseEventTypeDaoImpl"%>
<%@page import="jp.co.ctc_g.eim.framework.business.dao.BaseEventTypeDao"%>
<%@page import="jp.co.ctc_g.eim.framework.business.service.AdminAuthCheckPlugIn"%>
<%@page import="jp.co.ctc_g.eim.framework.business.service.WorkFlowConfService"%>
<%@page import="jp.co.ctc_g.eim.framework.common.util.EIMThreadContext"%>
<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>

<%@ page import = "jp.co.ctc_g.eim.framework.business.service.impl.*" %>
<%@ page import = "jp.co.ctc_g.eim.framework.business.domain.*" %>

<%@ page import = "java.util.*" %>
<%@ page import = "common.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	//Session
	EIMSession sess = null;
	EIMUser loginUser = null;
	boolean flg = false;
	//Message
	String message = null;
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
		loginUser = (EIMUser)sess.getAttribute("USER");
		if(!AdminAuthUtil.hasAnyAuth(loginUser))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.NOTADMINISTRATOR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.NOTADMINISTRATOR");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message));
			return;
		}
		if(EIMThreadContext.getEIMSession() == null)
		{
			flg = true;
			EIMThreadContext.putEIMSession(sess);
		}

		//Valiables
		WorkFlowConfServiceImpl wfcService = new WorkFlowConfServiceImpl();
		wfcService.setAdminAuthCheckPlugIn(new GeneralAdminAuthCheckPlugInImpl());
		wfcService.setBaseEventTypeDao(new BaseEventTypeDaoImpl());
		wfcService.setGuardConditionDao(new GuardConditionDaoImpl());
		wfcService.setStatusTypeKindDao(new StatusTypeKindDaoImpl());
		wfcService.setSysFuncDao(new SysFuncDaoImpl());

		WorkFlowConfDomain domain = wfcService.get();
		List sysFuncList = domain.getSysFuncList();

		SysFuncDomain sysFunc = null;

		//Root Node
		out.println("<systemFunctions>");

		for(int i = 0; i < sysFuncList.size(); i++)
		{
			//UserDefGroup
			sysFunc = (SysFuncDomain)sysFuncList.get(i);

			//XML
			out.println("<systemFunction");
				out.println(" name=\"" + StringUtils.xmlEncode(sysFunc.getName()) + "\"");
				out.println(" isBranch=\"" + "false" + "\"");
				out.println(" sysFuncId=\"" + sysFunc.getId() + "\"");
			out.println("/>");
		}

		//End Root Node
		out.println("</systemFunctions>");
	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), eime.getMessage()), eime);
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
		if(flg == true)
			EIMThreadContext.removeEIMSession();
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
