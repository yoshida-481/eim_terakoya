<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>
<%@ page import = "common.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>

<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.*"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.criteria.*"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.exception.EIMException"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.GroupService"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.TransactionContext"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext"%>

<%
 	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	// ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	// Session
	EIMSession sess = null;
	EIMUser loginUser = null;

	// Parameter
	String prmSearchGroupName = EIMUtils.getParameter(request, "serchGroupName");

	// Message
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

		// User
		loginUser = (EIMUser)sess.getAttribute("USER");

		// 管理者権限チェック
		if(!AdminAuthUtil.hasAnyAuth(loginUser))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.NOTADMINISTRATOR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.NOTADMINISTRATOR");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message));
			return;
		}

		// framework2を利用するための設定
		TransactionContext context = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
		EIMThreadContext.putTransactionContext(context);
		context.setLangId(sess.getLangId());
		context.setDBConnection(sess.getDBConnection());
		context.setUser(ConvertUtils.toUserDomain(loginUser));

		// グループ検索
		GroupService groupService = (GroupService)ApplicationContextLoader.getApplicationContext().getBean("groupService2");
		GroupCriteria groupCriteria = new GroupCriteria();
		groupCriteria.setName("*" + prmSearchGroupName + "*");
		List<GroupDomain> groupList = groupService.getList(groupCriteria);

		// 検索結果が0件の時
		if(groupList.size() == 0)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NORESULT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
		}

		// 検索結果が1000件より大きい時
		if(groupList.size() > Integer.parseInt(EIMConfig.get("GET_GROUPDATA_MAX_NUM")))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.OVERFLOWRESULT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
		}

		// グループのリストをgroupNameでソートする
		List<GroupDomain> sortedGroupList = AppObjectUtil.getStrSortedList(groupList, "getName", true);

		// Root Node
		out.println("<groups>");

 		for(int i = 0; i < sortedGroupList.size(); i++)
		{
			GroupDomain groupDomain = sortedGroupList.get(i);
			out.println("<group");
			out.println(" groupName=\"" + StringUtils.xmlEncode(groupDomain.getName()) + "\"");
			out.println(" parentGroupName=\"" + StringUtils.xmlEncode(EntryUtil.getParentGroupName(groupDomain)) + "\"");
			out.println(" groupId=\"" + groupDomain.getId() + "\"");
			out.println(">");
			out.println("</group>");
		}
		// End Root Node
		out.println("</groups>");
	}
	catch(EIMException eime)
	{
	 	Object[] paramId = {"serchGroupName=" + prmSearchGroupName};
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), eime.getMessage(), paramId), eime);
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
			if(EIMThreadContext.getTransactionContext() != null){
				EIMThreadContext.removeTransactionContext();
			}
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
