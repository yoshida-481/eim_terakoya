<%@page import="jp.co.ctc_g.eim.framework2.business.domain.criteria.MultipleCriteria"%>
<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>
<%@ page import = "common.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>

<%@ page import = "common.util.UserListUtil"%>

<%@ page import = "jp.co.ctc_g.eim.admin.business.domain.criteria.AdminUserCriteria"%>
<%@ page import = "jp.co.ctc_g.eim.admin.business.service.AdminUserService"%>

<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.criteria.MultipleCriteria"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.TransactionContext"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.exception.EIMException"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils"%>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());
	
	// ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");
	
	// Session
	EIMSession sess = null;
	EIMUser loginUser = null;
	
	//Parameter
	String prmUserId = EIMUtils.getParameter(request, "userId");
	String prmUserCode = EIMUtils.getParameter(request, "userCode");
	String prmUserName = EIMUtils.getParameter(request, "userName");
	String prmUserKana = EIMUtils.getParameter(request, "userKana");
	String prmUserMail = EIMUtils.getParameter(request, "userMail");
	String prmIsNotDisplayInvalidityUser = EIMUtils.getParameter(request, "isNotDisplayInvalidityUser");
	String prmIsNotDisplayValidityUser = EIMUtils.getParameter(request, "isNotDisplayValidityUser");
	String prmBelongingGroupName = EIMUtils.getParameter(request, "belongingGroupName");
	String prmIncludingChildGroup = EIMUtils.getParameter(request, "includingChildGroup");
	String prmOpeName = EIMUtils.getParameter(request, "opeName");
	
	// Message
	String message = null;
	Object[] paramId = {
		"userId=" + prmUserId,
		"userCode=" + prmUserCode,
		"userName=" + prmUserName,
		"userKana=" + prmUserKana,
		"userMail" + prmUserMail,
		"isNotDisplayInvalidityUser" + prmIsNotDisplayInvalidityUser,
		"isNotDisplayValidityUser" + prmIsNotDisplayValidityUser,
		"belongingGroupName" + prmBelongingGroupName,
		"includingChildGroup" + prmIncludingChildGroup,
		"opeName=" + prmOpeName
	};

	try
	{
		// Session
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
		if(!AdminAuthUtil.hasAnyAuth(loginUser))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.NOTADMINISTRATOR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.NOTADMINISTRATOR");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message));
			return;
		}
		
		// トランザクションコンテキスト
		if(EIMThreadContext.getTransactionContext() != null)
		{
			EIMThreadContext.removeTransactionContext();
		}
		TransactionContext context = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
		EIMThreadContext.putTransactionContext(context);
		context.setLangId(sess.getLangId());
		context.setDBConnection(sess.getDBConnection());
		context.setUser(ConvertUtils.toUserDomain(loginUser));
		
		//
		// ユーザ検索
		//
		
		List userList = null;
		
		// 検索条件の設定
		AdminUserCriteria adminUserCriteria = new AdminUserCriteria();
		
		if (prmUserId != null && prmUserId.length() > 0)
			adminUserCriteria.setIds(new MultipleCriteria(Arrays.asList(Long.parseLong(prmUserId))));
		
		if (prmUserCode != null && prmUserCode.length() > 0)
			adminUserCriteria.setCode("*" + prmUserCode + "*");
		
		if (prmUserName != null && prmUserName.length() > 0)
			adminUserCriteria.setName("*" + prmUserName + "*");
		
		if (prmUserKana != null && prmUserKana.length() > 0)
			adminUserCriteria.setKana("*" + prmUserKana + "*");
		
		if (prmUserMail != null && prmUserMail.length() > 0)
			adminUserCriteria.setMail("*" + prmUserMail + "*");
		
		if (prmIsNotDisplayInvalidityUser != null && prmIsNotDisplayInvalidityUser.equals("1"))
		{
			// 有効ユーザのみ検索対象とする
			adminUserCriteria.setDisable(false);
		}
			//エントリー参照画面からコールされた場合
			else if (prmIsNotDisplayValidityUser != null && prmIsNotDisplayValidityUser.equals("1"))
		{
			// 無効ユーザのみ検索対象とする
			adminUserCriteria.setDisable(true);
		}
		else
		{
			// 全ユーザを検索対象とする
			adminUserCriteria.setDisable(null);
		}
		
		if (prmBelongingGroupName != null && prmBelongingGroupName.length() > 0) {
			adminUserCriteria.setGroupName("*" + prmBelongingGroupName + "*");
			adminUserCriteria.setIncludingChildGroup(prmIncludingChildGroup.equals("true") ? true:false);
		}
		
		// 取得件数の制限
		adminUserCriteria.setLimit(Integer.parseInt(EIMConfig.get("GET_DATA_MAX_NUM")));
		adminUserCriteria.setLimitCondition(true);
		
		// ユーザ検索の実行
		AdminUserService adminUserService = (AdminUserService)ApplicationContextLoader.getApplicationContext().getBean("adminUserServiceForUserSearch");
		
		try
		{
			userList = adminUserService.getList(adminUserCriteria);
		}
		catch(jp.co.ctc_g.eim.framework2.common.exception.EIMException eime)
		{
			// 検索結果が上限数より大きい時
			if (eime.getMessageKey().equals("EIM.ERROR.LOGIC.SEARCH.RESULT.LIMIT.OVER"))
			{
				message = eime.getMessage();
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				return;
			}
			
			throw eime;
		}
		
		// 検索結果が0件の時
		if(userList.size() == 0)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NORESULT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
		}
		
		// ユーザのリストをuserCodeでソートする
		List sortedUserList = AppObjectUtil.getStrSortedList(userList, "getCode", true);
		
		//
		// 検索結果の出力
		//
		
		//Root Node
		out.println("<users>");
		
		for(int i = 0; i < sortedUserList.size(); i++)
		{
			// User
			UserDomain user = (UserDomain)sortedUserList.get(i);
			
			// Check For System
			if(user.getId() == 1 && loginUser.getId() != 1)
			{
				if(prmOpeName != null && prmOpeName.equals("OPE_HIST"))
				{
					// 履歴検索用のユーザリスト取得の場合
					// ユーザにかかわらずSystemユーザを返す
				}
				else
				{
					continue;
				}
			}
			
			// かな
			String userKana = "";
			if(user.getKana() != null)
			{
				userKana = user.getKana();
			}
			
			// Mail
			String userMail = "";
			if(user.getMail() != null)
			{
				userMail = user.getMail();
			}
			
			// 所属グループ名称
			String groupName = "";
			List belongingGroupList = user.getGroupList();
			if (belongingGroupList != null) {
				groupName = UserListUtil.toSeparatedGroupNames(belongingGroupList);
			}

			// 所属グループID
			String groupId = "";
			List belongingGroupIdList = user.getGroupList();
			if (belongingGroupIdList != null) {
				groupId = UserListUtil.toSeparatedGroupIds(belongingGroupIdList);
			}

			// 所属ロール名称
			String roleName = "";
			List belongingRoleList = user.getRoleList();
			if (belongingRoleList != null) {
				roleName = UserListUtil.toSeparatedRoleNames(belongingRoleList);
			}

			// 所属ロールID
			String roleId = "";
			List belongingRoleIdList = user.getRoleList();
			if (belongingRoleIdList != null) {
				roleId = UserListUtil.toSeparatedRoleIds(belongingRoleIdList);
			}

			// 無効フラグ
			String disable = "";
			if (user.isDisable())
			{
				disable = "on";
			}
			else
			{
				disable = "off";
			}
			
			// XML
			out.println("<user");
				out.println(" userId=\"" + user.getId() + "\"");
				out.println(" userCode=\"" + StringUtils.xmlEncode(user.getCode()) + "\"");
				out.println(" userName=\"" + StringUtils.xmlEncode(user.getName()) + "\"");
				out.println(" userKana=\"" + StringUtils.xmlEncode(userKana) + "\"");
				out.println(" userMail=\"" + StringUtils.xmlEncode(userMail) + "\"");
				out.println(" groupName=\"" + StringUtils.xmlEncode(groupName) + "\"");
				out.println(" groupId=\"" + StringUtils.xmlEncode(groupId) + "\"");
				out.println(" roleName=\"" + StringUtils.xmlEncode(roleName) + "\"");
				out.println(" roleId=\"" + StringUtils.xmlEncode(roleId) + "\"");
				out.println(" userDisable=\"" + disable + "\"");
				out.println(">");
			out.println("</user>");
		}
		
		// End Root Node
		out.println("</users>");
	}
	catch(jp.co.ctc_g.eim.framework2.common.exception.EIMException eime)
	{
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
