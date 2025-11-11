<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*"%>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>

<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.UserService"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.TransactionContext"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext"%>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");
	
	//Session
	EIMSession sess = null;
	EIMUser loginUser = null;
	boolean sessPutFlag = false;
	
	//Parameter
	String prmUserId = EIMUtils.getParameter(request, "userId");

	//Message
	String message = null;
	Object[] paramId = {
			"userId=" + prmUserId
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
		
		// ThreadLocalコンテキストにセッションを設定
		if(jp.co.ctc_g.eim.framework.common.util.EIMThreadContext.getEIMSession() == null)
		{
			jp.co.ctc_g.eim.framework.common.util.EIMThreadContext.putEIMSession(sess);
			sessPutFlag = true;
		}
		
		//User
		loginUser = (EIMUser)sess.getAttribute("USER");
		if(!AdminAuthUtil.hasSpecifiedOrGeneralAuth(loginUser, AppConstant.ADMIN_AUTH_ID_USER))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.NOTADMINISTRATOR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.NOTADMINISTRATOR");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message));
			return;
		}
		
		//User
		EIMUser user = UserUtils.getUserById(sess, Long.parseLong(prmUserId));
		if(user == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.USER.NOTFOUND");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.USER.NOTFOUND");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}
		
		List groups = GroupUtils.getGroupByUser(sess, user);
		for (int i = 0; i < groups.size(); i++) {
			EIMGroup group = (EIMGroup)groups.get(i);
			// SearchFramework 検索FW更新通知 対象：グループ
			AppUpdateNoticeUtils.updateNoticeInsert(group.getId(), "SEARCHFW_USER_EDIT_GROUP");
		}
		
		List roles = RoleUtils.getRoleByUser(sess, user);
		for (int j = 0; j < roles.size(); j++) {
			EIMRole role = (EIMRole)roles.get(j);
			// SearchFramework 検索FW更新通知 対象：ロール
			AppUpdateNoticeUtils.updateNoticeInsert(role.getId(), "SEARCHFW_USER_EDIT_ROLE");
		}
		
		// 全セキュリティを取得し、アクセスエントリのユーザを比較して対象のセキュリティを取得
		List<EIMSecurity> securityList = SecurityUtils.getSecurityList(sess);
		for (EIMSecurity security : securityList) {
			
			// 全セキュリティ取得
			List<EIMAccessEntry> accessEntryList = SecurityUtils.getAccessEntryList(sess, security);
			for (EIMAccessEntry accessEntry : accessEntryList) {
				
				// ユーザチェック
				if (accessEntry.getUser() == null ) {
					continue;
				}
				if (accessEntry.getUser().getId() == user.getId()) {
					// SearchFramework 検索FW更新通知 対象：セキュリティ
					AppUpdateNoticeUtils.updateNoticeInsert(security.getId(), "SEARCHFW_SECURITY_DELACENTRY_SECURITY");
					break;
				}
			}
		}
		
		//Delete
		if(EIMThreadContext.getTransactionContext() != null)
		{
			EIMThreadContext.removeTransactionContext();
		}
		
		TransactionContext context = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
		EIMThreadContext.putTransactionContext(context);
		context.setLangId(sess.getLangId());
		context.setDBConnection(sess.getDBConnection());
		context.setUser(ConvertUtils.toUserDomain(loginUser));
		
		UserDomain userDomain = new UserDomain(user.getId());
		userDomain.setCode(user.getCode());
		UserService userService = (UserService)ApplicationContextLoader.getApplicationContext().getBean("userService2");
		userService.delete(userDomain);
		
		// ユーザ削除の操作履歴はAOPに設定済み(userService#delete)
		// OperationHistoryUtils#createは削除する
		
		//Commit
		sess.commit();
		
		out.println("<OK></OK>");
		
	}
	catch(jp.co.ctc_g.eim.framework2.common.exception.EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), eime.getMessage(), paramId), eime);
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
		try {
			if(sessPutFlag) {
				jp.co.ctc_g.eim.framework.common.util.EIMThreadContext.removeEIMSession();
				sessPutFlag = false;
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
