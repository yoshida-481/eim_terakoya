<%@ page contentType = "text/xml; charset=UTF-8" %>

<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>
<%@ page import = "java.util.ArrayList"%>

<%@ page import = "jp.co.ctc_g.eim.framework.common.exception.EIMSysException" %>
<%@ page import = "jp.co.ctc_g.eim.framework.common.exception.EIMAppException" %>
<%@ page import = "jp.co.ctc_g.eim.framework.common.util.EIMThreadContext"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.TransactionContext" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.exception.EIMException"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.SecurityService"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.SecurityDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.StatusTypeDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.StatusSecurityDomain"%>

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
	EIMUser loginUser = null;
	boolean sessPutFlag = false;
	
	//Message
	String message = null;

	//Parameter
	
	// DefaultSecuityID
	String defaultSecId = request.getParameter("secId");
	
	// (コピー元)ステータスタイプID
	String fromStatusTypeIds = request.getParameter("fromStatusTypeIds");
	
	// (コピー先)ステータスタイプID
	String toStatusTypeIds = request.getParameter("toStatusTypeIds");
	
	Object[] paramId = {
			"defaultSecId=" + defaultSecId,
			"fromStatusTypeIds=" + fromStatusTypeIds,
			"toStatusTypeIds=" + toStatusTypeIds
			};
	
	try
	{
		/*
		 * param check
		 */
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
		if(EIMThreadContext.getEIMSession() == null)
		{
			EIMThreadContext.putEIMSession(sess);
			sessPutFlag = true;
		}
		
		//User
		loginUser = (EIMUser)sess.getAttribute("USER");
		if(!AdminAuthUtil.hasSpecifiedOrGeneralAuth(loginUser, AppConstant.ADMIN_AUTH_ID_SECURITY))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.NOTADMINISTRATOR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.NOTADMINISTRATOR");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message));
			return;
		}
		
		// ステータスタイプIDを設定
		if (StringUtils.isBlank(fromStatusTypeIds) || StringUtils.isBlank(toStatusTypeIds)) {
			// ステータスタイプが取得できません
			message = EIMResource.getMessage(sess, "MSG_NOT_STATUSTYPE");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("MSG_NOT_STATUSTYPE");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}
		String [] fromStatusTypeIdArray = fromStatusTypeIds.split(",");
		String [] toStatusTypeIdArray = toStatusTypeIds.split(",");
		
		if (fromStatusTypeIdArray.length == 0 
				|| toStatusTypeIdArray.length == 0 
				|| fromStatusTypeIdArray.length != toStatusTypeIdArray.length) {
			// ステータスタイプが取得できません
			message = EIMResource.getMessage(sess, "MSG_NOT_STATUSTYPE");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("MSG_NOT_STATUSTYPE");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}
		
		if(EIMThreadContext.getTransactionContext() != null)
		{
			EIMThreadContext.removeTransactionContext();
		}
		// TransactionContextの作成、設定
		TransactionContext context = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
		EIMThreadContext.putTransactionContext(context);
		context.setLangId(sess.getLangId());
		context.setDBConnection(sess.getDBConnection());
		context.setUser(ConvertUtils.toUserDomain(loginUser));
		
		// Securityサービス
		SecurityService securityService = (SecurityService)ApplicationContextLoader.getApplicationContext().getBean("securityService2");
		
		// デフォルトセキュリティを取得
		SecurityDomain securityDomain = null;
		if (!StringUtils.isBlank(defaultSecId)) {
			securityDomain = securityService.getById(Long.parseLong(defaultSecId));
			if (securityDomain == null) {
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.SEC.NOTFOUND");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.SEC.NOTFOUND");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				return;
			}
		} else {
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.SEC.NOTFOUND");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.SEC.NOTFOUND");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}
		
		List<StatusSecurityDomain> retList = new ArrayList<StatusSecurityDomain>();	// 返却用のステータス別セキュリティ格納用
		
		// ステータス別セキュリティのコピー
		for (int i = 0 ; i < fromStatusTypeIdArray.length ; i++) {
			
			// コピー元とコピー先が同一ステータスタイプの場合、無視する
			if (fromStatusTypeIdArray[i].equals(toStatusTypeIdArray[i])) {
				continue;
			}
			
			// (コピー先)ステータス別セキュリティ
			StatusSecurityDomain destStatusSecurity = new StatusSecurityDomain();
			destStatusSecurity.setStatusType(new StatusTypeDomain(Long.parseLong(toStatusTypeIdArray[i])));
			
			// (コピー元)ステータス別セキュリティの取得
			StatusSecurityDomain fromStatusSecurity = securityService.getStatusSecurityBySecurityAndStatusType(
					securityDomain, new StatusTypeDomain(Long.parseLong(fromStatusTypeIdArray[i])));

			// (コピー元)ステータス別セキュリティが存在する場合
			if (fromStatusSecurity != null) {
			
				StatusSecurityDomain retStatusSecurity = securityService.copyStatusSecurity(
												securityDomain, 
												fromStatusSecurity, 
												destStatusSecurity);
				
				if (retStatusSecurity != null) {
					retList.add(retStatusSecurity);
				}
			}
		}
		
		// ステータス別セキュリティのコピーが実行された場合
		if (retList.size() > 0) {
		
			// SearchFramework 検索FW更新通知 対象：セキュリティ
			AppUpdateNoticeUtils.updateNoticeInsert(securityDomain.getId(), "SEARCHFW_SECURITY_CREATESTSEC_SECURITY");
			
			// 履歴追加
			for (StatusSecurityDomain retStatusSecurity : retList) {
				EIMStatusSecurity retStatusSec = StatusSecurityUtils.getStatusSecurityById(sess, retStatusSecurity.getId());
				OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.CREATE_STATUS_SECURITY, 
						EIMConstant.TARGET_CREATE, EIMConstant.SECURITY, retStatusSec,
						null, null, null, null );
			}
		}
		
		// 出力XML - デフォルトセキュリティを返却する
		out.println("<security secId=\"" + securityDomain.getId() + "\"/>");
		
		// コミット
		sess.commit();
	}
	catch(EIMSysException eimse) {
		out.clear();
		message = EIMResource.getMessageValue(sess, "EIM.ERROR.SYSTEMERROR");
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.error(AppMessageUtils.makeLogMessage(loginUser.getId(), eimse.getMessage(), paramId), eimse);
		try{
			if(sess != null){
				sess.rollback();
			}
		}
		catch (Exception se) {
			out.clear();
			message = EIMResource.getMessageValue(sess, "EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
	}
	catch(EIMAppException eimappe)
	{
		out.clear();
		message = eimappe.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), eimappe.getMessage()), eimappe);
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
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), eime.getMessage()), eime);
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
			if(sessPutFlag) {
				EIMThreadContext.removeEIMSession();
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