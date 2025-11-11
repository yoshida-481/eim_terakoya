<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>
<%@ page import = "common.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "jp.co.ctc_g.eim.admin.business.domain.NamespaceDomain" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.TransactionContext" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.exception.EIMException"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.WorkflowService" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.WorkflowDomain" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.StatusTypeDomain" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.ObjectService"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.criteria.ObjectCriteria"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.criteria.*"%>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	//Session
	EIMSession sess = null;
	EIMUser loginUser = null;

	//Parameter
	String prmWorkflowId = request.getParameter("workflowId");

	//Message
	String message = null;

	try {
		//Session
		sess = EIMUtils.getSession(request);
		if (sess == null) {
			message = EIMResource.getMessage(request, "EIM.ERROR.SESSIONTIMEOUT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.SESSIONTIMEOUT");
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}

		//User
		loginUser = (EIMUser) sess.getAttribute("USER");
		if (!AdminAuthUtil.hasSpecifiedOrGeneralAuth(loginUser, AppConstant.ADMIN_AUTH_ID_DOCUMENTTYPE)) {
			message = EIMResource.getMessage(sess, "EIM.ERROR.NOTADMINISTRATOR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.NOTADMINISTRATOR");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message));
			return;
		}

		//定義名称のあいまい検索を行うためV5APIを使用
		if (EIMThreadContext.getTransactionContext() != null) {
			EIMThreadContext.removeTransactionContext();
		}

		TransactionContext context = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
		EIMThreadContext.putTransactionContext(context);
		context.setLangId(sess.getLangId());
		context.setDBConnection(sess.getDBConnection());
		context.setUser(ConvertUtils.toUserDomain(loginUser));

		ObjectService objectService = (ObjectService) ApplicationContextLoader.getApplicationContext().getBean("objectService2");
		WorkflowService workflowService = (WorkflowService) ApplicationContextLoader.getApplicationContext().getBean("workflowService2");

		boolean isExist = false; // 対象のワークフロータイプの割当てられたオブジェクトが存在有無

		if (!StringUtils.isBlank(prmWorkflowId)) {
			WorkflowDomain workflowDomain = workflowService.getById(Long.parseLong(prmWorkflowId));
			List<StatusTypeDomain> statusTypeList = workflowDomain.getStatusTypeList();
			ObjectCriteria objectCriteria = new ObjectCriteria();
			// 該当WFの保持するステータスタイプのIDを設定
			MultipleCriteria<Long> statusTypeIds = new MultipleCriteria<Long>();
			for (StatusTypeDomain statusTypeDomain : statusTypeList) {
				statusTypeIds.add(statusTypeDomain.getId());
			}
			if(statusTypeIds.size() > 0){
				objectCriteria.setStatusTypeIds(statusTypeIds);
				// オブジェクトを全て取得
				List<ObjectDomain> objectList = objectService.getList(objectCriteria);
				if (objectList != null && objectList.size() > 0) {
					// 該当オブジェクトが1件以上の場合
					isExist = true;
				}
			}
		}

		//XML
		out.println("<workflow_exist");
		out.println(" satisfy=\"" + (isExist ? "true" : "") + "\"");
		out.println("/>");
	} catch (EIMException eime) {
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), eime.getMessage()), eime);
	} catch (Exception e) {
		out.clear();
		message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
	} finally {
		try {
			if (EIMThreadContext.getTransactionContext() != null) {
				EIMThreadContext.removeTransactionContext();
			}

			if (sess != null) {
				sess.close();
			}
		} catch (Exception se) {
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
	}
%>
