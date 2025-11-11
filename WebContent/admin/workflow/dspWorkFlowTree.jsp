<%@page import="java.util.stream.Collectors"%>
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
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.criteria.WorkflowCriteria" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.WorkflowService" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.WorkflowDomain" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils" %>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	//Session
	EIMSession sess = null;
	EIMUser loginUser = null;

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
		if (!AdminAuthUtil.hasSpecifiedOrGeneralAuth(loginUser, AppConstant.ADMIN_AUTH_ID_WORKFLOW)) {
			message = EIMResource.getMessage(sess, "EIM.ERROR.NOTADMINISTRATOR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.NOTADMINISTRATOR");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message));
			return;
		}

		//Namespace
		NamespaceDomain namespaceDomain = (NamespaceDomain) sess.getAttribute("namespace");
		String adminAppId = (String)session.getAttribute("ADMIN_APP_ID");

		//Root Node
		out.println("<workflows>");

		//定義名称のあいまい検索を行うためV5APIを使用
		if (EIMThreadContext.getTransactionContext() != null) {
			EIMThreadContext.removeTransactionContext();
		}

		TransactionContext context = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
		EIMThreadContext.putTransactionContext(context);
		context.setLangId(sess.getLangId());
		context.setDBConnection(sess.getDBConnection());
		context.setUser(ConvertUtils.toUserDomain(loginUser));

		WorkflowService workflowService = (WorkflowService) ApplicationContextLoader.getApplicationContext().getBean("workflowService2");
		List<WorkflowDomain> workflowDomainList = new ArrayList<WorkflowDomain>();

		WorkflowCriteria workflowCriteria = new WorkflowCriteria();
		//最新のみ
		workflowCriteria.setLatest(true);

		if (namespaceDomain == null || namespaceDomain.getName().equals("")) {
			//汎用システム管理は全て出力
			workflowDomainList = workflowService.getList(workflowCriteria);
		} else {
			//汎用システム管理以外はネームスペースの前方一致で出力
			String defSearchName = NamespaceUtil.concatenate(namespaceDomain.getName(), null) + "*";
			workflowCriteria.setDefinitionName(defSearchName);
			workflowDomainList = workflowService.getList(workflowCriteria);
		}
		//他言語名称でソートを実装
		workflowDomainList = AppObjectUtil.getStrSortedList(workflowDomainList, "getName", true);

		// ドキュメント管理の表示対象外ネームスペースを取得
		boolean isDocument = AppConstant.ADMIN_APP_ID_DOCUMENT.equals(adminAppId);
		String excludeNameSpaceCSV = EIMConfig.get("NAME_SPACE_TO_EXCLUDE_IN_DOCUMENT");
		String[] excludeNameSpaceArray = excludeNameSpaceCSV.split(",");

		outerLoop:
			for (int i = 0; i < workflowDomainList.size(); i++) {
			//WorkFlow
			WorkflowDomain workFlowDomain = (WorkflowDomain) workflowDomainList.get(i);

			// ドキュメント管理の場合表示対象外ネームスペースのワークフローを除去する
			if (isDocument) {
				for (int j = 0; j < excludeNameSpaceArray.length; j++) {

					String excludeNameSpace = excludeNameSpaceArray[j].trim();
					if (workFlowDomain.getDefinitionName().indexOf(excludeNameSpace) == 0) {
						continue outerLoop;
					}
				}
			}

			//XML
			out.println("<workflow");
			if (namespaceDomain == null || namespaceDomain.getName().equals("")) {
				if (AppConstant.ADMIN_APP_ID_GENERAL.equals(adminAppId)) {
					//汎用システム管理は定義名をネームスペース付で表示
					out.println(" workFlowName=\""
							+ StringUtils.xmlEncode(NamespaceUtil.getDefNameWithNamespaceParentheses(workFlowDomain.getName(), workFlowDomain
									.getDefinitionName())) + "\"");
				} else {
					out.println(" workFlowName=\"" + StringUtils.xmlEncode(workFlowDomain.getName()) + "\"");
				}
			} else {
				out.println(" workFlowName=\"" + StringUtils.xmlEncode(workFlowDomain.getName()) + "\"");
			}

			out.println(" workFlowId=\"" + workFlowDomain.getId() + "\"");
			out.println(" workFlowRev=\"" + workFlowDomain.getRevision() + "\"");
			Date mdate = workFlowDomain.getModificationDate();
			out.println(" mdate=\"" + DateUtils.getDBTzToCLTzDate(sess, mdate, "EIM.FORMAT.DATETIME") + "\"");
			out.println(" islatest=\"" + "true" + "\"");
			out.println(">");
			out.println("</workflow>");
		}

		//End Root Node
		out.println("</workflows>");

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
