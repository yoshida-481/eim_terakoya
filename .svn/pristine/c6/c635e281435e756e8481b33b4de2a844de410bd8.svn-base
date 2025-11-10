<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>
<%@ page import = "common.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.SecurityDomain" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.StatusSecurityDomain" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.WorkflowDomain" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.SecurityService" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.TransactionContext" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.exception.EIMException"%>
<%@ page import = "jp.co.ctc_g.eim.admin.business.domain.NamespaceDomain" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.criteria.SecurityCriteria" %>
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

	//Parameter
	String prmInitFlag = EIMUtils.getParameter(request, "initFlag");
	String prmSecurityName = EIMUtils.getParameter(request, "securityName");

	//Message
	String message = null;
	Object[] paramId = {
			"initFlag=" + prmInitFlag,
			"securityName=" + prmSecurityName
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
		loginUser = (EIMUser)sess.getAttribute("USER");
		if(!AdminAuthUtil.hasSpecifiedOrGeneralAuth(loginUser, AppConstant.ADMIN_AUTH_ID_SECURUTY_ENTRY) &&
				!AdminAuthUtil.hasSpecifiedOrGeneralAuth(loginUser, AppConstant.ADMIN_AUTH_ID_SECURITY))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.NOTADMINISTRATOR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.NOTADMINISTRATOR");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}


		boolean item0Flag = false;
		int searchLimitMax = -1;
		// 次の条件を全て満たす場合に、セキュリティの空リストを返却する。
		// ・SECURITY_DISPLAY_POLICYが"SEARCH"
		// ・prmInitFlagが"true"
		// ・prmSecurityNameが空の場合
		String securityDisplayParam = EIMConfig.get("SECURITY_DISPLAY_POLICY");
		if(securityDisplayParam!=null && securityDisplayParam.equals("ALL")) {
			// 全件表示
		} else if(securityDisplayParam!=null && securityDisplayParam.equals("SEARCH")) {
			// 検索時表示
			if(prmInitFlag.equals("true")){
				item0Flag = true;
			}
		} else {
			// エラー
			message = EIMResource.getMessageValue(sess, "EIM.ERROR.CONFIG.SETTING.ERROR", new Object[]{"SECURITY_DISPLAY_POLICY"});
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessageValue("EIM.ERROR.CONFIG.SETTING.ERROR", new Object[]{"SECURITY_DISPLAY_POLICY"});
			log.error(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}

		// 検索結果上限を取得する
		String securityLimitParam = EIMConfig.get("SEARCH_SECURITY_RESULT_MAX");
		if(securityLimitParam!=null){
			searchLimitMax = Integer.parseInt(securityLimitParam);
		} else {
			// エラー
			message = EIMResource.getMessageValue(sess, "EIM.ERROR.CONFIG.SETTING.ERROR", new Object[]{"SEARCH_SECURITY_RESULT_MAX"});
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessageValue("EIM.ERROR.CONFIG.SETTING.ERROR", new Object[]{"SEARCH_SECURITY_RESULT_MAX"});
			log.error(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}

		StringBuffer sb = new StringBuffer();

		//Root Node
		sb.append("<secList>\n");

		// item0Flagがtrueの場合は0件返却
		if(!item0Flag) {

			String adminAppId = (String)session.getAttribute("ADMIN_APP_ID");

			boolean isDocument = AppConstant.ADMIN_APP_ID_DOCUMENT.equals(adminAppId);
			boolean isGeneral = AppConstant.ADMIN_APP_ID_GENERAL.equals(adminAppId);

			//定義情報のあいまい検索を行うためV5APIを使用
			NamespaceDomain namespaceDomain = (NamespaceDomain)sess.getAttribute("namespace");

			if(EIMThreadContext.getTransactionContext() != null)
			{
				EIMThreadContext.removeTransactionContext();
			}

			TransactionContext context = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
			EIMThreadContext.putTransactionContext(context);
			context.setLangId(sess.getLangId());
			context.setDBConnection(sess.getDBConnection());
			context.setUser(ConvertUtils.toUserDomain(loginUser));

			SecurityService securityService = null;
			if (isDocument) {
				// ドキュメント管理の場合、ステータス別セキュリティは取得しない
				securityService = (SecurityService)ApplicationContextLoader.getApplicationContext().getBean("securityServiceLight2");
			} else {
				securityService = (SecurityService)ApplicationContextLoader.getApplicationContext().getBean("securityServiceForStatusSecurityTree");
			}

			//定義(セッション)名称のあいまい検索を行うためV5APIを使用
			String newSecurityName = prmSecurityName.trim();
			String defSearchName = "";
			SecurityCriteria securityCriteria = new SecurityCriteria();

			if (isGeneral) {
				// 定義名称での検索
				// 部分一致の為、前後に*追加
				defSearchName = "*";
				if(StringUtils.isBlank(newSecurityName) != true )
				{
					defSearchName += newSecurityName + "*";
				}
				securityCriteria.setDefinitionName(defSearchName);
			} else {
				// ドキュメント管理または帳票管理
				// ネームスペース＆名称を検索条件とする（ネームスペース、名称のあいまい検索を行うためV5APIを使用）
				String namespaceStr = (String)sess.getAttribute("ADMIN_NAMESPACE");
				defSearchName = NamespaceUtil.concatenate(namespaceStr, null) + "*";

				if(newSecurityName != null && !newSecurityName.equals(""))
				{
					// 部分一致の為、*追加
					newSecurityName = "*" + newSecurityName + "*";
				}
				securityCriteria.setDefinitionName(defSearchName);
				securityCriteria.setName(newSecurityName);
			}

			// セキュリティ一覧取得
			List<SecurityDomain> securityDomainList = securityService.getList(securityCriteria);

			// ドキュメント管理の場合はネームスペース付きを除外する
			if (isDocument) {

				String excludeNameSpaceCSV = EIMConfig.get("NAME_SPACE_TO_EXCLUDE_IN_DOCUMENT");
				if (excludeNameSpaceCSV != null) {
					String[] excludeNameSpaceArray = excludeNameSpaceCSV.split(",");

					List<SecurityDomain> filterdSecurityDomainList = new ArrayList<>();
					for (SecurityDomain securityDomain : securityDomainList) {

						boolean isDocumentNameSpace = true;
						for (int i = 0; i < excludeNameSpaceArray.length; i++) {

							String excludeNameSpace = excludeNameSpaceArray[i].trim();
							if (securityDomain.getDefinitionName().indexOf(excludeNameSpace) == 0) {
								isDocumentNameSpace = false;
							}
						}
						if (isDocumentNameSpace) {
							filterdSecurityDomainList.add(securityDomain);
						}
					}
					securityDomainList = filterdSecurityDomainList;
				}
			}

			// 検索結果が0件の時
			if(!(prmInitFlag.equals("true")) && securityDomainList.isEmpty())
			{
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NORESULT");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				return;
			}

			if(searchLimitMax!=-1 && (securityDomainList.size() > searchLimitMax) ) {
				// セキュリティの数がオーバしていた場合エラーにする。
				// エラー
				out.clear();
				message = EIMResource.getMessageValue(sess, "EIM.ERROR.LOGIC.SEARCH.RESULT.LIMIT.OVER", new Object[]{securityDomainList.size(),searchLimitMax});
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessageValue("EIM.ERROR.LOGIC.SEARCH.RESULT.LIMIT.OVER", new Object[]{securityDomainList.size(),searchLimitMax});
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				return;
			}

			// ドキュメント管理の場合、ステータス別セキュリティは取得しない
			if (isDocument) {

				//他言語名称でソートを実装
				securityDomainList = AppObjectUtil.getStrSortedList(securityDomainList, "getName", true);

				for(int i =0 ; i < securityDomainList.size() ; i++){

					// デフォルトセキュリティのノード作成
					sb.append("	<security")
					  .append(" label=\"")
					  .append(StringUtils.xmlEncode(securityDomainList.get(i).getName()))
					  .append("\"")
					  .append(" isBranch=\"");

					sb.append("false\"");

					sb.append(" secId=\"")
					  .append(securityDomainList.get(i).getId())
					  .append("\"")
					  .append("/>\n");
				}
			}

			// 汎用システム管理、帳票用システム管理の場合、ステータス別セキュリティも取得する
			else {
				//Security List
				//ネームスペースの前方一致で一覧取得
				List secList = null;
				if(!isGeneral)
				{
					//帳票は他言語名称でソートを実装
					securityDomainList = AppObjectUtil.getStrSortedList(securityDomainList, "getName", true);
				}

				//WorkFlow
				WorkflowDomain workflow = null;

				// ステータスセキュリティに紐付いたワークフロー
				WorkflowDomain tempWorkFlow = null;

				// デフォルトセキュリティの数
				for(int i = 0; i < securityDomainList.size(); i++)
				{
					//Security
					SecurityDomain sec = securityDomainList.get(i);

					// ステータスセキュリティリスト
					List<StatusSecurityDomain> stSecList = sec.getStatusSecurityList();

					String secLabelName = null;
					if (namespaceDomain == null || namespaceDomain.getName().equals(""))
					{
						// 汎用システム管理は定義名をネームスペース付で表示
						secLabelName = NamespaceUtil.getDefNameWithNamespaceParentheses(sec.getName(), sec.getDefinitionName());
					}
					else
					{
						secLabelName = sec.getName();
					}

					// デフォルトセキュリティのノード作成
					sb.append("	<security")
					  .append(" label=\"")
					  .append(StringUtils.xmlEncode(secLabelName))
					  .append("\"")
					  .append(" isBranch=\"");

					// isBranchの為に子供の存在判定
					if (0 < stSecList.size()) {
						sb.append("true\"");
					} else {
						sb.append("false\"");
					}

					sb.append(" secId=\"")
					  .append(sec.getId())
					  .append("\"")
					  .append(">\n");

					// ステータスセキュリティの数
					for (int j=0; j<stSecList.size(); j++) {
						StatusSecurityDomain stSec = (StatusSecurityDomain)stSecList.get(j);
						tempWorkFlow = stSec.getWorkflow();

						String wfLabelName = null;

						// ワークフローがヌルだったりワークフローIDが前回と同じだったら
						if (workflow == null) {
							// 現在のワークフローを入れ替え
							workflow = tempWorkFlow;
							if (namespaceDomain == null || namespaceDomain.getName().equals(""))
							{
								// 汎用システム管理は定義名をネームスペース付で表示
								wfLabelName = NamespaceUtil.getDefNameWithNamespaceParentheses(workflow.getName(), workflow.getDefinitionName());
							}
							else
							{
								wfLabelName = workflow.getName();
							}
							// ワークフロー部のノード作成
							sb.append("		<workflow id=\"")
							  .append(workflow.getId())
							  .append("\" label=\"")
							  .append(StringUtils.xmlEncode(wfLabelName))
							  .append("\" isBranch=\"true\">\n");
						// 前回と同じワークフローだったら
						} else if (workflow.getId() != tempWorkFlow.getId()) {
							// 現在のワークフローを入れ替え
							workflow = tempWorkFlow;
							if (namespaceDomain == null || namespaceDomain.getName().equals(""))
							{
								// 汎用システム管理は定義名をネームスペース付で表示
								wfLabelName = NamespaceUtil.getDefNameWithNamespaceParentheses(workflow.getName(), workflow.getDefinitionName());
							}
							else
							{
								wfLabelName = workflow.getName();
							}
							// ワークフロー部のノード作成
							sb.append("		</workflow>\n")
							  .append("		<workflow id=\"")
							  .append(workflow.getId())
							  .append("\" label=\"")
							  .append(StringUtils.xmlEncode(wfLabelName))
							  .append("\" isBranch=\"true\">\n");
						}
						// ステータスセキュリティ部のノード作成
						sb.append("			<statusSecurity label=\"")
						  .append(StringUtils.xmlEncode(stSec.getStatusType().getName()))
						  .append("\" secId=\"")
						  .append(stSec.getId())
						  .append("\" statusTypeId=\"")
						  .append(stSec.getStatusType().getId())
						  .append("\" isBranch=\"false\"/>\n");
					}
					if (0 < stSecList.size()) {
						sb.append("		</workflow>\n")
						  .append("	</security>\n");
						workflow = null;
					} else {
						sb.insert(sb.length()-2, "/");
					}
				}
			}
		}

		//End Root Node
		sb.append("</secList>\n");
		//output XML
		out.println(sb.toString());

	}
	catch(EIMException eime)
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
