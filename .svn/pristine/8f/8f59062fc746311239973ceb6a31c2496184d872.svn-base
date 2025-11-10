<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>
<%@ page import = "common.util.*" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.SecurityDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.OtherNameDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.criteria.SecurityCriteria"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.SecurityService"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.TransactionContext"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.exception.EIMException"%>
<%@ page import = "jp.co.ctc_g.eim.admin.business.domain.NamespaceDomain"%>

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
		
		
		//Root Node
		out.println("<secList>");
		
		// item0Flagがtrueの場合は0件返却
		if(!item0Flag) {			

			TransactionContext context = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
			EIMThreadContext.putTransactionContext(context);
			context.setLangId(sess.getLangId());
			context.setDBConnection(sess.getDBConnection());
			context.setUser(ConvertUtils.toUserDomain(loginUser));
			
			SecurityService securityService = (SecurityService)ApplicationContextLoader.getApplicationContext().getBean("securityServiceLight2");

			NamespaceDomain namespaceDomain = (NamespaceDomain)sess.getAttribute("namespace");
			//定義名称のあいまい検索を行うためV5APIを使用
			String newSecurityName = prmSecurityName.trim();
			String defSearchName = "";
			SecurityCriteria securityCriteria = new SecurityCriteria();
			
			// ドキュメント管理
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
			
			List<SecurityDomain> securityDomainList = securityService.getList(securityCriteria);
			
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

			// 多言語名称が存在しないセキュリティについては、定義名称を設定して表示する。
			if (securityDomainList != null) {
				// 多言語名称がnullなら定義名称をセット
				for (SecurityDomain security : securityDomainList) {
					if (security.getName() == null) {
						OtherNameDomain otherNameDomain = new OtherNameDomain();
						otherNameDomain.setLangId(context.getLangId());
						otherNameDomain.setName(security.getDefinitionName());
						security.getNameList().add(otherNameDomain);
					}
				}
			}
			
			//他言語名称でソートを実装
			securityDomainList = AppObjectUtil.getStrSortedList(securityDomainList, "getName", true);
			
			
			//Security List
			//デフォルトセキュリティのみ取得
			for(int i =0 ; i < securityDomainList.size() ; i++){
				//XML
				out.println("<security");
					out.println(" secId=\"" + securityDomainList.get(i).getId() + "\"");
					out.println(" secName=\"" + StringUtils.xmlEncode(securityDomainList.get(i).getName()) + "\"");
				out.println(">");
				out.println("</security>");
			}
		}
		
		//End Root Node
		out.println("</secList>");
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
