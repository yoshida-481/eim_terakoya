<%@ page contentType="text/xml; charset=UTF-8"%>

<%@ page import="eim.bo.*"%>
<%@ page import="eim.net.*"%>
<%@ page import="eim.util.*"%>

<%@ page import="org.apache.commons.logging.*"%>
<%@ page import="common.util.*"%>

<%@ page import="org.springframework.security.core.Authentication"%>
<%@ page import="org.springframework.security.core.context.SecurityContext"%>
<%@ page import="org.springframework.security.core.context.SecurityContextHolder"%>
<%@ page import="jp.co.ctc_g.eim.framework2.common.enumeration.SessionAttributeNameEnum"%>
<%@ page import="jp.co.ctc_g.eim.framework2.common.security.EIMUsernamePasswordAuthenticationToken"%>

<%
	// Error logging
	Log log = LogFactory.getLog(this.getClass().getName());

	// Content type
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	EIMUser user = null;
	EIMSession sess = null;

	// Parameter
	String prmNoSso = request.getParameter("noSso");
	String prmLangId = request.getParameter("langId");
	String prmUserTzOffset = request.getParameter("userTzOffset");

	// Message
	String message = null;
	Object[] paramId = {
			"noSso=" + prmNoSso,
			"langId=" + prmLangId,
			"userTzOffset=" + prmUserTzOffset
	};

	try {
		sess = EIMUtils.getSession(request);
		if (sess == null) {
			message = EIMResource.getMessage(request, "EIM.ERROR.SESSIONTIMEOUT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.SESSIONTIMEOUT");
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}

		user = (EIMUser) sess.getAttribute("USER");

		// セッション情報を取得
		boolean sso = "true".equalsIgnoreCase((String) session.getAttribute("sso"));
		String langId = (String) session.getAttribute(SessionAttributeNameEnum.LANG.getSymbol());

		// SSOログイン済かつnoSsoパラメータが有効な場合、ログアウトする
		if (sso && "true".equalsIgnoreCase(prmNoSso)) {
			try {
				message = EIMResource.getMessage(request, "EIM.ERROR.SESSIONTIMEOUT");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.SESSIONTIMEOUT");
				log.warn(AppMessageUtils.makeLogMessage(message));
				return;
			} finally {
				SecurityContextHolder.clearContext();
				session.invalidate();
			}
		}

		// クライアントタイムゾーンオフセットを更新
		session.setAttribute("clTzOffset", prmUserTzOffset);

		boolean updateLang = false;
		if (prmLangId != null && !prmLangId.equalsIgnoreCase(langId)) {
			// 言語IDを更新
			session.setAttribute(SessionAttributeNameEnum.LANG.getSymbol(), prmLangId);
			langId = prmLangId;
			updateLang = true;

			// セッションユーザを更新
			user = UserUtils.getUserById(sess, user.getId());
			session.setAttribute("USER", user);
		}

		// EIMANAGER独自認証か否かを取得
		boolean internalAuth = false;
		SecurityContext securityContext = (SecurityContext) session.getAttribute("SPRING_SECURITY_CONTEXT");
		if (securityContext != null) {
			Authentication authentication = securityContext.getAuthentication();
			if (authentication instanceof EIMUsernamePasswordAuthenticationToken) {
				internalAuth = true;
			}
		}

		// Set response body
		out.println("<result");
		out.println(" sso=\"" + Boolean.toString(sso) + "\"");
		out.println(" internalAuth=\"" + Boolean.toString(internalAuth) + "\"");
		out.println(" langId=\"" + StringUtils.xmlEncode(langId) + "\"");
		out.println(" updateLang=\"" + Boolean.toString(updateLang) + "\"");
		out.println(">");

		out.println("<user");
		out.println(" userId=\"" + user.getId() + "\"");
		out.println(" userCode=\"" + StringUtils.xmlEncode(user.getCode()) + "\"");
		out.println(" userName=\"" + StringUtils.xmlEncode(user.getName()) + "\"");
		out.println(" userKana=\"" + StringUtils.xmlEncode(user.getKana()) + "\"");
		out.println(" userMail=\"" + StringUtils.xmlEncode(user.getMail()) + "\"");
		out.println(" userAdmin=\"" + user.getAdmin() + "\"");
		out.println("/>");

		out.println("</result>");

		// Commit
		sess.commit();

	} catch (EIMException eime) {
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(user.getId(), eime.getMessage()), eime);
	} catch (Exception e) {
		out.clear();
		message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
	} finally {
		if (sess != null) {
			try {
				sess.close();
			} catch (Exception se) {
				out.clear();
				message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
			}
		}
	}
%>
