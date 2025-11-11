<%@ page contentType="text/xml; charset=UTF-8"%>

<%@ page import="eim.bo.*"%>
<%@ page import="eim.net.*"%>
<%@ page import="eim.util.*"%>

<%@ page import="org.apache.commons.logging.*"%>
<%@ page import="common.util.*"%>

<%
	// Error logging
	Log log = LogFactory.getLog(this.getClass().getName());

	// Content type
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	EIMUser user = null;
	EIMSession sess = null;

	// Message
	String message = null;
	Object[] paramId = {};

	try {
		sess = EIMUtils.getSession(request);
		if (sess == null) {
			message = EIMResource.getMessage(request, "EIM.ERROR.SESSIONTIMEOUT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.SESSIONTIMEOUT");
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}

		// Remove session attribute
		session.removeAttribute("sso");

		// Admin
		user = (EIMUser) sess.getAttribute("USER");
		String adminAppId = (String) session.getAttribute("ADMIN_APP_ID");
		if (adminAppId == null || !EIMXmlConfigAdminAuth.hasAnyAuthInSpecifiedAdminApp(user, adminAppId)) {
			// 管理者権限がありません。
			message = EIMResource.getMessage(sess, "EIM.ERROR.NOTADMINISTRATOR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.NOTADMINISTRATOR");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			session.removeAttribute("USER");
			return;
		}

		// Set response body
		out.println("<result/>");

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
