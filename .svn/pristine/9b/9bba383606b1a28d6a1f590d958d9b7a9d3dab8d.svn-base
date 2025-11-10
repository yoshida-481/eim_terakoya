<%@ page contentType="text/xml; charset=UTF-8"%>

<%@ page import="eim.bo.*"%>
<%@ page import="eim.net.*"%>
<%@ page import="eim.util.*"%>

<%@ page import="org.apache.commons.logging.*"%>
<%@ page import="common.util.*"%>

<%@ page import="jp.co.ctc_g.eim.framework2.common.enumeration.SessionAttributeNameEnum"%>

<%
	// Error logging
	Log log = LogFactory.getLog(this.getClass().getName());

	// Content type
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	// Parameter
	String prmLangId = request.getParameter("langId");

	// Session
	EIMSession sess = null;
	EIMUser user = null;

	// Message
	String message = null;
	Object[] paramId = { "langId=" + prmLangId };

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
		String langId = (String) session.getAttribute(SessionAttributeNameEnum.LANG.getSymbol());

		if (prmLangId != null && !prmLangId.equalsIgnoreCase(langId)) {
			// 言語IDを更新
			session.setAttribute(SessionAttributeNameEnum.LANG.getSymbol(), prmLangId);

			// セッションユーザを更新
			user = UserUtils.getUserById(sess, user.getId());
			session.setAttribute("USER", user);
		}

		out.println("<user");
		out.println(" userId=\"" + user.getId() + "\"");
		out.println(" userCode=\"" + StringUtils.xmlEncode(user.getCode()) + "\"");
		out.println(" userName=\"" + StringUtils.xmlEncode(user.getName()) + "\"");
		out.println(" userKana=\"" + StringUtils.xmlEncode(user.getKana()) + "\"");
		out.println(" userMail=\"" + StringUtils.xmlEncode(user.getMail()) + "\"");
		out.println(" userAdmin=\"" + user.getAdmin() + "\"");
		out.println("/>");

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
