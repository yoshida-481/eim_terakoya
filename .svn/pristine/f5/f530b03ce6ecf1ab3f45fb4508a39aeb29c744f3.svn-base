<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "jp.co.ctc_g.eim.framework.common.exception.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>

<%
	Log log = LogFactory.getLog(this.getClass().getName());

	// ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	EIMSession sess = null;
	EIMUser user = null;

	// メッセージ用
	String message = null;
	Object[] paramId = null;

	try {
		// セッション取得
		sess = EIMUtils.getSession(request);
		if(sess == null)
		{
			message = EIMResource.getMessage(request, "EIM.ERROR.SESSIONTIMEOUT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.SESSIONTIMEOUT");
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}
		user = (EIMUser)sess.getAttribute("USER");

		out.println("<documentBaseUrlConf>");

		// ドキュメントのベースURL
		String documentUrl = StringUtils.xmlEncode(EIMConfig.getValue("DOCUMENT_URL") + EIMConfig.getValue("QUERY_STRING")) + "objId=";
		out.println("<documentBaseUrl>" + documentUrl + "</documentBaseUrl>");

		// 原本ドキュメントのベースURL
		String originalDocumentUrl = StringUtils.xmlEncode(EIMConfig.getValue("PRIVATE_LATEST_DOCUMENT_URL")) + "objId=";
		out.println("<originalDocumentBaseUrl>" + originalDocumentUrl + "</originalDocumentBaseUrl>");

		// 公開ドキュメントのベースURL
		String publicDocumentUrl = StringUtils.xmlEncode(EIMConfig.getValue("PUBLIC_LATEST_DOCUMENT_URL")) + "objId=";
		out.println("<publicDocumentBaseUrl>" + publicDocumentUrl + "</publicDocumentBaseUrl>");


		// ドキュメントアクセスURL出力有無追加
		String eimFilePath = EIMConfig.get("DOC_ACCESS_URL_PATH_INCLUDE");
		String originalFilePath = EIMConfig.get("ORG_DOC_ACCESS_URL_PATH_INCLUDE");
		String publicFilePath = EIMConfig.get("PUBLIC_DOC_ACCESS_URL_PATH_INCLUDE");

		out.println("<docAccessUrl>" + eimFilePath + "</docAccessUrl>");
		out.println("<originalDocAccessUrl>" + originalFilePath + "</originalDocAccessUrl>");
		out.println("<publicDocAccessUrl>" + publicFilePath + "</publicDocAccessUrl>");

		out.println("</documentBaseUrlConf>");

	} catch(EIMSysException eimse) {
		out.clear();
		message = EIMResource.getMessageValue(sess, "EIM.ERROR.SYSTEMERROR");
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.error(AppMessageUtils.makeLogMessage(user.getId(), eimse.getMessage(), paramId), eimse);
	} catch(EIMAppException eimae) {
		out.clear();
		message = eimae.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(user.getId(), eimae.getMessage(), paramId), eimae);
	} catch(EIMException eime) 	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(user.getId(), eime.getMessage(), paramId), eime);
	} catch(Exception e) {
		out.clear();
		message = EIMResource.getMessageValue(sess, "EIM.ERROR.SYSTEMERROR");
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
	} finally {
		try {
			if (sess != null) {
				sess.close();
			}
		} catch (Exception se) {
			out.clear();
			message = EIMResource.getMessageValue(sess, "EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
	}
%>