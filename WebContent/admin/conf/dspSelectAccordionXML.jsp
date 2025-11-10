<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	// ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	// Message
	String message = null;

	try
	{
		String selectAccordion = EIMConfig.get("DISPLAY_ACCORDION_NUM");

		// プロパティファイルの値が0,1の場合
		if(selectAccordion.equals("0" ) || selectAccordion.equals("1" ) ) {
			out.println("<accordion");
			out.println(" selectAccordion=\"" + selectAccordion + "\"");
			 out.println(">");
			out.println("</accordion>");
		} else {
			// プロパティファイルの値が0,1以外の場合は、グループ検索アコーディオンを非表示に設定
			out.println("<accordion");
			out.println(" selectAccordion=\"" + "1" + "\"");
			out.println(">");
			out.println("</accordion>");
		}
	}
	catch(Exception e)
	{
		out.clear();
		message = EIMResource.getMessage("EIM.ERROR.SYSTEMERROR");
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
	}
%>
