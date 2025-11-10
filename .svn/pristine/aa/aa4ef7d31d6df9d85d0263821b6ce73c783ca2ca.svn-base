<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>
<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>

<%@ page import = "jp.co.ctc_g.eim.framework.common.util.EIMThreadContext"%>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");
	
	//Session
	EIMSession sess = null;
	EIMUser user = null;
	boolean sessPutFlag = false;
	
	//Message
	String message = null;
	ArrayList paramIdList = new ArrayList();

	
	try
	{
		//Session
		sess = EIMUtils.getSession(request);
		if(sess == null) {
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
		
		StringBuffer messageParamsBuffer = new StringBuffer();

		// 属性コピー & 貼付けチェックボックス
		int returnSelectCheckBox = 0;
		String selectCheckBox = EIMConfig.get("ATTRIBUTE_COPY_PASTE_DEFAULT_CHECKBOX_SELECT");
		
		String upperSelectCheckBox = selectCheckBox.toUpperCase();
		
		if (upperSelectCheckBox.equals("ON")) {
			returnSelectCheckBox = 1;
		} else if (upperSelectCheckBox.equals("OFF")) {
		} else {						// 設定に誤りがある
			messageParamsBuffer.append("ATTRIBUTE_COPY_PASTE_DEFAULT_CHECKBOX_SELECT");
		}
		
		// 属性情報入力タブ表示エリアの非表示化
		int returnAttributeTabExpandable = 0;
		String attributeTabExpandable = EIMConfig.get("ATTRIBUTE_TAB_EXPANDABLE");
		
		String upperAttributeTabExpandable = attributeTabExpandable.toUpperCase();
		
		if (upperAttributeTabExpandable.equals("TRUE")) {
			returnAttributeTabExpandable = 1;
		} else if (upperAttributeTabExpandable.equals("FALSE")) {
		} else {						// 設定に誤りがある
			if (messageParamsBuffer.length() > 0)
				messageParamsBuffer.append(" | ");
			messageParamsBuffer.append("ATTRIBUTE_TAB_EXPANDABLE");
		}
		
		if (messageParamsBuffer.length() > 0) {
			// エラー出力
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.CONFIG.SETTING.ERROR", new Object[]{messageParamsBuffer.toString()});
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
		}
		else {
			// 結果出力
			out.println("<result>");
			out.println("<result");
			out.println(" selectCheckBox=\"" + String.valueOf(returnSelectCheckBox) + "\"");
			out.println(" attributeTabExpandable=\"" + String.valueOf(returnAttributeTabExpandable) + "\">");
			out.println("</result>");
			out.println("</result>");
		}
	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(eime.getMessage()));
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