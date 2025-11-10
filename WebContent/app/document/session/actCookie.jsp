<%@ page contentType = "text/html; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.AppMessageUtils" %>

<%

	/**
	 * Cookie情報：言語ID　取得処理
	 * ※ Cookie情報に言語IDが存在しない場合は、空文字を返す
	 */
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	String message = null;
	String langId = null;
	
	try{
		// Cookie情報(配列)を取得
		Cookie cookies[] = request.getCookies();
	 
		// 言語IDを抽出
		Cookie cookie = null;
		if(cookies != null) {
			for(int i = 0; i < cookies.length; i++) {
				if(cookies[i].getName().equals(EIMConfig.get("COOKIE_LANG_ID"))) {
					cookie = cookies[i];
		        }
		    }
		}
		
		StringBuffer sb = new StringBuffer(" ");
		sb.append(EIMConfig.get("COOKIE_LANG_ID"));
		sb.append("=\"");
		if(cookie != null){
			langId = cookie.getValue();
			sb.append(langId);
		}
		sb.append("\"");
		
		out.println("<user");
			out.println(sb.toString());
			out.println(">");
		out.println("</user>");
		
	}
	catch(Exception e)
	{
		out.clear();
		message = EIMResource.getMessage(langId, "EIM.ERROR.SYSTEMERROR");
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
	}
	
%>