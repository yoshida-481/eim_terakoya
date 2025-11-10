<%@ page contentType="image/jpeg; charset=UTF-8"%>
<%@ page import="eim.util.*"%>
<%@ page import="eim.bo.*"%>

<%@ page import="java.io.*"%>
<%@ page import="org.apache.commons.logging.*"%>
<%@ page import="common.util.*"%>


<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	// Parameter
	String screenType = request.getParameter("screenType");
	
	// Message
	String message = null;
	
	// Image File Name
	String logoImageFileName = null;
			
	File file = null;	
	
	try {		
		
		// Image File Name - read from config file			
		if (screenType.equals(AppConstant.SCREEN_TYPE_LOGIN)) {
			logoImageFileName = EIMConfig.get("APP_LOGIN_LOGO_FILE");
		} else if (screenType.equals(AppConstant.SCREEN_TYPE_TOP)) {
			logoImageFileName = EIMConfig.get("APP_TOP_LOGO_FILE");
		}
	    
		// config.propertiesにイメージファイルが指定されていない時
		if (logoImageFileName.trim().equals("")) {
			out.println("<logoImage");
			out.println(" name=\"\"");
			out.println(">");
			out.println("</logoImage>");			
			return;
		}
	
		// path
		String imageFilePath = application.getRealPath("logo/" + logoImageFileName);		
		
		file = new File(imageFilePath);
		
		if (!file.exists()) {
			// エラー：config.propertiesに設定されたイメージファイルが存在しない時
			message = EIMResource.getMessage("EIM.ERROR.LOGO.NOLOGOIMAGEFILE");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGO.NOLOGOIMAGEFILE");
			log.error(AppMessageUtils.makeLogMessage(message));
			return;
		} else {
			out.println("<logoImage");
			out.println(" name=\"" + logoImageFileName + "\"");
			out.println(">");
			out.println("</logoImage>");			
		}
		
	} catch (Exception e) {
		e.getStackTrace();
	}

%>
