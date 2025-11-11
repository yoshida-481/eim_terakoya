<%@ page contentType = "text/xml; charset=UTF-8" %>

<%@page import="jp.co.ctc_g.eim.framework2.business.domain.entity.OtherNameDomain"%>
<%@page import="jp.co.ctc_g.eim.app.form.business.domain.UIControlConfDomain"%>
<%@page import="jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader"%>
<%@page import="jp.co.ctc_g.eim.framework2.common.exception.EIMException"%>
<%@page import="jp.co.ctc_g.eim.framework2.common.util.ConfigUtils"%>
<%@page import="jp.co.ctc_g.eim.app.form.business.service.UIControlConfService"%>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>
<%@ page import = "common.util.*" %>

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

	//Message
	String message = null;

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
		if(!AdminAuthUtil.hasAnyAuth(loginUser))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.NOTADMINISTRATOR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.NOTADMINISTRATOR");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message));
			return;
		}
		
		//LangId
		String langId = sess.getLangId();
		
		/*
		 * Get UIControlList
		 */
		 
		// Root Node start
		out.println("<result>");
		
		
		// dataTypeList start
		out.println("<dataTypeList>");
		
		UIControlConfService uicontrolConfService = 
			(UIControlConfService)ApplicationContextLoader.getApplicationContext()
			.getBean("uIControlConfService");
		
		List<UIControlConfDomain> uiControlConfList = uicontrolConfService.getList();
		
		// 帳票管理用の場合
		String appId = (String)sess.getAttribute("ADMIN_APP_ID");
		if (appId.equals(AppConstant.ADMIN_APP_ID_FORM)) {
			List<UIControlConfDomain> remakeUiControlConfList = new ArrayList<UIControlConfDomain>();
			for (UIControlConfDomain uiControlConf : uiControlConfList) {
				// UIコントロール：ドキュメント検索 は使用しない
				if (uiControlConf.getBeanId().equals(ConfigUtils.getByKey("UI_CONTROL_ID_RELATED_DOCUMEMT"))) {
					continue;
				}
				remakeUiControlConfList.add(uiControlConf);
			}
			uiControlConfList = remakeUiControlConfList;
		}

		Map<String, List<UIControlConfDomain>> dataTypeListMap = UIControlUtil.getListDividedIntoValueTypeAndMultiple(uiControlConfList);
		
		Set<String> dataTypeNameAndMultipleSet = dataTypeListMap.keySet();
		for(String dataTypeNameAndMultipleStr : dataTypeNameAndMultipleSet){
			
			// "_" 区切り文字によって文字列を分割
			String[] dataTypeNameAndMultipleStrs = dataTypeNameAndMultipleStr.split(AppConstant.DELIMITER_UNDER_SCORE);
			String type = dataTypeNameAndMultipleStrs[0];
			String multiple = dataTypeNameAndMultipleStrs[1];
			
			// item(outer) start
			out.println("<item");
				out.println(" type=\"" + type + "\"");
			out.println(">");
			
			// SingleList/MultipleList start
			if(multiple.equals(AppConstant.UICONTROL_SINGLE)){
				out.println("<singleList>");
			} else {
				out.println("<multipleList>");
			}
			
			// UIControlList start
			out.println("<uIControlList>");
			
			List<UIControlConfDomain> list = dataTypeListMap.get(dataTypeNameAndMultipleStr);
			for(UIControlConfDomain uIControlConfDomain : list){
				
				String name = "";
				List<OtherNameDomain> nameList = uIControlConfDomain.getNameList();
				for(OtherNameDomain otherNameDomain : nameList){
					if(otherNameDomain.getLangId().equals(langId)){
						// ログイン言語と一致する場合、表示名として設定
						name = otherNameDomain.getName();
						break;
					}
				}
				
				// item(inner)
				out.println("<item");
					out.println(" id=\"" + StringUtils.xmlEncode(uIControlConfDomain.getBeanId()) + "\"");
					out.println(" type=\"" + StringUtils.xmlEncode(uIControlConfDomain.getTypeEnum().getName()) + "\"");
					out.println(" name=\"" + StringUtils.xmlEncode(name) + "\"");
					out.println(" refmasterTypeName=\"" + 
							StringUtils.xmlEncode(uIControlConfDomain.getRefMasterObjectTypeName()) + "\"");
				out.println("/>");
			}
			
			// UIControlList end
			out.println("</uIControlList>");
			
			// SingleList/MultipleList end
			if(multiple.equals(AppConstant.UICONTROL_SINGLE)){
				out.println("</singleList>");
			} else {
				out.println("</multipleList>");
			}
			
			// item(outer) end
			out.println("</item>");
		}
		
		// dataTypeList end
		out.println("</dataTypeList>");
		
		// Root Node end
		out.println("</result>");
		
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
