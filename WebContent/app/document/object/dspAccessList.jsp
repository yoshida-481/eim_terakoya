<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");
	
	//Session
	EIMSession sess = null;
	EIMUser user = null;
	
	//Parameter
	String prmObjId = request.getParameter("objId");

	//Message
	String message = null;
	Object[] paramId = {
			"objId=" + prmObjId
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
		
		user = (EIMUser)sess.getAttribute("USER");
		
		//Object
		EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		
		if(object == null)
		{
			// 指定のドキュメント、フォルダ、またはタグを取得できませんでした。
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOTEXIST.DOCFOLTAG");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOTEXIST.DOCFOLTAG");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}
		else if(!SecurityUtils.authorized(sess, object, sess.getUser(), AppConstant.ACCESS_ROLE_ALWAYS_READ))
		{
			message = EIMResource.getMessageValue(sess, "EIM.ERROR.LOGIC.NOACCESS");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessageValue("EIM.ERROR.LOGIC.NOACCESS");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}
		
		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
		
		// ユーザーが対象に対して公開読取権限しかなく、ステータスが公開済で無い場合
		if (helper.isReadOnlyAccess(object) 
				&& object.getStatus() != null 
				&& object.getStatus().getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_PUBLIC) 
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOTPUBLIC", new Object[]{StringUtils.xmlEncode(object.getName())});
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOTPUBLIC", new Object[]{StringUtils.xmlEncode(object.getName())});
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, new Object[]{paramId}));
			return;
		}
		
		//Root Node
		out.println("<accessList>");
		
		//Access List
		List accsList = AccessUtils.getAccessList(sess, object);
		for(int i = 0; i < accsList.size(); i++)
		{
			//Access
			EIMAccess access = (EIMAccess)accsList.get(i);
			
			//Date
			Date date = (Date)access.getDate();
			String accessDate = DateUtils.getDBTzToCLTzDate(sess, date, "EIM.FORMAT.DATETIME");
			
			//Action
			String action = access.getAction();
			
			//XML
			out.println("<access");
				out.println(" userName=\"" + StringUtils.xmlEncode(access.getUser().getName()) + "\"");
				out.println(" accessDate=\"" + StringUtils.xmlEncode(accessDate) + "\"");
				if(action.indexOf("|") == -1) {
					out.println(" action=\"" + StringUtils.xmlEncode(EIMResource.getMessage(sess, action)) + "\"");
				} else {
					String delimiter = EIMConfig.get("ACCESS_HISTORY_DELIMITER");
					String key = action.substring(0, action.indexOf(delimiter));
					String param = action.substring(action.indexOf(delimiter) + 1, action.length());
					String[] params = {param};
					out.println(" action=\"" + StringUtils.xmlEncode(EIMResource.getMessage(sess, key, params)) + "\"");
				}
				String userMail = "";
				if (access.getUser().getMail() != null) {
					userMail = access.getUser().getMail();
				}
				out.println(" userMail=\"" + StringUtils.xmlEncode(userMail) + "\"");
			out.println(">");
			out.println("</access>");
		}
		
		//End Root Node
		out.println("</accessList>");
	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(user.getId(), eime.getMessage(), paramId), eime);
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
