<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>
<%@ page import = "jp.co.ctc_g.eim.admin.business.domain.NamespaceDomain" %>

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

		//User
		user = (EIMUser)sess.getAttribute("USER");

		//Namespace
		NamespaceDomain namespaceDomain = (NamespaceDomain)sess.getAttribute("namespace");
		// システム管理のアプリケーション種別を取得
		String appId = (String)session.getAttribute("ADMIN_APP_ID");
		boolean isForm = AppConstant.ADMIN_APP_ID_FORM.equals(appId);
		boolean isGeneral = AppConstant.ADMIN_APP_ID_GENERAL.equals(appId);
		
		if(!isForm)
		{
			//ObjectType
			EIMObjectType objType = ObjectUtils.getObjectTypeById(sess, Long.parseLong(prmObjId));
	
			//選択ドキュメントタイプが存在しない場合
			if(objType == null)
			{
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODOCUMENTTYPE");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODOCUMENTTYPE");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
				return;
			}
	
			out.println("<objType");
			out.println(" label=\"" + StringUtils.xmlEncode(objType.getName()) + "\"");
			out.println(" objTypeId=\"" + objType.getId() + "\"");
			if (namespaceDomain == null || namespaceDomain.getName().equals(""))
			{
				// 汎用システム管理は定義名をネームスペース付で表示
				out.println(" objTypeName=\"" + StringUtils.xmlEncode(NamespaceUtil.getDefNameWithNamespaceParentheses(objType.getName(), objType.getDefName())) + "\"");
			}
			else
			{
				out.println(" objTypeName=\"" + StringUtils.xmlEncode(objType.getName()) + "\"");
			}
	
			long secId = 0;
			String secName = "";
			String secDefName = "";
	
			if (objType.getSecurity() != null)
			{
				EIMSecurity sec = SecurityUtils.getSecurityById(sess, objType.getSecurityId());
	
				secId = sec.getId();
				secName = sec.getName();
				secDefName = sec.getDefName();
			}
			//String adminAppId = (String)session.getAttribute("ADMIN_APP_ID");
			//boolean isGeneral = AppConstant.ADMIN_APP_ID_GENERAL.equals(adminAppId);
			
			out.println(" secId=\"" + secId + "\"");
			if (isGeneral)
			{
				// 汎用システム管理は定義名をネームスペース付で表示
				out.println(" secName=\"" + StringUtils.xmlEncode(NamespaceUtil.getDefNameWithNamespaceParentheses(secName, secDefName)) + "\"");
			}
			else
			{
				out.println(" secName=\"" + StringUtils.xmlEncode(secName) + "\"");
			}
			out.println(">");
			out.println("</objType>");
		}
		else
		{
			
			// システム管理（帳票）の場合、objectを取得する
			EIMObject obj = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
			if(obj == null)
			{
				// エラーメッセージ表示
				message = EIMResource.getMessage(sess, "EIM.ERROR.OBJECT.NOTFOUND");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.OBJECT.NOTFOUND");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message));
				return;
			}
			
			long secId = 0;
			String secName = "";
			String secDefName = "";

			if (obj.getSecurity() != null)
			{
				EIMSecurity sec = SecurityUtils.getSecurityById(sess, obj.getSecurityId());
				secId = sec.getId();
				secName = sec.getName();
				secDefName = sec.getDefName();
			}

			out.println("<obj");
			out.println(" label=\"" + StringUtils.xmlEncode(obj.getName()) + "\"");
			out.println(" secId=\"" + secId + "\"");
			out.println(" secName=\"" + StringUtils.xmlEncode(secName) + "\"");
			out.println(">");
			out.println("</obj>");
		}
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