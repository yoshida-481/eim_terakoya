<%@ page contentType = "text/xml; charset=UTF-8" %>
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
	
	//Parameter
	String prmEntryId = request.getParameter("entryId");
	String prmStSecId = request.getParameter("stSecId");

	//Message
	String message = null;
	Object[] paramId = {
			"entryId=" + prmEntryId
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
		loginUser = (EIMUser)sess.getAttribute("USER");
		
		//セキュリティー→アクセスエントリー経由ロールを見る時、セキュリティー管理権限の認証が必要なしと判断されますので、
		//下記の認証をコメントアウトします by lin.chen at 2010/03/24
/*
		if(!AdminAuthUtil.hasSpecifiedOrGeneralAuth(loginUser, AppConstant.ADMIN_AUTH_ID_SECURITY))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.NOTADMINISTRATOR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.NOTADMINISTRATOR");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message));
			return;
		}
*/

		//Entry
		EIMAccessEntry entry = SecurityUtils.getAccessEntryById(sess, Long.parseLong(prmEntryId));
		if(entry == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOACCESSENTRY");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOACCESSENTRY");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}
		
		StringBuffer sb = new StringBuffer();
		
		// Root Node
		sb.append("<roleList>\n");
		// StatusSecurity
		EIMStatusSecurity stSec = null;
		// RoleList
		List defaultRoleList = null;
		
		// ステータス別セキュリティが選択されている場合
		if ( prmStSecId != null && !prmStSecId.equals("")) {
			stSec = StatusSecurityUtils.getStatusSecurityById(sess, Long.parseLong(prmStSecId));
			if(stSec == null)
			{
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.STATUSSEC.NOTFOUND");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.STATUSSEC.NOTFOUND");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				return;
			}
			
			
			defaultRoleList = SecurityUtils.getAccessRoleList(sess, entry, null);
			
			List stRoleList = SecurityUtils.getAccessRoleList(sess, entry, stSec);
			
			for (int i = 0; i<defaultRoleList.size(); i++) {
				EIMAccessRole dfRole = (EIMAccessRole)defaultRoleList.get(i);
				sb.append("	<role roleId=\"")
				  .append(dfRole.getId())
				  .append("\" name=\"")
				  .append(StringUtils.xmlEncode(dfRole.getType().getName()))
				  .append("\" dispName=\"")
				  .append(StringUtils.xmlEncode(dfRole.getType().getDispName()))
				  .append("\">\n		<stsec id=\"0\" permit=\"")
				  .append(dfRole.getPermit().getValue())
				  .append("\"/>\n");
				for (int j = 0; j<stRoleList.size(); j++) {
					EIMAccessRole stRole = (EIMAccessRole)stRoleList.get(j);
					EIMStatusSecurity tempStSec = stRole.getStatusSecurity();
					if (stRole.getId() == dfRole.getId() && tempStSec != null) {
						sb.append("		<stsec id=\"")
						  .append(Long.toString(tempStSec.getId()))
						  .append("\" permit=\"")
						  .append(stRole.getPermit().getValue())
						  .append("\"/>\n");
						
						break;
					}
				}
				sb.append("	</role>\n");
			}
			
		// デフォルトが選択されている場合
		} else {
			// ロールリストはデフォルトのみ
			defaultRoleList = SecurityUtils.getAccessRoleList(sess, entry, null);
			// アクセスロールの数
			for(int i = 0; i < defaultRoleList.size(); i++) {
				EIMAccessRole role = (EIMAccessRole)defaultRoleList.get(i);
				
				sb.append("	<role roleId=\"")
				  .append(role.getId())
				  .append("\" name=\"")
				  .append(StringUtils.xmlEncode(role.getType().getName()))
				  .append("\" dispName=\"")
				  .append(StringUtils.xmlEncode(role.getType().getDispName()))
				  .append("\">\n		<stsec id=\"0\" permit=\"")
				  .append(role.getPermit().getValue())
				  .append("\"/>\n	</role>");
			}
		}

		//End Root Node
		sb.append("</roleList>\n");
		
		// XML
		String xmlString = sb.toString();
		
		// 出力
		out.print(xmlString);
		
	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), eime.getMessage(), paramId), eime);
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
