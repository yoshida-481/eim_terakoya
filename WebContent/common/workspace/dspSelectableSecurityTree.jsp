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
	String prmWsId = request.getParameter("wsId");
	
	//Message
	String message = null;
	Object[] paramId = {
			"wsId=" + prmWsId
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
		
		//WorkSpace
		EIMObject wsObj = null;
		if(prmWsId != null && !prmWsId.equals(""))
		{
			wsObj = ObjectUtils.getObjectById(sess, Long.parseLong(prmWsId));
			if(wsObj == null){
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOWORKSPACE");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOWORKSPACE");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				return;
			}
		}
		
		//Root Node
		out.println("<result>");
		
		//************** 「使用可能セキュリティ」の取得 **************
		boolean isAllSec = false;
		
		//使用可能制限フラグ取得
		long secLimitFlag = AppObjectUtil.getIntAttr(sess, wsObj, EIMConfig.get("ATTR_NAME_WORKSPACE_LIMIT_SECURITY_FLAG"), Integer.MIN_VALUE);
		if(secLimitFlag == Integer.MIN_VALUE || secLimitFlag == 0)
		{
			//属性なしor「全て」選択
			isAllSec = true;
			
		}else{
			isAllSec = false;
		}
		
		//XML
		out.println("<secList");
			out.println(" isAllSec=\"" + isAllSec + "\"");
		out.println(">");
		
		//使用可能セキュリティID配列の取得
		long[] secIds = AppObjectUtil.getIntAttrs(sess, wsObj, EIMConfig.get("ATTR_NAME_WORKSPACE_SELECTABLE_SECURITY"));
		List secList = new ArrayList();
		if(secIds != null)
		{
			// 使用可能セキュリティに設定できるセキュリティは、デフォルトセキュリティのみ
			for(int i=0; i < secIds.length; i++){
				EIMSecurity sec = SecurityUtils.getSecurityById(sess, secIds[i]);
				if(sec == null){
					// 「削除されたセキュリティ」として表示
					EIMSecurity deletedSec = new EIMSecurity(-1, EIMResource.getMessage(sess, "EIM.LABEL.WORKSPACE.DELETED.SECURITY"), "");
					secList.add(deletedSec);
				}else{
					secList.add(sec);
				}
			}
			
			// 取得したセキュリティ一覧をソートして出力
			secList = AppObjectUtil.getStrSortedList(secList, "getName", true);
			for(int i=0; i < secList.size(); i++){
				EIMSecurity sec = (EIMSecurity)secList.get(i);
				//XML
				out.println("<security");
					out.println(" secId=\"" + sec.getId() + "\"");
					out.println(" secName=\"" + StringUtils.xmlEncode(sec.getName()) + "\"");
				out.println(" />");
			}
		}
		
		out.println("</secList>");
		
		//End Root Node
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
