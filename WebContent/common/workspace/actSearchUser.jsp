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
	String prmUserCode = EIMUtils.getParameter(request, "userCode");
	String prmUserName = EIMUtils.getParameter(request, "userName");
	String prmUserKana = EIMUtils.getParameter(request, "userKana");
	String prmOpeName = EIMUtils.getParameter(request, "opeName");
	String prmIsNotDisplayInvalidityUser = EIMUtils.getParameter(request, "isNotDisplayInvalidityUser");
	
	//Message
	String message = null;
	Object[] paramId = {
			"userCode=" + prmUserCode,
			"userName=" + prmUserName,
			"userKana=" + prmUserKana,
			"opeName=" + prmOpeName,
			"isNotDisplayInvalidityUser" + prmIsNotDisplayInvalidityUser
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

		List userList = null;
		
		if (prmIsNotDisplayInvalidityUser == null)
		{
			//Search User
			//全ユーザを検索対象とする
			userList = UserUtils.searchUser(	sess,
												prmUserCode,
												prmUserName,
												prmUserKana,
												null,
												false);
		}
		//エントリー参照画面からコールされた場合
		else
		{
			//Search User
			//有効ユーザのみ検索対象とする
			userList = UserUtils.searchUser(	sess,
												prmUserCode,
												prmUserName,
												prmUserKana,
												null,
												0,
												false);			
		}
		
		// 検索結果が0件の時
		if(userList.size() == 0)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NORESULT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
		}
		
		// 検索結果が1000件より大きい時
		if(userList.size() > Integer.parseInt(EIMConfig.get("GET_DATA_MAX_NUM")))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.OVERFLOWRESULT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
		}
		
		// ユーザのリストをuserCodeでソートする
		List sortedUserList = AppObjectUtil.getStrSortedList(userList, "getCode", true);
		
		//Root Node
		out.println("<users>");
		
		for(int i = 0; i < sortedUserList.size(); i++)
		{
			//User
			EIMUser user = (EIMUser)sortedUserList.get(i);
			
			//Check For System
			if(user.getId() == 1 && loginUser.getId() != 1)
			{
				if(prmOpeName != null && prmOpeName.equals("OPE_HIST"))
				{
					// 履歴検索用のユーザリスト取得の場合
					// ユーザにかかわらずSystemユーザを返す
				}
				else
				{
					continue;
				}
			}
			
			//かな
			String userKana = "";
			if(user.getKana() != null)
			{
				userKana = user.getKana();
			}
			
			//Mail
			String userMail = "";
			if(user.getMail() != null)
			{
				userMail = user.getMail();
			}
			
			//所属グループ名称の取得
			String groupName = AppUserUtil.getBelongGroupName(sess, user);
			
			//所属ロール名称の取得
			String roleName = AppUserUtil.getBelongRoleName(sess, user);

			String disable = "";
			if (user.getDisable() == 1)
			{
				disable = "on";
			}
			else
			{
				disable = "off";				
			}
			
			//XML
			out.println("<user");
				out.println(" userId=\"" + user.getId() + "\"");
				out.println(" userCode=\"" + StringUtils.xmlEncode(user.getCode()) + "\"");
				out.println(" userName=\"" + StringUtils.xmlEncode(user.getName()) + "\"");
				out.println(" userKana=\"" + StringUtils.xmlEncode(userKana) + "\"");
				out.println(" userMail=\"" + StringUtils.xmlEncode(userMail) + "\"");
				out.println(" groupName=\"" + StringUtils.xmlEncode(groupName) + "\"");
				out.println(" roleName=\"" + StringUtils.xmlEncode(roleName) + "\"");
				out.println(" userDisable=\"" + disable + "\"");
				out.println(">");
			out.println("</user>");
		}
		
		//End Root Node
		out.println("</users>");
		
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
