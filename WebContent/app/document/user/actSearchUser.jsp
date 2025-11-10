<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>

<%@ page import = "jp.co.ctc_g.eim.app.document.business.util.AccesableSecurityUtil"%>

<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.*"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.*"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.TransactionContext" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.enumeration.*" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils"%>

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
	String prmObjId = EIMUtils.getParameter(request, "objId");
	String prmUserIds = EIMUtils.getParameter(request, "userIds");
	String prmUserCode = EIMUtils.getParameter(request, "userCode");
	String prmUserName = EIMUtils.getParameter(request, "userName");
	String prmUserKana = EIMUtils.getParameter(request, "userKana");
	String prmIsNotDisplayInvalidityUser = EIMUtils.getParameter(request, "isNotDisplayInvalidityUser");
	Boolean isFilter = Boolean.valueOf(EIMUtils.getParameter(request, "isFilter"));
	
	//Message
	String message = null;
	Object[] paramId = {
			"userIds=" + prmUserIds,
			"userCode=" + prmUserCode,
			"userName=" + prmUserCode,
			"userKana=" + prmUserKana,
			"isNotDisplayInvalidityUser" + prmIsNotDisplayInvalidityUser
			};
	

	try
	{
		// アクセスセキュリティ絞り込みフラグ
		// trueの場合、アクセスセキュリティで絞り込んだ情報をクライアントに返却
		Boolean accessSecurityNarrowDownFlag = Boolean.valueOf(EIMConfig.get("ACCESS_SECURITY_NARROW_DOWN"));
		
		sess = EIMUtils.getSession(request);
		
		if(sess == null)
		{
			message = EIMResource.getMessage(request, "EIM.ERROR.SESSIONTIMEOUT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.SESSIONTIMEOUT");
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}
		loginUser = (EIMUser)sess.getAttribute("USER"); 
		
		
		// V5APIを使用
		if(EIMThreadContext.getTransactionContext() != null)
		{
			EIMThreadContext.removeTransactionContext();
		}
		TransactionContext context = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
		EIMThreadContext.putTransactionContext(context);
		context.setLangId(sess.getLangId());
		context.setDBConnection(sess.getDBConnection());
		context.setUser(ConvertUtils.toUserDomain(loginUser));
		
		List<EIMUser> userList = null;
		
		// IDを指定されていた場合、IDから直接取得する
		if (prmUserIds != null && prmUserIds.length() > 0)
		{
			List<Long> idList = new ArrayList<Long>();
			List<String> idStrList = Arrays.asList(prmUserIds.split(","));
			for (String str: idStrList)
			{
				idList.add(Long.valueOf(str));
			}
			userList = UserUtils.getUserByIds(sess, idList);
		}
		// IDの指定がない場合、通常の検索処理を行う
		else
		{
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
		
		
		// アクセスセキュリティ絞り込みフラグがtureかつドキュメント管理の場合、アクセスセキュリティで絞り込む
		// システム管理から本処理が呼び出されることがあるためisAppを設ける
		if(accessSecurityNarrowDownFlag && isFilter)
		{
			// 対象オブジェクト参照可能ユーザリスト
			List<UserDomain> accesableUserList = AccesableSecurityUtil.getAccesableUserList(Long.parseLong(prmObjId));
			
			// 対象オブジェクト参照可能ユーザリストを比較用にセットに格納
			HashSet<Long> accesableUserIdSet = new HashSet<Long>();
			for(UserDomain accesableUser: accesableUserList)
			{
				accesableUserIdSet.add(accesableUser.getId());
			}
			
			// 表示ユーザ情報を抽出
			List<EIMUser> searchUserList = new ArrayList<EIMUser>();
			for(EIMUser searchUser: userList)
			{
				if(accesableUserIdSet.contains(searchUser.getId()))
				{
					// 検索結果ユーザが対象オブジェクトへアクセス可能ユーザの場合、ユーザリストに追加
					searchUserList.add(searchUser);
				}
			}
			// セキュリティで絞り込んだユーザ情報のみ返却
			userList = searchUserList;
		}
		
		//Root Node
		out.println("<users>");
		
		for(int i = 0; i < userList.size(); i++)
		{
			//User
			EIMUser user = (EIMUser)userList.get(i);
			
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
			
			//Group
			String groupName = "";
			List groupList = GroupUtils.getGroupByUser(sess, user);
			for(int j = 0; j < groupList.size(); j++)
			{
				EIMGroup group = (EIMGroup)groupList.get(j);
				if(j == 0)
				{
					groupName += group.getName();
				}
				else
				{
					groupName += "," + group.getName();
				}
			}
			
			//Role
			String roleName = "";
			List roleList = RoleUtils.getRoleByUser(sess, user);
			for(int j = 0; j < roleList.size(); j++)
			{
				EIMRole role = (EIMRole)roleList.get(j);
				if(j == 0)
				{
					roleName += role.getName();
				}
				else
				{
					roleName += "," + role.getName();
				}
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
		try
		{
			if(EIMThreadContext.getTransactionContext() != null)
			{
				EIMThreadContext.removeTransactionContext();
			}
			if(sess != null)
			{
				sess.close();
			}
		}
		catch (Exception se)
		{
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
	}
%>
