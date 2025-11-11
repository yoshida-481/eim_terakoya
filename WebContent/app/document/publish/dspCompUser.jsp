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
	String prmCompId = EIMUtils.getParameter(request, "compId");
	Boolean isFilter = Boolean.valueOf(EIMUtils.getParameter(request, "isFilter"));

	//Message
	String message = null;
	Object[] paramId = {
			"compId=" + prmCompId
			};

	try
	{
		// アクセスセキュリティ絞り込みフラグ
		// trueの場合、アクセスセキュリティで絞り込んだ情報をクライアントに返却
		Boolean accessSecurityNarrowDownFlag = Boolean.valueOf(EIMConfig.get("ACCESS_SECURITY_NARROW_DOWN"));
		
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
		
		
		// 対象複合グループに所属するユーザリスト
		List<EIMUser> userList = new ArrayList<EIMUser>();
		EIMComp targetComp = CompUtils.getCompById(sess, Long.parseLong(prmCompId));
		if (targetComp != null) {
			userList = CompUtils.getUserList(sess, targetComp, 0);
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
			
			// 表示ユーザ情報抽出
			List<EIMUser> accesableCompUserList = new ArrayList<EIMUser>();
			List<Long> accesableCompUserIdList = new ArrayList<Long>();
			for(EIMUser compUser: userList)
			{
				if(accesableUserIdSet.contains(compUser.getId()) && !accesableCompUserIdList.contains(compUser.getId()))
				{
					// リストに追加
					accesableCompUserList.add(compUser);
					accesableCompUserIdList.add((long)compUser.getId());
				}
			}
			// セキュリティで絞り込んだユーザ情報のみ返却
			userList = accesableCompUserList;
		}
		
		//重複ユーザの排除
		HashMap<Long , EIMUser> userMap = new HashMap<Long , EIMUser>();
		for(EIMUser user : userList){
			userMap.put((long)user.getId(),user);
		}
		
		
		//Root Node
		out.println("<userList>");
		
		//for(int i = 0; i < userList.size(); i++)
		for (Map.Entry<Long , EIMUser> userEntry : userMap.entrySet())
		{
			//User
			//EIMUser user = (EIMUser)userList.get(i);
			EIMUser user = (EIMUser)userEntry.getValue();
		
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
		out.println("</userList>");
		
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