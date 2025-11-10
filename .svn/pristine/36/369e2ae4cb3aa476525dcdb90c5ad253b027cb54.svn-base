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
<%@ page import = "jp.co.ctc_g.eim.framework2.common.exception.EIMException"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils"%>
<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	class RecurrentUtils
	{
	   /**
		* コンストラクタ
		*/
		public RecurrentUtils()
		{
		}
		
		public void write(EIMSession	sess,
						  EIMGroup		group,
						  Boolean		accessSecurityNarrowDownFlag, 
						  List<Long>	accesableGroupIdList,
						  JspWriter		out)
		throws Exception
		{
			//Child Group
			List childGroupList = GroupUtils.getChildGroupList(sess, group);
			for(int i = 0; i < childGroupList.size(); i++)
			{
				//Group
				EIMGroup childGroup = (EIMGroup)childGroupList.get(i);
				
				if(accessSecurityNarrowDownFlag && !accesableGroupIdList.contains((long)childGroup.getId()))
				{
					// アクセスセキュリティ絞込みフラグがtrueかつ参照可能グループでない場合、XMLを生成しない
					continue;
				}
				//XML
				out.println("<group");
					out.println(" label=\"" + StringUtils.xmlEncode(childGroup.getName()) + "\"");
					out.println(" groupId=\"" + childGroup.getId() + "\"");
				out.println(">");
				
				//Recurrenty
				write(sess, childGroup, accessSecurityNarrowDownFlag, accesableGroupIdList, out);
				
				out.println("</group>");
			}
		}
	}
	
	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");
	
	//Session
	EIMSession sess = null;
	EIMUser loginUser = null;

	//Parameter
	String prmObjId = EIMUtils.getParameter(request, "objId");
	Boolean isFilter = Boolean.valueOf(EIMUtils.getParameter(request, "isFilter"));

	//Message
	String message = null;
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
		
		// サービス取得
		GroupService groupService = (GroupService)ApplicationContextLoader.getApplicationContext().getBean("groupServiceForSelectEntry");
		
		
		// 返却用ルートグループリスト
		List<EIMGroup> rootGroupList = new ArrayList<EIMGroup>();
		// ルートグループIDリスト
		List<Long> rootGroupIdList = new ArrayList<Long>();
		// 対象オブジェクトの参照権限を保有するユーザが所属するグループIDリスト(親階層含む)
		List<Long> accesableGroupIdList = new ArrayList<Long>();
		
		// ルートグループリスト取得
		rootGroupList = GroupUtils.getRootGroupList(sess);
		
		// アクセスセキュリティ絞り込みフラグがtureかつドキュメント管理の場合、アクセスセキュリティで絞り込む
		// システム管理から本処理が呼び出されることがあるためisAppを設ける
		if(accessSecurityNarrowDownFlag && isFilter)
		{
			// 対象オブジェクト参照可能ユーザリスト
			List<UserDomain> accesableUserList = AccesableSecurityUtil.getAccesableUserList(Long.parseLong(prmObjId));
			
			// 対象オブジェクトの参照権限を保有するユーザが所属するルートグループリスト
			List<GroupDomain> accesableRootGroupList = new ArrayList<GroupDomain>();
			for(UserDomain accesableUser: accesableUserList)
			{
				List<GroupDomain> accesableUserGroupList = accesableUser.getGroupList();
				for(GroupDomain accesableGroup: accesableUserGroupList)
				{
					// ルートグループリストに格納
					accesableRootGroupList.add(getRootGroup(accesableGroup));
					// グループIDリスト(親階層含む)に格納
					accesableGroupIdList = getGroupIdContainParent(accesableGroup, accesableGroupIdList);
				}
			}
			
			// グループツリー最上位リスト(全て)
			List<GroupDomain> rootGroupListAll = groupService.getRootList();
			
			// グループツリー最上位リスト(画面返却用)
			rootGroupList = new ArrayList<EIMGroup>();
			rootGroupIdList = new ArrayList<Long>();
			for(GroupDomain rootGroup: rootGroupListAll)
			{
				for(GroupDomain accesableRootGroup: accesableRootGroupList)
				{
					if(rootGroup.getId() == accesableRootGroup.getId())
					{
						if(!rootGroupIdList.contains(rootGroup.getId()))
						{
							// 返却用にGroupDomainをEIMGroupに詰め替える
							rootGroupList.add(new EIMGroup(rootGroup.getId(), rootGroup.getName(), null));
							rootGroupIdList.add(rootGroup.getId());
						}
					}
				}
			}
		}
		
		//Root Node
		out.println("<groups>");
		
		for(int i = 0; i < rootGroupList.size(); i++)
		{
			//Group
			EIMGroup group = (EIMGroup)rootGroupList.get(i);
			
			//XML
			out.println("<group");
				out.println(" label=\"" + StringUtils.xmlEncode(group.getName()) + "\"");
				out.println(" groupId=\"" + group.getId() + "\"");
			out.println(">");
			
			//Recurrently
			RecurrentUtils ru = new RecurrentUtils();
			ru.write(sess, group, accessSecurityNarrowDownFlag, accesableGroupIdList, out);
			
			//End Top Object
			out.println("</group>");
		}
		
		//End Root Node
		out.println("</groups>");
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
<%!
GroupDomain getRootGroup(GroupDomain group)
{
	GroupDomain parentGroup = group.getParent();
	
	if(parentGroup != null)
	{
		parentGroup = getRootGroup(parentGroup);
	} 
	else
	{
		// 親が存在しない場合、自分を返却
		return group;
	}
	
	return parentGroup;
}
%>
<%!
List<Long> getGroupIdContainParent(GroupDomain group, List<Long> groupIdList)
{
	if(!groupIdList.contains(group.getId()))
	{
		groupIdList.add(group.getId());
	}
	
	GroupDomain parentGroup = group.getParent();
	
	if(parentGroup != null)
	{
		// 親が存在する場合、再帰的に取得
		groupIdList = getGroupIdContainParent(parentGroup, groupIdList);
	}
	
	return groupIdList;
}
%>