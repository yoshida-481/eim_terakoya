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
						  EIMRole		role,
						  Boolean		accessSecurityNarrowDownFlag,
						  List<Long>	accesableRoleIdList,
						  JspWriter		out)
		throws Exception
		{
			//Child Group
			List childRoleList = RoleUtils.getChildRoleList(sess, role);
			for(int i = 0; i < childRoleList.size(); i++)
			{
				//Role
				EIMRole childRole = (EIMRole)childRoleList.get(i);
				
				if(accessSecurityNarrowDownFlag && !accesableRoleIdList.contains((long)childRole.getId()))
				{
					// アクセスセキュリティ絞込みフラグがtrueかつ参照可能グループでない場合、XMLを生成しない
					continue;
				}
				// XML
				out.println("<role");
					out.println(" label=\"" + StringUtils.xmlEncode(childRole.getName()) + "\"");
					out.println(" roleId=\"" + childRole.getId() + "\"");
				out.println(">");
				
				//Recurrenty
				write(sess, childRole, accessSecurityNarrowDownFlag, accesableRoleIdList, out);
				
				out.println("</role>");
			}
		}
	}

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");
	
	//Session
	EIMSession sess = null;
	EIMUser user = null;

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
		user = (EIMUser)sess.getAttribute("USER");
		
		
		// V5APIを使用
		if(EIMThreadContext.getTransactionContext() != null)
		{
			EIMThreadContext.removeTransactionContext();
		}
		TransactionContext context = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
		EIMThreadContext.putTransactionContext(context);
		context.setLangId(sess.getLangId());
		context.setDBConnection(sess.getDBConnection());
		context.setUser(ConvertUtils.toUserDomain(user));
		
		// サービス取得
		RoleService roleService = (RoleService)ApplicationContextLoader.getApplicationContext().getBean("roleServiceForSelectEntry");
		
		// 返却用ルートロールリスト
		List<EIMRole> rootRoleList = new ArrayList<EIMRole>();
		// ルートロールIDリスト
		List<Long> rootRoleIdList = new ArrayList<Long>();
		// 対象オブジェクトの参照権限を保有するユーザが所属するロールIDリスト(親階層含む)
		List<Long> accesableRoleIdList = new ArrayList<Long>();
		
		// ルートロールリスト取得
		rootRoleList = RoleUtils.getRootRoleList(sess);
		
		// アクセスセキュリティ絞り込みフラグがtureかつドキュメント管理の場合、アクセスセキュリティで絞り込む
		// システム管理から本処理が呼び出されることがあるためisAppを設ける
		if(accessSecurityNarrowDownFlag && isFilter)
		{
			// 対象オブジェクト参照可能ユーザリスト
			List<UserDomain> accesableUserList = AccesableSecurityUtil.getAccesableUserList(Long.parseLong(prmObjId));
			
			// 対象オブジェクトの参照権限を保有するユーザが所属するロールリスト
			List<RoleDomain> accesableRootRoleList = new ArrayList<RoleDomain>();
			for(UserDomain accesableUser: accesableUserList)
			{
				List<RoleDomain> accesableRoleList = accesableUser.getRoleList();
				for(RoleDomain accesableRole: accesableRoleList)
				{
					// ルートロールリストに格納
					accesableRootRoleList.add(getRootRole(accesableRole));
					// ロールIDリスト(親階層含む)に格納
					accesableRoleIdList = getRoleIdContainParent(accesableRole, accesableRoleIdList);
				}
			}
			
			// グループツリー最上位リスト(全て)
			List<RoleDomain> rootRoleListAll = roleService.getRootList();
			
			// グループツリー最上位リスト(画面返却用)
			rootRoleList = new ArrayList<EIMRole>();
			rootRoleIdList = new ArrayList<Long>();
			for(RoleDomain rootRole: rootRoleListAll)
			{
				for(RoleDomain accesableRootRole: accesableRootRoleList)
				{
					if(rootRole.getId() == accesableRootRole.getId())
					{
						if(!rootRoleIdList.contains(rootRole.getId()))
						{
							// 返却用にRoleDomainをEIMRoleに詰め替える
							rootRoleList.add(new EIMRole(rootRole.getId(), rootRole.getName(), null));
							rootRoleIdList.add(rootRole.getId());
						}
					}
				}
			}
		}
		
		//Root Node
		out.println("<roles>");
		
		for(int i = 0; i < rootRoleList.size(); i++)
		{
			//Role
			EIMRole role = (EIMRole)rootRoleList.get(i);
			
			//XML
			out.println("<role");
				out.println(" label=\"" + StringUtils.xmlEncode(role.getName()) + "\"");
				out.println(" roleId=\"" + role.getId() + "\"");
			out.println(">");

			//Recurrently
			RecurrentUtils ru = new RecurrentUtils();
			ru.write(sess, role, accessSecurityNarrowDownFlag, accesableRoleIdList, out);
			
			//End Top Object
			out.println("</role>");
		}
		
		//End Root Node
		out.println("</roles>");
	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(user.getId(), eime.getMessage()), eime);
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
RoleDomain getRootRole(RoleDomain role)
{
	RoleDomain parentRole = role.getParent();
	
	if(parentRole != null)
	{
		parentRole = getRootRole(parentRole);
	} 
	else
	{
		// 親が存在しない場合、自分を返却
		return role;
	}
	
	return parentRole;
}
%>
<%!
List<Long> getRoleIdContainParent(RoleDomain role, List<Long> roleIdList)
{
	if(!roleIdList.contains(role.getId()))
	{
		roleIdList.add(role.getId());
	}
	
	RoleDomain parentRole = role.getParent();
	
	if(parentRole != null)
	{
		// 親が存在する場合、再帰的に取得
		roleIdList = getRoleIdContainParent(parentRole, roleIdList);
	}
	
	return roleIdList;
}
%>
