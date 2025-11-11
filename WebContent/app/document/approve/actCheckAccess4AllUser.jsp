<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>

<%@ page import = "jp.co.ctc_g.eim.framework.business.domain.entity.UserDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework.business.dao.UserDefGroupDao"%>
<%@ page import = "jp.co.ctc_g.eim.framework.business.service.impl.UserDefGroupConfServiceImpl"%>
<%@ page import = "jp.co.ctc_g.eim.framework.business.domain.UserDefGroupDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework.business.service.UserDefGroupConfService"%>
<%@ page import = "org.springframework.context.ApplicationContext"%>
<%@ page import = "jp.co.ctc_g.eim.framework.business.domain.BelongDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework.business.domain.AssignEntryDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework.common.util.ApplicationContextLoader"%>
<%@ page import = "jp.co.ctc_g.eim.framework.business.dao.AssignEntryDao"%>
<%@ page import = "jp.co.ctc_g.eim.framework.common.util.EIMThreadContext"%>
<%@ page import = "jp.co.ctc_g.eim.framework.business.domain.entity.ObjectDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework.business.service.UserDefGroupPlugIn"%>


<%	

class Users
{
	private List repeatCheckList = new ArrayList();
	
	public void add(	EIMUser		user,
						JspWriter	out
					)
	throws Exception
	{
		if (!repeatCheckList.contains(String.valueOf(user.getId())))
		{
			out.println("<user");
				out.println(" label=\"" + StringUtils.xmlEncode(user.getName()) + "\"");
				out.println(" type=\"user\"");
				out.println(" userId=\"" + user.getId() + "\"");
				out.println(" userCode=\"" + StringUtils.xmlEncode(user.getCode()) + "\"");
				out.println(" userName=\"" + StringUtils.xmlEncode(user.getName()) + "\"");
				out.println(" userKana=\"" + StringUtils.xmlEncode(user.getKana()) + "\"");
				out.println(" userMail=\"" + StringUtils.xmlEncode(user.getMail()) + "\"");
			out.println(">");
			out.println("</user>");
			
			repeatCheckList.add(String.valueOf(user.getId()));
		}
	}
}

// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());
	
	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");
	
	//Session
	EIMSession sess = null;
	EIMUser loginUser = null;
	
	//Parameter
	String prmObjId = request.getParameter("objId");

	//Member
	List<EIMUser> allUserList = (List<EIMUser>)new ArrayList();
	List groupList = new ArrayList();
	List roleList = new ArrayList();
	List compList = new ArrayList();
	List userDefGroupList = new ArrayList();
	
	//Message
	String message = null;
	Object[] paramId = {
			"objId=" + prmObjId
			};
	boolean sessPutflg = false;
	
	try
	{
		//Session
		sess = EIMUtils.getSession(request);
		if(sess == null)
		{
			message = EIMResource.getMessage(request, "EIM.ERROR.SESSIONTIMEOUT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}
		loginUser = (EIMUser)sess.getAttribute("USER");
		
		sessPutflg = false;
		if(EIMThreadContext.getEIMSession() == null)
		{
			//Service、Dao呼出に必要
			EIMThreadContext.putEIMSession(sess);
			sessPutflg = true;
		}
		
		//Object
		EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		if(object == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODOCUMENT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId))	;
			return;
		}
		
		// User
		int pos = 0;
		while(true)
		{
			String param = EIMUtils.getParameter(request, "user_" + pos);
			if( param == null || param.length() == 0 )
			{
				break;
			}
			
			//user
			EIMUser selectApprover = UserUtils.getUserById(sess, Long.parseLong(param));
			if(selectApprover == null)
			{
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOUSER");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId))	;
				return;			
			}
			
			//Disable
			if(selectApprover.getDisable() == 1)
			{			
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOAPPROVER.INVALIDITY.USER", new Object[]{selectApprover.getName()});
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOAPPROVER.INVALIDITY.USER", new Object[]{selectApprover.getName()});
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				return;	
			}
			
			//Security
			EIMSecurity sec = object.getSecurity();
			if(sec != null)
			{
				//Check
				if(!SecurityUtils.authorized(sess, object, selectApprover, EIMAccessRole.STATUS_UP))
				{
					pos++;
					continue;
					
				}
			}
			allUserList.add(selectApprover);
			pos++;
		}

		// Group
		pos = 0;
		while(true)
		{
			String param = EIMUtils.getParameter(request, "group_" + pos);
			if( param == null || param.length() == 0 )
			{
				break;
			}
			
			//Group
			EIMGroup group = GroupUtils.getGroupById(sess, Long.parseLong(param));
			if(group == null)
			{
				out.clear();
				message = EIMResource.getMessage(sess, "EIM.ERROR.GROUP.NOTFOUND");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.GROUP.NOTFOUND");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				return;			
			}
			
			//Member
			List userList = GroupUtils.getUserListRecurrently(sess, group, 0, false);

			//Security
			EIMSecurity sec = object.getSecurity();
			if(sec != null)
			{

				for(int i = 0; i < userList.size(); i++)
				{
					//User
					EIMUser user = (EIMUser)userList.get(i);
					
					//Check
					if(!SecurityUtils.authorized(sess, object, user, EIMAccessRole.STATUS_UP))
					{
						continue;
					}
					allUserList.add(user);
				}
			}
			pos++;
		}
		
		// Role
		pos = 0;
		while(true)
		{
			String param = EIMUtils.getParameter(request, "role_" + pos);
			if( param == null || param.length() == 0 )
			{
				break;
			}
			
			//Role
			EIMRole role = RoleUtils.getRoleById(sess, Long.parseLong(param));
			//ロールが取得できない場合はエラーにする
			if(role == null)
			{
				out.clear();
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.ROLE.NOTFOUND");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.ROLE.NOTFOUND");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				return;
			}
			
			//Member
			List userList = RoleUtils.getUserList(sess, role, 0, false);
			
			//Security
			EIMSecurity sec = object.getSecurity();
			if(sec != null)
			{
				for(int i = 0; i < userList.size(); i++)
				{
					//User
					EIMUser user = (EIMUser)userList.get(i);
					
					//Check
					if(!SecurityUtils.authorized(sess, object, user, EIMAccessRole.STATUS_UP))
					{
						continue;
					}
					allUserList.add(user);
				}
			}
			pos++;
		}
		
		
		// Comp
		pos = 0;
		while(true)
		{
			String param = EIMUtils.getParameter(request, "comp_" + pos);
			if( param == null || param.length() == 0 )
			{
				break;
			}
			
			//Comp
			EIMComp comp = CompUtils.getCompById(sess, Long.parseLong(param));
			//複合グループが取得できない場合はエラーにする
			if(comp == null)
			{
				out.clear();
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.COMP.NOTFOUND");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.COMP.NOTFOUND");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				return;
			}

			//Member
			List userList =  new ArrayList();
			CompUtils.getUserListRecurrently(sess, comp.getGroup(), comp.getRole(), 0, userList);

			//Security
			EIMSecurity sec = object.getSecurity();
			if(sec != null)
			{
				for(int i = 0; i < userList.size(); i++)
				{
					//User
					EIMUser user = (EIMUser)userList.get(i);
					
					//Check
					if(!SecurityUtils.authorized(sess, object, user, EIMAccessRole.STATUS_UP))
					{
						continue;
					}
					allUserList.add(user);
				}
			}
			pos++;
		}
		
		// UserDefGroup
		pos = 0;
		while(true)
		{
			String param = EIMUtils.getParameter(request, "userDefGroup_" + pos);
			if( param == null || param.length() == 0 )
			{
				break;
			}
			
			//userDefGroup
			ApplicationContext context = ApplicationContextLoader.getContext();
			UserDefGroupConfService uds = (UserDefGroupConfService)context.getBean("UserDefGroupConfService");
			UserDefGroupDomain userDefGroupDomain = uds.getUserDefGroup(Long.parseLong(param));
			
			//ユーザ定義グループが取得できない場合はエラーにする
			if(userDefGroupDomain == null)
			{
				out.clear();
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.USERDEFGROUP.NOTFOUND");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.USERDEFGROUP.NOTFOUND");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				return;
			}

			//Member
			String key = userDefGroupDomain.getKey();
			ApplicationContext keycontext = ApplicationContextLoader.getContext();
			UserDefGroupPlugIn userGroup = (UserDefGroupPlugIn)keycontext.getBean(key);
			ObjectDomain objectDomain = new ObjectDomain(object);
			List<UserDomain> userDomainList = userGroup.getUserListByObject(objectDomain);
					
			//Security
			EIMSecurity sec = object.getSecurity();
			if(sec != null)
			{
				for(UserDomain userDomain : userDomainList)
				{
					//User
					EIMUser user = userDomain.createEIMUser();
					
					//Check
					if(!SecurityUtils.authorized(sess, object, user, EIMAccessRole.STATUS_UP))
					{
						continue;
					}
					allUserList.add(user);
				}
			}
			pos++;
		}

		//ユーザ名称でソート
		allUserList = AppObjectUtil.getStrSortedList(allUserList, "getName", true);

		// output User
		out.println("<userList>");
		Users users = new Users();
		for (int k = 0 ; k < allUserList.size() ; k++)
		{
			users.add(allUserList.get(k), out);
		}
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
		message = EIMResource.getMessage("EIM.ERROR.SYSTEMERROR");
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
	}
	finally
	{
		try{
			//Remove Session from Thread Local Table
			if(sessPutflg == true){
				EIMThreadContext.removeEIMSession();
				sessPutflg = false;
			}
			if(sess != null) sess.close();
		}
		catch (Exception se) {
			out.clear();
			message = EIMResource.getMessage("EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(se.getMessage()));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
	}
	
%>
