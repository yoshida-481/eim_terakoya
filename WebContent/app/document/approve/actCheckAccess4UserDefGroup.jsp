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
	String prmObjId = EIMUtils.getParameter(request, "objId");
	String prmUserDefGroup = EIMUtils.getParameter(request, "userDefGroupId");
	
	//Message
	String message = null;
	Object[] paramId = {
			"objId=" + prmObjId,
			"userDefGroupId=" + prmUserDefGroup
			};
	
	// val
	boolean sessPutflg = false;
	
	try{
		sess = EIMUtils.getSession(request);
		if(sess == null)
		{
			message = EIMResource.getMessage(request, "EIM.ERROR.SESSIONTIMEOUT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.SESSIONTIMEOUT");
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}
		sessPutflg = false;
		if(EIMThreadContext.getEIMSession() == null)
		{
			//Service、Dao呼出に必要
			EIMThreadContext.putEIMSession(sess);
			sessPutflg = true;
		}
		loginUser = (EIMUser)sess.getAttribute("USER");
		
		//Object
		EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		if(object == null)
		{
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODOCUMENT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODOCUMENT");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}
		
		//userDefGroup
		ApplicationContext context = ApplicationContextLoader.getContext();
		UserDefGroupConfService uds = (UserDefGroupConfService)context.getBean("UserDefGroupConfService");
		UserDefGroupDomain userDefGroupDomain = uds.getUserDefGroup(Long.parseLong(prmUserDefGroup));
		
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
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOAPPROVER.USERDEFGROUP", 
							new Object[]{userDefGroupDomain.getName(), user.getName()});
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
					return;
				}
			}
		}

		//ユーザ名称でソート
		userDomainList = AppObjectUtil.getStrSortedList(userDomainList, "getDefName", true);

		//XML出力
		out.println("<userDefGroup label=\"" + StringUtils.xmlEncode(userDefGroupDomain.getName()) + "\""
				+ " type=\"userDefGroup\""
				+ " userDefGroupId=\"" + userDefGroupDomain.getId() + "\""
				+ " userDefGroupName=\"" + StringUtils.xmlEncode(userDefGroupDomain.getName()) + "\""
				+ ">");

		Users users = new Users();
		for (int k = 0 ; k < userDomainList.size() ; k++)
		{
			//User
			UserDomain userDomain = userDomainList.get(k);
			EIMUser user = userDomain.createEIMUser();
			users.add(user, out);
		}
		
		out.println("</userDefGroup>");
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
			//Remove Session from Thread Local Table
			if(sessPutflg == true){
				EIMThreadContext.removeEIMSession();
				sessPutflg = false;
			}
			
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
