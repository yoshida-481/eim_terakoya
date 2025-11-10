<%@ page contentType="text/xml; charset=UTF-8" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.bo.*" %>
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
	String prmObjTypeId = request.getParameter("objTypeId");

	//Message
	String message = null;
	Object[] paramId = {
			"objTypeId=" + prmObjTypeId
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
		if(!AdminAuthUtil.hasSpecifiedOrGeneralAuth(loginUser, AppConstant.ADMIN_AUTH_ID_DOCUMENTTYPE))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.NOTADMINISTRATOR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.NOTADMINISTRATOR");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message));
			return;
		}
		
		//Object Type
		EIMObjectType objType = ObjectUtils.getObjectTypeById(sess, Long.parseLong(prmObjTypeId));
		if(objType == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.OBJTYPE.NOTFOUND");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.OBJTYPE.NOTFOUND");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}
		
		//Format List
		List formatList = FileUtils.getFormatListByType(sess, objType);
		
		//Root Node
		out.println("<formats>");
		
		HashSet formatSet = new HashSet();	// フォーマット重複チェック用のHashSet。子から親への順番で格納されることが前提。
		
		for(int i = 0; i < formatList.size(); i++)
		{
			//Format
			EIMFormat format = (EIMFormat)formatList.get(i);
			
			// 自分と親とで重複するフォーマットがある場合、自分のだけを表示
			if (!formatSet.add(new Long(format.getId()))) {
				continue;
			}
			
			//Direcotry
			EIMDirectory dir = format.getDirectory();
			
			//XML Out
			out.println("<format");
				out.println(" formatId=\"" + format.getId() + "\"");
				out.println(" formatName=\"" + StringUtils.xmlEncode(format.getName()) + "\"");
				out.println(" path=\"" + StringUtils.xmlEncode(dir.getPath()) + "\"");
				out.println(" objTypeId=\"" + format.getObjectType().getId() + "\"");
				out.println(" objTypeName=\"" + StringUtils.xmlEncode(format.getObjectType().getName()) + "\"");
				out.println(" def=\"" + format.isDefault() + "\"");
				if (objType.getId() != format.getObjectType().getId()) {
					out.println(" isParentFormat=\"true\"");	// 親から継承したフォーマットの場合はtrue
				}
				out.println(">");
			out.println("</format>");
		}
		
		//End Root Node
		out.println("</formats>");
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
