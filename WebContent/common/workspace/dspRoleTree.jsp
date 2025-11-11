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

	class RecurrentUtils
	{
	   /**
	    * コンストラクタ
		*/
		public RecurrentUtils()
		{
		}
		
		public void write(EIMSession		sess,
											EIMRole			role,
											JspWriter		out)
		throws Exception
		{
			//Child Group
			List childRoleList = RoleUtils.getChildRoleList(sess, role);
			for(int i = 0; i < childRoleList.size(); i++)
			{
				//Role
				EIMRole childRole = (EIMRole)childRoleList.get(i);
				
				//XML
				out.println("<role");
					out.println(" label=\"" + StringUtils.xmlEncode(childRole.getName()) + "\"");
					out.println(" roleId=\"" + childRole.getId() + "\"");
				out.println(">");
				
				//Recurrenty
				write(sess, childRole, out);
				
				out.println("</role>");
			}
		}
	}
	
	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");
	
	//Session
	EIMSession sess = null;
	EIMUser loginUser = null;

	//Message
	String message = null;

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
		
		//Root Node
		out.println("<roles>");
		
		//Root RoleList
		List rootRoleList = RoleUtils.getRootRoleList(sess);
		
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
			ru.write(sess, role, out);
			
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
