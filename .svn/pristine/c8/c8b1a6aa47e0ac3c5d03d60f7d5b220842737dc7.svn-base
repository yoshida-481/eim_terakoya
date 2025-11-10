<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "java.io.*" %>
<%@ page import = "java.util.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());
	
	//Session
	EIMSession sess = null;
	EIMUser user = null;
	
	//Message
	String message = null;
	
	try{
		//ContentType
		response.setContentType("text/xml; charset=UTF-8");
		response.setHeader("Cache-Control", "max-age=0, must-revalidate");
		
		//PrintWriter
		PrintWriter pw = response.getWriter();
		
		//Session
		sess = EIMUtils.getSession(request);
		
		//User
		user = sess.getUser();
		String strUserId = Long.toString(user.getId());
		
		//Object Type Publisher Template
		EIMObjectType objType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_PUBLIC_NOTIFICATION_TEMPLATE"));
		
		//Search Publisher Template Object
		List templateObjList = SearchUtils.searchObject(sess,
														objType,
														strUserId,
														false,
														true,
														0,
														null,
														null,
														null,
														null,
														null,
														null,
														null,
														null,
														null,
														null);

		//Root Node
		pw.println("<nodes>");
		
		for(int i = 0; i < templateObjList.size(); i++)
		{
			//Template Object
			EIMObject object = (EIMObject)templateObjList.get(i);
			String label = object.getAttribute(EIMConfig.get("ATTR_NAME_PUBLIC_NOTIFICATION_TEMPLATE_NAME")).getString();
			
			//XML Out
			pw.println("<node");
				pw.println(" label=\"" + StringUtils.xmlEncode(label) + "\"");
				pw.println(" isBranch=\"false\"");
				pw.println(" templateId=\"" + object.getId() + "\"");
				pw.println(" templateName=\"" + object.getName() + "\"");
				pw.println(" icon=\"favoriteIcon\"");
				pw.println(">");
			pw.println("</node>");
		}
		
		//End Root Node
		pw.println("</nodes>");
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
