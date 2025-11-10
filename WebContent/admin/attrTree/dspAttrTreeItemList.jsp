<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>
<%@ page import = "common.bo.AttributeTreeItem"%>
<%@ page import = "common.bo.AttributeTree"%>

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
	String prmAttrTreeId = request.getParameter("attrTreeId");
	if(StringUtils.isBlank(prmAttrTreeId))
	{
		return;
	}

	//Message
	String message = null;
	Object[] paramId = {
			"attrTreeId=" + prmAttrTreeId
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
		if(!AdminAuthUtil.hasSpecifiedOrGeneralAuth(loginUser, AppConstant.ADMIN_AUTH_ID_ATTRIUTE_TREE))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.NOTADMINISTRATOR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.NOTADMINISTRATOR");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message));
			return;
		}
		
		//Get Attribute Tree
		//Attribute Tree
		AttributeTree attributeTree = AttributeTreeUtil.getAttributeTreeById(sess, Long.parseLong(prmAttrTreeId));
		if(attributeTree == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.ATTRIBUTETREE.NOTFOUND");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.ATTRIBUTETREE.NOTFOUND");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}
		
		//Attribute Type List
		List attrTreeItemList = attributeTree.getTreeItemList();
		
		//Root Node
		out.println("<attrTreeItemList>");
		
		for(int i = 0; i < attrTreeItemList.size(); i++)
		{
			//Attribute Type
			AttributeTreeItem attributeTreeItem = (AttributeTreeItem)attrTreeItemList.get(i);
			//XML
			out.println("<attrTreeItem");
				out.println(" attrTreeItemId=\"" + attributeTreeItem.getId() + "\"");
				out.println(" attrTypeId=\"" + attributeTreeItem.getType().getId() + "\"");
				out.println(" attrTypeName=\"" + StringUtils.xmlEncode(attributeTreeItem.getType().getName()) + "\"");
				out.println(" viewNoValuesFlag=\"" + attributeTreeItem.isViewNoValues() + "\"");
			out.println(">");
			out.println("</attrTreeItem>");
		}
		
		//End Root Node
		out.println("</attrTreeItemList>");
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
