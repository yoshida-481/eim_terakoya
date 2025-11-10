<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>


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
	String prmRelationTypeId = request.getParameter("relationTypeId");
	String prmAttTypeId = request.getParameter("attTypeId");

	//Message
	String message = null;
	Object[] paramId = {
			"relTypeId=" + prmRelationTypeId,
			"attTypeId=" + prmAttTypeId,
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

		//Relation Type
		EIMRelationType relationType = RelationUtils.getRelationTypeById(sess, Long.parseLong(prmRelationTypeId));

		if(relationType == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.RELTYPE.NOTFOUND");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.RELTYPE.NOTFOUND");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}

		//Attribute Type
		EIMAttributeType attType = AttributeUtils.getAttributeTypeById(sess, Long.parseLong(prmAttTypeId));
		if(attType == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.ATTTYPE.NOTFOUND");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.ATTTYPE.NOTFOUND");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}

		//Release
		RelationAttributeUtils.releaseAttributeType(sess, relationType, attType);

		//Create Operation History
		OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.RELEASE_ATTRIBUTE_FROM_RELATION_TYPE,
				EIMConstant.TARGET_UPDATE, EIMConstant.RELATION_TYPE, relationType,
				null,  EIMConstant.ATTRIBUTE_TYPE, attType, null);

		//Commit
		sess.commit();

		//XML
		out.println("<attType");
			out.println(" attTypeId=\"" + attType.getId() + "\"");
			out.println(" attTypeName=\"" + StringUtils.xmlEncode(attType.getName()) + "\"");
			out.println(" valTypeId=\"" + attType.getValueType().getId() + "\"");
			out.println(" valTypeName=\"" + StringUtils.xmlEncode(attType.getValueType().getName()) + "\"");
			out.println(">");
		out.println("</attType>");
	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), eime.getMessage(), paramId), eime);
		try{
			if(sess != null){
				sess.rollback();
			}
		}
		catch (Exception se) {
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
	}
	catch(Exception e)
	{
		out.clear();
		message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
		try{
			if(sess != null){
				sess.rollback();
			}
		}
		catch (Exception se) {
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
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
