<%@ page contentType="text/xml; charset=UTF-8" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>

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

System.out.println("dspRelationType prmRelationTypeId = " + prmRelationTypeId);

	//Message
	String message = null;
	Object[] paramId = {
			"relationTypeId=" + prmRelationTypeId
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

		//Other Attribute Type
		List otherList = RelationUtils.getOtherRelationTypeNameList(sess, relationType.getId());

		System.out.println("mutual == " + relationType.getIsMutual());

		out.println("<relationType");
		out.println(" relationTypeId=\"" + relationType.getId() + "\"");
		if(otherList != null)
		{
			out.println(" " + LanguageFieldUtil.PARAM_OTHRE_CNT + "=\"" + otherList.size() + "\"");
		}
		out.println(" revisionUp=\"" + relationType.getRevisionUp().getId() + "\"");
//		out.println(" revisionUp=\"3\"");
		out.println(" mutual=\"" + relationType.getIsMutual() + "\"");
		out.println(">");
		for(int i=0;i<otherList.size();i++){
			EIMOtherName eimOtherName = (EIMOtherName)otherList.get(i);
			out.println("<lang");
				out.println(" " + LanguageFieldUtil.PARAM_OTHRE_LID + "=\"" + eimOtherName.getLangId() + "\"");
				out.println(" " + LanguageFieldUtil.PARAM_OTHRE_NAME + "=\"" + StringUtils.xmlEncode(eimOtherName.getName()) + "\"");
			out.println("/>");
		}
		out.println("</relationType>");

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
