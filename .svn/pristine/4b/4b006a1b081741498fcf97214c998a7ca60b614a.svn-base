<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
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
	EIMUser user = null;
	
	//Parameter
	String prmObjId = request.getParameter("objId");
	String prmParentObjId = request.getParameter("parentObjId");
	
	//Message
	String message = null;
	Object[] paramId = {
			"objId=" + prmObjId,
			"parentobjId=" + prmParentObjId
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
		user = (EIMUser)sess.getAttribute("USER");
		
		// ドキュメントリンクの場合のみ呼び出す
		
		// 条件判定ヘルパー
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
		
		//Object
		EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		if(object == null)
		{
			throw new EIMException(sess, "EIM.ERROR.LOGIC.ZIPDL.DOCLINK.BASE.NOTFOUND");
		}
		
		// Parent Object
		EIMObject parentObject = ObjectUtils.getObjectById(sess, Long.parseLong(prmParentObjId));
		if(parentObject == null)
		{
			throw new EIMException(sess, "EIM.ERROR.LOGIC.PARENT.NOTFOUND");
		}
		
		// 上位フォルダステータス。無ステータスか編集中で無ければNG
		if (parentObject.getStatus() != null
				&& parentObject.getStatus().getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_EDITTING) {
			throw new EIMException(sess, "EIM.ERROR.LOGIC.PARENT.NOTUPDATING", new Object[]{object.getName()});
		}
		
		// 作成権限
		if (!SecurityUtils.authorized(sess, parentObject, sess.getUser(), EIMAccessRole.CREATE)) {
			throw new EIMException(sess, "EIM.ERROR.LOGIC.NOUPDATELINKSETTINGROLE");
		}
		
		// ドキュメントリンクのみ有効
		EIMRelation rel = RelationUtils.getRelationByParentAndChild(sess, helper.getRelationTypeOfDocLink(), parentObject, object);
		if( rel == null ) {
			throw new EIMException(sess, "EIM.ERROR.LOGIC.NOTEXIST.DOCLINK");
		}
		EIMAttribute attr = rel.getAttributeByName(EIMConfig.get("ATTR_NAME_LINK_UPDATE_TIMING"));
		
		//Root Node
		out.println("<objectList>");

		//XML
		out.println("<object");
			out.println(" objId=\"" + prmObjId + "\"");
			out.println(" objTypeId=\"" + object.getType().getId()+ "\"");
			out.println(" objTypeName=\"" + StringUtils.xmlEncode(object.getType().getName()) + "\"");
			out.println(" objName=\"" + StringUtils.xmlEncode(object.getName()) + "\"");
			out.println(" documentLinkUpdateTiming=\"" + attr.getInt() + "\"");
		out.println(">");
		out.println("</object>");
		
		//End Root Node
		out.println("</objectList>");

	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(user.getId(), eime.getMessage(), paramId), eime);
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
