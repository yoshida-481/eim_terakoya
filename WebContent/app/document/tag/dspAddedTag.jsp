<%@ page contentType="text/xml; charset=UTF-8"%>
<%@ page import="eim.bo.*"%>
<%@ page import="eim.net.*"%>
<%@ page import="eim.util.*"%>
<%@ page import="java.util.*"%>

<%@ page import="org.apache.commons.logging.*"%>
<%@ page import="common.util.*"%>
<%@ page import="app.document.search.EIMDocSearchType"%>

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
	/* EIMObjID of Tag */
	String prmObjId = request.getParameter("objId");

	//Message
	String message = null;
	Object[] paramId = { "objId=" + prmObjId };

	try {

		//Session
		sess = EIMUtils.getSession(request);
		if (sess == null) {
			message = EIMResource.getMessage(request,"EIM.ERROR.SESSIONTIMEOUT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.SESSIONTIMEOUT");
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}

		// User
		user = (EIMUser) sess.getAttribute("USER");
		// Object
		EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		if (object == null || !SecurityUtils.authorized(sess, object, user, EIMAccessRole.READ)) {
			message = EIMResource.getMessage(sess,"EIM.ERROR.LOGIC.NOTEXIST.DOCFOLWS");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOTEXIST.DOCFOLWS");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(),message, paramId));
			return;
		}

		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
		// 付与されているタグ取得
		List objList = new ArrayList();
		long[] tags = AppObjectUtil.getIntAttrs(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_TAG"));
		if( tags != null ) {
			// タグ一覧をSearchUtilsで取得
			objList = AppSearchUtils.searchObjectsByConditionMaker(sess, EIMDocSearchType.DISPLAY_ADDED_TAGLIST,
											EIMAccessRole.READ, null, tags);
		}
		objList = (ArrayList)AppObjectUtil.getStrSortedList(objList, "getName", true);
		out.println("<tagList");
		out.println("  number=\"" + String.valueOf(objList.size()) + "\">");
		for (Iterator i = objList.iterator(); i.hasNext();) {
			EIMObject obj = (EIMObject) i.next();
			out.println("<tagObject");
			out.println(" objId=\"" + obj.getId() + "\"");
			out.println(" objName=\"" + StringUtils.xmlEncode(obj.getName()) + "\"");
			String objTypeName = "";
			if(helper.isTypeOfFolder(obj.getType())) {
				objTypeName = helper.getObjTypeNameFolderXmlEscaped();
			} else if(helper.isTypeOfTag(obj.getType())) {
				objTypeName = EIMConfig.get("OBJECT_TYPE_NAME_TAG");
			} else {
				objTypeName = StringUtils.xmlEncode(obj.getType().getDefName());
			}
			out.println(" objTypeName=\"" + objTypeName + "\"");
			// パス
			String path = StringUtils.nullToBlank(AppObjectUtil.getPath(obj));
			out.println(" path=\"" + StringUtils.xmlEncode(path) + "\"");
			// 署名・暗号化状態
			long signencr = AppObjectUtil.getIntAttr(sess, obj, EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_STATUS"), 0);
			out.println(" signencr=\"" + signencr + "\"");
			// 有効期限
			boolean expiration = false;
			Date expirationDate = AppObjectUtil.getDateAttr(sess, obj, EIMConfig.get("ATTR_NAME_FOLDER_EFFECT_DATE"));
			if(expirationDate != null) {
				expiration = DateUtils.judgeExpirationDate(sess, expirationDate);
			}
			out.println(" expiration=\"" + expiration + "\"");
			// readOnly
			out.println(" readOnly=\"" + helper.isReadOnlyAccess(obj) + "\"");
			out.println(" isWorkflowFolder=\"false\"");
			out.println(" isDocument=\"false\"");
			out.println(" isDocumentLink=\"false\"/>");
		}
		out.println("</tagList>");
	} catch (EIMException eime) {
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(user.getId(), eime.getMessage(), paramId), eime);
	} catch (Exception e) {
		out.clear();
		message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
	} finally {
		try {
			if (sess != null) {
				sess.close();
			}
		} catch (Exception se) {
			out.clear();
			message = EIMResource.getMessage(sess,"EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()),se);
		}
	}
%>
