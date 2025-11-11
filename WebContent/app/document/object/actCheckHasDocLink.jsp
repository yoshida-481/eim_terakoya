<%@page import="common.util.AppLogicUtil.ProcessFolderTreeWalker"%>
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
	String prmIsFolder = request.getParameter("isFolder");

	//Message
	String message = null;
	Object[] paramId = {
			"objId=" + prmObjId,
			"isFolder=" + prmIsFolder
			};
	try
	{
		//Session
		sess = EIMUtils.getSession(request);
		if(sess == null) {
			message = EIMResource.getMessage(request, "EIM.ERROR.SESSIONTIMEOUT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.SESSIONTIMEOUT");
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}
		
		user = (EIMUser)sess.getAttribute("USER");
		
		boolean isFolder = prmIsFolder.equals("true") ? true : false;	//フォルダ判定
		
		//Object
		EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		if (object == null || !SecurityUtils.authorized(sess, object, sess.getUser(),EIMAccessRole.READ)) {
			if (isFolder) {
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOFOLDER");
			} else {
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODOCUMENT");
			}
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			
			if (isFolder) {
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOFOLDER");
			} else {
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODOCUMENT");
			}
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}

		String strHasDocLink = "false";
		// ドキュメントの場合
		if( isFolder != true ) {

			// 「ドキュメントリンク」属性の取得
			if( AppObjectUtil.getIntAttr(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_DOCUMENT_LINK"), 0) == 1 )
				strHasDocLink = "true";
		}
		// フォルダの場合
		else {
			AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

			// 性能向上のためフォルダ配下のオブジェクトを一括ロードする
			List<Integer> roleIdList = Arrays.asList(EIMAccessRole.READ);
			helper.loadChildObjectsRecursive(object, roleIdList, AppObjectConditionHelper.LoadingModeEnum.VERSION);

			// フォルダ配下にドキュメントリンクが存在するドキュメントがあるかどうかを確認する
			if( helper.isExistDocumentWithLinkUnderFolder(sess, object) )
				strHasDocLink = "true";
		}
		
		//XML
		out.println("<result");
			out.println(" isHasDocLink=\"" + strHasDocLink + "\"");
		out.println(">");
		out.println("</result>");
	}
	catch(EIMException eime) {
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(user.getId(), eime.getMessage(), paramId), eime);
	}
	catch(Exception e) {
		out.clear();
		message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
	}
	finally {
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