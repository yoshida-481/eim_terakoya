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

		String strHasFirstVersionDocLink = "false";
		// ドキュメントの場合
		if( isFolder != true ) {

			// バージョンを取得する
			EIMVersion version = VersionUtils.getVersion(sess, object);
			if (version != null) {
				// ドキュメンリンクを取得する
				EIMRelationType relTypeDocLink = RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_LINK"));
				List docLinkList = RelationUtils.getParentRelationListByRelType(sess, object, relTypeDocLink);
				// ドキュメントが初版、且つ、ドキュメントリンクが存在する場合
				if (version.getList().size() == 1 && docLinkList.size() > 0)
					strHasFirstVersionDocLink = "true";
			}
		}
		// フォルダの場合
		else {

			AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

			// 性能向上のためフォルダ配下のオブジェクトを一括ロードする
			List<Integer> roleIdList = Arrays.asList(EIMAccessRole.READ);
			helper.loadChildObjectsRecursive(object, roleIdList, AppObjectConditionHelper.LoadingModeEnum.VERSION);

			// フォルダ配下にドキュメントリンクが存在するドキュメントがあるかどうかを確認する
			// 公開取り消しの場合、WF付きフォルダのみが対象
			// WF付きフォルダはWFの版を重ねることはないため、ドキュメントリンクの有無のみをチェックする
			if (helper.isExistDocumentWithLinkUnderFolder(sess, object))
				strHasFirstVersionDocLink = "true";
		}
		
		//XML
		out.println("<result");
			out.println(" isHasFirstVersionDocLink=\"" + strHasFirstVersionDocLink + "\"");
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