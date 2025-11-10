<%@ page contentType="text/xml; charset=UTF-8" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>
<%@ page import = "java.io.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>

<%@page import="org.apache.commons.lang3.ArrayUtils"%>

<%
	Log log = LogFactory.getLog(this.getClass().getName());

	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	EIMSession sess = null;
	EIMUser loginUser = null;

	Object[] paramId = null;

	// Message
	String message = null;

	try
	{
		// セッション情報の取得
		sess = EIMUtils.getSession(request);
		if(sess == null)
		{
			message = EIMResource.getMessage("EIM.ERROR.SESSIONTIMEOUT");
			log.warn(AppMessageUtils.makeLogMessage(message));
			response.sendError(1001);
			return;
		}
		loginUser = (EIMUser)sess.getAttribute("USER");

		// クライアントから受け渡されたパラメタの取得
		String prmFileName = EIMUtils.getParameter(request,"compFileName");
		String prmOrgObjId = EIMUtils.getParameter(request,"orgObjId");
		String prmDstObjId = EIMUtils.getParameter(request,"dstObjId");
		String prmIsMail = EIMUtils.getParameter(request,"isCompMail");
		String prmDoLayoutAnalyze = EIMUtils.getParameter(request,"doAnalyzeLayout");

		paramId = new Object[]{
				"compFileName=" + prmFileName,
				"orgObjId=" + prmOrgObjId,
				"dstObjId=" + prmDstObjId,
				"isCompMail=" + prmIsMail,
				"doAnalyzeLayout=" + prmDoLayoutAnalyze,
				};

		// Windows禁止文字チェック
		AppObjectUtil.checkValidateFName(sess, prmFileName);
		
		// ファイル名チェック
		String fileExt = StringUtils.getFileExt(prmFileName);
		String[] nameArray = prmFileName.split("\\.");
		if (StringUtils.isBlank(fileExt) || fileExt.trim().equals(".") || 
			(StringUtils.isBlank(nameArray[0]) && isHalfBlankOnly(nameArray[0])))
		{
			// スペースのみのファイル名は不可
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.FILENAME.INVALID");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.FILENAME.INVALID");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}

		// 比較元ドキュメントオブジェクト、比較対象ドキュメントオブジェクトを取得
		EIMObject orgDocObj = ObjectUtils.getObjectById(sess, Long.parseLong(prmOrgObjId));
		EIMObject dstDocObj = ObjectUtils.getObjectById(sess, Long.parseLong(prmDstObjId));
		if(orgDocObj == null || dstDocObj == null) 
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODOCUMENT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODOCUMENT");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}

		// 権限チェック
		if( (!SecurityUtils.authorized( sess, orgDocObj, sess.getUser(), EIMAccessRole.READ)) 
			|| (!SecurityUtils.authorized( sess, dstDocObj, sess.getUser(), EIMAccessRole.READ )))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOREADROLE");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOREADROLE");
			log.error(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}

		// 公開文書比較オブジェクト作成
		EIMObjectType objTypeDocCompare = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_DOC_COMPARE"));
		if( objTypeDocCompare == null )
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.SYSTEMERROR");
			log.error(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}
		
		EIMObject objDocComp = ObjectUtils.createObject(sess, objTypeDocCompare, prmFileName);
		if( objDocComp == null )
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.SYSTEMERROR");
			log.error(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}
		
		// 作成したオブジェクトに属性を設定
		EIMAttributeType attTypeOrgId = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_COMP_FILE_SRC_ID"));
		EIMAttributeType attTypeDstId = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_COMP_FILE_DEST_ID"));
		EIMAttributeType attTypeIsMail = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_NOTIF_MAIL_FLAG"));
		EIMAttributeType attTypeDoLayoutAnalyze = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_ANALYZE_LAYOUT_FLAG"));

		ObjectAttributeUtils.setAttribute(sess, objDocComp, attTypeOrgId, Long.parseLong(prmOrgObjId));
		ObjectAttributeUtils.setAttribute(sess, objDocComp, attTypeDstId, Long.parseLong(prmDstObjId));
		ObjectAttributeUtils.setAttribute(sess, objDocComp, attTypeIsMail, Long.parseLong(prmIsMail));
		ObjectAttributeUtils.setAttribute(sess, objDocComp, attTypeDoLayoutAnalyze, Long.parseLong(prmDoLayoutAnalyze));

		//Create Operation History
		OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, AppConstant.COMPARING_PUBLIC_FILES,
			EIMConstant.TARGET_CREATE, EIMConstant.OBJECT, objDocComp, null, null, null, null);

		//Commit
		sess.commit();
		out.println("<ok></ok>");
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
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
	}
	catch(Exception e)
	{
		out.clear();
		log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
		try{
			if(sess != null){
				sess.rollback();
			}
		}
		catch (Exception se) {
			out.clear();
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
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
	}
%>

<%!
/**
 * 指定文字列が半角スペースのみで構成されているかかどうかチェックする
 */
private static boolean isHalfBlankOnly(String str)
{
	for(int ii = 0; ii < str.length(); ii++) {
		char c = str.charAt(ii);
		if(c != ' ') {
			return false;
		}
	}
	return true;
}
%>
