<%@ page contentType="text/html; charset=UTF-8" %>
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

	response.setContentType("text/html; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	EIMSession sess = null;
	EIMUser loginUser = null;

	Object[] paramId = null;

	//Message
	String message = null;

	try
	{
		//セッション情報の取得
		sess = EIMUtils.getSession(request);
		if(sess == null)
		{
			message = EIMResource.getMessage("EIM.ERROR.SESSIONTIMEOUT");
			log.warn(AppMessageUtils.makeLogMessage(message));
			response.sendError(1001);
			return;
		}
		loginUser = (EIMUser)sess.getAttribute("USER");

		//クライアントから受け渡されたパラメタの取得
		String prmFileName = EIMUtils.getParameter(request,"fileName");
		String prmDocTypeId = EIMUtils.getParameter(request,"docTypeId");
		String prmFolderObjId = EIMUtils.getParameter(request,"folderObjId");
		String prmJoinedDocIds = EIMUtils.getParameter(request,"joinedDocIds");
		paramId = new Object[]{
				"fileName=" + prmFileName,
				"docTypeId=" + prmDocTypeId,
				"folderObjId=" + prmFolderObjId,
				"joinedDocIds=" + prmJoinedDocIds,
				};

		// [09/01/19 added by ik.]
		// Windows禁止文字チェック
		AppObjectUtil.checkValidateFName(sess, prmFileName);
		
		// ファイル名チェック
		String name = prmFileName.substring(0,1);
		if ( StringUtils.isBlank(name) && isHalfBlankOnly(name) )
		{
			// スペースのみのファイル名は不可
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.FILENAME.INVALID");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.FILENAME.INVALID");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}

		//オブジェクトタイプの取得
		EIMObjectType docType = null;
		//オブジェクトタイプチェック
		if (prmDocTypeId != null && ObjectUtils.getObjectTypeById(sess, Long.parseLong(prmDocTypeId)) == null) {
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.DOCTYPE.NOTFOUND");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.DOCTYPE.NOTFOUND");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}
		if(prmDocTypeId == null || prmDocTypeId.length() == 0) {
			docType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT"));
			prmDocTypeId = Long.toString(docType.getId());
		} else {
			docType = ObjectUtils.getObjectTypeById(sess, Long.parseLong(prmDocTypeId));
		}

		//親オブジェクトの取得
		EIMObject parentObj = ObjectUtils.getObjectById(sess, Long.parseLong(prmFolderObjId));
		if(parentObj == null) {
			message = EIMResource.getMessage(sess,"EIM.ERROR.LOGIC.PDFJOIN.NOFOLWS");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.PDFJOIN.NOFOLWS");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}

		// 権限チェック
		String [] ids = prmJoinedDocIds.split(",");
		if( PublishAddonUtils.checkPDFJoinAuth( sess, ids, Long.parseLong(prmFolderObjId)) == false ) {
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOPDFJOINROLE");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOPDFJOINROLE");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}

		// ステータスが公開中 and 公開ファイル拡張子が PDF か否か
		EIMFormat publicFormat = FileUtils.getFormatByName(sess, EIMConfig.get("FORMAT_NAME_PUBLIC"));
		String strIds = ""; // for Operation History
		for( int i = 0; i < ids.length; i++ ) {
			EIMObject docObj = ObjectUtils.getObjectById(sess,Long.parseLong(ids[i]));
			if( docObj == null ) {
				message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.SYSTEMERROR");
				log.error(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				return;
			}
			if( docObj.getStatus() == null ) {
				message = EIMResource.getMessage(sess,"EIM.ERROR.LOGIC.NOTPUBLIC", new Object[]{docObj.getName()});
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				return;
			}
			if( docObj.getStatus().getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_PUBLIC ) {
				message = EIMResource.getMessage(sess,"EIM.ERROR.LOGIC.NOTPUBLIC", new Object[]{docObj.getName()});
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				return;
			}
			EIMFile filePDF = FileUtils.getFile(sess, docObj, publicFormat);
			if (filePDF == null) {
				message = EIMResource.getMessage(sess,"EIM.ERROR.LOGIC.NOTPUBLIC", new Object[]{docObj.getName()});
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				return;
			}
			if( !filePDF.getExt().equals(EIMConfig.get("PDF_EXT")) ) {
				message = EIMResource.getMessage(sess,"EIM.ERROR.LOGIC.PUBLICFILE.NOTPDF", new Object[]{docObj.getName()});
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				return;
			}
			strIds = strIds + docObj.getName() + "(" + docObj.getId() + ")";
			if( i + 1 < ids.length ) strIds = strIds + ",";
		}

		//指定された PDF 結合オブジェクトの作成
		EIMObjectType pdfJoinObjType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_JOINPDF"));
		EIMObject pdfJoinObj = ObjectUtils.createObject(sess, pdfJoinObjType, prmFileName);
		// ドキュメントタイプID設定
		EIMAttributeType attType = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_TYPE_ID"));
		ObjectAttributeUtils.setAttribute(sess, pdfJoinObj, attType, Long.parseLong(prmDocTypeId));
		// 親ワークスペース or フォルダID設定
		attType = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_PARENT_ID"));
		ObjectAttributeUtils.setAttribute(sess, pdfJoinObj, attType, Long.parseLong(prmFolderObjId));
		// 結合対象オブジェクトID設定
		attType = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_JOINEDPDF_ID"));
		String [] splits = prmJoinedDocIds.split(",");
		long[] integerArray = new long[splits.length];
		for( int i = 0; i < splits.length; i++ ) {
			integerArray[i] = Long.parseLong(splits[i]);
		}
		ObjectAttributeUtils.setAttribute(sess, pdfJoinObj, attType, TypeConvertUtils.convertToBuildTypeArray(integerArray));
		// 登録者設定
		attType = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_PDFJOIN_USER"));
		ObjectAttributeUtils.setAttribute(sess, pdfJoinObj, attType, loginUser.getId());

		//Create Operation History
		OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, AppConstant.UNITING_PUBLIC_FILES,
			AppConstant.INFO_UNITING_PUBLIC_FILES, EIMConstant.OBJECT, pdfJoinObj,
			AppConstant.TARGET_TO_UNITE, EIMConstant.OBJECT, strIds,
			null);

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
}%>
