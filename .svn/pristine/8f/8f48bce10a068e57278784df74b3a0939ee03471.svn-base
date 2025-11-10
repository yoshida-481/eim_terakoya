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
	EIMUser loginUser = null;

	//Parameter
	String prmObjId = request.getParameter("objId");

	//Message
	String message = null;
	Object[] paramId = {
			"objId=" + prmObjId,
			};
	try
	{
		//Session
		sess = EIMUtils.getSession(request);
		if(sess == null)
		{
			message = EIMResource.getMessage(request, "EIM.ERROR.SESSIONTIMEOUT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.error(AppMessageUtils.makeLogMessage(message));
			return;
		}
		loginUser = (EIMUser)sess.getAttribute("USER");

		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

		// 対象文書またはフォルダが選択されていない場合
		if( prmObjId == null || prmObjId == "" ) {
			message = EIMResource.getMessage(sess, "EIM.ERROR.INPUT.NODOCANDFOLDER");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.INPUT.NODOCANDFOLDER");
			log.error(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId))	;
			return;
		}

		// Object
		EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		if(object == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOTEXIST.DOCFOL");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOTEXIST.DOCFOL");
			log.error(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId))	;
			return;
		}

		// 権限チェック
		if( PublishAddonUtils.checkSetPDFSecurityAuth(sess, Long.parseLong(prmObjId)) == false ) {
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOPDFSECCHANGEROLE");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOPDFSECCHANGEROLE");
			log.error(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId))	;
			return;
		}

		// ステータスが公開中 and 公開ファイル拡張子が PDF か否か
		EIMFormat publicFormat = FileUtils.getFormatByName(sess, EIMConfig.get("FORMAT_NAME_PUBLIC"));
		if( object.getStatus() == null ) {
			message = EIMResource.getMessage(sess,"EIM.ERROR.LOGIC.NOTPUBLIC", new Object[]{object.getName()});
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
		}
		if( object.getStatus().getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_PUBLIC ) {
			message = EIMResource.getMessage(sess,"EIM.ERROR.LOGIC.NOTPUBLIC", new Object[]{object.getName()});
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
		}
		EIMFile filePDF = FileUtils.getFile(sess, object, publicFormat);
		if (filePDF == null) {
			message = EIMResource.getMessage(sess,"EIM.ERROR.LOGIC.NOTPUBLIC", new Object[]{object.getName()});
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
		}
		if( !filePDF.getExt().equals(EIMConfig.get("PDF_EXT")) ) {
			message = EIMResource.getMessage(sess,"EIM.ERROR.LOGIC.PUBLICFILE.NOTPDF", new Object[]{object.getName()});
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
		}

		// PDFセキュリティ設定オブジェクトが既に存在
		EIMObject pdfSecSettingObj = AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_SECPDF"), String.valueOf(object.getId()));
		if( pdfSecSettingObj != null ) {
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.PUBLICFILE.NOWSETTING");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
		}

		// PDF 署名オブジェクト取得
		out.println("<publishFileSecurity ");
		EIMObjectType objTypePDFSig = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_SIGNPDF"));
		EIMObject pdfSigObj  = ObjectUtils.getObjectByTypeAndName(sess, objTypePDFSig, String.valueOf(object.getId()));
		if( pdfSigObj != null ) {
			out.println(PublishAddonUtils.getSetSecurityConfig(sess,pdfSigObj));
		} else {
			// 上位WF付きフォルダからの取得を試みる
			long upperObjId = helper.getUpperFolderWithWorkflowObjId(object);
			if( upperObjId != -1 ) {
				// 上位WF付きフォルダが存在
				pdfSigObj = ObjectUtils.getObjectByTypeAndName(sess, objTypePDFSig, String.valueOf(upperObjId));
				if( pdfSigObj != null ) {
					// 上位WF付きフォルダのPDF署名オブジェクトから設定値を返す
					out.println(PublishAddonUtils.getSetSecurityConfig(sess,pdfSigObj));
				} else {
					out.println(" doSetSecurity=\"false\"");
				}
			} else {
				out.println(" doSetSecurity=\"false\"");
			}
		}
		out.println(">");
		out.println("</publishFileSecurity>");
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
		message = EIMResource.getMessage("EIM.ERROR.SYSTEMERROR");
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
	}
	finally
	{
		try{
			if(sess != null) sess.close();
		}
		catch (Exception se) {
			out.clear();
			message = EIMResource.getMessage("EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(se.getMessage()));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
	}

%>