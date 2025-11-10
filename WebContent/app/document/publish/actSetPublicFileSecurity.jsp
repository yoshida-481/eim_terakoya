<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>
<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>
<%@ page import = "app.document.approve.*" %>
<%@ page import = "jp.co.ctc_g.eim.framework.common.util.EIMThreadContext"%>

<%

	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	//Session
	EIMSession sess = null;
	EIMUser  user = null;
	boolean sessPutFlag = false;

	//Parameter
	String prmObjId = request.getParameter("objId");
	String prmDoSetSecurity = EIMUtils.getParameter(request,"doSetSecurity");
	String prmDoSetSecurityPassword = EIMUtils.getParameter(request,"doSetSecurityPassword");
	String prmSecurityPassword = EIMUtils.getParameter(request,"securityPassword");
	String prmDoSetReferencePassword = EIMUtils.getParameter(request,"doSetReferencePassword");
	String prmReferencePassword = EIMUtils.getParameter(request,"referencePassword");
	String prmForbidPrint = EIMUtils.getParameter(request,"forbidPrint");
	String prmForbidEdit = EIMUtils.getParameter(request,"forbidEdit");
	String prmForbidAnnotate = EIMUtils.getParameter(request,"forbidAnnotate");
	String prmForbidReproduce = EIMUtils.getParameter(request,"forbidReproduce");

	//Message
	String message = null;
	Object[] paramId = {
			"objId=" + prmObjId,
			"doSetSecurity="+prmDoSetSecurity,
			"doSetSecurityPassword="+prmDoSetSecurityPassword,
			"securityPassword="+prmSecurityPassword,
			"doSetReferencePassword="+prmDoSetReferencePassword,
			"referencePassword="+prmReferencePassword,
			"forbidPrint="+prmForbidPrint,
			"forbidEdit="+prmForbidEdit,
			"forbidAnnotate="+prmForbidAnnotate,
			"forbidReproduce="+prmForbidReproduce,
			};

	try
	{
		//セッション取得
		sess = EIMUtils.getSession(request);
		if(sess == null)
		{
			message = EIMResource.getMessage(request, "EIM.ERROR.SESSIONTIMEOUT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.SESSIONTIMEOUT");
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}

		// ThreadLocalコンテキストにセッションを設定
		if(EIMThreadContext.getEIMSession() == null)
		{
			EIMThreadContext.putEIMSession(sess);
			sessPutFlag = true;
		}


		//ログインユーザ取得
		user = (EIMUser)sess.getAttribute("USER");

		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

		//対象オブジェクト取得
		EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		if(object == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODOCUMENT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODOCUMENT");
			log.error(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}

		// 権限チェック
		if( PublishAddonUtils.checkSetPDFSecurityAuth(sess, Long.parseLong(prmObjId)) == false ) {
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOPDFSECCHANGEROLE");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOPDFSECCHANGEROLE");
			log.error(AppMessageUtils.makeLogMessage(user.getId(), message, paramId))	;
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
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.PUBLICFILE.NOWSETTING");
			log.error(AppMessageUtils.makeLogMessage(user.getId(), message, paramId))	;
			return;
		}

		// PDFセキュリティ設定オブジェクト作成
		pdfSecSettingObj = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_SECPDF"), String.valueOf(object.getId()));
		PublishAddonUtils.setSetSecurityConfig(
				sess,
				pdfSecSettingObj,
				prmDoSetSecurity,
				prmDoSetSecurityPassword,
				prmSecurityPassword,
				prmDoSetReferencePassword,
				prmReferencePassword,
				prmForbidPrint,
				prmForbidEdit,
				prmForbidAnnotate,
				prmForbidReproduce
				);
		AppObjectUtil.setAttr(sess, pdfSecSettingObj, EIMConfig.get("ATTR_NAME_WEPUB_PDF_PDFSIG_USER"), user.getId());

		// PDF署名オブジェクトがなければ作成
		EIMObject pdfSignObj = AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_SIGNPDF"), String.valueOf(object.getId()));
		EIMObject upperPdfSignObj = null;
		if( pdfSignObj == null ) {
			// 上位WF付きフォルダからの取得を試みる
			long upperObjId = helper.getUpperFolderWithWorkflowObjId(object);
			if( upperObjId != -1 ) {
				// 上位WF付きフォルダが存在
				upperPdfSignObj = AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_SIGNPDF"), String.valueOf(upperObjId));
				if( upperPdfSignObj == null ) {
					// 上位WF付きフォルダに署名存在しない → PDF署名オブジェクト生成(属性は空)
					pdfSignObj = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_SIGNPDF"), String.valueOf(object.getId()));
					PublishAddonUtils.setSignAndSetSecurityConfig(sess,pdfSignObj,"0","0","0","1","0","0","0", "", "","0","0","","0","","0","0","0","0");
				} else {
					// 上位WF付きフォルダに署名存在しない → PDF署名オブジェクト生成(属性は上位WF付きフォルダと一致)
					pdfSignObj = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_SIGNPDF"), String.valueOf(object.getId()));
					PublishAddonUtils.copyPDFSignAndSecAttrs2Obj(sess,upperPdfSignObj,pdfSignObj);
				}
			} else {
				// 上位WF付きフォルダが存在しない → PDF署名オブジェクト生成(属性は空)
				pdfSignObj = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_SIGNPDF"), String.valueOf(object.getId()));
				PublishAddonUtils.setSignAndSetSecurityConfig(sess,pdfSignObj,"0","0","0","1","0","0","0", "", "","0","0","","0","","0","0","0","0");
			}
		}
		AppObjectUtil.setAttr(sess, pdfSignObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_STATUS"), 4);

		// SearchFramework 検索FW更新通知 対象：ドキュメント
		AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), "SEARCHFW_SET_PUBLIC_FILE_SECURITY_DOCUMENT");

		//Create Operation History
		String path = StringUtils.nullToBlank(AppObjectUtil.getPath(object));
		OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, AppConstant.SECSETTING_PUBLIC_FILES,
			AppConstant.INFO_SECSETTING_PUBLIC_FILES, EIMConstant.OBJECT, pdfSecSettingObj,
			AppConstant.TARGET_TO_SETTING, EIMConstant.OBJECT, object.getName() + "(" + String.valueOf(object.getId()) + ")",
			path);

		sess.commit();
		out.println("<OK></OK>");
	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(user.getId(), eime.getMessage(), paramId), eime);
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
			if(sessPutFlag) {
				EIMThreadContext.removeEIMSession();
				sessPutFlag = false;
			}

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
