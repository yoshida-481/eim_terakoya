<%@ page contentType="text/html; charset=UTF-8" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.util.*" %>

<%@ page import = "java.util.*" %>
<%@ page import = "java.io.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>
<%@ page import = "addon.*" %>
<%@ page import = "jp.co.ctc_g.eim.framework.common.util.EIMThreadContext"%>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	class AttrCheckUtils
	{
	   /**
		* コンストラクタ
		*/
		public AttrCheckUtils()
		{
		}

	   /**
	    * 再帰的にオブジェクト属性を調べ、公開処理失敗ONが存在するかどうか返却する。
	    *
	    * @param sess EIMSessionインスタンス
	    * @param objType 親オブジェクトタイプ
	    * @param isExistsPDFFailed 公開処理失敗属性の存在
	    * @return 公開処理失敗属性の存在(true:失敗あり)
	    */
		public boolean isExistsPDFFailed(EIMSession sess, EIMObject object)
	    throws Exception
		{
			//Relation Type
			EIMRelationType relType = RelationUtils.getRelationTypeByName(sess, "ドキュメント");

			boolean isExistsPDFFailed = false;
			return isExistsPDFFailed(sess, object, relType, isExistsPDFFailed);
		}

	    private boolean isExistsPDFFailed(EIMSession sess, EIMObject parentObj, EIMRelationType relType, boolean isExistsPDFFailed)
	    throws Exception
	    {

			//Child Relation
			List childRelList = new ArrayList();

			childRelList = RelationUtils.getChildRelationListByRelType(sess, parentObj, relType,EIMAccessRole.READ);
			for(int i = 0; i < childRelList.size(); i++)
			{
				//Relation
				EIMRelation relation = (EIMRelation)childRelList.get(i);

				//Child Object
				EIMObject childObj = relation.getChild();

				long pdfFailFlg = AppObjectUtil.getIntAttr(sess, childObj, EIMConfig.get("ATTR_NAME_DOCUMENT_PUB_PROC_FAIL"), -1);
				if (pdfFailFlg == AppConstant.FLAG_ON)
				{
					isExistsPDFFailed = true;
				}

				isExistsPDFFailed = isExistsPDFFailed(sess, childObj, relType, isExistsPDFFailed);
			}

			return isExistsPDFFailed;

	    }

	}

	//ContentType
	response.setContentType("text/html; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	//Session
	EIMSession sess = null;
	EIMUser loginUser = null;
	boolean sessPutFlag = false;

	//Message
	String message = null;
	Object[] paramId = null;

	try
	{
		//Session
		sess = EIMUtils.getSession(request);
		if(sess == null)
		{
			message = EIMResource.getMessage("EIM.ERROR.SESSIONTIMEOUT");
			log.warn(AppMessageUtils.makeLogMessage(message));
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
		}

		// ThreadLocalコンテキストにセッションを設定
		if(EIMThreadContext.getEIMSession() == null)
		{
			EIMThreadContext.putEIMSession(sess);
			sessPutFlag = true;
		}

		//Login User
		loginUser = (EIMUser)sess.getAttribute("USER");

		// Helper
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

		//MultiPartFormUtils
		MultiPartFormUtils mpf = null;
		try{
			mpf = new MultiPartFormUtils(request);
		}
		catch(EIMException eime) {
			sess.rollback();
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), eime.getMessage(), paramId), eime);
			out.println(AppMessageUtils.makeErrorTagByMessage(eime.getMessage()));
			return;
		}

		//Parameter
		String prmObjId = mpf.getParameter("objId");
		String prmParentObjId = mpf.getParameter("parentObjId");
		String fileName = mpf.getParameter("fileName");
		paramId = new Object[]{
				"objId=" + prmObjId,
				"parentObjId=" + prmParentObjId,
				"fileName=" + fileName
				};

		// ファイル存在チェック
		if(mpf.getFileSize() == 0)
		{
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOUPLOADFILE");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
		}

		// オブジェクト取得
		EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		if(object == null)
		{
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODOCUMENT");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
		}

		// ファイル名称チェック
		if(!fileName.equals(object.getName()))
		{
			message = EIMResource.getMessage("EIM.ERROR.INPUT.DEFFERENTFILENAME");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
		}

		// PDF変換オブジェクトのオブジェクトタイプ
		EIMObjectType objTypePDFConv = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_CONVPDF"));

		// PDF変換失敗のドキュメントかチェック
//		if (!AppObjectUtil.isPDFConvertFailed(sess, object, objTypePDFConv))
		// 公開処理失敗属性を保持するドキュメントのみ差し戻し対象に変更
		long pdfFailFlg = AppObjectUtil.getIntAttr(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_PUB_PROC_FAIL"), AppConstant.FLAG_OFF);
		if (pdfFailFlg == AppConstant.FLAG_OFF)
		{
			message = EIMResource.getMessage("EIM.ERROR.INPUT.NOTPDFFALEDDOCUMENT");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
		}

		// Systemセキュリティに属するユーザかチェック
		if(!AppSecurityUtils.authorizedBySystemSecurity(sess, EIMAccessRole.UPDATE))
		{
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOREPLACEROLE");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
		}

		// 署名・暗号化中でないかチェック
		long signencr = AppObjectUtil.getIntAttr(sess, object, helper.getAttrNameOfSignEncStatus(), AppConstant.SIGNENCR_KIND_NOT_SIGNENCR);
		if (signencr == AppConstant.SIGNENCR_KIND_PROCESSING_SIGNENCR)
		{
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.CANNOT.REPLACE.WITH.SIGN.AND.ENCR.PROC");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
		}


		EIMFormat formatSignEnc = null;
		if (OptionConfData.getInstance().SignAndEncrFlg) {
			formatSignEnc = FileUtils.getFormatByName(sess, EIMConfig.get("FORMAT_NAME_SIGNENCR"));
			if (signencr == AppConstant.SIGNENCR_KIND_SIGNENCR)
			{
				// 署名・暗号化済
				if (formatSignEnc == null)
				{
					// 署名・暗号化フォーマットが取得できない
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.SIGN.AND.ENCR.FORMAT.NOTFOUND");
					log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					return;
				}
				else if (FileUtils.getFile(sess, object, formatSignEnc) == null)
				{
					// 署名・暗号化ファイルが取得できない
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.NO.SIGN.AND.ENCR.FILE.WITHDOCNAME",
							new Object[]{object.getName()});
					log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					return;
				}
			}
		}

		// 公開処理失敗属性を削除する
		AppObjectUtil.setAttr(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_PUB_PROC_FAIL"), AppConstant.FLAG_OFF);
		object = ObjectUtils.getObjectById(sess, object.getId());

		//
		// PDF変換に失敗したファイルをコピーする
		//
		// 親オブジェクトの取得
		EIMObject parentObj = ObjectUtils.getObjectById(sess, Long.parseLong(prmParentObjId));
		if(parentObj == null)
		{
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOWSFOL");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
		}

		// パスの取得
		String path = AppObjectUtil.getPath(parentObj);
		if(path == null) {
			// ワークスペースの場合、パス属性の値を保持していない
			path = "/";
		}
		path += parentObj.getName() + "/";

		// Relation Type
		EIMRelationType relType = RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));

		String langId = sess.getLangId();
		String newObjName = EIMConfig.get("REPLACE_FILENAME_PREFIX_" + langId)
						+ " - " + object.getName();

		// Create Object
		EIMObject newObj = ObjectUtils.createObject(sess, object.getType(), newObjName);

//		// Create Relation
//		while(true) {
//			try {
//				RelationUtils.createRelation(sess, relType, parentObj, newObj, EIMConstant.DEPU_CHECK_NAME_REV);
//			} catch(EIMException ecp) {
//				int errCode = ecp.getCode();
//				if(errCode == 914) {
//					// 同名オブジェクトがあれば接頭辞「差し替え」を付加する
//					newObjName = (sess.getLangId().equals(AppConstant.LANG_VALUE_JA)
//									? EIMConfig.get("REPLACE_FILENAME_PREFIX_JA") : EIMConfig.get("REPLACE_FILENAME_PREFIX_EN"))
//									+ " - " + newObjName;
//					ObjectUtils.rename(sess, newObj, newObjName);
//					continue;
//				}
//			}
//			break;
//		}

		// 属性の継承
		ObjectAttributeUtils.inheritAttribute(sess, object, newObj);

		// パス属性の設定
		AppObjectUtil.setPath(sess, newObj, path);

		// ファイルの継承
		FileUtils.inheritFile(sess, object, newObj);
		EIMFormat formatPublic = FileUtils.getFormatByName(sess, EIMConfig.get("FORMAT_NAME_PUBLIC"));
		if(FileUtils.getFile(sess, newObj, formatPublic) != null)
		{
			FileUtils.deleteFile(sess, newObj, formatPublic);
		}

		// Format
		EIMFormat format = FileUtils.getDefaultFormat(sess, object.getType());

		// Rename File
		FileUtils.renameFile(sess, newObj, format, newObjName);

		// Set Security
		if(parentObj.getSecurity() != null)
		{
			SecurityUtils.setSecurity(sess, newObj, parentObj.getSecurity());
		}

		// Access
		AccessUtils.createAccess(sess, newObj, "EIM.ACCESS.TYPE.INITIALREGIST");

		// 差し替え元ドキュメントをごみ箱へ

		// ごみ箱オブジェクト
		EIMObject recycleObj = AppObjectUtil.getObject(sess,
				EIMConfig.get("OBJECT_TYPE_NAME_RECYCLE"), EIMConfig.get("OBJECT_TYPE_NAME_RECYCLE"));

		if (recycleObj == null) {
			message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.SYSTEMERROR");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}

		/* ごみ箱への関連付け */
		RelationUtils.createRelation(sess, relType, recycleObj, newObj, EIMConstant.DEPU_CHECK_NONE);

		/* 「パス」属性をごみ箱のパスに変更 */
		AppObjectUtil.setPath(sess, newObj, "/" + recycleObj.getName() + "/");

		/* systemのセキュリティ設定 */
		EIMSecurity systemSec = SecurityUtils.getSecurityByName(sess, EIMConfig.get("SECURITY_NAME_SYSTEM"));
		SecurityUtils.setSecurity(sess, newObj, systemSec);

		//Create Operation History
		OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.MOVE_TO_RECYCLEBOX,
				EIMConstant.TARGET_DELETE, EIMConstant.OBJECT, newObj,
				null, null, null, path);

		//
		// 元のオブジェクトから、署名・暗号化に関する属性、ファイルを削除する
		//
		if (OptionConfData.getInstance().SignAndEncrFlg) {
			AppObjectUtil.setAttr(sess, object, helper.getAttrNameOfSignEncStatus(), AppConstant.SIGNENCR_KIND_NOT_SIGNENCR);
			AppObjectUtil.deleteAttribute(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_VER"));
			object = ObjectUtils.getObjectById(sess, object.getId());
			EIMFile signEncFile = FileUtils.getFile(sess, object, formatSignEnc);
			if (signEncFile != null)
			{
				File substance = new File(signEncFile.getDirectory().getPath() + FileUtils.getFileName(object, signEncFile));
				if(substance.exists())
				{
					substance.delete();
				}
				FileUtils.deleteFile(sess, object, formatSignEnc);
			}
		}
		//
		// アップロードしたファイルを元のオブジェクトに紐付ける
		//
		// ディレクトリの取得
		EIMDirectory dir = format.getDirectory();

		// 拡張子の取得
		String fileExt = StringUtils.getFileExt(fileName);
		if(fileExt == null)
		{
			fileExt = "";
		}

		//Checkin
		FileUtils.checkin(sess, object, format, fileName, mpf.getFileSize());

		// 元のファイル名でアップロード
		mpf.upload(dir.getPath() + object.getId() + fileExt);

		//
		// PDF変換オブジェクトの作成
		//
		EIMObject createdPDFConvObj = null;		//PDF変換オブジェクトの作成対象になったObject
		boolean hasPDFFailedDocument = false;	//配下に公開処理失敗ドキュメントがないか
		long higherWFFolderID = AppObjectUtil.getIntAttr(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_HIGHER_WF_FOLDER"), -1);
		if (higherWFFolderID == -1)
		{
			// PDF変換オブジェクトを生成
			ObjectUtils.createObject(sess, objTypePDFConv, prmObjId);
			createdPDFConvObj = object;
		}
		else
		{
			EIMObject folderObj = ObjectUtils.getObjectById(sess, higherWFFolderID);

			// 上位WFフォルダ以下にPDF変換失敗がなければPDF変換オブジェクトを生成
			AttrCheckUtils util = new AttrCheckUtils();
			hasPDFFailedDocument = util.isExistsPDFFailed(sess, folderObj);
			if (!hasPDFFailedDocument)
			{
				ObjectUtils.createObject(sess, objTypePDFConv, "" + folderObj.getId());
				createdPDFConvObj = folderObj;
			}
		}

		// ワークフロー公開処理オブジェクトの取得
		EIMObject wfpubObj = AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_WFPUB"), String.valueOf(createdPDFConvObj.getStatus().getType().getId()));

		// PDF署名オブジェクトのステータス情報設定
		if(wfpubObj != null && (higherWFFolderID == -1 || !hasPDFFailedDocument))// 上位WF付きフォルダがない、もしくは上位WF付きフォルダ内に公開処理失敗オブジェクトがない
		{
			// PDF署名オブジェクトタイプの取得
			EIMObjectType objTypePDFSign = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_SIGNPDF"));
			EIMAttribute attPdfSign = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_FLG"));
			// PDF署名オブジェクトの取得
			EIMObject pdfSignObj = ObjectUtils.getObjectByTypeAndName(sess, objTypePDFSign, String.valueOf(createdPDFConvObj.getId()));
			if(pdfSignObj != null){
				// ステータスを1(処理中)に設定
				AppObjectUtil.setAttr(sess, pdfSignObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_STATUS"), 1);
			} else if(attPdfSign != null && attPdfSign.getInt() == AppConstant.FLAG_ON) {
				// PDF署名オブジェクトの作成
				pdfSignObj = ObjectUtils.createObject(sess, objTypePDFSign, String.valueOf(createdPDFConvObj.getId()));
				PublishAddonUtils.copyPDFSignAndSecAttrs2Obj(sess, wfpubObj, pdfSignObj);
				// ステータスを1(処理中)に設定
				AppObjectUtil.setAttr(sess, pdfSignObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_STATUS"), 1);
			}
		}

		// URL挿入オブジェクトのステータス情報設定
		if(wfpubObj != null && (higherWFFolderID == -1 || !hasPDFFailedDocument))// 上位WF付きフォルダがない、もしくは上位WF付きフォルダ内に公開処理失敗オブジェクトがない
		{
			// URL挿入オブジェクトタイプの取得
			EIMObjectType objTypeInsertURL = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_INSERTURL"));
			EIMAttribute attInsertURL = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_INSERT_URL_FLG"));
			// PDF署名オブジェクトの取得
			EIMObject urlInsertObj = ObjectUtils.getObjectByTypeAndName(sess, objTypeInsertURL, String.valueOf(createdPDFConvObj.getId()));
			if(urlInsertObj == null && attInsertURL != null && attInsertURL.getInt() == AppConstant.FLAG_ON) {
				// URL挿入オブジェクトの作成
				ObjectUtils.createObject(sess, objTypeInsertURL, String.valueOf(createdPDFConvObj.getId()));
			}
		}

		// アクセス履歴の更新
		AccessUtils.createAccess(sess, object, "EIM.ACCESS.TYPE.REPLACE");

		// ### SEARCH FRAMEWORK 検索FW更新通知 差し替え後のドキュメントは元のオブジェクトに紐付けのため、元オブジェクトID 処理種別キーを指定
		if(helper.isTypeOfFolder(parentObj.getType())) {
			AppUpdateNoticeUtils.updateNoticeInsert(parentObj.getId(), "SEARCHFW_REPLACE_PARENT_WFFOLDER");
		}
		AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), "SEARCHFW_REPLACE_DOCUMENT");

		//Create Operation History
		OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.REPLACE,
				EIMConstant.TARGET_UPDATE, EIMConstant.OBJECT, object,
				null, null, null, path);

		sess.commit();
	}
	catch(EIMException eime)
	{
		out.clear();
		message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
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
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
	}
	catch(Exception e)
	{
		out.clear();
		message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
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
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
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
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
		}
	}
%>
