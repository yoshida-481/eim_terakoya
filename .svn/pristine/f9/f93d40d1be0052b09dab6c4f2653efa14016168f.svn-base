<%@page import="java.text.SimpleDateFormat"%>
<%@ page contentType="text/xml; charset=UTF-8" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>
<%@ page import = "java.io.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>

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
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
		}
		loginUser = (EIMUser)sess.getAttribute("USER");

		MultiPartFormUtilsForZipArchive mpf = null;
		try{
			mpf = new MultiPartFormUtilsForZipArchive(request);
		}
		catch(EIMException eime) {
			sess.rollback();
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), eime.getMessage(), paramId), eime);
			out.println(AppMessageUtils.makeErrorTagByMessage(eime.getMessage()));
			return;
		}
		//クライアントから受け渡されたパラメタの取得
		String prmObjId = mpf.getParameter("objId");
		String prmObjTypeId = mpf.getParameter("documentTypeId");
		String prmCreateUserId = mpf.getParameter("createUserId");
		String prmFileName = mpf.getParameter("fileName");
		paramId = new Object[]{
				"objId=" + prmObjId,
				"objTypeId=" + prmObjTypeId,
				"createUserId=" + prmCreateUserId,
				"fileName=" + prmFileName
				};


		//ドキュメントタイプの取得
		EIMObjectType documentObjType = getDocumentObjType(sess, prmObjTypeId);
		if(documentObjType == null)
		{
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODOCUMENTTYPE");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
		}

		//親オブジェクトの取得
		EIMObject parentObj = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		if(parentObj == null)
		{
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOFOLDER");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
		}

		//親オブジェクトがゴミ箱の下に移動していないかのチェック
		if (AppObjectUtil.isObjectInRecycle(sess, parentObj)) {
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.SELECTFOLDERDELETED");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
		}

		// 作成権限のチェック
		if(!SecurityUtils.authorized(sess, parentObj, sess.getUser(), EIMAccessRole.CREATE))
		{
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOCREATEROLE");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			sess.close();
			return;
		}
		//ワークフロー付きフォルダ下の場合は、編集中以外は作成できない
		if (parentObj.getStatus() != null) {
			if (parentObj.getStatus().getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_EDITTING) {
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOCREATEROLE");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				sess.close();
				return;
			}
		}

		//パスの取得
		String path = AppObjectUtil.getPath(parentObj);
		if(path == null) {
			// ワークスペースの場合、パス属性の値を保持していない
			path = "/";
		}
		path += parentObj.getName() + "/";

		// ファイルサイズのチェック(最大値)
		long size = mpf.getFileSize();
		long maxFileSize = Long.parseLong(EIMConfig.get("UPLOAD_FILE_SIZE_MAX"));
		if(size < 0 || maxFileSize < size) {
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.UPLOAD.FILE.TOTALSIZE.OVER", new Object[]{maxFileSize});
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
		}


		//拡張子の確認（大文字小文字は無視）
		String fileExt = StringUtils.getFileExt(mpf.getFileName()).replaceFirst(".", "");
		if(fileExt.toUpperCase().equals("ZIP") == false) {
			message = EIMResource.getMessage("EIM.ERROR.INPUT.NOTZIPFILE");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
		}

		/*
		 * 一時格納フォルダオブジェクトの作成し、属性（EIMファイル名、登録先パス）を設定する。
		 */
		//一時格納フォルダオブジェクトタイプの取得
		EIMObjectType tmpFolderObjType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_TMP_STORE"));
		if (tmpFolderObjType == null) {
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOTMPSTOREFOLDERTYPE");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
		}

		Date now = new Date();
		SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMddHHmmss");
		String objName = "zipUL_" + prmCreateUserId + "_" + sdf.format(now);
		EIMObject object = ObjectUtils.createObject(sess, tmpFolderObjType, objName);
		if(object == null)
		{
			sess.rollback();
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.FAILMAKEOBJ");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
		}

		EIMAttributeType attType = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_ZIP_FILE_NAME"));
		ObjectAttributeUtils.setAttribute(sess, object, attType, mpf.getFileName());
		attType = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_REGIST_PATH"));
		ObjectAttributeUtils.setAttribute(sess, object, attType, path);

		/*
		 * アップロードしたファイルを一時格納フォルダに移動し、ZIP解凍する。
		 */
		String tmpBasePath = EIMConfig.get("TEMP") + object.getId();
		String tmpUnZipPath = tmpBasePath + "/unzip";
		File tmpBaseFolder = new File(tmpBasePath);
		File tmpUnzipFolder = new File(tmpUnZipPath);
		if(tmpUnzipFolder.mkdirs() == false) {
			sess.rollback();
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODEFAULTTMPFOLDER");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
		}

		try{
			String zipFilePath = tmpBasePath + "/" + mpf.getFileName();
			mpf.move(zipFilePath);
			ZipUtil.unZip(sess, zipFilePath, tmpUnZipPath);
		}
		catch(EIMException e) {
			//ZIP解凍に失敗した場合は、フォルダ削除処理を行う。
			//削除に失敗した場合は一時格納フォルダオブジェクトをコミットし、後々削除する。
			if(FileUtil.clean(tmpBaseFolder) == false) {
				sess.commit();
			}
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), e.getMessage(), paramId));
			if(e.getMessageKey().equals("EIM.ERROR.INPUT.NOTZIPFILE")) {
				message = EIMResource.getMessage("EIM.ERROR.INPUT.NOTZIPFILE");
			}
			else if(e.getMessageKey().equals("EIM.ERROR.NOT.DECOMPRESS")) {
				message = EIMResource.getMessage("EIM.ERROR.NOT.DECOMPRESS");
			}
			else {
				message = EIMResource.getMessage("EIM.ERROR.SYSTEMERROR");
			}
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
		}
		catch(IOException e) {
			//ZIP解凍に失敗した場合は、フォルダ削除処理を行う。
			//削除に失敗した場合は一時格納フォルダオブジェクトをコミットし、後々削除する。
			if(FileUtil.clean(tmpBaseFolder) == false) {
				sess.commit();
			}
			throw e;
		}

		/*
		 * DBから一時格納フォルダ情報（オブジェクト）を取得する。その中で「作成からクライアントのタイムアウト
		 * 時間以上が経過しているフォルダ」 は、今後使用されないことが確定しているため削除し、DBから一時
		 * 格納フォルダ情報（オブジェクト）も削除する。
		 * 一時格納フォルダが存在しない場合も、同様にDBから一時格納フォルダ情報（オブジェクト）を削除する。
		 */
		List tmpFolderList = ObjectUtils.getObjectListByType(sess, tmpFolderObjType,EIMAccessRole.READ);
		long timeoutMillisecond = request.getSession().getMaxInactiveInterval() * 1000;
		for(int ii = 0; ii < tmpFolderList.size(); ii++) {
			EIMObject o = (EIMObject)tmpFolderList.get(ii);
			Date createDate = o.getCreateDate();
			if(createDate.getTime() + timeoutMillisecond < now.getTime()) {
				File deleteFolder = new File(EIMConfig.get("TEMP") + "/" + o.getId());
				if(deleteFolder.exists() == false || FileUtil.clean(deleteFolder)) {
					ObjectUtils.deleteObject(sess, o);
				}
			}
		}


		out.println("<objList");
			out.println(" objId=\"" + object.getId() + "\"");
		out.println(">");

		/*
		 * 一時格納フォルダをウォーキングして、登録可否のチェックを行う。
		 */
		LumpUploadManager manager = new LumpUploadManager(sess, documentObjType, tmpUnZipPath, path);
		manager.createParentObjectMap(tmpUnzipFolder, mpf.getFileName());
		ArrayList retList = manager.recursiveCheckAssign(tmpUnzipFolder, true);
		retList = setLowerObjectCannotUpload(retList);
		retList = setUppderObjectConfirmUpload(retList);


		//表示XML文字列を作成
		for(int ii = 0; ii < retList.size(); ii++) {
			LumpUploadXmlData data = (LumpUploadXmlData)retList.get(ii);
			out.println(data.toXMLString());
		}

		out.println("</objList>");

		//Commit
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

<%!
/**
 * ドキュメントのオブジェクトタイプを取得する。
 */
private EIMObjectType getDocumentObjType(EIMSession sess,  String id) throws Exception
{
	if (id != null && ObjectUtils.getObjectTypeById(sess, Long.parseLong(id)) == null) {
		return null;
	}

	return (id == null || id.length() == 0) ?
			ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT")) :
			ObjectUtils.getObjectTypeById(sess, Long.parseLong(id));
}

/*
 * 以下の場合、配下のオブジェクトのステータスを "CANNOT_UPLOAD" に変更する。
 *   1.自分自身（data）がフォルダ
 *   2.自分自身（data）のステータスが "CANNOT_UPLOAD"
 *   3.自分自身（data）の配下に登録不可以外のオブジェクト（child）がある。
 */
private ArrayList setLowerObjectCannotUpload(ArrayList list)
{
	for(int ii = 0; ii < list.size(); ii++) {
		LumpUploadXmlData data = (LumpUploadXmlData)list.get(ii);
		if(data.isFolder() && (data.isConfirmAssign() || data.cannotAssign())) {
			for(int jj = ii+1; jj < list.size(); jj++) {
				LumpUploadXmlData child = (LumpUploadXmlData)list.get(jj);
				if(child.isChild(data.getPath())) {
					if(data.cannotAssign() && child.cannotAssign() == false) {
						child.setCannotAssign(EIMResource.getMessage("EIM.ERROR.INPUT.UPPEROBJ.CANNOTASSIGN"));
					}
				}
				else {
					break;
				}
			}
		}
	}

	return list;
}

/*
 * 以下の場合、フォルダ（data）のステータスを "CONFIRM_AND_CHECK_UPLOAD" に変更する。
 *   1.自分自身（data）がフォルダ
 *   2.自分自身（data）のステータスが "CONFIRM_UPLOAD"
 *   3.自分自身（data）の配下に登録可能なオブジェクト（child）がある。
 */
private ArrayList setUppderObjectConfirmUpload(ArrayList list)
{
	for(int ii = 0; ii < list.size(); ii++) {
		LumpUploadXmlData data = (LumpUploadXmlData)list.get(ii);
		if(data.isFolder() && (data.isConfirmAssign() || data.cannotAssign())) {
			for(int jj = ii+1; jj < list.size(); jj++) {
				LumpUploadXmlData child = (LumpUploadXmlData)list.get(jj);
				if(child.isChild(data.getPath())) {
					if(data.isConfirmAssign() && child.canAssign()) {
						//上記Ａ
						data.setCheckedConfirm(EIMResource.getMessage("EIM.ERROR.INPUT.SUBORDINATEOBJ.ASSIGN"));
						break;
					}
				}
				else {
					break;
				}
			}
		}
	}

	return list;
}

%>

