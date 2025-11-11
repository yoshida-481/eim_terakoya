package servlet.dl;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.zip.CRC32;
import java.util.zip.CheckedInputStream;
import java.util.zip.Deflater;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import common.bo.TagTreeItem;
import common.util.AppConstant;
import common.util.AppMessageUtils;
import common.util.AppObjectConditionHelper;
import common.util.AppObjectUtil;
import common.util.OptionConfData;
import common.util.TagUtil;
import eim.bo.EIMException;
import eim.bo.EIMFile;
import eim.bo.EIMFormat;
import eim.bo.EIMObject;
import eim.bo.EIMResource;
import eim.bo.EIMUser;
import eim.net.EIMSession;
import eim.util.AccessUtils;
import eim.util.EIMConfig;
import eim.util.EIMConstant;
import eim.util.EIMUtils;
import eim.util.FileUtils;
import eim.util.ObjectUtils;
import eim.util.OperationHistoryUtils;
import eim.util.StringUtils;

/**
 * ドキュメントをZIP形式にまとめてダウンロードするDownloadサーブレットのベースクラス。
 * <p>
 * このクラスは
 * <code>{@link jakarta.servlet.http.HttpServlet}</code>
 * を拡張している。
 *
 * @version	1.0.0
 * @since		1.0.0
 */
abstract class AbstractZIPDownloadDocuments extends HttpServlet
{
	// フォワード先JSP定義
	private final String ERROR_PAGE = "/servlet/HtmlErrorMessageServlet";

	// エラーメッセージ定義
	private final String ERROR_ATTR_KEY = "errorMessage";

	private int READ_BUF_SIZE = 1;

	/**
	 * ダウンロード処理実行に必要なアクセス権限をチェックする。
	 * @param session
	 * @param object
	 * @param helper
	 *
	 * @return boolean チェック結果
	 * @throws Exception
	 */
	abstract boolean checkAccessRight(EIMSession session, EIMObject object, AppObjectConditionHelper helper) throws Exception;

	/**
	 * ダウンロード対象のオブジェクトを取得する。
	 * @param session
	 * @param object
	 * @param helper
	 *
	 * @return EIMObject オブジェクト
	 * @throws Exception
	 */
	abstract EIMObject getAlternateObject(EIMSession session, EIMObject object, AppObjectConditionHelper helper) throws Exception;

	/**
	 * フォーマットを取得する。
	 * @param session
	 * @param object
	 *
	 * @return EIMFormat フォーマット
	 * @throws Exception
	 */
	abstract EIMFormat getFormat(EIMSession session, EIMObject object, AppObjectConditionHelper helper) throws Exception;

	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

		// Error Logging
		Log log = LogFactory.getLog(this.getClass().getName());

		// Session
		EIMSession sess = null;
		EIMUser loginUser = null;

		// Parameter
		String prmESignFile = request.getParameter("eSignFile");
		if( prmESignFile == null ) {
			prmESignFile = "false";
		}
		String prmIsCommaConcatObjId = request.getParameter("isCommaConcatObjId");
		boolean isCommaConcatObjId = (prmIsCommaConcatObjId != null && prmIsCommaConcatObjId.equals("true") ? true : false);

		String[] prmObjIds;
		if (isCommaConcatObjId) {
			prmObjIds = request.getParameter("objId").split(",");
		} else {
			prmObjIds = request.getParameterValues("objId");
		}
		String[] prmIsDocumentLinks = request.getParameterValues("isDocumentLink");
		// ドキュメントリンクフラグが渡されていない場合は、フラグ値"false"として扱う。
		if( prmObjIds != null && prmObjIds.length > 0 && prmIsDocumentLinks == null )
		{
			prmIsDocumentLinks = new String[prmObjIds.length];
			for(int i = 0; i < prmObjIds.length; i++)
			{
				prmIsDocumentLinks[i] = "false";
			}
		}

		//Message
		String message = null;
		boolean rbFlg = false;
		ArrayList paramIdList = new ArrayList();
		paramIdList.add("eSignFile=" + prmESignFile);
		for(int i = 0 ; i < prmObjIds.length ; i++)
		{
			paramIdList.add("objId[" + i + "]=" + prmObjIds[i]);
			paramIdList.add("isDocumentLink[" + i + "]=" + prmIsDocumentLinks[i]);
		}
		Object[] paramId = paramIdList.toArray();

		// Cache
		response.setHeader("Cache-Control", "max-age=0, must-revalidate");

		try { READ_BUF_SIZE = Integer.parseInt(EIMConfig.get("FILE.DOWNLOAD.READBYTE.LENGTH")); } catch(NumberFormatException nfe) {}
		try {
			//Session
			sess = EIMUtils.getSession(request);
			if (sess == null) {
				message = EIMResource.getMessage(request, "EIM.ERROR.SESSIONTIMEOUT");
				log.warn(AppMessageUtils.makeLogMessage(message));
				request.setAttribute(ERROR_ATTR_KEY, message);
				getServletContext().getRequestDispatcher(ERROR_PAGE).forward(request, response);
				return;
			}
			//User
			loginUser = (EIMUser)sess.getAttribute("USER");

			//Helper
			AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

			// 対象ファイルのリストを作成する
			ArrayList fileArray = new ArrayList();
			long totalSize = 0;

			try {
				List objIdList = new ArrayList();
				List isDocLinkList = new ArrayList();
				for(int i = 0; i < prmObjIds.length; i++ ) {
					objIdList.add(prmObjIds[i]);
					isDocLinkList.add(prmIsDocumentLinks[i]);
				}
				totalSize = getDocumentFromListRecursive(sess, objIdList, isDocLinkList, (prmESignFile.equals("true"))?true:false, fileArray, "", true, helper);
			}
			catch( EIMException eime) {

				rbFlg = true;
				message = eime.getMessage();
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, eime.getMessageParams()));
				// 改行コードを javascript の alert() のために「\\n」に置換する
				message = message.replaceAll("\\x0D\\x0A|\\x0D|\\x0A", "\\"+"\\"+"n");
				request.setAttribute(ERROR_ATTR_KEY, message);
				getServletContext().getRequestDispatcher(ERROR_PAGE).forward(request, response);
				return;
			}

			// アーカイブするファイルが一つも無い場合はエラーにする
			if(fileArray.size() == 0) {
				rbFlg = true;
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.ZIPDL.NODOC");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				request.setAttribute(ERROR_ATTR_KEY, message);
				getServletContext().getRequestDispatcher(ERROR_PAGE).forward(request, response);
				return;
			}

			// ZIP圧縮前の全ファイルの合計サイズチェック
			// ※Long型で解釈しないとGB単位のファイルを扱えない
			if (totalSize > Long.parseLong(EIMConfig.get("DOWNLOAD_FILES_MAXSIZE"))) {
				rbFlg = true;
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.DOCUMENT.SIZEOVER");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				request.setAttribute(ERROR_ATTR_KEY, message);
				getServletContext().getRequestDispatcher(ERROR_PAGE).forward(request, response);
				return;
			}

			// ZIP圧縮
			Date date = new Date();
			SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMddHHmmss");
			OutputStream out = response.getOutputStream();
			response.setContentType("application/octet-stream");
			response.setHeader("Content-Disposition", "attachment; filename=\"" + EIMConfig.get("DOWNLOAD_ZIPFILE_PREFIX") + "_" + sdf.format(date) + ".zip" + "\"");
			deflate(out, fileArray);
			out.flush();
			out.close();

			//Commit
			sess.commit();
		}
		catch(EIMException eime)
		{
			rbFlg = true;
			message = eime.getMessage();
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId), eime);
			request.setAttribute(ERROR_ATTR_KEY, message);
			getServletContext().getRequestDispatcher(ERROR_PAGE).forward(request, response);
		}
		catch(Exception e)
		{
			rbFlg = true;
			log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
			message = EIMResource.getMessage("EIM.ERROR.SYSTEMERROR");
			request.setAttribute(ERROR_ATTR_KEY, message);
			getServletContext().getRequestDispatcher(ERROR_PAGE).forward(request, response);
		}
		finally
		{
			try{
				if(sess != null){
					if(rbFlg){
						sess.rollback();
					}
					sess.close();
				}
			}
			catch (Exception se) {
				log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
				message = EIMResource.getMessage("EIM.ERROR.SYSTEMERROR");
				request.setAttribute(ERROR_ATTR_KEY, message);
				getServletContext().getRequestDispatcher(ERROR_PAGE).forward(request, response);
			}
		}
	}

	protected void doGet(	HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		doPost(request, response);
	}

	/**
	 * 渡されてきたオブジェクトのリストからドキュメントを取得する再帰処理
	 *
	 * @param sess EIMSessionインスタンス
	 * @param prmObjIds ドキュメント取得対象のオブジェクトのリスト
	 * @param prmIsDocumentLinks オブジェクトがドキュメントリンクのものかどうかを示すフラグのリスト
	 * @param bSignFile 署名・暗号化ファイルを取得するかどうか
	 * @param fileArray 最終的にZIPアーカイブ化するファイルのリスト(本関数の処理でファイルオブジェクトが追加される)
	 * @param relativePath アーカイブのトップからの相対パス
	 * @param isFolTagRecursive リスト中にフォルダ・タグがある場合に再帰処理を行うかどうか
	 * @param helper AppObjectConditionHelperインスタンス
	 *
	 * @return 取得したドキュメントの合計サイズ
	 * @throws Exception
	 */
	private long getDocumentFromListRecursive(EIMSession sess, List prmObjIds, List prmIsDocumentLinks, boolean bSignFile, ArrayList fileArray, String relativePath, boolean isFolTagRecursive, AppObjectConditionHelper helper) throws Exception {

		// prmObjIdsが示すドキュメントオブジェクトのファイルリストを作成する。本関数の最後にfileArrayに追加する
		ArrayList tmpArray = new ArrayList();
		// tmpArrayに格納したファイルがドキュメントリンクのものかどうか示すMap
		HashMap isDocLinkMap = new HashMap();
		// prmObjIdsが示すドキュメントオブジェクトのファイルと、(フォルダ・タグが存在する場合は)それ以下の階層のファイルサイズ合計
		long tmpSize = 0;
		// 「prmObjIdsが示すオブジェクトのファイル名・フォルダ名・タグ名」と EIMObjectインスタンスのMap
		// 名称重複のチェックに使用する
		HashMap fileNameMap = new HashMap();

		Iterator i = prmObjIds.iterator();
		Iterator j = prmIsDocumentLinks.iterator();

		// 署名暗号化機能がOFFになっている場合、署名・暗号化ファイルの取得は行わない
		if (!OptionConfData.getInstance().SignAndEncrFlg) {
			bSignFile = false;
		}

		for (;i.hasNext() && j.hasNext();) {
			String objId = (String)i.next();
			String isDocLink = (String)j.next();

			//Object
			EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(objId));
			if(object == null)	{
				// ドキュメントリンクであった場合
				if( isDocLink.equals("true") ) {
					throw new EIMException(sess, "EIM.ERROR.LOGIC.ZIPDL.DOCLINK.BASE.NOTFOUND", new Object[]{objId,isDocLink});
				}
				// ドキュメント・フォルダ・タグであった場合
				else {
					throw new EIMException(sess, "EIM.ERROR.LOGIC.NODOCORFOLORTAG", new Object[]{objId,isDocLink});
				}
			}

			// Check Access Right
			boolean check = checkAccessRight(sess, object, helper);
			if (check == false) {
				throw new EIMException(sess, "EIM.ERROR.LOGIC.NOACCESS", new Object[]{objId,isDocLink});
			}

			// オブジェクトがドキュメントの場合の処理
			if( helper.isTypeOfDocument(object.getType()) ) {
				// Alternate Object
				EIMObject alternateObject = getAlternateObject(sess, object, helper);

				//Format
				EIMFormat format = null;
				//署名・暗号化済ドキュメントを取得する場合
				if( bSignFile == true ) {
					//ドキュメントについて署名・暗号化済みドキュメントが存在する場合
					if( AppConstant.SIGNENCR_KIND_SIGNENCR == AppObjectUtil.getIntAttr(sess, alternateObject, EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_STATUS"), AppConstant.SIGNENCR_KIND_NOT_SIGNENCR) ) {
						format = FileUtils.getFormatByName(sess, EIMConfig.get("FORMAT_NAME_SIGNENCR"));
					}
					//存在しない場合
					else {
						format = getFormat(sess, alternateObject, helper);
					}
				}
				//署名・暗号化済ドキュメントを取得しない場合
				else {
					format = getFormat(sess, alternateObject, helper);
				}

				//File
				EIMFile file = FileUtils.getFile(sess, alternateObject, format);
				if(file == null) {
					throw new EIMException(sess, "EIM.ERROR.LOGIC.NODOCUMENT", new Object[]{objId,isDocLink});

				}
				tmpSize += file.getSize();

				//Substance
				File substance = null;
				FileUtils.prepareFileAccess(sess, alternateObject, file);
				substance = new File(FileUtils.getFilePath(alternateObject, file));
				if(!substance.exists())	{
					throw new EIMException(sess, "EIM.ERROR.LOGIC.NODOCUMENT", new Object[]{objId,isDocLink});
				}

				// duplicate check
				if( !isDocLink.equals("true") ) {	// ドキュメントの場合のみ重複チェックを行う
					EIMObject sameNameObj = (EIMObject)fileNameMap.get(file.getName());
					// 同名のオブジェクトがある場合は例外
					if( sameNameObj != null ) {
						// エラーメッセージの作成 - <フォルダ/ドキュメント/タグのラベル>[<パス + オブジェクト名>]
						String path1 = TagUtil.getErrorMessageForDupNameErr(sess, object, helper);
						String path2 = TagUtil.getErrorMessageForDupNameErr(sess, sameNameObj, helper);

						String[] message = {path1, path2};
						throw new EIMException(sess, "EIM.ERROR.LOGIC.ZIPDL.DOCFOLNAME.SAME", message);
					}
					fileNameMap.put(file.getName(), object);	// ファイル名は格納しておく
				}
				InFileData ifd = new InFileData(file, substance, relativePath);
				tmpArray.add(ifd);
				isDocLinkMap.put(ifd, isDocLink);

				//Access
				AccessUtils.createAccess(sess, object, "EIM.ACCESS.TYPE.DOWNLOAD");

				//パス
				String path = StringUtils.nullToBlank(AppObjectUtil.getPath(object));

				//Create Operation History
				OperationHistoryUtils.create(sess, common.util.AppConstant.DOCUMENT, EIMConstant.DOWNLOAD_DOCUMENT,
						EIMConstant.TARGET_DOWNLOAD, EIMConstant.OBJECT, object,
						null, null, null, path);
			}
			// オブジェクトがフォルダの場合の処理
			else if( helper.isTypeOfFolder(object.getType()) ) {

				// duplicate check
				EIMObject sameNameObj = (EIMObject)fileNameMap.get(object.getName());
				// 同名のオブジェクトがある場合は例外
				if( sameNameObj != null ) {
					// エラーメッセージの作成 - <フォルダ/ドキュメント/タグのラベル>[<パス + オブジェクト名>]
					String path1 = TagUtil.getErrorMessageForDupNameErr(sess, object, helper);
					String path2 = TagUtil.getErrorMessageForDupNameErr(sess, sameNameObj, helper);

					String[] message = {path1, path2};
					throw new EIMException(sess, "EIM.ERROR.LOGIC.ZIPDL.DOCFOLNAME.SAME", message);
				}
				fileNameMap.put(object.getName(), object);	// ファイル名は格納しておく

				if(isFolTagRecursive) {	//フォルダに対して再帰処理を行う場合
					//ドキュメントリンクを含むオブジェクトを取得する
					HashMap isDocLinkMapTmp = new HashMap();
					List childObjs = helper.getChildObjectsWithDocLinkInAccessibleStatus(object, isDocLinkMapTmp);
					if( childObjs != null ) {
						List childOids = new ArrayList();
						List childIsDocLinks = new ArrayList();
						for(Iterator jj = childObjs.iterator(); jj.hasNext(); ) {
							EIMObject objTmp = (EIMObject)jj.next();
							childOids.add(String.valueOf( objTmp.getId() ));
							childIsDocLinks.add((String) isDocLinkMapTmp.get(objTmp));
						}
						//相対パスの作成
						String tmpRelPath = relativePath + object.getName() + "/";
						//再帰処理
						tmpSize += getDocumentFromListRecursive(sess, childOids, childIsDocLinks, bSignFile, fileArray, tmpRelPath, true, helper);
					}
				}
			}
			// オブジェクトがタグの場合の処理
			else if( helper.isTypeOfTag(object.getType()) ) {

				// duplicate check
				EIMObject sameNameObj = (EIMObject)fileNameMap.get(object.getName());
				// 同名のオブジェクトがある場合は例外
				if( sameNameObj != null ) {
					// エラーメッセージの作成 - <フォルダ/ドキュメント/タグのラベル>[<パス + オブジェクト名>]
					String path1 = TagUtil.getErrorMessageForDupNameErr(sess, object, helper);
					String path2 = TagUtil.getErrorMessageForDupNameErr(sess, sameNameObj, helper);

					String[] message = {path1, path2};
					throw new EIMException(sess, "EIM.ERROR.LOGIC.ZIPDL.DOCFOLNAME.SAME", message);
				}
				fileNameMap.put(object.getName(), object);	// ファイル名は格納しておく

				if(isFolTagRecursive) {	//タグに対して再帰処理を行う場合
					// タグ配下のオブジェクトを全て取得する
					TagTreeItem tagItems = TagUtil.getTagTreeWithChild(sess, object);
					if( tagItems != null && tagItems.getTreeItemList() != null ) {
						//相対パスの作成
						String tmpRelPath = relativePath + object.getName() + "/";
						//再帰処理
						tmpSize += getDocumentUnderTagRecursive(sess, tagItems.getTreeItemList(), bSignFile, fileArray, tmpRelPath, helper);
					}
				}
			}
		}

		// ドキュメントリンクの場合はファイル名に接頭辞を付ける
		Iterator k = tmpArray.iterator();
		for (; k.hasNext(); ) {
			InFileData tmpFileData = (InFileData)k.next();
			if( ((String)isDocLinkMap.get(tmpFileData)).equals("true") ) {
				String langId = sess.getLangId();
				String fileName = EIMConfig.get("DOCLINK_FILENAME_PREFIX_" + langId) + " - " + tmpFileData._file.getName();
				EIMFile fileTmp = tmpFileData._file;
				// 重複チェック
				while( true ) {
					EIMObject sameNameObj = (EIMObject)fileNameMap.get(fileName);
					// 同名のオブジェクトがある場合は例外
					if( sameNameObj != null ) {
						fileName = EIMConfig.get("DOCLINK_FILENAME_PREFIX_" + langId) + " - " + fileName;
						continue;
					}
					break;
				}
				// 重複の無い名称になったら、ファイル名を差し替え
				tmpFileData._file = new EIMFile( fileTmp.getFormat(), fileTmp.getDirectory(), fileName, fileTmp.getExt(), fileTmp.getSize());
			}
		}
		fileArray.addAll(tmpArray);
		return tmpSize;
	}

	/**
	 * 渡されてきたオブジェクトのリストからドキュメントを取得する再帰処理
	 *
	 * @param sess EIMSessionインスタンス
	 * @param tagChildObjs ドキュメント取得対象のタグ配下オブジェクト(TagTreeItem)のリスト
	 * @param prmIsDocumentLinks オブジェクトがドキュメントリンクのものかどうかを示すフラグのリスト
	 * @param bSignFile 署名・暗号化ファイルを取得するかどうか
	 * @param fileArray 最終的にZIPアーカイブ化するファイルのリスト(本関数の処理でファイルオブジェクトが追加される)
	 * @param relativePath アーカイブのトップからの相対パス
	 * @param helper AppObjectConditionHelperインスタンス
	 *
	 * @return 取得したドキュメントの合計サイズ
	 * @throws Exception
	 */
	private long getDocumentUnderTagRecursive(EIMSession sess, List tagChildObjs, boolean bSignFile, ArrayList fileArray, String relativePath, AppObjectConditionHelper helper) throws Exception {

		long tmpSize = 0;
		List childOids = new ArrayList();
		List childIsDocLinks = new ArrayList();

		Iterator i = tagChildObjs.iterator();
		for (; i.hasNext(); ) {
			TagTreeItem childItem = (TagTreeItem)i.next();
			EIMObject object = childItem.getEimObject();

			// フォルダ・タグの場合はTagTreeItem._treeItemListについて再帰処理
			if( (helper.isTypeOfFolder(object.getType()) || helper.isTypeOfTag(object.getType())) &&
					childItem.getTreeItemList() != null) {
				//相対パスの作成
				String tmpRelPath = relativePath + object.getName() + "/";
				//再帰処理
				tmpSize += getDocumentUnderTagRecursive(sess, childItem.getTreeItemList(), bSignFile, fileArray, tmpRelPath, helper);
			}

			//リストに格納
			childOids.add( String.valueOf(object.getId()) );
			childIsDocLinks.add("false");	//タグ配下にはリンクは存在しない
		}

		//作成したリストについてファイルを取得(タグ配下の処理なので、フォルダ・タグについて再帰処理はさせない)
		if( childOids.size() > 0 ) {
			tmpSize += getDocumentFromListRecursive(sess, childOids, childIsDocLinks, bSignFile, fileArray, relativePath, false, helper);
		}
		return tmpSize;
	}

	/**
	 * ファイルをZIP形式で圧縮
	 * @param os		圧縮結果を出力する OutputStream
	 * @param files	圧縮するファイル情報群(InFileData の ArrayList)
	 * @throws IOException	圧縮に失敗
	*/
	private void deflate(OutputStream os, ArrayList files) throws IOException {

		ZipOutputStream zipOut = new ZipOutputStream(new BufferedOutputStream(os), Charset.forName("MS932"));
        zipOut.setLevel(Deflater.DEFAULT_COMPRESSION);

        //
        for (int ii = 0; ii < files.size(); ii++) {
        	entryFile(zipOut, (InFileData)files.get(ii));
        }
        zipOut.close();
	}

	/**
	 * ZIPにファイルを登録
	 * @param zipOut	ZipOutputStream
	 * @param fileData	エントリするファイルデータ
	 * @throws IOException	エントリの登録に失敗
	*/
	private void entryFile(final ZipOutputStream zipOut, InFileData fileData) throws IOException {

		ZipEntry entry = new ZipEntry(fileData._relPath + fileData._file.getName());
		zipOut.putNextEntry(entry);

	    long totalSize = 0;
		FileInputStream fin = new FileInputStream(fileData._substance);
	    if (fin != null) {
	    	byte buf[] = new byte[READ_BUF_SIZE];
	    	int readSize;
	    	CheckedInputStream in = new CheckedInputStream(new BufferedInputStream(fin), new CRC32());
	    	while ((readSize = in.read(buf, 0, READ_BUF_SIZE)) != -1) {
	    		totalSize += (long)readSize;
	    		zipOut.write(buf, 0, readSize);
	    	}
	    	in.close();
	    	entry.setCrc(in.getChecksum().getValue());
	    }

	    entry.setSize(totalSize);
	    entry.setCompressedSize(totalSize);
	    zipOut.closeEntry();
	}

	// ファイルデータ格納用内部クラス
	private class InFileData {

		/** EIMファイルオブジェクト */
		public EIMFile _file;
		/** ファイル */
		public File _substance;
		/** 相対パス */
		public String _relPath;

		InFileData(EIMFile file, File substance, String relPath){
			_file = file;
			_substance = substance;
			_relPath = relPath;
		}
	}
}