package servlet.dl;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URLEncoder;
import java.util.List;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import common.util.AppConstant;
import common.util.AppMessageUtils;
import common.util.AppObjectConditionHelper;
import common.util.AppObjectUtil;
import eim.bo.EIMException;
import eim.bo.EIMFile;
import eim.bo.EIMFormat;
import eim.bo.EIMObject;
import eim.bo.EIMResource;
import eim.bo.EIMUser;
import eim.bo.EIMVersion;
import eim.net.EIMSession;
import eim.util.AccessUtils;
import eim.util.EIMConfig;
import eim.util.EIMConstant;
import eim.util.EIMUtils;
import eim.util.FileUtils;
import eim.util.ObjectUtils;
import eim.util.OperationHistoryUtils;
import eim.util.StringUtils;
import eim.util.VersionUtils;

/**
 * ドキュメントオブジェクトをダウンロードするDownloadサーブレットのベースクラス。
 * <p>
 * このクラスは
 * <code>{@link jakarta.servlet.http.HttpServlet}</code>
 * を拡張している。
 *
 * @version	1.0.0
 * @since		1.0.0
 */
abstract class AbstractDownloadLatestDocument extends HttpServlet
{
	/** フォワード先JSP定義 */
	private final String ERROR_PAGE = "/servlet/HtmlErrorMessageServlet";
	private final String SESSION_ERROR_PAGE = "/servlet/DocumentHtmlErrorMessageServlet";

	/** エラーメッセージ定義 */
	private final String ERROR_ATTR_KEY = "errorMessage";

	private final String PUBLICATION_TYPE_KEY = "publicationType";

	private int READ_BUF_SIZE = 1;

	/**
	 * ダウンロード処理実行に必要なアクセス権限をチェックする。
	 *
	 * @param session EIMセッション
	 * @param object EIMオブジェクト
	 * @param helper
	 * @return boolean チェック結果
	 * @throws Exception 例外
	 */
	abstract boolean checkAccessRight(EIMSession session, EIMObject object, AppObjectConditionHelper helper) throws Exception;

	/**
	 * ダウンロード対象のオブジェクトを取得する。
	 *
	 * @param session EIMセッション
	 * @param object EIMオブジェクト
	 * @param helper
	 * @return EIMObject オブジェクト
	 * @throws Exception 例外
	 */
	@SuppressWarnings("unchecked")
	protected EIMObject getAlternateObject(EIMSession session, EIMObject object, AppObjectConditionHelper helper) throws Exception {

		EIMVersion version = VersionUtils.getVersion(session, object);

		List<EIMObject> objectList = version.getList();

		if(objectList.size() == 1){
			// 1の場合
			return object;
		}else if(objectList.size() > 1){
			// 1より多い場合

			// 最新履歴番号オブジェクトを取得
			EIMObject latestObject = version.getLatest();
			// 最新履歴番号の１つ前のオブジェクトを取得
			EIMObject latestPrevObject = version.getObjectByRev(latestObject.getRev() -1);

			// １つ前のオブジェクトにロックユーザーが設定されている場合は
			// １つ前のオブジェクトを対象とする
			if(latestPrevObject.getLockUser() != null && latestPrevObject.getLockDate() != null){

				// 引数と最新履歴番号のオブジェクトが同じ場合は
				// 最新履歴番号オブジェクトを対象とする。
				if(object.getId() == latestObject.getId()){
					return latestObject;
				}else{
					return latestPrevObject;
				}

			}else{
				// 設定されていない場合は最新履歴番号のオブジェクトを対象とする
				return latestObject;
			}
		}

		return object;
	}

	/**
	 * フォーマットを取得する。
	 *
	 * @param session EIMセッション
	 * @param object EIMオブジェクト
	 * @return EIMFormat フォーマット
	 * @throws Exception 例外
	 */
	abstract EIMFormat getFormat(EIMSession session, EIMObject object) throws Exception;

	abstract String getPublicationType() throws Exception;

	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException
	{
		// Error Logging
		Log log = LogFactory.getLog(this.getClass().getName());

		// Session
		EIMSession sess = null;
		EIMUser loginUser = null;

		// Parameter
		String prmObjId = request.getParameter("objId");

		//Message
		String message = null;
		Object[] paramId = {
				"objId=" + prmObjId
				};

		// Cache
		response.setHeader("Cache-Control", "max-age=0, must-revalidate");

		try { READ_BUF_SIZE = Integer.parseInt(EIMConfig.get("FILE.DOWNLOAD.READBYTE.LENGTH")); } catch(NumberFormatException nfe) {}
		try {

			// EIMセッションチェック
			sess = authenticate(request);
			if (sess == null) {
				// ダウンロードエラーページ経由でログイン画面に遷移させる
				message = EIMResource.getMessage(request, "EIM.ERROR.LOGIC.DIRECTDOWNLOAD.NO.LOGIN");
				log.warn(AppMessageUtils.makeLogMessage(message));
				request.setAttribute("objId", prmObjId);
				request.setAttribute("appId", "html");
				request.setAttribute(ERROR_ATTR_KEY, message);
				request.setAttribute(PUBLICATION_TYPE_KEY, getPublicationType());
				getServletContext().getRequestDispatcher(SESSION_ERROR_PAGE).forward(request, response);
				return;
			}

			//User
			loginUser = (EIMUser)sess.getAttribute("USER");

			//Helper
			AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

			// オブジェクトの存在チェック
			EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
			if(object == null)
			{
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODOCUMENT");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				request.setAttribute(ERROR_ATTR_KEY, message);
				getServletContext().getRequestDispatcher(ERROR_PAGE).forward(request, response);
				return;
			}

			// ダウンロード対象ドキュメントを取得
			EIMObject alternateObject = getAlternateObject(sess, object, helper);

			// 権限チェック
			boolean check = checkAccessRight(sess, alternateObject, helper);
			if (check == false) {
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOACCESS");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				request.setAttribute(ERROR_ATTR_KEY, message);
				getServletContext().getRequestDispatcher(ERROR_PAGE).forward(request, response);
				return;
			}

			// オブジェクトタイプがドキュメントの子孫であるかをチェック
			if(!helper.isTypeOfDocument(object.getType())){
				// 子孫でない場合
				// 「ドキュメントを指定して下さい。」
				message = EIMResource.getMessage(sess, "EIM.ERROR.INPUT.NODOCUMENT");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				request.setAttribute(ERROR_ATTR_KEY, message);
				getServletContext().getRequestDispatcher(ERROR_PAGE).forward(request, response);
				return;
			}

			// 以下、ダウンロードに必要な処理

			// フォーマットを取得
			EIMFormat format = getFormat(sess, alternateObject);

			// EIMファイルを取得
			EIMFile file = FileUtils.getFile(sess, alternateObject, format);
			if(file == null)
			{
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODOCUMENT");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				request.setAttribute(ERROR_ATTR_KEY, message);
				getServletContext().getRequestDispatcher(ERROR_PAGE).forward(request, response);
				return;
			}

			//Substance
			File substance = null;
			FileUtils.prepareFileAccess(sess, alternateObject, file);
			substance = new File(FileUtils.getFilePath(alternateObject, file));
			if(!substance.exists())
			{
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODOCUMENT");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				request.setAttribute(ERROR_ATTR_KEY, message);
				getServletContext().getRequestDispatcher(ERROR_PAGE).forward(request, response);
				return;
			}

			// アクセス履歴
			AccessUtils.createAccess(sess, alternateObject, "EIM.ACCESS.TYPE.DOWNLOAD");

			// パス
			String path = StringUtils.nullToBlank(AppObjectUtil.getPath(object));

			// 操作履歴
			OperationHistoryUtils.create(sess, common.util.AppConstant.DOCUMENT, EIMConstant.DOWNLOAD_DOCUMENT,
					EIMConstant.TARGET_DOWNLOAD, EIMConstant.OBJECT, alternateObject,
					null, null, null, path);

			// ファイル名のエンコード
			String fileName = null;
			if (EIMConfig.get("DOWNLOAD_FILENAME_ENCODE_JAP_ONLY")
					.equals(AppConstant.DOWNLOAD_FILENAME_ENCODE_JAP_ONLY_ON)) {
				// MS932でエンコード
				// (英語Windowsで日本語ファイル名をダウンロードすると文字化け発生)
				fileName = new String(file.getName().getBytes(AppConstant.ENCODE_MS932), "ISO-8859-1");
			} else {
				// UTF-8でエンコード
				// (日本語ファイル名のファイルを「開く」とファイル名は文字化けしている。保存するときは問題なし。)
				fileName = URLEncoder.encode(file.getName(), AppConstant.ENCODE_UTF8).replace("+", "%20");
			}

			//OutputStream
			OutputStream out = response.getOutputStream();
			FileInputStream in = new FileInputStream(substance);
			response.setContentType("application/octet-stream");
			response.setHeader("Content-Disposition", "attachment; filename=\"" + fileName + "\"; filename*="
					+ AppConstant.ENCODE_UTF8 + "\'\'"
					+ URLEncoder.encode(file.getName(), AppConstant.ENCODE_UTF8).replace("+", "%20"));

			byte[] readBytes = new byte[READ_BUF_SIZE];
			int buff = 0;
			while((buff = in.read(readBytes)) != -1)
			{
				//out.write(readBytes);
				out.write(readBytes, 0, buff);
			}
			in.close();
			out.flush();
			out.close();

			//Commit
			sess.commit();

		}
		catch(EIMException eime)
		{
			try{
				if(sess != null){
					sess.rollback();
				}
			}
			catch (Exception se) {
				log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
				message = EIMResource.getMessage("EIM.ERROR.SYSTEMERROR");
				request.setAttribute(ERROR_ATTR_KEY, message);
				getServletContext().getRequestDispatcher(ERROR_PAGE).forward(request, response);
			}

			message = eime.getMessage();
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId), eime);
			request.setAttribute(ERROR_ATTR_KEY, message);
			getServletContext().getRequestDispatcher(ERROR_PAGE).forward(request, response);
		}
		catch(Exception e)
		{
			try{
				if(sess != null){
					sess.rollback();
				}
			}
			catch (Exception se) {
				log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
				message = EIMResource.getMessage("EIM.ERROR.SYSTEMERROR");
				request.setAttribute(ERROR_ATTR_KEY, message);
				getServletContext().getRequestDispatcher(ERROR_PAGE).forward(request, response);
			}
			log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
			message = EIMResource.getMessage("EIM.ERROR.SYSTEMERROR");
			request.setAttribute(ERROR_ATTR_KEY, message);
			getServletContext().getRequestDispatcher(ERROR_PAGE).forward(request, response);
		}
		finally
		{
			try{
				if(sess != null){
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

	protected void doGet(	HttpServletRequest	request,
												HttpServletResponse	response)
	throws ServletException, IOException
	{
		doPost(request, response);
	}

	private EIMSession authenticate(HttpServletRequest request) throws Exception
	{
		EIMSession sess = null;

		try {
			sess = EIMUtils.getSession(request);

			if(sess != null){
				// EIMセッションがある場合
				// EIMセッションを返却する
				return sess;
			} else {
				// EIMANAGER認証の場合

				// セッションが無いのでエラーとみなす
				return null;
			}

		} catch (Exception e) {
			throw e;
		}
	}

}