package servlet.dl;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URLEncoder;

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
import eim.bo.EIMAccessRole;
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
import eim.util.SecurityUtils;
import eim.util.StringUtils;
import jp.co.ctc_g.eim.framework2.common.exception.EIMException;

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
abstract class AbstractDownloadDocument extends HttpServlet
{
	// フォワード先JSP定義
	private final String ERROR_PAGE = "/servlet/HtmlErrorMessageServlet";

	// エラーメッセージ定義
	private final String ERROR_ATTR_KEY = "errorMessage";

	private int READ_BUF_SIZE = 1;

	/**
	 * ダウンロード処理実行に必要なアクセス権限をチェックする。
	 *
	 * @param session
	 * @param object
	 * @param helper
	 * @return boolean チェック結果
	 * @throws Exception
	 */
	abstract boolean checkAccessRight(EIMSession session, EIMObject object, AppObjectConditionHelper helper) throws Exception;

	/**
	 * ダウンロード対象のオブジェクトを取得する。
	 *
	 * @param session
	 * @param object
	 * @param helper
	 * @return EIMObject オブジェクト
	 * @throws Exception
	 */
	abstract EIMObject getAlternateObject(EIMSession session, EIMObject object, AppObjectConditionHelper helper) throws Exception;

	/**
	 * フォーマットを取得する。
	 *
	 * @param session
	 * @param object
	 * @return EIMFormat フォーマット
	 * @throws Exception
	 */
	abstract EIMFormat getFormat(EIMSession session, EIMObject object) throws Exception;

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

			//Object
			EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
			if(object == null || !SecurityUtils.authorized(sess, object, loginUser, EIMAccessRole.READ))
			{
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODOCUMENT");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				request.setAttribute(ERROR_ATTR_KEY, message);
				getServletContext().getRequestDispatcher(ERROR_PAGE).forward(request, response);
				return;
			}

			// Check Access Right
			boolean check = checkAccessRight(sess, object, helper);
			if (check == false) {
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOACCESS");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				request.setAttribute(ERROR_ATTR_KEY, message);
				getServletContext().getRequestDispatcher(ERROR_PAGE).forward(request, response);
				return;
			}

			// Alternate Object
			EIMObject alternateObject = getAlternateObject(sess, object, helper);

			//Format
			EIMFormat format = getFormat(sess, alternateObject);

			//File
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

			//Access
			AccessUtils.createAccess(sess, object, "EIM.ACCESS.TYPE.DOWNLOAD");

			//パス
			String path = StringUtils.nullToBlank(AppObjectUtil.getPath(object));

			//Create Operation History
			OperationHistoryUtils.create(sess, common.util.AppConstant.DOCUMENT, EIMConstant.DOWNLOAD_DOCUMENT,
					EIMConstant.TARGET_DOWNLOAD, EIMConstant.OBJECT, object,
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
}