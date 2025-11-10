package servlet.dl;

import java.io.BufferedInputStream;
import java.io.BufferedWriter;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.nio.charset.Charset;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import org.apache.commons.io.FileUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import common.util.AppConstant;
import common.util.AppMessageUtils;
import eim.bo.EIMException;
import eim.bo.EIMResource;
import eim.bo.EIMUser;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.EIMUtils;
import eim.util.StringUtils;


/**
 * Urlリンクファイルのダウンロード処理を実施するDownloadサーブレットです。
 *
 */
public class DownLoadUrlLinkFile extends HttpServlet {

	/**
	 * シリアルバージョンUID
	 */
	private static final long serialVersionUID = 9146574969757543414L;

	/** フォワード先JSP定義 */
	private final String ERROR_PAGE = "/servlet/HtmlErrorMessageServlet";

	/** エラーメッセージ定義 */
	private final String ERROR_ATTR_KEY = "errorMessage";

	/** 出力ログ */
	Log log = LogFactory.getLog(this.getClass().getName());

	/**
	 * URLリンクのダウンロード区分：ドキュメント
	 */
	private static final String URLLINK_KIND_DOCUMENT = "1";

	/**
	 * URLリンクのダウンロード区分：原本
	 */
	private static final String URLLINK_KIND_ORIGINAL = "2";

	/**
	 * URLリンクのダウンロード区分：公開
	 */
	private static final String URLLINK_KIND_PUBLIC = "3";

	/**
	 *
	 * .urlファイルを生成します。
	 * @param request HttpServletRequest
	 * @param response HttpServletResponse
	 */
	private void createUrlLinkFile(HttpServletRequest request, HttpServletResponse response)
		throws ServletException, IOException
	{
		EIMSession sess = null;
		String message = null;
		EIMUser loginUser = null;

		try
		{
			String charset;
			String newline;

			// セッション取得
			sess = EIMUtils.getSession(request);
			if(sess == null)
			{
				message = EIMResource.getMessage(request, "EIM.ERROR.SESSIONTIMEOUT");
				log.warn(AppMessageUtils.makeLogMessage(message));
				request.setAttribute(ERROR_ATTR_KEY, message);
				getServletContext().getRequestDispatcher(ERROR_PAGE).forward(request, response);
				return;
			}

			// ログインユーザ情報取得
			loginUser = (EIMUser)sess.getAttribute("USER");

			// 出力文字コード取得
			charset = AppConstant.ENCODE_UTF8;

			if(StringUtils.isBlank(charset))
			{
				throw new EIMException(sess ,
						"EIM.ERROR.LOGIC.CSVDOWNLOAD.CHARSET.NOTFOUND");
			}

			// 出力改行コード取得
			newline = EIMConfig.get("CSV_DOWNLOAD_NEWLINE");
			if(newline == null ||
				"".equals(newline))
			{
				throw new EIMException(sess ,
						"EIM.ERROR.LOGIC.CSVDOWNLOAD.NEWLINE.NOTFOUND");
			}

			// リクエストデータ取得
			String searchResultOrg	= EIMUtils.getParameter(request, "searchResult");

			// Flex側でXMLエンコーディングされない制御文字を除去する
			String searchResult = searchResultOrg.replaceAll("[\\00-\\x1f\\x7f]", "");

			//脆弱性対応(リクエストが改竄されてないか確認)
			if (searchResult.startsWith("<urlLinkList>") && searchResult.endsWith("</urlLinkList>")
				&& searchResult.indexOf("urlString=\"http") > 0 && (searchResult.indexOf("client/#/documents/login?objId=") > 0
					|| searchResult.indexOf("servlet/DownloadPublicLatestDocument?objId=") > 0 || searchResult.indexOf("servlet/DownloadPrivateLatestDocument?objId=") > 0
					|| searchResult.indexOf("client/#/portals/main/workspaces") > 0)) {
				//何もしない
			} else {
				throw new EIMException(sess ,"EIM.ERROR.LOGIC.ILLEGAL.REQUEST");
			}

			// パーサ確保
			javax.xml.parsers.DocumentBuilderFactory bdbf;
			javax.xml.parsers.DocumentBuilder bdb;
			bdbf = javax.xml.parsers.DocumentBuilderFactory.newInstance();
			bdb= bdbf.newDocumentBuilder();

			// データ情報XML解析
			Document ddoc;
			ddoc = bdb.parse( new ByteArrayInputStream(searchResult.getBytes(AppConstant.CSV_XMLENCODING)) );

			// データ行出力
			Element root = ddoc.getDocumentElement();
			NodeList objList = root.getElementsByTagName("urlData");

			// データ抽出
			int dataSize = objList.getLength();

			// 対象のリンクが複数の場合はzip圧縮
			if ( dataSize > 1) {

				// zip圧縮処理
				createZipDownLoad(sess, response, charset, newline, objList, dataSize, loginUser.getId());

			} else if (dataSize == 1) {

				// 1件の場合は.urlでダウンロード
				createUrlLinkDownLoad(sess, response, charset, newline, objList);

			} else {

				// 0件の場合はありえないので、システムエラーとする
				throw new Exception();
			}

		}
		catch(EIMException eime)
		{
			// 出力先情報を元に戻す。
			response.setContentType(null);
			response.setHeader("Content-Disposition", null);

			// エラー情報を出力する。
			message = eime.getMessage();
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message), eime);
			message = message.replaceAll("\n","\\\\n");
			request.setAttribute(ERROR_ATTR_KEY, message);
			getServletContext().getRequestDispatcher(ERROR_PAGE).forward(request, response);
		}
		catch(Exception e)
		{
			// 出力先情報を元に戻す。
			response.setContentType(null);
			response.setHeader("Content-Disposition", null);

			// エラー情報を出力する。
			log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
			message = EIMResource.getMessage("EIM.ERROR.SYSTEMERROR");
			request.setAttribute(ERROR_ATTR_KEY, message);
			getServletContext().getRequestDispatcher(ERROR_PAGE).forward(request, response);
		}
		finally
		{
			try
			{
				// セッションクローズ
				if(sess != null)
					sess.close();

			}
			catch (Exception se)
			{
				// 出力先情報を元に戻す。
				response.setContentType(null);
				response.setHeader("Content-Disposition", null);

				// エラー情報を出力する。
				log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
				message = EIMResource.getMessage("EIM.ERROR.SYSTEMERROR");
				request.setAttribute(ERROR_ATTR_KEY, message);
				getServletContext().getRequestDispatcher(ERROR_PAGE).forward(request, response);
			}
		}
	}

	/**
	 * URLリンクファイルをダウンロード
	 *
	 * @param sess セッション
	 * @param response レスポンス
	 * @param charset キャラセット
	 * @param newline 改行文字
	 * @param objList データのリスト
	 * @throws Exception 例外
	 */
	private void createUrlLinkDownLoad(EIMSession sess, HttpServletResponse response,
			String charset, String newline, NodeList objList) throws Exception {

		PrintWriter out = null;
		try {
			// 出力ストリーム取得
			out = response.getWriter();
			// urlリンクが1件の場合は.URLの形でダウンロード
			Element elm = (Element)objList.item(0);

			// 出力ファイル名
			String fileName = getFileName(sess, elm);
			// ファイル情報をヘッダにセットする。
			setResponseHeader(response, charset, fileName);

			StringBuilder fileData = getUrlFileData(newline, elm);

			// 出力
			out.print(fileData.toString());

		}catch (Exception e) {
			throw e;

		} finally  {
			// 出力ファイルクローズ
			if(out != null)
				out.close();
		}
	}

	/**
	 * URLリンクファイル名を作成
	 * @parm sess セッション
	 * @param elm URLリンクデータ
	 * @return ファイル名
	 */
	private String getFileName(EIMSession sess, Element elm) {
		String fileName = elm.getAttribute("linkFileName");
		String kind = elm.getAttribute("kind");

		String fileSufix = "";

		if (URLLINK_KIND_DOCUMENT.equals(kind)) {
			//ドキュメント
			fileSufix = EIMResource.getMessage(sess, "EIM.LABEL.URLLINK.FILE.SUFIX");

		} else if (URLLINK_KIND_ORIGINAL.equals(kind)){
			// 原本
			fileSufix = EIMResource.getMessage(sess, "EIM.LABEL.URLLINK.ORIGINALFILE.SUFIX");

		} else if (URLLINK_KIND_PUBLIC.equals(kind)){
			// 公開
			fileSufix = EIMResource.getMessage(sess, "EIM.LABEL.URLLINK.PUBLICFILE.SUFIX");

		} else {
			fileSufix = EIMResource.getMessage(sess, "EIM.LABEL.URLLINK.FILE.SUFIX");
		}

		fileName = fileName + fileSufix;

		return fileName;
	}

	/**
	 * 複数のURLリンクファイルをダウンロードする場合、zipに固めてダウンロードを実行
	 *
	 * @param sess セッション
	 * @param response レスポンス
	 * @param charset キャラセット
	 * @param newline 改行文字
	 * @param objList データのリスト
	 * @param dataSize データサイズ
	 * @throws IOException 例外
	 */
	private void createZipDownLoad(EIMSession sess, HttpServletResponse response,
			String charset, String newline, NodeList objList, int dataSize, long userId) throws Exception
			 {

		Date date = new Date();
		SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMddHHmmss");
		String ymdStr = sdf.format(date);

		String tempDir = EIMConfig.getValue("TEMP"); //サーバの添付ディレクトリ

		// ダウンロードファイル用作業ディレクトリ  添付ディレクトリパス + ユーザ名＋日付
		String workDir = tempDir + String.valueOf(userId) + ymdStr;

		ZipOutputStream zos = null;
		BufferedWriter bw = null;
		InputStream in = null;

		String fileName = "FILES_" + ymdStr + ".zip";
		// ファイル情報をヘッダにセットする。
		setResponseHeader(response, charset, fileName);

		try {
			zos = new ZipOutputStream(response.getOutputStream(), Charset.forName("MS932"));

			// ワークディレクトリ作成
			File dir = new File(workDir);
			dir.mkdir();

			// データ行出力
			for (int i = 0; i < dataSize; i++) {

				Element elm = (Element) objList.item(i);

				// 1件分のurlリンクファイルデータを取得
				StringBuilder fileData = getUrlFileData(newline, elm);

				// ファイル出力
				File file = new File(workDir + File.separator + getFileName(sess, elm));
				bw = new BufferedWriter(new FileWriter(file));
				bw.write(fileData.toString());
				bw.close();

				// zipファイルへの圧縮
				in = new BufferedInputStream(new FileInputStream(file));
				// ファイルをzip形式に圧縮するためにセット
				zos.putNextEntry(new ZipEntry(file.getName()));

				byte[] b = new byte[1024];
				int len;
				while ((len = in.read(b)) != -1) {
					// zip形式で書き込む
					zos.write(b, 0, len);
				}
				zos.closeEntry();
				in.close();
			}

		} catch (Exception e) {

			throw e;
		} finally {

			if (bw != null) {
				bw.close();
			}

			if (in != null) {
				in.close();
			}

			if (zos != null) {
				zos.close();
			}

			// zipファイル生成用一時ディレクトリをディレクトリごと削除
			File delFile = new File(workDir);
			FileUtils.deleteDirectory(delFile);

		}
	}

	/**
	 * ファイル情報をヘッダにセットする
	 * @param response レスポンス
	 * @param charset キャラセット
	 * @param fileName ダウンロードファイル名
	 * @throws UnsupportedEncodingException 例外
	 */
	private void setResponseHeader(HttpServletResponse response,
			String charset, String fileName) throws UnsupportedEncodingException {

		// ファイル情報ヘッダセット
		response.setHeader("Cache-Control", "max-age=0, must-revalidate");
		response.setContentType("application/octet-stream;charset=" + charset);

		response.setHeader("Content-Disposition", "attachment; filename=\""
				+ URLEncoder.encode(fileName, charset).replace("+", "%20") + "\"; filename*="
				+ AppConstant.ENCODE_UTF8 + "\'\'"
				+ URLEncoder.encode(fileName, AppConstant.ENCODE_UTF8).replace("+", "%20"));
	}

	/**
	 * URLリンクファイルのデータを取得する
	 *
	 * @param newline 改行文字
	 * @param elm urlリンクを作成するため、クライアントから送信されたXMLデータ1件分
	 * @return ファイルデータ
	 */
	private StringBuilder getUrlFileData(String newline, Element elm) {
		StringBuilder fileData = new StringBuilder();

		// urlLink
		fileData.append("[InternetShortcut]");
		fileData.append(newline);
		fileData.append("URL=");
		fileData.append(elm.getAttribute("urlString"));
		return fileData;
	}

	/**
	 * GET処理<br>
	 * createCsvを呼び出します。
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
		createUrlLinkFile(request, response);
	}

	/**
	 * POST処理<br>
	 * createCsvを呼び出します。
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
		createUrlLinkFile(request, response);
	}

}