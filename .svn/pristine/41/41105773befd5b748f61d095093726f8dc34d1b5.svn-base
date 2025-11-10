package servlet.dl;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import common.util.AppConstant;
import common.util.AppMessageUtils;
import common.util.CsvOutputUtil;
import eim.bo.EIMException;
import eim.bo.EIMResource;
import eim.bo.EIMUser;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.EIMConstant;
import eim.util.EIMUtils;
import eim.util.OperationHistoryUtils;
import eim.util.StringUtils;


/**
 * CSVファイルのダウンロード処理を実施するDownloadサーブレットの親クラスです。
 * <p>
 * このクラスは
 * <code>{@link jakarta.servlet.http.HttpServlet}</code>
 * を拡張している。
 *
 */
abstract class AbstractDownloadCSVFile extends HttpServlet {

	/** シリアルバージョンUID */
	private static final long serialVersionUID = 1L;

	/** フォワード先JSP定義 */
	private final String ERROR_PAGE = "/servlet/HtmlErrorMessageServlet";

	/** エラーメッセージ定義 */
	public final String ERROR_ATTR_KEY = "errorMessage";

	/** エスケープ済ダブルクォーテーション */
	private static final String ESCAPE_DQ = AppConstant.CSV_ESCDQUOTATION + "\"";

	/** 出力ログ */
	Log log = LogFactory.getLog(this.getClass().getName());

	/**
	 * 出力ファイル名を作成します。
	 * @param sess EIMSession
	 * @return ファイル名
	 * @throws Exception
	 */
	abstract String getFileName(EIMSession sess) throws Exception;

	/**
	 * ヘッダ行を生成します。
	 * @param sess EIMSession
	 * @param keyList キー格納リスト
	 * @param doc Document
	 * @param outputUserMail メールアドレス出力フラグ
	 * @return header 作成したヘッダ行
	 */
	abstract String makeHeader(EIMSession sess, HttpServletRequest request, javax.xml.parsers.DocumentBuilder bdb, List<String> keyList) throws Exception;

	/**
	 * データ行を生成します。
	 * @param objName オブジェクト名
	 * @param elm Element
	 * @param keyList キー格納配列
	 * @return header 作成したデータ行リスト
	 */
	abstract List<String> makeDataRowList(EIMSession sess, HttpServletRequest request, javax.xml.parsers.DocumentBuilder bdb, List<String> keyList) throws Exception;

	/**
	 * CSVファイルを生成します。
	 * @param request HttpServletRequest
	 * @param response HttpServletResponse
	 *
	 * @throws ServletException
	 * @throws IOException
	 */
	public void createCsv(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

		EIMSession sess = null;
		String message = null;
		EIMUser loginUser = null;
		PrintWriter out = null;

		try {
			// セッション取得
			sess = EIMUtils.getSession(request);
			if (sess == null) {
				message = EIMResource.getMessage(request, "EIM.ERROR.SESSIONTIMEOUT");
				log.warn(AppMessageUtils.makeLogMessage(message));
				request.setAttribute(ERROR_ATTR_KEY, message);
				getServletContext().getRequestDispatcher(ERROR_PAGE).forward(request, response);
				return;
			}

			String charset;
			String newline;

			// ログインユーザ情報取得
			loginUser = (EIMUser)sess.getAttribute("USER");

			// ファイル名生成
			String outputFileName = getFileName(sess);

			// 出力文字コード取得
			charset = CsvOutputUtil.getCharCodeSetting(sess);

			// 出力改行コード取得
			newline = EIMConfig.get("CSV_DOWNLOAD_NEWLINE");
			if (newline == null || newline.equals("")) {
				throw new EIMException(sess, "EIM.ERROR.LOGIC.CSVDOWNLOAD.NEWLINE.NOTFOUND");
			}

			// パーサ確保
			javax.xml.parsers.DocumentBuilderFactory bdbf;
			javax.xml.parsers.DocumentBuilder bdb;
			bdbf = javax.xml.parsers.DocumentBuilderFactory.newInstance();
			bdb= bdbf.newDocumentBuilder();

			// ヘッダ行生成
			List<String> keyList = new ArrayList<String>();
			String header = makeHeader(sess, request, bdb, keyList);

			// データ行生成
			List<String> dataList = makeDataRowList(sess, request, bdb, keyList);

			// ファイル情報ヘッダセット
			response.setHeader("Cache-Control", "max-age=0, must-revalidate");
			response.setContentType("application/octet-stream;charset=" + charset);
			response.setHeader("Content-Disposition", "attachment; filename=\"" + outputFileName + "\"");

			// 出力ストリーム取得
//			out = response.getWriter();
			out = CsvOutputUtil.setOutputStream(response);

			// CSVファイルへ情報出力
			writeCSV(out, header, dataList, newline);

			// 操作履歴
			boolean isAdmin = (sess.getAttribute("ADMIN_APP_ID") != null);
			String applicationType = isAdmin ? AppConstant.SYSTEM : AppConstant.DOCUMENT;

			OperationHistoryUtils.create(sess, applicationType, AppConstant.DOWNLOAD_CSV,
					EIMConstant.TARGET_DOWNLOAD, EIMConstant.DIRECTORY, outputFileName,
					null, null, null, null);

			//Commit
			sess.commit();

			// セッションクローズ・出力先クローズはfinally句で行う。

		} catch (EIMException eime) {
			// 出力先情報を元に戻す。
			response.setContentType(null);
			response.setHeader("Content-Disposition", null);

			// エラー情報を出力する。
			message = eime.getMessage();
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message), eime);
			message = message.replaceAll("\n","\\\\n");
			request.setAttribute(ERROR_ATTR_KEY, message);
			getServletContext().getRequestDispatcher(ERROR_PAGE).forward(request, response);

		} catch (Exception e) {
			// 出力先情報を元に戻す。
			response.setContentType(null);
			response.setHeader("Content-Disposition", null);

			// エラー情報を出力する。
			log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
			message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
			request.setAttribute(ERROR_ATTR_KEY, message);
			getServletContext().getRequestDispatcher(ERROR_PAGE).forward(request, response);

		} finally {
			try {
				// セッションクローズ
				if (sess != null)
					sess.close();

				// 出力ファイルクローズ
				if (out != null)
					out.close();

			} catch (Exception se) {
				// 出力先情報を元に戻す。
				response.setContentType(null);
				response.setHeader("Content-Disposition", null);

				// エラー情報を出力する。
				log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
				message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
				request.setAttribute(ERROR_ATTR_KEY, message);
				getServletContext().getRequestDispatcher(ERROR_PAGE).forward(request, response);
			}
		}
	}

	/**
	 * CSVファイルにヘッダ、データ行を出力します。
	 * @param out PrintWriter
	 * @param dataList 出力データ行リスト
	 */
	private void writeCSV(PrintWriter out, String header, List<String> dataList, String newline) {

		if (!StringUtils.isBlank(header)) {
			out.print(header + newline);
		}
		for (String dataStr : dataList) {
			out.print(dataStr + newline);
		}
	}

	/**
	 * 文字列をエスケープする。<BR>
	 * エスケープ方法は、Excel2003に従う。
	 * @param str String
	 *
	 * @return String エスケープしてダブルクォーテーションで囲んだ文字列
	 */
	public String escString(String str) {
		if (str == null)
			return "\"\"";

		str = StringUtils.convertReturnCede(str);
		str = str.replaceAll("\"", ESCAPE_DQ);

		return "\"" + str + "\"";
	}


	/**
	 * GET処理<br>
	 * createCsvを呼び出します。
	 * @param request HttpServletRequest
	 * @param response HttpServletResponse
	 *
	 * @throws IOException
	 * @throws ServletException
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
		createCsv(request, response);
	}

	/**
	 * POST処理<br>
	 * createCsvを呼び出します。
	 * @param request
	 * @param response
	 *
	 * @throws IOException
	 * @throws ServletException
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
		createCsv(request, response);
	}
}
