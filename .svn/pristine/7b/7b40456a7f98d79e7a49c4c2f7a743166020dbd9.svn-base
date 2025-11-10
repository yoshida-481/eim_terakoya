package common.util;

import java.io.OutputStreamWriter;
import java.io.PrintWriter;

import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.http.HttpServletResponse;

import eim.bo.*;
import eim.net.EIMSession;
import eim.util.*;

/**
 * CSVファイル出力関連　共通クラス
 */
public class CsvOutputUtil {

	/**
	 * 設定ファイル(config_doc.properties)内、CSVファイル出力時文字コードを取得します。
	 * UTF8_BOM有とする設定値の場合、UTF-8を返却します。
	 * 
	 * @param sess セッション
	 * @return CSVファイル出力時文字コード
	 * @throws Exception
	 */
	public static String getCharCodeSetting(EIMSession sess) throws Exception {

		String charset;
		String settingCharset = EIMConfig.get("CSV_DOWNLOAD_CHARSET");
		if (StringUtils.isBlank(settingCharset)) {
			throw new EIMException(sess , "EIM.ERROR.LOGIC.CSVDOWNLOAD.CHARSET.NOTFOUND");

		} else if (AppConstant.CSV_OUTPUT_UTF8_BOM.equals(settingCharset)) {
			charset = AppConstant.CSV_OUTPUT_UTF8;

		} else {
			charset = settingCharset;
		}
		return charset;
	}

	/**
	 * PrintWriterの初期化を行います。
	 * 
	 * @param response HttpServletResponse
	 * @return PrintWriter
	 * @throws Exception
	 */
	public static PrintWriter setOutputStream(HttpServletResponse response) throws Exception {

		PrintWriter out = null;
		if (AppConstant.CSV_OUTPUT_UTF8_BOM.equals(EIMConfig.get("CSV_DOWNLOAD_CHARSET"))) {
			
			ServletOutputStream  ss = response.getOutputStream();
			ss.write(0xef);
			ss.write(0xbb);
			ss.write(0xbf);
			OutputStreamWriter osw = new OutputStreamWriter(ss, AppConstant.CSV_OUTPUT_UTF8);
			out = new PrintWriter(osw);

		} else {
			
			// 出力ストリーム取得
			out = response.getWriter();
		}
		return out;
	}
}
