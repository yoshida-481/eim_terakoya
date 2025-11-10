package servlet.dl;

import java.io.ByteArrayInputStream;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import jakarta.servlet.http.HttpServletRequest;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import common.util.AppConstant;
import eim.bo.EIMException;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.EIMUtils;
import eim.util.StringUtils;


/**
 * CSVファイルのダウンロード処理を実施するDownloadサーブレットです。
 */
public class DownLoadOpeHistCSVFile extends AbstractDownloadCSVFile {

	/**
	 * シリアルバージョンUID
	 */
	private static final long serialVersionUID = -6913167295834449281L;

	/**
	 * 出力ファイル名を作成します。
	 * @param sess EIMSession
	 * @return ファイル名
	 * @throws Exception
	 */
	String getFileName(EIMSession sess) throws Exception {

		//ファイル名ヘッダ取得
		String fileheader = EIMConfig.get("CSV_DOWNLOAD_HISTORY_FILEHEADER");
		if (StringUtils.isBlank(fileheader)) {
			throw new EIMException(sess, "EIM.ERROR.LOGIC.CSVDOWNLOAD.HEADER.NOTFOUND");
		}
		
		//現在日付取得
		Date date = new Date();
		SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMddHHmmss");
		
		//ファイル名作成し返却
		return fileheader + sdf.format(date) + ".csv";
	}

	/**
	 * ヘッダ行を生成します。
	 * @param sess EIMSession
	 * @param keyList キー格納リスト
	 * @param doc Document
	 * @param outputUserMail メールアドレス出力フラグ
	 * @return header 作成したヘッダ行
	 */
	String makeHeader(EIMSession sess, HttpServletRequest request, javax.xml.parsers.DocumentBuilder bdb, List<String> keyList) throws Exception {

		String header = "";

		String listTitle = EIMUtils.getParameter(request, "listTitle");
		Document tdoc = bdb.parse(new ByteArrayInputStream(listTitle.getBytes(AppConstant.CSV_XMLENCODING)));

		// ルート要素を取得
		Element root = tdoc.getDocumentElement();
		
		// "FIELD"要素のリストを取得
		NodeList nd = root.getElementsByTagName("FIELD");
		
		for (int i = 0; i < nd.getLength(); i++) {
			// デリミタ出力
			if (i > 0)
				header += AppConstant.CSV_COLDELIMITER;
			
			// タイトル情報を出力
			Element element = (Element)nd.item(i);
			header += "\"" + element.getAttribute("label") + "\"";
			
			// 配列に実装列名格納
			keyList.add(element.getAttribute("id"));
		}
		
		return header;
	}

	/**
	 * データ行を生成します。
	 * @param objName オブジェクト名
	 * @param elm Element
	 * @param keyList キー格納配列
	 * @return header 作成したデータ行リスト
	 */
	List<String> makeDataRowList(EIMSession sess, HttpServletRequest request, javax.xml.parsers.DocumentBuilder bdb, List<String> keyList) throws Exception {

		// リクエストデータ取得
		String searchResultOrg = EIMUtils.getParameter(request, "searchResult");
		// Flex側でXMLエンコーディングされない制御文字を除去する
		String searchResult = searchResultOrg.replaceAll("[\\00-\\x1f\\x7f]", "");
		
		// データ情報XML解析
		Document ddoc = bdb.parse( new ByteArrayInputStream(searchResult.getBytes(AppConstant.CSV_XMLENCODING)) );

		// データ行生成
		List<String> dataList = new ArrayList<String>();
		Element root = ddoc.getDocumentElement();
		NodeList objList = root.getElementsByTagName("operationHistory");
		for (int i = 0; i < objList.getLength(); i++) {
			dataList.add(makeDataRow(sess, (Element)objList.item(i), keyList));
		}
		return dataList;
	}

	/**
	 * データ行を生成します。
	 * @param sess EIMSess
	 * @param elm Element
	 * @param keyList キー格納配列
	 * @return header 作成したデータ行リスト
	 */
	private String makeDataRow(EIMSession sess, Element elm, List<String> keyList) {

		StringBuffer sb = new StringBuffer();
		for (int i = 0; i < keyList.size(); i++) {
			// キー取得
			String key = keyList.get(i);
			if (StringUtils.isBlank(key))
				continue;
		
			// デリミタ出力
			if (i > 0)
				sb.append(AppConstant.CSV_COLDELIMITER);

			// データ取得
			if (key.charAt(0) == '@')
				key = key.substring(1);

			sb.append(escString(elm.getAttribute(key)));
		}
		return sb.toString();
	}
	
}