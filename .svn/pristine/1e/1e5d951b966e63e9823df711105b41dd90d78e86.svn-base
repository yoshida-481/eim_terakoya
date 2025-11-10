package servlet.dl;

import java.io.ByteArrayInputStream;
import java.text.SimpleDateFormat;
import java.util.*;

import jakarta.servlet.http.HttpServletRequest;

import org.w3c.dom.*;

import common.util.AppConstant;

import eim.util.*;
import eim.bo.*;
import eim.net.*;


/**
 * CSVファイルのダウンロード処理を実施するDownloadサーブレットです。
 *
 */
public class DownloadAccHistCSVFile extends AbstractDownloadCSVFile {

	/** シリアルバージョンUID */
	private static final long serialVersionUID = 1L;

	/**
	 * 出力ファイル名を作成します。
	 * @param sess EIMSession
	 * @return ファイル名
	 * @throws Exception
	 */
	String getFileName(EIMSession sess) throws Exception {
		
		//ファイル名ヘッダ取得
		String fileheader = EIMConfig.get("CSV_DOWNLOAD_ACCESS_HISTORY_FILEHEADER");
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
		
		// リクエストデータ取得
		String listTitle = EIMUtils.getParameter(request, "listTitle");
		
		// メールアドレス出力フラグ取得
		boolean outputUserMail = this.isOutputUserMail();

		// タイトル情報XML解析
		Document tdoc = bdb.parse( new ByteArrayInputStream(listTitle.getBytes(AppConstant.CSV_XMLENCODING)) );
		
		StringBuffer sb = new StringBuffer();
		sb.append(escString(EIMResource.getMessage(sess, "LBL_ACCESS_HISTORY_CSV_HEADER_OBJNAME")));
		
		// ルート要素を取得
		Element root = tdoc.getDocumentElement();
		
		// "FIELD"要素のリストを取得
		NodeList nd = root.getElementsByTagName("FIELD");
		for (int i = 0; i < nd.getLength(); i++) {
			// デリミタ出力
			sb.append(AppConstant.CSV_COLDELIMITER);
			
			// タイトル情報を出力
			Element element = (Element)nd.item(i);
			sb.append(escString(element.getAttribute("label")));
			
			// 配列に実装列名格納
			String key = element.getAttribute("id");
			keyList.add(key);

			// メールアドレスを出力する場合、ユーザ名の次の項目に追加
			if (outputUserMail && key.equals("@userName")) {
				
				sb.append(AppConstant.CSV_COLDELIMITER);
				sb.append(escString(EIMResource.getMessage(sess, "LBL_ACCESS_HISTORY_CSV_HEADER_USERMAIL")));
				keyList.add("@userMail");
			}
		}
		
		tdoc = null;
		
		return sb.toString();
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
		String objName = EIMUtils.getParameter(request, "objName");
		// Flex側でXMLエンコーディングされない制御文字を除去する
		String searchResult = searchResultOrg.replaceAll("[\\00-\\x1f\\x7f]", "");
		
		// データ情報XML解析
		Document ddoc = bdb.parse( new ByteArrayInputStream(searchResult.getBytes(AppConstant.CSV_XMLENCODING)) );

		// データ行生成
		List<String> dataList = new ArrayList<String>();
		Element root = ddoc.getDocumentElement();
		NodeList objList = root.getElementsByTagName("access");
		for (int i = 0; i < objList.getLength(); i++) {
			dataList.add(makeDataRow(objName, (Element)objList.item(i), keyList));
		}
		return dataList;
	}

	/**
	 * 設定ファイルからメールアドレス出力フラグを取得します。
	 */
	private boolean isOutputUserMail() {
		// メールアドレス出力フラグ取得
		boolean outputUserMail = false;
		try {
			// 設定があり、値が1の場合のみ、メールアドレス出力フラグをON
			String outputMailAddressConf = EIMConfig.get("CSV_DOWNLOAD_ACCESS_HISTORY_OUTPUT_MAILADDRESS");
			if (Integer.valueOf(outputMailAddressConf) == AppConstant.FLAG_ON) {
				outputUserMail = true;
			}
		} catch (NumberFormatException e) {
		}
		return outputUserMail;
	}
	
	/**
	 * データ行を生成します。
	 * @param objName オブジェクト名
	 * @param elm Element
	 * @param keyList キー格納配列
	 * @return header 作成したデータ行リスト
	 */
	private String makeDataRow(String objName, Element elm, List<String> keyList) {

		StringBuffer sb = new StringBuffer();
		// オブジェクト名出力
		sb.append(escString(objName));
		
		for (int i = 0 ; i < keyList.size() ; i++) {
			// キー取得
			String key = keyList.get(i);
			if (StringUtils.isBlank(key))
				continue;
		
			// デリミタ出力
			sb.append(AppConstant.CSV_COLDELIMITER);

			// データ取得
			if (key.charAt(0) == '@')
				key = key.substring(1);
			
			sb.append(escString(elm.getAttribute(key)));
		}
		return sb.toString();
	}
}
