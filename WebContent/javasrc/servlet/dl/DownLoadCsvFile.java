package servlet.dl;

import java.io.ByteArrayInputStream;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import jakarta.servlet.http.HttpServletRequest;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import common.util.AppConstant;
import eim.bo.EIMException;
import eim.bo.EIMResource;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.EIMUtils;
import eim.util.StringUtils;


/**
 * CSVファイルのダウンロード処理を実施するDownloadサーブレットです。
 */
public class DownLoadCsvFile extends AbstractDownloadCSVFile {

	/**
	 * シリアルバージョンUID
	 */
	private static final long serialVersionUID = 9146574969757543414L;

	/**
	 * 出力ファイル名を作成します。
	 * @param sess EIMSession
	 * @return ファイル名
	 * @throws Exception
	 */
	String getFileName(EIMSession sess) throws Exception {
		
		//ファイル名ヘッダ取得
		String fileheader = EIMConfig.get("CSV_DOWNLOAD_FILEHEADER");
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
	 * @param sess EIMSess
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
		NodeList objList = root.getElementsByTagName("object");
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
			if (key.equals("@public")) {
				sb.append(escString(getPublic(sess,elm)));

			} else if(key.equals("@status")) {
				sb.append(escString(getStatus(sess,elm)));

			} else if(key.equals("@signencr")) {
				sb.append(escString(getSignencr(sess,elm)));

			// (例:attType_(ID番号)_multivalueがkeyに含まれる為keyからmultivalueが含まれるか判定)
			} else if(key.indexOf("multivalue") > 0)  {
				sb.append(outputAttrData(elm, key));

			} else {
				if (key.charAt(0) == '@')
					key = key.substring(1);

				sb.append(escString(elm.getAttribute(key)));
			}
		}
		return sb.toString();
	}

	/**
	 * 複数値属性を出力します。
	 * @param elm Element
	 * @param key キー
	 * @return 属性値(複数値がある場合、'|'でつなぐ)
	 */
	private String outputAttrData(Element elm, String key) {
		
		String tempValue="";
		
		NodeList tmpNodeList = elm.getElementsByTagName(key);
		if (tmpNodeList != null && tmpNodeList.getLength() > 0) {
			Node tmpMultivalueNode = tmpNodeList.item(0);
			if (tmpMultivalueNode.hasChildNodes()) {
				NodeList tmpValueNodeList = tmpMultivalueNode.getChildNodes();
				NamedNodeMap attrs = null;
				
				if (tmpValueNodeList !=null && tmpValueNodeList.getLength() > 0) {
					for (int j = 0; j < tmpValueNodeList.getLength(); j++) {
						attrs = tmpValueNodeList.item(j).getAttributes();

						if (attrs != null) {
							String tmpValueSt = attrs.getNamedItem("value").toString();
							if (tmpValueSt.startsWith("value=")) {
								String value = tmpValueSt.substring(7,tmpValueSt.length()-1);
								
								if (j == (tmpValueNodeList.getLength() -1)) {
									tempValue += value;

								} else if (j != (tmpValueNodeList.getLength() -1)) {
									tempValue += value + "|";
								}
							}
						}
					}
					
					if(tempValue.endsWith("|")) {
						tempValue =  tempValue.substring(0,tempValue.length()-1);
					}
				}
			}
		}
		
		return escString(tempValue);
	}

	/**
	 * 公開/非公開カラム値取得
	 * @param sess EIMSession
	 * @param elm Element
	 * 
	 * @return String 公開/非公開カラム値
	 */
	private String getPublic(EIMSession sess, Element elm)
	{
		String status = elm.getAttribute("statusTypeKind");
		
		// ステータスがあるかどうか
		boolean isNoStatus = StringUtils.isBlank(status);
		// ステータス値
		int stat = isNoStatus ? 0 :Integer.parseInt(status);
		// 「公開」列にアイコンがあるか？
		boolean isExistsPublicDoc = elm.getAttribute("isDspPubIconForNoWF").equals("true");
		
		// 公開
		if (stat == AppConstant.STATUS_TYPE_KIND_ID_PUBLIC || isExistsPublicDoc) {
			return EIMResource.getMessage(sess, "EIM.SEARCH.CSVDOWNLOAD.HEADER.PUBLIC");
		//非公開
		} else if (!isNoStatus) {
			return EIMResource.getMessage(sess, "EIM.SEARCH.CSVDOWNLOAD.HEADER.PRIVATE");
		}
		
		// 空欄表示（「ステータスなし」かつ「公開列のアイコンが存在しない」）
		return "";
	}

	/**
	 * ステータスカラム値取得
	 * @param sess EIMSession
	 * @param elm Element
	 * 
	 * @return String ステータスカラム値
	 */
	private String getStatus(EIMSession sess, Element elm)
	{
		String LockUser = elm.getAttribute("lockUserName");
		String expiration = elm.getAttribute("expiration");
		boolean noWF = elm.getAttribute("isDspPubIconForNoWF").equals("true");
		
		// 有効期限切れ
		if (expiration.equals("true")) {
			return EIMResource.getMessage(sess, "EIM.SEARCH.CSVDOWNLOAD.HEADER.EXPIRE");

		} else if (!StringUtils.isBlank(LockUser)) {
			return EIMResource.getMessage(sess, "EIM.SEARCH.CSVDOWNLOAD.HEADER.REVISE") + "(" + LockUser + ")";

		} else if (elm.getAttribute("ocrProcessStatus") != "") {
			return elm.getAttribute("statusTypeName");
		}
		
		return noWF ? "" : elm.getAttribute("statusTypeName");
	}
	
	/**
	 * 暗号化カラム値取得
	 * @param sess EIMSession
	 * @param elm Element
	 * 
	 * @return String 暗号化カラム値
	 */
	private String getSignencr(EIMSession sess, Element elm)
	{
		String strSignencr = elm.getAttribute("signencr");
		int signencr = StringUtils.isBlank(strSignencr) ? 0 : Integer.parseInt(strSignencr);
		
		// 暗号化済
		if (signencr == AppConstant.SIGNENCR_KIND_SIGNENCR) {
			return EIMResource.getMessage(sess, "EIM.SEARCH.CSVDOWNLOAD.HEADER.ENCRYPTED");

		// 処理中
		} else if (signencr == AppConstant.SIGNENCR_KIND_PROCESSING_SIGNENCR) {
			return EIMResource.getMessage(sess, "EIM.SEARCH.CSVDOWNLOAD.HEADER.PROCESSING");

		// 失敗
		} else if(signencr == AppConstant.SIGNENCR_KIND_FAILED) {
			return EIMResource.getMessage(sess, "EIM.SEARCH.CSVDOWNLOAD.HEADER.FAILED");
		}
		
		return "";
	}
	
}