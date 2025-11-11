package servlet.dl;

import java.io.ByteArrayInputStream;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.Map.Entry;

import jakarta.servlet.http.HttpServletRequest;

import org.w3c.dom.*;

import common.util.AppConstant;
import common.util.AppObjectConditionHelper;
import common.util.AppObjectUtil;

import eim.util.*;
import eim.bo.*;
import eim.net.*;


/**
 * アクセス履歴を検索し、CSVファイルダウンロード処理を実施するDownloadサーブレットです。
 *
 */
public class DownloadAccHistCSVFileWithSearch extends AbstractDownloadCSVFile {

	/** シリアルバージョンUID */
	private static final long serialVersionUID = 1L;

	/** 出力アクセス履歴項目キー：名称 */
	private static final String OUTPUT_DATA_KEY_OBJNAME = "objName";
	/** 出力アクセス履歴項目キー：アクセス日時 */
	private static final String OUTPUT_DATA_KEY_DATETIME = "dateTime";
	/** 出力アクセス履歴項目キー：ユーザ */
	private static final String OUTPUT_DATA_KEY_USERNAME = "userName";
	/** 出力アクセス履歴項目キー：メールアドレス */
	private static final String OUTPUT_DATA_KEY_USERMAIL = "userMail";
	/** 出力アクセス履歴項目キー：アクセス内容 */
	private static final String OUTPUT_DATA_KEY_DETAIL = "accDetail";
	
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
		
		// メールアドレス出力フラグ取得
		boolean outputUserMail = this.isOutputUserMail();

		StringBuffer sb = new StringBuffer();

		sb.append(escString(EIMResource.getMessage(sess, "LBL_ACCESS_HISTORY_CSV_HEADER_OBJNAME")));
		sb.append(AppConstant.CSV_COLDELIMITER);
		sb.append(escString(EIMResource.getMessage(sess, "LBL_ACCESS_HISTORY_CSV_HEADER_DATETIEM")));
		sb.append(AppConstant.CSV_COLDELIMITER);
		sb.append(escString(EIMResource.getMessage(sess, "LBL_ACCESS_HISTORY_CSV_HEADER_USERNAME")));
		sb.append(AppConstant.CSV_COLDELIMITER);
		if (outputUserMail) {
			sb.append(escString(EIMResource.getMessage(sess, "LBL_ACCESS_HISTORY_CSV_HEADER_USERMAIL")));
			sb.append(AppConstant.CSV_COLDELIMITER);
		}
		sb.append(escString(EIMResource.getMessage(sess, "LBL_ACCESS_HISTORY_CSV_HEADER_DETAIL")));

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
		String targetObjectOrg = EIMUtils.getParameter(request, "targetObject");
		// Flex側でXMLエンコーディングされない制御文字を除去する
		String targetObjectStr = targetObjectOrg.replaceAll("[\\00-\\x1f\\x7f]", "");
		
		// メールアドレス出力フラグ取得
		boolean outputUserMail = this.isOutputUserMail();

		// 対象オブジェクト情報XML解析
		Document tdoc = bdb.parse( new ByteArrayInputStream(targetObjectStr.getBytes(AppConstant.CSV_XMLENCODING)) );
		// 処理対象オブジェクトをMap化<key:オブジェクトID, value:オブジェクト名>
		List<Map.Entry<Integer, String>> targetObjList = getOutputTargetMap(tdoc);
		tdoc = null;

		// 出力対象データをリスト化
		List<Map<String, String>> outputDataList = getOutputTargetAccData(sess, targetObjList);

		// データ行生成
		List<String> dataList = new ArrayList<String>();
		for (Map<String, String> outputDataMap : outputDataList) {
			dataList.add(makeDataRow(outputDataMap, outputUserMail));
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
	 * クライアントからわたってきた出力対象をMap化します。
	 * オブジェクト名の昇順でソートされた状態にします。
	 * 
	 * @param doc クライアントからの引数XML
	 * @return 出力対象リスト(valueのMap<key:オブジェクトID, value:オブジェクト名>のオブジェクト名でソートしたリスト)
	 */
	private List<Map.Entry<Integer, String>> getOutputTargetMap(Document doc) {

		Map<Integer, String> targetObjMap = new HashMap<Integer, String>(); 
		// ルート要素を取得
		Element root = doc.getDocumentElement();
		
		// 要素のリストを取得
		NodeList nd = root.getElementsByTagName("object");
		// クライアントからの情報をMap化
		for (int i = 0; i < nd.getLength(); i++) {
			// タイトル情報を出力
			Element element = (Element)nd.item(i);
			
			// 要素抽出
			String objId = element.getAttribute("id");
			String objName = element.getAttribute("name");
			
			targetObjMap.put(Integer.valueOf(objId), objName);
		}
		// Mapのvalueでソートを実行
		List<Map.Entry<Integer, String>> mapValuesList = new ArrayList<Map.Entry<Integer, String>>(targetObjMap.entrySet());
		Collections.sort(mapValuesList, new Comparator<Map.Entry<Integer, String>>() {
			public int compare(Entry<Integer, String> entry1, Entry<Integer, String> entry2) {
				// 昇順
				return entry1.getValue().compareTo(entry2.getValue());
			}
		});
		return mapValuesList;
	}
	
	/**
	 * クライアントからわたってきた出力対象のアクセス履歴を取得します。
	 * 
	 * @param sess EIMSession
	 * @param targetObjList 出力対象リスト(valueのMap<key:オブジェクトID, value:オブジェクト名>のオブジェクト名でソートしたリスト)
	 * @return 出力対象データリスト
	 */
	private List<Map<String, String>> getOutputTargetAccData(EIMSession sess, List<Map.Entry<Integer, String>> targetObjList) throws Exception {

		List<Map<String, String>> outputDataList = new ArrayList<Map<String, String>>();

		for (Map.Entry<Integer, String> entry : targetObjList) {
			Integer objId =  entry.getKey();
			String objName =  entry.getValue();

			// オブジェクト取得
			EIMObject object = ObjectUtils.getObjectById(sess, objId);
			
			if (object == null || !SecurityUtils.authorized(sess, object, sess.getUser(),EIMAccessRole.READ)) {
				throw new EIMException(sess, "EIM.ERROR.LOGIC.NOTEXIST.DOCFOLTAG");
			}

			// 条件判定ヘルパー作成
			AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
			
			// ドキュメントでない、かつ、タグでない、かつ、ワークフローなしフォルダの場合
			if (!helper.isTypeOfDocument(object.getType()) 
				&& !helper.isTypeOfTag(object.getType())
				&& !AppObjectUtil.isWFFolder(sess, object)) {

				// {0} はワークフローを設定していないフォルダです。
				throw new EIMException(sess, "EIM.ERROR.LOGIC.SELECT.FOLDER.NOWF", new Object[]{objName});
			}
			
			// ユーザーが対象に対して公開読取権限しかなく、ステータスが公開済で無い場合
			if (helper.isReadOnlyAccess(object) 
				&& object.getStatus() != null 
				&& object.getStatus().getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_PUBLIC) {

				// {0}は公開済みではありません。
				throw new EIMException(sess, "EIM.ERROR.LOGIC.NOTPUBLIC", new Object[]{objName});
			}

			// アクセス履歴取得
			List accsList = AccessUtils.getAccessList(sess, object);
			for (int i = 0; i < accsList.size(); i++) {

				Map<String, String> outputDataMap = new HashMap<String, String>();

				//Access
				EIMAccess access = (EIMAccess)accsList.get(i);
				
				// 出力項目抽出
				// アクセス日時
				Date date = (Date)access.getDate();
				String accessDate = DateUtils.getDBTzToCLTzDate(sess, date, "EIM.FORMAT.DATETIME");
				// ユーザ
				String accessUser = access.getUser().getName();
				// メールアドレス
				String accessUserMail = "";
				if (access.getUser().getMail() != null) {
					accessUserMail = access.getUser().getMail();
				}
				// アクセス内容
				String action = access.getAction();
				String accessDetail = "";
				if (action.indexOf("|") == -1) {
					accessDetail = EIMResource.getMessage(sess, action);
				} else {
					String delimiter = EIMConfig.get("ACCESS_HISTORY_DELIMITER");
					String key = action.substring(0, action.indexOf(delimiter));
					String param = action.substring(action.indexOf(delimiter) + 1, action.length());
					String[] params = {param};
					accessDetail = EIMResource.getMessage(sess, key, params);
				}
				// 出力データ追加
				outputDataMap.put(OUTPUT_DATA_KEY_OBJNAME, objName);
				outputDataMap.put(OUTPUT_DATA_KEY_DATETIME, accessDate);
				outputDataMap.put(OUTPUT_DATA_KEY_USERNAME, accessUser);
				outputDataMap.put(OUTPUT_DATA_KEY_USERMAIL, accessUserMail);
				outputDataMap.put(OUTPUT_DATA_KEY_DETAIL, accessDetail);
				outputDataList.add(outputDataMap);
			}
		}

		return outputDataList;
	}
	
	/**
	 * データ行を生成します。
	 * @param outputDataMap 出力データ格納Map
	 * @param outputUserMail メールアドレス出力フラグ
	 * @return header 作成したデータ行リスト
	 */
	private String makeDataRow(Map<String, String> outputDataMap, boolean outputUserMail) {

		StringBuffer sb = new StringBuffer();

		sb.append(escString(outputDataMap.get(OUTPUT_DATA_KEY_OBJNAME)));
		sb.append(AppConstant.CSV_COLDELIMITER);
		sb.append(escString(outputDataMap.get(OUTPUT_DATA_KEY_DATETIME)));
		sb.append(AppConstant.CSV_COLDELIMITER);
		sb.append(escString(outputDataMap.get(OUTPUT_DATA_KEY_USERNAME)));
		sb.append(AppConstant.CSV_COLDELIMITER);
		if (outputUserMail) {
			sb.append(escString(outputDataMap.get(OUTPUT_DATA_KEY_USERMAIL)));
			sb.append(AppConstant.CSV_COLDELIMITER);
		}
		sb.append(escString(outputDataMap.get(OUTPUT_DATA_KEY_DETAIL)));

		return sb.toString();
	}
}
