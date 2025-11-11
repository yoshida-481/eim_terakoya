package servlet.dl;

import java.io.ByteArrayInputStream;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import jakarta.servlet.http.HttpServletRequest;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import common.util.AppConstant;
import eim.bo.EIMException;
import eim.bo.EIMOperationHistory;
import eim.bo.EIMResource;
import eim.bo.EIMUser;
import eim.net.EIMSession;
import eim.util.DateUtils;
import eim.util.EIMConfig;
import eim.util.EIMUtils;
import eim.util.OperationHistoryUtils;
import eim.util.StringUtils;
import eim.util.UserUtils;


/**
 * 操作履歴を検索してCSVファイルのダウンロード処理を実施するDownloadサーブレットです。
 *
 */
public class DownLoadSearchOpeHistCSVFile extends AbstractDownloadCSVFile {

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

		StringBuffer sb = new StringBuffer();

		// リクエスト情報取得
		String listTitle = EIMUtils.getParameter(request, "listTitle");
		String prmFromTime	= EIMUtils.getParameter(request, "fromTime");
		String prmToTime = EIMUtils.getParameter(request, "toTime");
		String prmUserId	= EIMUtils.getParameter(request, "selectUserId");

		// 改行文字(nullチェックは親クラスで実施済み)
		String newline = EIMConfig.get("CSV_DOWNLOAD_NEWLINE");

		// 検索条件出力
		String serachInfo_startDate = "";
		String serachInfo_endDate = "";
		String serachInfo_userName = "";

		if (AppConstant.LANG_VALUE_JA.equals(sess.getLangId())) {
			serachInfo_startDate = "検索期間(From)" + AppConstant.CSV_COLDELIMITER;
			serachInfo_endDate = "検索期間(To)" + AppConstant.CSV_COLDELIMITER;
			serachInfo_userName = "ユーザ指定" + AppConstant.CSV_COLDELIMITER;

		} else if (AppConstant.LANG_VALUE_EN.equals(sess.getLangId())) {
			serachInfo_startDate = "Search Term(From)" + AppConstant.CSV_COLDELIMITER;
			serachInfo_endDate = "Search Term(To)" + AppConstant.CSV_COLDELIMITER;
			serachInfo_userName = "Specified User Name" + AppConstant.CSV_COLDELIMITER;
		}
		
		// 検索期間(From)
		if (!prmFromTime.toUpperCase().equals("NULL") && prmFromTime != "") {
			serachInfo_startDate =  serachInfo_startDate + prmFromTime;
		}
		sb.append(serachInfo_startDate);
		sb.append(newline);

		// 検索期間(To)
		if (!prmToTime.toUpperCase().equals("NULL") && prmToTime != "") {
			serachInfo_endDate = serachInfo_endDate + prmToTime;
		}
		sb.append(serachInfo_endDate);
		sb.append(newline);

		// 検索指定ユーザ
		if (!prmUserId.toUpperCase().equals("NULL") && prmUserId != "") {
			EIMUser user = UserUtils.getUserById(sess, Long.parseLong(prmUserId));
			if (user != null) {
				serachInfo_userName = serachInfo_userName + user.getName();
			}
		}
		sb.append(serachInfo_userName);
		sb.append(newline);
		sb.append(newline);

		// ヘッダ行出力
		String header = "";

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
		sb.append(header);

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
		
		// 出力対象データを取得
		List<EIMOperationHistory> outputDataList = getOutputTargetDataList(sess, request);

		// EIMOperationHistoryにはユーザコードが含まれないため、
		// ユーザIDを元にユーザを検索し、該当ユーザ情報を取得する必要がある

		// ユーザIDのSetを作成
		Set<Long> userIdSet = new HashSet<Long>();
		for (EIMOperationHistory history : outputDataList) {
			long userId = history.getUserId();
			if (userIdSet.contains(userId)) {
				continue;
			}
			userIdSet.add(userId);
		}

		// ユーザIDとユーザ情報のマップを作成
		List<EIMUser> users = UserUtils.getUserByIds(sess, new ArrayList<Long>(userIdSet));
		Map<Long, EIMUser> userIdAndDomainMap = new HashMap<Long, EIMUser>();
		for (EIMUser user : users) {
			long userId = user.getId();
			userIdAndDomainMap.put(userId, user);
		}

		// データ行生成
		List<String> dataList = new ArrayList<String>();
		for (int i = 0; i < outputDataList.size(); i++) {
			dataList.add(makeDataRow(sess, outputDataList.get(i), keyList, userIdAndDomainMap));
		}
		return dataList;
	}
	
	/**
	 * クライアントからわたってきた条件で操作履歴を取得します。
	 * 
	 * @param sess EIMSession
	 * @param request HttpServletRequest
	 * @return 出力対象データリスト
	 */
	private List<EIMOperationHistory> getOutputTargetDataList(EIMSession sess, HttpServletRequest request) throws Exception {

		// リクエストデータ取得
		String prmFromTime	= EIMUtils.getParameter(request, "fromTime");
		String prmToTime = EIMUtils.getParameter(request, "toTime");
		String prmUserId	= EIMUtils.getParameter(request, "selectUserId");

		// Parameter(From Date)
		Date fromDate = null;
		if (!prmFromTime.toUpperCase().equals("NULL") && prmFromTime != "") {
			fromDate = DateUtils.editExpirationDate(sess, StringUtils.getDateFromString(prmFromTime, EIMResource.getMessage(sess, "EIM.FORMAT.DATETIME")));
		}

		// Parameter(From Date)
		Date toDate = null;
		if (!prmToTime.toUpperCase().equals("NULL") && prmToTime != "") {
			toDate = DateUtils.editExpirationDate(sess, StringUtils.getDateFromString(prmToTime, EIMResource.getMessage(sess, "EIM.FORMAT.DATETIME")));
		}

		// Parameter(User ID)
		long userId = -1;
		if (!prmUserId.toUpperCase().equals("NULL") && prmUserId != "") {
			userId = Long.parseLong(prmUserId);
		}

		//検索上限数
		int limitNum = Integer.parseInt(EIMConfig.get("DOWNLOAD_OPEHIST_CSVFILE_SEARCH_RESULT_MAX"));

		//検索
		return OperationHistoryUtils.search(sess, userId, fromDate, toDate, limitNum);
	}

	/**
	 * データ行を出力します。
	 * @param sess EIMSession
	 * @param opeHist 操作履歴データ
	 * @param keyList キー格納配列
	 * @param userIdAndDomainMap ユーザIDとユーザ情報のMap
	 * @return 作成したデータ行リスト
	 */
	private String makeDataRow(EIMSession sess, EIMOperationHistory opeHist, List<String> keyList, Map<Long, EIMUser> userIdAndDomainMap)throws Exception
	{
		StringBuffer sb = new StringBuffer();

		for (int i = 0 ; i < keyList.size(); i++)
		{
			// キー取得
			String key = keyList.get(i);
			if(key == null)
				continue;

			if (key.equals("@acdate")) {
				//access Date
				String acDate = DateUtils.getDBTzToCLTzDate(sess, opeHist.getAccessDate(), "EIM.FORMAT.DATETIME");
				sb.append(escString(acDate));

			} else if (key.equals("@userCode")) {
				//User Code
				long userID = opeHist.getUserId();
				if (userIdAndDomainMap.get(userID) != null) {
					sb.append(userIdAndDomainMap.get(userID).getCode());
				} else {
					sb.append("");
				}

			} else if (key.equals("@userName")) {
				//User Name
				sb.append(opeHist.getUserName());

			} else if (key.equals("@appType")) {
				//Application Type
				String appType = opeHist.getApplicationType();
				sb.append(appType);

			} else if (key.equals("@opType")) {
				//Operation Type
				String opeType = opeHist.getOperationType();
				sb.append(opeType);

			} else if (key.equals("@rcInfo_A")) {
				if (opeHist.getRecordInfoA() != null) {
					String rcInfo_A = opeHist.getRecordInfoA();
					sb.append(rcInfo_A);
				}

			} else if (key.equals("@rcType_A")) {
				if (opeHist.getRecordTypeA() != null) {
					String rcType_A = opeHist.getRecordTypeA();
					sb.append(escString(rcType_A));
				}

			} else if (key.equals("@rcName_A")) {
				if(opeHist.getRecordNameA() != null) {
					String rcName_A = opeHist.getRecordNameA();
					sb.append(escString(rcName_A));
				}

			} else if (key.equals("@rcInfo_B")) {
				if (opeHist.getRecordInfoB() != null) {
					String rcInfo_B = opeHist.getRecordInfoB();
					sb.append(escString(rcInfo_B));
				}

			} else if (key.equals("@rcType_B")) {
				if (opeHist.getRecordTypeB() != null) {
					String rcType_B = opeHist.getRecordTypeB();
					sb.append(escString(rcType_B));
				}

			} else if (key.equals("@rcName_B")) {
				if (opeHist.getRecordNameB() != null) {
					String rcName_B = opeHist.getRecordNameB();
					sb.append(escString(rcName_B));
				}

			} else if (key.equals("@detail")) {
				if (opeHist.getDetail() != null) {
					String detail = opeHist.getDetail();
					sb.append(escString(detail));
				}
			}
			// カンマ
			if (i < keyList.size() - 1)
				sb.append(AppConstant.CSV_COLDELIMITER);
		}
		return sb.toString();
	}

}