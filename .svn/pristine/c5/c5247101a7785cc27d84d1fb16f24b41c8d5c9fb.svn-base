package batch.migration;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.sql.SQLException;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;

import javax.sql.DataSource;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.context.ApplicationContext;

import batch.migration.domain.ImportAttributeResultDomain;
import batch.migration.utils.UpdateAttributeUtils;
import common.util.AppConstant;
import common.util.AppMessageUtils;
import eim.bo.EIMException;
import eim.bo.EIMResource;
import eim.net.EIMSession;
import eim.util.CipherUtils;
import eim.util.EIMConfig;
import eim.util.StringUtils;
import jp.co.ctc_g.eim.framework.common.util.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework2.business.dao.UserDao;
import jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework2.business.service.ObjectService;
import jp.co.ctc_g.eim.framework2.business.service.ObjectTypeService;
import jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.common.context.TransactionContext;
import jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum;
import jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils;

/**
 * CSV属性一括登録バッチ
 *
 */
public class ImportAttributeForCSVBatch {

	/** セッション */
	private static EIMSession sess;

	/** ログ */
	private static Log log = LogFactory.getLog(ImportAttributeForCSVBatch.class);

	/** サービス */
	private static ObjectTypeService objectTypeService = null;
	private static ObjectService objectService = null;

	/** ユーザ */
	private static UserDomain userDomain;

	/** カンマ */
	private static final String csvSeparatorFormat = ",";


	/**
	 * CSV一括登録処理
	 *
	 * @param args ファイル名 ユーザ パスワード
	 */
	public static void main(String[] args) {

		String messageLang = null;
		try {
			// 開始ログ
			log.info(" " + "属性CSV取り込バッチStart");

			// メッセージ出力言語
			messageLang = EIMConfig.getValue("MESSAGELANG");

			if (args.length < 3) {
				log.error("引数が不足しています。ユーザ、パスワード、ファイルパスを入力してください。");
				return;
			}

			String filePath = args[0]; // ファイル名(パス付） 例：c:\\temp\\test.csv
			String userName = args[1]; // ログインユーザ名
			String passWord = args[2]; // ログインパスワード

			// 引数チェック
			if (!initCheck(userName, passWord, filePath, messageLang)) {
				// チェックエラーの場合中止
				return;
			}

			// CSVファイル文字コード取得
			String csvSettingCharset = EIMConfig.get("CSV_DOWNLOAD_CHARSET");
			if (StringUtils.isBlank(csvSettingCharset)){
				// 文字コード設定がない場合
				log.error(EIMResource.getMessage(messageLang, "EIM.ERROR.LOGIC.CSVDOWNLOAD.CHARSET.NOTFOUND"));
				return;
			}
			String csvCharset = csvSettingCharset.equals(AppConstant.CSV_OUTPUT_UTF8_BOM) ?
					AppConstant.CSV_OUTPUT_UTF8 : csvSettingCharset;

			// 初期処理(認証)
			if (!init(userName, passWord)) {
				// 認証失敗の場合中止
				return;
			}

			// csvファイル読み込み
			log.info(filePath + "を読み込み開始");
			InputStream fis = null;
			InputStreamReader isr = null;
			BufferedReader br = null;
			ArrayList<String[]> csvData;
			try {
				fis = newInputStream(new FileInputStream(filePath), csvSettingCharset);
				isr = new InputStreamReader(fis, csvCharset);
				br = new BufferedReader(isr);
				csvData = fixCsvData(br);
			} catch (EIMException e) {
				// チェックエラーの場合中止
				return;
			} finally {
				if (br != null) {
					br.close();
					br = null;
				}
				if (isr != null) {
					isr.close();
					isr = null;
				}
				if (fis != null) {
					fis.close();
					fis = null;
				}
			}

			// ヘッダの取得。csvファイル1行目をヘッダ行として扱う。
			List<String> targetHeaderList;
			try {
				targetHeaderList = checkHeader((String[]) csvData.get(0));
			} catch (EIMException e) {
				// チェックエラーの場合中止
				return;
			}

			// 行単位の処理変数。csvファイル2行目以降をデータ行として扱う。
			List<String> importErrors = new ArrayList<String>(); // 全体のエラーメッセージを格納
			int successLines = 0; // 成功した行数

			// CSVの総件数(ヘッダ行除く)
			int totalCount = csvData.size() -1;
			// 処理件数
			int processCount = 0 ;

			// serviceの取得
			objectTypeService = (ObjectTypeService) ApplicationContextLoader.getContext().getBean("objectTypeService2");
			objectService = (ObjectService) ApplicationContextLoader.getContext().getBean("objectService4");

			// 0はヘッダなので、処理をスキップ
			for (int i = 1; i < csvData.size(); i++) {

				// 行単位のインポート処理実行
				ImportAttributeResultDomain importResult = UpdateAttributeUtils.executeImport(targetHeaderList,
						csvData.get(i), objectService, objectTypeService,sess);

				if(importResult.isUpdated()) {
					successLines++;
					processCount++;
				} else {

					List<String> tempErrorMessage = importResult.getTempErrorMessage();

					for(String s : tempErrorMessage) {
						importErrors.add(s);
					}
					//skipLines++;
					processCount++;
				}
				if (processCount % AppConstant.PROCESS_COUNT == 0 || processCount == totalCount) {
					NumberFormat nfNum = NumberFormat.getNumberInstance();
					log.info(nfNum.format(processCount) + "/" +  nfNum.format(totalCount) + "件 処理済");
				}
			}

			// エラーメッセージから、成否を判定し、コンソールとログに出力
			if(importErrors.size() != 0) {
				for(String s : importErrors) {
					log.error(s);
				}
				// DOSコンソールにエラーメッセージor正常終了メッセージを返却
				log.info(successLines + "件の登録が正常に完了しました。一部データにエラーが発生している為、ログファイルを確認して下さい。");
			} else {
				log.info(successLines + "件の登録が正常に完了しました。");
			}

			// トランザクション制御
			sess.commit();
			sess.close();

		} catch (EIMException eimException) {
			String message = null;
			try {
				message = EIMResource.getMessageValue(messageLang,
				eimException.getMessageKey(), eimException.getMessageParams());
			} catch (EIMException e) {
				// 万が一見つからない場合
				message = "The message is not found. Please confirm all the resource files.";
			}

			String logMessage = null;
			if (eimException.getCode() == 0) {
				logMessage = message;
				log.error("エラーが発生した為、処理を中止しました。ログファイルを確認してください。");
			} else {
				logMessage = "[rscode:" + eimException.getCode() + "] " + message;
			}

			log.error(logMessage, eimException);

			try {
				// ロールバック
				if(sess != null){
					// クリティカルなエラーの場合はロールバック
					sess.rollback();
				}
			} catch (Exception e1) {
				log.error(e1);
			}

			//log.error(e);

		} catch (Exception e) {

			log.error("エラーが発生した為、処理を中止しました。ログファイルを確認してください。");
			log.error(e);

			try {
				// ロールバック
				if(sess != null){
					// クリティカルなエラーの場合はロールバック
					sess.rollback();
				}
			} catch (Exception e1) {
				log.error(e1);
			}

		} finally {
			 try {
				// セッションクローズ
				EIMThreadContext.removeTransactionContext();
				if(sess != null) {
					sess.close();
				}
			} catch(Exception se) {
				log.error(AppMessageUtils.makeLogMessage(1,se.getMessage()), se);

			}
			log.info(" " + "属性CSV取り込バッチEnd");
		}
	}

	/**
	 * 初期処理および
	 * 更新ユーザログインチェック、取り込みCSV存在チェックを実施する
	 *
	 * @param userName ユーザ名（ユーザコード）
	 * @param passWord パスワード
	 * @throws Exception 例外
	 */
	private static boolean init(String userName, String passWord) throws Exception {

		// lang取得
		String lang = "";
		String EIM_CONFIG_LANG = "MESSAGELANG";
		String DEFAULT_LANG = "JA";
		if (EIMConfig.get(EIM_CONFIG_LANG) != null) {
			lang = EIMConfig.get(EIM_CONFIG_LANG);
		} else {
			lang = DEFAULT_LANG;
		}
		// Session
		ApplicationContext context = ApplicationContextLoader.getContext();
		// ユーザ認証のためのDB接続を開始する。
		startConnectionForAuth(lang, context);

		// 認証処理を実行する
		boolean chkResult =false;
		// 通常の認証チェックを行う。
		chkResult = authCheck(userName, passWord, context, lang);

		// トランザクション終了
		sess.close();


		if (chkResult) {
			// DB接続を開始する。
			startConnection(lang, context);

		}


		return chkResult;
	}

	/**
	 * 認証処理のためのＤＢ接続
	 *
	 * @param lang
	 * @param context
	 * @throws Exception
	 * @throws SQLException
	 */
	private static void startConnectionForAuth(String lang,
			ApplicationContext context) throws Exception, SQLException {
		DataSource ds = (DataSource) context.getBean("dataSource");
		// sess = new EIMSession(user, lang);
		sess = new EIMSession();
		sess.setConnection(ds.getConnection());
		sess.setConsoleMode(true);
		sess.setLang(lang);
		sess.getDBConnection().setAutoCommit(false);

		// トランザクション取得
		TransactionContext context1 = new TransactionContext(
				ConnectionModeEnum.DECLARATIVE);
		EIMThreadContext.putTransactionContext(context1);
		context1.setLangId(lang);
		context1.setDBConnection(sess.getDBConnection());
	}

	/**
	 * ＤＢ接続
	 *
	 * @param lang
	 * @param context
	 * @throws Exception
	 * @throws SQLException
	 */
	private static void startConnection(String lang, ApplicationContext context)
			throws Exception, SQLException {
		DataSource ds = (DataSource) context.getBean("dataSource");
		//sess = new EIMSession(user, lang);
		sess = new EIMSession();
		sess.setConnection(ds.getConnection());
		sess.setConsoleMode(true);
		sess.setLang(lang);
		sess.setUser(ConvertUtils.toEIMUser(userDomain));
		sess.getDBConnection().setAutoCommit(false);
		jp.co.ctc_g.eim.framework.common.util.EIMThreadContext.putEIMSession(sess);
		
		// トランザクション取得
		TransactionContext context1 = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
		EIMThreadContext.putTransactionContext(context1);
		context1.setLangId(lang);
		context1.setDBConnection(sess.getDBConnection());
		context1.setUser(ConvertUtils.toUserDomain(sess.getUser()));
	}

	/**
	 * ログイン（認証）チェックを行う
	 * @param userName ユーザコード
	 * @param passWord パスワード
	 * @param context コンテキスト
	 * @param messageLang メッセージ出力言語
	 * @return
	 * @throws Exception 例外
	 * @throws EIMException 例外
	 */
	private static boolean authCheck(String userName, String passWord,
			ApplicationContext context, String messageLang) throws Exception, EIMException {

		// 下位互換保持のため、EIMv5APIを使用する際に必要となるUserDomainを取得、セッション属性にセットする
		UserDao userDao = (UserDao)context.getBean("userDaoLight2");
		userDomain = userDao.getByCode(userName);

		if (userDomain == null) {
			log.info("IDまたはパスワードが不正です。");
			return false;
		}

		String cipherPass = CipherUtils.getMessageDigest(sess, passWord);

		if (!cipherPass.equals(userDao.getPasswordByCode(userName))) {
			log.info("IDまたはパスワードが不正です。");
			return false;
		}
		if (userDomain.isDisable()) {
			log.info("IDまたはパスワードが不正です。");
			return false;
		}

		return true;
	}

	/**
	 * ヘッダチェックを行う
	 *
	 * @param rowHeader
	 *            ヘッダ行の配列
	 * @return 更新対象のヘッダリスト
	 * @throws EIMException
	 *             チェック例外
	 */
	private static List<String> checkHeader(String[] rowHeader)
			throws EIMException {

		// ヘッダのリスト
		List<String> tempHeaderList = Arrays.asList(rowHeader);
		List<String> resultHeaderList = new ArrayList<String>();

		// 基本属性を除外し、拡張属性のみリストとする
		List<String> essentialAttrList = Arrays
				.asList(AppConstant.ESSENTIAL_ATTRIBUTE_DEFNAME);
		// 公開、名称、履歴、場所、ステータスは基本属性ではないが、基本属性と同じくインポート処理の対象外（本バッチでは英語は考慮しない）
		List<String> notTargetAttrList = Arrays
				.asList(AppConstant.CSV_NOTTARGET_ATTRIBUTE_DEFNAME);

		// 本バッチでは英語は考慮しないため、「文書ID」固定（セッション言語に依存しない）
		if (EIMConfig.get("ATTR_NAME_DOCUMENT_SEARCH_INDEX").equals(tempHeaderList.get(tempHeaderList.size() - 1)) == false) {
			log.error("ヘッダーの最後は「文書ID」でなければなりません。");
			throw new EIMException();
		}

		// 同じ名称のヘッダが複数登録されている場合、処理終了
		HashSet<String> hs=new HashSet<String>();

		for (String h : tempHeaderList) {
			if(h.equals("")) {
				// ヘッダの空白は許可するので、""の場合重複チェックはしない
				continue;
			}
			if(hs.contains(h)){
				log.error("ヘッダー行が不正です。");
				throw new EIMException();
			}
			else{
				hs.add(h);
			}
		}

		// インポート対象外の属性をまとめる
		List<String> checkAttrrList = new ArrayList<String>();
		checkAttrrList.addAll(essentialAttrList);
		checkAttrrList.addAll(notTargetAttrList);

		// 基本属性を除外
		for (String h : tempHeaderList) {

			// 基本属性と同名の属性は空白とし、インポート処理の対象外
			if (checkAttrrList.contains(h)) {
				resultHeaderList.add("");

			} else {
				resultHeaderList.add(h);
			}
		}
		return resultHeaderList;
	}

	/**
	 * ファイルの読み込判定と拡張子チェックを行う
	 *
	 * true:ファイルが読め、かつ、拡張子が".csv" false:ファイルが読み込めず、または拡張子が".csv"ではない
	 *
	 * @param file 対象ファイル
	 * @return 判定結果
	 * @throws Exception 例外
	 */
	private static boolean canFileRead(File file) {
		return file.isFile() && file.canRead() && file.getPath().endsWith(".csv");
	}

	/**
	 * インポートしたcsvデータをインポート可能な行単位に纏める
	 * STEP 1: ヘッダ行を取り込む
	 * STEP 2: ヘッダ行を「"」単位で分割
	 * STEP 3: 「"」で分割した結果[項目名][,][項目名][,][項目名][,][項目名]になるはず
	 * STEP 4: 項目名のみ取り出す
	 * STEP 5: 詳細データ行を取り込む
	 * STEP 6: 「"」と「"」の間を項目値としてString[]に格納する（改行も考慮）
	 * STEP 7: データ行とヘッダ行の整合性をチェック
	 * STEP 8: 整理したcsvデータを返却
	 *
	 * @param br インポートしたcsvデータ
	 * @return インポート可能なデータ（Stringの配列）のリスト
	 * @throws IOException IO例外
	 * @throws EIMException チェック例外
	 */
	private static ArrayList<String[]> fixCsvData(BufferedReader br)
			throws IOException, EIMException {

		String line;
		ArrayList<String[]> csvData = new ArrayList<String[]>();

		// ヘッダ行の取り込み
		String headerLine = br.readLine();
		if (headerLine == null || StringUtils.isBlank(headerLine)) {
			log.error("ヘッダー行が不正です。");
			throw new EIMException();
		}
		// 取り込んだcsvの行を「"」で分割。
		String[] colstemp = headerLine.split("\""); //ヘッダ行
		// ヘッダのフォーマットチェック
		cheackCsvHeaderFormat(colstemp);
		// Headerの「"」の総数
		int quaCountHeader = headerLine.split("\"").length;

		// 明細行の「"」のカウント
		int quaCount = 0; // ダブルクォーテーションの数カウンタ
		//int quaCountDetail = 0; //全明細データ数

		// ヘッダの項目数
		int headerCounts = quaCountHeader / 2;

		// ヘッダの項目を格納する配列
		String[] cols = new String[headerCounts];
		// 「,」しか入っていない配列を除く
		for (int i = 0; i < headerCounts; i++) {
			cols[i] = colstemp[i * 2 + 1];
		}
		// Header行取り込み完成
		csvData.add(cols);

		// データ途中で改行ありフラグ
		boolean hasNewLineInData = false;

		// 初期化
		cols = new String[headerCounts];
		int columCount = 0;
		int lineCount = 1; // データ用

		// データ行の取り込み
		while ((line = br.readLine()) != null) {
			// csv行数をカウント

			// 1文字単位で分割

			//colstemp = line.split("");
			String[] lineDataArray = new String[line.length()] ;// データ行
			for (int k = 0; k < line.length(); k++) {
			    // 配列に順番に格納する
				lineDataArray[k] = String.valueOf(line.charAt(k));
			}

			if (hasNewLineInData) {
				//データの中に改行があった場合、
				// 先頭文字は、「",」以外のパターンでは、必ずダブルクォーテーションでは始まらないのでエラーにする。
				if (lineDataArray.length > 1 && ("\"").equals(lineDataArray[0]) && (",").equals(lineDataArray[1])) {

					hasNewLineInData = false; //初期化
				} else if (lineDataArray.length > 0 &&("\"").equals(lineDataArray[0])){
					throwInvaridFormat(lineCount);
				} else {
					hasNewLineInData = false; //初期化
				}
			}


			// 結合用文字列
			String letsCombine = "";
			for (int i = 0; i < lineDataArray.length; i++) {
				/*
				 * 「"」のカウント。 「"」が奇数個目の場合文字を結合します。 「"」が偶数個目の場合文字列を格納します。
				 */
				if (("\"").equals(lineDataArray[i])) {
					quaCount += 1;
					//quaCountDetail += 1;
				} else if (quaCount % 2 == 1) {
					letsCombine += lineDataArray[i];
				} else if (quaCount % 2 == 0) {
					// 偶数個目の「"」の次の文字が「,」でなけらばフォーマットエラー
					if (!(",").equals(lineDataArray[i])) {
						throwInvaridFormat(lineCount);
					}
					/*
					 * 格納先がnullの場合、そのまま格納。 格納先がnullでない場合、改行及び結合して格納。
					 */
					if (cols[columCount] == null) {
						cols[columCount] = letsCombine;
					} else {
						cols[columCount] += EIMConfig.get("CSV_DOWNLOAD_NEWLINE");
						cols[columCount] += letsCombine;
					}
					columCount++;
					letsCombine = "";

					if (columCount >= headerCounts) {
						throwInvaridFormat(lineCount);

					}

				}
			}
			// 取り込んだ行の終わりの処理 格納先がnullの場合、そのまま格納。 格納先がnullでない場合、改行及び結合して格納。

			if (cols[columCount] == null) {
				cols[columCount] = letsCombine;
			} else {
				cols[columCount] += EIMConfig.get("CSV_DOWNLOAD_NEWLINE");
				cols[columCount] += letsCombine;
			}
			// Header行の「"」の数と一致したらデータ行取り込み完了、変数の初期化を行う
			if (quaCount == quaCountHeader) {
				csvData.add(cols);
				quaCount = 0;
				columCount = 0;
				cols = new String[headerCounts];
				lineCount++;
			} else {
				hasNewLineInData = true;
			}
		}
		// 最終行のフォーマットがおかしい場合
		if (quaCount != 0 && quaCount != quaCountHeader ) {
			throwInvaridFormat(lineCount);
		}

		return csvData;
	}

	/**
	 * CSVの型式エラー
	 * @param lineCount
	 * @throws EIMException
	 */
	private static void throwInvaridFormat(int lineCount) throws EIMException {
		log.error(lineCount + "行目：取込用CSVの形式が不正です。");
		throw new EIMException();
	}

	/**
	 * ヘッダのフォーマットチェック
	 *
	 * @param splitByQuotation 「"」で分割されたString配列
	 * @throws EIMException 例外
	 */
	private static void cheackCsvHeaderFormat(String[] splitByQuotation)
			throws EIMException {
		for (int i = 0; i < splitByQuotation.length; i += 2) {
			if (i == 0) {
				continue;
			}
			// 偶数個目の配列が[,]でない場合エラー
			if (!csvSeparatorFormat.equals(splitByQuotation[i])) {
				log.error("ヘッダー行が不正です。");
				throw new EIMException();
			}
		}
	}

	/**
	 * 初期値チェック
	 * @param userName ユーザ名
	 * @param passWord パスワード
	 * @param importFile 入力ファイル
	 * @param messageLang メッセージ出力言語
	 * @throws EIMException 例外
	 */
	private static boolean initCheck(String userName, String passWord,
			String importFile, String messageLang) throws EIMException {
		// 引数チェック
		if (StringUtils.isBlank(userName) || StringUtils.isBlank(passWord)) {
			log.error("IDまたはパスワードが不正です。");
			return false;
		}
		if (StringUtils.isBlank(importFile)) {
			log.error("指定された取込用CSVファイルが存在しません。");
			return false;
		}
		// 取り込みファイルのチェック
		File filePath = new File(importFile);
		if (!canFileRead(filePath)) {
			log.error("指定された取込用CSVファイルが存在しません。");
			return false;
		}
		return true;
	}

	/**
	 * InputStreamの初期化を行います。
	 *
	 * @param inputStream 入力ストリーム
	 * @param settingCharset CSVファイル文字コード
	 * @return BOM処理済の入力ストリーム
	 * @throws IOException 例外
	 */
	private static InputStream newInputStream(InputStream inputStream, String settingCharset) throws IOException {
		if (!settingCharset.equals(AppConstant.CSV_OUTPUT_UTF8_BOM)) {
			// UTF_BOM有とする設定値以外の場合、引数のInputStreamをそのまま返す
			return inputStream;
		}

		if (!inputStream.markSupported()) {
			// InputStreamがmark()、reset()をサポートしていない場合はBufferedInputStreamでラップする
			inputStream = new BufferedInputStream(inputStream);
		}

		// BOM終了位置にマークを設定する
		inputStream.mark(3);

		// 先頭から3バイト読み込み、BOMか否かを判定する
		byte buf[] = new byte[3];
		int read = inputStream.read(buf, 0, 3);
		if (read < 3 ||
				buf[0] != (byte) 0xef ||
				buf[1] != (byte) 0xbb ||
				buf[2] != (byte) 0xbf) {
			// 先頭がBOMでない場合、ストリーム位置をBOM判定用に読み込む前まで戻す
			inputStream.reset();
		}

		return inputStream;
	}

}
