package addon;

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import jakarta.servlet.http.HttpServletRequest;

import org.apache.commons.lang3.SystemUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import batch.PDFConvertWatcher;
import batch.PDFConvertWatcher.DebugMode;
import common.util.AppConstant;
import common.util.AppMessageUtils;
import common.util.AppObjectConditionHelper;
import common.util.AppObjectUtil;
import common.util.AppUpdateNoticeUtils;
import eim.bo.EIMAttribute;
import eim.bo.EIMAttributeType;
import eim.bo.EIMException;
import eim.bo.EIMFile;
import eim.bo.EIMFormat;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMResource;
import eim.net.EIMSession;
import eim.util.AttributeUtils;
import eim.util.EIMConfig;
import eim.util.EIMUtils;
import eim.util.FTPUtils;
import eim.util.FileUtils;
import eim.util.ObjectAttributeUtils;
import eim.util.ObjectUtils;
import eim.util.StringUtils;
import jp.co.ctc_g.eim.app.document.business.service.PdfConverterPlugin;
import jp.co.ctc_g.eim.framework.business.domain.GuardConditionExecDomain;
import jp.co.ctc_g.eim.framework2.integration.dao.plugin.DatabasePlugInLoader;

/**
 * 公開処理・PDF変換用アドオン
 */
public class PublishCommandAddOnPDFConvert implements PublishCommandAddOn {

	private final String ID = "ctc_createPublishFile";

	private final String SWF_MODULE_NAME = "CTCAddOnPDFConvertSetting";

	private final int doConvertOn = 1;

	private final int doConvertOff = 0;

	/** 半角スペース */
	private static String blank_ = " ";

	/** プラグイン */
	/** PDF変換機能モジュール */
	private PdfConverterPlugin pdfConverterPlugin;

	/** PDF変換機能モジュール(Office用) */
	private PdfConverterPlugin officePdfConverterPlugin;

	/** PDF変換機能モジュール(HGPScan用) */
	private PdfConverterPlugin hgPdfConverterPlugin;

	/**
	 * PDF変換処理の実施可否を取得してラベルに設定するメソッド
	 *
	 * @param sess セッション情報
	 * @param wfpubObj ワークフロー公開処理オブジェクト
	 *
	 * @return PDF変換処理の実施可否を示すラベルのXML
	 */
	public String getPublishStatusLabel(EIMSession sess, EIMObject wfpubObj){

		boolean convertFlag = false;
		String outString = "";
		String convertInfo = "";
		EIMAttribute attFlag = null;

		// 属性「PDF変換実施フラグ」の取得
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_FLG"));
		}

		if(attFlag != null){
			long flagVal = attFlag.getInts()[0];
			// 属性が存在し、かつ値がONの場合のみtrue
			if(flagVal == doConvertOn){
				convertFlag = true;
			}
		}

		if(convertFlag){
			convertInfo = EIMResource.getMessage(sess, "EIM.CTC.CREATE.PUBLISH.DO.CONVERT");
		}
		else{
			convertInfo = EIMResource.getMessage(sess, "EIM.CTC.CREATE.PUBLISH.NONE.CONVERT");
		}

		// 出力文字列の生成
		outString = "<setting";
		outString += " label=\"" + StringUtils.xmlEncode(convertInfo) + "\"";
		outString += " />";

		return outString;
	}

	/**
	 * PDF変換処理の設定情報を取得するメソッド
	 *
	 * @param sess セッション情報
	 * @param wfpubObj ワークフロー公開処理オブジェクト
	 *
	 * @return PDF変換処理の設定情報を示すXML
	 */
	public String getPublishCommandSetting(EIMSession sess, EIMObject wfpubObj){

		String outString = "";
		String convertFlagName = "false";
		EIMAttribute attFlag = null;

		// 属性「PDF変換実施フラグ」の取得
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_FLG"));
		}

		if(attFlag != null){
			long flagVal = attFlag.getInts()[0];
			// 属性が存在し、かつ値がONの場合のみtrue
			if(flagVal == doConvertOn){
				convertFlagName = "true";
			}
		}

		// 出力文字列の生成
		outString = "<macroSetting";
		outString += " id=\"" + StringUtils.xmlEncode(this.ID) + "\"";
		outString += " swf=\"" + StringUtils.xmlEncode(this.SWF_MODULE_NAME) + "\"";
		outString += " doConvertToPDF=\"" + StringUtils.xmlEncode(convertFlagName) + "\"";
		outString += " />";

		return outString;
	}

	/**
	 * PDF変換処理の設定情報を取得するメソッド
	 *
	 * @param sess セッション情報
	 * @param wfpubObj ワークフロー公開処理オブジェクト
	 *
	 * @return Map形式 の公開処理情報<br>
	 */
	public Map<String, Map<String, String>> getPublishCommandSettingList(EIMSession sess, EIMObject wfpubObj) throws Exception {
		Map<String, java.util.Map<String, String>> map = new HashMap<String, java.util.Map<String, String>>();

		String convertFlagName = "false";
		EIMAttribute attFlag = null;

		// 属性「PDF変換実施フラグ」の取得
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_FLG"));
		}

		if(attFlag != null){
			long flagVal = attFlag.getInts()[0];
			// 属性が存在し、かつ値がONの場合のみtrue
			if(flagVal == doConvertOn){
				convertFlagName = "true";
			}
		}

		// Mapに設定
		Map<String, String> paramMap = new HashMap<String, String>();
		paramMap.put("swf", StringUtils.xmlEncode(this.SWF_MODULE_NAME));
		paramMap.put("doConvertToPDF", StringUtils.xmlEncode(convertFlagName));
		map.put(StringUtils.xmlEncode(this.ID), paramMap);

		return map;
	}

	/**
	 * PDF変換処理の設定更新メソッド
	 *
	 * @param sess セッション情報
	 * @param request 更新リクエスト情報
	 * @param wfpubObj ワークフロー公開処理オブジェクト
	 *
	 * @throws Exception	フレームワークで例外が発生した場合<br>
	 * 						予期せぬ例外が発生した場合
	 */
	public void updatePublishCommandSetting(EIMSession sess,
			HttpServletRequest request,  EIMObject wfpubObj) throws Exception{

		long[] doConvert = {doConvertOff};

		//Parameter
		String prmDoConvert = EIMUtils.getParameter(request, "ctc_PDFConvertSetting_doesConvert");

		// パラメータチェック
		if(StringUtils.isBlank(prmDoConvert)){
			throw new EIMException(sess, "EIM.ERROR.SYSTEMERROR");
		}

		// PDF変換を行う場合
		if(prmDoConvert.equals("true")){
			doConvert[0] = doConvertOn;
		}

		// 属性「PDF変換実施フラグ」の設定
		AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_FLG"), doConvert);

		return;
	}

	/**
	 * PDF変換処理のオンライン用公開処理実行メソッド
	 *
	 * @param sess セッション情報
	 * @param object 処理対象オブジェクト
	 *
	 * @throws Exception DBアクセス時にエラーが発生した場合
	 */
	@SuppressWarnings("unchecked")
	public void doPublishCommandOnline(EIMSession sess, EIMObject object) throws Exception
	{
		//チェックイン処理
		List allObj = AppObjectUtil.getChildEIMObjectRecurrently(sess, object, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));
		allObj.add(object);

		for (int i = 0; i < allObj.size(); i++) {
			checkin(sess, (EIMObject)allObj.get(i));
		}

	}

	/**
	 * PDF変換処理のバッチ用公開処理実行メソッド
	 *
	 * @param sess セッション情報
	 * @param prntObject PDF変換対象オブジェクト
	 * @return 正常終了:true、異常終了:false
	 * @throws Exception DBアクセス時にエラーが発生した場合
	 */
	@SuppressWarnings("unchecked")
	public boolean doPublishCommandBatch(EIMSession sess, EIMObject prntObject) throws Exception
	{
		// Error Logging
		Log log = LogFactory.getLog(this.getClass().getName());

		List chldObject = AppObjectUtil.getChildEIMObjectRecurrently(sess,
				prntObject, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));
		chldObject.add(prntObject);

		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

		// 公開ファイルをチェックインしたか否かのフラグ
		boolean[] isCheckIn = new boolean[chldObject.size()];

		boolean isError = false;
		for (int i = 0; i < chldObject.size(); i++)
		{
			EIMObject object = (EIMObject)chldObject.get(i);

			//Object Type
			EIMObjectType objType = ObjectUtils.getObjectTypeById(sess, object.getType().getId());

			if (helper.isTypeOfFolder(objType))
			{
				// フォルダは変換対象外
				continue;
			}

			//Default Format
			EIMFormat defaultFormat = null;
			try
			{
				defaultFormat = FileUtils.getDefaultFormat(sess, objType);
			}
			catch(Exception dfe)
			{
				log.warn(" " + EIMResource.getMessage("EIM.ERROR.LOGIC.NODEFAULTFORM"));
				log.warn(AppMessageUtils.makeLogMessage(dfe.getMessage()), dfe);

				isError = true;
				setPublicProcessFail(sess, object);

				continue;
			}

			//File
			EIMFile file = FileUtils.getFile(sess, object, defaultFormat);

			//Original File
			File orgFile = new File(EIMConfig.get("WORK") + object.getId() + file.getExt());

			//Get File
			String getFilePath = file.getDirectory().getPath() + FileUtils.getFileName(object, file);

			if (PDFConvertWatcher.debugMode == DebugMode.OFF && SystemUtils.IS_OS_WINDOWS) {	// デバッグモードではファイル転送しない, PDF変換サーバでの実行のみファイル転送する
				//FTP
				try
				{
					FTPUtils.getFile(	EIMConfig.get("FTP_HOST"),
														EIMConfig.get("FTP_USER"),
														EIMConfig.get("FTP_PASS"),
														new File(getFilePath),
														orgFile);
				}
				catch(Exception ftpe)
				{
					log.warn(AppMessageUtils.makeLogMessage(0,ftpe.getMessage(),new
							Object[]{object.getId(),getFilePath,orgFile.getPath()}), ftpe);
					isError = true;
					setPublicProcessFail(sess, object);

					continue;
				}
			}

			//PDF File
			File pdfFile = new File(EIMConfig.get("WORK") + StringUtils.getFileBody(file.getName()) + EIMConfig.get("PDF_EXT"));

			//PDF変換前に変換元ファイル名を元に戻す (PDFのヘッダ部にファイル名が表示される場合に対応)
			File renameFile = new File(EIMConfig.get("WORK") + object.getName());
			if (renameFile.exists()) renameFile.delete();
			orgFile.renameTo(renameFile);

			//PDF変換対象でなければ即公開処理
			if(!isPDFConvertEnable(sess, object))
			{
				//Format PDF
				EIMFormat formatPublic = FileUtils.getFormatByName(sess, EIMConfig.get("FORMAT_NAME_PUBLIC"));
				File dstFile = new File(formatPublic.getDirectory().getPath() + object.getId() + file.getExt());

				if (PDFConvertWatcher.debugMode == DebugMode.OFF && SystemUtils.IS_OS_WINDOWS) {	// デバッグモードではファイル転送しない, PDF変換サーバでの実行のみファイル転送する
					try
					{
						FTPUtils.putFile(	EIMConfig.get("FTP_HOST"),
								EIMConfig.get("FTP_USER"),
								EIMConfig.get("FTP_PASS"),
								renameFile,
								dstFile);
					}
					catch(Exception e){
						String[] logValues = {String.valueOf((object.getId()))};
						log.warn(AppMessageUtils.makeLogMessage(0,e.getMessage(),logValues), e);
					}
				}

				//Check In
				FileUtils.checkin(sess, object, formatPublic, renameFile.getName(), renameFile.length());

				renameFile.delete();	//ファイルの削除

				continue;
			}

			Date date = new Date();
			SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMddHHmmss");

			// 発行コマンド
			String cmd = null;
			// コマンド成否
			boolean wasCmdSuccess = false;
			// Pluginを使用したかチェック
			boolean usePlugin = false;
			if (PDFConvertWatcher.debugMode == DebugMode.SUCCESS) {
				// デバッグモードではPDF変換せずに成功とする
				wasCmdSuccess = true;
			} else if (PDFConvertWatcher.debugMode == DebugMode.FAILURE) {
				// デバッグモードではPDF変換せずに失敗とする
				wasCmdSuccess = false;
			} else {
				// デバッグモードでない場合
				// HGPScan、Office変換どちらのプラグインを使うか判定
				if( officePdfConverterPlugin != null &&  officePdfConverterPlugin.canUse() &&  officePdfConverterPlugin.canSupported(orgFile)) {
					pdfConverterPlugin = officePdfConverterPlugin;
					usePlugin = true;
					System.out.println("Office変換利用");
				}
				if (hgPdfConverterPlugin != null &&  hgPdfConverterPlugin.canUse() &&  hgPdfConverterPlugin.canSupported(orgFile)) {
					pdfConverterPlugin = hgPdfConverterPlugin;
					usePlugin = true;
					System.out.println("HGPScan変換利用");
				}

				if ( usePlugin == false) {
					if (Boolean.valueOf(EIMConfig.get("LIBREOFFICE_USE"))) {	// LibreOfficeによる変換を行うか
						// Command
						String openOfficeExe = null;
						String inputPath = null;
						String outputPath = null;

						EIMFormat publicFormat = FileUtils.getFormatByName(sess, EIMConfig.get("FORMAT_NAME_PUBLIC"));
						String originalExtension = object.getName().substring(object.getName().lastIndexOf("."));

						if(SystemUtils.IS_OS_WINDOWS){
							openOfficeExe = EIMConfig.get("PDF_CONV_LIBREOFFICE_EXE_FOR_WIN");
							// 出力元
							inputPath = renameFile.getPath();
							outputPath = pdfFile.getPath().substring(0, pdfFile.getPath().lastIndexOf("\\"));
						} else if(SystemUtils.IS_OS_LINUX){
							openOfficeExe = EIMConfig.get("PDF_CONV_LIBREOFFICE_EXE_FOR_LINUX");
							// 出力元
							inputPath = defaultFormat.getDirectory().getPath() + object.getId() + originalExtension;
							outputPath = publicFormat.getDirectory().getPath();
						}

						List<String> command = new ArrayList<String>();
						command.add(openOfficeExe);
						command.add(EIMConfig.get("PDF_CONV_LIBREOFFICE_OPTION"));
						command.add("--convert-to pdf");
						command.add("--outdir");
						command.add(outputPath);	//出力先
						command.add(inputPath);	//出力元
						cmd = command.toString();
						log.info(String.join(" ", command));

						// Run
						try {
							Runtime runtime = Runtime.getRuntime();
							Process p = runtime.exec(String.join(" ", command));
							if (!p.waitFor(Integer.parseInt(EIMConfig.get("PDF_CONV_LIBREOFFICE_TIMEOUT")), TimeUnit.SECONDS)) {
								log.warn(EIMConfig.get("PDF_CONV_LIBREOFFICE_TIMEOUT") + "秒以内に変換処理が終わらずタイムアウトエラーが発生");
							}
							p.destroy();
						} catch (Exception e) {
							throw e;
						}

						File publicFile = null;
						if (SystemUtils.IS_OS_WINDOWS) {
							publicFile = pdfFile;
						} else if(SystemUtils.IS_OS_LINUX) {
							publicFile = new File(outputPath + object.getId() + EIMConfig.get("PDF_EXT"));
						}

						if(publicFile.exists()) {
							wasCmdSuccess = true;
						} else {
							log.warn("LibreOfficeによるPDF変換が失敗");
							wasCmdSuccess = false;
						}
					} else {
						// YSS利用
						// コマンド文字列として使用する、トークンのリストを作成
						List<String> cmdList = new ArrayList<String>();
						cmdList.add(EIMConfig.get("PDF_CONV_EXE"));
						cmdList.add(EIMConfig.get("PDF_INPUTFILE_CMD"));
						cmdList.add("\"" + renameFile.getPath() + "\"");
						cmdList.add(EIMConfig.get("PDF_OUTPUTFILE_CMD"));
						cmdList.add("\"" + pdfFile.getPath() + "\"");
						cmdList.add(EIMConfig.get("PDF_PRINTER_CMD"));
						cmdList.add("\"" + EIMConfig.get("PDF_PRINTER") + "\"");
						cmdList.add(EIMConfig.get("PDF_ADD_CMD"));
						cmdList.add(EIMConfig.get("PDF_TIMEOUT_CMD"));
						cmdList.add(EIMConfig.get("PDF_TIMEOUT"));
						// ログ出力の設定がONの場合のみ
						String logOutputFlag = EIMConfig.get("PDF_DETAIL_LOG_OUTPUT_FLAG");
						if (Integer.valueOf(logOutputFlag) == AppConstant.FLAG_ON) {
							cmdList.add("-lc");
							cmdList.add("\"" + EIMConfig.get("PDF_DETAIL_LOG") + "eim_detail_" +sdf.format(date) + ".log" + "\"");
						}

						StringBuilder sb = new StringBuilder();
						for(int index = 0 ; index < cmdList.size() ; index++){
							String token = cmdList.get(index);
							// デリミタをトークンの間に挿入
							if(index > 0){
								sb.append(blank_);
							}
							sb.append(token);
						}
						cmd = sb.toString();

						//Convert
						Process process = null;
						InputStream is = null;
						BufferedReader br = null;

						try {
							// コマンド実行
							String[] cmdarray = (String[])cmdList.toArray(new String[0]);
							process = Runtime.getRuntime().exec(cmdarray, null, null);
							is = process.getInputStream();
							br = new BufferedReader(new InputStreamReader(is));
						} catch (Exception e) {

							log.warn(cmd);
							log.warn(AppMessageUtils.makeLogMessage(0,e.getMessage(),new Object[]{object.getId()}), e);
							renameFile.delete();	//変換元ファイルの削除
							throw e;
						}

						// 読み取り行の最後まで移動
						while(br.readLine() != null)
						{
						}
						int rcode = process.exitValue();

						if(rcode == 0) {
							wasCmdSuccess = true;
						}
					}
				}

				 // Pluginを使用する場合
				 if ( usePlugin) {
					// 出力オブジェクトIDを設定
					pdfConverterPlugin.setObjectId(object.getId());
					// 出力元ファイルのパスを設定
					pdfConverterPlugin.setInputFile(renameFile);
					// 出力先ファイルのパスを設定
					pdfConverterPlugin.setOutputFile(pdfFile);
					// 実行コマンド取得
					cmd = pdfConverterPlugin.getExecCommandLine();
					// 実行し、結果を取得
					try {
						wasCmdSuccess = pdfConverterPlugin.exec();
					} catch (Exception e) {
						// コマンド実行エラーではなくプラグイン自体のエラーをキャッチ
						log.warn(cmd);
						log.warn(AppMessageUtils.makeLogMessage(0,e.getMessage(),new Object[]{object.getId()}), e);
						renameFile.delete();	//変換元ファイルの削除
						throw e;
					}
				}
			}

			//*********
			// Succeed
			//*********
			if(wasCmdSuccess)
			{
				//Sleep
				Thread.sleep(Integer.parseInt(EIMConfig.get("WAIT")));

				//Format PDF
				EIMFormat formatPDF = FileUtils.getFormatByName(sess, EIMConfig.get("FORMAT_NAME_PUBLIC"));

				//Checkin
				try
				{
					FileUtils.checkin(sess, object, formatPDF, pdfFile.getName(), pdfFile.length());
					isCheckIn[i] = true;
				}
				catch(Exception e)
				{
					log.warn(cmd);
					log.warn(" " + EIMResource.getMessage("EIM.ERROR.LOGIC.FAILEDCHECKIN"));
					log.warn(AppMessageUtils.makeLogMessage(0,e.getMessage(),new Object[]{object.getId()}), e);

					isError = true;
					setPublicProcessFail(sess, object);

					renameFile.delete();	//変換元ファイルの削除
					pdfFile.delete();		//変換先PDFファイルの削除
					continue;
				}

				//Put File Path
				String putFilePath = formatPDF.getDirectory().getPath() + object.getId() + EIMConfig.get("PDF_EXT");

				if (PDFConvertWatcher.debugMode != DebugMode.OFF || !SystemUtils.IS_OS_WINDOWS) {	// デバッグモードではファイル転送しない, PDF変換サーバでの実行のみファイル転送する
					// デバッグモードではファイル転送しない
					;
				} else {
					//FTP
					try
					{
						FTPUtils.putFile(	EIMConfig.get("FTP_HOST"),
															EIMConfig.get("FTP_USER"),
															EIMConfig.get("FTP_PASS"),
															pdfFile,
															new File(putFilePath));
					}
					catch(Exception ftppe)
					{
						log.warn(AppMessageUtils.makeLogMessage(0,ftppe.getMessage(),new Object[]{object.getId()}), ftppe);

						isError = true;
						setPublicProcessFail(sess, object);

						continue;
					} finally {
						renameFile.delete();	//変換元ファイルの削除
						pdfFile.delete();		//変換先PDFファイルの削除
					}
				}

				// 更新後のドキュメントオブジェクトを再取得する
				object = ObjectUtils.getObjectById(sess, object.getId());

				// 「公開処理失敗」属性を削除する
				long failedFlag = AppObjectUtil.getIntAttr(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_PUB_PROC_FAIL"), AppConstant.FLAG_OFF);
				if (failedFlag == AppConstant.FLAG_ON)
					AppObjectUtil.deleteAttribute(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_PUB_PROC_FAIL"));

				// SearchFramework 検索FW更新通知 対象：ドキュメント
				AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), "SEARCHFW_PDFCONV_DONE_DOCUMENT");
			}
			else
			{
				//*********
				// Failure
				//*********
				log.warn(cmd);
				String message = null;
				if (!usePlugin) {
					Object[] args = new Object[]{renameFile.getName(), String.valueOf(object.getId())};
					// 既存モジュールを使用した場合のエラー
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.FAILEDCONVERT", args);
				} else {
					// プラグインを使用した場合のエラー
					message = pdfConverterPlugin.getMessage();
				}
				log.warn(" " + message);

				renameFile.delete();	//変換元ファイルの削除

				isError = true;
				setPublicProcessFail(sess, object);
			}

			// PDF変換処理を実行した場合は日時属性を更新する
			// DBサーバーのシステム日時で「PDF変換処理実行日時」を更新する
			Date execDate = getDBSysDate(sess);
			AppObjectUtil.setAttr(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_PDF_CONV_EXEC_DATE"), execDate);

			// PDF変換処理が成功した場合は更新日時を補整する
			// 「ドキュメントの更新日時>=PDF変換処理実行日時」を条件に公開PDF作成後に原本が更新された状態であることを判定している。
			// 一方、PDF変換処理で、作成した公開PDFファイルをチェックインした時もドキュメントの更新日時が更新されている。
			// ここで、公開PDFファイルをチェックインした日時とPDF変換処理実行日時が一致してしまうと、誤って原本更新状態と判定されてしまう。
			// これを回避するために、PDF変換処理終了時点において、必ず「ドキュメントの更新日時」<「PDF変換処理実行日時」となるよう更新日時を補整する。
			if (wasCmdSuccess)
			{
				Date mDate = object.getModifyDate();
				if (mDate.getTime() == execDate.getTime())
				{
					// 更新日時を1秒減算する
					Date adjustedDate = new Date(mDate.getTime() - 1000);
					updateModificationDate(sess, object.getId(), adjustedDate);
				}
			}
		}

		if (isError)
		{
			// 生成したファイル、オブジェクトを削除する

			// 公開ドキュメントのフォーマット
			EIMFormat formatPDF = FileUtils.getFormatByName(sess,
				EIMConfig.get("FORMAT_NAME_PUBLIC"));

			for (int i = 0; i < chldObject.size(); i++)
			{
				EIMObject object = (EIMObject)chldObject.get(i);

				//Object Type
				EIMObjectType objType = ObjectUtils.getObjectTypeById(sess, object.getType().getId());

				if (helper.isTypeOfFolder(objType))
				{
					// フォルダは変換対象外
					continue;
				}

				//delete Public Document Object
				// 公開ドキュメントのEIMFileを削除（チェックインした場合のみ）
				if (isCheckIn[i]) {
					deletePublicDocumentObject(sess, object, formatPDF);
				}
			}
			// SearchFramework 検索FW更新通知 対象：WF付きフォルダと配下のフォルダおよびドキュメント(PDF変換失敗)
			AppUpdateNoticeUtils.updateNoticeInsertWFObject(sess, prntObject,
					"SEARCHFW_PDFCONV_DOCUMENT", "SEARCHFW_PDFCONV_FOLDER",
					"SEARCHFW_PDFCONV_CHILD_DOCUMENT", "SEARCHFW_PDFCONV_CHILD_FOLDER");

			return false;
		}
		return true;
	}

	/**
	 * PDF変換処理が非同期終了かどうかを返却するメソッド
	 *
	 * @param sess セッション情報
	 * @param object 処理対象オブジェクト
	 * @return 非同期終了(PDF変換対象)ならtrueを返却
	 *
	 * @throws Exception DBアクセス時にエラーが発生した場合
	 */
	public boolean isNeedAsyncProcess(EIMSession sess, EIMObject object) throws Exception
	{
		return isNeedAsyncProcess(sess, object, null);
	}

	/**
	 * PDF変換処理が非同期終了かどうかを返却するメソッド
	 *
	 * @param sess セッション情報
	 * @param object 処理対象オブジェクト
	 * @param guardConditionExecDomain ガード条件ドメイン
	 * @return 非同期終了(PDF変換対象)ならtrueを返却
	 *
	 * @throws Exception DBアクセス時にエラーが発生した場合
	 */
	public boolean isNeedAsyncProcess(EIMSession sess, EIMObject object,
				GuardConditionExecDomain guardConditionExecDomain) throws Exception
	{
		// 現状のPDF変換処理ではguardConditionExecDomainは使用しないので、
		// 処理分岐はありません。使用の際に変更をお願いします。

		// 属性「PDF変換実施フラグ」の取得
		EIMAttribute attFlag = null;

		// ワークフロー公開処理オブジェクトの取得
		EIMObject wfpubObj = AppObjectUtil.getWorkFlowProcessing(sess, object);
		if(wfpubObj == null){
			return false;
		}

		attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_FLG"));
		if(attFlag == null){
			return false;
		}

		//PDF変換チェック
		long flagVal = attFlag.getInts()[0];
		// 属性が存在し、かつ値がOFFの場合PDF変換対象外
		if(flagVal == doConvertOff){
			return false;
		}

		// オブジェクト自体がPDF変換可能かチェックする
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
		if (helper.isTypeOfFolder(object.getType())) {
			// フォルダ配下に変換可能なPDFオブジェクトが存在するかチェック
			return this.hasConvertObject(sess, object);
		} else {
			// PDF変換可能なドキュメントかチェック
			return this.isPDFConvertEnable(sess, object);
		}
	}

	/**
	 * PDF変換処理の非同期処理実行メソッド
	 *
	 * @param sess セッション情報
	 * @param object 処理対象オブジェクト
	 * @throws Exception DBアクセス時にエラーが発生した場合
	 */
	public void doAsyncProcess(EIMSession sess, EIMObject object) throws Exception
	{
		// PDF変換処理実行日時を取得
		Date pdfConvExecDate = AppObjectUtil.getDateAttr(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_PDF_CONV_EXEC_DATE"));
		// オブジェクトのMDATE（原本ファイルの更新日時）
		Date modifyDate = object.getModifyDate();

		// 原本の更新日時(MDATE)が「PDF変換処理実行日時」以降の場合、公開ファイルを削除
		if(pdfConvExecDate != null && modifyDate.getTime() >= pdfConvExecDate.getTime()){
			// 公開済みファイル削除
			EIMFormat newObjPubFormat = FileUtils.getFormatByName(sess, EIMConfig.get("FORMAT_NAME_PUBLIC"));
			deleteFile(sess, object, newObjPubFormat);
		}

		//PDF変換対象のファイルの場合、PDF変換オブジェクトを作成
		EIMObjectType objTypePDFConv = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_CONVPDF"));
		ObjectUtils.createObject(sess, objTypePDFConv, String.valueOf(object.getId()));
	}

	/**
	 * オブジェクトがPDF変換可能かどうかを返却する
	 * @param sess セッション情報
	 * @param object 対象オブジェクト
	 * @return 変換可能であればtrueを返却する
	 * @throws Exception DBアクセス時にエラーが発生した場合
	 */
	private boolean isPDFConvertEnable(EIMSession sess, EIMObject object) throws Exception
	{

		EIMFormat format = FileUtils.getDefaultFormat(sess, object.getType());
		if (format == null) {
			//フォルダ
			return true;
		}

		// 拡張子のチェック
		EIMFile file = FileUtils.getFile(sess, object, format);
		if (file == null) {
			//フォルダ
			return true;
		}

		//設定ファイルからPDF可能ファイル拡張子を取得
		String[] convFileTypeArray = this.getConvertEnableEXT();

		//PDF変換対象か判定
		for (int i = 0; i < convFileTypeArray.length; i++) {
			if(file.getExt().equalsIgnoreCase("."+convFileTypeArray[i])) {
				return true;
			}
		}
		return false;
	}

	/**
	 * フォルダがPDF変換可能なオブジェクトを保持しているかチェックする
	 * @param sess セッション情報
	 * @param object 対象オブジェクト
	 * @return 1つでもPDF変換可能なオブジェクトがあればtrue
	 * @throws Exception
	 */
	@SuppressWarnings("unchecked")
	private boolean hasConvertObject(EIMSession sess, EIMObject object) throws Exception
	{
		// フォルダ配下にあるオブジェクトを全て取得
		List<EIMObject> objList = AppObjectUtil.getChildEIMObjectRecurrently(sess, object, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));

		// helper
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

		for (EIMObject child : objList) {
			// フォルダでない、かつオブジェクトが変換可能なものでない
			if (!helper.isTypeOfFolder(child.getType())) {
				// オブジェクトが変換可能なものか
				if (this.isPDFConvertEnable(sess, child)) {
					return true;
				}
			}
		}

		return false;
	}

	/**
	 * PDF変換可能な拡張子の配列を取得
	 * @return PDF変換可能な拡張子の配列
	 */
	private String[] getConvertEnableEXT()
	{
		//設定ファイルからPDF可能ファイル拡張子を取得
		String convert_file_type = EIMConfig.get("PDF_CONVERT_FILE_TYPE");
		String[] convFileTypeArray = convert_file_type.split(",");

		return convFileTypeArray;
	}

	/**
	 * ドキュメントオブジェクトをチェックインするメソッド
	 *
	 * @param sess セッション情報
	 * @param object チェックイン対象のオブジェクト
	 *
	 * @throws Exception DBアクセス時にエラーが発生した場合
	 */
	private void checkin(EIMSession sess, EIMObject object) throws Exception
	{
		//チェックイン処理
		EIMFormat format = FileUtils.getDefaultFormat(sess, object.getType());
		if (format == null) {
			// フォルダの場合は処理不要
			return;
		}

		EIMFile file = FileUtils.getFile(sess, object, format);
		if (file == null) {
			// フォルダの場合は処理不要
			return;
		}
		EIMFormat publicDocumentFormat = FileUtils.getFormatByName(sess, EIMConfig.get("FORMAT_NAME_PUBLIC"));
		FileUtils.prepareFileAccess(sess, object, file);
		File orgFile = new File(file.getDirectory().getPath() + FileUtils.getFileName(object, file));
		File dstFile = new File(publicDocumentFormat.getDirectory().getPath() + object.getId() + file.getExt());

		// ワークフロー公開処理オブジェクトの取得
		EIMObject wfpubObj = AppObjectUtil.getWorkFlowProcessing(sess, object);

		EIMAttribute attrInsertUrlFlg = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_INSERT_URL_FLG"));
		EIMAttribute attrSignFlg = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_FLG"));

		boolean isUrl = attrInsertUrlFlg != null && attrInsertUrlFlg.getInt() == 1 ? true : false;
		boolean isSign = attrSignFlg != null && attrSignFlg.getInt() == 1 ? true : false;

		// 原本ファイルがPDFの場合はファイル作成(PDFの場合後の処理でURL挿入、署名した際、公開ドキュメントをFTP転送するとシンボリックリンクとなっているため原本ファイルが差変わってしまう)
		if (file.getExt().equals(EIMConfig.get("PDF_EXT")) && (isUrl || isSign )) {
			// 原本ファイルがPDFかつURL挿入あり、または、原本ファイルがPDFかつ署名ありの場合は実ファイル作成
			FileUtils.copyFile(orgFile, dstFile);
		} else {
			// シンボリックリンク作成
			FileUtils.createSymbolicLink(orgFile, dstFile);
		}
		FileUtils.checkin(sess, object, publicDocumentFormat, file.getName(), file.getSize());
	}

	/**
	 * 公開処理失敗属性をONにする
	 * @param sess セッション情報
	 * @param object 対象オブジェクト
	 * @throws Exception DBアクセス時にエラーが発生した場合
	 */
	private void setPublicProcessFail(EIMSession sess, EIMObject object) throws Exception
	{
		EIMAttributeType attType = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_PUB_PROC_FAIL"));
		ObjectAttributeUtils.setAttribute(sess, object, attType, 1);
	}

	/**
	 * 生成した公開ドキュメントオブジェクトを削除する
	 * @param sess セッション情報
	 * @param object 対象オブジェクト
	 * @param formatPublic 公開ドキュメントのEIMフォーマット
	 * @throws Exception DBアクセス時にエラーが発生した場合
	 */
	private void deletePublicDocumentObject(EIMSession sess, EIMObject object, EIMFormat formatPublic) throws Exception
	{
		//PDFエラー時確認用
		System.out.println("削除されたドキュメントオブジェクトID： " + object.getId());
		FileUtils.deleteFile(sess, object, formatPublic);
	}

	/**
	 * 指定オブジェクトのファイル、メタ情報を削除します。
	 * @param sess セッション情報
	 * @param object 処理対象オブジェクト
	 * @param format 処理対象フォーマット
	 * @throws Exception
	 */
	private void deleteFile(EIMSession sess, EIMObject object, EIMFormat format) throws Exception {

		EIMFile newObjPubMetaInfo = FileUtils.getFile(sess, object, format);
		FileUtils.deleteFile(sess, object, format);

		if(newObjPubMetaInfo != null){
			File newObjPubFile = new File(newObjPubMetaInfo.getDirectory().getPath() + StringUtils.getFileBody(FileUtils.getFileName(object, newObjPubMetaInfo)) + newObjPubMetaInfo.getExt());
			if (newObjPubFile != null) {
				newObjPubFile.delete();
			}
		}
	}

	/**
	 * オブジェクトの更新日時を更新する。
	 * @throws Exception
	 */
	private static void updateModificationDate(EIMSession sess, long objId, Date mdate) throws Exception {

		// Connection
		Connection conn = sess.getDBConnection();

		// SQL
		String sql = "update EIMOBJ set mdate = ? where id = ?";

		PreparedStatement pstmt = null;

		try {

			// Statement
			pstmt = conn.prepareStatement(sql);

			// Prepare
			pstmt.setTimestamp(1, new java.sql.Timestamp(mdate.getTime()));
			pstmt.setLong(2, objId);

			// Execute
			pstmt.executeUpdate();

		} finally {
			if (pstmt != null)
				pstmt.close();
		}
	}

	/**
	 * DBサーバーのシステム日時を取得します。
	 * @return DBサーバーのシステム日時
	 * @throws Exception
	 */
	private static Date getDBSysDate(EIMSession sess) throws Exception {

		// このメソッド独自にDBコネクションを取得する
		Connection conn = sess.getDBConnection();

		// SQL
		// select システム日時 as system_date
		String selection = DatabasePlugInLoader.getPlugIn().getQueryStringTimestampTrancated() + " as system_date";
		// from DUAL (Oracleの場合)
		String sql = DatabasePlugInLoader.getPlugIn().getQueryStringSelectFromDummy(selection);

		PreparedStatement pstmt = null;
		ResultSet rs = null;
		Date sysDate = null;

		try {

			// Statement
			pstmt = conn.prepareStatement(sql);

			// Execute
			rs = pstmt.executeQuery();

			// Result
			rs.next();
			sysDate = rs.getTimestamp("system_date");

		} finally {
			if (rs != null)
				rs.close();
			if (pstmt != null)
				pstmt.close();
		}

		return sysDate;
	}

	/** プラグインSetter/Getter */
	/**
	 * PDFコンバータープラグイン(Office用)の設定
	 *
	 * @param officePdfConverterPlugin
	 */
	public void setOfficePdfConverterPlugin(PdfConverterPlugin plugin) {
		this.officePdfConverterPlugin = plugin;
	}
	/**
	 * PDFコンバータープラグイン(Office用)の取得
	 *
	 * @return  officePdfConverterPlugin
	 */
	public PdfConverterPlugin getOfficePdfConverterPlugin() {
		return this.officePdfConverterPlugin;
	}

	/**
	 * PDFコンバータープラグイン(HGPScan用)の設定
	 *
	 * @param hgPdfConverterPlugin
	 */
	public void setHGPdfConverterPlugin(PdfConverterPlugin plugin) {
		this.hgPdfConverterPlugin = plugin;
	}
	/**
	 * PDFコンバータープラグイン(HGPScan用)の取得
	 *
	 * @return  hgPdfConverterPlugin
	 */
	public PdfConverterPlugin getHGPdfConverterPlugin() {
		return this.hgPdfConverterPlugin;
	}
}
