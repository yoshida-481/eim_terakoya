/**
 *
 */
package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.InputStreamReader;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import common.enumeration.HGPScanErrorCodeEnum;
import common.util.AppConstant;
import common.util.AppMessageUtils;
import common.util.AppObjectConditionHelper;
import common.util.AppObjectUtil;
import common.util.AppUpdateNoticeUtils;
import common.util.MailUtil;
import common.util.SignatureStatusTypeUtils;
import eim.bo.EIMAttribute;
import eim.bo.EIMAttributeType;
import eim.bo.EIMFile;
import eim.bo.EIMFormat;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMResource;
import eim.bo.EIMStatus;
import eim.bo.EIMStatusType;
import eim.bo.EIMUser;
import eim.bo.EIMWorkFlow;
import eim.net.EIMSession;
import eim.util.AttributeUtils;
import eim.util.EIMConfig;
import eim.util.FTPUtils;
import eim.util.FileUtils;
import eim.util.ObjectAttributeUtils;
import eim.util.ObjectUtils;
import eim.util.StringUtils;
import eim.util.UserUtils;
import eim.util.WorkFlowUtils;
import jp.co.ctc_g.eim.app.document.business.service.PublishCommandPDFSignatureHGPScanService;
import jp.co.ctc_g.eim.framework.business.domain.EventHistoryDomain;
import jp.co.ctc_g.eim.framework.business.domain.EventLogDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.EventDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.EventTypeDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.OtherNameDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.StatusTypeDomain;
import jp.co.ctc_g.eim.framework.business.service.EventHistoryService;
import jp.co.ctc_g.eim.framework.common.util.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import yss.pdfmakeup.pdfmakeup;
import yss.pdfmakeup.pmudst;


/**
 * PDF 署名処理アドオン
 */
public class PublishCommandPDFSignatureHGPScanServiceImpl implements PublishCommandPDFSignatureHGPScanService {

	private final String CSV_EXTENSION = ".csv";
	private final String STRING_COMMA = ",";

	/**	List */
	private static List csvFileList = new ArrayList();
	//設定用
	private static List nameList = new ArrayList();
	private static List idList = new ArrayList();
	private static List sdateList = new ArrayList();
	private static List timeList = new ArrayList();
	private static List outDispStatusTypeList = new ArrayList();
	//取得用
	private static List userList = new ArrayList();
	private static List commentList = new ArrayList();
	private static List dateList = new ArrayList();
	private static List dispStatusTypeList = new ArrayList();

	//DateFormat
	private static final SimpleDateFormat dfm = new SimpleDateFormat("yyyy/MM/dd");
	private static final SimpleDateFormat dfmt = new SimpleDateFormat("HH:mm:ss");

	/**
	 * PDF署名処理のバッチ用公開処理実行メソッド
	 * 印影用csvファイル作成してハイパーギアで署名に利用する
	 *
	 * 1) 公開 Format から FTP GET
	 * 2) 初期設定の場合
	 * 2-1) PDF署名オブジェクトから情報取得
	 * 2-2) 承認情報を書き込んだ印影用csvファイル作成
	 * 3) 変更の場合
	 * 3-1) PDFセキュリティ設定オブジェクトから情報取得
	 * 3-2) 変更元ファイルのパスワード情報をPDF署名オブジェクトから取得
	 * 4) Makeup 実行
	 * 5) FTP PUT
	 * 6) PDFファイル・印影用csvファイル削除
	 *
	 * @param sess セッション情報
	 * @param prntObject PDF署名オブジェクト
	 * @return 正常終了:true、異常終了:false
	 * @throws Exception DBアクセス時にエラーが発生した場合
	 */
	public boolean doPublishCommandBatch(EIMSession sess, EIMObject prntObject) throws Exception
	{

		//Error Logging
		Log log = LogFactory.getLog(this.getClass().getName());

		//Document Object
		EIMObject docObject = ObjectUtils.getObjectById(sess, Long.parseLong(prntObject.getName()));

		//システム管理者なので権限チェックは不要
		if(docObject == null)
		{
			doErrorAct(sess, null, prntObject, null, log,
					EIMResource.getMessage("EIM.ERROR.LOGIC.NODOCUMENT",new Object[]{prntObject.getName()}), 0);

			// 署名オブジェクトを削除
			ObjectUtils.deleteObject(sess, prntObject);

			return false;
		}

		List chldObject = AppObjectUtil.getChildEIMObjectRecurrently(sess,
				docObject, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));
		chldObject.add(docObject);

		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

		//--- 公開処理失敗オブジェクトがないかチェック ---//
		//一つでも公開処理失敗のオブジェクトがあれば次のPDF署名オブジェクトの処理へ
		for (int i = 0; i < chldObject.size(); i++)
		{
			EIMObject object = (EIMObject)chldObject.get(i);
			EIMObjectType objType = ObjectUtils.getObjectTypeById(sess, object.getType().getId());

			if(!helper.isTypeOfFolder(objType) && AppObjectUtil.getIntAttr(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_PUB_PROC_FAIL"), -1) != 0)
			{
				//「3:処理失敗」を設定
				EIMAttributeType sigStatType = AttributeUtils.getAttributeTypeByName
									(sess, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_STATUS"));
				ObjectAttributeUtils.setAttribute(sess, prntObject, sigStatType, 3);
				return false;
			}
		}

		for (int i = 0; i < chldObject.size(); i++)
		{
			//リストの初期化
			csvFileList.clear();
			nameList.clear();
			idList.clear();
			sdateList.clear();
			timeList.clear();
			outDispStatusTypeList.clear();
			userList.clear();
			commentList.clear();
			dateList.clear();
			dispStatusTypeList.clear();

			EIMObject object = (EIMObject)chldObject.get(i);

			//Object Type
			EIMObjectType objType = ObjectUtils.getObjectTypeById(sess, object.getType().getId());

			if (helper.isTypeOfFolder(objType))
			{
				// フォルダは署名対象外
				continue;
			}

			//ワークフローを取得
			EIMStatus eimStatus = docObject.getStatus();
			EIMWorkFlow workFlow = WorkFlowUtils.getWorkFlowByStatusType(sess, eimStatus.getType());
			// ワークフロー公開処理オブジェクトの取得
			EIMObject wfpubObj = AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_WFPUB"), String.valueOf(eimStatus.getType().getId()));

			// 電子署名用言語取得
			EIMAttribute approveNameLang = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_APPROVE_NAME_LANG"));
			// ジョブ名取得
			EIMAttribute jobAttr = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_JOB_NAME"));

			// 電子署名用言語、ジョブ名が設定されていない場合
			if ( approveNameLang == null || jobAttr == null) {
				doErrorAct(sess, object, prntObject, null, log, "対象文書の電子署名用言語、もしくはジョブ名が取得できません。 " + docObject.getId() + " : " +docObject.getName() , 0);
				updateNoticePDFSignFailure(sess, docObject);
				return false;
			}

			//Pubilc Format(エラー処理のためここで取得)
			EIMFormat publicFormat  = FileUtils.getFormatByName(sess, EIMConfig.get("FORMAT_NAME_PUBLIC"));
			if(publicFormat == null)
			{
				doErrorAct(sess, object, prntObject, null, log,
						EIMResource.getMessage("EIM.ERROR.LOGIC.NOPUBLICFORM.WITHDOCNAME", new Object[]{prntObject.getName()}), 0);

				updateNoticePDFSignFailure(sess, docObject);
				return false;
			}


			//署名対象ファイルをFTP転送でDownload
			EIMFile file = FileUtils.getFile(sess, object, publicFormat);
			if(file == null)
			{
				doErrorAct(sess, object, prntObject, null, log,
						EIMResource.getMessage("EIM.ERROR.LOGIC.NOPUBLICFILE.WITHDOCNAME", new Object[]{object.getName()}), 0);
				updateNoticePDFSignFailure(sess, docObject);
				return false;
			}
			//pdfかどうかチェック pdf以外なら次へ
			if(file.getExt() == null || !file.getExt().equals(EIMConfig.get("PDF_EXT")))
			{
				continue;
			}

			EIMAttribute attrSigstat = prntObject.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_STATUS"));
			long sigstat = attrSigstat.getInt();

			//Original File
			File orgFile = new File(EIMConfig.get("PDF_SIGNATURE_IN_WORK") + object.getId() + file.getExt());
			//Get File Path
			String getFilePath = file.getDirectory().getPath() + FileUtils.getFileName(object, file);

			//FTP Download
			try
			{
				FTPUtils.getFile(EIMConfig.get("FTP_HOST"),
								EIMConfig.get("FTP_USER"),
								EIMConfig.get("FTP_PASS"),
								new File(getFilePath),
								orgFile);
			}
			catch(Exception e)
			{
				String errMsg = null;
				if(sigstat == 1)
				{
					errMsg += EIMResource.getMessage("EIM.LOG.PDF.SIGN.INITFAILED");
				}
				else if(sigstat == 4)
				{
					errMsg += EIMResource.getMessage("EIM.LOG.PDF.SIGN.CHANGEFAILED");
				}
				errMsg += EIMResource.getMessage("EIM.LOG.PDF.SIGN.FILENAME") + StringUtils.getFileBody(object.getName())
						+ "(" + prntObject.getName() + ")";
				errMsg += EIMResource.getMessage("EIM.LOG.PDF.PREFIX.ERROR")
						+ EIMResource.getMessage("EIM.ERROR.LOGIC.FTPDOWNLOAD")
						+ EIMResource.getMessage("EIM.LOG.PDF.PREFIX.ERRORCODE") + "-";
				log.warn(errMsg);
				throw e;
			}

			// Signature CSV File
			File csvFile = new File(EIMConfig.get("PDF_SIGNATURE_IN_WORK") + object.getId() + CSV_EXTENSION);
			csvFileList.add(csvFile);

			// ステータスタイプ設定XML取得処理
			HashMap<String,String[]> statusTypeXMLMap = SignatureStatusTypeUtils.getDisplayStatusName(sess, workFlow);
			//セキュリティ情報取得対象オブジェクト
			EIMObject secInfoObj = null;

			// 署名有無、承認者名挿入、承認日付挿入の属性を取得する
			//署名有無
			EIMAttribute doSign = prntObject.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_ONOFF"));
			//承認者名挿入
			EIMAttribute appName = prntObject.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_APPNAME"));
			//承認日付挿入
			EIMAttribute appDate = prntObject.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_APPDATE"));

			//----------- 初期設定の場合 ------------//
			if(sigstat == 1)
			{
				if(doSign != null)
				{
					EIMStatusType statusType = null;
					//対象ステータスタイプオブジェクトリスト
					List dostatTypeList  = new ArrayList();

					//「電子署名する」場合
					if(doSign.getInt() == 1)
					{
						List statusTypeList = workFlow.getStatusTypeList();

						//ステータスタイプが「承認依頼中」
						for(int j = 0; j < statusTypeList.size(); j++)
						{
							statusType = (EIMStatusType)statusTypeList.get(j);
							//ステータスタイプの種類が「承認依頼中(全員)」または「承認依頼中(一人)」
							if(statusType.getKind() == AppConstant.STATUS_TYPE_KIND_ID_APPROVE)
							{
								dostatTypeList.add(statusType);
							}
						}
						//承認者・承認日・コメントを取得するためのEventHistoryService
						EventHistoryService eventHistoryService =  (EventHistoryService)ApplicationContextLoader.getContext().getBean("eventHistoryService");
						EventHistoryDomain eventHistoryDomain = eventHistoryService.getByObjId(docObject.getId());
						List<EventLogDomain> eventList = eventHistoryDomain.getEventLogList();
						EventLogDomain latestApprovalEventLogDom = null;


						//承認者名挿入の値が「1:最終承認者のみ挿入」の場合
						//取得したリストのうちステップ数が最大値のオブジェクトのみ処理対象
						if(appName.getInt() == 1)
						{
							if(dostatTypeList.size() != 0)
							{
								statusType = (EIMStatusType)dostatTypeList.get(0);
								for(int j = 0; j < dostatTypeList.size(); j++)
								{
									if(statusType.getStep() < ((EIMStatusType)dostatTypeList.get(j)).getStep())
									{
										statusType = (EIMStatusType)dostatTypeList.get(j);
									}
								}
							}

							//ドキュメントオブジェクトとステータスタイプからステータスオブジェクトを取得
							EIMStatus wfstatus = WorkFlowUtils.getStatusByType(sess, docObject, statusType);
////////////////////////////修正部分1([承認者が最終承認者のみ]イベント履歴からベースイベントタイプが"承認"となる最新のものを取得し、そこから承認者、承認日、コメントを取得する)


							//一番新しいベースイベントタイプが"承認"となるEventLogDomainを取得
							for(int k = eventList.size()-1; k>-1 ; k--)
							{
								EventLogDomain eventLogdom = eventList.get(k);
								if(eventLogdom.getEvent().getEventType().getBaseEventType().getKey().equals("BaseEvtApprovalPlugIn"))
								{
									latestApprovalEventLogDom = eventLogdom;
									break;
								}
							}
							if(latestApprovalEventLogDom == null)
							{
								//エラーメッセージ「承認イベントがありません」
							}

							//承認者
							userList.add(latestApprovalEventLogDom.getEvent().getCUser().createEIMUser());
							//承認日
							dateList.add(latestApprovalEventLogDom.getEvent().createEIMEvent().getCDate());
							//ステータスタイプ名(XML設定)
							String statusTypeName = getStatusTypeNameFromXML(latestApprovalEventLogDom.getEvent(),statusTypeXMLMap);
							dispStatusTypeList.add(statusTypeName);
							//コメント取得
							if(latestApprovalEventLogDom.getEvent().getAttribute(EIMConfig.get("ATTR_NAME_APPROVER_COMMENT")) == null)
							{
								commentList.add("");
							}else{
								commentList.add(latestApprovalEventLogDom.getEvent().getAttribute(EIMConfig.get("ATTR_NAME_APPROVER_COMMENT")).getValues());
							}
////////////////////////////修正部分1終わり

//							//承認済みユーザ一覧を取得
//							userList = AppObjectUtil.getApprovedList(sess, wfstatus, dateList, commentList);

							//リストを「承認日」でソートし、承認日時が最新のオブジェクトを取得
							int index = 0;
							if(dateList.size() != 0)
							{
								for(int j = 0; j < dateList.size(); j++)
								{
									if(((Date)dateList.get(index)).compareTo((Date)dateList.get(j)) < 0)
									{
										index = j;
									}
								}
								addAppInfoToList(sess, index, approveNameLang);
							}
						}
						//「2:全承認者を挿入」の場合
						else
						{
////////////////////////////修正部分2([承認者が全承認者]イベント履歴からベースイベントタイプが"承認"となるものを取得し、そこから承認者、承認日、コメントを取得する)
							List<EventLogDomain> appEvList = new ArrayList<EventLogDomain>();
							if(Boolean.valueOf(EIMConfig.get("REQUEST_INCLUDE"))) {
								appEvList = getApproveAndApprovalRequestInfoEventList(eventList);
							}
							else
							{
								// 印影出力対象のリストをイベント履歴から作成する。最新のイベントからリストに詰める（既存署名処理）
								appEvList = getApproveInfoEventList(eventList);
							}

							// 印影出力対象のリストを元に出力情報を取得
							for(int kkk=appEvList.size()-1, appEvCnt=0; kkk>=0; kkk--)
							{

								EventLogDomain eventLogDomain = appEvList.get(kkk);
								EventDomain eventDomain = eventLogDomain.getEvent();
								//承認者
								userList.add(eventDomain.getCUser().createEIMUser());
								//承認日
								dateList.add(eventDomain.createEIMEvent().getCDate());
								//ステータスタイプ名(XML設定)
								dispStatusTypeList.add(getStatusTypeNameFromXML(eventDomain,statusTypeXMLMap));
								//コメント取得
								if(eventDomain.getAttribute(EIMConfig.get("ATTR_NAME_APPROVER_COMMENT")) == null)
								{
									commentList.add("");
								}else{
									commentList.add(eventDomain.getAttribute(EIMConfig.get("ATTR_NAME_APPROVER_COMMENT")).getValues());
								}
								addAppInfoToList(sess, appEvCnt, approveNameLang);
								appEvCnt++;
							}

////////////////////////////修正部分2終わり

						}

						//承認情報を書き込んだcsvファイル作成
						//--- csvファイル作成 ---//
						for(int j = nameList.size()-1; j >= 0; j--)
						{
							// ステータスタイプの表示名の設定で表示/非表示を判定する
							if(Boolean.valueOf(EIMConfig.get("SIGNATURE_STATUS_TYPE_CONF_DEFAURT_USE"))){
								// ステータスタイプの表示名が空か判定
								if(outDispStatusTypeList.get(j).toString().equals("")){
									// 空の場合は捺印しない
									continue;
								}
								// CSVファイル作成
								csvFile = writeSigunaturCsvFile(appName, appDate, csvFile, (String)nameList.get(j), sdateList.get(j).toString(), outDispStatusTypeList.get(j).toString());
							} else {
								// CSVファイル作成
								csvFile = writeSigunaturCsvFile(appName, appDate, csvFile, (String)nameList.get(j), sdateList.get(j).toString(), outDispStatusTypeList.get(j).toString());
							}

							if(csvFile == null)
							{
								doErrorAct(sess, object, prntObject, null, log,
										EIMResource.getMessage("EIM.ERROR.LOGIC.PDFSIG.HGPCSAN.STAMPFAIL"), 0);
								return false;
							}
						}

					}
				}
				//セキュリティ情報取得対象オブジェクトに「PDF署名」オブジェクトを設定
				secInfoObj = prntObject;
			}
			//----------- セキュリティ設定変更の場合 ------------//
			else if(sigstat == 4)
			{
				//PDFセキュリティ設定オブジェクトを取得
				EIMObjectType objTypePdfSec = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_SECPDF"));
				EIMObject pdfSecObj = ObjectUtils.getObjectByTypeAndName(sess, objTypePdfSec, prntObject.getName());
				if(pdfSecObj == null)
				{
					doErrorAct(sess, object, prntObject, null, log,
							EIMResource.getMessage("EIM.ERROR.LOGIC.PDFSIG.CHANGEDATA.CANNOTGET"), 0);
					updateNoticePDFSignFailure(sess, docObject);
					return false;
				}
				//セキュリティ情報取得対象オブジェクトに「PDFセキュリティ設定」オブジェクトを設定
				secInfoObj = pdfSecObj;
			}

			//セキュリティ設定用属性情報を取得
			EIMAttribute secOn = secInfoObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_ONOFF"));
			EIMAttribute secPassOn = secInfoObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECPASS_ONOFF"));
			EIMAttribute secPass = secInfoObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECPASS"));
			EIMAttribute refPassOn = secInfoObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_REFPASS_ONOFF"));
			EIMAttribute refPass = secInfoObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_REFPASS"));
			EIMAttribute secPrint = secInfoObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_FORBPRINT"));
			EIMAttribute secEdit = secInfoObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_FORBEDIT"));
			EIMAttribute secRem = secInfoObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_FORBADDREM"));
			EIMAttribute secRepr = secInfoObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_FORBREPR"));


			//--- PDF Makeup ここから ---//

			// 出力用PDFファイル
			File outPutPDFFile = orgFile;

			//PDFファイルへ署名設定
			if(doSign != null && appName != null)
			{
				if(doSign.getInt() == 1)
				{
					Process process = null;
					BufferedReader br = null;
					HGPScanErrorCodeEnum execCode = HGPScanErrorCodeEnum.UNEXECUTE;

					// 署名用ジョブ実行コマンド作成
					// コマンドラインフォーマット取得
					String format = EIMConfig.get("PDF_SIGNATURE_HGPSCAN_FORMAT");
					// ジョブ名取得
					String jobName = jobAttr.getString();
					// アプリケーションパス取得
					String applicationPath = EIMConfig.get("PDF_SIGNATURE_HGPSCAN");
					// コマンド実行タイムアウト時間取得
					String timeout = EIMConfig.get("PDF_SIGNATURE_HGPSCAN_TIME_OUT");
					// 書式にコマンドパラメータを配置して返却
					String execCommandLine = String.format(format, applicationPath, jobName, timeout);

					String sleepConfig = EIMConfig.get("PDF_SIGNATURE_HGPSCAN_DUPLICATE_SLEEP_TIME");
					int sleepTime = Integer.parseInt(sleepConfig);
					String retryConfig = EIMConfig.get("PDF_SIGNATURE_HGPSCAN_DUPLICATE_RETRY_LIMIT");
					int retry = Integer.parseInt(retryConfig);
					boolean signCompFlg = false;


					for (int cnt = 0; cnt < retry; cnt++) {
						try {
							// コマンド実行前に一瞬時間を置く
							Thread.sleep(1*1000);

							// コマンド実行
							process = Runtime.getRuntime().exec(execCommandLine);
				        	// プロセス終了を待つ
							process.waitFor();
							br = new BufferedReader(new InputStreamReader(process.getInputStream()));
						} catch (Exception e) {
							log.warn(execCommandLine);
							log.warn(AppMessageUtils.makeLogMessage(0,e.getMessage(),new Object[]{}), e);
							doErrorAct(sess, object, prntObject, null, log,
									EIMResource.getMessage("EIM.ERROR.LOGIC.PDFSIG.HGPCSAN.STAMPFAIL"), 0);
							return false;
						}
						// 読み取り行の最後まで移動
						try {
							while(br.readLine() != null) {}
						} finally {
							br.close();
						}
						int returnCode = process.exitValue();
						execCode = HGPScanErrorCodeEnum.codeOf(returnCode);
						process.destroy();

						// エラーの場合
						if (execCode != HGPScanErrorCodeEnum.NONE) {
							// 運用上発生する可能性のあるエラーの場合は一定時間停止後リトライ(例：重複実行、PDFが開かれているか権限がない等）
							if(execCode.code == HGPScanErrorCodeEnum.COMMAND_DUPLICATION.code || execCode.code == HGPScanErrorCodeEnum.OPEN_FILE.code ||
									execCode.code == HGPScanErrorCodeEnum.DATA_NOT_FOUND.code || execCode.code == HGPScanErrorCodeEnum.NOT_MONITORING.code ||
									execCode.code == HGPScanErrorCodeEnum.PROCCESSING_OTHER.code || execCode.code == HGPScanErrorCodeEnum.FAILED.code) {

								log.warn("署名コマンド実行に失敗しました。リトライします。：" + EIMResource.getMessage(execCode.key) + "エラーコード：" + returnCode);
								Thread.sleep(sleepTime * 1000);
								continue;
							} else {
								// 異常終了
								String messageBase = EIMResource.getMessage(execCode.key);
								//エラーコードをつける
								String messageFormat = EIMConfig.get("PDF_SIGNATURE_HGPSCAN_ERROR_FORMAT");
								String message  = String.format(messageFormat, returnCode, messageBase, outPutPDFFile.getName(), prntObject.getId());
								log.warn(message);
								log.warn("実行コマンド：" + execCommandLine);
								doErrorAct(sess, object, prntObject, null, log,
										EIMResource.getMessage("EIM.ERROR.LOGIC.PDFSIG.HGPCSAN.STAMPFAIL"), 0);
								return false;
							}
						} else {
							// 成功
							outPutPDFFile = new File(EIMConfig.get("PDF_SIGNATURE_OUT_WORK") + object.getId() + file.getExt());
							if(!outPutPDFFile.exists()){
								log.warn("実行コマンド：" + execCommandLine);
								log.warn("署名済みファイルが存在しません。" + EIMConfig.get("PDF_SIGNATURE_OUT_WORK") + object.getId() + file.getExt());
								return false;
							}
							signCompFlg = true;
							break;
						}
					}

					if(!signCompFlg) {
						// 署名できていないため異常終了
						return false;
					}
				}
			}

			// セキュリティ設定ある場合
			if(secOn != null)
			{
				if(secOn.getInt() != 0)
				{

					//出力先インスタンス作成
					pdfmakeup pobj = new pdfmakeup();//エラーコード取得用
					pmudst dst = new pmudst();
					dst.init();
					//編集元PDFファイルの読み込み
					//読み込み時には常に「PDF署名」オブジェクトからセキュリティパスワードまたは編集用パスワードを取得する必要あり
					String filepass = null;
					EIMAttribute oldsecPassOn = prntObject.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECPASS_ONOFF"));
					EIMAttribute oldsecPass = prntObject.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECPASS"));
					EIMAttribute oldrefPassOn = prntObject.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_REFPASS_ONOFF"));
					EIMAttribute oldrefPass = prntObject.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_REFPASS"));

					//セキュリティパスワードと参照用パスワードが設定されている
					if(oldsecPassOn != null && oldrefPassOn != null)
					{
						//セキュリティパスワードON
						if((oldsecPassOn.getInt() == 1) && (oldsecPass != null))
						{
							filepass = oldsecPass.getString();
						}
						//参照用パスワードON
						else if((oldrefPassOn.getInt() == 1) && (oldrefPass != null))
						{
							filepass = oldrefPass.getString();
						}
					}
					//セキュリティパスワードのみが設定されている
					else if(oldsecPassOn != null)
					{
						//セキュリティパスワードON
						if((oldsecPassOn.getInt() == 1) && (oldsecPass != null))
						{
							filepass = oldsecPass.getString();
						}
					}
					//参照用パスワードのみが設定されている
					else if(oldrefPassOn != null)
					{
						//参照用パスワードON
						if((oldrefPassOn.getInt() == 1) && (oldrefPass != null))
						{
							filepass = oldrefPass.getString();
						}
					}

					if(dst.addsrcfile(outPutPDFFile.getAbsolutePath(), filepass) < 0)
					{
						//出力先インスタンス解放
						dst.release();
						if(sigstat != 4)
						{
							secInfoObj = null;
						}
						doErrorAct(sess, object, prntObject, secInfoObj, log,
								EIMResource.getMessage("EIM.ERROR.LOGIC.PDFSIG.EXECPDFMAKEUP") ,pobj.geterrorno());
						updateNoticePDFSignFailure(sess, docObject);
						return false;
					}

					//PDFファイルへセキュリティ設定
					try
					{
						int ret = doSetPDFSecurity(secPassOn, secPass, refPassOn, refPass, secPrint,
								secEdit, secRem, secRepr, dst, pobj);
						if(ret != 0)
						{
							if(sigstat != 4)
							{
								secInfoObj = null;
							}
							doErrorAct(sess, object, prntObject, secInfoObj, log,
									EIMResource.getMessage("EIM.ERROR.LOGIC.PDFSIG.EXECPDFMAKEUP"), ret);
							updateNoticePDFSignFailure(sess, docObject);
							return false;
						}
					}
					catch(Exception e)
					{
						//出力先インスタンス解放
						dst.release();
						throw e;
					}

					//編集したPDFファイルを出力
					if(dst.outputpdf(outPutPDFFile.getAbsolutePath()) < 0)
					{
						//出力先インスタンス解放
						dst.release();
						if(sigstat != 4)
						{
							secInfoObj = null;
						}
						doErrorAct(sess, object, prntObject, secInfoObj, log,
								EIMResource.getMessage("EIM.ERROR.LOGIC.PDFSIG.EXECPDFMAKEUP"), pobj.geterrorno());
						updateNoticePDFSignFailure(sess, docObject);
						return false;
					}
					//出力先インスタンス解放
					dst.release();

				}
			}

			//--- PDF Makeup ここまで ---//

			//Put File Path
			String putFilePath = publicFormat.getDirectory().getPath() + object.getId() + file.getExt();

			//FTP Upload
			try
			{
				FTPUtils.putFile(EIMConfig.get("FTP_HOST"),
								EIMConfig.get("FTP_USER"),
								EIMConfig.get("FTP_PASS"),
								outPutPDFFile,
								new File(putFilePath));
			}
			catch(Exception e)
			{
				String errMsg = null;
				if(sigstat == 1)
				{
					errMsg += EIMResource.getMessage("EIM.LOG.PDF.SIGN.INITFAILED");
				}
				else if(sigstat == 4)
				{
					errMsg += EIMResource.getMessage("EIM.LOG.PDF.SIGN.CHANGEFAILED");
				}
				errMsg += EIMResource.getMessage("EIM.LOG.PDF.SIGN.FILENAME") + StringUtils.getFileBody(object.getName())
						+ "(" + prntObject.getName() + ")" + EIMResource.getMessage("EIM.LOG.PDF.PREFIX.ERROR")
						+ EIMResource.getMessage("EIM.ERROR.LOGIC.FTPUPLOAD") + EIMResource.getMessage("EIM.LOG.PDF.PREFIX.ERRORCODE")
						+ "-";
				log.warn(errMsg);
				throw e;
			}
			//成功後処理
			try
			{
				doSuccessAct(sess, object);
			}
			catch(Exception e)
			{
				throw e;
			}
		}
		return true;
	}


	/**
	 * PDF署名失敗後処理
	 *
	 * @param sess セッション情報
	 * @param object 処理対象ドキュメントオブジェクト
	 * @param prntObj PDF署名オブジェクト
	 * @param pdfsecObj PDFセキュリティ設定オブジェクト
	 * @param log ログ情報
	 * @param errMsg エラーメッセージ
	 * @param errCode エラーコード(PDFmakeupのエラーの場合のみ)
	 *
	 * @throws Exception 例外発生
	 */
	private void doErrorAct (EIMSession sess, EIMObject object, EIMObject prntObj, EIMObject pdfsecObj,
			Log log, String errMsg, int errCode)
	throws Exception
	{
		String errMessage = "";

		//ダウンロードしたPDFファイル及びCSVファイルを削除
		try
		{
			if(object != null)
			{
				delDownLoadPDF(sess, object);
				delCSVFiles();
			}
		}
		catch(Exception e)
		{
			throw e;
		}
		EIMAttribute attrSigstat = prntObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_STATUS"));
		long sigstat = 0;
		if(attrSigstat != null)
		{
			sigstat = attrSigstat.getInt();
		}
		//初期設定の場合
		if(sigstat == 1)
		{
			if(object != null)
			{
				//「公開処理失敗」を設定
				EIMAttributeType pubFail = AttributeUtils.getAttributeTypeByName
									(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_PUB_PROC_FAIL"));
				ObjectAttributeUtils.setAttribute(sess, object, pubFail, 1);

				//「3:処理失敗」を設定
				EIMAttributeType sigStatType = AttributeUtils.getAttributeTypeByName
									(sess, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_STATUS"));
				ObjectAttributeUtils.setAttribute(sess, prntObj, sigStatType, 3);
			}
			//エラーログ出力用にテンプレートを設定
			errMessage += EIMResource.getMessage("EIM.LOG.PDF.SIGN.INITFAILED");
		}
			//変更の場合
		else if(sigstat == 4)
		{
			//「2:処理済み」を設定
			EIMAttributeType sigStatType = AttributeUtils.getAttributeTypeByName
								(sess, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_STATUS"));
			ObjectAttributeUtils.setAttribute(sess, prntObj, sigStatType, 2);

			EIMObject secObj = null;
			if(pdfsecObj == null)
			{
				//PDFセキュリティ設定オブジェクトを取得
				EIMObjectType objTypePdfSec = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_SECPDF"));
				secObj = ObjectUtils.getObjectByTypeAndName(sess, objTypePdfSec, prntObj.getName());
			}
			else
			{
				secObj = pdfsecObj;
			}

			if(secObj != null)
			{
				//登録者に変更失敗メールを送信
				EIMAttribute uid = secObj.getAttribute(EIMConfig.get("ATTR_NAME_WEPUB_PDF_PDFSIG_USER"));
				EIMUser regUser = null;
				if(uid != null)
				{
					regUser = UserUtils.getUserById(sess, uid.getInt());
					if(regUser != null)
					{
						if(regUser.getMail() != null)
						{
							try
							{
								EIMThreadContext.put("PDF.SEQ.OBJ", secObj);
								EIMThreadContext.put("EIM.ERROR.LOGIC", errMsg);
								MailUtil.execute(sess, object, "IMPL.TYPE.SIGNATURE.FAILED");
							}
							catch(Exception e)
							{
								if(secObj != null)
								{
									ObjectUtils.deleteObject(sess, secObj);
								}
								throw e;
							}
						}
					}
				}
				ObjectUtils.deleteObject(sess, secObj);
			}
			//エラーログ出力用にテンプレートを設定
			errMessage += EIMResource.getMessage("EIM.LOG.PDF.SIGN.CHANGEFAILED");
		}
		//エラーログ共通出力内容を設定
		errMessage += EIMResource.getMessage("EIM.LOG.PDF.SIGN.FILENAME");
		if(object != null)
		{
			errMessage += StringUtils.getFileBody(object.getName()) + "(" + String.valueOf(object.getId()) + ")" ;
		}
		else
		{
			errMessage += "-" + "(" + prntObj.getName() + ")";
		}
		errMessage += EIMResource.getMessage("EIM.LOG.PDF.PREFIX.ERROR") + errMsg
					+ EIMResource.getMessage("EIM.LOG.PDF.PREFIX.ERRORCODE");
		if(errCode != 0)
		{
			errMessage += String.valueOf(errCode);
		}
		else
		{
			errMessage += "-";
		}
		log.warn(errMessage);
		sess.commit();
	}

	/**
	 * PDF署名処理成功時
	 *
	 * @param sess セッション情報
	 * @param object 処理対象ドキュメントオブジェクト
	 *
	 * @throws Exception 例外発生
	 */
	private void doSuccessAct (EIMSession sess, EIMObject object)throws Exception
	{
		//ダウンロードしたPDFファイル及びCSVファイルを削除
		try
		{
			delDownLoadPDF(sess, object);
			delCSVFiles();
		}
		catch(Exception e)
		{
			throw e;
		}
	}

	/**
	 * 署名CSVファイル記入処理
	 * @param appName 承認者名挿入属性
	 * @param appDate 承認日付挿入属性
	 * @param csvFile 書き込むCSVファイルたファイル
	 * @param name 承認者名
	 * @param sdatetime 承認実行日（時刻除く）
	 * @param approveStatusTypeName 承認ステータス名
	 * @return csvFile 作成し署名CSVファイルたファイル
	 * @throws Exception 例外発生
	 */
	private File writeSigunaturCsvFile(EIMAttribute appName, EIMAttribute appDate, File csvFile, String name, String sdatetime, String approveStatusTypeName) throws Exception
	{
		//Error Logging
		Log log = LogFactory.getLog(this.getClass().getName());

		// 承認者名
		String approver = "";
		// 承認日付
		String approveDate = "";

		// 承認者名挿入属性値が挿入する場合
		if((appName != null && appName.getInt() != 0)) {
			approver = name;
		}
		// 承認日付挿入属性値が挿入する場合
		if((appDate != null && appDate.getInt() != 0)) {
			approveDate = sdatetime;
		}

		BufferedWriter bw = null;
		try {
			bw = new BufferedWriter(new FileWriter(csvFile,true));
			bw.write("datestamp" + STRING_COMMA + approveStatusTypeName + STRING_COMMA + approveDate + STRING_COMMA + approver);
			bw.newLine();
		} catch (Exception e) {
			String errMsg = EIMResource.getMessage("EIM.ERROR.LOGIC.FAILED.PDF.SIG.HGPCSAN.CSV.CREATEFAIL");
			log.error(errMsg);
			log.error(e);
			return null;
		} finally {
			if (bw != null) {
			bw.close();
		}
		}

		return csvFile;
	}

	/**
	 * PDFセキュリティ設定
	 *
	 * @param secPassOn セキュリティパスワード設定有無
	 * @param secPass セキュリティパスワード
	 * @param refPassOn 参照用パスワード設定有無
	 * @param refPass 参照用パスワード
	 * @param secPrint 印刷許可設定
	 * @param secEdit 編集許可設定
	 * @param secRem 注釈追加許可設定
	 * @param secRepr 転載許可設定
	 * @param dst 出力先オブジェクト
	 * @param pobj pdfmakeupオブジェクト
	 *
	 * @return 処理結果(0,またはエラーコード)
	 * @throws Exception 例外発生
	 *
	 */
	private int doSetPDFSecurity(EIMAttribute secPassOn, EIMAttribute secPass, EIMAttribute refPassOn,
			EIMAttribute refPass, EIMAttribute secPrint, EIMAttribute secEdit, EIMAttribute secRem, EIMAttribute secRepr,
			pmudst dst, pdfmakeup pobj)throws Exception
	{
		//設定値を取得
		String srefPass = "";
		if((refPassOn != null) && (refPassOn.getInt() == 1))
		{
			if(refPass != null)
			{
				srefPass = refPass.getString();
			}
		}
		String ssecPass = "";
		if((secPassOn != null) && (secPassOn.getInt() == 1))
		{
			if(secPass != null)
			{
				ssecPass = secPass.getString();
			}
		}
		boolean ssecPrint = false;
		if((secPrint != null) &&  (secPrint.getInt()== 1))
		{
			ssecPrint = true;
		}
		boolean ssecEdit = false;
		if((secEdit != null) && (secEdit.getInt() == 1))
		{
			ssecEdit = true;
		}
		boolean ssecRepr = false;
		if((secRepr != null) && (secRepr.getInt() == 1))
		{
			ssecRepr = true;
		}
		boolean ssecRem = false;
		if((secRem != null) && (secRem.getInt() == 1))
		{
			ssecRem = true;
		}

		//セキュリティを設定
		if(dst.setsecurity(srefPass, ssecPass, ssecPrint, ssecEdit, ssecRepr, ssecRem) < 0)
		{
			return pobj.geterrorno();
		}

		return 0;
	}

	/**
	 * ダウンロードPDFファイル削除処理
	 *
	 * @param sess EIMSession
	 * @param object 処理対象オブジェクト
	 * @throws Exception 例外発生
	 */
	private void delDownLoadPDF(EIMSession sess, EIMObject object)throws Exception
	{
		//Pubilc Format
		EIMFormat publicFormat  = FileUtils.getFormatByName(sess, EIMConfig.get("FORMAT_NAME_PUBLIC"));
		EIMFile file = FileUtils.getFile(sess, object, publicFormat);
		if(file != null)
		{
			File orgPdfFile = new File(EIMConfig.get("PDF_SIGNATURE_IN_WORK") + object.getId() + file.getExt());
			if((orgPdfFile != null) && (orgPdfFile.exists() == true))
			{
				orgPdfFile.delete();
			}
			File outPutPdfFile = new File(EIMConfig.get("PDF_SIGNATURE_OUT_WORK") + object.getId() + file.getExt());
			if((outPutPdfFile != null) && (outPutPdfFile.exists() == true))
			{
				outPutPdfFile.delete();
			}
		}
	}

	/**
	 * CSVファイル削除処理
	 *
	 * @throws Exception 例外発生
	 */
	private void delCSVFiles()throws Exception
	{
		if(csvFileList.size() != 0)
		{
			for(int i = 0; i < csvFileList.size(); i++)
			{
				File delFile = (File)csvFileList.get(i);
				if((delFile != null) && (delFile.exists() == true))
				{
					delFile.delete();
				}
			}
		}
	}

	/**
	 * 承認者情報追加処理
	 *
	 * @param index リスト追加対象のインデックス値
	 * @param approveNameLang 電子署名用言語
	 * @throws Exception 例外発生
	 */
	private void addAppInfoToList(EIMSession sess, int index, EIMAttribute approveNameLang)throws Exception
	{
		EIMUser appUser = (EIMUser)userList.get(index);
		if(appUser != null)
		{
			// 電子署名用言語により、「承認者名」を設定
			if(approveNameLang.getString().equals("EN"))
			{
				nameList.add(UserUtils.getOtherUserName(sess, appUser.getId(), "EN"));
			} else if (approveNameLang.getString().equals("JA")){
				nameList.add(UserUtils.getOtherUserName(sess, appUser.getId(), "JA"));
			}

			//「承認者ID」
			idList.add(new Long(appUser.getId()));
			//「承認日」
			sdateList.add(dfm.format((Date)dateList.get(index)));
			//「承認時刻」
			timeList.add(dfmt.format((Date)dateList.get(index)));
			//「ステータスタイプ表示名」
			outDispStatusTypeList.add(dispStatusTypeList.get(index));
		}
	}



	/**
	 * @param sess セッション情報
	 * @param object PDF署名対象オブジェクト
	 */
	static private void updateNoticePDFSignFailure(EIMSession sess, EIMObject object) throws Exception
	{
		// SearchFramework 検索FW更新通知 対象：WFドキュメント、WF付きフォルダと配下のフォルダおよびドキュメント(PDF署名失敗)
		AppUpdateNoticeUtils.updateNoticeInsertWFObject(sess, object,
				"SEARCHFW_PDFSIGN_DOCUMENT", "SEARCHFW_PDFSIGN_FOLDER",
				"SEARCHFW_PDFSIGN_CHILD_DOCUMENT", "SEARCHFW_PDFSIGN_CHILD_FOLDER");

	}


	/**
	 * 最新の承認依頼者と全員承認のリストをイベント履歴より取得する。
	 *
	 * @param eventList	全イベント履歴のリスト(日付でソート済み)
	 * @return
	 * @throws Exception
	 */
	private List<EventLogDomain> getApproveAndApprovalRequestInfoEventList(List<EventLogDomain> eventList) throws Exception {

		// 各イベントタイプの最新のイベントのみ残す。ただし、全員承認のステータスについては依頼取消有無をチェックする。
		HashMap<Long, Boolean> eventMap = new HashMap<Long, Boolean>();
		HashMap<Long, Boolean> statusNotStepMap = new HashMap<Long, Boolean>();
		HashMap<Long, Boolean> statusStepMap = new HashMap<Long, Boolean>();
		List<EventLogDomain> appEvList = new ArrayList<EventLogDomain>();
		for(int kk=eventList.size()-1; kk>=0 ; kk--)
		{
			EventLogDomain eventLogDomain = eventList.get(kk);
			EventTypeDomain eventTypeDomain = eventLogDomain.getEvent().getEventType();
			if(eventTypeDomain.getBaseEventType().getKey().equals("BaseEvtApprovalPlugIn") ||
					eventTypeDomain.getBaseEventType().getKey().equals("BaseEvtApprovalRequestPlugIn"))
			{
				// 最後の承認者ではない場合
				if (eventTypeDomain.getGuardCondition().getId() == AppConstant.GUARD_COND_ID_FINAL_APPROVED_IS_NOT)
				{

					Boolean statusNotStepFlag = statusNotStepMap.get(eventTypeDomain.getFromStatusType().getId());
					Boolean statusStepFlag = statusStepMap.get(eventTypeDomain.getFromStatusType().getId());

					if((statusNotStepFlag == null || statusNotStepFlag != true ) && (statusStepFlag == null || statusStepFlag != true) )
					{
						appEvList.add(eventLogDomain);
						eventMap.put((long)eventTypeDomain.getId(), true);
					}
				}
				else
				{
					// 対象イベントと同じイベントタイプが登録されていれば対象外
					if( eventMap.get(eventTypeDomain.getId()) == null || eventMap.get(eventTypeDomain.getId()) != true )
					{

						if(appEvList.size() == 0)// 初回、appEvListに何も入っていない場合は、既存コード。条件無しでaddEvListに追加
						{
							appEvList.add(eventLogDomain);
							eventMap.put((long)eventTypeDomain.getId(), true);
						}

						// eventTypeDomainのfromステータスの順序がappEvListの最後の値より小さい場合、捺印ルートに追加（スキップ承認を考慮）
						else if(appEvList.get(appEvList.size()-1).getEvent().getFromStatus().getStatusType().getSeq() > eventTypeDomain.getFromStatusType().getSeq())
						{
							appEvList.add(eventLogDomain);
							eventMap.put((long)eventTypeDomain.getId(), true);
						}
					}
				}
			}
			else if(eventTypeDomain.getBaseEventType().getKey().equals("BaseEvtCancelApprovalRequestPlugIn"))
			{
				// ステータスが遷移しない依頼取消の場合、
				if(eventTypeDomain.getFromStatusType().getId() == eventTypeDomain.getToStatusType().getId())
				{
					statusNotStepMap.put((long)eventTypeDomain.getFromStatusType().getId(), true);
				}
				else
				{
					// ステータスが遷移する依頼取消場合は、遷移元の承認は含めない
					statusStepMap.put((long)eventTypeDomain.getToStatusType().getId(), true);
				}
			}
			else if(eventTypeDomain.getBaseEventType().getKey().equals("BaseEvtSendBackPlugIn") ||
					eventTypeDomain.getBaseEventType().getKey().equals("BaseEvtTakeBackRequestPlugIn") ||
					eventTypeDomain.getBaseEventType().getKey().equals("BaseEvtApprovalRequestPlugIn"))
			{
				// 差戻し、取戻し、承認依頼の場合、その後はチェックしなくて良い。
				break;
			}
		}
		return appEvList;
	}


	/**
	 * 最新の全員承認のリストをイベント履歴より取得する。
	 *
	 * @param eventList	全イベント履歴のリスト(日付でソート済み)
	 * @return
	 * @throws Exception
	 */
	private List<EventLogDomain> getApproveInfoEventList(List<EventLogDomain> eventList) throws Exception {

		// 各イベントタイプの最新のイベントのみ残す。ただし、全員承認のステータスについては依頼取消有無をチェックする。
		HashMap<Long, Boolean> eventMap = new HashMap<Long, Boolean>();
		HashMap<Long, Boolean> statusNotStepMap = new HashMap<Long, Boolean>();
		HashMap<Long, Boolean> statusStepMap = new HashMap<Long, Boolean>();
		List<EventLogDomain> appEvList = new ArrayList<EventLogDomain>();
		for(int kk=eventList.size()-1; kk>=0 ; kk--)
		{
			EventLogDomain eventLogDomain = eventList.get(kk);
			EventTypeDomain eventTypeDomain = eventLogDomain.getEvent().getEventType();
			if(eventTypeDomain.getBaseEventType().getKey().equals("BaseEvtApprovalPlugIn"))
			{
				// 最後の承認者ではない場合
				if (eventTypeDomain.getGuardCondition().getId() == AppConstant.GUARD_COND_ID_FINAL_APPROVED_IS_NOT)
				{

					Boolean statusNotStepFlag = statusNotStepMap.get(eventTypeDomain.getFromStatusType().getId());
					Boolean statusStepFlag = statusStepMap.get(eventTypeDomain.getFromStatusType().getId());

					if((statusNotStepFlag == null || statusNotStepFlag != true ) && (statusStepFlag == null || statusStepFlag != true) )
					{
						appEvList.add(eventLogDomain);
						eventMap.put((long)eventTypeDomain.getId(), true);
					}
				}
				else
				{
					// 対象イベントと同じイベントタイプが登録されていれば対象外
					if( eventMap.get(eventTypeDomain.getId()) == null || eventMap.get(eventTypeDomain.getId()) != true )
					{

						if(appEvList.size() == 0)// 初回、appEvListに何も入っていない場合は、既存コード。条件無しでaddEvListに追加
						{
							appEvList.add(eventLogDomain);
							eventMap.put((long)eventTypeDomain.getId(), true);
						}

						//  eventTypeDomainのfromステータスの順序がappEvListの最後の値より小さい場合、捺印ルートに追加（スキップ承認を考慮）
						else if(appEvList.get(appEvList.size()-1).getEvent().getFromStatus().getStatusType().getId() > eventTypeDomain.getFromStatusType().getId())
						{
							appEvList.add(eventLogDomain);
							eventMap.put((long)eventTypeDomain.getId(), true);
						}
					}
				}
			}
			else if(eventTypeDomain.getBaseEventType().getKey().equals("BaseEvtCancelApprovalRequestPlugIn"))
			{
				// ステータスが遷移しない依頼取消の場合、
				if(eventTypeDomain.getFromStatusType().getId() == eventTypeDomain.getToStatusType().getId())
				{
					statusNotStepMap.put((long)eventTypeDomain.getFromStatusType().getId(), true);
				}
				else
				{
					// ステータスが遷移する依頼取消場合は、遷移元の承認は含めない
					statusStepMap.put((long)eventTypeDomain.getToStatusType().getId(), true);
				}
			}
			else if(eventTypeDomain.getBaseEventType().getKey().equals("BaseEvtSendBackPlugIn") ||
					eventTypeDomain.getBaseEventType().getKey().equals("BaseEvtTakeBackRequestPlugIn") ||
					eventTypeDomain.getBaseEventType().getKey().equals("BaseEvtApprovalRequestPlugIn"))
			{
				// 差戻し、取戻し、承認依頼の場合、その後はチェックしなくて良い。
				break;
			}
		}
		return appEvList;
	}

	/**
	 * XMLから取得したステータスMapより指定言語のステータスタイプ名を返却する
	 * @param eventDomain
	 * @param statusTypeXMLMap
	 * @return ステータスタイプ名
	 */
	private static String getStatusTypeNameFromXML(EventDomain eventDomain,HashMap<String,String[]> statusTypeXMLMap){
		String statuTypeNameFromXML = "";
		String[] statustypeNames = statusTypeXMLMap.get(eventDomain.getFromStatus().getStatusType().getDefName());
		if(statustypeNames != null){
			if(EIMConfig.get("SIGNATURE_STATUS_TYPE_LANG").equals("JA")){
				statuTypeNameFromXML = statustypeNames[0];
			}else{
				statuTypeNameFromXML = statustypeNames[1];
			}
		}else{
			// statusTypeXMLMapに存在しない場合、DBのステータスタイプ名を設定する
			StatusTypeDomain fromStatustype = eventDomain.getFromStatus().getStatusType();
			for(OtherNameDomain otherName:fromStatustype.getNameList()){
				if(EIMConfig.get("SIGNATURE_STATUS_TYPE_LANG").equals("JA")){
					if(otherName.getLangId().equals("JA")){
						statuTypeNameFromXML = otherName.getName();
					}
				}else{
					if(otherName.getLangId().equals("EN")){
						statuTypeNameFromXML = otherName.getName();
					}
				}
			}
		}
		return statuTypeNameFromXML;
	}

}