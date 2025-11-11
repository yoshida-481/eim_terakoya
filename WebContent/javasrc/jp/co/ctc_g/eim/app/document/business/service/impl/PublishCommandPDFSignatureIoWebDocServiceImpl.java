/**
 *
 */
package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.io.File;
import java.io.UnsupportedEncodingException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import common.util.AppConstant;
import common.util.AppObjectConditionHelper;
import common.util.AppObjectUtil;
import common.util.AppUpdateNoticeUtils;
import common.util.MailUtil;
import common.util.SignatureStatusTypeUtils;
import eim.bo.EIMAttribute;
import eim.bo.EIMAttributeType;
import eim.bo.EIMException;
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
import jp.co.ctc_g.eim.app.document.business.service.PublishCommandPDFSignatureIoWebDocService;
import jp.co.ctc_g.eim.framework.business.domain.EventHistoryDomain;
import jp.co.ctc_g.eim.framework.business.domain.EventLogDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.EventDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.EventTypeDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.OtherNameDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.StatusTypeDomain;
import jp.co.ctc_g.eim.framework.business.service.EventHistoryService;
import jp.co.ctc_g.eim.framework.common.util.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import yss.iothe.iowebdoc.webdocmem;
import yss.pdfmakeup.pdfmakeup;
import yss.pdfmakeup.pmudst;
import yss.pdfmakeup.pmuobjimage;
import yss.pdfmakeup.pmuobjiod;


/**
 * PDF 署名処理アドオン
 */
public class PublishCommandPDFSignatureIoWebDocServiceImpl implements PublishCommandPDFSignatureIoWebDocService {

	/** 署名イメージ作成用 */
	//mm→point変換用
	private final double Point = 2.8346;
	//印影直径の最小値
	private final int MIN_IMAGE_SIZE = 0;
	private final int MIN_HEIGHT_SIZE = 0;
	private final double IMG_POS_RATE = 0.5625;

	/**	List */
	private static List imgFileList = new ArrayList();
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

	// IOD署名処理_失敗判定用
	private int stsIod = 0;

	//DateFormat
	private static final SimpleDateFormat dfm = new SimpleDateFormat("yyyy/MM/dd");
	private static final SimpleDateFormat dfmt = new SimpleDateFormat("HH:mm:ss");

	/** 画像フォーマット*/
	private static final String IMG_FORMAT_IOD = "iod";


	/**
	 * PDF署名処理のバッチ用公開処理実行メソッド
	 * 印影イメージ作成にIOWebDocで作成したテンプレートを利用する
	 *
	 * 1) 公開 Format から FTP GET
	 * 2) 初期設定の場合
	 * 2-1) PDF署名オブジェクトから情報取得
	 * 2-2) 印影イメージ作成・埋め込み
	 * 3) 変更の場合
	 * 3-1) PDFセキュリティ設定オブジェクトから情報取得
	 * 3-2) 変更元ファイルのパスワード情報をPDF署名オブジェクトから取得
	 * 4) YSS Makeup 実行
	 * 5) FTP PUT
	 * 6) PDFファイル・印影イメージファイル削除
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
			imgFileList.clear();
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
			File orgFile = new File(EIMConfig.get("WORK") + object.getId() + file.getExt());
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

			//ワークフローを取得
			EIMStatus eimStatus = docObject.getStatus();
			EIMWorkFlow workFlow = WorkFlowUtils.getWorkFlowByStatusType(sess, eimStatus.getType());

			// ステータスタイプ設定XML取得処理
			HashMap<String,String[]> statusTypeXMLMap = SignatureStatusTypeUtils.getDisplayStatusName(sess, workFlow);
			//セキュリティ情報取得対象オブジェクト
			EIMObject secInfoObj = null;

			EIMAttribute doSign = prntObject.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_ONOFF"));
			EIMAttribute appName = prntObject.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_APPNAME"));

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
						EventHistoryService eventHistoryService =  (EventHistoryService)ApplicationContextLoader.getContext().getBean("eventHistoryServiceWithTypeAndFromStatusAndComment");
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
								addAppInfoToList(sess, index);
							}
						}
						//「2:全承認者を挿入」の場合
						else
						{
////////////////////////////修正部分2([承認者が全承認者]イベント履歴からベースイベントタイプが"承認"となるものを取得し、そこから承認者、承認日、コメントを取得する)
							List<EventLogDomain> appEvList = new ArrayList<EventLogDomain>();
							if(Boolean.valueOf(EIMConfig.get("IOD_REQUEST_INCLUDE"))) {
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
								addAppInfoToList(sess, appEvCnt);
								appEvCnt++;
							}

////////////////////////////修正部分2終わり

						}
						//承認者名を埋め込んだ印影イメージファイル作成
						//--- イメージファイル作成 ---//
						//イメージファイル出力先へのパスを取得
						String imgoutPath = EIMConfig.get("WORK");
						for(int j = nameList.size()-1; j >= 0; j--)
						{
							String datetime = formatDate(new Date());
							File imageFileObj = null;

							// ステータスタイプの表示名の設定で表示/非表示を判定する
							if(EIMConfig.get("IOD_SIGNATURE_STATUS_TYPE_CONF_DEFAURT_USE").equals("true")){
								// ステータスタイプの表示名が空か判定
								if(outDispStatusTypeList.get(j).toString().equals("")){
									// 空の場合は捺印しない
									continue;
								}
								// 署名イメージ作成
								imageFileObj = makeStampImage(prntObject, (String)nameList.get(j), imgoutPath, datetime, sdateList.get(j).toString(), outDispStatusTypeList.get(j).toString());
							} else {
								// 署名イメージ作成
								imageFileObj = makeStampImage(prntObject, (String)nameList.get(j), imgoutPath, datetime, sdateList.get(j).toString(), outDispStatusTypeList.get(j).toString());
							}

							if(imageFileObj == null)
							{
								doErrorAct(sess, object, prntObject, null, log,
										EIMResource.getMessage("EIM.ERROR.LOGIC.PDFSIG.IODIMAGE.STAMPFAIL"), stsIod);
								return false;
							}
							imgFileList.add(imageFileObj);
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

			if(dst.addsrcfile(orgFile.getAbsolutePath(), filepass) < 0)
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

			//PDFファイルへ署名設定
			if(doSign != null && appName != null)
			{
				if(doSign.getInt() == 1)
				{
					for(int j = 0; j < imgFileList.size(); j++)
					{
						int ret = 0;
						try
						{
							if((ret = doPDFSignature(prntObject, pobj, dst))!= 0)
							{
								//出力先インスタンス解放
								dst.release();
								doErrorAct(sess, object, prntObject, null, log,
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
					}
				}
			}

			//PDFファイルへセキュリティ設定
			if(secOn != null)
			{
				if(secOn.getInt() != 0)
				{
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
				}
			}

			//編集したPDFファイルを出力
			if(dst.outputpdf(orgFile.getAbsolutePath()) < 0)
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
			//--- PDF Makeup ここまで ---//

			//Put File Path
			String putFilePath = publicFormat.getDirectory().getPath() + object.getId() + file.getExt();

			//FTP Upload
			try
			{
				FTPUtils.putFile(EIMConfig.get("FTP_HOST"),
								EIMConfig.get("FTP_USER"),
								EIMConfig.get("FTP_PASS"),
								orgFile,
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

		//ダウンロードしたPDFファイル及び印影イメージファイルを削除
		try
		{
			if(object != null)
			{
				delDownLoadPDF(sess, object);
				delImageFiles();
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
		//ダウンロードしたPDFファイル及び印影イメージファイルを削除
		try
		{
			delDownLoadPDF(sess, object);
			delImageFiles();
		}
		catch(Exception e)
		{
			throw e;
		}
	}

	/**
	 * イメージ署名ファイル作成処理
	 * @param prntObject ワークフロー公開処理用オブジェクト
	 * @param name 承認者名
	 * @param outpath 作成イメージファイル出力先ディレクトリ
	 * @param datetime 作成ファイル名(拡張子除く)
	 * @param sdatetime 承認実行日（時刻除く）
	 * @param approveStatusTypeName 承認ステータス名
	 *
	 * @return imageFileObj 作成した印影イメージファイル
	 * @throws Exception 例外発生
	 */
	private File makeStampImage(EIMObject prntObject, String name, String outpath, String datetime, String sdatetime, String approveStatusTypeName)
	throws Exception
	{
		File imageFileObj = this.createIoWebDocImage(prntObject,name, outpath, datetime, sdatetime, approveStatusTypeName);
		return imageFileObj;
	}

	/**
     * 日付オブジェクトに設定されている時間を"yyyyMMddHHmmssSSS"形式の文字列に
     * 変換します。
     * @param date 日付オブジェクトを指定します。
     * @return "yyyyMMddHHmmssSSS"形式の文字列を返します。
     */
    private String formatDate(Date date)
    {
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyyMMddHHmmssSSS");
        return dateFormat.format(date);
    }

	/**
	 * PDF署名処理
	 *
	 * @param prntObj PDF署名オブジェクト
	 * @param pobj pdfmakeupオブジェクト
	 * @param dst 出力先オブジェクト
	 *
	 * @return 処理結果(0,またはエラーコード)
	 * @throw Exception 例外発生
	 */
	private int doPDFSignature(EIMObject prntObject, pdfmakeup pobj, pmudst dst)throws Exception
	{
		//イメージ追記クラスへの設定値を取得
		EIMAttribute stampPosBase = prntObject.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_BASELINE"));
		EIMAttribute stampPosX = prntObject.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_BASELINE_X"));
		EIMAttribute stampPosY = prntObject.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_BASELINE_Y"));
		EIMAttribute insPage = prntObject.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_INSPAGE"));

		int imageSize = Integer.parseInt(EIMConfig.get("IOD_FILE_WIDTH_SIZE"));
		if(imageSize < MIN_IMAGE_SIZE)
		{
			imageSize = MIN_IMAGE_SIZE;
		}

		double widthRate = Double.parseDouble(EIMConfig.get("PDF_SIG_IMAGE_WIDTH_RATE"));
		double heightRate = Double.parseDouble(EIMConfig.get("PDF_SIG_IMAGE_HEIGHT_RATE"));

		if(widthRate == 0)
		{
			widthRate = 1.0;
		}
		if(heightRate == 0)
		{
			heightRate = 1.0;
		}

		for(int j = 0; j < imgFileList.size(); j++)
		{
			pmuobjiod objimg = dst.createobjiod();

			//挿入位置
			int val = 0;
			int basepos = 0; //defaultは「右上」に設定
			if(stampPosBase != null)
			{
				// switch使用のためintにキャスト
				basepos = (int) stampPosBase.getInt();
			}
			switch(basepos)
			{
				case 0: val = pmuobjimage.POS_RT; break;
				case 1: val = pmuobjimage.POS_RB; break;
				case 2: val = pmuobjimage.POS_LT; break;
				case 3: val = pmuobjimage.POS_LB; break;
				default:val = pmuobjimage.POS_RT; break;
			}
			if(objimg.setbasepos(val) < 0)
			{
				return pobj.geterrorno();
			}
			//位置調整
			double posx = 0;
			double posy = 0;
			//X座標の設定
			if(val == pmuobjimage.POS_RT || val == pmuobjimage.POS_RB)
			{
				if(stampPosX != null)
				{
					posx = -(stampPosX.getInt()) * Point - (imgFileList.size()-1-j) * imageSize * widthRate;
				}
				else
				{
					posx = -(imgFileList.size()-1-j) * imageSize * widthRate;
				}
			}
			else
			{
				if(stampPosX != null)
				{
					posx = stampPosX.getInt() * Point + j * imageSize * widthRate;
				}
				else
				{
					posx = j * imageSize * widthRate;
				}
			}
			//Y座標の設定
			if(val == pmuobjimage.POS_RT || val == pmuobjimage.POS_LT)
			{
				if(stampPosY != null)
				{
					posy = stampPosY.getInt() * Point;
				}
				else
				{
					posy = 0;
				}
			}
			else
			{
				if(stampPosY != null)
				{
					posy = -(stampPosY.getInt()) * Point - (imageSize * heightRate * IMG_POS_RATE);
				}
				else
				{
					posy = -(imageSize * heightRate * IMG_POS_RATE);
				}
			}
			if(objimg.movepos(posx, posy) < 0)
			{
				return pobj.geterrorno();
			}
			//レイヤ(背景)
			if(objimg.setlayer(pmuobjimage.LAYER_FRONT) < 0)
			{
				return pobj.geterrorno();
			}
			//挿入ページ
			int pagetype = 0; //defaultは「全ページ」に設定
			if(insPage != null)
			{
				// switch使用のためintにキャスト
				pagetype = (int) insPage.getInt();
			}
			switch(pagetype)
			{
				case 0: if(objimg.settargetpage(pmuobjimage.PAGETYPE_ALL, 0, 0) < 0)
						{
							return pobj.geterrorno();
						}
						break;
				case 1: if(objimg.settargetpage(pmuobjimage.PAGETYPE_PAGE, 1, 0) < 0)
						{
							return pobj.geterrorno();
						}
						break;
				default:if(objimg.settargetpage(pmuobjimage.PAGETYPE_ALL, 0, 0) < 0)
						{
							return pobj.geterrorno();
						}
						break;
			}

			if(objimg.setfilename(((File)imgFileList.get(j)).getAbsolutePath()) < 0)
			{
				return pobj.geterrorno();
			}
		}
		return 0;
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
			File pdfFile = new File(EIMConfig.get("WORK") + object.getId() + file.getExt());
			if((pdfFile != null) && (pdfFile.exists() == true))
			{
				pdfFile.delete();
			}
		}
	}

	/**
	 * 印影イメージファイル削除処理
	 *
	 * @throws Exception 例外発生
	 */
	private void delImageFiles()throws Exception
	{
		if(imgFileList.size() != 0)
		{
			for(int i = 0; i < imgFileList.size(); i++)
			{
				File delFile = (File)imgFileList.get(i);
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
	 * @throws Exception 例外発生
	 */
	private void addAppInfoToList(EIMSession sess, int index)throws Exception
	{
		EIMUser appUser = (EIMUser)userList.get(index);
		if(appUser != null)
		{
			// 「承認者名」※受信メール言語と同じ言語の名称を設定
			if(appUser.getLang().equals("EN"))
			{
				nameList.add(UserUtils.getOtherUserName(sess, appUser.getId(), appUser.getLang()));
			} else {
				nameList.add(appUser.getName());
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

					Boolean statusNotStepFlag = statusNotStepMap.get((long)eventTypeDomain.getFromStatusType().getId());
					Boolean statusStepFlag = statusStepMap.get((long)eventTypeDomain.getFromStatusType().getId());

					if((statusNotStepFlag == null || statusNotStepFlag != true ) && (statusStepFlag == null || statusStepFlag != true) )
					{
						appEvList.add(eventLogDomain);
						eventMap.put((long)eventTypeDomain.getId(), true);
					}
				}
				else
				{
					// 対象イベントと同じイベントタイプが登録されていれば対象外
					if( eventMap.get((long)eventTypeDomain.getId()) == null || eventMap.get((long)eventTypeDomain.getId()) != true )
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

					Boolean statusNotStepFlag = statusNotStepMap.get((long)eventTypeDomain.getFromStatusType().getId());
					Boolean statusStepFlag = statusStepMap.get((long)eventTypeDomain.getFromStatusType().getId());

					if((statusNotStepFlag == null || statusNotStepFlag != true ) && (statusStepFlag == null || statusStepFlag != true) )
					{
						appEvList.add(eventLogDomain);
						eventMap.put((long)eventTypeDomain.getId(), true);
					}
				}
				else
				{
					// 対象イベントと同じイベントタイプが登録されていれば対象外
					if( eventMap.get((long)eventTypeDomain.getId()) == null || eventMap.get((long)eventTypeDomain.getId()) != true )
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
	 * 印鑑の画像を作成してファイルに保存します。
	 * <ul>
	 * <li>作成される印鑑画像の説明<br/>
	 *      事前に作成されたIODファイルのテンプレートに承認者名、承認日（時刻除く）、承認ステータスを追記して印鑑画像を作成します。
	 *      印鑑画像に出力される項目は設定ファイルで定義された項目のみです。
	 *
	 * </li>
	 * <li>画像を保存するファイルの説明</br>
	 *      画像を保存先するファイルは、fileOutputPathで指定されたディレクトリに
	 *      保存されます。またファイルの名前は、
	 *      「<i>&lt;yyyyMMddHHmmssSSS形式のサーバー時刻&gt;</i>_<i>&lt;セッションID&gt;</i>.iod」
	 *      となります。
	 * </li>
	 * </ul>
	 * @param prntObject ワークフロー公開処理用オブジェクト
	 * @param userName 印鑑のイメージに使用する文字列を指定します。
	 * @param fileOutputPath ファイルの出力パスを指定します。
	 * @param datetime 作成ファイル名(拡張子除く)
	 * @param sdatetime 承認実行日（時刻除く）
	 * @param approveStatusTypeName 承認ステータス名
	 *
	 * @return 作成された印鑑のイメージを保存したファイルを返します。
	 * @throws Exception イメージファイルの作成に失敗した場合。エラーの原因は
	 *      以下のとおりです。
	 *      <ul>
	 *      <li>画像を保存するファイルが作成できない
	 *          ({@link java.io.IOException}が発生した場合)</li>
	 *      </ul>
	 */
	private File createIoWebDocImage(EIMObject prntObject, String userName, String fileOutputPath, String datetime, String sdatetime, String approveStatusTypeName) throws Exception {
		try
		{
			String imageFileName = datetime + "." + IMG_FORMAT_IOD;
			File imageFile = new File(fileOutputPath, imageFileName);

			// IODファイルを取得
			String iodTemplate = EIMConfig.get("IOD_FILE_TEMPLATE");

			// IODファイルの存在チェック
			File checkFileTemp = new File(iodTemplate);
			if (!checkFileTemp.exists()){
				throw new EIMException("EIM.ERROR.CONFIG.SETTING.ERROR", new Object[]{iodTemplate});
			}

			// 承認ユーザ名のbyte長設定チェック
			try {
				Integer.parseInt(EIMConfig.get("IOD_USER_LENGTH"));
			} catch (Exception e) {
				throw new EIMException("EIM.ERROR.CONFIG.SETTING.ERROR", new Object[]{"IOD_USER_LENGTH"});
			}

			// 承認ステータス名のbyte長設定チェック
			try {
				Integer.parseInt(EIMConfig.get("IOD_STATUS_LENGTH"));
			} catch (Exception e) {
				throw new EIMException("EIM.ERROR.CONFIG.SETTING.ERROR", new Object[]{"IOD_STATUS_LENGTH"});
			}


			webdocmem webDocMem = new webdocmem();

			// IOD処理の失敗判定変数を初期化
			stsIod = 0;

			// IODファイルをロード
			stsIod = webDocMem.loadiod(iodTemplate);
			// 処理失敗
			if(stsIod < 0) {
				return null;
			}

			stsIod = webDocMem.setoutiod(fileOutputPath + "/" + imageFileName);
			// 処理失敗
			if(stsIod < 0) {
				return null;
			}

			// IODファイルに表示する対象を設定から取得
			String outputItems = EIMConfig.get("IOD_OUTPUT_ITEM");
			String[] values = outputItems.split(",");

			String nameId = "name";		// 承認者を示す識別子
			String userNameData = "";	// 承認者名
			String dateId = "yyyymmdd";	// 承認日を示す識別子
			String approveDate = "";	// 承認日（時刻は除く）
			String statusId = "status";	// 承認ステータスを示す識別子
			String approveStatus = "";	// 承認ステータス

			// 承認日、承認者の捺印有無属性を取得する
			EIMAttribute isApproverStamp = prntObject.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_APPNAME"));//承認者名挿入
			EIMAttribute isApproveDateStamp = prntObject.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_APPDATE"));//承認日付挿入

			// ユーザ、承認日、承認ステータスのうち、指定のある項目のみ捺印に埋め込む
			String outputItem = "";
			for (int j = 0; j < values.length; j++)
			{
				outputItem = values[j];

				if(outputItem.equals("approveUser") && (isApproverStamp != null && isApproverStamp.getInt() != 0))
				{
					int byteLength = getByteLength(userName, "Shift_JIS");// IOWebDocのエンコードであるShift_JISを固定で指定
					userNameData = userName;
					if(byteLength > Integer.parseInt(EIMConfig.get("IOD_USER_LENGTH")))
					{
						nameId = "name_long";
					}
				}

				if(outputItem.equals("approveDay") && (isApproveDateStamp != null && isApproveDateStamp.getInt() != 0))
				{
					approveDate = sdatetime;
				}

				if(outputItem.equals("approveStatus"))
				{
					String statusTemp = approveStatusTypeName;

					int byteLength = getByteLength(statusTemp, "Shift_JIS");// IOWebDocの初期エンコードであるShift_JISを固定で指定
					approveStatus = statusTemp;
					if(byteLength > Integer.parseInt(EIMConfig.get("IOD_STATUS_LENGTH")))
					{
						statusId = "status_long";
					}
				}
			}

			// 承認者名を設定
			stsIod = webDocMem.setiddata(nameId, userNameData);
			// 処理失敗
			if(stsIod < 0) {
				return null;
			}
			// 承認日を設定
			stsIod = webDocMem.setiddata(dateId, approveDate);
			// 処理失敗
			if(stsIod < 0) {
				return null;
			}
			// 承認ステータスを設定
			stsIod = webDocMem.setiddata(statusId, approveStatus);
			// 処理失敗
			if(stsIod < 0) {
				return null;
			}

			// 情報を設定したIODを出力
			stsIod = webDocMem.outpage();
			// 処理失敗
			if(stsIod < 0) {
				return null;
			}

			// PDFクローズ、インスタンス解放
			stsIod = webDocMem.outend();
			// 処理失敗
			if(stsIod < 0) {
				return null;
			}

			webDocMem.release();

			return imageFile;

		} catch (Exception e)
		{
			throw new Exception(e);
		}
	}

	/**
	 * 文字エンコーディング Shift_JIS での文字列のバイト数を取得
	 *
	 * @param value 処理対象文字列
	 * @param param enc 文字エンコード("Shift_JIS", "UTF-8"など)
	 *
	 * @return 文字列のバイト数
	 */
	private int getByteLength(String value, String enc)
	{
		if(value == null || value.length() == 0 )
		{
			return 0;
		}

		int ret = 0;

		try
		{
			ret = value.getBytes(enc).length;
		}
		catch (UnsupportedEncodingException e)
		{
			ret = 0;
		}
		return ret;
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
			if(EIMConfig.get("IOD_SIGNATURE_STATUS_TYPE_LANG").equals("JA")){
				statuTypeNameFromXML = statustypeNames[0];
			}else{
				statuTypeNameFromXML = statustypeNames[1];
			}
		}else{
			// statusTypeXMLMapに存在しない場合、DBのステータスタイプ名を設定する
			StatusTypeDomain fromStatustype = eventDomain.getFromStatus().getStatusType();
			for(OtherNameDomain otherName:fromStatustype.getNameList()){
				if(EIMConfig.get("IOD_SIGNATURE_STATUS_TYPE_LANG").equals("JA")){
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