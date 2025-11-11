/**
 *
 */
package addon;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import jakarta.servlet.http.HttpServletRequest;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import addon.util.CreateStampImage;
import common.util.AppConstant;
import common.util.AppObjectConditionHelper;
import common.util.AppObjectUtil;
import common.util.AppUpdateNoticeUtils;
import common.util.MailUtil;
import common.util.OptionConfData;
import common.util.PublishAddonUtils;
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
import eim.util.EIMUtils;
import eim.util.FTPUtils;
import eim.util.FileUtils;
import eim.util.ObjectAttributeUtils;
import eim.util.ObjectUtils;
import eim.util.StringUtils;
import eim.util.UserUtils;
import eim.util.WorkFlowUtils;
import jp.co.ctc_g.eim.app.document.business.domain.ApprovalReqInfoDomain;
import jp.co.ctc_g.eim.app.document.business.domain.PDFSettingDomain;
import jp.co.ctc_g.eim.app.document.common.util.AppWorkFlowUtil;
import jp.co.ctc_g.eim.framework.business.domain.EventHistoryDomain;
import jp.co.ctc_g.eim.framework.business.domain.EventLogDomain;
import jp.co.ctc_g.eim.framework.business.domain.GuardConditionExecDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.EventDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.EventTypeDomain;
import jp.co.ctc_g.eim.framework.business.service.EventHistoryService;
import jp.co.ctc_g.eim.framework.common.util.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import yss.pdfmakeup.pdfmakeup;
import yss.pdfmakeup.pmudst;
import yss.pdfmakeup.pmuobjimage;
import yss.pdfmakeup.pmuobjtext;

/**
 * PDF 署名処理アドオン
 */
public class PublishCommandAddOnPDFSignature implements PublishCommandAddOn {
	private final String ID = "ctc_signPublishFile";
	private final String SWF_MODULE_NAME = "CTCAddOnPDFSignatureSetting";
	private final int On = 1;
	private final int Off = 0;

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
	//取得用
	private static List userList = new ArrayList();
	private static List commentList = new ArrayList();
	private static List dateList = new ArrayList();

	//DateFormat
	private static final SimpleDateFormat dfm = new SimpleDateFormat("yyyy/MM/dd");
	private static final SimpleDateFormat dfmt = new SimpleDateFormat("HH:mm:ss");


	/**
	 * PDF署名処理の設定情報を取得するメソッド
	 *
	 * @param sess セッション情報
	 * @param wfpubObj ワークフロー公開処理オブジェクト
	 *
	 * @return PDF署名処理の設定情報を示すXML
	 */
	public String getPublishCommandSetting(EIMSession sess, EIMObject wfpubObj)throws EIMException{

		if(!OptionConfData.getInstance().PDFSetPublicFileFlg
				&& !OptionConfData.getInstance().digitalSignFlg){
			return null;
		}
		String outString = "<macroSetting";
		outString += " id=\"" + StringUtils.xmlEncode(this.ID) + "\"";
		outString += " swf=\"" + StringUtils.xmlEncode(this.SWF_MODULE_NAME) + "\"";
		outString += PublishAddonUtils.getSignAndSetSecurityConfig(sess, wfpubObj);
		outString += " />";
		return outString;
	}

	/**
	 * PDF署名処理の設定情報を取得するメソッド
	 *
	 * @param sess セッション情報
	 * @param wfpubObj ワークフロー公開処理オブジェクト
	 *
	 * @return Map形式 の公開処理情報<br>
	 */
	public Map<String, Map<String, String>> getPublishCommandSettingList(EIMSession sess, EIMObject wfpubObj) throws Exception {
		Map<String, java.util.Map<String, String>> map = new HashMap<String, java.util.Map<String, String>>();

		if(!OptionConfData.getInstance().PDFSetPublicFileFlg
				&& !OptionConfData.getInstance().digitalSignFlg){
			return null;
		}

		// Mapに設定
		Map<String, String> paramMap = new HashMap<String, String>();

		paramMap.put("swf", StringUtils.xmlEncode(this.SWF_MODULE_NAME));
		paramMap.putAll(PublishAddonUtils.getSignAndSetSecurityConfigList(sess, wfpubObj));

		map.put(StringUtils.xmlEncode(this.ID), paramMap);

		return map;
	}

	/**
	 * PDF署名処理の設定更新メソッド
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
		// Parameters
		String prmDoSignAndSetSecurity = EIMUtils.getParameter(request, "ctc_PDFSignatureSetting_doSignAndSetSecurity");
		String prmDoSignPDF = EIMUtils.getParameter(request, "ctc_PDFSignatureSetting_doSignPDF");
		String prmInsertApproveDate = EIMUtils.getParameter(request, "ctc_PDFSignatureSetting_insertApproveDate");
		String prmInsertApproveUser = EIMUtils.getParameter(request, "ctc_PDFSignatureSetting_insertApproveUser");
		String prmInsertPage = EIMUtils.getParameter(request, "ctc_PDFSignatureSetting_insertPage");
		String prmInsertPlace	 = EIMUtils.getParameter(request, "ctc_PDFSignatureSetting_insertPlace");
		String prmInsertPlaceX	 = EIMUtils.getParameter(request, "ctc_PDFSignatureSetting_insertPlaceX");
		String prmInsertPlaceY	 = EIMUtils.getParameter(request, "ctc_PDFSignatureSetting_insertPlaceY");
		String prmApproveNamelang	 = EIMUtils.getParameter(request, "ctc_PDFSignatureSetting_approveNamelang");
		String prmSignJobName	 = EIMUtils.getParameter(request, "ctc_PDFSignatureSetting_signJobName");
		String prmDoSetSecurity = EIMUtils.getParameter(request, "ctc_PDFSignatureSetting_doSetSecurity");
		String prmDoSetSecurityPassword = EIMUtils.getParameter(request, "ctc_PDFSignatureSetting_doSetSecurityPassword");
		String prmSecurityPassword = EIMUtils.getParameter(request, "ctc_PDFSignatureSetting_securityPassword");
		String prmDoSetReferencePassword = EIMUtils.getParameter(request, "ctc_PDFSignatureSetting_doSetReferencePassword");
		String prmReferencePassword = EIMUtils.getParameter(request, "ctc_PDFSignatureSetting_referencePassword");
		String prmForbidPrint = EIMUtils.getParameter(request, "ctc_PDFSignatureSetting_forbidPrint");
		String prmForbidEdit = EIMUtils.getParameter(request, "ctc_PDFSignatureSetting_forbidEdit");
		String prmForbidAnnotate = EIMUtils.getParameter(request, "ctc_PDFSignatureSetting_forbidAnnotate");
		String prmForbidReproduce = EIMUtils.getParameter(request, "ctc_PDFSignatureSetting_forbidReproduce");
		/* Mandatory */
		if(StringUtils.isBlank(prmDoSignAndSetSecurity)){
			throw new EIMException(sess, "EIM.ERROR.SYSTEMERROR");
		}
		if(StringUtils.isBlank(prmDoSignPDF)){
			throw new EIMException(sess, "EIM.ERROR.SYSTEMERROR");
		}
		if(StringUtils.isBlank(prmDoSetSecurity)){
			throw new EIMException(sess, "EIM.ERROR.SYSTEMERROR");
		}
		PublishAddonUtils.setDoSignAndSetSecurityConfig(sess, wfpubObj, prmDoSignAndSetSecurity);
		PublishAddonUtils.setSignAndSetSecurityConfig(sess, wfpubObj, prmDoSignPDF, prmInsertApproveDate, prmInsertApproveUser, prmInsertPage, prmInsertPlace, prmInsertPlaceX, prmInsertPlaceY, prmApproveNamelang, prmSignJobName, prmDoSetSecurity, prmDoSetSecurityPassword, prmSecurityPassword, prmDoSetReferencePassword, prmReferencePassword, prmForbidPrint, prmForbidEdit, prmForbidAnnotate, prmForbidReproduce);
		return;
	}
	/**
	 * PDF署名処理の実施可否を取得してラベルに設定するメソッド
	 *
	 * @param sess セッション情報
	 * @param wfpubObj ワークフロー公開処理オブジェクト
	 *
	 * @return PDF署名処理の実施可否を示すラベルのXML
	 */
	public String getPublishStatusLabel(EIMSession sess, EIMObject wfpubObj){

		boolean SignatureFlag = false;
		String outString = "";
		String SignatureInfo = "";
		EIMAttribute attFlag = null;

		// 属性「PDF署名実施フラグ」の取得
		if( PublishAddonUtils.getDoSignAndSetSecurityConfig( sess, wfpubObj ) == 1 ) {
			SignatureFlag = true;
		}

		if(SignatureFlag){
			SignatureInfo = EIMResource.getMessage(sess, "EIM.CTC.CREATE.PUBLISH.DO.SIGNANDSEC");
		}
		else{
			SignatureInfo = EIMResource.getMessage(sess, "EIM.CTC.CREATE.PUBLISH.NONE.SIGNANDSEC");
		}

		// 出力文字列の生成
		outString = "<setting";
		outString += " label=\"" + StringUtils.xmlEncode(SignatureInfo) + "\"";
		outString += " />";

		return outString;
	}
	/**
	 * 署名処理が非同期終了かどうかを返却するメソッド<br>
	 * a) TRUE（非同期実行）
	 * ・「PDF署名」オブジェクト＝有
	 *      AND
	 *   ・対象文書の原本がPDF変換対象の拡張子 or PDF
	 * b) FALSE（同期実行）
	 *   ・上記以外
	 * @param sess セッション情報
	 * @param object 処理対象オブジェクト
	 * @return 非同期終了ならtrueを返却
	 *
	 * @throws Exception DBアクセス時にエラーが発生した場合
	 */
	public boolean isNeedAsyncProcess(EIMSession sess, EIMObject object) throws Exception
	{
		return isNeedAsyncProcess(sess, object, null);
	}

	/**
	 * 署名処理が非同期終了かどうかを返却するメソッド<br>
	 * a) TRUE（非同期実行）
	 * ・「PDF署名」オブジェクト＝有
	 *      AND
	 *   ・対象文書の原本がPDF変換対象の拡張子 or PDF
	 * b) FALSE（同期実行）
	 *   ・上記以外
	 * @param sess セッション情報
	 * @param object 処理対象オブジェクト
	 * @return 非同期終了ならtrueを返却
	 *
	 * @throws Exception DBアクセス時にエラーが発生した場合
	 */
	public boolean isNeedAsyncProcess(EIMSession sess, EIMObject object,
				GuardConditionExecDomain guardConditionExecDomain) throws Exception
	{
		boolean ret = false;

		if( guardConditionExecDomain == null )
		{
			ret = innerAsyncProcessNoDomain(sess, object);
		}
		else
		{
			ret = innerAsyncProcessDomain(sess, object, guardConditionExecDomain);
		}

		return ret;
	}

	/**
	 * PDF署名処理の非同期処理実行メソッド
	 * キュー登録（「PDF署名」オブジェクトの「ステータス」属性＝1（処理中）に設定）
	 * @param sess セッション情報
	 * @param object 処理対象オブジェクト
	 * @throws Exception DBアクセス時にエラーが発生した場合
	 */
	public void doAsyncProcess(EIMSession sess, EIMObject object) throws Exception
	{
		EIMObject pdfSignObj = AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_SIGNPDF"), String.valueOf(object.getId()));
		if( pdfSignObj != null ) {
			AppObjectUtil.setAttr(sess, pdfSignObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_STATUS"), 1);
		}
	}

	/**
	 * PDF署名処理のバッチ用公開処理実行メソッド
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
	 * @param sess セッション情報
	 * @param prntObject PDF署名オブジェクト
	 * @return 正常終了:true、異常終了:false
	 * @throws Exception DBアクセス時にエラーが発生した場合
	 */
	public boolean doPublishCommandBatch(EIMSession sess, EIMObject prntObject) throws Exception
	{
		//Error Logging
		Log log = LogFactory.getLog(this.getClass().getName());

		//Object Type
		EIMObjectType objTypePDFDiv = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_DIVIDEPDF"));
		EIMObjectType objTypePDFConv = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_CONVPDF"));
		EIMObjectType objTypeInsertURL = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_INSERTURL"));


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

		//該当ドキュメントに「PDF変換」または「PDF分割」オブジェクトが存在する場合
		EIMObject pdfDivObj = ObjectUtils.getObjectByTypeAndName(sess, objTypePDFDiv, prntObject.getName());
		EIMObject pdfConvObj = ObjectUtils.getObjectByTypeAndName(sess, objTypePDFConv, prntObject.getName());
		EIMObject insertURLObj  = ObjectUtils.getObjectByTypeAndName(sess, objTypeInsertURL, prntObject.getName());
		if(pdfDivObj != null || pdfConvObj != null || insertURLObj != null)
		{
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
			userList.clear();
			commentList.clear();
			dateList.clear();

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

			//セキュリティ情報取得対象オブジェクト
			EIMObject secInfoObj = null;

			EIMAttribute doSign = prntObject.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_ONOFF"));
			EIMAttribute appName = prntObject.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_APPNAME"));
			//----------- 初期設定の場合 ------------//
			if(sigstat == 1)
			{
				if(doSign != null && appName != null)
				{
					EIMStatusType statusType = null;
					//対象ステータスタイプオブジェクトリスト
					List dostatTypeList  = new ArrayList();

					//「電子署名する」かつ「承認者名挿入」の場合
					if((doSign.getInt() == 1) && (appName.getInt() != 0))
					{
						//ドキュメントのオブジェクトタイプからワークフローオブジェクトを取得
						EIMWorkFlow workFlow = WorkFlowUtils.getWorkFlowByType(sess, docObject.getType());
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
							for(int k = eventList.size()-1; k>-1 ; k--){
								EventLogDomain eventLogdom = eventList.get(k);
								if(eventLogdom.getEvent().getEventType().getBaseEventType().getKey().equals("BaseEvtApprovalPlugIn")){
									latestApprovalEventLogDom = eventLogdom;
									break;
								}
							}
							if(latestApprovalEventLogDom == null){
								//エラーメッセージ「承認イベントがありません」
							}
							//承認者
							userList.add(latestApprovalEventLogDom.getEvent().getCUser().createEIMUser());
							//承認日
							dateList.add(latestApprovalEventLogDom.getEvent().createEIMEvent().getCDate());
							//コメント取得
							if(latestApprovalEventLogDom.getEvent().getAttribute(EIMConfig.get("ATTR_NAME_APPROVER_COMMENT")) == null){
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
								addAppInfoToList(index);
							}
						}
						//「2:全承認者を挿入」の場合
						else
						{
////////////////////////////修正部分2([承認者が全承認者]イベント履歴からベースイベントタイプが"承認"となるものを取得し、そこから承認者、承認日、コメントを取得する)
							// 印影出力対象のリストをイベント履歴から作成する。最新のイベントからリストに詰める。
							List<EventLogDomain> appEvList = getApproveInfoEventList(eventList);

							// 印影出力対象のリストを元に出力情報を取得
							for(int kkk=appEvList.size()-1, appEvCnt=0; kkk>=0; kkk--)
							{
								EventLogDomain eventLogDomain = appEvList.get(kkk);
								EventDomain eventDomain = eventLogDomain.getEvent();
								//承認者
								userList.add(eventDomain.getCUser().createEIMUser());
								//承認日
								dateList.add(eventDomain.createEIMEvent().getCDate());
								//コメント取得
								if(eventDomain.getAttribute(EIMConfig.get("ATTR_NAME_APPROVER_COMMENT")) == null){
									commentList.add("");
								}else{
									commentList.add(eventDomain.getAttribute(EIMConfig.get("ATTR_NAME_APPROVER_COMMENT")).getValues());
								}
								addAppInfoToList(appEvCnt);
								appEvCnt++;
							}

////////////////////////////修正部分2終わり

//							for(int j = 0; j < dostatTypeList.size(); j++)
//							{
//								statusType = (EIMStatusType)dostatTypeList.get(j);
//								//ドキュメントオブジェクトとステータスタイプからステータスオブジェクトを取得
//								EIMStatus wfstatus = WorkFlowUtils.getStatusByType(sess, docObject, statusType);
//
//								//承認済みユーザ一覧を取得
//								userList = AppObjectUtil.getApprovedList(sess, wfstatus, dateList, commentList);
//								for(int jj = 0; jj < userList.size(); jj++)
//								{
//									addAppInfoToList(jj);
//								}
//							}
						}
						//承認者名を埋め込んだ印影イメージファイル作成
						//--- イメージファイル作成 ---//
						//イメージファイル出力先へのパスを取得
						String imgoutPath = EIMConfig.get("WORK");
						String exFilename = "";
						for(int j = 0; j < nameList.size(); j++)
						{
							String datetime = "";
							File imageFileObj = null;
							while(true)
							{
								//ファイル名取得(拡張子除く)
								datetime = formatDate(new Date());
						        if(j == 0 || exFilename.equals(datetime) == false)
						        {
						        	exFilename = datetime;
						        	break;
						        }
							}

						    imageFileObj = makeStampImage((String)nameList.get(j), imgoutPath, datetime);
							if(imageFileObj == null)
							{
								doErrorAct(sess, object, prntObject, null, log,
										EIMResource.getMessage("EIM.ERROR.LOGIC.PDFSIG.SIGNIMAGE.CREATEFAIL"), 0);
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
				if((doSign.getInt() == 1) && (appName.getInt() != 0))
				{
					for(int j = 0; j < nameList.size(); j++)
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
	 * PDF署名処理のオンライン用公開処理実行メソッド
	 *
	 * @param sess セッション情報
	 * @param object 処理対象オブジェクト
	 *
	 * @throws Exception DBアクセス時にエラーが発生した場合
	 */
	public void doPublishCommandOnline(EIMSession sess, EIMObject object) throws Exception
	{
		return;
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
	 *
	 * @param name 承認者名
	 * @param outpath 作成イメージファイル出力先ディレクトリ
	 * @param datetime 作成ファイル名(拡張子除く)
	 * @return imageFileObj 作成した印影イメージファイル
	 * @throws Exception 例外発生
	 */
	private File makeStampImage(String name, String outpath, String datetime)
	throws Exception
	{
		/** イメージの署名 **/
		//印影イメージの作成
		int imageSize = Integer.parseInt(EIMConfig.get("PDF_SIG_STAMP_IMAGE_SIZE"));
		if(imageSize < MIN_IMAGE_SIZE)
		{
			imageSize = MIN_IMAGE_SIZE;
		}
		CreateStampImage imageCreator = new CreateStampImage();
		File imageFileObj = imageCreator.create(name, EIMConfig.get("PDF_SIG_IMAGE_FONT"),
													imageSize - imageSize / 16, outpath, datetime);
		return imageFileObj;
	}

	/**
     * 日付オブジェクトに設定されている時間を"yyyyMMddHHmmssSSS"形式の文字列に
     * 変換します。
     * @param date 日付オブジェクトを指定します。
     * @return "yyyyMMddHHmmssSSS"形式の文字列を返します。
     */
    private String formatDate(Date date) {
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

		int imageSize = Integer.parseInt(EIMConfig.get("PDF_SIG_STAMP_IMAGE_SIZE"));
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

		//調整後のサイズが規定(80×125)以下になる場合は規定値を設定
		if(((widthRate * imageSize) < MIN_IMAGE_SIZE)
				|| (((imageSize + imageSize * IMG_POS_RATE) * heightRate) < MIN_HEIGHT_SIZE))
		{
			widthRate = 1.0;
			heightRate = 1.0;
			imageSize = MIN_IMAGE_SIZE;
		}

		for(int j = 0; j < nameList.size(); j++)
		{
			pmuobjimage objimg = dst.createobjimage();
			//挿入位置
			int val = 0;
			int basepos = 0; //とりあえずdefaultは「右上」に設定
			if(stampPosBase != null)
			{
				// switch使用のためintにキャスト
				basepos = (int)stampPosBase.getInt();
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
					posx = -(stampPosX.getInt()) * Point - (nameList.size()-1-j) * imageSize * widthRate;
				}
				else
				{
					posx = -(nameList.size()-1-j) * imageSize * widthRate;
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
			int pagetype = 0; //とりあえずdefaultは「全ページ」に設定
			if(insPage != null)
			{
				// switch使用のためintにキャスト
				pagetype = (int)insPage.getInt();
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

			//サイズ調整
			int wh = pmuobjimage.IMGWH_WH;
			if(objimg.setsize(wh, imageSize * widthRate, imageSize * heightRate) < 0)
			{
				return pobj.geterrorno();
			}
			int imgtype = pmuobjimage.IMGTYPE_PNG;
			if(objimg.setfilename(imgtype, ((File)(imgFileList.get(j))).getAbsolutePath()) < 0)
			{
				return pobj.geterrorno();
			}

			/** 下部の日付文字列 **/
			 //テキスト追記を作成
			pmuobjtext text = dst.createobjtext();
			// 挿入位置
			if(text.setbasepos(val) < 0)
			{
				return pobj.geterrorno();
			}
			// デフォルト位置指定 (YSS殿の指摘により追加)
			if( text.setpos(0, 0) < 0 )
			{
				return pobj.geterrorno();
			}
			/* 位置調整(移動) */
			double posx_string = posx;
			double posy_string = posy;
			if(val == pmuobjtext.POS_RT || val == pmuobjtext.POS_LT)
			{
				posy_string = posy + imageSize * heightRate + (imageSize * heightRate) / 16;
			}
			else
			{
				posy_string = posy + imageSize * heightRate * IMG_POS_RATE;
			}

			if(val == pmuobjtext.POS_LT || val == pmuobjtext.POS_LB)
			{
				posx_string = posx + imageSize * widthRate / 8;
			}

			if(text.movepos(posx_string, posy_string) < 0)
			{
				return pobj.geterrorno();
			}
			// レイヤ：背景
			if(text.setlayer(pmuobjtext.LAYER_FRONT) < 0)
			{
				return pobj.geterrorno();
			}
			/* 背景有無と色 */
			if(text.setbrushtype(pmuobjtext.BRUSHTYPE_NULL) < 0)
			{
				return pobj.geterrorno();
			}
			// 挿入ページ：全ページ
			switch(pagetype)
			{
				case 0: if(text.settargetpage(pmuobjtext.PAGETYPE_ALL, 0, 0) < 0)
						{
							return pobj.geterrorno();
						}
						break;
				case 1: if(text.settargetpage(pmuobjtext.PAGETYPE_PAGE, 1, 0) < 0)
						{
							return pobj.geterrorno();
						}
						break;
				default:if(text.settargetpage(pmuobjtext.PAGETYPE_ALL, 0, 0) < 0)
						{
							return pobj.geterrorno();
						}
						break;
			}

			/* 文字列 */
			if(text.setstring(String.valueOf((((Long)idList.get(j)).longValue())) + "\n"
					+ sdateList.get(j) + "\n" + timeList.get(j)) < 0)
			{
				return pobj.geterrorno();
			}
			/* フォント */
			if(text.setfont(EIMConfig.get("PDF_SIG_IMAGE_FONT")) < 0)
			{
				return pobj.geterrorno();
			}
			/* フォントサイズ（全角5文字は表示できるように） */
			double fontsize = imageSize * widthRate * 0.9 / 5;
			if(fontsize < 8)
			{
				fontsize = 8;
			}
			if(text.setfontsize(fontsize) < 0)
			{
				return pobj.geterrorno();
			}
			/* 文字の色(赤) */
			if(text.setfontcolor(255, 0, 0) < 0)
			{
				return pobj.geterrorno();
			}
			/* 枠線の有無と色 */
			if(text.setpentype(pmuobjtext.PENTYPE_NULL) < 0)
			{
				return pobj.geterrorno();
			}
			if(text.setbordersize((imageSize - imageSize / 16) * widthRate, imageSize / 2 * heightRate) < 0)
			{
				return pobj.geterrorno();
			}
			/* 強調文字 */
			if(text.setfontbold(true) < 0)
			{
				return pobj.geterrorno();
			}
			/* 斜体文字 */
			if(text.setfontitalic(false) < 0)
			{
				return pobj.geterrorno();
			}
			// 調節方法
			if(text.setbordertype(pmuobjtext.BORDER_NONAUTONEWLINE) < 0)
			{
				return pobj.geterrorno();
			}
			// ペンの幅 (YSS殿の指摘により追加)
			if(text.setpenwidth(1.0 * widthRate * heightRate) < 0)
			{
				return pobj.geterrorno();
			}

			//テキスト（枠線だけ）追記を作成
			// ※ 枠線はひとまず非表示にする
/*
			pmuobjtext text2 = dst.createobjtext();
			// 挿入位置
			if(text2.setbasepos(val) < 0)
			{
				return pobj.geterrorno();
			}
			// デフォルト位置指定 (YSS殿の指摘により追加)
			if( text2.setpos(0, 0) < 0 )
			{
				return pobj.geterrorno();
			}
			// 位置調整(移動)
			double posx_frame = 0;
			double posy_frame = 0;
			if(val == pmuobjtext.POS_LB || val == pmuobjtext.POS_LT)
			{
				posx_frame = posx + imageSize / 40 * widthRate;
			}
			else
			{
				posx_frame = posx - imageSize / 40 * widthRate;
			}

			if(val == pmuobjtext.POS_RT || val == pmuobjtext.POS_LT)
			{
				posy_frame = posy + imageSize / 40 * heightRate;
			}
			else
			{
				posy_frame = posy + imageSize * IMG_POS_RATE * heightRate - imageSize / 40 * heightRate;
			}

			if(text2.movepos(posx_frame, posy_frame) < 0)
			{
					return pobj.geterrorno();
			}
			// レイヤ：背景
			if(text2.setlayer(pmuobjtext.LAYER_FRONT) < 0)
			{
				return pobj.geterrorno();
			}
			// 挿入ページ：全ページ
			switch(pagetype)
			{
				case 0: if(text2.settargetpage(pmuobjtext.PAGETYPE_ALL, 0, 0) < 0)
				{
					return pobj.geterrorno();
				}
				break;
				case 1: if(text2.settargetpage(pmuobjtext.PAGETYPE_PAGE, 1, 0) < 0)
				{
					return pobj.geterrorno();
				}
				break;
				default:if(text2.settargetpage(pmuobjtext.PAGETYPE_ALL, 0, 0) < 0)
				{
					return pobj.geterrorno();
				}
				break;
			}
			// 枠線の有無と色
			if(text2.setpentype(pmuobjtext.PENTYPE_SOLID) < 0)
			{
				return pobj.geterrorno();
			}
			if(text2.setpencolor(255, 0, 0) < 0)
			{
				return pobj.geterrorno();
			}
			// ペンの幅
			if(text2.setpenwidth(1.0 * widthRate * heightRate) < 0)
			{
				return pobj.geterrorno();
			}
			// 背景有無と色
			if(text2.setbrushtype(pmuobjtext.BRUSHTYPE_NULL) < 0)
			{
				return pobj.geterrorno();
			}
			if(text2.setbordersize(imageSize * widthRate, (imageSize + imageSize * IMG_POS_RATE) * heightRate) < 0)
			{
				return pobj.geterrorno();
			}
			// 文字列 (YSS殿の指摘により追加)
			if(text2.setstring("") < 0)
			{
				return pobj.geterrorno();
			}
			// フォント (YSS殿の指摘により追加)
			if(text2.setfont(EIMConfig.get("PDF_SIG_IMAGE_FONT")) < 0)
			{
				return pobj.geterrorno();
			}
			// フォントサイズ (YSS殿の指摘により追加)
			// ※ 日付文字列と同じ値を設定
			if(text2.setfontsize(fontsize) < 0)
			{
				return pobj.geterrorno();
			}
			// 文字の色(赤) (YSS殿の指摘により追加)
			if(text2.setfontcolor(255, 0, 0) < 0)
			{
				return pobj.geterrorno();
			}
			// 強調文字 (YSS殿の指摘により追加)
			if(text2.setfontbold(true) < 0)
			{
				return pobj.geterrorno();
			}
			// 斜体文字 (YSS殿の指摘により追加)
			if(text2.setfontitalic(false) < 0)
			{
				return pobj.geterrorno();
			}
			// 調節方法 (YSS殿の指摘により追加)
			if(text2.setbordertype(pmuobjtext.BORDER_NONAUTONEWLINE) < 0)
			{
				return pobj.geterrorno();
			}
*/
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
	private void addAppInfoToList(int index)throws Exception
	{
		EIMUser appUser = (EIMUser)userList.get(index);
		if(appUser != null)
		{
			//「承認者名」
			nameList.add(appUser.getName());
			//「承認者ID」
			idList.add(new Long(appUser.getId()));
			//「承認日」
			sdateList.add(dfm.format((Date)dateList.get(index)));
			//「承認時刻」
			timeList.add(dfmt.format((Date)dateList.get(index)));
		}
	}

	/**
	 * 署名処理が非同期終了かどうかを返却するメソッド(ドメイン無し)
	 */
	private boolean innerAsyncProcessNoDomain(EIMSession sess, EIMObject object)throws Exception
	{
		boolean ret = false;
		// PDF 署名オブジェクトが存在
		EIMObject pdfSignObj = AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_SIGNPDF"), String.valueOf(object.getId()));
		if( pdfSignObj != null ) {

			//設定ファイルからPDF可能ファイル拡張子を取得
			String convert_file_type = EIMConfig.get("PDF_CONVERT_FILE_TYPE");
			convert_file_type = convert_file_type + ",pdf";
			String[] convFileTypeArray = convert_file_type.split(",");

			EIMFormat format = FileUtils.getDefaultFormat(sess, object.getType());
			EIMFile file = null;

			boolean isFolder = false;

			if (format == null) {
				//フォルダ
				isFolder = true;
			} else {
				// 拡張子のチェック
				file = FileUtils.getFile(sess, object, format);
				if (file == null) {
					//フォルダ
					isFolder = true;
				} else {
					//PDF変換対象か判定
					for (int i = 0; i < convFileTypeArray.length; i++) {
						if(file.getExt().equalsIgnoreCase("."+convFileTypeArray[i])) {
							ret = true;
							break;
						}
					}
				}
			}
			if( isFolder == true ) {
				// フォルダの場合, 配下に変換対象ファイルがあれば true
				List chldObjectList = AppObjectUtil.getChildEIMObjectRecurrently(sess,
					object, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));
				AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

				for (Iterator i = chldObjectList.iterator(); i.hasNext();) {
					EIMObject childObj = (EIMObject) i.next();
					// フォルダの場合は読み飛ばす
					if(helper.isTypeOfFolder(childObj.getType())){
						continue;
					}

					EIMFormat childFormat = FileUtils.getDefaultFormat(sess, childObj.getType());
					EIMFile childFile = FileUtils.getFile(sess, childObj, childFormat);
					for (int j = 0; j < convFileTypeArray.length; j++) {
						if(childFile.getExt().equalsIgnoreCase("."+convFileTypeArray[j])) {
							ret = true;
							break;
						}
					}
				}
			}
		}
		return ret;
	}

	/**
	 * 署名処理が非同期終了かどうかを返却するメソッド(ドメイン有り)
	 * 現在は、承認不要の場合のみ呼び出されるメソッドです。
	 *
	 * 以下の条件でtrueを返します。
	 * 　ST移予測での判定
	 * 　　・原本がPDF
	 * 　　・WF設定に署名の設定がある、または、署名オブジェクトが存在(取戻しの場合存在)
	 * 　公開実行時の判定
	 * 　　・原本がPDF
	 * 　　・画面の署名設定に変更があり、
	 * 　　・画面の
	 */
	private boolean innerAsyncProcessDomain(EIMSession sess, EIMObject object,
					GuardConditionExecDomain guardConditionExecDomain)throws Exception
	{
		// ※注意　2010/09/30 記
		//	以下の条件を満たす場合はfalseを返しますが、全体の挙動として問題ありません
		//	Ver4.1出荷前のため修正を控えますが、余裕があったら修正お願いします。
		//		・PDF変換設定あり
		//		・PDF署名設定有り
		//		・原本がPDF変換対象

		//パラメータを取得
		Map<String, Object> paramMap = guardConditionExecDomain.getParamMap();
		String processType = (String)paramMap.get(AppConstant.PARAM_KEY_REFER_TO_APPROVAL);

		boolean isPDFFlag = AppWorkFlowUtil.isPDFFile(sess, object);

		// ST遷移予測で呼ばれた場合はWF設定または署名オブジェクトからPDF設定を取得する
		if(processType != null && !processType.equals(AppConstant.PARAM_VALUE_REFER_TO_APPROVAL))
		{
			if( isPDFFlag == true )
			{
				if( AppWorkFlowUtil.isPDFSignDocument(sess, object) == true )
				{
					return true;
				}
			}
		}
		// 実行時には、WF設定またはローカル設定を参照
		else
		{
			if( isPDFFlag == true )
			{
				ApprovalReqInfoDomain approvalReqInfoDomain = new ApprovalReqInfoDomain(guardConditionExecDomain);
				PDFSettingDomain pdfSettingDomain = approvalReqInfoDomain.getPdfDomain();

				// ローカル設定の変更無しの場合はWF設定を、変更有りの場合はローカル設定を参照
				if( pdfSettingDomain.isLocalPDFOutputSet() == true )
				{
					if( pdfSettingDomain.isDoSignPDF() == true )
					{
						return true;
					}
				}
				else
				{
					if( AppWorkFlowUtil.isPDFSignDocument(sess, object) == true )
					{
						return true;
					}
				}

			}
		}
		return false;
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
						appEvList.add(eventLogDomain);
						eventMap.put((long)eventTypeDomain.getId(), true);
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
					eventTypeDomain.getBaseEventType().getKey().equals("BaseEvtTakeBackRequestPlugIn") )
			{
				// 差戻し、取戻しの場合、その後はチェックしなくて良い。
				break;
			}
		}
		return appEvList;
	}
}