/**
 *
 */
package addon;


import java.awt.Color;
import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import jakarta.servlet.http.HttpServletRequest;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.fontbox.ttf.TrueTypeCollection;
import org.apache.pdfbox.Loader;
import org.apache.pdfbox.io.RandomAccessReadBufferedFile;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;
import org.apache.pdfbox.pdmodel.PDPageContentStream;
import org.apache.pdfbox.pdmodel.PDPageTree;
import org.apache.pdfbox.pdmodel.font.PDFont;
import org.apache.pdfbox.pdmodel.font.PDType0Font;
import org.apache.pdfbox.pdmodel.graphics.state.PDExtendedGraphicsState;
import org.apache.pdfbox.util.Matrix;

import common.enumeration.RotationEnum;
import common.util.AppObjectConditionHelper;
import common.util.AppObjectUtil;
import common.util.AppUpdateNoticeUtils;
import common.util.OptionConfData;
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
import jp.co.ctc_g.eim.framework.business.domain.GuardConditionExecDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.service.ObjectService;
import jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader;

/**
 * PDF URL挿入アドオン
 */
public class PublishCommandAddOnPDFInsertURL implements PublishCommandAddOn {
	private final String ID = "ctc_PDFInsetURLPublishFile";
	private final String SWF_MODULE_NAME = "CTCAddOnPDFInsetURL";
	private final int doInserURLOn = 1;
	private final int Off = 0;

	public String url = null;
	public String no = null;

	Log log = LogFactory.getLog(PublishCommandAddOnPDFInsertURL.class);

	/**
	 * URL挿入処理の設定情報を取得するメソッド
	 *
	 * @param sess セッション情報
	 * @param wfpubObj ワークフロー公開処理オブジェクト
	 *
	 * @return URL挿入の設定情報を示すXML
	 */
	public String getPublishCommandSetting(EIMSession sess, EIMObject wfpubObj)throws EIMException{

		String outString = "";
		String insertURLFlagName = "false";
		EIMAttribute attFlag = null;

		// 属性「URL挿入実施フラグ」の取得
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_INSERT_URL_FLG"));
		}

		if(attFlag != null){
			long flagVal = attFlag.getInts()[0];
			// 属性が存在し、かつ値がONの場合のみtrue
			if(flagVal == doInserURLOn){
				insertURLFlagName = "true";
			}
		}
		//オプション処理
		if(!OptionConfData.getInstance().insertURLFlag){
			return null;
		}

		// 出力文字列の生成
		outString = "<macroSetting";
		outString += " id=\"" + StringUtils.xmlEncode(this.ID) + "\"";
		outString += " swf=\"" + StringUtils.xmlEncode(this.SWF_MODULE_NAME) + "\"";
		outString += " doinsertURLPDF=\"" + StringUtils.xmlEncode(insertURLFlagName) + "\"";
		outString += " />";

		return outString;

	}

	/**
	 * URL挿入処理の設定情報を取得するメソッド
	 *
	 * @param sess セッション情報
	 * @param wfpubObj ワークフロー公開処理オブジェクト
	 *
	 * @return Map形式 の公開処理情報<br>
	 */
	public Map<String, Map<String, String>> getPublishCommandSettingList(EIMSession sess, EIMObject wfpubObj) throws Exception {
		Map<String, java.util.Map<String, String>> map = new HashMap<String, java.util.Map<String, String>>();

		String insertURLFlagName = "false";
		EIMAttribute attFlag = null;

		// 属性「URL挿入実施フラグ」の取得
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_INSERT_URL_FLG"));
		}

		if(attFlag != null){
			long flagVal = attFlag.getInts()[0];
			// 属性が存在し、かつ値がONの場合のみtrue
			if(flagVal == doInserURLOn){
				insertURLFlagName = "true";
			}
		}

		//オプション処理
		if(!OptionConfData.getInstance().insertURLFlag){
			return null;
		}

		// Mapに設定
		Map<String, String> paramMap = new HashMap<String, String>();

		paramMap.put("swf", StringUtils.xmlEncode(this.SWF_MODULE_NAME));
		paramMap.put("doinsertURLPDF", StringUtils.xmlEncode(insertURLFlagName));
		map.put(StringUtils.xmlEncode(this.ID), paramMap);

		return map;
	}

	/**
	 * URL挿入の設定更新メソッド
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
		String prmDoInsertURL = EIMUtils.getParameter(request, "ctc_PDFInsertSetting_doInsertURL");

		long[] doInsert = {Off};

		// パラメータチェック
		if(StringUtils.isBlank(prmDoInsertURL)){
			throw new EIMException(sess, "EIM.ERROR.SYSTEMERROR");
		}

		// PDF変換を行う場合
		if(prmDoInsertURL.equals("true")){
			doInsert[0] = doInserURLOn;
		}

		// 属性「PDF変換実施フラグ」の設定
		AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_INSERT_URL_FLG"), doInsert);

		return;
	}
	/**
	 * URL挿入の実施可否を取得してラベルに設定するメソッド
	 *
	 * @param sess セッション情報
	 * @param wfpubObj ワークフロー公開処理オブジェクト
	 *
	 * @return URL挿入処理の実施可否を示すラベルのXML
	 */
	public String getPublishStatusLabel(EIMSession sess, EIMObject wfpubObj){

		boolean insertFlag = false;
		String outString = "";
		String insertInfo = "";
		EIMAttribute attFlag = null;

		// 属性「PDF分割実施フラグ」の取得
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_INSERT_URL_FLG"));
		}

		if(attFlag != null){
			long flagVal = attFlag.getInts()[0];
			// 属性が存在し、かつ値がONの場合のみtrue
			if(flagVal == doInserURLOn){
				insertFlag = true;
			}
		}

		if(insertFlag){
			insertInfo = EIMResource.getMessage(sess, "EIM.CTC.CREATE.PUBLISH.DO.DIVIDE");
		}
		else{
			insertInfo = EIMResource.getMessage(sess, "EIM.CTC.CREATE.PUBLISH.NONE.DIVIDE");
		}

		// 出力文字列の生成
		outString = "<setting";
		outString += " label=\"" + StringUtils.xmlEncode(insertInfo) + "\"";
		outString += " />";

		return outString;
	}
	/**
	 * URL挿入処理が非同期終了かどうかを返却するメソッド<br>
	 * a) TRUE（非同期実行）
	 * ・「URL挿入」オブジェクト＝有
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
	 * URL挿入処理が非同期終了かどうかを返却するメソッド<br>
	 * a) TRUE（非同期実行）
	 * ・「URL挿入」オブジェクト＝有
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
		// ワークフロー公開処理オブジェクトの取得
		EIMObject wfpubObj = AppObjectUtil.getWorkFlowProcessing(sess, object);
		if(wfpubObj != null){
			// 属性「ワークフロー公開処理URL挿入実施フラグ」の取得
			EIMAttribute attInsert = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_INSERT_URL_FLG"));
			// 設定されており, 1が設定されている(挿入する)
			if(attInsert != null && attInsert.getInts()[0] == doInserURLOn ){
				EIMFormat format = FileUtils.getDefaultFormat(sess, object.getType());
				if (format == null) {
					//フォルダ
					ret = true;
				} else {
					// 拡張子のチェック
					EIMFile file = FileUtils.getFile(sess, object, format);
					if (file == null) {
						//フォルダ
						ret = true;
					} else {
						//設定ファイルからPDF可能ファイル拡張子を取得
						String convert_file_type = EIMConfig.get("PDF_CONVERT_FILE_TYPE");
						convert_file_type = convert_file_type + ",pdf";
						String[] convFileTypeArray = convert_file_type.split(",");
						//PDF変換対象か判定
						for (int i = 0; i < convFileTypeArray.length; i++) {
							if(file.getExt().equalsIgnoreCase("."+convFileTypeArray[i])) {
								ret = true;
							}
						}
					}
				}
			}
		}
		return ret;
	}

	/**
	 * URL挿入処理の非同期処理実行メソッド
	 * @param sess セッション情報
	 * @param object 処理対象オブジェクト
	 * @throws Exception DBアクセス時にエラーが発生した場合
	 */
	public void doAsyncProcess(EIMSession sess, EIMObject object) throws Exception
	{
		// URL挿入対象のファイルの場合、URL挿入オブジェクトを作成
		// URL挿入オブジェクトの存在チェック
		EIMObjectType objTypePDFURL = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_INSERTURL"));
		EIMObject insertURLObj = ObjectUtils.getObjectByTypeAndName(sess, objTypePDFURL, String.valueOf(object.getId()));
		if (insertURLObj == null) {
			EIMObjectType objTypeInsertURL = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_INSERTURL"));
			ObjectUtils.createObject(sess, objTypeInsertURL, String.valueOf(object.getId()));
		}
	}

	/**
	 * URL挿入のバッチ用公開処理実行メソッド
	 * 1) 公開 Format から FTP GET
	 * 2) URL挿入オブジェクトから情報取得
	 * 3) URL挿入 実行
	 * 4) FTP PUT
	 * 5) PDFファイル削除
	 * @param sess セッション情報
	 * @param prntObject URL挿入オブジェクト
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
			return false;
		}

		List chldObject = AppObjectUtil.getChildEIMObjectRecurrently(sess,
				docObject, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));
		chldObject.add(docObject);

		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

		for (int i = 0; i < chldObject.size(); i++)
		{
			EIMObject object = (EIMObject)chldObject.get(i);

			//Object Type
			EIMObjectType objType = ObjectUtils.getObjectTypeById(sess, object.getType().getId());

			// 公開失敗の時
			if(!helper.isTypeOfFolder(objType) && AppObjectUtil.getIntAttr(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_PUB_PROC_FAIL"), -1) != 0)
			{
				log.error("既に公開失敗フラグが1のため処理を中断します。");
				return false;
			}

			if (helper.isTypeOfFolder(objType))
			{
				// フォルダは挿入対象外
				continue;
			}

			//Pubilc Format(エラー処理のためここで取得)
			EIMFormat publicFormat  = FileUtils.getFormatByName(sess, EIMConfig.get("FORMAT_NAME_PUBLIC"));
			if(publicFormat == null)
			{
				doErrorAct(sess, object, prntObject, null, log,
						EIMResource.getMessage("EIM.ERROR.LOGIC.NOPUBLICFORM.WITHDOCNAME", new Object[]{prntObject.getName()}), 0);

				updateNoticePDFInsertURL(sess, docObject);
				return false;
			}

			//挿入対象ファイルをFTP転送でDownload
			EIMFile file = FileUtils.getFile(sess, object, publicFormat);
			if(file == null)
			{
				doErrorAct(sess, object, prntObject, object, log,
						EIMResource.getMessage("EIM.ERROR.LOGIC.NOPUBLICFILE.WITHDOCNAME", new Object[]{object.getName()}), 0);
				updateNoticePDFInsertURL(sess, docObject);
				return false;
			}
			//pdfかどうかチェック pdf以外なら次へ
			if(file.getExt() == null || !file.getExt().equals(EIMConfig.get("PDF_EXT")))
			{
				continue;
			}

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
				log.warn(EIMResource.getMessage("EIM.LOG.PDF.URL.FAILED") + EIMResource.getMessage("EIM.LOG.PDF.URL.ORIGINALFILE")
						+ StringUtils.getFileBody(object.getName()) + "(" + object.getId() + ")"
						+ EIMResource.getMessage("EIM.LOG.PDF.PREFIX.ERROR")
						+ EIMResource.getMessage("EIM.ERROR.LOGIC.FTPDOWNLOAD")
						+ EIMResource.getMessage("EIM.LOG.PDF.PREFIX.ERRORCODE") + "-");
				throw e;
			}

			//--- URLとリビジョン番号 ---//

			url =  StringUtils.xmlEncode(EIMConfig.get("DOCUMENT_URL") +  EIMConfig.get("QUERY_STRING")) + "objId=" + object.getId();
			log.info("[URL挿入]" + url);

			long docId = object.getId();
			ObjectService objectService =(ObjectService)ApplicationContextLoader.getApplicationContext().getBean("objectService2");

			//Idからリビジョンを引っ張ってきて、ストリングに強制変換
			ObjectDomain doc = objectService.getById(docId);
			Integer oi = new Integer(doc.getRevision());
			no =oi.toString();

			// URLの書き込み実施（PDFBox利用）
			if(doPDFInsertURLByPDFBox(orgFile) != 0){
				doErrorAct (sess, object, object, object, log, getFilePath, 0);
				return false;
			}

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
				errMsg += EIMResource.getMessage("EIM.LOG.PDF.URL.ORIGINALFILE") + StringUtils.getFileBody(object.getName())
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
	 * URL挿入のオンライン用公開処理実行メソッド
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
	 * URL挿入失敗後処理
	 *
	 * @param sess セッション情報
	 * @param object 処理対象ドキュメントオブジェクト
	 * @param log ログ情報
	 * @param errMessage エラーメッセージ
	 * @param errCode エラーコード
	 *
	 * @throws Exception 例外発生
	 */
	private void doErrorAct (EIMSession sess, EIMObject object, EIMObject prntObj, EIMObject pdfsecObj,
			Log log, String errMsg, int errCode) throws Exception{

		//対象ドキュメントオブジェクトの「公開処理失敗」に(公開処理失敗)を設定
		EIMAttributeType pubFail = AttributeUtils.getAttributeTypeByName
									(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_PUB_PROC_FAIL"));
		ObjectAttributeUtils.setAttribute(sess, object, pubFail, 1);

		//Pubilc Format
		EIMFormat publicFormat  = FileUtils.getFormatByName(sess, EIMConfig.get("FORMAT_NAME_PUBLIC"));

		//ダウンロードしたPDFファイルを削除
		try
		{
			//公開フォーマット取得失敗時は以下の処理は行わない
			if(publicFormat != null)
			{
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
		}
		catch(Exception e)
		{
			throw e;
		}

		//ダウンロードしたPDFファイルを削除
		try
		{
			if(object != null)
			{
				delDownLoadPDF(sess, object);
			}
		}
		catch(Exception e)
		{
			throw e;
		}

		//エラーログ出力
		String errMessag = null;
		errMessag = (EIMResource.getMessage("EIM.LOG.PDF.URL.FAILED")
					+ EIMResource.getMessage("EIM.LOG.PDF.URL.ORIGINALFILE")
					+ StringUtils.getFileBody(object.getName()) + "(" + object.getId() +")")
					+ EIMResource.getMessage("EIM.LOG.PDF.PREFIX.ERROR")  + EIMResource.getMessage("EIM.ERROR.LOGIC.FTPUPLOAD")
					+ EIMResource.getMessage("EIM.LOG.PDF.PREFIX.ERRORCODE");
		if(errCode != 0)
		{
			errMessag += String.valueOf(errCode);
		}
		else
		{
			errMessag += "-";
		}
		log.warn(errMessag);
		return;
	}

	/**
	 * URL挿入処理成功時
	 *
	 * @param sess セッション情報
	 * @param object 処理対象ドキュメントオブジェクト
	 *
	 * @throws Exception 例外発生
	 */
	private void doSuccessAct (EIMSession sess, EIMObject object)throws Exception
	{
		//ダウンロードしたPDFファイルを削除
		try
		{
			delDownLoadPDF(sess, object);
		}
		catch(Exception e)
		{
			throw e;
		}
	}


	/**
	 * URL挿入処理(PDFBoxを利用)
	 *
	 * @param orgFile PDFファイル
	 *
	 * @return 処理結果(0,またはエラーコード)
	 * @throw Exception 例外発生
	 */
	private int doPDFInsertURLByPDFBox(File orgFile)throws Exception{

		//Error Logging
		Log log = LogFactory.getLog(this.getClass().getName());

		//設定ファイルからPDF出力に必要な値を取得
		String insertPosX = EIMConfig.get("INSERT_URL_DEFALT_POSE_X");
		String insertPosY = EIMConfig.get("INSERT_URL_DEFALT_POSE_Y");
		String size =  EIMConfig.get("INSERT_URL_FONT_SIZE");
		String insertPage = EIMConfig.get("INSERT_URL_PAGE");
		String minSize = EIMConfig.get("INSERT_URL_FONT_SIZE_MIN");
		String minWidth = EIMConfig.get("INSERT_URL_MIN_WIDTH");

		float lineX = 0;
		float lineY = 0;
		float fontSize = 11;
		int basepage = 0;
		float minFontSize = 5;
		float minFileWidth = 300;

		try {
			if (!insertPosX.isEmpty()) {
				lineX = Float.valueOf(insertPosX);
			}
			if (!insertPosY.isEmpty()) {
				lineY = Float.valueOf(insertPosY);
			}
			if (!size.isEmpty()) {
				fontSize = Float.valueOf(size);
			}
			if (!minSize.isEmpty()) {
				minFontSize = Float.valueOf(minSize);
			}
			if (!minWidth.isEmpty()) {
				minFileWidth = Float.valueOf(minWidth);
			}
			basepage = Integer.parseInt(insertPage);
		} catch (NumberFormatException e) {
			String errMsg = EIMResource.getMessage("EIM.ERROR.LOGIC.FAILED.PDF.URL.PDFBOX.FAILED") + EIMResource.getMessage("EIM.ERROR.LOGIC.FAILED.PDF.URL.PDFBOX.CONFIG.INVARID");
			log.error(errMsg);
			return -1;
		}

		// PDF文書
		PDDocument document = Loader.loadPDF(new RandomAccessReadBufferedFile(orgFile));
		// 暗号化されている場合
		if(document.isEncrypted()) {
			// セキュリティを全部削除
			document.setAllSecurityToBeRemoved(true);
		}

		/* 文字列 */
		String insertText = EIMResource.getMessage("EIM.LABEL.INSERTURL",new Object[]{url,no});
		/* 文字フォント */
		File file = new File(EIMConfig.get("INSERT_URL_FONT_FILE_PATH"));
        try (TrueTypeCollection collection = new TrueTypeCollection(file);){
        	PDFont font = PDType0Font.load(document, collection.getFontByName("Meiryo"), true);

			if(basepage == 0){
				// 0:全ページ
				PDPageTree pageTree = document.getPages();
				Iterator<PDPage> ite =pageTree.iterator();
				while (ite.hasNext()) {
					PDPage selectPage = ite.next();
					insertStringToPDF(document, selectPage, font, fontSize, lineX, lineY, insertText,minFileWidth,minFontSize);
				}
			}
			else if (basepage == 1){
				// 1:表紙のみ
				PDPage page = document.getPage(0);
				insertStringToPDF(document, page, font, fontSize, lineX, lineY, insertText,minFileWidth,minFontSize);
			}
			else {
				String errMsg = EIMResource.getMessage("EIM.ERROR.LOGIC.FAILED.PDF.URL.PDFBOX.FAILED") + EIMResource.getMessage("EIM.ERROR.LOGIC.FAILED.PDF.URL.PDFBOX.CONFIG.INVARID");
				log.error(errMsg);
				return -1;
			}

			document.save(orgFile);
        } catch (Exception e) {
			String errMsg = EIMResource.getMessage("EIM.ERROR.LOGIC.FAILED.PDF.URL.PDFBOX.FAILED");
			log.error(errMsg);
			log.error(e);
        	return -1;
		} finally {
			document.close();
		}
		return 0;

}

	/**
	 * 指定PDF文書に指定文字列を挿入
	 * @param document 挿入対象PDF文書
	 * @param page 挿入対象PDF文書ページ
	 * @param font 挿入文字フォント
	 * @param fontSize 挿入文字サイズ
	 * @param lineX 挿入文字X座標（ページ左下から）
	 * @param lineY 挿入文字Y座標（ページ左下から）
	 * @param insertText 挿入文字列
	 * @param minFileWidth 文字を可変とするファイル幅の閾値
	 * @param minFontSize 最小文字サイズ
	 * @throws IOException 例外
	 */
	private void insertStringToPDF(PDDocument document, PDPage page, PDFont font, float fontSize, float lineX, float lineY, String insertText, float minFileWidth, float minFontSize) throws IOException {

		int rotation = page.getRotation();
		PDPageContentStream contentStream = new PDPageContentStream(document, page, PDPageContentStream.AppendMode.APPEND, true);

		float width =  page.getMediaBox().getWidth();
		log.info("[URL挿入]rotation:" + rotation + "width:" + width);
		if ( rotation == RotationEnum.ROTATION_90.getValue() ) {
    		contentStream.transform(Matrix.getRotateInstance(Math.toRadians(rotation), page.getMediaBox().getWidth(), 0));
    		width =  page.getMediaBox().getHeight();
		} else if(rotation == RotationEnum.ROTATION_270.getValue()) {
			contentStream.transform(Matrix.getRotateInstance(Math.toRadians(rotation), 0, page.getMediaBox().getHeight()));
			width =  page.getMediaBox().getHeight();
		} else if(rotation == RotationEnum.ROTATION_180.getValue()) {
			contentStream.transform(Matrix.getRotateInstance(Math.toRadians(rotation),page.getMediaBox().getWidth(), page.getMediaBox().getHeight()));
			width =  page.getMediaBox().getWidth();
		}
		// 幅がminFileWidth以下の場合は文字サイズを最小文字サイズに変更する
		if (width < minFileWidth) {
			fontSize = minFontSize;
			log.info("[URL挿入]フォントサイズを変更しました。 width:" + width + " fontSize:" + fontSize);
		}

		//URL文字の透過率を1(無透過)に設定する
		PDExtendedGraphicsState state = new PDExtendedGraphicsState();
		state.setNonStrokingAlphaConstant(1.0f);
		contentStream.setGraphicsStateParameters(state);

    	contentStream.beginText();
    	contentStream.setFont(font, fontSize);
    	contentStream.setNonStrokingColor(Color.BLACK);
    	contentStream.newLineAtOffset(lineX, lineY);
    	contentStream.showText(insertText);
    	contentStream.endText();
    	contentStream.close();
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
	 * @param sess セッション情報
	 * @param object URL挿入対象オブジェクト
	 */
	static private void updateNoticePDFInsertURL(EIMSession sess, EIMObject object) throws Exception
	{
		// SearchFramework 検索FW更新通知 対象：WFドキュメント、WF付きフォルダと配下のフォルダおよびドキュメント(挿入失敗)
		AppUpdateNoticeUtils.updateNoticeInsertWFObject(sess, object,
				"SEARCHFW_INSERTURL_DOCUMENT" ,"SEARCHFW_INSERTURL_FOLDER" ,
				"SEARCHFW_INSERTURL_CHILD_DOCUMENT"	,"SEARCHFW_INSERTURL_CHILD_FOLDER");

	}



}