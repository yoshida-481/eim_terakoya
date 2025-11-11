package batch;

import java.io.File;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import javax.sql.DataSource;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.context.ApplicationContext;

import common.util.AppConstant;
import common.util.AppMessageUtils;
import common.util.AppObjectUtil;
import common.util.AppUpdateNoticeUtils;
import eim.bo.EIMAttribute;
import eim.bo.EIMAttributeType;
import eim.bo.EIMException;
import eim.bo.EIMFile;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMResource;
import eim.bo.EIMUser;
import eim.net.EIMSession;
import eim.util.AttributeUtils;
import eim.util.EIMConfig;
import eim.util.EIMConstant;
import eim.util.FileUtils;
import eim.util.ObjectAttributeUtils;
import eim.util.ObjectUtils;
import eim.util.SearchUtils;
import jp.co.ctc_g.eim.app.document.business.service.OcrProcessService;
import jp.co.ctc_g.eim.app.document.integration.dao.PDFBatchObjectDao;
import jp.co.ctc_g.eim.framework.common.util.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;
import jp.co.ctc_g.eim.framework2.integration.dao.plugin.DatabasePlugIn.SQLExceptionTypeEnum;
import jp.co.ctc_g.eim.framework2.integration.dao.plugin.DatabasePlugInLoader;

/**
*
* OCR処理実行バッチ
*
*/
public class OcrProcessMain {

	/** エラーキー:原本 */
	private static String ERROR_KEY_ORIGINAL = "原本";

	/** エラーキー:公開 */
	private static String ERROR_KEY_PUBLIC = "公開";

	/** 原本ファイル一時フォルダ */
	private static String ORIGINAL_TEMP_FOLDER = "original/";

	/** 公開ファイル一時フォルダ */
	private static String PUBLIC_TEMP_FOLDER = "public/";

	/** アプリケーションコンテキスト */
	private static ApplicationContext context;

	/** ログ */
	private static Log log = LogFactory.getLog(OcrProcessMain.class);

	/** ドキュメントObject */
	private static EIMObject documentObject = null;

	/** 原本ファイル */
	private static EIMFile originalfile = null;

	/** 公開ファイル */
	private static EIMFile publicfile = null;


	/**
	 * OCR処理実行バッチメイン処理
	 *
	 * @param args なし
	 */
	public static void main(String[] args) {
		try {
			// 開始ログ
			log.info(" " + EIMResource.getMessage("EIM.INFO.OCR.PROCESS.MAIN.START"));

			// 初期処理
			init();

			// OCR処理ループ
			while(true){

				// OCR処理
				ocrProcess();

				// 一定時間スリープ
				Thread.sleep(Integer.valueOf(ConfigUtils.getByKey("OCR_PROCESS_SLEEP_TIME")));
			}

		} catch(Exception e) {
			log.error(AppMessageUtils.makeLogMessage(1,e.getMessage()), e);

		} finally {
			// 終了ログ
			log.info(" " + EIMResource.getMessage("EIM.INFO.OCR.PROCESS.MAIN.END"));

		}

	}

	/**
	 * 初期処理
	 *
	 * @throws Exception 例外
	 */
	private static void init() throws Exception{

		// コンテキスト読込
		context = ApplicationContextLoader.getContext();

	}

	/**
	 * OCR処理
	 * @throws Exception 例外
	 */
	private static void ocrProcess() throws Exception{

		EIMSession sess = null;

		try{

			// セッション取得
			sess = getSession();

			//排他ロックに関するObjectDao
			PDFBatchObjectDao batchObjectDao = (PDFBatchObjectDao) context.getBean("objectDaoForPDFBatch");

			EIMObjectType objTypePDFDiv = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_DIVIDEPDF"));
			EIMObjectType objTypePDFSig = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_SIGNPDF"));
			EIMObjectType objTypeInsertURL = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_INSERTURL"));

			// WORKディレクトリクリア
			// 原本ファイル一時フォルダ生成
			File originalTempFolder = new File(EIMConfig.get("WORK") + ORIGINAL_TEMP_FOLDER);
			if(!originalTempFolder.exists()){
				originalTempFolder.mkdir();
			}

			// 公開ファイル一時フォルダ生成
			File publicFileTempFolder = new File(EIMConfig.get("WORK") + PUBLIC_TEMP_FOLDER);
			if(!publicFileTempFolder.exists()){
				publicFileTempFolder.mkdir();
			}
			// 一次フォルダ配下のファイルを削除
			File[] originalFiles = originalTempFolder.listFiles();
			for (int i = 0; i < originalFiles.length; i++) {
			  if (originalFiles[i].exists() && originalFiles[i].isFile()) {
				  try {
					  originalFiles[i].delete();
				  } catch ( Exception e) {
					  log.error("inディレクトリのクリアに失敗しました。" +  originalFiles[i].getPath());
				  }
			  }
			}

			File[] publicFiles = publicFileTempFolder.listFiles();
			for (int i = 0; i < publicFiles.length; i++) {
			  if (publicFiles[i].exists() && publicFiles[i].isFile()) {
				  try {
					  publicFiles[i].delete();
				  } catch ( Exception e) {
					  log.error("outディレクトリのクリアに失敗しました。" +  originalFiles[i].getPath());
				  }
			  }
			}


			// OCR処理オブジェクトリスト取得
			List<EIMObject> ocrProcessObjectList = getOcrProcessObjectList(sess);

			// OCR処理実行
			for(int i = 0; i < ocrProcessObjectList.size(); i++){
				int rockResult = 0;

				EIMObject ocrProcessObject = ocrProcessObjectList.get(i);

				// 実行前チェック PDF分割、PDF署名、URL挿入オブジェクトが存在するかチェック
				boolean existPDFBatchObject = existPDFBatchObject(sess, ocrProcessObject, objTypePDFDiv, objTypePDFSig, objTypeInsertURL);
				if(existPDFBatchObject){
					continue;
				}

				// OCR前処理
				boolean isCheck = ocrProcessBefore( sess, ocrProcessObject );
				if ( !isCheck ) {
					// 前処理チェックでNGの場合
					// OCR処理オブジェクトを削除
					deleteOCRObj(sess, ocrProcessObject);
					// OCR結果ステータス更新⇒失敗(9)
					updateOcrResultStatus(sess, documentObject, AppConstant.OCR_RESULT_STATUS_FAILURE);
					// OCR処理ステータス更新⇒完了(2)
					updateOcrProcessStatus(sess, documentObject, AppConstant.OCR_PROC_STATUS_PROCESS_COMPLETE);
					sess.commit();
					continue;
				}

				// OCR処理ステータス取得 存在しない場合スキップする。
				EIMAttribute ocrProcessStatusAttribute = documentObject.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_OCR_PROCESS_STATUS"));
				if ( ocrProcessStatusAttribute == null ) {
					log.warn(" " + EIMResource.getMessage("EIM.ERROR.LOGIC.FAILED.OCR.PROCESS.NOT.TARGET.PROCESS.STATUS",new Object[]{ocrProcessObject.getName()}));
					// OCR処理オブジェクトを削除
					deleteOCRObj(sess, ocrProcessObject);
					sess.commit();
					continue;
				}
				// OCR処理ステータスがOCR処理待(0),OCR処理中(1)以外の場合、スキップする。
				// ※OCR処理中(1)状態は、OCR処理スレッドが異常終了した場合を考慮している。
				// ※通常、OCR処理ステータスがOCR処理待(0),OCR処理中(1)以外(OCR処理完了(9))かつOCR処理オブジェクトが存在する状況は発生しないため。
				if(!(ocrProcessStatusAttribute.getInt() == AppConstant.OCR_PROC_STATUS_PROCESS_WAIT
				||   ocrProcessStatusAttribute.getInt() == AppConstant.OCR_PROC_STATUS_PROCESSING)){
					log.warn(" " + EIMResource.getMessage("EIM.ERROR.LOGIC.FAILED.OCR.PROCESS.NOT.TARGET.PROCESS.STATUS",new Object[]{ocrProcessObject.getName()}));
					// OCR処理オブジェクトを削除
					deleteOCRObj(sess, ocrProcessObject);
					sess.commit();
					continue;
				}

				// OCR処理ステータス更新⇒処理中(1)
				updateOcrProcessStatus(sess, documentObject, AppConstant.OCR_PROC_STATUS_PROCESSING);
				sess.commit();

				// ロック
				try{
					// OCR変換オブジェクトのDBレコードをロック
					rockResult = batchObjectDao.lockObjectById(ocrProcessObject.getId());
				}
				catch(Exception e)
				{
					// ロック利用不可(リソースビジー)の場合
					//   Oracle : エラーコード=54
					//   PostgreSQL : SQLステータス=55P03
					if (e instanceof SQLException && DatabasePlugInLoader.getPlugIn().matchesSQLException((SQLException) e, SQLExceptionTypeEnum.LOCK_NOT_AVAILABLE)) {
						log.debug("OCR変換をスキップしました。対象OCR変換オブジェクトが既に排他ロックが掛かっています。ID:" + ocrProcessObject.getId() + "  名称: " + ocrProcessObject.getName());
						sess.rollback();
					} else {
						log.warn(AppMessageUtils.makeLogMessage(0, EIMResource.getMessage("EIM.ERROR.LOGIC.BATCH.OBJECT.LOCK.FAIL"),
								new Object[]{ocrProcessObject.getName()}));
						log.warn(AppMessageUtils.makeLogMessage(e.getMessage()), e);
						// OCR処理オブジェクトを削除
						deleteOCRObj(sess, ocrProcessObject);
						// OCR結果ステータス更新⇒失敗(9)
						updateOcrResultStatus(sess, documentObject, AppConstant.OCR_RESULT_STATUS_FAILURE);
						// OCR処理ステータス更新⇒完了(2)
						updateOcrProcessStatus(sess, documentObject, AppConstant.OCR_PROC_STATUS_PROCESS_COMPLETE);
						sess.commit();
					}
					continue;
				}

				// オブジェクトIDのDBレコードが存在し、排他ロックを掛けた場合
				if ( rockResult == 1 ) {
					// 後続の処理を実行
				}
				// オブジェクトIDのDBレコードが存在しない場合
				else if (rockResult == 0) {
					log.info("OCR変換をスキップしました。対象OCR変換オブジェクトが存在しません。ID:" + ocrProcessObject.getId() + "  名称: " + ocrProcessObject.getName());
					continue;
				}

				OcrProcessService ocrProcessService = (OcrProcessService) context.getBean("ocrProcessService");
				ocrProcessService.setContext(context);
				ocrProcessService.setOcrProcessObject(ocrProcessObject);
				ocrProcessService.setDocumentObject(documentObject);
				ocrProcessService.setOriginalfile(originalfile);
				ocrProcessService.setPublicfile(publicfile);
				// OCR処理実行
				boolean isSuccsess = true;
				try {
					ocrProcessService.doOcr();
				} catch (Exception e) {
					// エラーの場合
					isSuccsess = false;
				}

				if ( isSuccsess == false) {
					// 失敗した場合
					log.error("予期せぬエラーでOCR処理が失敗しました。" + documentObject.getId() + " : " + documentObject.getName());
					// OCR処理オブジェクトを削除
					deleteOCRObj(sess, ocrProcessObject);
					// OCR結果ステータス更新⇒失敗(9)
					updateOcrResultStatus(sess, documentObject, AppConstant.OCR_RESULT_STATUS_FAILURE);
					// OCR処理ステータス更新⇒完了(2)
					updateOcrProcessStatus(sess, documentObject, AppConstant.OCR_PROC_STATUS_PROCESS_COMPLETE);
					sess.commit();
					continue;
				}

				// →成功した場合
				// SearchFramework 検索FW更新通知 対象：OCRドキュメント
				AppUpdateNoticeUtils.updateNoticeInsert(Long.valueOf(documentObject.getId()), "SEARCHFW_OCR_DOCUMENT");

				// OCR処理後処理
				ocrProcessEnd(sess, ocrProcessObject);

				// コミット
				sess.commit();
			}

		} catch(EIMException eime) {
			log.error(AppMessageUtils.makeLogMessage(1,eime.getMessage()), eime);
			try{
				// ロールバック
				if(sess != null){
					 sess.rollback();
				}

			} catch (Exception se) {
				log.error(AppMessageUtils.makeLogMessage(1,se.getMessage()), se);

			}

		} catch (Exception e) {
			log.error(AppMessageUtils.makeLogMessage(1,e.getMessage()), e);
			try{
				// ロールバック
				if(sess != null){
					 sess.rollback();
				}

			} catch (Exception se) {
				log.error(AppMessageUtils.makeLogMessage(1,se.getMessage()), se);

			}

		} finally {
			try{
				// セッションクローズ
				EIMThreadContext.removeEIMSession();
				if(sess != null){
					 sess.close();
				}

			} catch (Exception se) {
				log.error(AppMessageUtils.makeLogMessage(1,se.getMessage()), se);

			}
		}
	}

	/**
	 * セッション取得
	 *
	 * @return セッション
	 * @throws Exception 例外
	 */
	private static EIMSession getSession() throws Exception {

		// user取得
		EIMUser user = new EIMUser(1, null, null, null, null, null, 255, 0, null);

		// lang取得
		String lang = "";
		String EIM_CONFIG_LANG = "MESSAGELANG";
		String DEFAULT_LANG	= "JA";
		if(EIMConfig.get(EIM_CONFIG_LANG) != null){
			lang = EIMConfig.get(EIM_CONFIG_LANG);
		}else{
			lang = DEFAULT_LANG;
		}

		// データソース取得
		DataSource ds = (DataSource)context.getBean("dataSource");

		// セッション取得
		EIMSession sess = new EIMSession(user,lang);
		sess.setConnection(ds.getConnection());
		sess.setConsoleMode(true);
		sess.getDBConnection().setAutoCommit(false);
		EIMThreadContext.putEIMSession(sess);

		return sess;
	}

	/**
	 * OCR処理オブジェクトリスト取得
	 *
	 * @param sess セッション
	 * @return OCR処理対ブジェクトリスト
	 * @throws Exception 例外
	 */
	@SuppressWarnings({ "rawtypes" })
	private static List<EIMObject> getOcrProcessObjectList(EIMSession sess) throws Exception {

		// OCR処理オブジェクタイプを取得する。
		EIMObjectType ocrProcessObjectType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_OCR_PROCESS"));

		// OCR処理オブジェクトリストを取得する。
		List ocrProcessObjectList = SearchUtils.searchObject(	sess,
																ocrProcessObjectType,
																null,
																false,
																false,
																-1,
																null,
																null,
																null,
																null,
																null,
																null,
																null,
																null,
																null,
																null);

		// ID昇順にソート
		ocrProcessObjectList = AppObjectUtil.getIntSortedList(ocrProcessObjectList, "getId", true);

		// 作成者がシステム管理者のOCR処理オブジェクトを先頭に並び替え。
		// ※システム管理者が操作したOCR処理設定、OCR処理実行を優先的に処理させるため。
		List<EIMObject> retOcrProcessObjectList = new ArrayList<EIMObject>();
		for(int i = 0; i < ocrProcessObjectList.size(); i++){
			EIMObject object = (EIMObject)ocrProcessObjectList.get(i);
			if(object.getCreateUser().getId() == EIMConstant.SYSTEM_ADMIN_USER_ID){
				retOcrProcessObjectList.add(object);
			}
		}
		for(int i = 0; i < ocrProcessObjectList.size(); i++){
			EIMObject object = (EIMObject)ocrProcessObjectList.get(i);
			if(object.getCreateUser().getId() != EIMConstant.SYSTEM_ADMIN_USER_ID){
				retOcrProcessObjectList.add(object);
			}
		}
		return retOcrProcessObjectList;
	}

	/**
	 * PDF分割、PDF署名、URL挿入オブジェクトが存在するかチェック
	 * @param sess
	 * @param ocrObject OCR処理オブジェクト
	 * @param objTypePDFDiv PDF分割タイプ
	 * @param objTypePDFSig PDF署名タイプ
	 * @param objTypeInsertURL URL挿入タイプ
	 * @return existPDFBatchObject
	 * @throws Exception
	 */
	@SuppressWarnings({ "rawtypes" })
	private static boolean existPDFBatchObject(EIMSession sess, EIMObject ocrObject, EIMObjectType objTypePDFDiv, EIMObjectType objTypePDFSig, EIMObjectType objTypeInsertURL)throws Exception{
		boolean existPDFBatchObject = false;

		// 該当ドキュメントにPDF分割オブジェクトが存在する場合
		if(ObjectUtils.getObjectByTypeAndName(sess, objTypePDFDiv, ocrObject.getName()) != null){
			return true;
		}
		// 該当ドキュメントにURL挿入オブジェクトが存在する場合
		if(ObjectUtils.getObjectByTypeAndName(sess, objTypePDFSig, ocrObject.getName()) != null){
			return true;
		}
		// 該当ドキュメントにPDF署名オブジェクトが存在する場合
		if(ObjectUtils.getObjectByTypeAndName(sess, objTypeInsertURL, ocrObject.getName()) != null){
			return true;
		}
		return existPDFBatchObject;
	}

	/**
	 * OCR前処理
	 * 各種チェック実行
	 *
	 * @param sess セッション
	 * @throws Exception 例外
	 */
	private static boolean ocrProcessBefore(EIMSession sess, EIMObject ocrProcessObject) throws Exception{
		// OCR処理前各種チェック
		try{
			Object[] args = new Object[]{ocrProcessObject.getName()};

			// 対象文書を取得
			documentObject = ObjectUtils.getObjectById(sess, Long.parseLong(ocrProcessObject.getName()));
			if ( documentObject == null ) {
				// 通常対象文書は存在するため存在しない場合はエラーとする。
				log.error(" " + EIMResource.getMessage("EIM.ERROR.LOGIC.FAILED.OCR.PROCESS.NO.DOCUMENT",args));
				return false;
			}

			// 原本ファイルオブジェクト取得
			originalfile = FileUtils.getFile(sess, documentObject, FileUtils.getDefaultFormat(sess, documentObject.getType()));
			if(originalfile == null){
				args = new Object[]{ERROR_KEY_ORIGINAL,ocrProcessObject.getName()};
				log.error(" " + EIMResource.getMessage("EIM.ERROR.LOGIC.FAILED.OCR.PROCESS.NO.FILEOBJECT",args));
				return false;
			}

			// 公開ファイルオブジェクト取得
			publicfile = FileUtils.getFile(sess, documentObject, FileUtils.getFormatByName(sess, EIMConfig.get("FORMAT_NAME_PUBLIC")));
			if(publicfile == null){
				args = new Object[]{ERROR_KEY_PUBLIC,ocrProcessObject.getName()};
				log.error(" " + EIMResource.getMessage("EIM.ERROR.LOGIC.FAILED.OCR.PROCESS.NO.FILEOBJECT",args));
				return false;
			}

		} catch(Exception e){
			log.error(" OCR実行前の事前チェックに失敗しました。" , e );
			return false;
		}
		return true;
	}

	/**
	 * OCR処理後処理
	 *
	 * @param sess セッション
	 * @throws Exception 例外
	 */
	private static void ocrProcessEnd(EIMSession sess, EIMObject ocrProcessObject) throws Exception{

		// OCR結果ステータス更新⇒成功(1)
		updateOcrResultStatus(sess, documentObject, AppConstant.OCR_RESULT_STATUS_SUCCESS);

		// OCR処理ステータス更新⇒完了(2)
		updateOcrProcessStatus(sess, documentObject, AppConstant.OCR_PROC_STATUS_PROCESS_COMPLETE);

		// OCR処理オブジェクト削除
		deleteOCRObj(sess, ocrProcessObject);

	}

	/**
	 * OCR処理ステータス更新
	 *
	 * @param sess セッション
	 * @param documentObject ドキュメントオブジェクト
	 * @param status ステータス
	 * @throws Exception 例外
	 */
	private static void updateOcrProcessStatus(EIMSession sess,EIMObject documentObject, long status) throws Exception {
		if ( documentObject != null ) {
			// OCR処理ステータス更新
			EIMAttributeType attType = AttributeUtils.getAttributeTypeByName(sess,EIMConfig.get("ATTR_NAME_DOCUMENT_OCR_PROCESS_STATUS"));
			ObjectAttributeUtils.setAttribute(sess, documentObject, attType, status);
		}
	}

	/**
	 * OCR結果ステータス更新
	 *
	 * @param sess セッション
	 * @param documentObject ドキュメントオブジェクト
	 * @param status ステータス
	 * @throws Exception 例外
	 */
	private static void updateOcrResultStatus(EIMSession sess,EIMObject documentObject, long status) throws Exception {
		if ( documentObject != null ) {
			// OCR結果ステータス更新
			EIMAttributeType attType = AttributeUtils.getAttributeTypeByName(sess,EIMConfig.get("ATTR_NAME_DOCUMENT_OCR_RESULT_STATUS"));
			ObjectAttributeUtils.setAttribute(sess, documentObject, attType, status);
		}
	}

	// OCRオブジェクト削除
	private static void deleteOCRObj(EIMSession sess, EIMObject ocrObj) throws Exception{
		Log log = LogFactory.getLog(PDFConvertWatcher.class);
		try {
			// OCRオブジェクトを削除
			ocrObj = ObjectUtils.getObjectById(sess, ocrObj.getId());
			if (ocrObj != null) {
				ObjectUtils.deleteObject(sess, ocrObj);
			}

		} catch(Exception e) {
			log.error(" 後処理(OCRオブジェクト削除)に失敗しました。 :" + ocrObj.getName() + e);
			throw e;
		}
	}

}
