package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.Date;

import jp.co.ctc_g.eim.admin.business.domain.OcrProcessorDomain;
import jp.co.ctc_g.eim.app.document.business.service.OcrProcessService;
import jp.co.ctc_g.eim.app.document.business.service.OcrProcessorPlugIn;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.context.ApplicationContext;

import common.util.AppMessageUtils;

import eim.bo.EIMFile;
import eim.bo.EIMObject;
import eim.bo.EIMResource;
import eim.util.EIMConfig;
import eim.util.FTPUtils;

/**
*
* OCR処理サービス
*
*/
public class OcrProcessServiceImpl implements OcrProcessService{

	/** 原本ファイル一時フォルダ */
	private static String ORIGINAL_TEMP_FOLDER = "original/";

	/** 公開ファイル一時フォルダ */
	private static String PUBLIC_TEMP_FOLDER = "public/";

	/** アプリケーションコンテキスト */
	private ApplicationContext context;

	/** OCR処理オブジェクト */
	private EIMObject ocrProcessObject;

	/** 対象ドキュメントオブジェクト */
	private EIMObject documentObject;

	/** 原本ファイルオブジェクト */
	private EIMFile originalfile;

	/** 公開ファイルオブジェクト */
	private EIMFile publicfile;

	/** ログ */
	private static Log log = LogFactory.getLog(OcrProcessService.class);

	/** ログ接頭辞※スレッド識別のため */
	private String logPrefix = "";

	/**
	 * OCR処理実行
	 * @return 実行結果
	 * @throws Exception 例外
	 */
	public void doOcr() throws Exception {

		logPrefix = "[objectId=" + ocrProcessObject.getName() + "]";

		// 開始ログ
		log.info(" " + this.logPrefix + EIMResource.getMessage("EIM.INFO.OCR.PROCESS.START"));

		// 原本ファイル
		File originalFile = null;

		// 公開ファイル
		File publicFile = null;

		// ファイル接尾辞
		SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMddHHmmss");
		String pdfFileSuffix = "_" + sdf.format(new Date());

		try{
			File originalTempFolder = new File(EIMConfig.get("WORK") + ORIGINAL_TEMP_FOLDER);
			File publicFileTempFolder = new File(EIMConfig.get("WORK") + PUBLIC_TEMP_FOLDER);

			// 公開ファイル取得(EIMサーバー⇒OCR処理サーバー(ローカル))
			String serverDocumentFilePath = publicfile.getDirectory().getPath() + ocrProcessObject.getName() + publicfile.getExt();
			originalFile = new File(originalTempFolder.getAbsolutePath() + "/" + ocrProcessObject.getName() + pdfFileSuffix + publicfile.getExt());
			try
			{
				FTPUtils.getFile(	EIMConfig.get("FTP_HOST"),
									EIMConfig.get("FTP_USER"),
									EIMConfig.get("FTP_PASS"),
									new File(serverDocumentFilePath),
									originalFile);

			} catch(Exception e) {
				// ファイル取得エラーの場合
				log.error(this.logPrefix + AppMessageUtils.makeLogMessage(1,e.getMessage(),new
						Object[]{ocrProcessObject.getName(),serverDocumentFilePath,originalFile.getPath()}), e);

				// OCR処理実行⇒失敗
				throw e;
			}

			// 公開ファイル生成
			publicFile = new File(publicFileTempFolder.getAbsolutePath() + "/" + ocrProcessObject.getName() + pdfFileSuffix + publicfile.getExt());

			// OCR処理実行
			OcrProcessorPlugIn ocrProcessorPlugIn = (OcrProcessorPlugIn) context.getBean("ocrProcessorPlugIn");
			OcrProcessorDomain ocrProcessorDomain = new OcrProcessorDomain();
			ocrProcessorDomain.setObjectId(ocrProcessObject.getName());
			ocrProcessorDomain.setObjectName(documentObject.getName());
			ocrProcessorDomain.setInFileName(originalFile.getAbsolutePath());
			ocrProcessorDomain.setOutFileName(publicFile.getAbsolutePath());
			ocrProcessorPlugIn.setOcrProcessorDomain(ocrProcessorDomain);
			ocrProcessorPlugIn.ocrProcessExce();


			// OCR処理済公開ファイル差替(OCR処理サーバー(ローカル)⇒EIMサーバー)
			String publicDocumentFilePath = publicfile.getDirectory().getPath() + ocrProcessObject.getName() + publicfile.getExt();

			try
			{
				// PDFファイルの公開ファイルはシンボリックリンクであり、そのままputすると原本ファイルが置き変わってしまうためシンボリックリンクを削除する
				FTPUtils.deleteFile(	EIMConfig.get("FTP_HOST"),
									EIMConfig.get("FTP_USER"),
									EIMConfig.get("FTP_PASS"),
									new File(publicDocumentFilePath));
			} catch(Exception e) {
				log.error("シンボリックリンクの削除に失敗しました。" + publicDocumentFilePath , e);
				// OCR処理実行⇒失敗
				throw e;
			}
			System.out.println(publicDocumentFilePath);

			try
			{
				FTPUtils.putFile(	EIMConfig.get("FTP_HOST"),
									EIMConfig.get("FTP_USER"),
									EIMConfig.get("FTP_PASS"),
									publicFile,
									new File(publicDocumentFilePath));
			} catch(Exception e) {
				// ファイル差替エラーの場合
				log.error(this.logPrefix + AppMessageUtils.makeLogMessage(1,e.getMessage(),new
						Object[]{ocrProcessObject.getName(),publicFile.getPath(),publicDocumentFilePath}), e);

				// OCR処理実行⇒失敗
				throw e;
			}

			// OCR処理実行⇒成功

		} catch(Exception e) {
			log.error(AppMessageUtils.makeLogMessage(1,e.getMessage()), e);

			// OCR処理実行⇒失敗
			throw e;

		} finally {

			// 原本ファイルの削除
			if(originalFile!= null && originalFile.exists() && !originalFile.delete()){
				Object[] args = new Object[]{originalFile.getPath(),ocrProcessObject.getName()};
				log.warn(" " + this.logPrefix + EIMResource.getMessage("EIM.WARN.LOGIC.FAILED.OCR.PROCESS.FILE.DELETE", args));
			}

			// OCR処理済公開ファイルの削除
			if(publicFile!= null && publicFile.exists() && !publicFile.delete()){
				Object[] args = new Object[]{publicFile.getPath(),ocrProcessObject.getName()};
				log.warn(" " + this.logPrefix + EIMResource.getMessage("EIM.WARN.LOGIC.FAILED.OCR.PROCESS.FILE.DELETE", args));
			}

			// 終了ログ
			log.info(" " + this.logPrefix + EIMResource.getMessage("EIM.INFO.OCR.PROCESS.END"));
		}
	}

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.OcrProcessService#setContext(org.springframework.context.ApplicationContext)
	 */
	public void setContext(ApplicationContext context) {
		this.context = context;
	}

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.OcrProcessService#getOcrProcessObject()
	 */
	public EIMObject getOcrProcessObject() {
		return ocrProcessObject;
	}

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.OcrProcessService#setOcrProcessObject(eim.bo.EIMObject)
	 */
	public void setOcrProcessObject(EIMObject ocrProcessObject) {
		this.ocrProcessObject = ocrProcessObject;
	}

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.OcrProcessService#setOriginalfile(eim.bo.EIMFile)
	 */
	public void setOriginalfile(EIMFile originalfile) {
		this.originalfile = originalfile;
	}

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.OcrProcessService#setPublicfile(eim.bo.EIMFile)
	 */
	public void setPublicfile(EIMFile publicfile) {
		this.publicfile = publicfile;
	}

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.OcrProcessService#getDocumentObject()
	 */
	public EIMObject getDocumentObject() {
		return documentObject;
	}

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.OcrProcessService#setDocumentObject(eim.bo.EIMObject)
	 */
	public void setDocumentObject(EIMObject documentObject) {
		this.documentObject = documentObject;
	}

}
