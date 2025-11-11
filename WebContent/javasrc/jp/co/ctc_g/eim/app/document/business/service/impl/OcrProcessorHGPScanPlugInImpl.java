package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStreamReader;

import jp.co.ctc_g.eim.admin.business.domain.OcrProcessorDomain;
import jp.co.ctc_g.eim.app.document.business.service.OcrProcessorPlugIn;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import common.util.AppMessageUtils;
import common.util.AppUpdateNoticeUtils;
import common.enumeration.HGPScanErrorCodeEnum;

import eim.bo.EIMResource;
import eim.util.EIMConfig;

/**
 * HGPScanConverterアプリケーションのジョブを呼び出すコマンドプラグインの実装
 *
 * @since 1.6
 */
public class OcrProcessorHGPScanPlugInImpl implements OcrProcessorPlugIn {
	
	
	/** OCR処理ドメイン */
	private OcrProcessorDomain ocrProcessorDomain;
	
	/** 実行結果コード:デフォルトは異常なし */
	private HGPScanErrorCodeEnum execCode = HGPScanErrorCodeEnum.UNEXECUTE;
	/** 実行結果実コード */
	private int execCodeNum = HGPScanErrorCodeEnum.UNEXECUTE.code;
	
	/** 実行コマンド(変化しないため一度作成したら使いまわす) */
	private String execCommandLine;
	
	/** ログ接頭辞※スレッド識別のため */
	private String logPrefix = "";


	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.PdfConverterPlugin#exec()
	 * @since 1.6
	 */
	public void ocrProcessExce() throws Exception {
		// Logging
		Log log = LogFactory.getLog(this.getClass().getName());
		
		// ログ接頭辞([objectId=XXXXX objectName=XXXXX])
		this.logPrefix = "[objectId=" + ocrProcessorDomain.getObjectId()+" objectName=" + ocrProcessorDomain.getObjectName()+ "]";
		
		// 開始ログ
		log.info(" " + this.logPrefix + EIMResource.getMessage("EIM.INFO.OCR.PROCESSOR.START"));
		// 繰り返し回数
		String retryConfig = EIMConfig.get("OCR_PROCESSOR_HGPSCAN_RETRY_LIMIT");
		int retry = Integer.parseInt(retryConfig);
		// 重複時リトライ待機
		String sleepConfig = EIMConfig.get("OCR_PROCESSOR_HGPSCAN_OVERLAP_ERROR_SLEEP_TIME");
		int sleepTime = Integer.parseInt(sleepConfig);
		
		// OCR処理コマンド生成
		// コマンドラインフォーマット取得
		String format = EIMConfig.get("OCR_PROCESSOR_HGPSCAN_FORMAT");
		// アプリケーションパス取得
		String applicationPath = EIMConfig.get("OCR_PROCESSOR_HGPSCAN");
		// ジョブ名取得
		String jobName = EIMConfig.get("OCR_PROCESSOR_HGPSCAN_JOB_NAME");
		// コマンド実行タイムアウト時間取得
		String timeout = EIMConfig.get("OCR_PROCESSOR_HGPSCAN_TIME_OUT");
		// 書式にコマンドパラメータを配置して返却
		execCommandLine = String.format(format, applicationPath, jobName, timeout);

		boolean isSuccess = false;
		for (int cnt = 0; cnt < retry; cnt++) {
			Process process = null;
			BufferedReader br = null;
			try {
				// コマンド実行前に一瞬時間を置く
				Thread.sleep(1*1000);

				// コマンド実行
				process = Runtime.getRuntime().exec(execCommandLine);
				br = new BufferedReader(new InputStreamReader(process.getInputStream()));
			} catch (Exception e) {
				log.warn(execCommandLine);
				log.warn(AppMessageUtils.makeLogMessage(0,e.getMessage(),new Object[]{}), e);
				throw e;
			}
			// 読み取り行の最後まで移動
			try {
				while(br.readLine() != null) {}
			} finally {
				br.close();
			}
			int rcode = process.exitValue();
			process.destroy();
			
			// 実行結果の戻り値をセットする
			judgeMessageKey(rcode);

			// 実行結果が異常ある場合
			if (execCode != HGPScanErrorCodeEnum.NONE) {
				// 運用上発生する可能性のあるエラーの場合は一定時間停止後リトライ(例：重複実行、PDFが開かれているか権限がない等）
				if(execCode.code == HGPScanErrorCodeEnum.COMMAND_DUPLICATION.code || execCode.code == HGPScanErrorCodeEnum.OPEN_FILE.code ||
						execCode.code == HGPScanErrorCodeEnum.DATA_NOT_FOUND.code || execCode.code == HGPScanErrorCodeEnum.NOT_MONITORING.code ||
						execCode.code == HGPScanErrorCodeEnum.PROCCESSING_OTHER.code || execCode.code == HGPScanErrorCodeEnum.FAILED.code) {
					// log.warn("OCRコマンド実行に失敗しました。リトライします。：" + EIMResource.getMessage(execCode.key) + "エラーコード：" + rcode);
					Thread.sleep(sleepTime * 1000);
					continue;
				} else {
					// 異常終了する
					log.warn(getMessage());
					log.warn(execCommandLine);
					Object[] args = new Object[]{ocrProcessorDomain.getObjectId()};
					throw new Exception(" " + this.logPrefix + EIMResource.getMessage("EIM.ERROR.LOGIC.FAILED.OCR.PROCESS",args));
				}
			}else {
				// 成功
				// ファイル存在チェック
				File outFile = new File(ocrProcessorDomain.getOutFileName());
				if(!outFile.exists()){
					Object[] args = new Object[]{this.ocrProcessorDomain.getOutFileName(),this.ocrProcessorDomain.getObjectId()};
					throw new Exception(" " + this.logPrefix + EIMResource.getMessage("EIM.ERROR.LOGIC.FAILED.OCR.PROCESS.CREATE.OCR.PDF.FILE", args));
				}
				isSuccess = true;
				// 終了ログ
				log.info(" " + this.logPrefix + EIMResource.getMessage("EIM.INFO.OCR.PROCESSOR.END"));
				break;
			}
		}

		// リトライ回数を超えても成功できていない（breakできていない）場合異常終了とする
		if (isSuccess == false) {
			log.warn(getMessage());
			log.warn(execCommandLine);
			throw new Exception(" " + this.logPrefix + "リトライ回数の上限を超えました。OCR処理オブジェクト："+ ocrProcessorDomain.getObjectId());
		}
		
	}
	
	/**
	 * 実行結果の戻り値を判定し、エラー列挙型を逆引きする
	 *
	 * @param returnCode 実行結果コード
	 * @return エラータイプ型
	 */
	private HGPScanErrorCodeEnum judgeMessageKey(int returnCode) {
		this.execCodeNum = returnCode;
		this.execCode = HGPScanErrorCodeEnum.codeOf(returnCode);
		return this.execCode;
	}

	/**
	 * 実行結果のメッセージを取得する
	 *
	 */
	private String getMessage() throws Exception {
		String message = null;
		if (execCode == HGPScanErrorCodeEnum.NONE) {
			message = "";
		} else {
			String messageBase = EIMResource.getMessage(execCode.key);
			//エラーコードをつける
			String format = EIMConfig.get("OCR_PROCESSOR_HGPSCAN_ERROR_FORMAT");
			message = String.format(format, execCodeNum, messageBase, ocrProcessorDomain.getObjectName(), Integer.valueOf(ocrProcessorDomain.getObjectId()));
		}
		return message;
	}
	
	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.OcrProcessorPlugIn#setOcrProcessorDomain()
	 */
	public void setOcrProcessorDomain(OcrProcessorDomain ocrProcessorDomain)
	{
		this.ocrProcessorDomain = ocrProcessorDomain;
	}

}
