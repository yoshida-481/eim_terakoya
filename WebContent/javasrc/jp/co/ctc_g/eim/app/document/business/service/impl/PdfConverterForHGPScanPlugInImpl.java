package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Map;

import jp.co.ctc_g.eim.app.document.business.service.PdfConverterPlugin;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import common.util.AppMessageUtils;
import common.util.OptionConfData;
import common.enumeration.HGPScanErrorCodeEnum;

import eim.bo.EIMResource;
import eim.util.EIMConfig;

/**
 * HGPScanConverterアプリケーションのジョブを呼び出すコマンドプラグインの実装
 * 
 * @since 1.6
 */
public class PdfConverterForHGPScanPlugInImpl implements PdfConverterPlugin {
	
	/** 出力対象オブジェクトID */
	private long objectId;
	/** インプットファイル */
	private File inputFile;
	
	/** 実行結果コード:デフォルトは異常なし */
	private HGPScanErrorCodeEnum execCode = HGPScanErrorCodeEnum.UNEXECUTE;
	/** 実行結果実コード */
	private int execCodeNum = HGPScanErrorCodeEnum.UNEXECUTE.code;
	
	/** 実行コマンド(変化しないため一度作成したら使いまわす) */
	private String execCommandLine;

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.PdfConverterPlugin#canUse()
	 * @since 1.6
	 */
	public boolean canUse() throws Exception {
		return OptionConfData.getInstance().hgscanConvPdf;
	}

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.PdfConverterPlugin#canSupported(java.io.File)
	 * @since 1.6
	 */
	public boolean canSupported(File file) throws Exception {
		String extentionConfig = EIMConfig.get("PDF_CONV_HGPSCAN_FILE_TYPE");
		String[] extensions = extentionConfig.split(",");
		
		boolean isTarget = false;
		for (String extension : extensions) {
			isTarget = FilenameUtils.isExtension(file.getPath(), extension);
			if(!isTarget){
				isTarget = FilenameUtils.isExtension(file.getPath(), extension.toUpperCase());
			}
			if (isTarget) {
				break;
			}
		}
		return isTarget;
	}

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.PdfConverterPlugin#getExecCommandLine()
	 * @since 1.6
	 */
	public String getExecCommandLine() throws Exception {
		if (execCommandLine == null) {
			// コマンドラインフォーマット取得
			String format = EIMConfig.get("PDF_CONV_HGPSCAN_FORMAT");
			// アプリケーションパス取得
			String applicationPath = EIMConfig.get("PDF_CONV_HGPSCAN");
			// ジョブ名取得
			String jobName = EIMConfig.get("PDF_CONV_HGPSCAN_JOB_NAME");
			// コマンド実行タイムアウト時間取得
			String timeout = EIMConfig.get("PDF_CONV_HGPSCAN_TIME_OUT");
			// 書式にコマンドパラメータを配置して返却	
			execCommandLine = String.format(format, applicationPath, jobName, timeout);
		}
		return execCommandLine;
	}

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.PdfConverterPlugin#setObjectId(long)
	 * @since 1.6
	 */
	public void setObjectId(long objectId) {
		this.objectId = objectId;
	}

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.PdfConverterPlugin#setInputFile(java.io.File)
	 * @since 1.6
	 */
	public void setInputFile(File inputFile)  {
		this.inputFile = inputFile;
	}

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.PdfConverterPlugin#setOutputFile(java.io.File)
	 * @since 1.6
	 */
	public void setOutputFile(File outputFile) {
		// 本プラグインのコマンドはファイル出力先を使用しない
	}

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.PdfConverterPlugin#exec()
	 * @since 1.6
	 */
	public boolean exec() throws Exception {
		// Error Logging
		Log log = LogFactory.getLog(this.getClass().getName());
		
		// 繰り返し回数
		String retryConfig = EIMConfig.get("PDF_CONV_HGPSCAN_RETRY_LIMIT");
		int retry = Integer.parseInt(retryConfig);
		// 重複時リトライ待機
		String sleepConfig = EIMConfig.get("PDF_CONV_HGPSCAN_OVERLAP_ERROR_SLEEP_TIME");
		int sleepTime = Integer.parseInt(sleepConfig);
		
		for (int cnt = 0; cnt < retry; cnt++) {
			if (cnt != 0) {
				// 初回以外
				if (execCode != HGPScanErrorCodeEnum.COMMAND_DUPLICATION) {
					// 重複以外の場合、ループ終了
					break;
				} else {
					// 重複実行エラーの場合,一定時間停止後リトライ
					log.debug(getMessage());
					Thread.sleep(sleepTime * 1000);
				}
			}
			
			Process process = null;
			BufferedReader br = null;
			try {
				// コマンド実行
				process = Runtime.getRuntime().exec(getExecCommandLine());
				br = new BufferedReader(new InputStreamReader(process.getInputStream()));
			} catch (Exception e) {
				log.warn(getExecCommandLine());
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
	
			judgeMessageKey(rcode);
		}

		// エラーがなければtrue,それ以外ならfalseを返却
		return (execCode == HGPScanErrorCodeEnum.NONE);
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
	 * @see jp.co.ctc_g.eim.app.document.business.service.PdfConverterPlugin#getMessage()
	 * @since 1.6
	 */
	public String getMessage() throws Exception {
		String message = null;
		if (execCode == HGPScanErrorCodeEnum.NONE) {
			message = "";
		} else {
			String messageBase = EIMResource.getMessage(getMessageKey());
			//エラーコードをつける
			String format = EIMConfig.get("PDF_CONV_HGPSCAN_ERROR_FORMAT");
			message = String.format(format, execCodeNum, messageBase, inputFile.getName(), objectId);
		}
		return message;
	}

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.PdfConverterPlugin#getMessageKey()
	 * @since 1.6
	 */
	public String getMessageKey() throws Exception {
		return execCode.key;
	}

}
