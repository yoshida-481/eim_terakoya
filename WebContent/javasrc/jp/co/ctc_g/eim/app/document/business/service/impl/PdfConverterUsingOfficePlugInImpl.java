package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStreamReader;
import java.lang.ProcessBuilder.Redirect;
import java.util.concurrent.TimeUnit;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import common.util.AppMessageUtils;
import eim.util.EIMConfig;
import jp.co.ctc_g.eim.app.document.business.service.PdfConverterPlugin;

/**
 * HGPScanConverterアプリケーションのジョブを呼び出すコマンドプラグインの実装
 *
 * @since 1.6
 */
public class PdfConverterUsingOfficePlugInImpl implements PdfConverterPlugin {

	/** 出力対象オブジェクトID */
	private long objectId;
	/** インプットファイル */
	private File inputFile;
	/** アウトプットファイル */
	private File outputFile;

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.PdfConverterPlugin#canUse()
	 */
	public boolean canUse() throws Exception {
		// LibreOffice不使用ならtrue
		return !Boolean.valueOf(EIMConfig.get("LIBREOFFICE_USE"));
	}

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.PdfConverterPlugin#canSupported(java.io.File)
	 */
	public boolean canSupported(File file) throws Exception {
		String excelExtentionConfig = EIMConfig.get("PDF_CONVERT_FILE_TYPE_USING_OFFICE_EXCEL");
		String wordExtentionConfig = EIMConfig.get("PDF_CONVERT_FILE_TYPE_USING_OFFICE_WORD");
		String powerPOintExtentionConfig = EIMConfig.get("PDF_CONVERT_FILE_TYPE_USING_OFFICE_POWER_POINT");
		String hgpScanExtentionConfig = EIMConfig.get("PDF_CONV_HGPSCAN_FILE_TYPE");
		String extentionConfig = excelExtentionConfig + "," + wordExtentionConfig + "," + powerPOintExtentionConfig + "," + hgpScanExtentionConfig;
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
	 */
	public String getExecCommandLine() throws Exception {
		return "./PDFConvertService/usingOffice/PDFConvert.bat" +
				" /in:\"" + inputFile.getAbsolutePath() + "\"" +
				" /out:\"" + outputFile.getAbsolutePath() + "\"" +
				" /excelExts:\"" + EIMConfig.get("PDF_CONVERT_FILE_TYPE_USING_OFFICE_EXCEL") + "\"" +
				" /wordExts:\"" + EIMConfig.get("PDF_CONVERT_FILE_TYPE_USING_OFFICE_WORD") + "\"" +
				" /powerPointExts:\"" + EIMConfig.get("PDF_CONVERT_FILE_TYPE_USING_OFFICE_POWER_POINT") + "\"";
	}

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.PdfConverterPlugin#setObjectId(long)
	 */
	public void setObjectId(long objectId) {
		this.objectId = objectId;
	}

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.PdfConverterPlugin#setInputFile(java.io.File)
	 */
	public void setInputFile(File inputFile)  {
		this.inputFile = inputFile;
	}

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.PdfConverterPlugin#setOutputFile(java.io.File)
	 */
	public void setOutputFile(File outputFile) {
		this.outputFile = outputFile;
	}

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.PdfConverterPlugin#exec()
	 */
	public boolean exec() throws Exception {
		// Error Logging
		Log log = LogFactory.getLog(this.getClass().getName());

		Process process = null;
		BufferedReader br = null;
		try {
			// コマンド実行
			// splitしているのはコマンドに引数を連携する際、連続するスペースが1つに変更されるてしまうのを防ぐため
			// "ファイル   名.xlsx"→"ファイル 名.xlsx"に変換されてしまう。
			// splitし配列にすることで、各要素間にスペースが追加される。
			// 例）"ファイル   名.xlsx"
			//    ["ファイル","","","","名.xlsx"]→"ファイル"+" "+" "+" "+"名.xlsx"
			ProcessBuilder pb = new ProcessBuilder(getExecCommandLine().split(" "));
			pb.redirectOutput(Redirect.INHERIT);
			pb.redirectError(Redirect.INHERIT);
			process = pb.start();

			br = new BufferedReader(new InputStreamReader(process.getInputStream()));
		} catch (Exception e) {
			log.warn(getExecCommandLine());
			log.warn(AppMessageUtils.makeLogMessage(0,e.getMessage(),new Object[]{}), e);
			// throw e; 実行し続ける
		} finally {
			// 読み取り行の最後まで移動
			try {
				while(br != null && br.readLine() != null) {}
			} finally {
				br.close();
				process.getInputStream().close();
				process.getOutputStream().close();
				process.getErrorStream().close();
			}
		}

		int rcode = 1;
		if (process != null) {
			int timeout = Integer.valueOf(EIMConfig.get("PDF_TIMEOUT"));
			boolean end =process.waitFor(timeout, TimeUnit.SECONDS);
			if (end) {
				// 正常終了
				rcode = process.exitValue();
				process.destroy();
			} else {
				// タイムアウト
				log.warn("PDF変換プロセス(Office)がタイムアウトしました。" + inputFile.getAbsolutePath());
				rcode = -1;
				process.destroy();
			}
		}

		// エラーがなければtrue,それ以外ならfalseを返却
		return (rcode == 0);
	}

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.PdfConverterPlugin#getMessage()
	 */
	public String getMessage() throws Exception {
		return "PDF変換に失敗しました。";
	}

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.PdfConverterPlugin#getMessageKey()
	 */
	public String getMessageKey() throws Exception {
		return "";
	}

}
