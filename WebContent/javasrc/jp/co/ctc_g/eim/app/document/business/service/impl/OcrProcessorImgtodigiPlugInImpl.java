package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

import jp.co.ctc_g.eim.admin.business.domain.OcrProcessorDomain;
import jp.co.ctc_g.eim.app.document.business.service.OcrProcessorPlugIn;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import eim.bo.EIMResource;
import eim.util.EIMConfig;


/**
 * OCR処理プラグイン
 *
 */
public class OcrProcessorImgtodigiPlugInImpl implements OcrProcessorPlugIn {

	/** バッチプロセス成功戻り値 */
	private static int PROCESS_RETURN_CODE_SUCCESS = 0;
	
	/** OCR処理ドメイン */
	private OcrProcessorDomain ocrProcessorDomain;
	
	/** ログ */
	private static Log log = LogFactory.getLog(OcrProcessorPlugIn.class);
	
	/** ログ接頭辞※スレッド識別のため */
	private String logPrefix = "";

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.OcrProcessorPlugIn#ocrProcessExce()
	 */
	public void ocrProcessExce() throws Exception {
		
		 // エラー発生時、エラーログ一覧
		 // ------------------------------------------------------------------------------------------------------------------------------
		 // ｜[番号]:エラーメッセージ						｜説明
		 // ------------------------------------------------------------------------------------------------------------------------------
		 // ｜[11]:Function parameter error					｜関数のパラメータが不正です。
		 // ｜[12]:Command argument error					｜コマンドのパラメータが不正です。
		 // ｜[13]:Missing mandatory parameter error		｜必須パラメータが指定されていません。
		 // ｜[20]:File open error							｜ファイルが開けませんでした。
		 // ｜[21]:File read error							｜ファイルが読込めませんでした。
		 // ｜[22]:File write error							｜ファイルに書込めませんでした。 
		 // ｜[23]:Invalid data								｜データが不正です。 
		 // ｜[24]:Execute error							｜プロセス起動に失敗しました。 
		 // ｜[25]:Directory access error					｜ディレクトリアクセスでエラーが発生しました。 
		 // ｜[27]:Password error							｜パスワードが不正です。 
		 // ｜[28]:Library load error / No function			｜ライブラリがロード出来ませんでした。 またはライブラリから関数が見つかりませんでした。 
		 // ｜[29]:Runtime Process error					｜起動したプロセスがエラーを返しました。 
		 // ｜[30]:Incorrect Setup / Environmental error	｜正しくセットアップされていません。 
		 // ｜[31]:System call error						｜システムコールまたはAPI呼出しでエラーが発生しました。 
		 // ｜[32]:License error							｜ライセンスが不正です。 
		 // ｜[33]:image processing error					｜画像処理でエラーが発生しました。 
		 // ｜[34]:0 Division error							｜０割が発生しました。 
		 // ｜[35]:Calculation error						｜計算エラーが発生しました。 
		 // ｜[200]:Internal error							｜内部エラーが発生しました。 
		 // ｜[201]:Memory error							｜メモリエラーが発生しました。 
		 // ｜[9998]:EOF error								｜ファイルまたはデータから予期しない終了を検出しました。 
		 // ｜[9999]:Unknown error							｜予期しないエラーが発生しました。 
		 // ------------------------------------------------------------------------------------------------------------------------------
		
		// ログ接頭辞([objectId=XXXXX])
		this.logPrefix = "[objectId=" + ocrProcessorDomain.getObjectId() + "]";
		
		// 開始ログ
		log.info(" " + this.logPrefix + EIMResource.getMessage("EIM.INFO.OCR.PROCESSOR.START"));

		InputStream inputStream = null;
		InputStreamReader inputStreamReader = null;
		BufferedReader bufferedReader = null;
		try{
			// OCR処理コマンド生成
			List<String> commandList = new ArrayList<String>();
			commandList.add(EIMConfig.get("OCR_PROCESSOR_IMGTODIGI"));							// OCR処理実行ファイルパス
			commandList.add(EIMConfig.get("OCR_PROCESSOR_IMGTODIGI_INPUTFILE_CMD"));			// 入力ファイルパス
			commandList.add(this.ocrProcessorDomain.getInFileName());
			commandList.add(EIMConfig.get("OCR_PROCESSOR_IMGTODIGI_OUTPUTFILE_CMD"));			// 出力ファイルパス
			commandList.add(this.ocrProcessorDomain.getOutFileName());
			commandList.add(EIMConfig.get("OCR_PROCESSOR_IMGTODIGI_HISTOGRAM_DICTIONARY_CMD"));	// ヒストグラム辞書
			commandList.add(EIMConfig.get("OCR_PROCESSOR_IMGTODIGI_HISTOGRAM_DICTIONARY"));
			commandList.add(EIMConfig.get("OCR_PROCESSOR_IMGTODIGI_FFT_DICTIONARY_CMD"));		// FFT辞書
			commandList.add(EIMConfig.get("OCR_PROCESSOR_IMGTODIGI_FFT_DICTIONARY"));
			commandList.add(EIMConfig.get("OCR_PROCESSOR_IMGTODIGI_INI_CMD"));					// INIファイル
			commandList.add(EIMConfig.get("OCR_PROCESSOR_IMGTODIGI_INI"));
			ProcessBuilder processBuilder = new ProcessBuilder(commandList);
			
			// エラー出力を標準出力にリダイレクト
			// ※この処理を入れないと、バッファへの出力待ちでデットロックが発生してしまうケースがある。
			// 　先に標準出力が全て来るまでループしているが、その間に標準エラーに書かれてバッファーが一杯になると、外部プロセスはブロッキングされる。
			processBuilder.redirectErrorStream(true);
			
			// OCR処理実行
			Process process = processBuilder.start();
			
			// 標準出力読込
			String outLog = "";
			String line  = "";
			inputStream = process.getInputStream();
			inputStreamReader = new InputStreamReader(inputStream);
			bufferedReader = new BufferedReader(inputStreamReader);
			while((line = bufferedReader.readLine()) != null)
			{
				outLog += " " + this.logPrefix + line + "\r\n";
			}
			
			// OCR処理実行結果判定 
			process.waitFor();
			if(process.exitValue() == PROCESS_RETURN_CODE_SUCCESS){
				// 成功
				
				// ファイル存在チェック
				File outFile = new File(ocrProcessorDomain.getOutFileName());
				if(!outFile.exists()){
					Object[] args = new Object[]{this.ocrProcessorDomain.getOutFileName(),this.ocrProcessorDomain.getObjectId()};				
					throw new Exception(" " + this.logPrefix + EIMResource.getMessage("EIM.ERROR.LOGIC.FAILED.OCR.PROCESS.CREATE.OCR.PDF.FILE", args));
					
				}
				
			} else {
				// 失敗
				
				// OCR処理エラー時のみログ出力
				log.error(outLog);
				
				Object[] args = new Object[]{ocrProcessorDomain.getObjectId()};
				throw new Exception(" " + this.logPrefix + EIMResource.getMessage("EIM.ERROR.LOGIC.FAILED.OCR.PROCESS",args));
				
			}
			
		} catch(Exception e) {
			throw e;
			
		} finally {
			
			// 入力ストリームクローズ
			if(inputStream != null){
				inputStream.close();
			}
			// 入力ストリームリーダークローズ
			if(inputStreamReader != null){
				inputStreamReader.close();
			}
			// バッファーリーダークローズ
			if(bufferedReader != null){
				bufferedReader.close();
			}
		}
		
		// 終了ログ
		log.info(" " + this.logPrefix + EIMResource.getMessage("EIM.INFO.OCR.PROCESSOR.END"));
	}
	
	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.OcrProcessorPlugIn#setOcrProcessorDomain()
	 */
	public void setOcrProcessorDomain(OcrProcessorDomain ocrProcessorDomain)
	{
		this.ocrProcessorDomain = ocrProcessorDomain;
	}

}
