package jp.co.ctc_g.eim.app.document.business.service;

import jp.co.ctc_g.eim.admin.business.domain.OcrProcessorDomain;

/**
*
* OCR処理プラグインインターフェース
*
*/
public interface OcrProcessorPlugIn {
	
	/**
	 * OCR処理実行
	 * 
	 * @throws 例外
	 */
	public void ocrProcessExce() throws Exception;
	
	/**
	 * OCR処理ドメイン設定
	 * 
	 * @param ocrProcessorDomain OCR処理ドメイン
	 */
	public void setOcrProcessorDomain(OcrProcessorDomain ocrProcessorDomain);
	
}
