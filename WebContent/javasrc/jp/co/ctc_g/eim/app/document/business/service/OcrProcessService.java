package jp.co.ctc_g.eim.app.document.business.service;

import org.springframework.context.ApplicationContext;

import eim.bo.EIMFile;
import eim.bo.EIMObject;

/**
*
* OCR処理サービスインターフェース
*
*/
public interface OcrProcessService  {

	/** OCR処理実行ステータス */
	enum Status { ACTIVE, SUCCESS, FAIL; }

	/**
	 * OCR処理を実行する
	 *
	 * 	@return OCR処理結果
	 * @throws Exception
	 */
	public void doOcr() throws Exception;

	/**
	 * OCR処理オブジェクトをセットする
	 *
	 * @param ocrProcessObject OCR処理オブジェクト
	 */
	public void setContext(ApplicationContext context);

	/**
	 * OCR処理実行ステータス取得
	 *
	 * @return OCR処理オブジェクト
	 */
	public EIMObject getOcrProcessObject();

	/**
	 * ドキュメントオブジェクト取得
	 *
	 * @return ドキュメントオブジェクト
	 */
	public EIMObject getDocumentObject();

	/**
	 * OCR処理オブジェクトをセットする
	 *
	 * @param ocrProcessObject OCR処理オブジェクト
	 */
	public void setOcrProcessObject(EIMObject ocrProcessObject);

	/**
	 * 原本ファイルオブジェクトをセットする
	 *
	 * @param originalfile 原本ファイルオブジェクト
	 */
	public void setOriginalfile(EIMFile originalfile);

	/**
	 * 公開ファイルオブジェクトをセットする
	 *
	 * @param publicfile 公開ファイルオブジェクト
	 */
	public void setPublicfile(EIMFile publicfile);

	/**
	 * ドキュメントオブジェクトをセットする
	 *
	 * @param documentObject ドキュメントオブジェクト
	 */
	public void setDocumentObject(EIMObject documentObject);

}
