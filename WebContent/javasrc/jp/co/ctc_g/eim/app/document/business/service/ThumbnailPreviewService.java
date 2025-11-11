package jp.co.ctc_g.eim.app.document.business.service;

/**
 * サムネイル・プレビュー用のビジネスサービスです。
 * @since Ver 6.18
 */
public interface ThumbnailPreviewService {


	/**
	 * 指定されたIDのサムネイルデータを取得します。<br>
	 * <br>
	 * @param id オブジェクトID
	 * @return サムネイルデータ
	 */
	public String getImage(long id) throws Exception;

	/**
	 * 指定されたIDのプレビューデータの存在チェックを行います。<br>
	 * 存在しない場合は作成処理を行う<br>
	 * <br>
	 * @param id オブジェクトID
	 * @return 存在結果
	 */
	public Boolean checkExistsPdf(long id) throws Exception;

}
