package jp.co.ctc_g.eim.app.document.business.service;

import jp.co.ctc_g.eim.framework2.business.domain.entity.FileDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;

/**
 *
 * ドキュメント変換登録サービスインターフェイス
 *
 */
public interface ConvertDocumentService {
	/**
	 * 指定されたObjectからThumbnailを作成する
	 *
	 * @param object オブジェクト
	 * @param file 対象ファイル
	 * @return 変換処理を実行した場合はtrue
	 * @throws Exception
	 */
	public Boolean convertDocumentForThumbnail(ObjectDomain object, FileDomain file) throws Exception;

	/**
	 * 指定されたObjectからPreviewを作成する
	 *
	 * @param object オブジェクト
	 * @param file 対象ファイル
	 * @return 変換処理を実行した場合はtrue
	 * @throws Exception
	 */
	public Boolean convertDocumentForPreview(ObjectDomain object, FileDomain file) throws Exception;

	/**
	 * 指定されたObjectからThumbnailを作成する
	 *
	 * @param object オブジェクト
	 * @param file 対象ファイル
	 * @return 変換処理を実行した場合はtrue
	 * @throws Exception
	 */
	public Boolean syncConvertDocumentForThumbnail(ObjectDomain object, FileDomain file) throws Exception;

	/**
	 * 指定されたObjectからPreviewを作成する
	 *
	 * @param object オブジェクト
	 * @param file 対象ファイル
	 * @return 変換処理を実行した場合はtrue
	 * @throws Exception
	 */
	public Boolean syncConvertDocumentForPreview(ObjectDomain object, FileDomain file) throws Exception;

	/**
	 * 対象のファイルがThumbnail対象かチェックする
	 *
	 * @param object オブジェクト
	 * @param file 対象ファイル
	 * @return Thumbnail対象か
	 * @throws Exception
	 */
	public Boolean checkTargetConvertForThumbnail(ObjectDomain object, FileDomain file) throws Exception;

	/**
	 * 対象のファイルがPreview対象かチェックする
	 *
	 * @param object オブジェクト
	 * @param file 対象ファイル
	 * @return Preview対象か
	 * @throws Exception
	 */
	public Boolean checkTargetConvertForPreview(ObjectDomain object, FileDomain file) throws Exception;

	/**
	 * 対象のファイルのThumbnail削除
	 *
	 * @param object オブジェクト
	 * @throws Exception
	 */
	public void clearFormatForThumbnail(ObjectDomain object) throws Exception;

	/**
	 * 対象のファイルのPreview削除
	 *
	 * @param object オブジェクト
	 * @throws Exception
	 */
	public void clearFormatForPreview(ObjectDomain object) throws Exception;

	/**
	 * 対象のファイルがThumbnail変換が許可されているかチェックする
	 *
	 * @param object オブジェクト
	 * @return Thumbnail変換が許可されているか
	 * @throws Exception
	 */
	public Boolean checkAllowedConversionForThumbnail(ObjectDomain object) throws Exception;

	/**
	 * 対象のファイルがPreview変換が許可されているかチェックする
	 *
	 * @param object オブジェクト
	 * @return Preview変換が許可されているか
	 * @throws Exception
	 */
	public Boolean checkAllowedConversionForPreview(ObjectDomain object) throws Exception;
}
