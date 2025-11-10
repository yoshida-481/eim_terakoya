package jp.co.ctc_g.eim.app.document.business.service;

import java.util.concurrent.CompletableFuture;

import jp.co.ctc_g.eim.framework2.business.domain.entity.FileDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;

/**
 * サムネイルおよびプレビュー用PDFファイルへの変換処理を非同期で実行するサービスインターフェイス
 */
public interface ConvertDocumentAsyncService {
	/**
	 * 指定されたObjectからThumbnailを作成する
	 *
	 * @param object オブジェクト
	 * @param file 対象ファイル
	 * @throws Exception
	 */
	public CompletableFuture<Boolean> convertDocumentForThumbnail(ObjectDomain object, FileDomain file) throws Exception;


	/**
	 * 指定されたObjectからPreviewを作成する
	 *
	 * @param object オブジェクト
	 * @param file 対象ファイル
	 * @throws Exception
	 */
	public CompletableFuture<Boolean> convertDocumentForPreview(ObjectDomain object, FileDomain file) throws Exception;

}
