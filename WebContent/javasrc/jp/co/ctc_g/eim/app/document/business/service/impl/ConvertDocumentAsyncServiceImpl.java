package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.CompletableFuture;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.hc.client5.http.HttpHostConnectException;
import org.apache.hc.client5.http.HttpResponseException;
import org.apache.hc.client5.http.classic.methods.HttpPost;
import org.apache.hc.client5.http.entity.mime.MultipartEntityBuilder;
import org.apache.hc.client5.http.impl.classic.CloseableHttpClient;
import org.apache.hc.client5.http.impl.classic.CloseableHttpResponse;
import org.apache.hc.client5.http.impl.classic.HttpClients;
import org.apache.hc.core5.http.ContentType;
import org.apache.hc.core5.http.HttpEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

import eim.util.EIMConfig;
import jp.co.ctc_g.eim.app.document.business.dao.AsyncTxControlObjectDao;
import jp.co.ctc_g.eim.app.document.business.service.ConvertDocumentAsyncService;
import jp.co.ctc_g.eim.app.document.common.util.FileUtils;
import jp.co.ctc_g.eim.framework2.business.dao.FileDao;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.DirectoryDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.FileDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.FormatDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework2.business.service.DirectoryService;
import jp.co.ctc_g.eim.framework2.business.service.FormatService;
import jp.co.ctc_g.eim.framework2.business.service.ObjectService;
import jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.common.context.TransactionContext;
import jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum;
import jp.co.ctc_g.eim.framework2.common.exception.EIMException;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;
import jp.co.ctc_g.eim.framework2.integration.util.internal.FileUtil;

/**
 * サムネイルおよびプレビュー用PDFファイルへの変換処理を非同期で実行するサービス実装
 * @since 6.18
 */
@EnableAsync
public class ConvertDocumentAsyncServiceImpl implements ConvertDocumentAsyncService {

	/** Logger */
	private static final Log log = LogFactory.getLog(ConvertDocumentAsyncServiceImpl.class);

	/** オブジェクトサービス */
	private ObjectService objectService;

	/** ファイルDao */
	private FileDao fileDao;

	/** フォーマットサービス */
	private FormatService formatService = null;

	/** ディレクトリサービス */
	private DirectoryService directoryService;

	/** 非同期Executor */
	private ThreadPoolTaskExecutor taskExecutor;

	/** 非同期トランザクション制御用オブジェクトDao */
	private AsyncTxControlObjectDao asyncTxControlObjectDao;

	/** Thumbnailタスク管理用 **/
	private final Set<Long> poolTaskSetsForThumbnail = new HashSet<>();

	/** Previewタスク管理用 **/
	private final Set<Long> poolTaskSetsForPreview = new HashSet<>();

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.ConvertDocumentService#convertDocumentForThumbnail(jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain, jp.co.ctc_g.eim.framework2.business.domain.entity.FileDomain)
	 */
	@Async("thumbnailPreviewTaskExecutor")
	public CompletableFuture<Boolean> convertDocumentForThumbnail(ObjectDomain object, FileDomain file) throws Exception {
		if (!poolTaskSetsForThumbnail.add(object.getId())) {
			// トランザクション待機中の同一オブジェクトに対するファイル変換は行わない
			return CompletableFuture.completedFuture(false);
		}

		try {
			try {
				// 非同期の場合トランザクションが引き継がれないため、作成する
				TransactionContext context = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
				UserDomain user = object.getModificationUser();
				context.setUser(user);
				EIMThreadContext.putTransactionContext(context);

				// ドキュメント登録・更新トランザクション終了を待機
				asyncTxControlObjectDao.waitTransactionEnd(object);
			} catch (EIMException e) {
				if (e.getMessageKey().equals("EIM.ERROR.OBJECT.NOTFOUND.DETAIL")) {
					// 対象のオブジェクトが存在しない場合は終了(オブジェクト登録処理がロールバックされた場合を想定)
					if (log.isDebugEnabled()) {
						log.debug(e.getMessage());
					}
					return CompletableFuture.completedFuture(false);
				}
				throw e;
			} finally {
				poolTaskSetsForThumbnail.remove(object.getId());
			}

			// ファイル再取得 (ディレクトリ情報を取得)
			FileDomain targetFile = fileDao.getByObjectAndFormat(object, file.getFormat());
			// TODO ファイル更新処理がロールバックされた場合は処理を終了する。シーケンス番号で判定すべきだが、V6.18時点シーケンス番号が未使用。

			String targetAttTypeName = EIMConfig.get("ATTR_NAME_NOT_ALLOWED_CONVERSION_THUMBNAIL");
			AttributeTypeDomain attributeType = new AttributeTypeDomain(targetAttTypeName);

			FormatDomain thumbnailFormat = formatService.getByDefinitionName(EIMConfig.get("FORMAT_NAME_THUMBNAIL"));
			if (thumbnailFormat == null) {
				throw new EIMException("EIM.ERROR.LOGIC.FORMAT.NOTFOUND");
			}

			String requestCreateThumbnailUrl = EIMConfig.get("REQUEST_CREATE_THUMBNAIL_URL");
			if (requestCreateThumbnailUrl == null || requestCreateThumbnailUrl.isEmpty()) {
				throw new EIMException("EIM.ERROR.LOGIC.REQUEST.CREATE.THUMBNAIL.URL.NOTFOUND");
			}

			// ファイル変換実行
			convertDocument(object, targetFile, thumbnailFormat, attributeType, requestCreateThumbnailUrl, ".jpg");
			if (log.isDebugEnabled()) {
				log.debug(String.format("Completed creating for %s : %s" , thumbnailFormat.getDefinitionName(), targetFile.getName()));
			}
		} catch (Exception e) {
			log.warn("File conversion failed.", e);
			throw e;
		} finally {
			EIMThreadContext.removeTransactionContext();
		}
		return CompletableFuture.completedFuture(true);
	}

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.ConvertDocumentService#convertDocumentForPreview(jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain, jp.co.ctc_g.eim.framework2.business.domain.entity.FileDomain)
	 */
	@Async("thumbnailPreviewTaskExecutor")
	public CompletableFuture<Boolean> convertDocumentForPreview(ObjectDomain object, FileDomain file) throws Exception {
		if (!poolTaskSetsForPreview.add(object.getId())) {
			// トランザクション待機中の同一オブジェクトに対するファイル変換は行わない
			return CompletableFuture.completedFuture(false);
		}

		try {
			try {
				// 非同期の場合トランザクションが引き継がれないため、作成する
				TransactionContext context = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
				UserDomain user = object.getModificationUser();
				context.setUser(user);
				EIMThreadContext.putTransactionContext(context);

				// ドキュメント登録・更新トランザクション終了を待機
				asyncTxControlObjectDao.waitTransactionEnd(object);
			} catch (EIMException e) {
				if (e.getMessageKey().equals("EIM.ERROR.OBJECT.NOTFOUND.DETAIL")) {
					// 対象のオブジェクトが存在しない場合は終了(オブジェクト登録処理がロールバックされた場合を想定)
					if (log.isDebugEnabled()) {
						log.debug(e.getMessage());
					}
					return CompletableFuture.completedFuture(false);
				}
				throw e;
			} finally {
				poolTaskSetsForPreview.remove(object.getId());
			}

			FormatDomain previewFormat = formatService.getByDefinitionName(EIMConfig.get("FORMAT_NAME_PREVIEW"));
			if (previewFormat == null) {
				throw new EIMException("EIM.ERROR.LOGIC.FORMAT.NOTFOUND");
			}

			if (file.getExt() != null && file.getExt().equals(".pdf")) {
				// 原本がPDFの場合はプレビューフォーマットにシンボリックリンク作成

				DirectoryDomain alwaysDirectory = directoryService.getOnlineByFormat(new FormatDomain(ConfigUtils.getByKey("FORMAT_NAME_ALWAYS")));
				if (alwaysDirectory == null) {
					throw new EIMException("EIM.ERROR.LOGIC.DIRECTORY.NOTFOUND");
				}

				File orgFile = new File(alwaysDirectory.getPath() + object.getId() + file.getExt());
				File dstFile = new File(previewFormat.getOnlineDirectory().getPath() + object.getId() + file.getExt());
				FileUtils.createSymbolicLink(orgFile, dstFile);

				// Checkin
				FileDomain targetFile = new FileDomain();
				targetFile.setDirectory(previewFormat.getOnlineDirectory());
				targetFile.setFormat(previewFormat);
				targetFile.setName(object.getId() + ".pdf");
				targetFile.setSize(file.getSize());
				fileDao.create(object, targetFile);
				if (log.isDebugEnabled()) {
					log.debug(String.format("FileDao.create: %d", object.getId()));
				}

				return CompletableFuture.completedFuture(true);
			}

			// ファイル再取得 (ディレクトリ情報を取得)
			FileDomain targetFile = fileDao.getByObjectAndFormat(object, file.getFormat());
			// TODO ファイル更新処理がロールバックした場合は処理を終了する。シーケンス番号で判定すべきだが、V6.18時点シーケンス番号が未使用。

			String targetAttTypeName = EIMConfig.get("ATTR_NAME_NOT_ALLOWED_CONVERSION_PREVIEW");
			AttributeTypeDomain attributeType = new AttributeTypeDomain(targetAttTypeName);

			String requestCreatePdfUrl = EIMConfig.get("REQUEST_CREATE_PDF_URL");
			if (requestCreatePdfUrl == null || requestCreatePdfUrl.isEmpty()) {
				throw new EIMException("EIM.ERROR.LOGIC.REQUEST.CREATE.PDF.URL.NOTFOUND");
			}

			// ファイル変換実行
			convertDocument(object, targetFile, previewFormat, attributeType, requestCreatePdfUrl, ".pdf");
			if (log.isDebugEnabled()) {
				log.debug(String.format("Completed creating for %s : %s" , previewFormat.getDefinitionName(), targetFile.getName()));
			}
		} catch (Exception e) {
			log.warn("File conversion failed.", e);
			throw e;
		} finally {
			EIMThreadContext.removeTransactionContext();
		}

		return CompletableFuture.completedFuture(true);
	}

	/**
	 * コンバートの実処理
	 * @param object
	 * @param file
	 * @param exportFormat
	 * @param attribute
	 * @param userId
	 * @param requestURL
	 * @throws Exception
	 */
	private void convertDocument(ObjectDomain object, FileDomain file, FormatDomain exportFormat, AttributeTypeDomain attributeType, String requestURL, String exportExt) throws Exception {

		// Source file
		String srcFilePathName = FileUtil.getFilePath(object, file);
		File srcFile = new File(srcFilePathName);
		if (!srcFile.exists()) {
			// 変換元ファイルが存在しない場合
			log.warn(String.format("Source file does not exists.[srcFilePathName: %s]", srcFilePathName));
			return;
		}

		if (log.isDebugEnabled()) {
			log.debug(String.format("Convert document: %d", object.getId()));
		}

		// Http request
		MultipartEntityBuilder builder = MultipartEntityBuilder.create();
		builder.addBinaryBody(
				"file",
				new File(srcFilePathName),
				ContentType.APPLICATION_OCTET_STREAM,
				srcFile.getName());
		HttpEntity multipart = builder.build();

		HttpPost httpPost = new HttpPost(requestURL);
		httpPost.setEntity(multipart);

		try (CloseableHttpClient httpClient = HttpClients.createDefault()) {
			// ConversionServiceを呼び出してファイル変換
			CloseableHttpResponse response = null;
			try {
				response = httpClient.execute(httpPost);
			} catch (HttpHostConnectException e) {
				// ConversionServiceに接続できない(変換サーバーが停止している場合はキューをクリア)
				this.clearQueue();
				log.error("ConversionService is not connected.");
				throw e;
			} catch (IOException e) {
				// リクエスト送信時にI/O例外
				log.error("I/O exception occurred.");
				throw e;
			}

			// 変換結果取得
			int statusCode = response.getCode();
			if (log.isDebugEnabled()) {
				log.debug(String.format("Request result status: %d", statusCode));
			}

			if (statusCode == 200) {
				// ファイル変換成功
				HttpEntity entity = response.getEntity();
				if (entity != null) {
					try (InputStream inputStream = entity.getContent()) {
						// Directory
						DirectoryDomain exportDir = exportFormat.getOnlineDirectory();

						// Save file
						long size = FileUtils.upload(
								exportDir.getPath() + object.getId() + exportExt,
								inputStream);

						// Checkin
						FileDomain targetFile = new FileDomain();
						targetFile.setDirectory(exportDir);
						targetFile.setFormat(exportFormat);
						targetFile.setName(object.getId() + exportExt);
						targetFile.setSize(size);
						fileDao.create(object, targetFile);
						if (log.isDebugEnabled()) {
							log.debug(String.format("FileDao.create: %d", object.getId()));
						}
					} catch (Exception e) {
						log.error(String.format("Failed to save file after conversion.[id: %d]", object.getId()));
						throw e;
					}
				}
			} else if (statusCode == 422 || statusCode == 413) {
				// ファイル変換不可 or ファイルサイズ上限超過 (変換不可フラグ属性をtrueにして正常終了)
				log.info(String.format("status code: %d, ConversionService cannot convert data.[id: %d]", statusCode, object.getId()));
				// 新規登録時の属性更新処理との競合を回避する
				objectService.getByIdForUpdate(object.getId());
				objectService.setAttributeSingleLong(object, attributeType, 1);
				// TODO ファイルサイズ上限超過は送信前にチェックしてファイル変換リクエストを送信しない方がいい
			} else if (statusCode == 503) {
				// 処理待ちキュー容量超過 (警告ログを出力して正常終了)
				log.warn(String.format("status code: %d, Processing queue capacity exceeded.[id: %d]", statusCode, object.getId()));
			} else {
				// 予期しないステータスコード
				throw new HttpResponseException(statusCode, String.format("An unexpected response was received.[id: %d]", object.getId()));
			}
		}
	}

	/**
	 * Queueのクリア
	 */
	private void clearQueue() {

        BlockingQueue<Runnable> queue = taskExecutor.getThreadPoolExecutor().getQueue();
        queue.clear();
	}

	/**
	 * オブジェクトサービスを設定します。
	 * @param objectService
	 */
	public void setObjectService(ObjectService objectService) {
		this.objectService = objectService;
	}

	/**
	 * ファイルDaoを設定します。
	 * @param fileDao
	 */
	public void setFileDao(FileDao fileDao) {
		this.fileDao = fileDao;
	}

	/**
	 * フォーマットサービスを設定します。
	 * @param formatService
	 */
	public void setFormatService(FormatService formatService) {
		this.formatService = formatService;
	}

	/**
	 * ディレクトリサービスを設定します。
	 * @param directoryService
	 */
	public void setDirectoryService(DirectoryService directoryService) {
		this.directoryService = directoryService;
	}

	/**
	 * 非同期Executorを設定します。
	 * @param taskExecutor
	 */
	public void setTaskExecutor(ThreadPoolTaskExecutor taskExecutor) {
		this.taskExecutor = taskExecutor;
	}

	/**
	 * 非同期トランザクション制御用オブジェクトDaoを設定します。
	 * @param asyncTxControlObjectDao 非同期トランザクション制御用オブジェクトDao
	 */
	public void setAsyncTxControlObjectDao(AsyncTxControlObjectDao asyncTxControlObjectDao) {
		this.asyncTxControlObjectDao = asyncTxControlObjectDao;
	}

}