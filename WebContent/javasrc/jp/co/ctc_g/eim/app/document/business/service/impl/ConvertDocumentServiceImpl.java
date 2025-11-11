package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.RejectedExecutionException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import eim.util.EIMConfig;
import eim.util.StringUtils;
import jp.co.ctc_g.eim.app.document.business.service.ConvertDocumentAsyncService;
import jp.co.ctc_g.eim.app.document.business.service.ConvertDocumentService;
import jp.co.ctc_g.eim.framework2.business.dao.FileDao;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.FileDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.FormatDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.service.ObjectService;

/**
 * ドキュメント登録更新時にサムネイルやプレビュー用PDFを作成するサービス実装
 * @since 6.18
 */
public class ConvertDocumentServiceImpl implements ConvertDocumentService {

	/** Logger */
	private static final Log log = LogFactory.getLog(ConvertDocumentServiceImpl.class);

	/** オブジェクトサービス */
	private ObjectService objectService;

	/** ファイルDao */
	private FileDao fileDao;

	/** ファイル変換非同期サービス */
	private ConvertDocumentAsyncService convertDocumentAsyncService;

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.ConvertDocumentService#convertDocumentForThumbnail(jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain, jp.co.ctc_g.eim.framework2.business.domain.entity.FileDomain)
	 */
	@Override
	public Boolean convertDocumentForThumbnail(ObjectDomain object, FileDomain file) throws Exception {
		return this.doConvertDocumentForThumbnail(object, file, false);
	}

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.ConvertDocumentService#convertDocumentForPreview(jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain, jp.co.ctc_g.eim.framework2.business.domain.entity.FileDomain)
	 */
	@Override
	public Boolean convertDocumentForPreview(ObjectDomain object, FileDomain file) throws Exception {
		return this.doConvertDocumentForPreview(object, file, false);
	}

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.ConvertDocumentService#convertDocumentForThumbnail(jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain, jp.co.ctc_g.eim.framework2.business.domain.entity.FileDomain)
	 */
	@Override
	public Boolean syncConvertDocumentForThumbnail(ObjectDomain object, FileDomain file) throws Exception {
		return this.doConvertDocumentForThumbnail(object, file, true);
	}

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.ConvertDocumentService#convertDocumentForPreview(jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain, jp.co.ctc_g.eim.framework2.business.domain.entity.FileDomain)
	 */
	@Override
	public Boolean syncConvertDocumentForPreview(ObjectDomain object, FileDomain file) throws Exception {
		return this.doConvertDocumentForPreview(object, file, true);
	}

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.ConvertDocumentService#checkTargetConvertForThumbnail(jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain, jp.co.ctc_g.eim.framework2.business.domain.entity.FileDomain)
	 */
	public Boolean checkTargetConvertForThumbnail(ObjectDomain object, FileDomain file) throws Exception {

		String ext = StringUtils.getFileExt(file.getName());

		if (ext != null) {
			ext = ext.replace(".", "");
		}

		// Thumbnail変換用
		String thumbnailFileType = EIMConfig.get("THUMBNAIL_FILE_TYPE");
		if(thumbnailFileType == null) {
			return false;

		}
		String[] thumbnailFileTypeList = thumbnailFileType.split(",");

		for (String fileType : thumbnailFileTypeList) {
			fileType = fileType.trim();
			if (fileType.equals(ext)) {
				return true;
			}
		}

		return false;

	}

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.ConvertDocumentService#checkTargetConvertForPreview(jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain, jp.co.ctc_g.eim.framework2.business.domain.entity.FileDomain)
	 */
	public Boolean checkTargetConvertForPreview(ObjectDomain object, FileDomain file) throws Exception {

		String ext = StringUtils.getFileExt(file.getName());

		if (ext != null) {
			ext = ext.replace(".", "");
		}

		String previewFileType = EIMConfig.get("PREVIEW_FILE_TYPE");
		if(previewFileType == null) {
			return false;

		}
		String[] previewFileTypeList = previewFileType.split(",");

		for (String fileType : previewFileTypeList) {
			fileType = fileType.trim();
			if (fileType.equals(ext)) {
				return true;
			}
		}

		return false;

	}

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.ConvertDocumentService#clearFormatForThumbnail(jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain)
	 */
	public void clearFormatForThumbnail(ObjectDomain object) throws Exception {
		String targetAttTypeName = EIMConfig.get("ATTR_NAME_NOT_ALLOWED_CONVERSION_THUMBNAIL");
		String formatName = EIMConfig.get("FORMAT_NAME_THUMBNAIL");

		clearFormat(object, targetAttTypeName, formatName);
	}

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.ConvertDocumentService#clearFormatForPreview(jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain)
	 */
	public void clearFormatForPreview(ObjectDomain object) throws Exception {
		String targetAttTypeName = EIMConfig.get("ATTR_NAME_NOT_ALLOWED_CONVERSION_PREVIEW");
		String formatName = EIMConfig.get("FORMAT_NAME_PREVIEW");

		clearFormat(object, targetAttTypeName, formatName);
	}

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.ConvertDocumentService#checkAllowedConversionForThumbnail(jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain)
	 */
	public Boolean checkAllowedConversionForThumbnail(ObjectDomain object) throws Exception {
		String targetAttTypeName = EIMConfig.get("ATTR_NAME_NOT_ALLOWED_CONVERSION_THUMBNAIL");
		return checkConvertTarget(object, targetAttTypeName);
	}

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.ConvertDocumentService#checkAllowedConversionForPreview(jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain)
	 */
	public Boolean checkAllowedConversionForPreview(ObjectDomain object) throws Exception {

		String targetAttTypeName = EIMConfig.get("ATTR_NAME_NOT_ALLOWED_CONVERSION_PREVIEW");
		return checkConvertTarget(object, targetAttTypeName);

	}

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.ConvertDocumentService#convertDocumentForThumbnail(jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain, jp.co.ctc_g.eim.framework2.business.domain.entity.FileDomain)
	 */
	private Boolean doConvertDocumentForThumbnail(ObjectDomain object, FileDomain file, Boolean isSync) throws Exception {
		try {
			if (isSync) {
				// ファイル変換同期実行
				CompletableFuture<Boolean> future = convertDocumentAsyncService.convertDocumentForThumbnail(object, file);
		        future.join();
			} else {
				// ファイル変換非同期実行 (呼び出し元で引数が書き換えられる可能性があるので複製する)
				convertDocumentAsyncService.convertDocumentForThumbnail(object.clone(), file.clone());
			}

			return true;
		} catch (RejectedExecutionException e) {
			// タスクが実行対象として受け入れ不可能な場合
			log.warn(String.format("ファイル変換処理待ちキューの上限を超過したため追加が拒否されました。[id: %d]", object.getId()));
			return false;
		}
	}

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.ConvertDocumentService#convertDocumentForPreview(jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain, jp.co.ctc_g.eim.framework2.business.domain.entity.FileDomain)
	 */
	private Boolean doConvertDocumentForPreview(ObjectDomain object, FileDomain file, Boolean isSync) throws Exception {
		try {
			if (isSync) {
				// ファイル変換同期実行
				CompletableFuture<Boolean> future = convertDocumentAsyncService.convertDocumentForPreview(object, file);
		        future.join();
			} else {
				// ファイル変換非同期実行 (呼び出し元で引数が書き換えられる可能性があるので複製する)
				convertDocumentAsyncService.convertDocumentForPreview(object.clone(), file.clone());
			}
			return true;
		} catch (RejectedExecutionException e) {
			// タスクが実行対象として受け入れ不可能な場合
			log.warn(String.format("ファイル変換処理待ちキューの上限を超過したため追加が拒否されました。[id: %d]", object.getId()));
			return false;
		}
	}
	/**
	 * Covnert対象かチェック
	 * @param object
	 * @param targetAttTypeName
	 * @return
	 * @throws Exception
	 */
	private Boolean checkConvertTarget(ObjectDomain object, String targetAttTypeName) throws Exception {

		AttributeDomain notAllowedConversion = object.getAttribute(targetAttTypeName);

		if (notAllowedConversion != null && notAllowedConversion.getLong() == 1) {
	        return false;
		}

		return true;

	}

	/**
	 * 対象のフォーマットを削除する
	 * @param object
	 * @param targetAttTypeName
	 * @param formatName
	 * @throws Exception
	 */
	private void clearFormat(ObjectDomain object, String targetAttTypeName, String formatName) throws Exception {

		AttributeTypeDomain attributeType = new AttributeTypeDomain(targetAttTypeName);
		FormatDomain format = new FormatDomain(formatName);

		// 不要なファイルは削除しコンバート対象とする
		deleteCurrentDocument(object, format);

		// 処理対象に戻す
		objectService.setAttributeSingleLong(object, attributeType, 0);
	}

	/**
	 * 前のドキュメントを削除する
	 * @param object
	 * @param exportFormat
	 * @throws Exception
	 */
	private void deleteCurrentDocument(ObjectDomain object, FormatDomain exportFormat) throws Exception {
		FileDomain file = new FileDomain();
		file.setFormat(exportFormat);

		fileDao.delete(object, file);
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
	 * ファイル変換非同期サービスを設定します。
	 * @param convertDocumentAsyncService
	 */
	public void setConvertDocumentAsyncService(ConvertDocumentAsyncService convertDocumentAsyncService) {
		this.convertDocumentAsyncService = convertDocumentAsyncService;
	}

}
