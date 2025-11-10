package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.util.Base64;
import java.util.HashMap;
import java.util.Map;

import javax.imageio.ImageIO;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.fasterxml.jackson.databind.ObjectMapper;

import eim.util.EIMConfig;
import jp.co.ctc_g.eim.app.document.business.service.ConvertDocumentService;
import jp.co.ctc_g.eim.app.document.business.service.ThumbnailPreviewService;
import jp.co.ctc_g.eim.framework2.business.dao.FileDao;
import jp.co.ctc_g.eim.framework2.business.domain.entity.FileDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.FormatDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.service.ObjectService;
import jp.co.ctc_g.eim.framework2.common.exception.EIMException;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;
import jp.co.ctc_g.eim.framework2.integration.util.internal.FileUtil;

/**
 * サムネイル・プレビュー用のビジネスサービス実装クラスです。
 * @since Ver 6.18
 */
public class ThumbnailPreviewServiceImpl implements ThumbnailPreviewService {

	private final Log log = LogFactory.getLog(ThumbnailPreviewServiceImpl.class);

	/** オブジェクトサービス */
	private ObjectService objectService;
	/** ファイルDao */
	private FileDao fileDao;
	/** ドキュメント変換サービス */
	private ConvertDocumentService convertDocumentService;

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.ThumbnailPreviewService#getImage(long)
	 */
	public String getImage(long id) throws Exception {

		ObjectDomain documentDomain = this.objectService.getById(id);
		if (documentDomain == null) {
			throw new EIMException("EIM.ERROR.LOGIC.NODOCUMENT");
		}

		// Format
		FormatDomain format = new FormatDomain(ConfigUtils.getByKey("FORMAT_NAME_THUMBNAIL"));

		// コンバートが許可されているかチェック
		if (!convertDocumentService.checkAllowedConversionForThumbnail(documentDomain)) {
			return "";
		}

		// File
		FileDomain file = fileDao.getByObjectAndFormat(documentDomain, format);
		if (file == null) {

			FormatDomain alwaysFormat = new FormatDomain(ConfigUtils.getByKey("FORMAT_NAME_ALWAYS"));
			FileDomain alwaysFile = fileDao.getByObjectAndFormat(documentDomain, alwaysFormat);
			if (alwaysFile == null) {
				throw new EIMException("EIM.ERROR.LOGIC.NODOCUMENT");
			}

			// Substance
			File substance = new File(FileUtil.getFilePath(documentDomain, alwaysFile));
			if (!substance.exists()) {
				// 実体ファイルが存在しない場合は例外にせず空を返却
				if (log.isDebugEnabled()) {
					log.debug(String.format("The original entity file does not exist.[id: %d]", id));
				}
				return "";
			}

			// Thumbnail変換
			if (convertDocumentService.checkTargetConvertForThumbnail(documentDomain, alwaysFile)) {
				// 非同期で変換処理を実行させる
		        convertDocumentService.convertDocumentForThumbnail(documentDomain, alwaysFile);

				int retryCount = 0;
				String retryCountConfig = EIMConfig.get("REQUEST_CREATE_THUMBNAIL_RETRY_COUNT");
				if (retryCountConfig != null && !retryCountConfig.isEmpty()) {
					retryCount = Integer.parseInt(retryCountConfig);
				}

				int count = 0;
				while (count <= retryCount && file == null) {
					// Thumnnailが登録されるまでポーリング
					Thread.sleep(1000);
					file = fileDao.getByObjectAndFormat(documentDomain, format);
					count++;
				}
				if (file == null) {
			        // リトライしてもファイルが存在しない場合(変換中または変換失敗)は空を返却
					log.info(String.format("Timeout waiting for thumbnail file conversion.[id: %d]", id));
					return "";
				}
			} else {
				return "";
			}
		}

		// サムネイルファイルをjson文字列に変換して返却
		String inputPath = file.getDirectory().getPath() + file.getName();

		File tempInputFile = new File(inputPath);

		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		ImageIO.write(ImageIO.read(tempInputFile), "jpg", baos);

		byte[] valueArray = baos.toByteArray();
		// Base64エンコード
		Base64.Encoder encoder = Base64.getEncoder();
		String encoded = encoder.encodeToString(valueArray);
		// json変換
		ObjectMapper objectMapper = new ObjectMapper();
		Map<String, String> map = new HashMap<>();
		map.put("value", encoded);
		return objectMapper.writeValueAsString(map);
	}

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.ThumbnailPreviewService#checkExistsPdf(long)
	 */
	public Boolean checkExistsPdf(long id) throws Exception {

		ObjectDomain documentDomain = this.objectService.getById(id);
		if (documentDomain == null) {
			throw new EIMException("EIM.ERROR.LOGIC.NODOCUMENT");
		}

		// Format
		FormatDomain format = new FormatDomain(ConfigUtils.getByKey("FORMAT_NAME_PREVIEW"));

		// コンバートが許可されているかチェック
		if (!convertDocumentService.checkAllowedConversionForPreview(documentDomain)) {
			return false;
		}

		// File
		FileDomain file = fileDao.getByObjectAndFormat(documentDomain, format);
		if (file == null) {

			FormatDomain alwaysFormat = new FormatDomain(ConfigUtils.getByKey("FORMAT_NAME_ALWAYS"));
			FileDomain alwaysFile = fileDao.getByObjectAndFormat(documentDomain, alwaysFormat);
			if (alwaysFile == null) {
				throw new EIMException("EIM.ERROR.LOGIC.NODOCUMENT");
			}

			// Substance
			FileUtil.prepareFileAccess(documentDomain, alwaysFile);
			File substance = new File(FileUtil.getFilePath(documentDomain, alwaysFile));
			if (!substance.exists()) {
				throw new EIMException("EIM.ERROR.LOGIC.NODOCUMENT");
			}

			// Preview変換
			if (convertDocumentService.checkTargetConvertForPreview(documentDomain, alwaysFile)) {
				// 非同期で変換処理を実行させる
				convertDocumentService.convertDocumentForPreview(documentDomain, alwaysFile);

				int retryCount = 0;
				String retryCountConfig = EIMConfig.get("REQUEST_CREATE_PDF_RETRY_COUNT");
				if (retryCountConfig != null && !retryCountConfig.isEmpty()) {
					retryCount = Integer.parseInt(retryCountConfig);
				}

		        int count = 0;
		        while (count <= retryCount && file == null) {
					// Previewが登録されるまでポーリング
					Thread.sleep(1000);
		        	file = fileDao.getByObjectAndFormat(documentDomain, format);
					count++;
		        }
				if (file == null) {
			        // リトライしてもファイルが存在しない場合(変換中または変換失敗)はfalseを返却
					log.info(String.format("Timeout waiting for preview file conversion.[id: %d]", id));
					return false;
				}
			} else {
				return false;
			}
		}
		return true;
	}

	public void setObjectService(ObjectService objectService) {
		this.objectService = objectService;
	}

	public void setFileDao(FileDao fileDao) {
		this.fileDao = fileDao;
	}

	public void setConvertDocumentService(ConvertDocumentService convertDocumentService) {
		this.convertDocumentService = convertDocumentService;
	}

}