package jp.co.ctc_g.eim.app.document.common.aop.advice;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import jp.co.ctc_g.eim.app.document.business.service.ConvertDocumentService;
import jp.co.ctc_g.eim.framework2.business.domain.entity.FileDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.FormatDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.service.FormatService;
import jp.co.ctc_g.eim.framework2.business.service.ObjectService;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;

/**
 * ファイル変換アドバイスクラス
 */
public class ConvertDocumentServiceAdvice {

	/** Logger */
	@SuppressWarnings("unused")
	private static final Log log = LogFactory.getLog(ConvertDocumentServiceAdvice.class);

	/** ドキュメント変換サービス */
	private ConvertDocumentService convertDocumentService;
	/** オブジェクトサービス */
	private ObjectService objectService;
	/** フォーマットサービス */
	private FormatService formatService = null;

	/**
	 * FileDaoのCreate処理の後に実行
	 * @param object
	 * @param file
	 * @throws Exception
	 */
	public void afterCreateAdvice(ObjectDomain object, FileDomain file) throws Exception {
		afterAdvice(object, file);
	}

	/**
	 * FileDaoのUpdate処理の後に実行
	 * @param object
	 * @param file
	 * @throws Exception
	 */
	public void afterUpdateAdvice(ObjectDomain object, FileDomain file) throws Exception {
		afterAdvice(object, file);
	}

	/**
	 * ThumbnailとPreview変換の実処理
	 * @param object
	 * @param file
	 * @throws Exception
	 */
	private void afterAdvice(ObjectDomain object, FileDomain file) throws Exception {

		FormatDomain format = formatService.getById(file.getFormat().getId());

		// Formatが原本ドキュメント時に実行する
		if (format.getDefinitionName().equals(ConfigUtils.getByKey("FORMAT_NAME_ALWAYS"))) {
			file.setFormat(format);
			object = objectService.getById(object.getId());

			// 前回のフォーマットをクリアする
			convertDocumentService.clearFormatForThumbnail(object);
			convertDocumentService.clearFormatForPreview(object);

			// Thumbnail変換
			if (convertDocumentService.checkTargetConvertForThumbnail(object, file)) {
				// 非同期で変換処理を実行させる
				convertDocumentService.convertDocumentForThumbnail(object, file);
			}

			// Preview変換
			if (convertDocumentService.checkTargetConvertForPreview(object, file)) {
				// 非同期で変換処理を実行させる
				convertDocumentService.convertDocumentForPreview(object, file);
			}
		}

	}

	public void setConvertDocumentService(ConvertDocumentService convertDocumentService) {
		this.convertDocumentService = convertDocumentService;
	}

	public void setObjectService(ObjectService objectService) {
		this.objectService = objectService;
	}

	public void setFormatService(FormatService formatService) {
		this.formatService = formatService;
	}
}
