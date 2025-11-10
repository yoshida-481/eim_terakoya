package jp.co.ctc_g.eim.app.document.presentation.batch;

import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import eim.net.EIMSession;
import eim.util.EIMConfig;
import jp.co.ctc_g.eim.app.document.business.service.ConvertDocumentService;
import jp.co.ctc_g.eim.app.document.common.util.AppDocumentLogicUtil;
import jp.co.ctc_g.eim.framework2.business.dao.FileDao;
import jp.co.ctc_g.eim.framework2.business.domain.entity.FileDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.FormatDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchConditionGroup;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchLimitCountCondition;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchSelectObject;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchSelectObject.SearchConditionBuildHelper;
import jp.co.ctc_g.eim.framework2.business.service.FormatService;
import jp.co.ctc_g.eim.framework2.business.service.ObjectService;
import jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.common.context.TransactionContext;
import jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum;
import jp.co.ctc_g.eim.framework2.common.exception.EIMException;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;

/**
 * ThumbnailとPreview用のデータを作成します。<br>
 */
public class ThumbnailPreviewCreator {

	/** ログ */
	private static Log log = LogFactory.getLog(ThumbnailPreviewCreator.class);
	/** TransactionContext */
	private static TransactionContext context = null;
	/** Session */
	private static EIMSession sess = null;
	/** コンテキストファイル */
	private static final String[] contextFiles = { "applicationContext.xml" };
	/** オブジェクトサービス */
	private static ObjectService objectService;
	/** フォーマットサービス */
	private static FormatService formatService;
	/** ファイルDao */
	private static FileDao fileDao;
	/** ドキュメント変換サービス */
	private static ConvertDocumentService convertDocumentService;

	/**
	 * ThumbnailとPreviewの変換処理を実行します。<br>
	 * 変換対象拡張子のファイルかつ変換が未完了データが変換対象です。<br>
	 * <br>
	 * @throws Exception 例外発生
	 * @since Ver 6.6
	 */
	public static void main(String[] args) throws Exception {

		// 開始ログ
		log.info("サムネイルプレビュー変換処理開始");

		try {
			try {
				// コンテキストファイル読み込み
				ApplicationContextLoader.init(contextFiles);
				//トランザクション
				context = new TransactionContext(ConnectionModeEnum.CONSOLE);
				EIMThreadContext.putTransactionContext(context);
				//サービス取得
				init();
			} catch (Exception e) {
				throw new Exception("DB接続でエラーが発生しました。", e);
			}

			// 原本ドキュメントのフォーマット取得
			FormatDomain alwaysFormat = formatService.getByDefinitionName(ConfigUtils.getByKey("FORMAT_NAME_ALWAYS"));
			if (alwaysFormat == null) {
				throw new EIMException("EIM.ERROR.LOGIC.FORMAT.NOTFOUND");
			}

			FormatDomain thumbnailFormat = formatService.getByDefinitionName(ConfigUtils.getByKey("FORMAT_NAME_THUMBNAIL"));
			if (thumbnailFormat == null) {
				throw new EIMException("EIM.ERROR.LOGIC.FORMAT.NOTFOUND");
			}

			FormatDomain previewFormat = formatService.getByDefinitionName(ConfigUtils.getByKey("FORMAT_NAME_PREVIEW"));
			if (previewFormat == null) {
				throw new EIMException("EIM.ERROR.LOGIC.FORMAT.NOTFOUND");
			}

			// 対象のオブジェクトを抽出
			SearchSelectObject searchSelectObject = new SearchSelectObject();
			SearchConditionBuildHelper helper = new SearchConditionBuildHelper();
			SearchConditionGroup searchConditionGroup = helper.group(helper.opAnd());

			// オブジェクトタイプ指定
			ObjectTypeDomain docObjTypeDomain = AppDocumentLogicUtil.getDocumentObjectType();
			List<Long> docObjTypes = AppDocumentLogicUtil.getChildObjectTypeList(docObjTypeDomain.getId());
			searchConditionGroup.addCondition(helper.inArray(helper.opAnd(), SearchSelectObject.PsedoAttributeTypeEnum.TYPE, helper.opIn(), docObjTypes.toArray()));

			searchSelectObject.setCondition(searchConditionGroup);

			//最大取得件数
			SearchLimitCountCondition limitCountCondition = new SearchLimitCountCondition(Integer.MAX_VALUE,false);

			List<ObjectDomain> objectList = objectService.getList(searchSelectObject, limitCountCondition);
			for(int i =0; i < objectList.size();i++) {
				ObjectDomain documentDomain = objectList.get(i);

				// パスの指定が正しい場合
				if (documentDomain.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_PASS")) == null) {
					continue;
				}

				FileDomain file = null;
				// コンバートが許可されているかチェック
				if (convertDocumentService.checkAllowedConversionForThumbnail(documentDomain)) {

					// Thumbnail File
					FileDomain thumbnailFile = fileDao.getByObjectAndFormat(documentDomain, thumbnailFormat);
					if (thumbnailFile == null) {

						// AlwaysFile
						file = fileDao.getByObjectAndFormat(documentDomain, alwaysFormat);
						if (file == null) {
							continue;
						}

						// サムネイル変換
						Boolean result = convertDocumentService.syncConvertDocumentForThumbnail(documentDomain, file);

				        if (result) {
							log.info(documentDomain.getName() + "のサムネイル変換が完了しました。");
				        } else {
							log.info(documentDomain.getName() + "のサムネイル変換が失敗しました。");
				        }
					}
				}

				// コンバートが許可されているかチェック
				if (convertDocumentService.checkAllowedConversionForPreview(documentDomain)) {

					// Preview File
					FileDomain previewFile = fileDao.getByObjectAndFormat(documentDomain, previewFormat);
					if (previewFile == null) {

						// AlwaysFile
						if (file == null) {
							 file = fileDao.getByObjectAndFormat(documentDomain, alwaysFormat);
							if (file == null) {
								continue;
							}
						}

						// プレビュー変換
						Boolean result = convertDocumentService.syncConvertDocumentForPreview(documentDomain, file);

				        if (result) {
							log.info(documentDomain.getName() + "のプレビュー変換が完了しました。");
				        } else {
							log.info(documentDomain.getName() + "のプレビュー変換が失敗しました。");
				        }
					}
				}
			}

		} catch (Exception e) {
			throw new Exception("エラーが発生しました。", e);
		} finally {
			try {
				if (sess != null) {
					sess.close();
				}
			} catch (Exception se) {
				log.warn(se.getMessage(), se);
			}

			// 終了ログ
			log.info("サムネイルプレビュー変換処理終了");
		}

		System.exit(0);
	}

	/**
	 * 初期データの取得を行います
	 * <br>
	 * @throws Exception
	 */
	private static void init() throws Exception {

		objectService = (ObjectService) ApplicationContextLoader.getApplicationContext().getBean("objectService2");
		formatService = (FormatService) ApplicationContextLoader.getApplicationContext().getBean("formatService2");
		fileDao = (FileDao) ApplicationContextLoader.getApplicationContext().getBean("fileDao2");
		convertDocumentService = (ConvertDocumentService) ApplicationContextLoader.getApplicationContext().getBean("convertDocumentService");
	}
}
