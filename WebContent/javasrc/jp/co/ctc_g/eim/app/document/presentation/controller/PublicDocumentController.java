package jp.co.ctc_g.eim.app.document.presentation.controller;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.multipart.MultipartFile;

import common.util.AppMessageUtils;
import eim.bo.EIMResource;
import jp.co.ctc_g.eim.app.document.business.service.PublicDocumentService;
import jp.co.ctc_g.eim.app.document.common.util.StringUtils;
import jp.co.ctc_g.eim.app.document.presentation.form.PublicDocumentForm;
import jp.co.ctc_g.eim.app.form.presentation.controller.RestController;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.service.ObjectService;
import jp.co.ctc_g.eim.framework2.business.service.ObjectTypeService;
import jp.co.ctc_g.eim.framework2.common.exception.EIMException;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;
import jp.co.ctc_g.eim.framework2.common.util.ResourceUtils;

/**
 * ドキュメントフォームコントロールクラス
 * Spring MVC のコントローラクラスです。
 * @since Ver6.8
 */
@Controller
@RequestMapping(value = "/rest/public-document")
public class PublicDocumentController extends RestController {

	/** ロガー */
	private Log logger = LogFactory.getLog(this.getClass().getName());

	/** PDF変換オブジェクトを作成するサービス */
	private PublicDocumentService publicDocumentService;

	/** オブジェクトタイプサービス */
	private ObjectTypeService objectTypeService;

	/** オブジェクトサービス */
	private ObjectService objectService;
	
	/**
	 * PDF変換オブジェクトを作成します<br>
	 *
	 * @param id オブジェクトID
	 * @throws Exception 例外処理
	 * @since Ver 6.13
	 */
	@RequestMapping(value = "/create_async", method=RequestMethod.POST)
	@ResponseBody
	public void createAsync(@RequestBody PublicDocumentForm ids) throws Exception {

		// 変換対象IDのリストを取得する
		long[] idList = ids.getIds();

		// PDF変換オブジェクトを作成
		for(int i = 0; i<idList.length; i++){
			ObjectDomain object = new ObjectDomain(idList[i]);
			publicDocumentService.createAsync(object);
		}

	}

	/**
	 * 公開PDFを事前登録します<br>
	 *
	 * @param id オブジェクトID
	 * @throws Exception 例外処理
	 * @since Ver 6.16
	 */
	@RequestMapping(value = "/preRegistPublicPdf", method=RequestMethod.POST, produces="text/plain;charset=UTF-8")
	@ResponseBody
	public String preRegistPublicPdf(@RequestParam long objId, @RequestParam("file") MultipartFile file) throws Exception {
		
		if(file.getSize() == 0){
			// ファイルサイズ0
			String message = ResourceUtils.getByKey("EIM.ERROR.LOGIC.NOUPLOADFILE");
			logger.error(message);
			return AppMessageUtils.makeErrorTagByMessage(message);
		}
		
		// ファイルサイズ判定(最大ファイルサイズ)
		long maxFileSize = Long.parseLong(ConfigUtils.getByKey("UPLOAD_FILE_SIZE_MAX"));
		if (file.getInputStream().available() > maxFileSize) {
			String message = ResourceUtils.getByKey("EIM.ERROR.LOGIC.UPLOAD.FILE.SIZE.OVER", maxFileSize);
			logger.error(message);
			return AppMessageUtils.makeErrorTagByMessage(message);
		}

		// 拡張子チェック
		String ext = StringUtils.getFileExt(file.getOriginalFilename());
		if (!ext.equals(".pdf")) {
			String message = ResourceUtils.getByKey("EIM.ERROR.INPUT.DIFFERENT.EXT");
			logger.error(message);
			return AppMessageUtils.makeErrorTagByMessage(message);
		}

		// オブジェクト取得
		ObjectDomain object = objectService.getById(objId);
		if (object == null) {
			String message = ResourceUtils.getByKey("EIM.ERROR.LOGIC.NODOCUMENT");
			logger.error(message);
			return AppMessageUtils.makeErrorTagByMessage(message);
		}
		
		try {
			publicDocumentService.preRegistPublicPdf(object, file);
		} catch (EIMException e) {
			logger.error(e.getMessage(), e);
			return AppMessageUtils.makeErrorTagByMessage(e.getMessage());
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			return AppMessageUtils.makeErrorTagByMessage(EIMResource.getMessage("EIM.ERROR.SYSTEMERROR"));
		}
		
		return null;
	}
	
	/**
	 * 事前登録した公開PDFを削除します<br>
	 *
	 * @param id オブジェクトID
	 * @throws Exception 例外処理
	 * @since Ver 6.16
	 */
	@RequestMapping(value = "/deletePreRegistPdf", method=RequestMethod.POST)
	@ResponseBody
	public void deletePreRegistPdf(@RequestBody PublicDocumentForm ids) throws Exception {

		long objId = ids.getIds()[0];

		// オブジェクト取得
		ObjectDomain object = objectService.getById(objId);
		if (object == null) {
			throw new EIMException("EIM.ERROR.LOGIC.NODOCUMENT");
		}
		publicDocumentService.deletePreRegistPdf(object);
	}
	
	/**
	 * PDF変換オブジェクトを作成するサービスを取得します。
	 * @return publicDocumentService
	 */
	public PublicDocumentService getPublicDocumentService() {
		return publicDocumentService;
	}

	/**
	 * PDF変換オブジェクトを作成するサービスを設定します。
	 * @param publicDocumentService
	 */
	public void setPublicDocumentService(PublicDocumentService publicDocumentService) {
		this.publicDocumentService = publicDocumentService;
	}

	/**
	 * @return objectTypeService
	 */
	public ObjectTypeService getObjectTypeService() {
		return objectTypeService;
	}

	/**
	 * @param objectTypeService セットします objectTypeService
	 */
	public void setObjectTypeService(ObjectTypeService objectTypeService) {
		this.objectTypeService = objectTypeService;
	}

	/**
	 * @return objectService
	 */
	public ObjectService getObjectService() {
		return objectService;
	}

	/**
	 * @param objectService セットする objectService
	 */
	public void setObjectService(ObjectService objectService) {
		this.objectService = objectService;
	}
}
