package jp.co.ctc_g.eim.app.document.presentation.controller;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import jp.co.ctc_g.eim.app.document.business.service.ThumbnailPreviewService;
import jp.co.ctc_g.eim.framework2.presentation.web.controller.RestController;

/**
 * サムネイル・プレビュー用のコントロールクラス
 * Spring MVC のコントローラクラスです。
 * @since Ver 6.18
 */
@Controller
@RequestMapping(value = "/rest/thumbnailpreview")
public class ThumbnailPreviewController extends RestController {

	private final Log log = LogFactory.getLog(ThumbnailPreviewController.class);

	/** ドキュメントサムネイルに関する操作を行うサービス */
	private ThumbnailPreviewService thumbnailPreviewService;

	/**
	 * ドキュメントサムネイルを取得します。<br>
	 * 本メソッドは{@link  jp.co.ctc_g.eim.app.document.business.service.ThumbnailPreviewService#getImage(long id) Service}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.app.document.business.service.ThumbnailPreviewService#getImage(long id)
	 * @since VerX.X
	 */
	@RequestMapping(value = "/getImage", method=RequestMethod.GET)
	@ResponseBody
	public String getImage(@RequestParam("id") long id) {
		String jsonString;

		try{
			jsonString = thumbnailPreviewService.getImage(id);
		} catch (Exception e) {
			log.warn("Failed to get thumbnail image.", e);
			jsonString = "";
		}
		return jsonString;
	}

	@RequestMapping(value = "/checkExistsPdf", method=RequestMethod.GET)
	@ResponseBody
	public Boolean checkExistsPdf(@RequestParam("id") long id) throws Exception {
		return thumbnailPreviewService.checkExistsPdf(id);
	}

	/**
	 * ドキュメントサムネイルに関する操作を行うサービスを取得します。
	 * @return ドキュメントサムネイルに関する操作を行うサービス
	 */
	public ThumbnailPreviewService getThumbnailPreviewService() {
		return thumbnailPreviewService;
	}

	/**
	 * ドキュメントサムネイルに関する操作を行うサービスを設定します。
	 * @param documentFormTypeService ドキュメントサムネイルに関する操作を行うサービス
	 */
	public void setThumbnailPreviewService(ThumbnailPreviewService thumbnailPreviewService) {
		this.thumbnailPreviewService = thumbnailPreviewService;
	}

}
