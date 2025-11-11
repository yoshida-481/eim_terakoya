package jp.co.ctc_g.eim.app.document.presentation.controller;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import jp.co.ctc_g.eim.app.document.business.service.DocumentWebDAVService;
import jp.co.ctc_g.eim.app.form.presentation.controller.RestController;

/**
 * WebDAVコントロールクラス
 * Spring MVC のコントローラクラスです。
 * @since Ver6.8
 */
@Controller
@RequestMapping(value = "/rest/webDAV")
public class WebDAVController extends RestController {

	/** webDAVに関する操作を行うサービス */
	private DocumentWebDAVService webDAVService;


	/**
	 * 編集可否を取得します。<br>
	 * 本メソッドは{@link jp.co.ctc_g.eim.app.document.business.service.DocumentWebDAVService#checkStatus(long id) Service}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.app.document.business.service.DocumentWebDAVService#checkStatus(long id,)
	 * @since Ver6.8
	 */
	@RequestMapping(value = "/checkStatus", method=RequestMethod.POST)
	@ResponseBody
	public void checkStatus(@RequestParam("id") long id) throws Exception {
		webDAVService.checkStatus(id);
	}

	/**
	 * webDAVに関する操作を行うサービスを取得します。
	 * @return webDAVに関する操作を行うサービス
	 */
	public DocumentWebDAVService getWebDAVService() {
		return webDAVService;
	}

	/**
	 * webDAVに関する操作を行うサービスを設定します。
	 * @param webDAVService webDAVに関する操作を行うサービス
	 */
	public void setWebDAVService(DocumentWebDAVService webDAVService) {
		this.webDAVService = webDAVService;
	}


}
