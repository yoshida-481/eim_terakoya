package jp.co.ctc_g.eim.app.document.presentation.controller;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import jp.co.ctc_g.eim.app.document.business.service.CoverDocumentFormService;
import jp.co.ctc_g.eim.app.form.business.domain.FormDomain;
import jp.co.ctc_g.eim.app.form.presentation.controller.RestController;
import jp.co.ctc_g.eim.app.form.presentation.form.FormForm;

/**
 * ドキュメントフォームコントロールクラス
 * Spring MVC のコントローラクラスです。
 * @since Ver6.8
 */
@Controller
@RequestMapping(value = "/rest/coverdocumentform")
public class CoverDocumentFormController extends RestController {

	/** 表紙ドキュメントフォームに関する操作を行うサービス */
	private CoverDocumentFormService coverDocumentFormService;

	/**
	 * 表紙ドキュメントフォームオブジェクトを作成します。<br>
	 * 本メソッドは{@link  jp.co.ctc_g.eim.app.document.business.service.CoverDocumentFormService#createCover(FormDomain form) Service}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.app.document.business.service.CoverDocumentFormService#createCover(FormDomain form)
	 * @since Ver6.8
	 */
	@RequestMapping(value = "/createCover", method=RequestMethod.POST)
	@ResponseBody
	public FormDomain createCover(@RequestBody FormForm dto) throws Exception {

		FormDomain newForm = coverDocumentFormService.createCover(dto.convert(new FormDomain()));
		return newForm;
	}

	/**
	 * ドキュメントフォームに関する操作を行うサービスを取得します。
	 * @return ドキュメントフォームに関する操作を行うサービス
	 */
	public CoverDocumentFormService getCoverDocumentFormService() {
		return coverDocumentFormService;
	}

	/**
	 * ドキュメントフォームに関する操作を行うサービスを設定します。
	 * @param documentFormService ドキュメントフォームに関する操作を行うサービス
	 */
	public void setCoverDocumentFormService(CoverDocumentFormService coverDocumentFormService) {
		this.coverDocumentFormService = coverDocumentFormService;
	}
}
