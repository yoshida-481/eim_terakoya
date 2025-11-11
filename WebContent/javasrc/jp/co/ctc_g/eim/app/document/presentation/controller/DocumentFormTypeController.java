package jp.co.ctc_g.eim.app.document.presentation.controller;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import jp.co.ctc_g.eim.app.document.business.service.DocumentFormTypeService;
import jp.co.ctc_g.eim.app.form.business.domain.FormTypeDomain;
import jp.co.ctc_g.eim.app.form.presentation.controller.RestController;

/**
 * ドキュメントフォームタイプコントロールクラス
 * Spring MVC のコントローラクラスです。
 * @since Ver6.8
 */
@Controller
@RequestMapping(value = "/rest/documentformtype")
public class DocumentFormTypeController extends DocumentRestController {
	
	/** ドキュメントフォームタイプに関する操作を行うサービス */
	private DocumentFormTypeService documentFormTypeService;
	
	/**
	 * ドキュメントフォームタイプを取得します。<br>
	 * 本メソッドは{@link  jp.co.ctc_g.eim.app.document.business.service.DocumentFormTypeService#getByIdAndParent(long id, long parentId) Service}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.app.document.business.service.DocumentFormTypeService#getByIdAndParent(long id, long parentId)
	 * @since Ver6.8
	 */
	@RequestMapping(value = "/getByIdAndParent", method=RequestMethod.GET)
	@ResponseBody
	public FormTypeDomain getByIdAndParent(@RequestParam("id") long id, @RequestParam("parentId") long parentId) throws Exception {
		FormTypeDomain formType = null;
		try{
			
			formType = documentFormTypeService.getByIdAndParent(id, parentId);
			
		} finally {
			clearFlexContext();
		}
		
		return formType;
	}

	/**
	 * ドキュメントフォームタイプに関する操作を行うサービスを取得します。
	 * @return ドキュメントフォームタイプに関する操作を行うサービス
	 */
	public DocumentFormTypeService getDocumentFormTypeService() {
		return documentFormTypeService;
	}

	/**
	 * ドキュメントフォームタイプに関する操作を行うサービスを設定します。
	 * @param documentFormTypeService ドキュメントフォームタイプに関する操作を行うサービス
	 */
	public void setDocumentFormTypeService(DocumentFormTypeService documentFormTypeService) {
		this.documentFormTypeService = documentFormTypeService;
	}
	
}
