package jp.co.ctc_g.eim.admin.presentation.web.controller;

import java.util.List;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import jp.co.ctc_g.eim.admin.presentation.web.form.CodeTypeCopyForm;
import jp.co.ctc_g.eim.admin.presentation.web.form.CodeTypeForm;
import jp.co.ctc_g.eim.app.form.presentation.controller.RestController;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.CodeTypeCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.entity.CodeTypeDomain;
import jp.co.ctc_g.eim.framework2.business.service.CodeTypeService;

/**
 * コードタイプコントロールクラス
 * Spring MVC のコントローラクラスです。
 * @since Ver6.8
 */
@Controller
@RequestMapping(value = "/rest/codeType")
public class CodeTypeController extends RestController {

	/** コードに関する操作を行うサービス */
	private CodeTypeService codeTypeService;

	/**
	 * コードタイプオブジェクトを作成します。<br>
	 * 本メソッドは{@link  jp.co.ctc_g.eim.app.document.business.service.CodeTypeService#create(CodeTypeDomain codeType) Service}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.app.document.business.service.CodeTypeService#create(CodeTypeDomain codeType)
	 * @since Ver6.8
	 */
	@RequestMapping(value = "/create", method=RequestMethod.POST)
	@ResponseBody
	public CodeTypeDomain create(@RequestBody CodeTypeForm codeType) throws Exception {
		CodeTypeDomain newCode = codeTypeService.create(codeType.convert(new CodeTypeDomain()));
		return newCode;
	}

	/**
	 * コードタイプオブジェクトを更新します。<br>
	 * 本メソッドは{@link  jp.co.ctc_g.eim.app.document.business.service.CodeTypeService#update(CodeTypeDomain form) Service}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.app.document.business.service.CodeTypeService#update(CodeTypeDomain form)
	 * @since Ver6.8
	 */
	@RequestMapping(value = "/update", method=RequestMethod.POST)
	@ResponseBody
	public void update(@RequestBody CodeTypeForm dto) throws Exception {
		codeTypeService.update(dto.convert(new CodeTypeDomain()));
	}

	/**
	 * コードタイプオブジェクトを削除します。<br>
	 * 本メソッドは{@link  jp.co.ctc_g.eim.app.document.business.service.CodeTypeService#delete(CodeTypeDomain form) Service}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.app.document.business.service.CodeTypeService#delete(CodeTypeDomain form)
	 * @since Ver6.8
	 */
	@RequestMapping(value = "/delete", method=RequestMethod.POST)
	@ResponseBody
	public void delete(@RequestBody CodeTypeForm dto) throws Exception {
		codeTypeService.delete(dto.convert(new CodeTypeDomain()));
	}

	/**
	 * コードタイプオブジェクトを取得します。<br>
	 * 本メソッドは{@link  jp.co.ctc_g.eim.app.document.business.service.CodeTypeService#getById(long id) Service}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.app.document.business.service.CodeTypeService#getById(long id)
	 * @since Ver6.8
	 */
	@RequestMapping(value = "/getById", method=RequestMethod.POST)
	@ResponseBody
	public CodeTypeDomain getById(@RequestParam("id") long id) throws Exception {
		CodeTypeDomain code = codeTypeService.getById(id);
		return code;
	}

	/**
	 * コードタイプオブジェクトを取得します。<br>
	 * 本メソッドは{@link  jp.co.ctc_g.eim.app.document.business.service.CodeTypeService#getByDefinitionName(long id) Service}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.app.document.business.service.CodeTypeService#getByDefinitionName(long id)
	 * @since Ver6.8
	 */
	@RequestMapping(value = "/getByDefinitionName", method=RequestMethod.POST)
	@ResponseBody
	public CodeTypeDomain getByDefinitionName(@RequestParam("definitionName") String definitionName) throws Exception {
		CodeTypeDomain code = codeTypeService.getByDefinitionName(definitionName);
		return code;
	}

	/**
	 * コードタイプオブジェクト一覧を取得します。<br>
	 * 本メソッドは{@link  jp.co.ctc_g.eim.app.document.business.service.CodeTypeService#getById(long id) Service}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.app.document.business.service.CodeTypeService#getById(long id)
	 * @since Ver6.8
	 */
	@RequestMapping(value = "/getList", method=RequestMethod.POST)
	@ResponseBody
	public List<CodeTypeDomain> getList(@RequestBody CodeTypeCriteria criteria) throws Exception {
		List<CodeTypeDomain> codeList = codeTypeService.getList(criteria);
		return codeList;
	}

	/**
	 * コードタイプオブジェクトをコピーします。<br>
	 * 本メソッドは{@link  jp.co.ctc_g.eim.app.document.business.service.CodeTypeService#copy(CodeTypeDomain form1, CodeTypeDomain form2) Service}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.app.document.business.service.CodeTypeService#copy(CodeTypeDomain form1, CodeTypeDomain form2)
	 * @since Ver6.8
	 */
	@RequestMapping(value = "/copy", method=RequestMethod.POST)
	@ResponseBody
	public CodeTypeDomain copy(@RequestBody CodeTypeCopyForm codeType) throws Exception {
		CodeTypeDomain codeType1 = codeType.getCodeType1().convert(new CodeTypeDomain());
		CodeTypeDomain codeType2 = codeType.getCodeType2().convert(new CodeTypeDomain());
		CodeTypeDomain newCode = codeTypeService.copy(codeType1, codeType2);
		return newCode;
	}


	/**
	 * コードに関する操作を行うサービスを取得します。
	 * @return コードに関する操作を行うサービス
	 */
	public CodeTypeService getCodeTypeService() {
		return codeTypeService;
	}

	/**
	 * コードに関する操作を行うサービスを設定します。
	 * @param codeTypeService コードに関する操作を行うサービス
	 */
	public void setCodeTypeService(CodeTypeService codeTypeService) {
		this.codeTypeService = codeTypeService;
	}

}
