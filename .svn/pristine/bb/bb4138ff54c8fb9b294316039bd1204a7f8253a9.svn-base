package jp.co.ctc_g.eim.admin.presentation.web.controller;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import jp.co.ctc_g.eim.admin.presentation.web.form.CodeCreateForm;
import jp.co.ctc_g.eim.admin.presentation.web.form.CodeForm;
import jp.co.ctc_g.eim.admin.presentation.web.form.CodeTypeForm;
import jp.co.ctc_g.eim.app.form.presentation.controller.RestController;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.CodeCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.CodeTypeAndValueCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.MultipleCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.entity.CodeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.CodeTypeDomain;
import jp.co.ctc_g.eim.framework2.business.service.CodeService;

/**
 * コードコントロールクラス
 * Spring MVC のコントローラクラスです。
 * @since Ver6.8
 */
@Controller
@RequestMapping(value = "/rest/code")
public class CodeController extends RestController {

	/** コードに関する操作を行うサービス */
	private CodeService codeService;

	/**
	 * コードオブジェクトを作成します。<br>
	 * 本メソッドは{@link  jp.co.ctc_g.eim.app.document.business.service.CodeService#create(CodeTypeDomain codeType, CodeDomain code) Service}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.app.document.business.service.CodeService#create(CodeTypeDomain codeType, CodeDomain code)
	 * @since Ver6.8
	 */
	@RequestMapping(value = "/create", method=RequestMethod.POST)
	@ResponseBody
	public CodeDomain create(@RequestBody CodeCreateForm codeCreate) throws Exception {

		CodeTypeForm codeType = codeCreate.getCodeType();
		CodeForm code = codeCreate.getCode();
		CodeDomain newCode = codeService.create(codeType.convert(new CodeTypeDomain()), code.convert(new CodeDomain()));
		return newCode;
	}

	/**
	 * コードオブジェクトを更新します。<br>
	 * 本メソッドは{@link  jp.co.ctc_g.eim.app.document.business.service.CodeService#update(CodeDomain form) Service}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.app.document.business.service.CodeService#update(CodeDomain form)
	 * @since Ver6.8
	 */
	@RequestMapping(value = "/update", method=RequestMethod.POST)
	@ResponseBody
	public void update(@RequestBody CodeForm dto) throws Exception {
		codeService.update(dto.convert(new CodeDomain()));
	}

	/**
	 * コードオブジェクトを削除します。<br>
	 * 本メソッドは{@link  jp.co.ctc_g.eim.app.document.business.service.CodeService#delete(CodeDomain form) Service}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.app.document.business.service.CodeService#delete(CodeDomain form)
	 * @since Ver6.8
	 */
	@RequestMapping(value = "/delete", method=RequestMethod.POST)
	@ResponseBody
	public void delete(@RequestBody CodeForm dto) throws Exception {
		codeService.delete(dto.convert(new CodeDomain()));
	}

	/**
	 * コードオブジェクトを取得します。<br>
	 * 本メソッドは{@link  jp.co.ctc_g.eim.app.document.business.service.CodeService#getById(long id) Service}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.app.document.business.service.CodeService#getById(long id)
	 * @since Ver6.8
	 */
	@RequestMapping(value = "/getById", method=RequestMethod.POST)
	@ResponseBody
	public CodeDomain getById(@RequestParam("id") long id) throws Exception {
		CodeDomain code = codeService.getById(id);
		return code;
	}

	/**
	 * コードオブジェクトを取得します。<br>
	 * 本メソッドは{@link  jp.co.ctc_g.eim.app.document.business.service.CodeService#getById(long id) Service}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.app.document.business.service.CodeService#getById(long id)
	 * @since Ver6.8
	 */
	@RequestMapping(value = "/getList", method=RequestMethod.POST)
	@ResponseBody
	public List<CodeDomain> getList(@RequestBody CodeCriteria criteria) throws Exception {
		List<CodeDomain> codeList = codeService.getList(criteria);
		return codeList;
	}

	/**
	 * コードオブジェクトに名称を設定します。<br>
	 * 本メソッドは{@link  jp.co.ctc_g.eim.app.document.business.service.CodeService#setOtherName(CodeDomain form, String otherName1, String otherName2) Service}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.app.document.business.service.CodeService#setOtherName(CodeDomain form, String otherName1, String otherName2)
	 * @since Ver6.8
	 */
	@RequestMapping(value = "/setOtherName", method=RequestMethod.POST)
	@ResponseBody
	public void setOtherName(@RequestBody CodeCreateForm codeCreate) throws Exception {
		CodeForm code = codeCreate.getCode();
		String otherName1 = codeCreate.getOtherName1();
		String otherName2 = codeCreate.getOtherName2();
		codeService.setOtherName(code.convert(new CodeDomain()), otherName1, otherName2);
	}

	/**
	 * コードオブジェクトに名称を設定します。<br>
	 * 本メソッドは{@link  jp.co.ctc_g.eim.app.document.business.service.CodeService#removeOtherName(CodeDomain form, String otherName) Service}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.app.document.business.service.CodeService#removeOtherName(CodeDomain form, String otherName)
	 * @since Ver6.8
	 */
	@RequestMapping(value = "/removeOtherName", method=RequestMethod.POST)
	@ResponseBody
	public void removeOtherName(@RequestBody CodeCreateForm codeCreate) throws Exception {
		CodeForm code = codeCreate.getCode();
		String otherName1 = codeCreate.getOtherName1();
		codeService.removeOtherName(code.convert(new CodeDomain()), otherName1);
	}

	/**
	 * 複数のIDからコードタイプマップを取得します。<br>
	 * 本メソッドは{@link  jp.co.ctc_g.eim.app.document.business.service.CodeService#getListMapCodeTypeIds(MultipleCriteria<Long> criteria) Service}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.app.document.business.service.CodeService#getListMapCodeTypeIds(MultipleCriteria<Long> criteria)
	 * @since Ver6.8
	 */
	@RequestMapping(value = "/getListMapCodeTypeIds", method=RequestMethod.POST)
	@ResponseBody
	public Map<Long, List<CodeDomain>> getListMapCodeTypeIds(@RequestBody MultipleCriteria<Long> criteria) throws Exception {
		return codeService.getListMapCodeTypeIds(criteria);
	}

	/**
	 * 複数のタイプからコードタイプIDマップを取得します。<br>
	 * 本メソッドは{@link  jp.co.ctc_g.eim.app.document.business.service.CodeService#getListMapCodeTypeIdsByTypeAndValue(MultipleCriteria<CodeTypeAndValueCriteria> criteria) Service}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.app.document.business.service.CodeService#getListMapCodeTypeIdsByTypeAndValue(MultipleCriteria<CodeTypeAndValueCriteria> criteria)
	 * @since Ver6.8
	 */
	@RequestMapping(value = "/getListMapCodeTypeIdsByTypeAndValue", method=RequestMethod.POST)
	@ResponseBody
	public Map<Long, List<CodeDomain>> getListMapCodeTypeIdsByTypeAndValue(@RequestBody MultipleCriteria<CodeTypeAndValueCriteria> criteria) throws Exception {
		return codeService.getListMapCodeTypeIdsByTypeAndValue(criteria);
	}

	/**
	 * コードを並び替えます。<br>
	 * 本メソッドは{@link  jp.co.ctc_g.eim.app.document.business.service.CodeService#sortSequence(CodeTypeDomain form, List<CodeDomain> codeList) Service}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.app.document.business.service.CodeService#sortSequence(CodeTypeDomain form, List<CodeDomain> codeList)
	 * @since Ver6.8
	 */
	@RequestMapping(value = "/sortSequence", method=RequestMethod.POST)
	@ResponseBody
	public void sortSequence(@RequestBody CodeCreateForm codeCreate) throws Exception {
		CodeTypeForm codeType = codeCreate.getCodeType();
		List<CodeForm> codeList = codeCreate.getCodeList();
		List<CodeDomain> codeDomainList = new ArrayList<CodeDomain>();
		for (int i = 0; i < codeList.size(); i++) {
			codeDomainList.add(codeList.get(i).convert(new CodeDomain()));
		}
		codeService.sortSequence(codeType.convert(new CodeTypeDomain()), codeDomainList);
	}

	/**
	 * コードに関する操作を行うサービスを取得します。
	 * @return コードに関する操作を行うサービス
	 */
	public CodeService getCodeService() {
		return codeService;
	}

	/**
	 * コードに関する操作を行うサービスを設定します。
	 * @param codeService コードに関する操作を行うサービス
	 */
	public void setCodeService(CodeService codeService) {
		this.codeService = codeService;
	}

}
