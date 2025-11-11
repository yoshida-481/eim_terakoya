package jp.co.ctc_g.eim.admin.presentation.web.controller;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import jp.co.ctc_g.eim.admin.presentation.web.dto.FormTypeDTO;
import jp.co.ctc_g.eim.admin.presentation.web.dto.FormTypeFolderDTO;
import jp.co.ctc_g.eim.admin.presentation.web.dto.FormWorkspaceDTO;
import jp.co.ctc_g.eim.admin.presentation.web.dto.SecurityDTO;
import jp.co.ctc_g.eim.admin.presentation.web.form.criteria.FormTypeCriteriaForm;
import jp.co.ctc_g.eim.admin.presentation.web.form.criteria.FormTypeFolderCriteriaForm;
import jp.co.ctc_g.eim.admin.presentation.web.form.criteria.FormWorkspaceCriteriaForm;
import jp.co.ctc_g.eim.app.document.business.service.DocumentFormService;
import jp.co.ctc_g.eim.app.form.business.domain.FormDomain;
import jp.co.ctc_g.eim.app.form.business.domain.FormTypeDomain;
import jp.co.ctc_g.eim.app.form.business.domain.FormTypeFolderDomain;
import jp.co.ctc_g.eim.app.form.business.domain.FormWorkspaceDomain;
import jp.co.ctc_g.eim.app.form.business.domain.criteria.FormTypeCriteria;
import jp.co.ctc_g.eim.app.form.business.domain.criteria.FormWorkspaceCriteria;
import jp.co.ctc_g.eim.app.form.business.service.FormTypeFolderService;
import jp.co.ctc_g.eim.app.form.business.service.FormTypeService;
import jp.co.ctc_g.eim.app.form.business.service.FormWorkspaceService;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.MultipleCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.LanguageDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.SecurityDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.WorkflowDomain;
import jp.co.ctc_g.eim.framework2.business.service.LanguageService;
import jp.co.ctc_g.eim.framework2.business.service.ObjectTypeService;
import jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.common.context.TransactionContext;


/**
 * 帳票ワークスペースコントロールクラス Spring MVC のコントローラクラスです。
 *
 * @since Ver1.0
 */
@Controller
@RequestMapping({ "/rest/admin/form-workspace" })
public class FormWorkspaceController extends AdminRestController {
	public static final String ATTRIBUTE_TYPE_NAME_OTHER_LANGUAGES	= "app.form.dev:他言語名称";
	public static final String ATTRIBUTE_TYPE_NAME_FORM_WORKSPACE_ID	= "app.form.dev:帳票ワークスペースID";
	public static final String ATTRIBUTE_TYPE_NAME_FORM_TYPE_ID	= "app.form.dev:帳票タイプID";
	public static final String ATTRIBUTE_TYPE_NAME_FORM_TYPE_FOLDER_ID	= "app.form.dev:帳票タイプフォルダID";
	public static final String FORM_TYPE_NAME = "帳票タイプ";

	public static final String OBJECT_TYPE_NAME_FORM_WORKSPACE = "app.form.dev:帳票ワークスペース";
	public static final String OBJECT_TYPE_NAME_FORM_TYPE = "app.form.dev:帳票";
	public static final String OBJECT_TYPE_NAME_FORM_TYPE_FOLDER = "app.form.dev:帳票タイプフォルダ";


	/** ログ */
	private static Log formWsLog = LogFactory.getLog(FormWorkspaceController.class);

	/** ドキュメントフォームに関する操作を行うサービス */
	private DocumentFormService documentFormService;

	/** 帳票ワークスペースサービス */
	private FormWorkspaceService formWorkspaceService;

	/** 帳票ワークスペースサービス */
	private FormWorkspaceService formWorkspaceService2;

	/** 帳票タイプサービス */
	private FormTypeService formTypeService;

	/** 帳票タイプフォルダサービス */
	private FormTypeFolderService formTypeFolderService;

	/** オブジェクトタイプサービス */
	private ObjectTypeService objectTypeService2;

	/** 言語サービス */
	private LanguageService languageService;

	/**
	 * 指定したIDの帳票ワークスペースを取得します。<br>
	 * 本メソッドは{@link jp.co.ctc_g.eim.app.form.business.service.FormWorkspaceService#getById(Long formWorkspaceId)
	 * Service}へ処理を委譲します。
	 *
	 * @see jp.co.ctc_g.eim.app.form.business.service.FormWorkspaceService#getById(FormWorkspaceCriteria
	 *      formWorkspaceCriteria)
	 * @since Ver1.0
	 */
	@RequestMapping(value = "/getById", method=RequestMethod.POST)
	@ResponseBody
	public FormWorkspaceDTO getById(@RequestBody FormWorkspaceCriteriaForm criteria, HttpServletRequest request, HttpServletResponse response) throws Exception {

		FormWorkspaceDTO formWorkspaceDto = new FormWorkspaceDTO();

		try {
			this.initFlexContext(request, response);

			int loginLangIdIndex = getLangIdIndex();

			FormWorkspaceDomain formWorkspace = formWorkspaceService2.getById(criteria.getFormWorkspaceId());

			formWorkspaceDto.setId(formWorkspace.getId());
			formWorkspaceDto.setName(formWorkspace.getName());

			// セキュリティ
			SecurityDTO securityDTO = new SecurityDTO();
			SecurityDomain securityDomain = formWorkspace.getSecurity();

			if (securityDomain != null) {
				securityDTO.setId(securityDomain.getId());
				securityDTO.setName(securityDomain.getName());
				securityDTO.setDefinitionName(securityDomain.getDefinitionName());
			}
			formWorkspaceDto.setSecurity(securityDTO);

			AttributeDomain attrDomain = formWorkspace.getAttribute(ATTRIBUTE_TYPE_NAME_OTHER_LANGUAGES);
			List<String> languageList = new ArrayList<String>();
			if (attrDomain != null )
			{
				languageList = attrDomain.getStringList();
			}
			formWorkspaceDto.setLanguageList(languageList);

			List<FormTypeDomain> formTypeList = formWorkspace.getFormTypeList();


			if (formTypeList != null) {
				ArrayList<FormTypeDTO> formTypeDTOList = new ArrayList<FormTypeDTO>();
				for (int idx = 0; idx < formTypeList.size(); idx++) {
					// 帳票タイプが設定されている帳票ワークスペースのみ返却する
					formTypeDTOList.add(this.convertToFormTypeDTO(formTypeList.get(idx), loginLangIdIndex));
				}

				formWorkspaceDto.setChildren(formTypeDTOList);

			}
		} finally {
			clearFlexContext();
		}

		return formWorkspaceDto;

	}


	/**
	 * 指定した条件に合致する全ての帳票ワークスペースを取得します。<br>
	 * 本メソッドは{@link jp.co.ctc_g.eim.app.form.business.service.FormWorkspaceService#getList(FormWorkspaceCriteria formWorkspaceCriteria)
	 * Service}へ処理を委譲します。
	 *
	 * @see jp.co.ctc_g.eim.app.form.business.service.FormWorkspaceService#getList(FormWorkspaceCriteria
	 *      formWorkspaceCriteria)
	 * @since Ver1.0
	 */
	@RequestMapping(value = "/getList", method = RequestMethod.POST)
	@ResponseBody
	public List<FormWorkspaceDTO> getList(HttpServletRequest request, HttpServletResponse response) throws Exception {

		ArrayList<FormWorkspaceDTO> dtoList = new ArrayList<FormWorkspaceDTO>();

		try {
			this.initFlexContext(request, response);

			FormWorkspaceCriteria criteria = new FormWorkspaceCriteria();


			List<FormWorkspaceDomain> formWorkspaceList = this.formWorkspaceService.getList(criteria);
			for (int idx = 0; idx < formWorkspaceList.size(); ++idx) {
				FormWorkspaceDomain formWorkspace = (FormWorkspaceDomain) formWorkspaceList.get(idx);

				FormWorkspaceDTO dto = new FormWorkspaceDTO();
				dto.setId(formWorkspace.getId());
				dto.setName(formWorkspace.getName());

				AttributeDomain attributeDomain = formWorkspace.getAttribute(ATTRIBUTE_TYPE_NAME_OTHER_LANGUAGES);
				int loginLangIdIndex = getLangIdIndex();

				if (attributeDomain != null) {
					dto.setName(attributeDomain.getStringList().get(loginLangIdIndex));
				}


				List<FormTypeDomain> formTypeList = formWorkspace.getFormTypeList();
				if (formTypeList != null) {
					ArrayList<FormTypeDTO> formTypeDTOList = new ArrayList<FormTypeDTO>();
					for (int idy = 0; idy < formTypeList.size(); ++idy) {
						// 帳票タイプが設定されている帳票ワークスペースのみ返却する
						formTypeDTOList.add(this.convertToFormTypeDTO(formTypeList.get(idy), loginLangIdIndex));
					}

					dto.setChildren(formTypeDTOList);

				}

				dtoList.add(dto);
			}
		} finally {
			clearFlexContext();
		}

		return dtoList;
	}


	/**
	 * 帳票ワークスペースを登録します。<br>
	 * 本メソッドは{@link  jp.co.ctc_g.eim.app.form.business.service.FormWorkspaceService#create(FormWorkspaceDomain form) Service}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.app.form.business.service.FormService#create(FormWorkspaceDomain form)
	 * @since Ver1.0
	 */
	@RequestMapping(value = "/create", method=RequestMethod.POST)
	@ResponseBody
	public FormWorkspaceDomain create(@RequestBody FormWorkspaceCriteriaForm criteria, HttpServletRequest request, HttpServletResponse response) throws Exception {
		FormWorkspaceDomain newFormWsDomain;

		try {
			initFlexContext(request, response);

			FormWorkspaceDomain formWorkspaceDomain = new FormWorkspaceDomain();
			ObjectTypeDomain objType = objectTypeService2.getByDefinitionName(OBJECT_TYPE_NAME_FORM_WORKSPACE);
			formWorkspaceDomain.setName(criteria.getName());
			formWorkspaceDomain.setType(objType);

			List<AttributeTypeDomain> attrTypeList = formWorkspaceDomain.getType().getAttributeTypeList();
			List<String> nameList = criteria.getNameList();

			AttributeDomain attrDomain = new AttributeDomain();
			for (int idx = 0; idx < attrTypeList.size(); idx++) {
				// 属性タイプ設定
				AttributeTypeDomain attrType = attrTypeList.get(idx);
				attrDomain.setAttributeType(attrType);
				if(ATTRIBUTE_TYPE_NAME_OTHER_LANGUAGES.equals(attrType.getDefinitionName())) {
					attrDomain.setStringList(nameList);

				}

			}
			formWorkspaceDomain.getAttributeList().add(attrDomain);


			newFormWsDomain = formWorkspaceService.create(formWorkspaceDomain);

		} finally {
			clearFlexContext();
		}

		return newFormWsDomain;
	}


	/**
	 * 帳票ワークスペースを更新します。<br>
	 * 本メソッドは{@link  jp.co.ctc_g.eim.app.form.business.service.FormWorkspaceService#update(FormWorkspaceDomain form) Service}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.app.form.business.service.FormService#update(FormWorkspaceDomain form)
	 * @since Ver1.0
	 */
	@RequestMapping(value = "/update", method=RequestMethod.POST)
	@ResponseBody
	public Map<String, String> update(@RequestBody FormWorkspaceCriteriaForm criteria, HttpServletRequest request, HttpServletResponse response) throws Exception {

		Map<String, String> resultMap = new HashMap<String, String>();

		try {
			initFlexContext(request, response);

			formWsLog.info(criteria.getNameList().toString());

			FormWorkspaceDomain formWorkspaceDomain = formWorkspaceService2.getById(criteria.getFormWorkspaceId());

			formWorkspaceDomain.setName(criteria.getName());

			formWorkspaceDomain.getAttribute(ATTRIBUTE_TYPE_NAME_OTHER_LANGUAGES).setStringList(criteria.getNameList());

			formWorkspaceService.update(formWorkspaceDomain);

			resultMap.put("result", "OK");

		} finally {
			clearFlexContext();
		}

		return resultMap;
	}


	/**
	 * 帳票ワークスペースを削除します。<br>
	 * 本メソッドは{@link  jp.co.ctc_g.eim.app.form.business.service.FormWorkspaceService#delete(FormWorkspaceCriteriaForm criteria) Service}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.app.form.business.service.FormWorkspaceService#delete(FormWorkspaceCriteriaForm criteria)
	 * @since Ver1.0
	 */
	@RequestMapping(value = "/delete", method=RequestMethod.POST)
	@ResponseBody
	public Map<String, String> delete(@RequestBody FormWorkspaceCriteriaForm criteria, HttpServletRequest request, HttpServletResponse response) throws Exception {

		Map<String, String> resultMap = new HashMap<String, String>();

		try {
			initFlexContext(request, response);

			FormWorkspaceDomain formWorkspaceDomain = formWorkspaceService2.getById(criteria.getFormWorkspaceId());

			formWorkspaceService.delete(formWorkspaceDomain);

			resultMap.put("result", "OK");

		} finally {
			clearFlexContext();
		}

		return resultMap;
	}


	/**
	 * 指定された帳票タイプ検索条件に合致する帳票タイプを取得します。<br>
	 * @param formTypeCriteria 帳票タイプ検索条件<br>
	 * <br>
	 * @return 指定された帳票タイプ検索条件に合致する帳票タイプのリスト<br>
	 * <p style="padding-left:4em">
	 * 検索条件に合致する帳票タイプが見つからない場合は空リストを返します。
	 * <br>
	 * 本メソッドは{@link jp.co.ctc_g.eim.app.form.business.service.FormTypeService#getList(FormTypeCriteria criteria) Service}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.app.form.business.service.FormTypeService#getById(FormTypeCriteria criteria)
	 * @since Ver1.0
	 */
	@RequestMapping(value = "/getFormTypeList", method=RequestMethod.POST)
	@ResponseBody
	public List<FormTypeDTO> getFormTypeList(@RequestBody FormTypeCriteriaForm criteria, HttpServletRequest request, HttpServletResponse response) throws Exception {

		List<FormTypeDTO> dtoList = new ArrayList<FormTypeDTO>();

		try {
			initFlexContext(request, response);

			int loginLangIdIndex = getLangIdIndex();

			// 条件を設定する
			FormTypeCriteria formTypeCriteria = new FormTypeCriteria();
			MultipleCriteria<Long> multipleCriteria = new MultipleCriteria<Long>();

			for (Long id : criteria.getIds()) {
				multipleCriteria.add(id);
			}
			formTypeCriteria.setIds(multipleCriteria);

			List<FormTypeDomain> domainList = formTypeService.getList(formTypeCriteria);

			// DomainをDTOに変換します
			for (int idx = 0; idx < domainList.size(); idx++) {
				FormTypeDomain domain = domainList.get(idx);
				FormTypeDTO dto = this.convertToFormTypeDTO(domain, loginLangIdIndex);
				dtoList.add(dto);
			}
		} finally {
			clearFlexContext();
		}

		return dtoList;
	}

	/**
	 * 指定された帳票タイプを帳票ワークスペースに追加する。<br>
	 * @param criteria 帳票タイプID<br>
	 * <br>
	 * 本メソッドは{@link jp.co.ctc_g.eim.app.form.business.service.FormWorkspaceService#addFormType(formWorkspace, formTypeLis) Service}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.app.form.business.service.FormTypeService#addFormType(formWorkspace, formTypeList)
	 * @since Ver1.0
	 */
	@RequestMapping(value = "/addFormType", method=RequestMethod.POST)
	@ResponseBody
	public Map<String, String>  addFormType(@RequestBody FormTypeCriteriaForm criteria, HttpServletRequest request, HttpServletResponse response) throws Exception {

		Map<String, String> resultMap = new HashMap<String, String>();

		try {
			initFlexContext(request, response);

			// 追加帳票タイプを取得
			FormTypeCriteria formTypeCriteria = new FormTypeCriteria();
			MultipleCriteria<Long> multipleCriteria = new MultipleCriteria<Long>();
			for (Long id : criteria.getIds()) {
				multipleCriteria.add(id);
			}
			formTypeCriteria.setIds(multipleCriteria);
			List<FormTypeDomain> formTypeList = formTypeService.getList(formTypeCriteria);

			// ワークスペースを取得
			FormWorkspaceDomain formWorkspace = formWorkspaceService2.getById(criteria.getFormWorkspaceId());

			// 帳票タイプを追加
			formWorkspaceService2.addFormType(formWorkspace, formTypeList);

			resultMap.put("result", "OK");

		} finally {
			clearFlexContext();
		}

		return resultMap;
	}


	/**
	 * 指定された帳票タイプを帳票ワークスペースに設定する。<br>
	 * @param criteria 帳票タイプID<br>
	 * <br>
	 * 本メソッドは{@link jp.co.ctc_g.eim.app.form.business.service.FormWorkspaceService#setFormType(formWorkspace, formTypeLis) Service}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.app.form.business.service.FormTypeService#setFormType(formWorkspace, formTypeList)
	 * @since Ver1.0
	 */
	@RequestMapping(value = "/setFormType", method=RequestMethod.POST)
	@ResponseBody
	public Map<String, String>  setFormType(@RequestBody FormTypeCriteriaForm criteria, HttpServletRequest request, HttpServletResponse response) throws Exception {

		Map<String, String> resultMap = new HashMap<String, String>();

		try {
			initFlexContext(request, response);

			// ワークスペースを取得
			FormWorkspaceDomain formWorkspace = formWorkspaceService2.getById(criteria.getFormWorkspaceId());

			// 帳票タイプを取得
			List<FormTypeDomain> formTypeList  = formWorkspace.getFormTypeList();

			// 削除対象帳票タイプに帳票タイプフォルダが紐付いている場合、チェックエラーとする
			List<FormTypeFolderDomain>  formTypeFolderList = formTypeFolderService.getListByIds(criteria.getFormWorkspaceId(), criteria.getIds());
			if (formTypeFolderList !=null && formTypeFolderList.size() > 0) {
				resultMap.put("result", "CHECK_ERROR");
				return resultMap;
			}

			// 帳票タイプを設定
			int loopCnt = criteria.getIds().size();
			for (int idx = 0; idx < loopCnt; idx++) {
				int loopCnty = formTypeList.size();
				Long formTypeId = criteria.getIds().get(idx);
				for (int idy = 0; idy < loopCnty; idy++) {
					if (formTypeId.equals(formTypeList.get(idy).getId())) {
						formTypeList.remove(idy);
						break;
					}
				}

			}
			formWorkspaceService2.setFormType(formWorkspace, formTypeList);

			resultMap.put("result", "OK");

		} finally {
			clearFlexContext();
		}

		return resultMap;
	}


	/**
	 * 検索条件に合致する帳票タイプを取得します。<br>
	 * @param criteria 帳票タイプ検索条件<br>
	 * <br>
	 * @return 検索条件に合致する帳票タイプのリスト<br>
	 * <p style="padding-left:4em">
	 * 検索条件に合致する帳票タイプが見つからない場合は空リストを返します。
	 * <br>
	 * 本メソッドは{@link jp.co.ctc_g.eim.app.form.business.service.FormTypeService#getByDefinitionName(String definitionName) Service}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.app.form.business.service.FormTypeService#getByDefinitionName(String definitionName)
	 * @since Ver1.0
	 */
	@RequestMapping(value = "/getByDefinitionName", method=RequestMethod.POST)
	@ResponseBody
	public FormTypeDTO getByDefinitionName(@RequestBody FormTypeCriteriaForm criteria, HttpServletRequest request, HttpServletResponse response) throws Exception {

		FormTypeDTO formTypeDto = new FormTypeDTO();

		try {
			initFlexContext(request, response);

			FormTypeDomain formDomain = formTypeService.getByDefinitionName(criteria.getDefinitionName());
			formTypeDto.setId(formDomain.getId());
			formTypeDto.setDefinitionName(formDomain.getDefinitionName());
			formTypeDto.setRootType(true);
			formTypeDto.setRootTypeDefName(FORM_TYPE_NAME);

			// DomainをDTOに変換します
			List<ObjectTypeDomain> domainList  = formDomain.getChildList();

			// 帳票タイプを設定する
			List<FormTypeDTO> dtoList = new ArrayList<FormTypeDTO>();
			if (domainList !=null ) {
				for (int idx = 0; idx < domainList.size(); idx++) {
					ObjectTypeDomain domain = domainList.get(idx);
					FormTypeDTO dto = this.convertToFormTypeDTO2(domain);
					dto.setRootType(false);
					dtoList.add(dto);
				}

			}

			formTypeDto.setChildrenType(dtoList);

		} finally {
			clearFlexContext();
		}

		return formTypeDto;
	}


	/**
	 * 指定したIDの帳票タイプフォルダを取得します。<br>
	 * 本メソッドは{@link jp.co.ctc_g.eim.app.form.business.service.FormTypeFolderService#getById(FormTypeFolderCriteriaForm criteria)
	 * Service}へ処理を委譲します。
	 *
	 * @see jp.co.ctc_g.eim.app.form.business.service.FormTypeFolderService#getById(FormTypeFolderCriteriaForm
	 *      criteria)
	 * @since Ver1.0
	 */
	@RequestMapping(value = "/getFormFolderById", method=RequestMethod.POST)
	@ResponseBody
	public FormTypeFolderDTO getFormFolderById(@RequestBody FormTypeFolderCriteriaForm criteria, HttpServletRequest request, HttpServletResponse response) throws Exception {

		FormTypeFolderDTO formTypeFolderDto;

		try {
			this.initFlexContext(request, response);
			int loginLangIdIndex = getLangIdIndex();

			FormTypeFolderDTO parentFormTypeFolderDto = new FormTypeFolderDTO();

			Long objId = criteria.getFormFolderId();
			FormTypeFolderDomain formFolderDomain = formTypeFolderService.getById(objId);

			AttributeDomain attributeDomain = formFolderDomain.getAttribute(ATTRIBUTE_TYPE_NAME_FORM_TYPE_FOLDER_ID);
			FormTypeFolderDomain parentFormFolderDomain = null;

			if (attributeDomain != null && attributeDomain.getLong() != 0) {
				parentFormFolderDomain = formTypeFolderService.getById(attributeDomain.getLong());
				parentFormTypeFolderDto= convertToFormTypeFolderDTO(parentFormFolderDomain, loginLangIdIndex);
			}

			formTypeFolderDto = convertToFormTypeFolderDTO(formFolderDomain, loginLangIdIndex);
			formTypeFolderDto.setParentFolder(parentFormTypeFolderDto);

			AttributeDomain attrDomain = formFolderDomain.getAttribute(ATTRIBUTE_TYPE_NAME_OTHER_LANGUAGES);
			List<String> languageList = new ArrayList<String>();
			if (attrDomain != null )
			{
				languageList = attrDomain.getStringList();
			}
			formTypeFolderDto.setLanguageList(languageList);

		} finally {
			clearFlexContext();
		}

		return formTypeFolderDto;

	}


	/**
	 * 指定した条件に合致する帳票タイプフォルダリストを取得します。<br>
	 * 本メソッドは{@link jp.co.ctc_g.eim.app.form.business.service.FormWorkspaceService#getFormTypeFolderList(Long formWorkspaceId, Long formTypeId)
	 * Service}へ処理を委譲します。
	 *
	 * @see jp.co.ctc_g.eim.app.form.business.service.formTypeFolderService#getList(Long formWorkspaceId, Long formTypeId)
	 * @since Ver1.0
	 */
	@RequestMapping(value = "/getFormTypeFolderList", method = RequestMethod.POST)
	@ResponseBody
	public List<FormTypeFolderDTO> getFormTypeFolderList(@RequestBody FormTypeFolderCriteriaForm criteria, HttpServletRequest request, HttpServletResponse response) throws Exception {

		// 条件を設定する
		List<FormTypeFolderDTO> dtoList = new ArrayList<FormTypeFolderDTO>();

		try {
			this.initFlexContext(request, response);
			int loginLangIdIndex = getLangIdIndex();

			Long formWorkspaceId = criteria.getFormWorkspaceId();
			Long formTypeId = criteria.getFormTypeId();

			List<FormTypeFolderDomain> domainList = formTypeFolderService.getList(formWorkspaceId, formTypeId);

			// DomainをDTOに変換します
			if (domainList != null ) {
				for (int i = 0; i < domainList.size(); i++) {
					FormTypeFolderDomain domain = domainList.get(i);
					FormTypeFolderDTO dto = this.convertToFormTypeFolderDTO(domain, loginLangIdIndex);
					dtoList.add(dto);
				}
			}
		} finally {
			clearFlexContext();
		}

		return dtoList;
	}



	/**
	 * 帳票フォルダを登録します。<br>
	 * 本メソッドは{@link  jp.co.ctc_g.eim.app.form.business.service.FormTypeFolderService#create(FormTypeFolderDomain formTypeFolder) Service}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.app.form.business.service.FormTypeFolderService#create(FormTypeFolderDomain formTypeFolder)
	 * @since Ver1.0
	 */
	@RequestMapping(value = "/formFolderCreate", method=RequestMethod.POST)
	@ResponseBody
	public FormTypeFolderDomain formFolderCreate(@RequestBody FormTypeFolderCriteriaForm criteria, HttpServletRequest request, HttpServletResponse response) throws Exception {

		FormTypeFolderDomain newFormFolderWsDomain;

		try {
			initFlexContext(request, response);

			formWsLog.info(ToStringBuilder.reflectionToString(criteria, ToStringStyle.DEFAULT_STYLE));

			FormTypeFolderDomain formTypeFolder = new FormTypeFolderDomain();

			List<String> nameList = criteria.getNameList();

			// オブジェクト名に言語フィールドの0番目の値を設定
			formTypeFolder.setName(nameList.get(0));

			ObjectTypeDomain objType = objectTypeService2.getByDefinitionName(OBJECT_TYPE_NAME_FORM_TYPE_FOLDER);
			formTypeFolder.setType(objType);

			// 属性「多言語リスト」設定
			List<AttributeTypeDomain> attrTypeList = formTypeFolder.getType().getAttributeTypeList();

			AttributeDomain nameListAttr = new AttributeDomain();
			AttributeDomain formTypeFolderAttr = new AttributeDomain();
			AttributeDomain formTypeAttr = new AttributeDomain();
			AttributeDomain formWorkspaceAttr = new AttributeDomain();

			for (int idx = 0; idx < attrTypeList.size(); idx++) {
				// 属性タイプ設定
				AttributeTypeDomain attrType = attrTypeList.get(idx);

				// 属性「多言語リスト」
				if(ATTRIBUTE_TYPE_NAME_OTHER_LANGUAGES.equals(attrType.getDefinitionName())) {
					nameListAttr.setAttributeType(attrType);
					nameListAttr.setStringList(nameList);

				}
				// 属性「帳票タイプフォルダ」
				if (criteria.getParentFormFolderId() != null) {
					if(ATTRIBUTE_TYPE_NAME_FORM_TYPE_FOLDER_ID.equals(attrType.getDefinitionName())) {
						formTypeFolderAttr.setAttributeType(attrType);
						formTypeFolderAttr.getLongList().clear();
						formTypeFolderAttr.setLong(criteria.getParentFormFolderId());

					}
				}

				// 属性「帳票タイプ」
				if(ATTRIBUTE_TYPE_NAME_FORM_TYPE_ID.equals(attrType.getDefinitionName())) {
					formTypeAttr.setAttributeType(attrType);
					formTypeAttr.getLongList().clear();
					formTypeAttr.setLong(criteria.getFormTypeId());

				}
				// 属性「帳票ワークスペース」
				if(ATTRIBUTE_TYPE_NAME_FORM_WORKSPACE_ID.equals(attrType.getDefinitionName())) {
					formWorkspaceAttr.setAttributeType(attrType);
					formWorkspaceAttr.getLongList().clear();
					formWorkspaceAttr.setLong(criteria.getFormWorkspaceId());

				}

			}

			// 属性「多言語リスト」設定
			formTypeFolder.getAttributeList().add(nameListAttr);

			if (criteria.getParentFormFolderId() != null) {
				// 属性「帳票タイプフォルダID」設定
				formTypeFolder.getAttributeList().add(formTypeFolderAttr);
			}

			// 属性「帳票タイプID」設定
			formTypeFolder.getAttributeList().add(formTypeAttr);

			// 属性「帳票ワークスペースID」設定
			formTypeFolder.getAttributeList().add(formWorkspaceAttr);

			// 帳票フォルダを登録
			newFormFolderWsDomain = formTypeFolderService.create(formTypeFolder);

		} finally {
			clearFlexContext();
		}

		return newFormFolderWsDomain;
	}


	/**
	 * 帳票フォルダを更新します。<br>
	 * 本メソッドは{@link  jp.co.ctc_g.eim.app.form.business.service.FormTypeFolderService#update(FormTypeFolderDomain formTypeFolder) Service}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.app.form.business.service.FormTypeFolderService#update(FormTypeFolderDomain formTypeFolder)
	 * @since Ver1.0
	 */
	@RequestMapping(value = "/formFolderUpdate", method=RequestMethod.POST)
	@ResponseBody
	public Map<String, String> formFolderUpdate(@RequestBody FormTypeFolderCriteriaForm criteria, HttpServletRequest request, HttpServletResponse response) throws Exception {

		Map<String, String> resultMap = new HashMap<String, String>();

		try {
			initFlexContext(request, response);

			// 帳票フォルダを取得
			FormTypeFolderDomain formTypeFolder = formTypeFolderService.getById(criteria.getFormFolderId());

			// 属性「多言語リスト」設定
			formTypeFolder.getAttribute(ATTRIBUTE_TYPE_NAME_OTHER_LANGUAGES).setStringList(criteria.getNameList());

			// 帳票フォルダを更新
			formTypeFolderService.update(formTypeFolder);


			resultMap.put("result", "OK");

		} finally {
			clearFlexContext();
		}
		return resultMap;
	}


	/**
	 * 帳票フォルダを削除します。<br>
	 * 本メソッドは{@link  jp.co.ctc_g.eim.app.form.business.service.FormTypeFolderService#delete(FormTypeFolderCriteriaForm criteria) Service}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.app.form.business.service.FormTypeFolderService#delete(FormTypeFolderCriteriaForm criteria)
	 * @since Ver1.0
	 */
	@RequestMapping(value = "/formFolderDelete", method=RequestMethod.POST)
	@ResponseBody
	public Map<String, String> formFolderDelete(@RequestBody FormTypeFolderCriteriaForm criteria, HttpServletRequest request, HttpServletResponse response) throws Exception {

		Map<String, String> resultMap = new HashMap<String, String>();

		try {
			initFlexContext(request, response);

			// 帳票フォルダを取得
			FormTypeFolderDomain formTypeFolder = formTypeFolderService.getById(criteria.getFormFolderId());

			// 帳票フォルダを削除
			formTypeFolderService.delete(formTypeFolder);

			resultMap.put("result", "OK");

		} finally {
			clearFlexContext();
		}

		return resultMap;
	}


	/**
	 * 帳票ワークスペースサービスを取得します。
	 *
	 * @return 帳票ワークスペースサービス
	 * @since Ver 1.0
	 */
	public FormWorkspaceService getFormWorkspaceService2() {
		return this.formWorkspaceService2;
	}

	/**
	 * 帳票ワークスペースサービスを設定します。
	 *
	 * @param formWorkspaceService2 帳票ワークスペースサービス
	 */
	public void setFormWorkspaceService2(FormWorkspaceService formWorkspaceService2) {
		this.formWorkspaceService2 = formWorkspaceService2;
	}

	/**
	 * 帳票ワークスペースサービスを取得します。
	 *
	 * @return 帳票ワークスペースサービス
	 * @since Ver 1.0
	 */
	public FormWorkspaceService getFormWorkspaceService() {
		return this.formWorkspaceService;
	}

	/**
	 * 帳票ワークスペースサービスを設定します。
	 *
	 * @param formWorkspaceService
	 *            帳票ワークスペースサービス
	 */
	public void setFormWorkspaceService(FormWorkspaceService formWorkspaceService) {
		this.formWorkspaceService = formWorkspaceService;
	}



	/**
	 * 帳票タイプサービスを取得します。
	 * @return 帳票タイプサービス
	 * @since Ver 1.0
	 */
	public FormTypeService getFormTypeService() {
		return formTypeService;
	}

	/**
	 * 帳票タイプサービスを設定します。
	 * @param formTypeService 帳票タイプサービス
	 * @since Ver 1.0
	 */
	public void setFormTypeService(FormTypeService formTypeService) {
		this.formTypeService = formTypeService;
	}


	/**
	 * 帳票タイプフォルダサービスを取得します。
	 *
	 * @return 帳票タイプフォルダサービス
	 * @since Ver 1.0
	 */
	public FormTypeFolderService getFormTypeFolderService() {
		return this.formTypeFolderService;
	}

	/**
	 * 帳票タイプフォルダサービスを設定します。
	 *
	 * @param formWorkspaceService
	 *            帳票タイプフォルダサービス
	 */
	public void setFormTypeFolderService(FormTypeFolderService formTypeFolderService) {
		this.formTypeFolderService = formTypeFolderService;
	}


	/**
	 * オブジェクトタイプサービスを取得します。
	 *
	 * @return オブジェクトタイプサービス
	 * @since Ver 1.0
	 */
	public ObjectTypeService getObjectTypeService2() {
		return this.objectTypeService2;
	}

	/**
	 * オブジェクトタイプサービスを設定します。
	 *
	 * @param objectTypeService2
	 *            オブジェクトタイプサービス
	 */
	public void setObjectTypeService2(ObjectTypeService objectTypeService2) {
		this.objectTypeService2 = objectTypeService2;
	}


	/**
	 * 言語サービスを取得します。
	 * @return 言語サービス
	 * @since Ver 3.0
	 */
	public LanguageService getLanguageService() {
		return languageService;
	}

	/**
	 * 言語サービスを設定します。
	 * @param formWorkspaceService 言語サービス
	 */
	public void setLanguageService(LanguageService languageService) {
		this.languageService = languageService;
	}

	/**
	 * 帳票タイプドメインを帳票タイプDTOに変換して返却します。
	 *
	 * @param domain 帳票タイプドメイン
	 * @return 帳票タイプDTO
	 */
	private FormTypeDTO convertToFormTypeDTO2(ObjectTypeDomain domain) {
		FormTypeDTO dto = new FormTypeDTO();

		dto.setId(domain.getId());
		dto.setDefinitionName(domain.getDefinitionName());


		List<ObjectTypeDomain> formTypeList = domain.getChildList();
		if (formTypeList != null) {
			List<FormTypeDTO> formTypeDTOList = new ArrayList<FormTypeDTO>();
			for (int idx = 0; idx < formTypeList.size(); idx++) {
				formTypeDTOList.add(convertToFormTypeDTO2((FormTypeDomain) formTypeList.get(idx)));
			}
			dto.setChildrenType(formTypeDTOList);
		}

		return dto;
	}


	/**
	 * 帳票タイプドメインを帳票タイプDTOに変換して返却します。
	 *
	 * @param domain 帳票タイプドメイン
	 * @return 帳票タイプDTO
	 */
	private FormTypeDTO convertToFormTypeDTO(FormTypeDomain domain, int loginLangIdIndex) {
		FormTypeDTO dto = new FormTypeDTO();
		WorkflowDomain workflow = new WorkflowDomain();

		dto.setId(domain.getId());
		dto.setName(domain.getName());
		dto.setDefinitionName(domain.getDefinitionName());

		workflow = domain.getWorkflow();
		if (workflow != null) {
			dto.setWorkflowId(domain.getWorkflow().getId());
			dto.setWorkflowName(domain.getWorkflow().getDefinitionName());

		}

		List<FormTypeFolderDomain> formTypeFolderList = domain.getFormTypeFolderList();
		if (formTypeFolderList != null) {

			List<FormTypeFolderDTO> formTypeFolderDTOList = new ArrayList<FormTypeFolderDTO>();
			for (int idx = 0; idx < formTypeFolderList.size(); idx++) {
				formTypeFolderDTOList.add(convertToFormTypeFolderDTO(formTypeFolderList.get(idx), loginLangIdIndex));
			}
			dto.setChildren(formTypeFolderDTOList);
		}

		return dto;
	}


	/**
	 * 帳票タイプフォルダドメインを帳票タイプフォルダDTOに変換して返却します。
	 *
	 * @param domain  帳票タイプフォルダドメイン
	 * @return 帳票タイプフォルダDTO
	 */
	private FormTypeFolderDTO convertToFormTypeFolderDTO(FormTypeFolderDomain domain, int loginLangIdIndex) {
		FormTypeFolderDTO dto = new FormTypeFolderDTO();
		SecurityDomain secDomain = domain.getSecurity();

		dto.setId(domain.getId());
		dto.setName(domain.getName());

		AttributeDomain attributeDomain = domain.getAttribute(ATTRIBUTE_TYPE_NAME_OTHER_LANGUAGES);

		if (attributeDomain != null) {
			dto.setName(attributeDomain.getStringList().get(loginLangIdIndex));
		}

		if (secDomain != null) {
			// dto.setSecurityName(secDomain.getDefinitionName());
			dto.setSecurityName(secDomain.getName());
		}

		List<FormTypeFolderDomain> formTypeFolderList = domain.getFormTypeFolderList();
		if (formTypeFolderList != null) {
			List<FormTypeFolderDTO> formTypeFolderDTOList = new ArrayList<FormTypeFolderDTO>();
			for (int idx = 0; idx < formTypeFolderList.size(); idx++) {
				formTypeFolderDTOList.add(convertToFormTypeFolderDTO(formTypeFolderList.get(idx), loginLangIdIndex));
			}
			dto.setChildren(formTypeFolderDTOList);
		}

		return dto;
	}

	/**
	 * 言語リストからログイン言語IDのインデックスを取得します。
	 * @return インデックス
	 * @throws Exception
	 */
	private int getLangIdIndex() throws Exception {
		int index = -1;
		TransactionContext tx = EIMThreadContext.getTransactionContext();
		String langId = tx.getLangId();

		List<LanguageDomain> langList = languageService.getList();

		for (int idx = 0; idx < langList.size(); idx++) {
			LanguageDomain lang = langList.get(idx);
			if (langId.equals(lang.getLangId())) {
				index = idx;
				break;
			}
		}

		return index;
	}

	/**
	 * ドキュメントフォームオブジェクトを取得します。<br>
	 * 本メソッドは{@link  jp.co.ctc_g.eim.app.document.business.service.DocumentFormService#getById(long id) Service}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.app.document.business.service.DocumentFormService#getById(long id)
	 * @since Ver6.8
	 */
	@RequestMapping(value = "/getByIdAdmin", method=RequestMethod.GET)
	@ResponseBody
	public FormDomain getByIdAdmin(@RequestParam("id") long id) throws Exception {
		FormDomain form = documentFormService.getByIdAdmin(id);
		form.setModificationDate(
				convertDateClass(form.getModificationDate()));

		return form;
	}

	/**
	 * 日付型を変更（java.sql.Date → java.util.Date）する
	 */
    private Date convertDateClass(Date date) throws Exception {
        // sql.dateの場合、変換処理を行う
        if (date != null && date.getClass().getName().equals("java.sql.Date")) {
            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
            date = sdf.parse(sdf.format(date));
        }
        return date;
    }

    /**
	 * ドキュメントフォームに関する操作を行うサービスを設定します。
	 * @param documentFormService ドキュメントフォームに関する操作を行うサービス
	 */
	public void setDocumentFormService(DocumentFormService documentFormService) {
		this.documentFormService = documentFormService;
	}

}
