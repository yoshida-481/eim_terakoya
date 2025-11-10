package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;

import org.apache.commons.beanutils.BeanUtils;

import common.bo.AttributeValueMaster;
import common.util.AppConstant;
import common.util.AppUpdateNoticeUtils;
import common.util.AttributeMasterUtil;
import eim.bo.EIMAccessRole;
import eim.bo.EIMObject;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.ObjectUtils;
import jp.co.ctc_g.eim.app.document.business.domain.DocumentDomain;
import jp.co.ctc_g.eim.app.document.business.domain.FolderDomain;
import jp.co.ctc_g.eim.app.document.business.domain.PlaceDomain;
import jp.co.ctc_g.eim.app.document.business.domain.criteria.FolderCriteria;
import jp.co.ctc_g.eim.app.document.business.domain.criteria.WorkspaceCriteria.WorkspaceItemEnum;
import jp.co.ctc_g.eim.app.document.business.service.FolderService;
import jp.co.ctc_g.eim.app.document.common.util.AccessUtils;
import jp.co.ctc_g.eim.app.document.common.util.AppDocumentLogicUtil;
import jp.co.ctc_g.eim.app.document.common.util.AppDocumentUtil;
import jp.co.ctc_g.eim.app.document.common.util.AttributeUtil;
import jp.co.ctc_g.eim.app.document.common.util.OperationHistoryUtils;
import jp.co.ctc_g.eim.app.document.common.util.RelationUtils;
import jp.co.ctc_g.eim.app.document.common.util.StringUtils;
import jp.co.ctc_g.eim.framework2.business.dao.ObjectDao;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AccessRoleTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.RelationDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.RelationTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.SecurityDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchConditionCompare;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchConditionGroup;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchLimitCountCondition;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchOperatorEnum;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchSelectObject;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchSelectObject.SearchConditionBuildHelper;
import jp.co.ctc_g.eim.framework2.business.service.AttributeTypeService;
import jp.co.ctc_g.eim.framework2.business.service.ObjectService;
import jp.co.ctc_g.eim.framework2.business.service.ObjectTypeService;
import jp.co.ctc_g.eim.framework2.business.service.RelationService;
import jp.co.ctc_g.eim.framework2.business.service.RelationTypeService;
import jp.co.ctc_g.eim.framework2.business.service.UserService;
import jp.co.ctc_g.eim.framework2.business.service.WorkflowService;
import jp.co.ctc_g.eim.framework2.common.enumeration.DuplicateCheckModeEnum;
import jp.co.ctc_g.eim.framework2.common.exception.EIMException;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;
import jp.co.ctc_g.eim.framework2.common.util.ResourceUtils;

/**
 * 【ドキュメントAPI】
 * @see FolderService
 */
public class FolderServiceImpl implements FolderService {

	/** オブジェクトDao */
	private ObjectDao objectDao = null;

	/** オブジェクトサービス */
	private ObjectService objectService = null;

	/** オブジェクトタイプサービス */
	private ObjectTypeService objectTypeService = null;

	/** リレーションタイプサービス */
	private RelationTypeService relationTypeService = null;

	/** リレーションサービス */
	private RelationService relationService = null;

	/** 属性タイプサービス */
	private AttributeTypeService attributeTypeService = null;

	/** ワークフローサービス */
	private WorkflowService workflowService = null;

	/** FolderDomainを取得する時に使う	*/
	private List<WorkspaceItemEnum> itemListForSingleTargetMethod = null;

	/** オブジェクトタイプ名 */
	private String objectTypeName = null;

	/** ユーザサービス */
	private UserService userService = null;

	/** ごみ箱の検索条件追加フラグ */
	private boolean includeGarbageBox = false;

	/**
	 * フォルダIDを指定してフォルダを取得します。
	 *
	 * @param id フォルダID
	 * @return フォルダドメイン
	 * @throws Exception
	 */
	public FolderDomain getById(long id) throws Exception {
		if (id < 1) {
			return null;
		}
		// オブジェクトの取得
		ObjectDomain objectDomain = objectService.getById(id);

		if (objectDomain == null) {
			return null;
			// オブジェクトが存在しない、エラー処理
			// throw new EIMException("EIM.ERROR.LOGIC.NOWSFOL");
		}

		// オブジェクトタイプチェック
		if (!AppDocumentLogicUtil.isTypeOfFolder(objectDomain.getType())) {
			return null;
			//throw new EIMException("EIM.ERROR.LOGIC.NOTEXIST.DOCFOLWS");
		}

		FolderDomain folderDomain = getFolderByObjectDomain(objectDomain);

		// 子階層フォルダ、ドキュメントのデータを取得
		folderDomain = getFolderAndDocumentList(folderDomain, objectDomain);

		return folderDomain;
	}

	/**
	 * パスを指定してフォルダを取得します。
	 *
	 * @param path パス
	 * @return フォルダドメイン
	 * @throws Exception
	 */
	public FolderDomain getByPath(String path) throws Exception {
		if (StringUtils.isEmpty(path)) {
			throw new EIMException("EIM.ERROR.LOGIC.PATH.VALUE.ILLEGAL");
		}
		// 取得リスト
		List<ObjectDomain> resultList = new ArrayList<ObjectDomain>();
		FolderDomain resultDomain = null;

		SearchSelectObject searchSelectObject = new SearchSelectObject();
		SearchConditionBuildHelper helper = new SearchConditionBuildHelper();
		SearchConditionGroup searchConditionGroup = helper.group(helper.opAnd());

		// オブジェクト名を取得
		int pos = path.lastIndexOf("/");
		String objName = path.substring(pos + 1);
		// 「パス」属性値を取得
		String attrPath = path.substring(0, pos + 1);

		// オブジェクト名条件設定
		searchConditionGroup.addCondition(helper.eq(helper.opAnd(), SearchSelectObject.PsedoAttributeTypeEnum.NAME, objName));
		// 「パス」属性条件設定
		AttributeTypeDomain attributeType = attributeTypeService.getByDefinitionName(ConfigUtils.getByKey("ATTR_NAME_FOLDER_PASS"));
		// パス
		searchConditionGroup.addCondition(helper.eq(helper.opAnd(), attributeType, attrPath));

		// オブジェクトタイプ指定
		ObjectTypeDomain folderObjTypeDomain = AppDocumentLogicUtil.getFolderObjectType();
		List<Long> folderObjTypes = AppDocumentLogicUtil.getChildObjectTypeList(folderObjTypeDomain.getId());
		searchConditionGroup.addCondition(helper.inArray(helper.opAnd(), SearchSelectObject.PsedoAttributeTypeEnum.TYPE, helper.opIn(),
				folderObjTypes.toArray()));

		searchSelectObject.setCondition(searchConditionGroup);

		resultList = objectService.getList(searchSelectObject, null);
		if (resultList != null && resultList.size() > 0) {
			// WSのフォルダドメインをFWのオブジェクトドメインに変換する
			resultDomain = getFolderByObjectDomain(resultList.get(0));

			// 権限チェック
			if (!objectService.authorized((ObjectDomain) resultDomain, new AccessRoleTypeDomain("READ"))) {
				// アクセス権がありません
				throw new EIMException("EIM.ERROR.LOGIC.NOACCESS");
			}

			// 子階層フォルダ、ドキュメントのデータを取得
			resultDomain = getFolderAndDocumentList(resultDomain, resultList.get(0));
		}

		return resultDomain;
	}

	/**
	 * 指定した条件のフォルダを取得します。
	 * （名称（部分一致）、ID（複数）、属性を指定。AND検索のみ対応。）
	 *
	 * @param criteria フォルダクライテリア
	 * @return フォルダドメインリスト
	 * @throws Exception
	 */
	public List<FolderDomain> getList(FolderCriteria criteria) throws Exception {
		if (criteria == null) {
			// 引数criteriaとしてnullが入力されました。
			throw new EIMException("EIM.ERROR.LOGIC.CRITERIA.VALUE.ILLEGAL");
		}

		// 属性についてidからdefinitionNameの補完
		compAttrDefinitionName(criteria);

		// オブジェクトの検索条件を指定
		SearchSelectObject searchSelectObject = new SearchSelectObject();
		SearchConditionBuildHelper helper = new SearchConditionBuildHelper();
		SearchConditionGroup searchConditionGroup = helper.group(helper.opAnd());

		// ドキュメント名称（部分一致）
		if (criteria.getName() != null) {
			// オブジェクト名条件設定
			searchConditionGroup.addCondition(helper.like(helper.opAnd(), SearchSelectObject.PsedoAttributeTypeEnum.NAME, helper.opLike(), criteria
					.getName()));
		}
		// ID（複数）指定
		if (criteria.getIds() != null) {
			searchConditionGroup.addCondition(helper.inArray(helper.opAnd(), SearchSelectObject.PsedoAttributeTypeEnum.ID, helper.opIn(), criteria
					.getIds().getArrayList().toArray()));
		}

		// 属性検索リスト指定
		if (criteria.getAttributeList() != null) {
			for (AttributeDomain attr : criteria.getAttributeList()) {
				if(attr == null){
					continue;
				}
				AttributeTypeDomain attributeType = attr.getAttributeType();

				if(attributeType.getId() < 1){
					String targetAttTypeName = attributeType.getDefinitionName();
					attributeType = attributeTypeService.getByDefinitionName(attributeType.getDefinitionName());
					if(attributeType == null){
						//対象の属性タイプ{0}が存在しません。
						Object[] param = {targetAttTypeName};
						throw new EIMException("EIM.ERROR.LOGIC.ATTRIBUTETYPE.NOTFOUND",param);
					}
				}

				switch (attributeType.getValueType()) {
				case LONG:
					if (!attributeType.isMultiple()) {
						//完全一致
						searchConditionGroup.addCondition(helper.eq(helper.opAnd(), attributeType, attr.getLong()));
					} else {
						//ORで完全一致
						SearchConditionGroup searchChildConditionGroup = helper.group(helper.opAnd());
						for (long condition : attr.getLongList()) {
							searchChildConditionGroup.addCondition(helper.eq(helper.opOr(), attributeType, condition));
						}
						searchConditionGroup.addCondition(searchChildConditionGroup);
					}
					break;
				case STRING:
					if (!attributeType.isMultiple()) {
						//部分一致
						searchConditionGroup.addCondition(helper.like(helper.opAnd(), attributeType,helper.opLike(), attr.getString()));
					} else {
						//ORで部分一致
						SearchConditionGroup searchChildConditionGroup = helper.group(helper.opAnd());
						for (String condition : attr.getStringList()) {
							searchChildConditionGroup.addCondition(helper.like(helper.opOr(), attributeType,helper.opLike(), condition));
						}
						searchConditionGroup.addCondition(searchChildConditionGroup);
					}
					break;
				case DATE:
					if (!attributeType.isMultiple()) {
						//完全一致
						searchConditionGroup.addCondition(helper.eq(helper.opAnd(), attributeType, attr.getDate()));
					} else {
						//ORで完全一致
						//searchConditionGroup.addCondition(helper.inArray(helper.opAnd(), attributeType, helper.opIn(), attr.getDateList().toArray()));
						SearchConditionGroup searchChildConditionGroup = helper.group(helper.opAnd());
						for (Date condition : attr.getDateList()) {
							searchChildConditionGroup.addCondition(helper.eq(helper.opOr(), attributeType, condition));
						}
						searchConditionGroup.addCondition(searchChildConditionGroup);
					}
					break;
				case TEXT:
					if (!attributeType.isMultiple()) {
						//完全一致
						searchConditionGroup.addCondition(helper.like(helper.opAnd(), attributeType, helper.opLike(), attr.getText()));
					} else{
						//ORで部分一致
						SearchConditionGroup searchChildConditionGroup = helper.group(helper.opAnd());
						for (String condition : attr.getTextList()) {
							searchChildConditionGroup.addCondition(helper.like(helper.opOr(), attributeType,helper.opLike(), condition));
						}
						searchConditionGroup.addCondition(searchChildConditionGroup);

					}
					break;
				case DOUBLE:
					if (!attributeType.isMultiple()) {
						//完全一致
						searchConditionGroup.addCondition(helper.eq(helper.opAnd(), attributeType, attr.getDouble()));
					} else {
						//ORで完全一致
						SearchConditionGroup searchChildConditionGroup = helper.group(helper.opAnd());
						for (Double condition : attr.getDoubleList()) {
							searchChildConditionGroup.addCondition(helper.eq(helper.opOr(), attributeType, condition));
						}
						searchConditionGroup.addCondition(searchChildConditionGroup);
					}
					break;
				}
			}
		}

		// 「パス」属性条件設定
		AttributeTypeDomain pathAttr = AppDocumentLogicUtil.getDocPathAttributeType();
		if (criteria.getPlace() != null) {
			PlaceDomain place = criteria.getPlace();
			// パスの取得
			String path = place.getPath();
			if (path == null) {
				// ワークスペースの場合、パス属性の値を保持していない
				path = "/";
			}
			path += place.getName() + "/";
			// 部分一致
			searchConditionGroup.addCondition(helper.like(helper.opAnd(), pathAttr, helper.opLike(), path + "*"));
		}

		if (!includeGarbageBox) {
			searchConditionGroup.addCondition(helper.like(helper.opAnd(), pathAttr, helper.opNotLike(), "/"+ConfigUtils.getByKey("OBJECT_TYPE_NAME_RECYCLE")+"/" + "*"));
		}

		// オブジェクトタイプ指定criteria
		ObjectTypeDomain folderObjTypeDomain = null;
		if (criteria.getObjectTypeId() == null) {
			folderObjTypeDomain = AppDocumentLogicUtil.getFolderObjectType();
			if (folderObjTypeDomain == null) {
				// 指定のフォルダタイプが取得できませんでした。
				throw new EIMException("EIM.ERROR.LOGIC.NOFOLDERTYPE");
			}
			List<Long> folderObjTypes = AppDocumentLogicUtil.getChildObjectTypeList(folderObjTypeDomain.getId());
			searchConditionGroup.addCondition(helper.inArray(helper.opAnd(), SearchSelectObject.PsedoAttributeTypeEnum.TYPE, helper.opIn(),
					folderObjTypes.toArray()));
		} else {
			// objectTypeIdが設定されている場合
			folderObjTypeDomain = objectTypeService.getById(criteria.getObjectTypeId());
			if (!AppDocumentLogicUtil.isTypeOfFolder(folderObjTypeDomain)) {
				// 指定のフォルダタイプが取得できませんでした。
				throw new EIMException("EIM.ERROR.LOGIC.NOFOLDERTYPE");
			}
			long objTypeId = folderObjTypeDomain.getId();
			searchConditionGroup.addCondition(helper.eq(helper.opAnd(), SearchSelectObject.PsedoAttributeTypeEnum.TYPE, objTypeId));
		}

		//権限チェック
		searchSelectObject.setAccessRoleType(new AccessRoleTypeDomain("READ"));
		searchSelectObject.setCondition(searchConditionGroup);

		//最大取得件数 何も設定しない場合は無制限
		SearchLimitCountCondition limitCountCondition = new SearchLimitCountCondition(Integer.MAX_VALUE,false);
		if(criteria.getLimit() != null && criteria.getLimit() > 0){
			if(criteria.isLimitCondition() != null && criteria.isLimitCondition()){
				limitCountCondition = new SearchLimitCountCondition(criteria.getLimit(),true);
			}else{
				limitCountCondition = new SearchLimitCountCondition(criteria.getLimit(),false);
			}
		}

		List<ObjectDomain> objectList = objectService.getList(searchSelectObject, limitCountCondition);
		if (objectList == null || objectList.size() == 0) {
			return null;
		}

		// 取得リスト
		List<FolderDomain> resultList = new ArrayList<FolderDomain>();

		// WSのフォルダドメインをFWのオブジェクトドメインに変換する
		for (ObjectDomain objectDomain : objectList) {
			FolderDomain folderDomain = getFolderByObjectDomain(objectDomain);
			resultList.add(folderDomain);
		}

		return resultList;
	}

	/**
	 * フォルダを登録します。
	 *
	 * @param folderDomain フォルダドメイン
	 * @param placeDomain ワークスペースドメインまたはフォルダドメイン
	 * @return フォルダドメイン
	 * @throws Exception
	 */
	public FolderDomain create(FolderDomain folderDomain, PlaceDomain placeDomain) throws Exception {
		if (folderDomain == null) {
			// 引数folderDomainとしてnullが入力されました。
			throw new EIMException("EIM.ERROR.LOGIC.OBJECT.VALUE.ILLEGAL");
		}

		if (placeDomain == null) {
			// 引数parentDomainとしてnullが入力されました。
			throw new EIMException("EIM.ERROR.LOGIC.OBJECT.VALUE.ILLEGAL");
		}

		// 親オブジェクト
		ObjectDomain parentObject = objectService.getById(placeDomain.getId());
		if (parentObject == null) {
			// 選択したフォルダは存在しません。
			throw new EIMException("EIM.ERROR.LOGIC.NOFOLDER");
		}

		// Windows禁止文字チェック
		AppDocumentUtil.checkValidateFName(folderDomain.getName());

		ObjectTypeDomain objType = folderDomain.getType();

		// objTypeとオブジェクトタイプ定義名称が両方設定されている場合
		if (objType != null && !StringUtils.isEmpty(objType.getDefinitionName())) {
			objType = objectTypeService.getByDefinitionName(objType.getDefinitionName());
		} else if (objType != null && StringUtils.isEmpty(objType.getDefinitionName()) && objType.getId() > 0) {
			// idのみが設定されている場合
			objType = objectTypeService.getById(objType.getId());
		} else {
			// 以外の場合
			objType = objectTypeService.getByDefinitionName(objectTypeName);
		}
		// フォルダタイプチェック
		if (!AppDocumentLogicUtil.isTypeOfFolder(objType)) {
			throw new EIMException("EIM.ERROR.LOGIC.NOFOLDERTYPE");
		}
		folderDomain.setType(objType);

		// リスト属性の場合の登録値チェック
		checkValueIncludeInListDef(folderDomain);

		// ワークフロー付フォルダ配下にワークフロー付フォルダタイプを指定した場合
		if ((AppDocumentLogicUtil.isTypeOfFolderWithWorkflow(parentObject) || AppDocumentLogicUtil.isTypeOfFolderUnderFolderWithWorkflow(parentObject))
				&& workflowService.getByObjectType(objType) != null) {
			throw new EIMException("EIM.ERROR.LOGIC.CANTSET.UNDERWF.WFFOLDER");
		}

		// ステータスチェック
		if (parentObject.getStatus() != null) {
			// 親オブジェクトのステータスが「編集中」以外の場合はエラー
			if (parentObject.getStatus().getType().getBase().getId() != AppConstant.STATUS_TYPE_KIND_ID_EDITTING) {
				// 作成権限がありません
				throw new EIMException("EIM.ERROR.LOGIC.NOCREATEROLE");
			}
		}

		// 属性についてidからdefinitionNameの補完
		compAttrDefinitionName(folderDomain);

		String path = AppDocumentUtil.getPath(parentObject);
		if (path == null) {
			// ワークスペースの場合、パス属性の値を保持していない
			path = "/";
		}
		path += parentObject.getName() + "/";

		ObjectDomain objectDomain = new ObjectDomain();
		FolderDomain folder = new FolderDomain();

		try {
			BeanUtils.copyProperties(objectDomain, folderDomain);
			objectDomain.setAttributeList(new ArrayList<AttributeDomain>());

			// 指定されたフォルダオブジェクトの作成
			objectDomain = objectService.create(objectDomain, DuplicateCheckModeEnum.NONE);
			// baseの情報が無いため再取得
			objectDomain = objectService.getById(objectDomain.getId());

			// 「ドキュメント」リレーションタイプ
			RelationTypeDomain relationTypeDomain = AppDocumentLogicUtil.getDocumentRelType();

			// リレーション作成
			RelationDomain relationDomain = new RelationDomain();
			relationDomain.setType(relationTypeDomain);
			relationDomain.setParent(parentObject);
			relationDomain.setChild(objectDomain);
			//フォルダ名称で重複チェックが正しく処理されない為、コメントアウト
			//relationService.create(relationDomain, DuplicateCheckModeEnum.INHERITTYPE);
			RelationUtils.createRelation(relationDomain);

			objectDomain.setAttributeList(folderDomain.getAttributeList());

			// 属性情報の更新
			AttributeUtil.updateAttribute(objectDomain, relationTypeDomain);

			// 「パス」属性値を設定
			AppDocumentUtil.setPath(objectDomain, path);

			// 各属性の初期値を設定
			ObjectTypeDomain attributeTypeObjectType = new ObjectTypeDomain(ConfigUtils.getByKey("OBJECT_TYPE_NAME_ATTRIBUTETYPE"));
			for (AttributeTypeDomain attributeType : objType.getAttributeTypeList()) {
				AttributeDomain objAttr = objectDomain.getAttribute(attributeType.getDefinitionName());
				if (objAttr == null) {
					 objAttr = getAttributeById(objectDomain.getAttributeList() , String.valueOf(objectDomain.getId()));
				}
				AttributeDomain attr = getInitialAttributeDomain(attributeTypeObjectType, attributeType, objAttr);
				if (attr != null) AppDocumentUtil.setAttr(objectDomain, attr);
			}

			// セキュリティーを設定
			SecurityDomain parentSecurityDomain = parentObject.getSecurity();
			SecurityDomain securityDomain = folderDomain.getSecurity();

			if (securityDomain != null) {
				objectService.setSecurity(objectDomain, securityDomain);
			}else{
				objectService.setSecurity(objectDomain, parentSecurityDomain);
			}

			// WF付フォルダ直下に作成する場合
			if (AppDocumentLogicUtil.isTypeOfFolderWithWorkflow(parentObject)) {
				// 属性「上位WFフォルダ」設定
				AppDocumentUtil.setAttrLong(objectDomain, ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_HIGHER_WF_FOLDER"), parentObject.getId());
				// 上位WF付フォルダのステータスを設定(引継ぐ)
				objectDao.setStatus(objectDomain, parentObject.getStatus());
			} else {
				// 親フォルダの「上位WFフォルダ」属性を取得
				long higherWfFolderId = AppDocumentUtil.getIntAttr(parentObject, ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_HIGHER_WF_FOLDER"));
				if (higherWfFolderId != 0) {
					// 属性「上位WFフォルダ」設定
					AppDocumentUtil.setAttrLong(objectDomain, ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_HIGHER_WF_FOLDER"), higherWfFolderId);
					// 上位WFフォルダ取得
					ObjectDomain higherWfFolder = objectService.getById(higherWfFolderId);
					// 上位WF付フォルダのステータスを設定(引継ぐ)
					objectDao.setStatus(objectDomain, higherWfFolder.getStatus());
				}
			}

			// WSのフォルダドメインをFWのオブジェクトドメインに変換する
			objectDomain = objectService.getById(objectDomain.getId());
			folder = getFolderByObjectDomain(objectDomain);

			// Access History - 「新規登録」
			AccessUtils.createAccess(objectDomain, "EIM.ACCESS.TYPE.INITIALREGIST");

			//操作履歴 AOP利用
			//OperationHistoryUtils.createOperationHistory(objectDomain, 2, 2005, 1, 6, objectDomain.getId(), objectDomain.getName(),
			//											-1, -1, -1, null, objectDomain.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PASS")).getStringList().get(0));

		} catch (Exception e) {
			throw e;
		}

		return folder;
	}

	/**
	 * フォルダの名称と属性、セキュリティを更新します。
	 *
	 * @param folderDomain フォルダドメイン
	 * @throws Exception
	 */
	public void update(FolderDomain folderDomain) throws Exception {
		if (folderDomain == null) {
			// 引数folderDomainとしてnullが入力されました。
			throw new EIMException("EIM.ERROR.LOGIC.OBJECT.VALUE.ILLEGAL");
		}

		// オブジェクトの取得
		ObjectDomain objectDomain = objectService.getById(folderDomain.getId());
		if (objectDomain == null) {
			// オブジェクトが存在しない、エラー処理
			throw new EIMException("EIM.ERROR.LOGIC.NOTEXIST.DOCFOLWS");
		}

		boolean isChangeName = false;
		if (!StringUtils.isEmpty(folderDomain.getName()) && !folderDomain.getName().equals(objectDomain.getName())) {
			isChangeName = true;
		}

		// オブジェクトタイプの取得
		ObjectTypeDomain objectTypeDomain = objectDomain.getType();
		if (objectTypeDomain != null) {
			// オブジェクトタイプチェック
			if (!AppDocumentLogicUtil.isTypeOfFolder(objectTypeDomain)) {
				throw new EIMException("EIM.ERROR.LOGIC.NOTEXIST.DOCFOLWS");
			}
		}

		// リスト属性の場合の登録値チェック
		checkValueIncludeInListDef(folderDomain);

		// 属性についてidからdefinitionNameの補完
		compAttrDefinitionName(folderDomain);

		try {
			// 「ドキュメント」リレーションタイプ
			RelationTypeDomain relType = AppDocumentLogicUtil.getDocumentRelType();

			// オブジェクト名が変更された場合
			if (isChangeName) {
				objectDomain.setName(folderDomain.getName());
				// 改名処理
				AppDocumentLogicUtil.renameObject(objectDomain, relType);
				// Access History - 「改名」
				AccessUtils.createAccess(objectDomain, "EIM.ACCESS.TYPE.CHANGENAME");
				//操作履歴
				OperationHistoryUtils.createOperationHistory(objectDomain, 2, 2017, 3, 12, objectDomain.getId(), objectDomain.getName(),
															-1, -1, -1, null, objectDomain.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PASS")).getStringList().get(0));
			}

			SecurityDomain security = objectDomain.getSecurity();
			SecurityDomain newSecurity = folderDomain.getSecurity();
			if (newSecurity != null && security.getId() != newSecurity.getId()) {
				objectDomain.setSecurity(folderDomain.getSecurity());
				AppDocumentLogicUtil.applySecurity(relType, objectDomain, newSecurity);

				//操作履歴
				OperationHistoryUtils.createOperationHistory(objectDomain, 2, 2019, 3, 12, objectDomain.getId(), objectDomain.getName(),
															24, 9, newSecurity.getId(), newSecurity.getDefinitionName(), objectDomain.getAttribute(ConfigUtils.getByKey("ATTR_NAME_WORKSPACE_PASS")).getStringList().get(0));
			}
			objectDomain.setAttributeList(folderDomain.getAttributeList());

			// 属性情報の更新
			AttributeUtil.updateAttribute(objectDomain, relType);

			// Access History - 「属性更新」
			AccessUtils.createAccess(objectDomain, "EIM.ACCESS.TYPE.ATTRIBUTEUPDATE");

			//操作履歴
			OperationHistoryUtils.createOperationHistory(objectDomain, 2, 2018, 3, 12, objectDomain.getId(), objectDomain.getName(),
														-1, -1, -1, null, objectDomain.getAttribute(ConfigUtils.getByKey("ATTR_NAME_WORKSPACE_PASS")).getStringList().get(0));


		} catch (Exception e) {
			throw e;
		}
	}

	/**
	 * 入力規則がリスト定義の属性の登録値がリスト定義内に存在するかどうかをチェックする。
	 *
	 * @param folderDomain
	 * @return
	 * @throws Exception
	 */
	private void checkValueIncludeInListDef(FolderDomain folderDomain) throws Exception{

		EIMSession sess = jp.co.ctc_g.eim.framework.common.util.EIMThreadContext.getEIMSession();

		for (AttributeDomain attrDomain : folderDomain.getAttributeList()) {

			AttributeValueMaster attributeValueMaster = AttributeMasterUtil.getAttributeValueMasterByAttTypeId(sess,attrDomain.getAttributeType().getId());
			if (!Objects.isNull(attributeValueMaster)) {
				switch (attrDomain.getAttributeType().getValueType()) {
				// 数値型の場合
				case LONG:
					List<Long> masterLongList = new ArrayList<Long>();
					for (long val: attributeValueMaster.getInts()) {
						masterLongList.add(val);
					}
					for (Long val: attrDomain.getLongList()) {
						if (!masterLongList.contains(val)) {
							throw new EIMException("EIM.ERROR.LOGIC.ATTRIBUTE.VALUETYPE.LIST.VALUE.ILLEGAL",attrDomain.getAttributeType().getDefinitionName(),val);
						}
					}
					break;
				// DATE型の場合
				case DATE:
					for (Date val: attrDomain.getDateList()) {
						if (!Arrays.asList(attributeValueMaster.getDates()).contains(val)) {
							SimpleDateFormat sdf = new SimpleDateFormat(ResourceUtils.getByKey("EIM.FORMAT.DATE"));
							throw new EIMException("EIM.ERROR.LOGIC.ATTRIBUTE.VALUETYPE.LIST.VALUE.ILLEGAL",attrDomain.getAttributeType().getDefinitionName(),sdf.format(val));
						}
					}
					break;
				// DOUBLE型の場合
				case DOUBLE:
					List<Double> masterDoubleList = new ArrayList<Double>();
					for (double val: attributeValueMaster.getDoubles()) {
						masterDoubleList.add(val);
					}
					for (double val: attrDomain.getDoubleList()) {
						if (!masterDoubleList.contains(val)) {
							throw new EIMException("EIM.ERROR.LOGIC.ATTRIBUTE.VALUETYPE.LIST.VALUE.ILLEGAL",attrDomain.getAttributeType().getDefinitionName(),String.valueOf(val));
						}
					}
					break;
				// STRING型の場合
				case STRING:
					for (String val: attrDomain.getStringList()) {
						if (!Arrays.asList(attributeValueMaster.getStrings()).contains(val)) {
							throw new EIMException("EIM.ERROR.LOGIC.ATTRIBUTE.VALUETYPE.LIST.VALUE.ILLEGAL",attrDomain.getAttributeType().getDefinitionName(),val);
						}
					}
					break;
				// TEXT型の場合
				case TEXT:
					for (String val: attrDomain.getTextList()) {
						if (!Arrays.asList(attributeValueMaster.getTexts()).contains(val)) {
							throw new EIMException("EIM.ERROR.LOGIC.ATTRIBUTE.VALUETYPE.LIST.VALUE.ILLEGAL",attrDomain.getAttributeType().getDefinitionName(),val);
						}
					}
					break;
				case CODE:
				case OBJECT:
				case USER:
				default:
					break;
				}
			}
		}
	}

	/**
	 * 属性idのみが設定されており、definitionNameがnullのデータについて、definitionNameとvaluetypeを補完する。
	 *
	 * @param documentDomain
	 * @return
	 * @throws Exception
	 */
	private void compAttrDefinitionName(Object obj) throws Exception {
		if (obj instanceof FolderDomain) {
			FolderDomain folderDomain = (FolderDomain)obj;
			if (!Objects.isNull(folderDomain.getAttributeList())) {
				for (AttributeDomain domain:folderDomain.getAttributeList()) {
					if (Objects.isNull(domain.getAttributeType().getDefinitionName()) && domain.getAttributeType().getId() != 0) {
						AttributeTypeDomain attributeType = attributeTypeService.getById(domain.getAttributeType().getId());
						domain.getAttributeType().setDefinitionName(attributeType.getDefinitionName());
						domain.getAttributeType().setValueType(attributeType.getValueType());
					}

				}
			}
		} else if (obj instanceof FolderCriteria){
			FolderCriteria folderCriteria = (FolderCriteria)obj;
			if (!Objects.isNull(folderCriteria.getAttributeList())) {
				for (AttributeDomain domain:folderCriteria.getAttributeList()) {
					if (Objects.isNull(domain.getAttributeType().getDefinitionName()) && domain.getAttributeType().getId() != 0) {
						AttributeTypeDomain attributeType = attributeTypeService.getById(domain.getAttributeType().getId());
						domain.getAttributeType().setDefinitionName(attributeType.getDefinitionName());
						domain.getAttributeType().setValueType(attributeType.getValueType());
					}

				}
			}
		}
	}

	/**
	 * 指定したフォルダをごみ箱に移動します。
	 *
	 * @param folderDomainList フォルダドメインリスト
	 * @throws Exception
	 */
	public void dispose(List<FolderDomain> folderDomainList) throws Exception {
		if (folderDomainList == null) {
			// 引数folderDomainListとしてnullが入力されました。
			throw new EIMException("EIM.ERROR.LOGIC.OBJECT.VALUE.ILLEGAL");
		}
		// 「ドキュメント」リレーションタイプ
		RelationTypeDomain relationTypeDomain = AppDocumentLogicUtil.getDocumentRelType();

		// システムごみ箱オブジェクト
		ObjectTypeDomain systemRecycleObjType = AppDocumentLogicUtil.getRecycleObjectType();
		ObjectDomain systemRecycleObj = objectService.getByTypeAndName(systemRecycleObjType, ConfigUtils.getByKey("OBJECT_TYPE_NAME_RECYCLE"));
		if (systemRecycleObj == null) {
			throw new EIMException("EIM.ERROR.LOGIC.NORECYCLEBOX");
		}

		ObjectTypeDomain folderObjType = AppDocumentLogicUtil.getFolderObjectType();
		List<Long> folderObjTypeIds = AppDocumentLogicUtil.getChildObjectTypeList(folderObjType.getId());

		// 渡されてきたオブジェクト分ループ
		for (FolderDomain folderDomain : folderDomainList) {
			ObjectDomain objectDomain = objectService.getById(folderDomain.getId());
			if (objectDomain == null) {
				throw new EIMException("EIM.ERROR.LOGIC.NODOCORFOLORTAG");
			}

			// オブジェクトタイプチェック
			if (!folderObjTypeIds.contains(objectDomain.getType().getId())) {
				throw new EIMException("EIM.ERROR.LOGIC.NOTEXIST.DOCFOLWS");
			}

			// 選択対象がタグが付与されたドキュメント・フォルダ・タグの場合は削除できない。
			if (AppDocumentLogicUtil.isTagAssignedObject(objectDomain, relationTypeDomain, true)) {
				throw new EIMException("EIM.ERROR.LOGIC.CANTDELTAGGEDDOCFOLORTAG");
			}

			if(objectDomain.getStatus() != null) {
				if(objectDomain.getStatus().getType().getBase().getId() != AppConstant.STATUS_TYPE_KIND_ID_EDITTING &&
						objectDomain.getStatus().getType().getBase().getId() != AppConstant.STATUS_TYPE_KIND_ID_PUBLIC) {
					//・WF付フォルダのステータスが編集中、公開済み以外はエラー
					throw new EIMException("EIM.ERROR.LOGIC.CANT.DELETE.STATUS");
				}
			}

			//子フォルダにWF付フォルダがあるか確認
			List<ObjectDomain> childList = AppDocumentUtil.getChildObjectByRelTypeAndParent(relationTypeDomain,objectDomain, null);
			for(ObjectDomain childObjectDomain : childList){
				if (folderObjTypeIds.contains(childObjectDomain.getType().getId())) {
					if(objectDomain.getStatus() != null) {
						if(objectDomain.getStatus().getType().getBase().getId() != AppConstant.STATUS_TYPE_KIND_ID_EDITTING &&
								objectDomain.getStatus().getType().getBase().getId() != AppConstant.STATUS_TYPE_KIND_ID_PUBLIC) {
							//・WF付フォルダのステータスが編集中、公開済み以外はエラー
							throw new EIMException("EIM.ERROR.LOGIC.CANT.DELETE.STATUS");
						}
					}
				}
			}

			List<RelationDomain> relList = AppDocumentUtil.getParentRelationByRelTypeAndChild(relationTypeDomain, objectDomain,
					new AccessRoleTypeDomain("READ"));
			if (relList == null || relList.size() == 0) {
				throw new EIMException("EIM.ERROR.LOGIC.NOFOLDER");
			}
			ObjectDomain parentObj = relList.get(0).getParent();

			// フォルダ配下のタグを再帰的に取得して、タグの物理削除を行う
			AppDocumentLogicUtil.physicalDeleteTagUnderFolder(objectDomain, relationTypeDomain);

			// ワークスペース固有ごみ箱オブジェクト
			List<ObjectDomain> wsRecycleObjList = new ArrayList<>();
			SearchSelectObject selectObject = new SearchSelectObject();
			SearchConditionBuildHelper helper = new SearchConditionBuildHelper();
			SearchConditionGroup conditionGroup = helper.group(helper.opAnd());

			// ワークスペース固有ごみ箱オブジェクトタイプを取得
			ObjectTypeDomain wsRecycleObjType = objectTypeService.getByDefinitionName(EIMConfig.get("OBJECT_TYPE_NAME_WORKSPACERECYCLE"));

			// オブジェクトタイプを条件に設定
			conditionGroup.addCondition(new SearchConditionCompare(helper.opAnd(), SearchSelectObject.PsedoAttributeTypeEnum.TYPE, helper.opEq(), wsRecycleObjType.getId()));

			// 属性名「パス」
			String folderPass = EIMConfig.get("ATTR_NAME_FOLDER_PASS");

			// パス属性を取得
			AttributeTypeDomain pathAttrType = attributeTypeService.getByDefinitionName(folderPass);

			// 削除対象オブジェクトワークスペース固有ごみ箱オブジェクトのパスを取得
			String path = objectDomain.getAttribute(folderPass).getStringList().get(0);
			String [] strs = path.split("/");
			String wsRecyclePath = "/" + strs[1] + "/";

			// パスを条件に設定
			conditionGroup.addCondition(new SearchConditionCompare(helper.opAnd(), pathAttrType, SearchOperatorEnum.EQ, wsRecyclePath));

			selectObject.setCondition(conditionGroup);

			// 上限なし
			SearchLimitCountCondition limitCondition = new SearchLimitCountCondition(-2, true);

			wsRecycleObjList = objectService.getList(selectObject, limitCondition);
			if (wsRecycleObjList == null || wsRecycleObjList.size() == 0) {
				throw new EIMException("EIM.ERROR.LOGIC.NOWSRECYCLEBOX");
			}

			// ごみ箱オブジェクト
			ObjectDomain recycleObj = null;

			/* 論理削除  */
			if (!AppDocumentUtil.isObjectInWsRecycle(objectDomain)) {
				// ワークスペース固有ごみ箱へ移動
				recycleObj = wsRecycleObjList.get(0);
				// Access History - 「ワークスペース固有ごみ箱へ削除」
				AccessUtils.createAccess(objectDomain, "EIM.ACCESS.TYPE.DELETETOWSRECBOX");
			} else {
				// システムのごみ箱へ移動
				recycleObj = systemRecycleObj;
				// Access History - 「ごみ箱へ削除」
				AccessUtils.createAccess(objectDomain, "EIM.ACCESS.TYPE.DELETETORECBOX");
			}
			// ごみ箱への関連付け
			RelationDomain relation = new RelationDomain();
			relation.setType(relationTypeDomain);
			relation.setParent(recycleObj);
			relation.setChild(objectDomain);
			RelationDomain recycleboxRelation = relationService.create(relation);

			// 属性情報更新処理
			AttributeUtil.updateAttributeForDelete(objectDomain, parentObj);

			// リレーションの削除
			List<RelationDomain> parentRelList = AppDocumentUtil.getParentRelationByRelTypeAndChild(relationTypeDomain, objectDomain,
					new AccessRoleTypeDomain("READ"));
			if (parentRelList != null) {
				for (int i = 0; i < parentRelList.size(); i++) {
					RelationDomain parentRelation = parentRelList.get(i);
					if (parentRelation.getId() != recycleboxRelation.getId()) {
						relationService.delete(parentRelation);
					}
				}
			}

			// SearchFramework 検索FW更新通知
			EIMSession sess = jp.co.ctc_g.eim.framework.common.util.EIMThreadContext.getEIMSession();
			EIMObject eimObj = ObjectUtils.getObjectById(sess, folderDomain.getId());
			EIMObject parentEimObj = ObjectUtils.getObjectById(sess, parentObj.getId());

			// フォルダ 対象フォルダ + 配下のフォルダ・ドキュメント・タグ + 親フォルダ・親ワークスペース
			// 配下のタグを付与されたドキュメント・フォルダ・タグについてはTagUtilで通知
			AppUpdateNoticeUtils.updateNoticeInsert(eimObj.getId(), "SEARCHFW_LOGICDEL_FOLDER");
			AppUpdateNoticeUtils.updateNoticeInsertChildRecursive(sess, eimObj, "SEARCHFW_LOGICDEL_CHILD_FOLDER", "SEARCHFW_LOGICDEL_CHILD_DOCUMENT", null, "SEARCHFW_LOGICDEL_CHILD_DOCUMENTLINK");
			AppUpdateNoticeUtils.updateNoticeInsertParent(sess, parentEimObj, "SEARCHFW_LOGICDEL_PARENT_FOLDER", "SEARCHFW_LOGICDEL_PARENT_WORKSPACE", null);

			//操作履歴
			//OperationHistoryUtils.createOperationHistory(objectDomain, 2, 2032, 2, 12, objectDomain.getId(), objectDomain.getName(),
			//											-1, -1, -1, null, objectDomain.getAttribute(ConfigUtils.getByKey("ATTR_NAME_WORKSPACE_PASS")).getStringList().get(0));
		}
	}

	/**
	 * 指定したフォルダを削除します。
	 *
	 * @param folderDomainList フォルダドメインリスト
	 * @throws Exception
	 */
	@SuppressWarnings("unchecked")
	public void delete(List<FolderDomain> folderDomainList) throws Exception {
		if (folderDomainList == null) {
			// 引数folderDomainListとしてnullが入力されました。
			throw new EIMException("EIM.ERROR.LOGIC.OBJECT.VALUE.ILLEGAL");
		}
		// 「ドキュメント」リレーションタイプ
		RelationTypeDomain relationTypeDomain = AppDocumentLogicUtil.getDocumentRelType();

		ObjectTypeDomain folderObjType = AppDocumentLogicUtil.getFolderObjectType();
		List<Long> folderObjTypeIds = AppDocumentLogicUtil.getChildObjectTypeList(folderObjType.getId());

		// 渡されてきたオブジェクト分ループ
		for (FolderDomain folderDomain : folderDomainList) {
			ObjectDomain objectDomain = objectService.getById(folderDomain.getId());
			if (objectDomain == null) {
				throw new EIMException("EIM.ERROR.LOGIC.NODOCORFOLORTAG");
			}

			// オブジェクトタイプチェック
			if (!folderObjTypeIds.contains(objectDomain.getType().getId())) {
				throw new EIMException("EIM.ERROR.LOGIC.NOTEXIST.DOCFOLWS");
			}

			// 選択対象がタグが付与されたドキュメント・フォルダ・タグの場合は削除できない。
			if (AppDocumentLogicUtil.isTagAssignedObject(objectDomain, relationTypeDomain, true)) {
				throw new EIMException("EIM.ERROR.LOGIC.CANTDELTAGGEDDOCFOLORTAG");
			}

			List<RelationDomain> relList = AppDocumentUtil.getParentRelationByRelTypeAndChild(relationTypeDomain, objectDomain,
					new AccessRoleTypeDomain("READ"));
			if (relList == null || relList.size() == 0) {
				throw new EIMException("EIM.ERROR.LOGIC.NOFOLDER");
			}

			ObjectDomain parentObj = relList.get(0).getParent();
			AttributeUtil.checkStatus(objectDomain, parentObj);

			// フォルダの場合は再帰的に全内容を物理削除する
			relationService.delete(relList.get(0));

			// フォルダ削除。フォルダツリーを再帰して削除実行
			AppDocumentLogicUtil.processFolderTree(relationTypeDomain, objectDomain);

			//操作履歴 AOP利用
			//OperationHistoryUtils.createOperationHistory(objectDomain, 2, 2069, 2, 12, objectDomain.getId(), objectDomain.getName(),
			//											-1, -1, -1, null, objectDomain.getAttribute(ConfigUtils.getByKey("ATTR_NAME_WORKSPACE_PASS")).getStringList().get(0));

		}
	}

	/**
	 * 指定したフォルダをPlaceDomainの下に移動します。
	 *
	 * @param folderDomain フォルダドメイン
	 * @param placeDomain ワークスペースドメインまたはフォルダドメイン
	 * @throws Exception
	 */
	public void move(FolderDomain folderDomain, PlaceDomain placeDomain) throws Exception {
		if (folderDomain == null) {
			// 引数folderDomainとしてnullが入力されました。
			throw new EIMException("EIM.ERROR.LOGIC.OBJECT.VALUE.ILLEGAL");
		}

		if (placeDomain == null) {
			// 引数parentDomainとしてnullが入力されました。
			throw new EIMException("EIM.ERROR.LOGIC.OBJECT.VALUE.ILLEGAL");
		}

		// オブジェクトの取得
		ObjectDomain objectDomain = objectService.getById(folderDomain.getId());
		if (objectDomain == null) {
			// オブジェクトが存在しない、エラー処理
			throw new EIMException("EIM.ERROR.LOGIC.NOTEXIST.DOCFOLWS");
		}

		// オブジェクトタイプの取得
		ObjectTypeDomain objectTypeDomain = objectDomain.getType();
		if (objectTypeDomain != null) {
			// オブジェクトタイプチェック
			if (!AppDocumentLogicUtil.isTypeOfFolder(objectTypeDomain)) {
				throw new EIMException("EIM.ERROR.LOGIC.NOTEXIST.DOCFOLWS");
			}
		}

		// 親オブジェクト
		ObjectDomain parentObject = objectService.getById(placeDomain.getId());
		if (parentObject == null) {
			// 選択したフォルダは存在しません。
			throw new EIMException("EIM.ERROR.LOGIC.NOFOLDER");
		}

		// 移動先がゴミ箱か否かをチェック
		if (AppDocumentUtil.isObjectInRecycle(parentObject)) {
			throw new EIMException("EIM.ERROR.LOGIC.SELECTFOLDERDELETED");
		}

		// 親オブジェクトのパス
		String path = AppDocumentUtil.getPath(parentObject);
		if (path == null) {
			// ワークスペースの場合、パス属性の値を保持していない
			path = "/";
		}
		path += parentObject.getName() + "/";

		// リレーションタイプ
		RelationTypeDomain relationTypeDomain = AppDocumentLogicUtil.getDocumentRelType();

		// 移動先フォルダが自分自身の場合はエラー
		if (parentObject.getId() == objectDomain.getId()) {
			throw new EIMException("EIM.ERROR.INPUT.NOTPASTESAMEFOLDER");
		}

		// 移動先フォルダが切り取ったフォルダに属する場合はエラー
		if (AppDocumentUtil.isChildFolder(relationTypeDomain, parentObject, objectDomain)) {
			// 貼付け先のフォルダは、選択フォルダに属するフォルダです。
			throw new EIMException("EIM.ERROR.INPUT.NOTPASTEFOLDER");
		}

		try {
			// 対象がごみ箱内か判定
			boolean isParentRecycleBox = AppDocumentUtil.isObjectInRecycleWithoutRecycle(objectDomain);

			// 属性情報の更新処理
			AttributeUtil.updateAttributeForMove(objectDomain, parentObject, relationTypeDomain, path, isParentRecycleBox);

			// Delete Parent Relation
			List<RelationDomain> parentRelList = AppDocumentUtil.getParentRelationByRelTypeAndChild(relationTypeDomain, objectDomain,
					new AccessRoleTypeDomain("READ"));
			if (parentRelList != null) {
				for (RelationDomain relationDomain : parentRelList) {
					relationService.delete(relationDomain);
				}
			}
			
			// 親オブジェクトとのリレーションを作成
			RelationDomain relationDomain = new RelationDomain();
			relationDomain.setType(relationTypeDomain);
			relationDomain.setParent(parentObject);
			relationDomain.setChild(objectDomain);
			//重複チェック処理を修正
			//relationService.create(relationDomain, DuplicateCheckModeEnum.INHERITTYPE);
			RelationUtils.createRelation(relationDomain);

			if (isParentRecycleBox) {
				// ブランチ情報の復帰
				RelationTypeDomain relTypeBranch = relationTypeService.getByDefinitionName(ConfigUtils.getByKey("RELATION_TYPE_NAME_BRANCH"));
				// フォルダの場合
				AppDocumentLogicUtil.returnFolderBranch(objectDomain, relTypeBranch, relationTypeDomain);
			}

			if (isParentRecycleBox) {
				if (AppDocumentUtil.isObjectInWsRecycle(objectDomain)) {
					// Access History - 「ワークスペースごみ箱から復帰」
					AccessUtils.createAccess(objectDomain, "EIM.ACCESS.TYPE.BACKFROMWSRECBOX");
				} else {
					// Access History - 「ごみ箱から復帰」
					AccessUtils.createAccess(objectDomain, "EIM.ACCESS.TYPE.BACKFROMRECBOX");
				}
			} else {
				// Access History - 「移動」
				AccessUtils.createAccess(objectDomain, "EIM.ACCESS.TYPE.MOVE");
			}

			//操作履歴 AOP利用
			//OperationHistoryUtils.createOperationHistory(objectDomain, 2, 2016, 3, 12, objectDomain.getId(), objectDomain.getName(),
			//											-1, -1, -1, null, objectDomain.getAttribute(ConfigUtils.getByKey("ATTR_NAME_WORKSPACE_PASS")).getStringList().get(0));

		} catch (Exception e) {
			throw e;
		}

	}

	/**
	 * WSのフォルダドメインをFWのオブジェクトドメインに変換します。
	 *
	 * @param objectDomain
	 * @return
	 * @throws Exception
	 */
	private FolderDomain getFolderByObjectDomain(ObjectDomain objectDomain) throws Exception {
		FolderDomain folderDomain = new FolderDomain();
		BeanUtils.copyProperties(folderDomain, objectDomain);

		return folderDomain;
	}

	/**
	 * WSのドキュメントドメインをFWのオブジェクトドメインに変換します。
	 *
	 * @param objectDomain
	 * @return
	 * @throws Exception
	 */
	private DocumentDomain getDocumentByObjectDomain(ObjectDomain objectDomain) throws Exception {
		DocumentDomain documentDomain = new DocumentDomain();
		BeanUtils.copyProperties(documentDomain, objectDomain);

		// 作成者の設定
		UserDomain objectUser = objectDomain.getCreationUser();
		AttributeDomain createUserAttDoamin = objectDomain.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_CREATE"));
		if (objectUser != null && createUserAttDoamin != null) {

			if (createUserAttDoamin.getLong() > 0 && (objectUser.getId() != createUserAttDoamin.getLong())) {
				// userIDからUserDomainを取得
				UserDomain getUserDomain = userService.getById(createUserAttDoamin.getLong());
				documentDomain.setCreateUser(getUserDomain);
			} else {
				// objectDomainの作成者をDocumentDomainの作成者に設定
				documentDomain.setCreateUser(objectUser);
			}
		}

		return documentDomain;
	}

	/**
	 * 子階層フォルダ、ドキュメントのデータを取得します。
	 *
	 * @param workSpaceDomain
	 * @param objectDomain
	 * @return
	 * @throws Exception
	 */
	private FolderDomain getFolderAndDocumentList(FolderDomain folderDomain, ObjectDomain objectDomain) throws Exception {
		FolderDomain result = folderDomain;

		if (itemListForSingleTargetMethod == null || itemListForSingleTargetMethod.contains(WorkspaceItemEnum.FOLDER_LIST)
				|| itemListForSingleTargetMethod.contains(WorkspaceItemEnum.DOCUMENT_LIST)) {

			// 「ドキュメント」リレーションタイプ
			RelationTypeDomain relationTypeDomain = AppDocumentLogicUtil.getDocumentRelType();
			AccessRoleTypeDomain accessRoleType = new AccessRoleTypeDomain("READ");
			accessRoleType.setId(EIMAccessRole.READ);
			List<ObjectDomain> objList = AppDocumentUtil.getChildObjectByRelTypeAndParent(
					relationTypeDomain, objectDomain ,accessRoleType);
			//常時読取権限を保持しているか確認
			boolean isAccessable = objectService.authorized(objectDomain, new AccessRoleTypeDomain("ROLE_500"));
			boolean readOnlyAccessFlag = false;
			HashMap<Long, String> noSTPublicObjMap = new HashMap<Long,String>();	// 公開アイコン表示判定用Map

			if(!isAccessable){
				//常時読取権限が無い場合(公開読取権限のみ場合)
				readOnlyAccessFlag = true;
				//下のフォルダとドキュメント仕分け処理の中で公開ファイルのみを選別する
				noSTPublicObjMap = AppDocumentLogicUtil.getPublicDocumentMap(objList);
			}

			List<FolderDomain> folderObjList = new ArrayList<FolderDomain>();
			List<DocumentDomain> documentObjList = new ArrayList<DocumentDomain>();
			if (objList != null && objList.size() > 0) {
				ObjectTypeDomain folderObjType = AppDocumentLogicUtil.getFolderObjectType();
				List<Long> folderObjTypeIds = AppDocumentLogicUtil.getChildObjectTypeList(folderObjType.getId());

				ObjectTypeDomain docObjType = AppDocumentLogicUtil.getDocumentObjectType();
				List<Long> docObjTypeIds = AppDocumentLogicUtil.getChildObjectTypeList(docObjType.getId());

				for (ObjectDomain object : objList) {
					// 取得したオブジェクトを「ドキュメント」と「フォルダ」に分ける
					if (itemListForSingleTargetMethod == null || itemListForSingleTargetMethod.contains(WorkspaceItemEnum.FOLDER_LIST)) {
						if (folderObjTypeIds.contains(object.getType().getId())) {

							FolderDomain folder = getFolderByObjectDomain(object);
							folderObjList.add(folder);
							continue;
						}
					}
					if (itemListForSingleTargetMethod == null || itemListForSingleTargetMethod.contains(WorkspaceItemEnum.DOCUMENT_LIST)) {
						if (docObjTypeIds.contains(object.getType().getId())) {

							if(readOnlyAccessFlag){
								if(!noSTPublicObjMap.containsKey(object.getId())){
									//公開ドキュメントが存在しない場合は返却対象にしない
									continue;
								}
							}

							DocumentDomain documentDomain = getDocumentByObjectDomain(object);
							documentObjList.add(documentDomain);
							continue;
						}
					}
				}
			}
			result.setFolderList(folderObjList);
			result.setDocumentList(documentObjList);
		}
		return result;
	}

	/**
	 * @return objectDao
	 */
	public ObjectDao getObjectDao() {
		return objectDao;
	}

	/**
	 * @return objectService
	 */
	public ObjectService getObjectService() {
		return objectService;
	}

	/**
	 * @param objectService セットします objectService
	 */
	public void setObjectService(ObjectService objectService) {
		this.objectService = objectService;
	}

	/**
	 * @param objectDao セットします objectDao
	 */
	public void setObjectDao(ObjectDao objectDao) {
		this.objectDao = objectDao;
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
	 * @return relationTypeService
	 */
	public RelationTypeService getRelationTypeService() {
		return relationTypeService;
	}

	/**
	 * @param relationTypeService セットします relationTypeService
	 */
	public void setRelationTypeService(RelationTypeService relationTypeService) {
		this.relationTypeService = relationTypeService;
	}

	/**
	 * @return relationService
	 */
	public RelationService getRelationService() {
		return relationService;
	}

	/**
	 * @param relationService セットします relationService
	 */
	public void setRelationService(RelationService relationService) {
		this.relationService = relationService;
	}

	/**
	 * @return attributeTypeService
	 */
	public AttributeTypeService getAttributeTypeService() {
		return attributeTypeService;
	}

	/**
	 * @param attributeTypeService セットします attributeTypeService
	 */
	public void setAttributeTypeService(AttributeTypeService attributeTypeService) {
		this.attributeTypeService = attributeTypeService;
	}

	/**
	 * @return workflowService
	 */
	public WorkflowService getWorkflowService() {
		return workflowService;
	}

	/**
	 * @param workflowService セットします workflowService
	 */
	public void setWorkflowService(WorkflowService workflowService) {
		this.workflowService = workflowService;
	}

	/**
	 * @param itemListForSingleTargetMethod セットします itemListForSingleTargetMethod
	 */
	public void setItemListForSingleTargetMethod(List<WorkspaceItemEnum> itemListForSingleTargetMethod) {
		this.itemListForSingleTargetMethod = itemListForSingleTargetMethod;
	}

	/**
	 * @return itemListForSingleTargetMethod
	 */
	public List<WorkspaceItemEnum> getItemListForSingleTargetMethod() {
		return itemListForSingleTargetMethod;
	}

	/**
	 * @return objectTypeName
	 */
	public String getObjectTypeName() {
		return objectTypeName;
	}

	/**
	 * @param objectTypeName セットします objectTypeName
	 */
	public void setObjectTypeName(String objectTypeName) {
		this.objectTypeName = objectTypeName;
	}

	/**
	 * @return userService
	 */
	public UserService getUserService() {
		return userService;
	}

	/**
	 * @param userService セットします userService
	 */
	public void setUserService(UserService userService) {
		this.userService = userService;
	}

	/**
	 * @return includeGarbageBox
	 */
	public boolean isIncludeGarbageBox() {
		return includeGarbageBox;
	}

	/**
	 * @param includeGarbageBox セットします includeGarbageBox
	 */
	public void setIncludeGarbageBox(boolean includeGarbageBox) {
		this.includeGarbageBox = includeGarbageBox;
	}

	/**
	 * デフォルト値の入った属性を返却します。属性値が既に登録されている場合はnullで返却します。
	 *
	 * @param objectType
	 * @param attributeType
	 * @param objAttr
	 * @return attr
	 * @throws Exception
	 */
	private AttributeDomain getInitialAttributeDomain(ObjectTypeDomain objectType, AttributeTypeDomain attributeType, AttributeDomain objAttr) throws Exception {
		ObjectDomain attributeTypeObject = objectService.getByTypeAndName(objectType, String.valueOf(attributeType.getId()));
		if (attributeTypeObject == null) return null;

		AttributeDomain initialAttribute = null;
		AttributeDomain attr = null;

		switch (attributeType.getValueType()) {
		case LONG:
			initialAttribute = attributeTypeObject.getAttribute(ConfigUtils.getByKey("ATTRIBUTE_TYPE_NAME_INITIAL_LONG"));
			if (initialAttribute != null && (objAttr == null || objAttr.getLongList().size() == 0)) {
				attr = new AttributeDomain();
				attr.setAttributeType(attributeType);
				if (attr.getAttributeType().isMultiple()) { attr.setLongList(initialAttribute.getLongList()); }
				else { attr.setLong(initialAttribute.getLong()); }
		    }
			break;
		case STRING:
			initialAttribute = attributeTypeObject.getAttribute(ConfigUtils.getByKey("ATTRIBUTE_TYPE_NAME_INITIAL_STRING"));
			if (initialAttribute != null && (objAttr == null || objAttr.getStringList().size() == 0)) {
				attr = new AttributeDomain();
				attr.setAttributeType(attributeType);
				if (attr.getAttributeType().isMultiple()) { attr.setStringList(initialAttribute.getStringList());}
				else { attr.setString(initialAttribute.getString()); }
		    }
			break;
		case DOUBLE:
			initialAttribute = attributeTypeObject.getAttribute(ConfigUtils.getByKey("ATTRIBUTE_TYPE_NAME_INITIAL_DOUBLE"));
			if (initialAttribute != null && (objAttr == null || objAttr.getDoubleList().size() == 0)) {
				attr = new AttributeDomain();
				attr.setAttributeType(attributeType);
				if (attr.getAttributeType().isMultiple()) { attr.setDoubleList(initialAttribute.getDoubleList());}
				else { attr.setDouble(initialAttribute.getDouble()); }
		    }
			break;
		default:
			break;
		}

		return attr;
	}

	/**
	 * 指定されたIDの属性を取得します。
	 * @param attributeList
	 * @param id
	 * @return 属性（ない場合はnull）
	 */
	private AttributeDomain getAttributeById(List<AttributeDomain> attributeList, String id) {

		if (attributeList != null && Long.parseLong(id) != 0) {
			for (AttributeDomain attribute : attributeList) {
				AttributeTypeDomain attributeType = attribute.getAttributeType();
				if (attributeType != null) {
					if (id.equals(String.valueOf(attribute.getAttributeType().getId()))) {
						return attribute;
					}
				}
			}
		}
		return null;
	}

}
