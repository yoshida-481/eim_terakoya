package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.apache.commons.beanutils.BeanUtils;
import org.springframework.context.ApplicationContext;

import common.bo.AttributeValueMaster;
import common.util.AppConstant;
import common.util.AppOcrUtil;
import common.util.AppUpdateNoticeUtils;
import common.util.AttributeMasterUtil;
import common.util.OptionConfData;
import common.util.UpdateObjectLinkUtil;
import eim.bo.EIMAccessRole;
import eim.bo.EIMObject;
import eim.bo.EIMSearchConditionCompare;
import eim.bo.EIMSearchConditionGroup;
import eim.bo.EIMSearchOperatorEnum;
import eim.bo.EIMSearchSelectEIMObject;
import eim.bo.EIMSearchSelectEIMRelation;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.FTPUtils;
import eim.util.ObjectUtils;
import eim.util.SearchUtils;
import jp.co.ctc_g.eim.app.document.business.domain.DocumentAttributeTypeLayoutDomain;
import jp.co.ctc_g.eim.app.document.business.domain.DocumentDomain;
import jp.co.ctc_g.eim.app.document.business.domain.PlaceDomain;
import jp.co.ctc_g.eim.app.document.business.domain.criteria.DocumentCriteria;
import jp.co.ctc_g.eim.app.document.business.service.DocumentService;
import jp.co.ctc_g.eim.app.document.common.util.AccessUtils;
import jp.co.ctc_g.eim.app.document.common.util.AppDocumentLogicUtil;
import jp.co.ctc_g.eim.app.document.common.util.AppDocumentUtil;
import jp.co.ctc_g.eim.app.document.common.util.AppWorkFlowUtil;
import jp.co.ctc_g.eim.app.document.common.util.AttributeUtil;
import jp.co.ctc_g.eim.app.document.common.util.FileUtils;
import jp.co.ctc_g.eim.app.document.common.util.OperationHistoryUtils;
import jp.co.ctc_g.eim.app.document.common.util.RelationUtils;
import jp.co.ctc_g.eim.app.document.common.util.StringUtils;
import jp.co.ctc_g.eim.app.document.common.util.VersionUtils;
import jp.co.ctc_g.eim.app.form.business.domain.AttributeTypeLayoutDomain;
import jp.co.ctc_g.eim.app.form.business.domain.ObjectTypeLayoutDomain;
import jp.co.ctc_g.eim.app.form.business.service.ObjectTypeLayoutService;
import jp.co.ctc_g.eim.framework2.business.dao.FileDao;
import jp.co.ctc_g.eim.framework2.business.dao.ObjectDao;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.AttributeTypeCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.RelationCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AccessEntryDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AccessRoleDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AccessRoleTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.DirectoryDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.FileDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.FormatDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.RelationDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.RelationTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.SecurityDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.WorkflowDomain;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchConditionCompare;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchConditionGroup;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchLimitCountCondition;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchOperatorEnum;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchSelectObject;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchSelectObject.SearchConditionBuildHelper;
import jp.co.ctc_g.eim.framework2.business.service.AttributeTypeService;
import jp.co.ctc_g.eim.framework2.business.service.FormatService;
import jp.co.ctc_g.eim.framework2.business.service.ObjectService;
import jp.co.ctc_g.eim.framework2.business.service.ObjectTypeService;
import jp.co.ctc_g.eim.framework2.business.service.RelationService;
import jp.co.ctc_g.eim.framework2.business.service.RelationTypeService;
import jp.co.ctc_g.eim.framework2.business.service.SecurityService;
import jp.co.ctc_g.eim.framework2.business.service.UserService;
import jp.co.ctc_g.eim.framework2.business.service.WorkflowService;
import jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.common.context.TransactionContext;
import jp.co.ctc_g.eim.framework2.common.enumeration.AccessRolePermissionModeEnum;
import jp.co.ctc_g.eim.framework2.common.enumeration.DuplicateCheckModeEnum;
import jp.co.ctc_g.eim.framework2.common.enumeration.ValueTypeEnum;
import jp.co.ctc_g.eim.framework2.common.exception.EIMException;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;
import jp.co.ctc_g.eim.framework2.common.util.ResourceUtils;
import jp.co.ctc_g.eim.framework2.integration.util.internal.FileUtil;


/**
 * 【ドキュメントAPI】
 * @see DocumentService
 */
public class DocumentServiceImpl implements DocumentService {

	/** オブジェクトタイプサービス */
	private ObjectTypeService objectTypeService = null;

	/** オブジェクトサービス */
	private ObjectService objectService = null;

	/** WebDAV用オブジェクトサービス */
	private ObjectService webdavObjectService = null;

	/** オブジェクトDao */
	private ObjectDao objectDao = null;

	/** リレーションタイプサービス */
	private RelationTypeService relationTypeService = null;

	/** リレーションサービス */
	private RelationService relationService = null;

	/** ワークフローサービス */
	private WorkflowService workflowService = null;

	/** フォーマットサービス */
	private FormatService formatService = null;

	/** ファイルDao */
	private FileDao fileDao = null;

	/** 属性タイプサービス */
	private AttributeTypeService attributeTypeService = null;

	/** ユーザサービス */
	private UserService userService = null;

	/** ごみ箱の検索条件追加フラグ */
	private boolean includeGarbageBox = false;

	/** ファイルサイズ0を登録できるかフラグ */
	private boolean createFileZEROSize = false;

	/** DocumentDomainにファイルサイズを設定するかフラグ */
	private boolean includeFileSizeOfDocumentDomain = false;

	/** ファイルアップロードの方法(IO or FTP) */
	private String fileUploadRule = AppConstant.FILE_UPLOAD_RULE_IO;


	/**
	 * ドキュメントIDを指定してドキュメントを取得します。
	 *
	 * @param id ドキュメントID
	 * @return ドキュメント
	 * @throws Exception
	 */
	public DocumentDomain getById(long id) throws Exception {
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
		if (!AppDocumentLogicUtil.isTypeOfDocument(objectDomain.getType())) {
			return null;
			//throw new EIMException("EIM.ERROR.LOGIC.NOTEXIST.DOCFOLWS");
		}

		// WSのドキュメントドメインをFWのオブジェクトドメインに変換する
		DocumentDomain documentDomain = getDocumentByObjectDomain(objectDomain);

		return documentDomain;
	}

	/**
	 * パスを指定してドキュメントを取得します。
	 * 公開読取権限以上のユーザは常に最新版のドキュメントを取得します。
	 * 対象が無い場合はnullをアクセス権限が無い場合はExceptionを返却する。
	 * @param path パス
	 * @return ドキュメントドメイン
	 * @throws Exception
	 */
	public DocumentDomain getByPath(String path) throws Exception {
		if (StringUtils.isEmpty(path)) {
			throw new EIMException("EIM.ERROR.LOGIC.PATH.VALUE.ILLEGAL");
		}
		// 取得リスト
		List<ObjectDomain> resultList = new ArrayList<ObjectDomain>();
		DocumentDomain resultDomain = null;

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
		AttributeTypeDomain attributeType = AppDocumentLogicUtil.getDocPathAttributeType();
		// パス
		searchConditionGroup.addCondition(helper.eq(helper.opAnd(), attributeType, attrPath));

		// オブジェクトタイプ指定
		ObjectTypeDomain docObjTypeDomain = AppDocumentLogicUtil.getDocumentObjectType();
		List<Long> docObjTypes = AppDocumentLogicUtil.getChildObjectTypeList(docObjTypeDomain.getId());
		searchConditionGroup.addCondition(helper.inArray(helper.opAnd(), SearchSelectObject.PsedoAttributeTypeEnum.TYPE, helper.opIn(), docObjTypes
				.toArray()));

		searchConditionGroup.addCondition(helper.eq(helper.opAnd(), SearchSelectObject.PsedoAttributeTypeEnum.LATEST, 1));// Latestフラグ1
		searchSelectObject.setCondition(searchConditionGroup);

		resultList = objectService.getList(searchSelectObject, null);
		if (resultList != null && resultList.size() > 0) {

			if (resultList.size() > 1) {
				// 改訂中のドキュメントが存在する場合
				ObjectDomain firstDomain = (ObjectDomain) resultList.get(0);
				ObjectDomain secondDomain = (ObjectDomain) resultList.get(1);
				ObjectDomain latestDomain = null;
				if(firstDomain.getRevision() > secondDomain.getRevision()){
					latestDomain = firstDomain;
				}else{
					latestDomain = secondDomain;
				}

				if(latestDomain != null){
					if (objectService.authorized(latestDomain, new AccessRoleTypeDomain("READ"))) {
						resultDomain = getDocumentByObjectDomain(latestDomain);
					}else{
						// アクセス権がありません
						throw new EIMException("EIM.ERROR.LOGIC.NOACCESS");
					}
				}

			} else {
				ObjectDomain objectDomain = (ObjectDomain) resultList.get(resultList.size() - 1);
				// 改訂中ドキュメントが存在しない場合
				if (!objectService.authorized(objectDomain, new AccessRoleTypeDomain("READ"))) {
					// アクセス権がありません
					throw new EIMException("EIM.ERROR.LOGIC.NOACCESS");
				}
				// WSのドキュメントドメインをFWのオブジェクトドメインに変換する
				resultDomain = getDocumentByObjectDomain(resultList.get(resultList.size() - 1));
			}

			//ドキュメントリンクの場合は除外する。
			if(resultDomain != null && resultDomain.getPath().length() > 0){
				if(!attrPath.equals(resultDomain.getPath())){
					resultDomain = null;
				}
			}else{
				resultDomain = null;
			}

		}

		return resultDomain;
	}

	/**
	 * 指定した条件のドキュメントを取得します。
	 * （名称（部分一致）、ID（複数）、属性を指定。AND検索のみ対応）
	 *
	 * @param docCriteria ドキュメントクライテリア
	 * @return ドキュメントドメインリスト
	 * @throws Exception
	 */
	public List<DocumentDomain> getList(DocumentCriteria docCriteria) throws Exception {
		if (docCriteria == null) {
			// 引数docCriteriaとしてnullが入力されました。
			throw new EIMException("EIM.ERROR.LOGIC.CRITERIA.VALUE.ILLEGAL");
		}

		// 属性についてidからdefinitionNameの補完
		compAttrDefinitionName(docCriteria);

		// オブジェクトの検索条件を指定
		SearchSelectObject searchSelectObject = new SearchSelectObject();
		SearchConditionBuildHelper helper = new SearchConditionBuildHelper();
		SearchConditionGroup searchConditionGroup = helper.group(helper.opAnd());
		// ドキュメント名称（部分一致）
		if (docCriteria.getName() != null) {
			// オブジェクト名条件設定
			searchConditionGroup.addCondition(helper.like(helper.opAnd(), SearchSelectObject.PsedoAttributeTypeEnum.NAME, helper.opLike(),
					docCriteria.getName()));
		}
		// ID（複数）指定
		if (docCriteria.getIds() != null) {
			searchConditionGroup.addCondition(helper.inArray(helper.opAnd(), SearchSelectObject.PsedoAttributeTypeEnum.ID, helper.opIn(), docCriteria
					.getIds().getArrayList().toArray()));
		}
		// 属性検索リスト指定
		if (docCriteria.getAttributeList() != null) {
			for (AttributeDomain attr : docCriteria.getAttributeList()) {
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
						//searchConditionGroup.addCondition(helper.inArray(helper.opAnd(), attributeType, helper.opIn(), attr.getLongList().toArray()));
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
						//searchConditionGroup.addCondition(helper
						//		.inArray(helper.opAnd(), attributeType, helper.opIn(), attr.getDoubleList().toArray()));
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
		String path = null;
		if (docCriteria.getPlace() != null) {
			PlaceDomain place = docCriteria.getPlace();
			// パスの取得
			path = place.getPath();
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

		// オブジェクトタイプ指定
		ObjectTypeDomain docObjTypeDomain = null;
		if (docCriteria.getObjectTypeId() == null) {
			docObjTypeDomain = AppDocumentLogicUtil.getDocumentObjectType();
			if (docObjTypeDomain == null) {
				// 指定のドキュメントタイプが取得できませんでした。
				throw new EIMException("EIM.ERROR.LOGIC.NODOCUMENTTYPE");
			}
			List<Long> docObjTypes = AppDocumentLogicUtil.getChildObjectTypeList(docObjTypeDomain.getId());
			searchConditionGroup.addCondition(helper.inArray(helper.opAnd(), SearchSelectObject.PsedoAttributeTypeEnum.TYPE, helper.opIn(),
					docObjTypes.toArray()));

		} else {
			// objectTypeIdが設定されている場合
			docObjTypeDomain = objectTypeService.getById(docCriteria.getObjectTypeId());
			if (!AppDocumentLogicUtil.isTypeOfDocument(docObjTypeDomain)) {
				// 指定のドキュメントタイプが取得できませんでした。
				throw new EIMException("EIM.ERROR.LOGIC.NODOCUMENTTYPE");
			}
			long objTypeId = docObjTypeDomain.getId();
			searchConditionGroup.addCondition(helper.eq(helper.opAnd(), SearchSelectObject.PsedoAttributeTypeEnum.TYPE, objTypeId));
		}

		searchConditionGroup.addCondition(helper.eq(helper.opAnd(), SearchSelectObject.PsedoAttributeTypeEnum.LATEST, 1));// Latestフラグ1
		// 権限チェック
		searchSelectObject.setAccessRoleType(new AccessRoleTypeDomain("READ"));
		searchSelectObject.setCondition(searchConditionGroup);

		//最大取得件数
		SearchLimitCountCondition limitCountCondition = new SearchLimitCountCondition(Integer.MAX_VALUE,false);
		/*
		if(docCriteria.getLimit() != null && docCriteria.getLimit() > 0){
			if(docCriteria.isLimitCondition() != null && docCriteria.isLimitCondition()){
				limitCountCondition = new SearchLimitCountCondition(docCriteria.getLimit(),true);
			}else{
				limitCountCondition = new SearchLimitCountCondition(docCriteria.getLimit(),false);
			}
		}
		*/
		List<ObjectDomain> objectList = objectService.getList(searchSelectObject, limitCountCondition);
		if (objectList == null || objectList.size() == 0) {
			return null;
		}

		// 取得リスト
		List<DocumentDomain> resultList = new ArrayList<DocumentDomain>();

		// WSのドキュメントドメインをFWのオブジェクトドメインに変換する
		for (ObjectDomain objectDomain : objectList) {
			DocumentDomain documentDomain = getDocumentByObjectDomain(objectDomain);
			if(docCriteria.getPlace() != null && path != null){
				//ドキュメントリンクを除外する
				if(documentDomain != null && documentDomain.getPath().length() > 0 ){
					if(documentDomain.getPath().startsWith(path)){
						resultList.add(documentDomain);
					}
				}
			}else{
				resultList.add(documentDomain);
			}
		}
		//最大取得件数確認
		List<DocumentDomain> resultLimitList = new ArrayList<DocumentDomain>();
		if(docCriteria.getLimit() != null && docCriteria.getLimit() > 0){
			if(resultList.size() > docCriteria.getLimit() ){
				if(docCriteria.isLimitCondition() != null && docCriteria.isLimitCondition()){
					throw new EIMException("EIM.ERROR.LOGIC.SEARCH.RESULT.LIMIT.OVER",new Object[]{resultList.size(),docCriteria.getLimit()});
				}else{
					//上限件数分の結果を詰めなおす
					for(int i=0;i<docCriteria.getLimit();i++){
						resultLimitList.add(resultList.get(i));
					}

					resultList = resultLimitList;
				}


			}
		}
		return resultList;
	}

	/**
	 * ドキュメントを登録します。
	 *
	 * @param documentDomain ドキュメントドメイン
	 * @param placeDomain ワークスペースドメイン又はフォルダドメイン
	 * @param inputStream InputStream
	 * @return ドキュメントドメイン
	 * @throws Exception
	 */
	public DocumentDomain create(DocumentDomain documentDomain, PlaceDomain placeDomain, InputStream inputStream) throws Exception {
		if (documentDomain == null) {
			// 引数documentDomainとしてnullが入力されました。
			throw new EIMException("EIM.ERROR.LOGIC.OBJECT.VALUE.ILLEGAL");
		}

		if (placeDomain == null) {
			// 引数parentDomainとしてnullが入力されました。
			throw new EIMException("EIM.ERROR.LOGIC.OBJECT.VALUE.ILLEGAL");
		}
		// オブジェクトタイプの取得
		ObjectTypeDomain objectTypeDomain = documentDomain.getType();
		if (objectTypeDomain != null) {

			// オブジェクトタイプに定義名称しかセットされていない場合は、定義名称でオブジェクトタイプを取得する
			if (objectTypeDomain.getId() <= 0) {
				objectTypeDomain = objectTypeService.getByDefinitionName(objectTypeDomain.getDefinitionName());
			}

			// オブジェクトタイプチェック
			if (!AppDocumentLogicUtil.isTypeOfDocument(objectTypeDomain)) {
				throw new EIMException("EIM.ERROR.LOGIC.NODOCUMENTTYPE");
			}
		} else {
			// 「ドキュメント」オブジェクトタイプ名称の取得
			objectTypeDomain = AppDocumentLogicUtil.getDocumentObjectType();
			documentDomain.setType(objectTypeDomain);
		}

		// リスト属性の場合の登録値チェック
		checkValueIncludeInListDef(documentDomain);

		// 属性についてidからdefinitionNameの補完
		compAttrDefinitionName(documentDomain);

		// 親オブジェクトの取得
		ObjectDomain parentObject = objectService.getById(placeDomain.getId());
		if (parentObject == null) {
			// 選択したフォルダは存在しません。
			throw new EIMException("EIM.ERROR.LOGIC.NOFOLDER");
		}

		// 親オブジェクトがゴミ箱の下に移動していないかのチェック
		if (AppDocumentUtil.isObjectInRecycle(parentObject)) {
			throw new EIMException("EIM.ERROR.LOGIC.SELECTFOLDERDELETED");
		}

		// ワークフロー付きフォルダ下の場合は、編集中以外は作成できない
		if (parentObject.getStatus() != null) {
			if (parentObject.getStatus().getType().getBase().getId() != AppConstant.STATUS_TYPE_KIND_ID_EDITTING) {
				throw new EIMException("EIM.ERROR.LOGIC.NOCREATEROLE");
			}
			if (workflowService.getByObjectType(objectTypeDomain) != null) {
				throw new EIMException("EIM.ERROR.LOGIC.CANTSET.UNDERWF.WFDOCUMENT");
			}
		}

		// パスの取得
		String path = AppDocumentUtil.getPath(parentObject);
		if (path == null) {
			// ワークスペースの場合、パス属性の値を保持していない
			path = "/";
		}
		path += parentObject.getName() + "/";

		// フォーマット、ディレクトリの取得
		FormatDomain formatDomain = formatService.getDefaultByObjectType(objectTypeDomain);
		if (formatDomain == null) {
			// デフォルトフォーマットが設定されていません。
			throw new EIMException("EIM.ERROR.LOGIC.NODEFAULTFORMAT");
		}
		DirectoryDomain dir = formatDomain.getOnlineDirectory();

		// ファイル存在チェック
		if (inputStream == null) {
			throw new EIMException("EIM.ERROR.LOGIC.NOUPLOADFILE");
		}

		// ファイルサイズ判定(最大ファイルサイズ)
		long maxFileSize = Long.parseLong(ConfigUtils.getByKey("UPLOAD_FILE_SIZE_MAX"));
		if (inputStream.available() > maxFileSize) {
			throw new EIMException("EIM.ERROR.LOGIC.UPLOAD.FILE.SIZE.OVER", maxFileSize);
		}

		ObjectDomain objectDomain = new ObjectDomain();
		DocumentDomain document = new DocumentDomain();

		try {

			BeanUtils.copyProperties(objectDomain, documentDomain);

			// 指定されたドキュメントオブジェクトの作成
			objectDomain = objectService.create(objectDomain, DuplicateCheckModeEnum.NONE);

			// WebDAVからの登録時にオブジェクト名に採番された番号が入力されてしまう場合の修正処理
			if (!objectDomain.getName().equals(documentDomain.getName())) {
				String documentNumber = objectDomain.getName();

				// オブジェクト名の設定
				objectDomain.setName(documentDomain.getName());
				objectService.update(objectDomain);

				// 番号の設定
				AttributeTypeDomain attributeType = attributeTypeService.getByDefinitionName(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_NUMBER"));
				List<String> strList = new ArrayList<String>();
				strList.add(documentNumber);
				AppDocumentUtil.setAttributeStrings(objectDomain, attributeType, strList);
			}

			// 「ドキュメント」リレーションタイプ
			RelationTypeDomain relationTypeDomain = AppDocumentLogicUtil.getDocumentRelType();
			// 親オブジェクトとのリレーションを作成
			RelationDomain relationDomain = new RelationDomain();
			relationDomain.setType(relationTypeDomain);
			relationDomain.setParent(parentObject);
			relationDomain.setChild(objectDomain);
			//重複チェックを修正
			//relationService.create(relationDomain, DuplicateCheckModeEnum.INHERITTYPE);
			RelationUtils.createRelation(relationDomain);

			// 拡張子の取得
			String fileExt = StringUtils.getFileExt(documentDomain.getName());
			if (fileExt == null) {
				fileExt = "";
			}

			// パスを設定
			AppDocumentUtil.setPath(objectDomain, path);

			// 作成者
			UserDomain user = documentDomain.getCreateUser();
			if (user == null) {
				TransactionContext tx = EIMThreadContext.getTransactionContext();
				// セッションユーザー
				user = tx.getUser();
			}
			// 作成者を設定
			AppDocumentUtil.setAttrLong(objectDomain, ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_CREATE"), user.getId());

			// 各属性の初期値を設定
			ObjectTypeDomain attributeTypeObjectType = new ObjectTypeDomain(ConfigUtils.getByKey("OBJECT_TYPE_NAME_ATTRIBUTETYPE"));
			for (AttributeTypeDomain attributeType : objectTypeDomain.getAttributeTypeList()) {
				AttributeDomain objAttr = objectDomain.getAttribute(attributeType.getDefinitionName());
				if (objAttr == null) {
					 objAttr = getAttributeById(objectDomain.getAttributeList() , String.valueOf(objectDomain.getId()));
				}
				AttributeDomain attr = getInitialAttributeDomain(attributeTypeObjectType, attributeType, objAttr);
				if (attr != null) AppDocumentUtil.setAttr(objectDomain, attr);
			}


			// 上位フォルダからの属性引継ぎ
			List<Long> parentLowAttrIds = AppDocumentUtil.getIntAttrs(parentObject, ConfigUtils.getByKey("ATTR_NAME_WORKSPACE_TO_LOW_ATTR"));
			if (parentLowAttrIds != null && parentLowAttrIds.size() > 0) {
				List<AttributeTypeDomain> parentLowAttrTypes = new ArrayList<AttributeTypeDomain>();
				// 上位からの引継ぎ属性の設定内容に従い、parentObjから属性値をコピー
				// ただし、自身のタイプに該当属性が割り当てられているものに限る
				AttributeTypeCriteria attributeTypeCriteria = new AttributeTypeCriteria();
				attributeTypeCriteria.setObjectTypeId(objectTypeDomain.getId());
				List<AttributeTypeDomain> attributeTypeList = attributeTypeService.getList(attributeTypeCriteria);
				// 引継ぎ対象を自身のタイプに該当属性が割り当てられているものにフィルタリング
				for (Long parentLowAttrId : parentLowAttrIds) {
					for (AttributeTypeDomain attributeTypeDomain : attributeTypeList) {
						if (attributeTypeDomain.getId() == parentLowAttrId) {
							parentLowAttrTypes.add(attributeTypeDomain);
							continue;
						}
					}
				}
				// 「上位からの引継ぎ」属性の値を設定
				AppDocumentUtil.setAttrLongs(objectDomain, ConfigUtils.getByKey("ATTR_NAME_FOLDER_FROM_HIGH_ATTR"), parentLowAttrIds);
				// 各属性値の引継ぎ
				for (AttributeTypeDomain attributeType : parentLowAttrTypes) {
					AttributeDomain attr = parentObject.getAttribute(attributeType.getDefinitionName());
					if (attr != null) {
						AppDocumentUtil.setAttr(objectDomain, attr);
					}
				}
			}

			// 上位フォルダからのステータス引継ぎ
			if (parentObject.getStatus() != null) {
				objectDao.setStatus(objectDomain, parentObject.getStatus());
				// 「上位WFフォルダ」属性も登録
				AttributeDomain attrOfHigherWFFolder = parentObject.getAttribute(ConfigUtils.getByKey("ATTR_NAME_FOLDER_HIGHER_WF_FOLDER"));
				if (attrOfHigherWFFolder == null) {
					// WF付フォルダ直下
					AppDocumentUtil.setAttrLong(objectDomain, ConfigUtils.getByKey("ATTR_NAME_FOLDER_HIGHER_WF_FOLDER"), parentObject.getId());
				} else {
					// 「WF付フォルダ下のフォルダ」の下
					AppDocumentUtil.setAttr(objectDomain, attrOfHigherWFFolder);
				}
			}

			// セキュリティを設定
			SecurityDomain securityDomain = parentObject.getSecurity();
			if (securityDomain != null) {
				objectService.setSecurity(objectDomain, securityDomain);
			}

			// 取得した実ファイルをファイルサーバ上に配置する
			if(!createFileZEROSize && inputStream.available() == 0){
				throw new EIMException("EIM.ERROR.LOGIC.NOUPLOADFILE");
			}

			long fileSize = 0;
			File tmpFile = null;
			switch (fileUploadRule) {
				case AppConstant.FILE_UPLOAD_RULE_IO:
					// アップロード方法がIOの場合
					fileSize = FileUtils.upload(dir.getPath() + objectDomain.getId() + fileExt, inputStream);
					break;
				case AppConstant.FILE_UPLOAD_RULE_FTP:
					// アップロード方法がFTPの場合
					// 1度ローカルに配置してからFTPにて送付
					fileSize = FileUtils.upload(EIMConfig.get("TEMP") + objectDomain.getId() + fileExt, inputStream);
					tmpFile = new File(EIMConfig.get("TEMP") + objectDomain.getId() + fileExt);
					FTPUtils.putFile(EIMConfig.get("FTP_HOST"), EIMConfig.get("FTP_USER"), EIMConfig.get("FTP_PASS"),
							tmpFile,
							new File(dir.getPath() + objectDomain.getId() + fileExt));
					break;
				default:
					// IOでアップロードする
					fileSize = FileUtils.upload(dir.getPath() + objectDomain.getId() + fileExt, inputStream);
					break;
			}

			// チェックイン実行（DBに登録）
			FileUtils.checkin(objectDomain, formatDomain, objectDomain.getName(), fileSize);

			// 作成したドキュメント自身にステータスが無く、かつ上位フォルダにもステータスが無い場合は、
			// WFなしドキュメントとして、即公開する
			if (objectDomain.getStatus() == null && parentObject.getStatus() == null) {
				FileDomain file = fileDao.getByObjectAndFormat(objectDomain, formatDomain);
				FormatDomain publicDocumentFormat = formatService.getByDefinitionName(ConfigUtils.getByKey("FORMAT_NAME_PUBLIC"));
				File orgFile = new File(file.getDirectory().getPath() + StringUtils.getFileBody(FileUtil.getFileName(objectDomain, file)) + fileExt);
				File dstFile = new File(publicDocumentFormat.getOnlineDirectory().getPath() + objectDomain.getId() + fileExt);
				switch (fileUploadRule) {
					case AppConstant.FILE_UPLOAD_RULE_IO:
						// アップロード方法がIOの場合
						FileUtils.createSymbolicLink(orgFile, dstFile);
						break;
					case AppConstant.FILE_UPLOAD_RULE_FTP:
						// アップロード方法がFTPの場合
						FTPUtils.putFile(EIMConfig.get("FTP_HOST"), EIMConfig.get("FTP_USER"), EIMConfig.get("FTP_PASS"),
								tmpFile,
								dstFile);
						tmpFile.delete();
						break;
					default:
						FileUtils.createSymbolicLink(orgFile, dstFile);
						break;
				}
				FileUtils.checkin(objectDomain, publicDocumentFormat, file.getName(), file.getSize());
			}

			// WSのドキュメントドメインをFWのオブジェクトドメインに変換する
			objectDomain = objectService.getById(objectDomain.getId());
			document = getDocumentByObjectDomain(objectDomain);

			// FTP転送用のtmpファイルをクリアする
			if(tmpFile != null) {
				tmpFile.delete();
			}

			// Access History - 「新規登録」
			AccessUtils.createAccess(objectDomain, "EIM.ACCESS.TYPE.INITIALREGIST");

			//操作履歴 AOP利用
			//OperationHistoryUtils.createOperationHistory(objectDomain, 2, 2012, 1, 6, objectDomain.getId(), objectDomain.getName(),
			//											-1, -1, -1, null, objectDomain.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PASS")).getStringList().get(0));

		} catch (Exception e) {
			throw e;
		}

		return document;
	}

	/**
	 * 原本ファイルを取得します。
	 *
	 * @param documentDomain ドキュメントドメイン
	 * @return InputStream
	 * @throws Exception
	 */
	public InputStream getOriginalFile(DocumentDomain documentDomain) throws Exception {
		if (documentDomain == null) {
			// 引数documentDomainとしてnullが入力されました。
			throw new EIMException("EIM.ERROR.LOGIC.OBJECT.VALUE.ILLEGAL");
		}
		// Object
		ObjectDomain objectDomain = objectService.getById(documentDomain.getId());
		if (objectDomain == null) {
			// 選択したドキュメントは存在しません。
			throw new EIMException("EIM.ERROR.LOGIC.NODOCUMENT");
		}
		// フォーマットを取得
		FormatDomain formatDomain = formatService.getDefaultByObjectType(objectDomain.getType());
		// ファイル
		FileDomain file = fileDao.getByObjectAndFormat(objectDomain, formatDomain);
		if (file == null) {
			// 選択したドキュメントは存在しません。
			throw new EIMException("EIM.ERROR.LOGIC.NODOCUMENT");
		}

		// Substance
		File substance = new File(file.getDirectory().getPath() + StringUtils.getFileBody(FileUtil.getFileName(objectDomain, file)) + StringUtils.nullToBlank(file.getExt()));
		if (!substance.exists()) {
			throw new EIMException("EIM.ERROR.LOGIC.NODOCUMENT");

		}

		FileInputStream in = new FileInputStream(substance);

		// Access History - 「ダウンロード」
		AccessUtils.createAccess(objectDomain, "EIM.ACCESS.TYPE.DOWNLOAD");

		//操作履歴 AOP利用
		//OperationHistoryUtils.createOperationHistory(objectDomain, 2, 2030, 25, 12, objectDomain.getId(), objectDomain.getName(),
		//											-1, -1, -1, null, objectDomain.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PASS")).getStringList().get(0));

		return in;
	}

	/**
	 * 公開ファイルを取得します。
	 *
	 * @param documentDomain ドキュメントドメイン
	 * @return InputStream
	 * @throws Exception
	 */
	public InputStream getPublicFile(DocumentDomain documentDomain) throws Exception {
		if (documentDomain == null) {
			// 引数documentDomainとしてnullが入力されました。
			throw new EIMException("EIM.ERROR.LOGIC.OBJECT.VALUE.ILLEGAL");
		}
		// Object
		ObjectDomain objectDomain = objectService.getById(documentDomain.getId());
		if (objectDomain == null) {
			// 選択したドキュメントは存在しません。
			throw new EIMException("EIM.ERROR.LOGIC.NODOCUMENT");
		}
		// フォーマットを取得
		FormatDomain formatDomain = formatService.getByDefinitionName(ConfigUtils.getByKey("FORMAT_NAME_PUBLIC"));
		// ファイル
		FileDomain file = fileDao.getByObjectAndFormat(objectDomain, formatDomain);
		if (file == null) {
			// 選択したドキュメントは存在しません。
			throw new EIMException("EIM.ERROR.LOGIC.NODOCUMENT");
		}

		// Substance
		File substance = new File(file.getDirectory().getPath() + StringUtils.getFileBody(FileUtil.getFileName(objectDomain, file)) + StringUtils.nullToBlank(file.getExt()));
		if (!substance.exists()) {
			throw new EIMException("EIM.ERROR.LOGIC.NODOCUMENT");

		}

		FileInputStream in = new FileInputStream(substance);

		// Access History - 「ダウンロード」
		AccessUtils.createAccess(objectDomain, "EIM.ACCESS.TYPE.DOWNLOAD");

		//操作履歴 AOP利用
		//OperationHistoryUtils.createOperationHistory(objectDomain, 2, 2030, 25, 12, objectDomain.getId(), objectDomain.getName(),
		//													-1, -1, -1, null, objectDomain.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PASS")).getStringList().get(0));

		return in;
	}

	/**
	 * ドキュメントの名称と属性、セキュリティを更新します。
	 *
	 * @param documentDomain ドキュメントドメイン
	 * @throws Exception
	 */
	public void update(DocumentDomain documentDomain) throws Exception {
		if (documentDomain == null) {
			// 引数documentDomainとしてnullが入力されました。
			throw new EIMException("EIM.ERROR.LOGIC.OBJECT.VALUE.ILLEGAL");
		}

		// オブジェクトの取得
		ObjectDomain objectDomain = objectService.getById(documentDomain.getId());
		if (objectDomain == null) {
			// オブジェクトが存在しない、エラー処理
			throw new EIMException("EIM.ERROR.LOGIC.NOTEXIST.DOCFOLWS");
		}

		boolean isChangeName = false;
		if (!StringUtils.isEmpty(documentDomain.getName()) && !documentDomain.getName().equals(objectDomain.getName())) {
			isChangeName = true;
		}

		// オブジェクトタイプの取得
		ObjectTypeDomain objectTypeDomain = objectDomain.getType();
		if (objectTypeDomain != null) {
			// オブジェクトタイプチェック
			if (!AppDocumentLogicUtil.isTypeOfDocument(objectTypeDomain)) {
				throw new EIMException("EIM.ERROR.LOGIC.NOTEXIST.DOCFOLWS");
			}
		}

		// リスト属性の場合の登録値チェック
		checkValueIncludeInListDef(documentDomain);

		// 属性についてidからdefinitionNameの補完
		compAttrDefinitionName(documentDomain);

		try {
			// 「ドキュメント」リレーションタイプ
			RelationTypeDomain relType = AppDocumentLogicUtil.getDocumentRelType();

			if(isChangeName){
				//親情報を取得
				RelationCriteria relCriteria = new RelationCriteria();
				relCriteria.setChildObjectId(objectDomain.getId());
				relCriteria.setRelationTypeId(relType.getId());
				List<RelationDomain> relDomainList = relationService.getList(relCriteria);
				RelationDomain relDomain = relDomainList.get(0);
				ObjectDomain parentObject = relDomain.getParent();
				// ワークフロー付きフォルダ下の場合は、編集中以外は作成できない
				if (parentObject.getStatus() != null) {
					if (parentObject.getStatus().getType().getBase().getId() != AppConstant.STATUS_TYPE_KIND_ID_EDITTING) {
						throw new EIMException("EIM.ERROR.LOGIC.NORENAMEROLE");
					}
				}
			}

			// オブジェクト名が変更された場合
			if (isChangeName) {
				objectDomain.setName(documentDomain.getName());
				// 改名処理
				AppDocumentLogicUtil.renameObject(objectDomain, relType);
				// Access History - 「改名」
				AccessUtils.createAccess(objectDomain, "EIM.ACCESS.TYPE.CHANGENAME");

				//操作履歴
				OperationHistoryUtils.createOperationHistory(objectDomain, 2, 2017, 3, 12, objectDomain.getId(), objectDomain.getName(),
															-1, -1, -1, null, objectDomain.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PASS")).getStringList().get(0));
			}

			if (documentDomain.getSecurity() != null && documentDomain.getSecurity() != objectDomain.getSecurity()) {
				objectService.setSecurity(objectDomain, documentDomain.getSecurity());
			}

			objectDomain.setAttributeList(documentDomain.getAttributeList());

			// 属性情報の更新
			AttributeUtil.updateAttribute(objectDomain, relType);

			// Access History - 「属性更新」
			AccessUtils.createAccess(objectDomain, "EIM.ACCESS.TYPE.ATTRIBUTEUPDATE");

			//操作履歴
			OperationHistoryUtils.createOperationHistory(objectDomain, 2, 2018, 3, 12, objectDomain.getId(), objectDomain.getName(),
														-1, -1, -1, null, objectDomain.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PASS")).getStringList().get(0));

		} catch (Exception e) {
			throw e;
		}
	}

	/**
	 * 入力規則がリスト定義の属性の登録値がリスト定義内に存在するかどうかをチェックする。
	 *
	 * @param documentDomain
	 * @return
	 * @throws Exception
	 */
	private void checkValueIncludeInListDef(DocumentDomain documentDomain) throws Exception{

		EIMSession sess = jp.co.ctc_g.eim.framework.common.util.EIMThreadContext.getEIMSession();

		for (AttributeDomain attrDomain : documentDomain.getAttributeList()) {

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
		if (obj instanceof DocumentDomain) {
			DocumentDomain documentDomain = (DocumentDomain)obj;
			if (!Objects.isNull(documentDomain.getAttributeList())) {
				for (AttributeDomain domain:documentDomain.getAttributeList()) {
					if (Objects.isNull(domain.getAttributeType().getDefinitionName()) && domain.getAttributeType().getId() != 0) {
						AttributeTypeDomain attributeType = attributeTypeService.getById(domain.getAttributeType().getId());
						domain.getAttributeType().setDefinitionName(attributeType.getDefinitionName());
						domain.getAttributeType().setValueType(attributeType.getValueType());
					}

				}
			}
		} else if (obj instanceof DocumentCriteria){
			DocumentCriteria documentCriteria = (DocumentCriteria)obj;
			if (!Objects.isNull(documentCriteria.getAttributeList())) {
				for (AttributeDomain domain:documentCriteria.getAttributeList()) {
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
	 * ドキュメントを改訂します。
	 *
	 * @param documentDomain ドキュメントドメイン
	 * @param inputStream InputStream
	 * @throws Exception
	 */
	public void checkin(DocumentDomain documentDomain, InputStream inputStream) throws Exception {
		if (documentDomain == null) {
			// 引数documentDomainとしてnullが入力されました。
			throw new EIMException("EIM.ERROR.LOGIC.OBJECT.VALUE.ILLEGAL");
		}
		// ファイル存在チェック
		if (inputStream == null) {
			throw new EIMException("EIM.ERROR.LOGIC.NOUPLOADFILE");
		}

		// ファイルサイズ判定(最大ファイルサイズ)
		long maxFileSize = Long.parseLong(ConfigUtils.getByKey("UPLOAD_FILE_SIZE_MAX"));
		if (inputStream.available() > maxFileSize) {
			throw new EIMException("EIM.ERROR.LOGIC.UPLOAD.FILE.SIZE.OVER", maxFileSize);
		}

		Map<Long, ObjectDomain> revObjectMap = null;

		try{
			// 先に処理対象のドキュメントの同一バージョン内のオブジェクト全てに行ロックをかける
			revObjectMap = VersionUtils.getVersion(documentDomain);
			if (!revObjectMap.isEmpty()) {
				AppDocumentUtil.lockObjects(revObjectMap);
			}else{
				// オブジェクトが存在しない、エラー処理
				throw new EIMException("EIM.ERROR.LOGIC.NODOCUMENTORFOLDER");
			}
		}
		catch(EIMException e){

            if (e.getMessageKey().equals("EIM.ERROR.OBJECT.NOTFOUND")) {
                throw new EIMException("EIM.ERROR.LOGIC.NODOCUMENTORFOLDER");
            }
            else {
                throw e;
            }
		}

		// オブジェクトの取得
		ObjectDomain objectDomain = objectService.getById(documentDomain.getId());
		if (objectDomain == null) {
			// オブジェクトが存在しない、エラー処理
			throw new EIMException("EIM.ERROR.LOGIC.NODOCUMENTORFOLDER");
		}

		// ファイル名チェック
		if (StringUtils.isEmpty(documentDomain.getName())) {
			// オブジェクトの名称に、NULLまたは空文字を使用しています。
			throw new EIMException("EIM.ERROR.LOGIC.OBJECT.NAME.LENGTH.ILLEGAL");
		}

		String fileExt = StringUtils.getFileExt(documentDomain.getName());
		if (fileExt == null) {
			fileExt = "";
		}

		// ステータスのチェック
		// WFなし、[編集中]ドキュメントは、チェックインは出来るものとする
		if (!OptionConfData.getInstance().enableApproverCheckin) {
			if (objectDomain.getStatus() != null && objectDomain.getStatus().getType().getBase().getId() != AppConstant.STATUS_TYPE_KIND_ID_EDITTING) {
				throw new EIMException("EIM.ERROR.LOGIC.NOTUPDATING", objectDomain.getName());
			}
		}else{
			//WFなし、「編集中」、「承認依頼中」ドキュメントは、チェックインは出来るものとする
			if (objectDomain.getStatus() != null && objectDomain.getStatus().getType().getBase().getId() !=  AppConstant.STATUS_TYPE_KIND_ID_EDITTING
					&& objectDomain.getStatus().getType().getBase().getId() != AppConstant.STATUS_TYPE_KIND_ID_APPROVE) {
				throw new EIMException("EIM.ERROR.LOGIC.NOTUPDATING", objectDomain.getName());
			//「承認依頼中」ドキュメントにて直接編集が可能か判定する
		 	}else if (objectDomain.getStatus() != null && objectDomain.getStatus().getType().getBase().getId() == AppConstant.STATUS_TYPE_KIND_ID_APPROVE) {
				checkStatus(objectDomain);
			}
		}

		// WFなし、かつ、ロック中のドキュメントはチェックイン不可
		if (objectDomain.getStatus() == null && objectDomain.getLockUser() != null) {
			throw new EIMException("EIM.ERROR.LOGIC.NOTUPDATING", objectDomain.getName());
		}

		// 対象が最新のリビジョンかのチェック
		if (!objectDomain.isLatest()) {
			throw new EIMException("EIM.ERROR.LOGIC.NOCHECKINROLE");
		}

		// Check Lock User
		List<ObjectDomain> objectDomains = Arrays.asList(revObjectMap.values().toArray(new ObjectDomain[0]));
		ObjectDomain lockObj = new ObjectDomain();
		if (objectDomain.getRevision() > 0) {
			lockObj = objectDomains.get(objectDomains.size() - 2);
		} else {
			lockObj = objectDomains.get(objectDomains.size() - 1);
		}
		TransactionContext tx = EIMThreadContext.getTransactionContext();
		// オブジェクトがロックされている場合のみ、ユーザーの比較を行う
		// チェックアウト無しでチェックインする場合は、過去ドキュメントはロックされていない
		if (lockObj != null && lockObj.getLockUser() != null && lockObj.getLockUser().getId() != tx.getUser().getId()) {
			throw new EIMException("EIM.ERROR.LOGIC.NOCHECKOUTUSER");
		}
		// 直接編集でロックされている場合はチェックイン不可
		if (objectDomain != null && objectDomain.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_WEBDAV_LOCK_FLAG")) != null) {
			throw new EIMException("EIM.ERROR.LOGIC.OBJECT.LOCKED");
		}

		// 署名・暗号化中でないかチェック
		FormatDomain formatSignEnc = null;
		if (getSignAndEncrFlg()) {
			long signencr = AppDocumentUtil.getIntAttr(objectDomain, ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_SIGN_ENC_STATUS"));
			if (signencr == AppConstant.SIGNENCR_KIND_PROCESSING_SIGNENCR) {
				// 署名・暗号化処理中のため、チェックインできません。
				throw new EIMException("EIM.ERROR.LOGIC.CANNOT.CHECKIN.WITH.SIGN.AND.ENCR.PROC");
			}
			formatSignEnc = formatService.getByDefinitionName(ConfigUtils.getByKey("FORMAT_NAME_SIGNENCR"));
			if (signencr == AppConstant.SIGNENCR_KIND_SIGNENCR) {
				// 署名・暗号化済
				if (formatSignEnc == null) {
					// 署名・暗号化フォーマットが取得できない
					throw new EIMException("EIM.ERROR.LOGIC.SIGN.AND.ENCR.FORMAT.NOTFOUND");
				} else if (fileDao.getByObjectAndFormat(objectDomain, formatSignEnc) == null) {
					// 署名・暗号化ファイルが取得できない
					throw new EIMException("EIM.ERROR.LOGIC.NO.SIGN.AND.ENCR.FILE.WITHDOCNAME", objectDomain.getName());
				}
			}
		}

		// 改訂内容
		AttributeDomain attrInputRevisionContent = documentDomain.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_REV_CONTENT"));
		AttributeDomain attrRegisteredRevisionContent = objectDomain.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_REV_CONTENT"));
		if (attrInputRevisionContent == null) {
			if (attrRegisteredRevisionContent != null) {
				// チェックイン対象のドキュメントから改訂内容を削除する
				AttributeTypeDomain attributeType = attributeTypeService.getByDefinitionName(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_REV_CONTENT"));
				objectService.removeAttribute(objectDomain, attributeType);
			}
		} else {
			if (attrRegisteredRevisionContent == null || !attrInputRevisionContent.getString().equals(attrRegisteredRevisionContent.getString())) {
				// チェックイン対象のドキュメントに対して入力ドキュメントの改訂内容で更新する
				AttributeTypeDomain attributeType = attributeTypeService.getByDefinitionName(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_REV_CONTENT"));
				objectService.setAttributeSingleString(objectDomain, attributeType, attrInputRevisionContent.getString());
			}
		}

		// フォーマット、ディレクトリの取得
		FormatDomain formatDomain = formatService.getDefaultByObjectType(objectDomain.getType());
		if (formatDomain == null) {
			// デフォルトフォーマットが設定されていません。
			throw new EIMException("EIM.ERROR.LOGIC.NODEFAULTFORMAT");
		}
		DirectoryDomain dir = formatDomain.getOnlineDirectory();

		try {
			// 拡張子が変わる場合は変更前のファイルを削除
			FileDomain file = fileDao.getByObjectAndFormat(objectDomain, formatDomain);
			if (!fileExt.equals(StringUtils.nullToBlank(file.getExt()))) {
				File target = new File(dir.getPath() + StringUtils.getFileBody(FileUtil.getFileName(objectDomain, file)) + StringUtils.nullToBlank(file.getExt()));
				target.delete();
			}

			// 「ドキュメント」リレーションタイプ
			RelationTypeDomain relationTypeDomain = AppDocumentLogicUtil.getDocumentRelType();
			ObjectDomain parentObject = null;
			AccessRoleTypeDomain accessRoleType = new AccessRoleTypeDomain("READ");
			accessRoleType.setId(EIMAccessRole.READ);
			List<RelationDomain> relList = AppDocumentUtil.getParentRelationByRelTypeAndChild(relationTypeDomain, objectDomain,
					accessRoleType);
			if (relList != null && relList.size() > 0) {
				parentObject = relList.get(0).getParent();
			}

			// Rename
			if (!documentDomain.getName().equals(objectDomain.getName())) {
				//  同じ親を持つ同名のオブジェクトが既に存在するかチェック
				EIMSession sess = jp.co.ctc_g.eim.framework.common.util.EIMThreadContext.getEIMSession();
				EIMSearchSelectEIMRelation relationSelectTarget = new EIMSearchSelectEIMRelation();
				EIMSearchSelectEIMRelation.SearchConditionBuildHelper h = new EIMSearchSelectEIMRelation.SearchConditionBuildHelper();

				relationSelectTarget.setSourceObjects(EIMSearchSelectEIMRelation.SourceObjects.CHILD);
				relationSelectTarget.setCondition(h.group(h.opAnd())
						.addCondition(
								h.eq(h.opAnd(),
										EIMSearchSelectEIMRelation.PsedoAttributeTypeEnum.PARENT,
										parentObject.getId())
						)
				);

				EIMSearchSelectEIMObject parentSelectTarget = new EIMSearchSelectEIMObject();
				parentSelectTarget.setRole(EIMAccessRole.NONE);
				EIMSearchSelectEIMObject childSelectTarget = new EIMSearchSelectEIMObject();
				childSelectTarget.setRole(EIMAccessRole.READ);
				EIMSearchConditionGroup allConds = new EIMSearchConditionGroup(EIMSearchOperatorEnum.AND);
				EIMSearchConditionCompare condition = new EIMSearchConditionCompare(
						EIMSearchOperatorEnum.OR
						, EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.NAME
						, EIMSearchOperatorEnum.EQ
						, documentDomain.getName()
					);
				allConds.addCondition(condition);
				childSelectTarget.setCondition(allConds);

				List objList = SearchUtils.searchRelations(sess,	relationSelectTarget, parentSelectTarget, childSelectTarget, null);

				if (objList.size() > 0) {
					// 同じ親を持つ同名のオブジェクトが既に存在します。
					throw new EIMException("EIM.ERROR.LOGIC.OBJTYPE.NAME.DUPLICATE");
				}

				// 名前変更する時の重複チェックする際、名前のみでチェックするように変更する
				objectService.updateName(objectDomain, parentObject, relationTypeDomain, documentDomain.getName(), DuplicateCheckModeEnum.TYPE);
			}

			// 取得した実ファイルをファイルサーバ上に配置する
			long fileSize = FileUtils.upload(dir.getPath() + objectDomain.getId() + fileExt, inputStream);
			// チェックイン実行（DBに登録）
			FileUtils.checkin(objectDomain, formatDomain, documentDomain.getName(), fileSize);

			// WFなしドキュメントの場合、公開ドキュメントの置換え
			if (objectDomain.getStatus() == null) {
				// 公開ドキュメントとして登録
				FormatDomain publicDocumentFormat = formatService.getByDefinitionName(ConfigUtils.getByKey("FORMAT_NAME_PUBLIC"));
				file = fileDao.getByObjectAndFormat(objectDomain, formatDomain);
				File orgFile = new File(file.getDirectory().getPath() + StringUtils.getFileBody(FileUtil.getFileName(objectDomain, file)) + fileExt);
				File dstFile = new File(publicDocumentFormat.getOnlineDirectory().getPath() + objectDomain.getId() + fileExt);
				FileUtils.createSymbolicLink(orgFile, dstFile);
				FileUtils.checkin(objectDomain, publicDocumentFormat, file.getName(), file.getSize());

				// Delete Relation For Old Revision
				if (objectDomain.getRevision() > 0) {
					// Old Revision
					ObjectDomain latestObj = objectDomains.get(objectDomains.size() - 2);

					// 最新履歴に設定する
					VersionUtils.setLatest(objectDomain);

					// Unlock (これを実行しないとステータスが「改訂中」のまま残る)
					AppDocumentUtil.unLock(latestObj);

					// Parent Relation
					List<RelationDomain> parentRelList = AppDocumentUtil.getParentRelationByRelTypeAndChild(relationTypeDomain, latestObj,
							accessRoleType);
					if (parentRelList != null) {
						// Delete Relation
						for (int i = 0; i < parentRelList.size(); i++) {
							RelationDomain relationDomain = parentRelList.get(i);
							relationService.delete(relationDomain);
						}
					}

					// 前リビジョンがロックされている（チェックアウトされているドキュメントにチェックインを行う）場合
					if(latestObj.getLockUser() != null) {
						// リンク自動更新処理を呼び出す。
						// チェックアウト無しでチェックインする場合は、リンク自動更新処理は呼び出さない。
						// リンクリレーションの子オブジェクト（ドキュメント）を最新版のドキュメントに設定する
						EIMSession sess = jp.co.ctc_g.eim.framework.common.util.EIMThreadContext.getEIMSession();
						EIMObject object = ObjectUtils.getObjectById(sess, objectDomain.getId());

						UpdateObjectLinkUtil.actUpdateObjectLinkByDocument(sess, object);

						// OCR
						if(OptionConfData.getInstance().ocrFlg){
							// OCR処理ステータスが「0:処理待ち」の場合、OCR処理オブジェクトを作成する
							if(AppOcrUtil.getOcrProcessingStatus(sess, object) == AppConstant.OCR_PROC_STATUS_PROCESS_WAIT){
								AppOcrUtil.createOcrProcessingObject(sess, object);
							}
						}
					}
				}
			}

			// 元のオブジェクトから、署名・暗号化に関する属性、ファイルを削除する
			if (getSignAndEncrFlg()) {
				AppDocumentUtil.setAttrLong(objectDomain, ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_SIGN_ENC_STATUS"),
						AppConstant.SIGNENCR_KIND_NOT_SIGNENCR);
				AppDocumentUtil.deleteAttribute(objectDomain, ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_SIGN_ENC_STATUS"));

				objectDomain = objectService.getById(objectDomain.getId());
				FileDomain signEncFile = fileDao.getByObjectAndFormat(objectDomain, formatSignEnc);
				if (signEncFile != null) {
					File substance = new File(signEncFile.getDirectory().getPath() + StringUtils.getFileBody(FileUtil.getFileName(objectDomain, signEncFile)) + StringUtils.nullToBlank(signEncFile.getExt()));
					if (substance.exists()) {
						substance.delete();
					}
					fileDao.delete(objectDomain, signEncFile);
				}
			}

			// Access History - 「チェックイン」
			AccessUtils.createAccess(objectDomain, "EIM.ACCESS.TYPE.CHECKIN");

			//操作履歴 チェックイン時にファイル名が変更した場合に新しいファイル名で記録できないのでAOP廃止
			OperationHistoryUtils.createOperationHistory(objectDomain, 2, 2010, 3, 12, objectDomain.getId(), documentDomain.getName(),
														-1, -1, -1, null, objectDomain.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PASS")).getStringList().get(0));

		} catch (Exception e) {
			throw e;
		}
	}

	/**
	 * ドキュメントを改訂中に変更します。
	 *
	 * @param documentDomain ドキュメントドメイン
	 * @return DocumentDomain
	 * @throws Exception
	 */
	public DocumentDomain checkout(DocumentDomain documentDomain) throws Exception {
		if (documentDomain == null) {
			// 引数documentDomainとしてnullが入力されました。
			throw new EIMException("EIM.ERROR.LOGIC.OBJECT.VALUE.ILLEGAL");
		}

		Map<Long, ObjectDomain> revObjectMap = null;

		try{
			// 先に処理対象ドキュメントの同一バージョン内のオブジェクト全てに行ロックをかける
			revObjectMap = VersionUtils.getVersion(documentDomain);
			if (!revObjectMap.isEmpty()) {
				AppDocumentUtil.lockObjects(revObjectMap);
			}else{
				// オブジェクトが存在しない、エラー処理
				throw new EIMException("EIM.ERROR.LOGIC.NODOCUMENTORFOLDER");
			}
		}
		catch(EIMException e){
            if (e.getMessageKey().equals("EIM.ERROR.OBJECT.NOTFOUND")) {
                throw new EIMException("EIM.ERROR.LOGIC.NODOCUMENTORFOLDER");
            }
            else {
                throw e;
            }

		}

		// オブジェクトの取得
		ObjectDomain objectDomain = objectService.getById(documentDomain.getId());
		if (objectDomain == null) {
			// オブジェクトが存在しない、エラー処理
			throw new EIMException("EIM.ERROR.LOGIC.NODOCUMENTORFOLDER");
		}

		List<ObjectDomain> objectDomains = Arrays.asList(revObjectMap.values().toArray(new ObjectDomain[0]));

		// Check Object
		if (AppDocumentUtil.isObjectInRecycle(objectDomain)) {
			// 選択したドキュメントは存在しません。
			throw new EIMException("EIM.ERROR.LOGIC.NODOCUMENT");
		}

		// Check Status (WFなしドキュメントもチェックアウトできる)
		if (objectDomain.getStatus() != null){
			if(objectDomain.getStatus().getType().getBase().getId() != AppConstant.STATUS_TYPE_KIND_ID_PUBLIC) {
				// 未公開のドキュメントチェックインできない
				throw new EIMException("EIM.ERROR.LOGIC.NOTPUBLIC", objectDomain.getName());
			}

			//WF付フォルダ内のドキュメントは編集中ステータスでもチェックアウトできない
			//親情報を取得
			// 「ドキュメント」リレーションタイプ
			RelationTypeDomain relType = AppDocumentLogicUtil.getDocumentRelType();
			RelationCriteria relCriteria = new RelationCriteria();
			relCriteria.setChildObjectId(objectDomain.getId());
			relCriteria.setRelationTypeId(relType.getId());
			List<RelationDomain> relDomainList = relationService.getList(relCriteria);
			RelationDomain relDomain = relDomainList.get(0);
			ObjectDomain parentObject = relDomain.getParent();
			// ワークフロー付きフォルダ下の場合は、チェックアウトできない
			if (parentObject.getStatus() != null) {
				throw new EIMException("EIM.ERROR.LOGIC.NOCHECKOUTROLE");
			}
		}

		// Check Lock User
		UserDomain user = objectDomain.getLockUser();

		if ((user != null) || (objectDomain.getStatus() == null // WFなしドキュメントの場合はチェックアウト可否判定をする
				&& !AppDocumentLogicUtil.isCheckoutEnabledNoWFDoc(objectDomain, objectDomains))) {
			throw new EIMException("EIM.ERROR.LOGIC.CHECKOUTED", objectDomain.getName());
		}

		// 対象が最新のリビジョンかチェック
		ObjectDomain maxObjectDomain = objectDomains.get(objectDomains.size() - 1);
		if (maxObjectDomain.getRevision() != objectDomain.getRevision()) {
			throw new EIMException("EIM.ERROR.LOGIC.NOCHECKOUTROLE", objectDomain.getName());
		}

		// revisionUp(WebDAV用に生成したObjectServiceで実施する)
		ObjectDomain newObjectDomain = webdavObjectService.revisionUp(objectDomain);

		// WFありの場合、公開フォーマットのメタ情報と実ファイルを削除
		if (objectDomain.getStatus() != null) {
			FormatDomain formatDomain = formatService.getByDefinitionName(EIMConfig.get("FORMAT_NAME_PUBLIC"));
			FileDomain fileDomain = fileDao.getByObjectAndFormat(newObjectDomain, formatDomain);
			fileDao.delete(newObjectDomain, fileDomain);

			File newObjPubFile = new File(formatDomain.getOnlineDirectory().getPath() + FileUtil.getFileName(newObjectDomain, fileDomain));
			if (newObjPubFile != null) {
				newObjPubFile.delete();
			}
		}else{
			// WFなしの場合、公開フォーマットの実ファイルを削除し、シンボリックリンクを作成。
			FormatDomain formatDomain = formatService.getByDefinitionName(EIMConfig.get("FORMAT_NAME_PUBLIC"));
			FileDomain baseFile = fileDao.getByObjectAndFormat(newObjectDomain, formatService.getDefaultByObjectType(objectDomain.getType()));

			File orgFile = new File(baseFile.getDirectory().getPath() + FileUtil.getFileName(newObjectDomain, baseFile));
			File newObjPubFile = new File(formatDomain.getOnlineDirectory().getPath() + newObjectDomain.getId() + baseFile.getExt());
			FileUtils.createSymbolicLink(orgFile, newObjPubFile);
		}

		// update lockUser
		TransactionContext tx = EIMThreadContext.getTransactionContext();
		UserDomain loginUser = tx.getUser();
		objectDomain.setLockUser(loginUser);

		//元のObjectの更新者ユーザ情報を取得
		UserDomain modifyUser = objectDomain.getModificationUser();
		if(loginUser != modifyUser ){
			tx.setUser(modifyUser);
			try{
				webdavObjectService.update(objectDomain);
				//元のログインユーザにセッションを戻す
				tx.setUser(loginUser);
			}catch(Exception e){
				//元のログインユーザにセッションを戻す
				tx.setUser(loginUser);
				throw e;
			}
		}else{
			webdavObjectService.update(objectDomain);
		}


		// 非継承の属性を全て削除する
		for (int i = 0; i < AppConstant.NONINHERIT_ATTRIBUTE_DEFNAME.length; i++) {
			AppDocumentUtil.deleteAttribute(newObjectDomain, AppConstant.NONINHERIT_ATTRIBUTE_DEFNAME[i]);
		}

		// 非継承なオブジェクト型属性を削除する
		ObjectTypeLayoutService objectTypeLayoutService = (ObjectTypeLayoutService)ApplicationContextLoader.getApplicationContext().getBean("documentObjectTypeLayoutService");
		ObjectTypeLayoutDomain objTypeLayoutdomain = objectTypeLayoutService.getById(new Long(objectDomain.getType().getId()));
		for (AttributeTypeLayoutDomain atttypeLayout : objTypeLayoutdomain.getAttributeLayoutList()) {
			if (!((DocumentAttributeTypeLayoutDomain) atttypeLayout).isInheritanceFlag() && atttypeLayout.getValueType().equals(ValueTypeEnum.OBJECT)) {
				AppDocumentUtil.deleteAttribute(newObjectDomain, atttypeLayout.getDefinitionName());
			}
		}

		// パスを設定
		String path = StringUtils.nullToBlank(AppDocumentUtil.getPath(objectDomain));

		AppDocumentUtil.setPath(newObjectDomain, path);

		VersionUtils.setLatest(objectDomain);
		// ドキュメント管理の使用でlatestflagをobjectとnewObjectで２つ立てる
		// VersionUtils.setLatest(objectDomain);ではフラグを１つしか立てないようにしているので、
		// かならずこの後に呼ぶ
		VersionUtils.setLatestWithNoCheck(newObjectDomain, true);

		DocumentDomain returnDocumentDomain = getDocumentByObjectDomain(newObjectDomain);

		// Access History
		// 「ロック」
		AccessUtils.createAccess(objectDomain, "EIM.ACCESS.TYPE.LOCK");
		// 「レビジョンアップ」
		AccessUtils.createAccess(objectDomain, "EIM.ACCESS.TYPE.REBISIONUP");
		// 「ダウンロード」
		//AccessUtils.createAccess(objectDomain, "EIM.ACCESS.TYPE.DOWNLOAD");

		//操作履歴 AOP利用
		//OperationHistoryUtils.createOperationHistory(objectDomain, 2, 2011, 3, 12, objectDomain.getId(), objectDomain.getName(),
		//											-1, -1, -1, null, objectDomain.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PASS")).getStringList().get(0));


		return returnDocumentDomain;
	}

	/**
	 * ドキュメントの改訂中を取消しします。
	 *
	 * @param documentDomain ドキュメントドメイン
	 * @throws Exception
	 */
	public void cancelCheckout(DocumentDomain documentDomain) throws Exception {
		if (documentDomain == null) {
			// 引数documentDomainとしてnullが入力されました。
			throw new EIMException("EIM.ERROR.LOGIC.OBJECT.VALUE.ILLEGAL");
		}

		Map<Long, ObjectDomain> revObjectMap = null;

		try{
			// 先に処理対象ドキュメントの同一バージョン内のオブジェクト全てに行ロックをかける
			revObjectMap = VersionUtils.getVersion(documentDomain);
			if (!revObjectMap.isEmpty()) {
				AppDocumentUtil.lockObjects(revObjectMap);
			}else{
				// オブジェクトが存在しない、エラー処理
				throw new EIMException("EIM.ERROR.LOGIC.NODOCUMENTORFOLDER");
			}
		}
		catch(EIMException e){

            if (e.getMessageKey().equals("EIM.ERROR.OBJECT.NOTFOUND")) {
                throw new EIMException("EIM.ERROR.LOGIC.NODOCUMENTORFOLDER");
            }
            else {
                throw e;
            }
		}

		// オブジェクトの取得
		ObjectDomain objectDomain = objectService.getById(documentDomain.getId());
		if (objectDomain == null) {
			// オブジェクトが存在しない、エラー処理
			throw new EIMException("EIM.ERROR.LOGIC.NODOCUMENTORFOLDER");
		}

		// 引数のdocumentDomainがチェックアウト取消対象か判定する
		// Check Status
		UserDomain user = objectDomain.getLockUser();
		if (user == null) {
			DocumentDomain argDoc = new DocumentDomain();
			argDoc.setId(objectDomain.getId());
			List<DocumentDomain> docRevisionList = this.getRevisionList(argDoc);
			Iterator<DocumentDomain> ite = docRevisionList.iterator();
			DocumentDomain cancelTargetDomain = null;
			while (ite.hasNext()) {
				DocumentDomain docDomain = (DocumentDomain) ite.next();
				if (docDomain.isLatest() && docDomain.getLockUser() != null) {
					cancelTargetDomain = docDomain;
					break;
				}
			}

			if (cancelTargetDomain == null) {
				// 改訂中ではありません。
				throw new EIMException("EIM.ERROR.LOGIC.NOREVISING", objectDomain.getName());
			} else {
				// 取消対象の情報をDBから取得する
				objectDomain = objectService.getById(cancelTargetDomain.getId());
				if (objectDomain != null) {
					user = objectDomain.getLockUser();
				} else {
					// オブジェクトが存在しない、エラー処理
					throw new EIMException("EIM.ERROR.LOGIC.NODOCUMENTORFOLDER");
				}
			}
		}

		// Check Object
		if (AppDocumentUtil.isObjectInRecycle(objectDomain)) {
			// 選択したドキュメントは存在しません。
			throw new EIMException("EIM.ERROR.LOGIC.NODOCUMENT");
		}

		// Check Lock User and System Security
		TransactionContext tx = EIMThreadContext.getTransactionContext();
		UserDomain loginUser = tx.getUser();

		// ユーザがsystemセキュリティに所属しているか確認
		boolean isSystemSecurity = checkSystemSecurity(loginUser);

		// ユーザがチェックアウトしたユーザ、もしくはsystemセキュリティに所属したユーザでなければエラー
		if (user.getId() != loginUser.getId() && !isSystemSecurity) {
			throw new EIMException("EIM.ERROR.LOGIC.NOCHECKOUTUSER");
		}

		List<ObjectDomain> objectDomains = Arrays.asList(revObjectMap.values().toArray(new ObjectDomain[0]));
		ObjectDomain latestObj = objectDomains.get(objectDomains.size() - 1);
		// ステータスが編集中以外の場合はチェックアウト取消は出来ない
		// ただし、WFなしドキュメント(ステータスの値が0)は無条件でチェックアウト取消可能
		if (latestObj.getStatus() != null && latestObj.getStatus().getType().getBase().getId() != AppConstant.STATUS_TYPE_KIND_ID_EDITTING) {
			throw new EIMException("EIM.ERROR.LOGIC.DOCUMENT.NOTEDITING");
		}

		// 直接編集でロックしている場合は取消不可
		if (objectDomain.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_WEBDAV_LOCK_FLAG")) != null) {
			throw new EIMException("EIM.ERROR.LOGIC.NOCHECKOUTROLE", objectDomain.getName());
		}

		// UnLock
		objectDomain = AppDocumentUtil.unLock(objectDomain);

		// オブジェクト削除
		objectService.delete(latestObj);

		// 取消後、取消元を最新リビジョンにセットする
		VersionUtils.setLatestWithNoCheck(objectDomain, true);

		// Access History - 「ロック解除」
		AccessUtils.createAccess(objectDomain, "EIM.ACCESS.TYPE.LOCKCANCEL");

		//操作履歴 AOP利用
		//OperationHistoryUtils.createOperationHistory(objectDomain, 2, 2009, 3, 12, objectDomain.getId(), objectDomain.getName(),
		//													-1, -1, -1, null, objectDomain.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PASS")).getStringList().get(0));
	}

	/**
	 * ユーザがsystemセキュリティに所属しているか確認します。
	 *
	 * @param loginUser
	 * @return
	 * @throws Exception
	 */
	private boolean checkSystemSecurity(UserDomain loginUser) throws Exception{

		// systemセキュリティの情報を取得
		ApplicationContext context = ApplicationContextLoader.getApplicationContext();
		SecurityService securityService = (SecurityService) context.getBean("securityService2");
		List<AccessEntryDomain> accessEntryDomainList = securityService.getByDefinitionName(ConfigUtils.getByKey("SECURITY_NAME_SYSTEM")).getAccessEntryList();
		// systemセキュリティのEntryDomainを確認し、現在ログインしているユーザがsystemセキュリティに所属しているか確認
		for (AccessEntryDomain accessEntryDomain : accessEntryDomainList) {
			if (accessEntryDomain.getEntryElement().getId() == loginUser.getId()) {
				// 現在ログインしているユーザがSTATUS_UPの操作権限を持っていることを確認
				for (AccessRoleDomain accessRoleDomain : accessEntryDomain.getAccessRoleList()) {
					if (accessRoleDomain.getType().getId() == EIMAccessRole.STATUS_UP &&
								accessRoleDomain.getPermission().equals(AccessRolePermissionModeEnum.ACCEPT)) {
						return true;
					}
				}
			}
		}
		return false;
	}

	/**
	 * ドキュメントをごみ箱に移動します。
	 *
	 * @param documentDomainList ドキュメントドメインリスト
	 * @throws Exception
	 */
	public void dispose(List<DocumentDomain> documentDomainList) throws Exception {
		if (documentDomainList == null) {
			// 引数documentDomainListとしてnullが入力されました。
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

		ObjectTypeDomain docObjType = AppDocumentLogicUtil.getDocumentObjectType();
		List<Long> docObjTypeIds = AppDocumentLogicUtil.getChildObjectTypeList(docObjType.getId());

		// 渡されてきたオブジェクト分ループ
		for (DocumentDomain documentDomain : documentDomainList) {

			// 先に処理対象ドキュメントの同一バージョン内のオブジェクト全てに行ロックをかける
			Map<Long, ObjectDomain> revObjectMap = VersionUtils.getVersion(documentDomain);
			if (!revObjectMap.isEmpty()) {
				AppDocumentUtil.lockObjects(revObjectMap);
			}else{
				// オブジェクトが存在しない、エラー処理
				throw new EIMException("EIM.ERROR.LOGIC.NODOCUMENTORFOLDER");
			}

			ObjectDomain objectDomain = objectService.getById(documentDomain.getId());
			if (objectDomain == null) {
				throw new EIMException("EIM.ERROR.LOGIC.NODOCORFOLORTAG");
			}

			// オブジェクトタイプチェック
			if (!docObjTypeIds.contains(objectDomain.getType().getId())) {
				throw new EIMException("EIM.ERROR.LOGIC.NOTEXIST.DOCFOLWS");
			}

			// 選択対象がタグが付与されたドキュメント・フォルダ・タグの場合は削除できない。
			if (AppDocumentLogicUtil.isTagAssignedObject(objectDomain, relationTypeDomain, false)) {
				throw new EIMException("EIM.ERROR.LOGIC.CANTDELTAGGEDDOCFOLORTAG");
			}
			AccessRoleTypeDomain accessRoleType = new AccessRoleTypeDomain("READ");
			accessRoleType.setId(EIMAccessRole.READ);
			List<RelationDomain> relList = AppDocumentUtil.getParentRelationByRelTypeAndChild(relationTypeDomain, objectDomain,
					accessRoleType);
			if (relList == null || relList.size() == 0) {
				throw new EIMException("EIM.ERROR.LOGIC.NODOCUMENT");
			}
			ObjectDomain parentObj = relList.get(0).getParent();
			AttributeUtil.checkStatus(objectDomain, parentObj);

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
			String docPass = EIMConfig.get("ATTR_NAME_DOCUMENT_PASS");

			// パス属性を取得
			AttributeTypeDomain pathAttrType = attributeTypeService.getByDefinitionName(docPass);

			// 削除対象オブジェクトからワークスペース固有ごみ箱オブジェクトのパスを取得
			String path = objectDomain.getAttribute(docPass).getStringList().get(0);
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
					accessRoleType);
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
			EIMObject eimObj = ObjectUtils.getObjectById(sess, documentDomain.getId());
			EIMObject parentEimObj = ObjectUtils.getObjectById(sess, parentObj.getId());

			// ドキュメント 対象ドキュメント + 親フォルダ・親ワークスペース
			AppUpdateNoticeUtils.updateNoticeInsertObject(sess, eimObj, "SEARCHFW_LOGICDEL_DOCUMENT", true);
			AppUpdateNoticeUtils.updateNoticeInsertParent(sess, parentEimObj, "SEARCHFW_LOGICDEL_PARENT_FOLDER", "SEARCHFW_LOGICDEL_PARENT_WORKSPACE", null);

			//操作履歴 AOP利用
			//OperationHistoryUtils.createOperationHistory(objectDomain, 2, 2032, 2, 12, objectDomain.getId(), objectDomain.getName(),
			//													-1, -1, -1, null, objectDomain.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PASS")).getStringList().get(0));
		}
	}

	/**
	 * ドキュメントを削除します。
	 *
	 * @param documentDomainList ドキュメントドメインリスト
	 * @throws Exception
	 */
	public void delete(List<DocumentDomain> documentDomainList) throws Exception {
		if (documentDomainList == null) {
			// 引数documentDomainListとしてnullが入力されました。
			throw new EIMException("EIM.ERROR.LOGIC.OBJECT.VALUE.ILLEGAL");
		}

		// 「ドキュメント」リレーションタイプ
		RelationTypeDomain relationTypeDomain = AppDocumentLogicUtil.getDocumentRelType();

		ObjectTypeDomain docObjType = AppDocumentLogicUtil.getDocumentObjectType();
		List<Long> docObjTypeIds = AppDocumentLogicUtil.getChildObjectTypeList(docObjType.getId());

		// 渡されてきたオブジェクト分ループ
		for (DocumentDomain documentDomain : documentDomainList) {

			// 先に処理対象のドキュメントの同一バージョン内のオブジェクト全てに行ロックをかける
			// バージョンを取得
			Map<Long, ObjectDomain> revObjectMap = VersionUtils.getVersion(documentDomain);
			if (!revObjectMap.isEmpty()) {
				// 行ロックをかける
				AppDocumentUtil.lockObjects(revObjectMap);
			}else{
				// オブジェクトが存在しない、エラー処理
				throw new EIMException("EIM.ERROR.LOGIC.NODOCUMENTORFOLDER");
			}

			ObjectDomain objectDomain = objectService.getById(documentDomain.getId());
			if (objectDomain == null) {
				throw new EIMException("EIM.ERROR.LOGIC.NODOCORFOLORTAG");
			}

			// オブジェクトタイプチェック
			if (!docObjTypeIds.contains(objectDomain.getType().getId())) {
				throw new EIMException("EIM.ERROR.LOGIC.NOTEXIST.DOCFOLWS");
			}

			// 選択対象がタグが付与されたドキュメント・フォルダ・タグの場合は削除できない。
			if (AppDocumentLogicUtil.isTagAssignedObject(objectDomain, relationTypeDomain, false)) {
				throw new EIMException("EIM.ERROR.LOGIC.CANTDELTAGGEDDOCFOLORTAG");
			}

			if (objectDomain.isLatest()) {
				AccessRoleTypeDomain accessRoleType = new AccessRoleTypeDomain("READ");
				accessRoleType.setId(EIMAccessRole.READ);
				List<RelationDomain> relList = AppDocumentUtil.getParentRelationByRelTypeAndChild(relationTypeDomain, objectDomain,
						accessRoleType);
				if (relList == null || relList.size() == 0) {
					throw new EIMException("EIM.ERROR.LOGIC.NODOCUMENT");
				}

				ObjectDomain parentObj = relList.get(0).getParent();
				AttributeUtil.checkStatus(objectDomain, parentObj);
			}

			// 削除可能か否かを判定
			AttributeUtil.checkDeleteEnable(objectDomain);

			// バージョン分繰り返す
			for (Iterator<ObjectDomain> i = revObjectMap.values().iterator(); i.hasNext();) {
				ObjectDomain delTargetObj = i.next();

				AppDocumentUtil.deleteObjectAll(delTargetObj);// ドキュメント削除
			}

			//操作履歴 AOP利用
			//OperationHistoryUtils.createOperationHistory(objectDomain, 2, 2057, 2, 12, objectDomain.getId(), objectDomain.getName(),
			//													-1, -1, -1, null, objectDomain.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PASS")).getStringList().get(0));
		}
	}

	/**
	 * リビジョンリストを取得します。
	 *
	 * @param documentDomain ドキュメントドメイン
	 * @return List<DocumentDomain>
	 * @throws Exception
	 */
	public List<DocumentDomain> getRevisionList(DocumentDomain documentDomain) throws Exception {
		if (documentDomain == null) {
			// 引数documentDomainとしてnullが入力されました。
			throw new EIMException("EIM.ERROR.LOGIC.OBJECT.VALUE.ILLEGAL");
		}

		List<DocumentDomain> documentDomainList = new ArrayList<DocumentDomain>();

		// オブジェクトの取得
		ObjectDomain objectDomain = objectService.getById(documentDomain.getId());
		if (objectDomain == null) {
			// オブジェクトが存在しない、エラー処理
			throw new EIMException("EIM.ERROR.LOGIC.NODOCUMENTORFOLDER");
		}

		// オブジェクトタイプチェック
		if (!AppDocumentLogicUtil.isTypeOfDocument(objectDomain.getType())) {
			// ドキュメントを指定しない、エラー処理
			throw new EIMException("EIM.ERROR.LOGIC.NOTEXIST.DOCFOLWS");
		}

		// 先に処理対象のドキュメントの同一バージョン内のオブジェクトを取得する
		Map<Long, ObjectDomain> revObjectMap = VersionUtils.getVersion(objectDomain);
		List<ObjectDomain> objList = Arrays.asList(revObjectMap.values().toArray(new ObjectDomain[0]));

		// ブランチリレーションタイプの取得
		RelationTypeDomain relTypeBranch = relationTypeService.getByDefinitionName(ConfigUtils.getByKey("RELATION_TYPE_NAME_BRANCH"));

		// 削除フラグ属性タイプの取得
		AttributeTypeDomain attTypeDelFlag = attributeTypeService.getByDefinitionName(ConfigUtils.getByKey("ATTR_NAME_BRANCH_TARGET_DELETE"));

		for (int i = 0; i < objList.size(); i++) {
			// オブジェクト情報の取得
			ObjectDomain parentObject = (ObjectDomain) objList.get(i);
			// オブジェクト属性情報の設定

			// 「署名・暗号化状態」属性
			String attributeTypeName = ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_SIGN_ENC_STATUS");
			AttributeDomain attributeDomain = parentObject.getAttribute(attributeTypeName);
			if (attributeDomain == null) {
				attributeDomain = new AttributeDomain();
				AttributeTypeDomain attributeTypeDomain = attributeTypeService.getByDefinitionName(attributeTypeName);
				// 属性タイプ
				attributeDomain.setAttributeType(attributeTypeDomain);
				// 属性値
				attributeDomain.setLong(AppConstant.SIGNENCR_KIND_NOT_SIGNENCR);
				parentObject.getAttributeList().add(attributeDomain);
			}

			// 「PDF分割状態」属性
			attributeTypeName = ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PDF_DIVIDE_STATUS");
			attributeDomain = parentObject.getAttribute(attributeTypeName);
			if (attributeDomain == null) {
				attributeDomain = new AttributeDomain();
				AttributeTypeDomain attributeTypeDomain = attributeTypeService.getByDefinitionName(attributeTypeName);
				// 属性タイプ
				attributeDomain.setAttributeType(attributeTypeDomain);
				// 属性値
				attributeDomain.setLong(AppConstant.FLAG_OFF);
				parentObject.getAttributeList().add(attributeDomain);
			}

			// ドキュメントドメインに情報を設定する
			documentDomainList.add(getDocumentByObjectDomain(parentObject));
			AccessRoleTypeDomain accessRoleType = new AccessRoleTypeDomain("READ");
			accessRoleType.setId(EIMAccessRole.READ);
			List<RelationDomain> relationDomainList = AppDocumentUtil.getChildRelationListByRelType(relTypeBranch, parentObject,
					accessRoleType);

			// 再帰的にブランチツリーを取得します。
			List<DocumentDomain> branchTreeObject = getBranchTree(parentObject, relationDomainList, relTypeBranch, attTypeDelFlag);

			// 子リレーションを取得
			if (branchTreeObject == null || branchTreeObject.size() == 0) {
				// 対象のオブジェクトが最下位の場合終了
				continue;
			}

			for (int j = 0; j < branchTreeObject.size(); j++) {
				// ドキュメントドメインにブランチツリー情報を設定する
				documentDomainList.add(branchTreeObject.get(j));
			}
		}

		return documentDomainList;
	}

	/**
	 * 再帰的にブランチツリーを取得します。
	 *
	 * @param parentObject 親オブジェクト
	 * @param parentNode 親ノード情報
	 * @param childRelList 子リレーションリスト
	 */
	private List<DocumentDomain> getBranchTree(ObjectDomain parentObject, List<RelationDomain> childRelList, RelationTypeDomain relTypeBranch,
			AttributeTypeDomain attTypeDelFlag) throws Exception {

		List<DocumentDomain> resutltList = new ArrayList<DocumentDomain>();
		// 子リレーションを取得
		if (childRelList == null || childRelList.size() <= 0) {
			// 対象のオブジェクトが最下位の場合終了
			return null;
		}

		// ブランチリストの生成
		List<ObjectDomain> branchList = new ArrayList<ObjectDomain>();
		for (int i = 0; i < childRelList.size(); i++) {
			// 削除チェック
			RelationDomain rel = childRelList.get(i);
			AttributeDomain delFlag = rel.getAttribute(attTypeDelFlag.getDefinitionName());
			if (delFlag != null && delFlag.getLong() == AppConstant.FLAG_ON) {
				continue;
			}

			// オブジェクトのリストの取得
			ObjectDomain childObject = rel.getChild();

			Map<Long, ObjectDomain> versionMap = VersionUtils.getVersion(childObject);
			branchList = Arrays.asList(versionMap.values().toArray(new ObjectDomain[0]));
			// ノードリストの生成
			List<DocumentDomain> nodeList = new ArrayList<DocumentDomain>();
			for (int j = 0; j < branchList.size(); j++) {
				// オブジェクト情報の取得
				ObjectDomain nodeObject = branchList.get(j);

				// オブジェクト属性情報の設定
				// 「署名・暗号化状態」属性
				String attributeTypeName = ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_SIGN_ENC_STATUS");
				AttributeDomain attributeDomain = parentObject.getAttribute(attributeTypeName);
				if (attributeDomain == null) {
					attributeDomain = new AttributeDomain();
					AttributeTypeDomain attributeTypeDomain = attributeTypeService.getByDefinitionName(attributeTypeName);
					// 属性タイプ
					attributeDomain.setAttributeType(attributeTypeDomain);
					// 属性値
					attributeDomain.setLong(AppConstant.SIGNENCR_KIND_NOT_SIGNENCR);
					parentObject.getAttributeList().add(attributeDomain);
				}

				// 「PDF分割状態」属性
				attributeTypeName = ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PDF_DIVIDE_STATUS");
				attributeDomain = parentObject.getAttribute(attributeTypeName);
				if (attributeDomain == null) {
					attributeDomain = new AttributeDomain();
					AttributeTypeDomain attributeTypeDomain = attributeTypeService.getByDefinitionName(attributeTypeName);
					// 属性タイプ
					attributeDomain.setAttributeType(attributeTypeDomain);
					// 属性値
					attributeDomain.setLong(AppConstant.FLAG_OFF);
					parentObject.getAttributeList().add(attributeDomain);
				}

				// 返却リストに追加
				nodeList.add(getDocumentByObjectDomain(nodeObject));
				AccessRoleTypeDomain accessRoleType = new AccessRoleTypeDomain("READ");
				accessRoleType.setId(EIMAccessRole.READ);
				List<RelationDomain> relationDomainList = AppDocumentUtil.getChildRelationListByRelType(relTypeBranch, nodeObject,
						accessRoleType);
				// 子ブランチの情報を再帰的に取得
				List<DocumentDomain> objectList = getBranchTree(nodeObject, relationDomainList, relTypeBranch, attTypeDelFlag);
				if (objectList == null || objectList.size() == 0) {
					continue;
				} else {
					nodeList.addAll(objectList);
				}
			}
			resutltList.addAll(nodeList);
		}
		return resutltList;
	}

	/**
	 * ドキュメントリストの対象バージョンを削除します。
	 *
	 * @param documentDomainList ドキュメントドメイン
	 * @throws Exception
	 */
	public void deleteRevision(List<DocumentDomain> documentDomainList) throws Exception {
		if (documentDomainList == null) {
			// 引数documentDomainListとしてnullが入力されました。
			throw new EIMException("EIM.ERROR.LOGIC.OBJECT.VALUE.ILLEGAL");
		}
		// ブランチリレーションタイプの取得
		RelationTypeDomain relTypeBranch = relationTypeService.getByDefinitionName(ConfigUtils.getByKey("RELATION_TYPE_NAME_BRANCH"));

		ObjectTypeDomain docObjType = AppDocumentLogicUtil.getDocumentObjectType();
		List<Long> docObjTypeIds = AppDocumentLogicUtil.getChildObjectTypeList(docObjType.getId());

		for (DocumentDomain documentDomain : documentDomainList) {

			Map<Long, ObjectDomain> revObjectMap = null;

			try{

				// 先に処理対象ドキュメントの同一バージョン内のオブジェクト全てに行ロックをかける
				revObjectMap = VersionUtils.getVersion(documentDomain);
				if (!revObjectMap.isEmpty()) {
					AppDocumentUtil.lockObjects(revObjectMap);
				}else{
					// オブジェクトが存在しない、エラー処理
					throw new EIMException("EIM.ERROR.LOGIC.NODOCUMENTORFOLDER");
				}

			}
			catch(EIMException e){
	            if (e.getMessageKey().equals("EIM.ERROR.OBJECT.NOTFOUND")) {
	                throw new EIMException("EIM.ERROR.LOGIC.NODOCUMENTORFOLDER");
	            }
	            else {
	                throw e;
	            }

			}

			ObjectDomain objectDomain = objectService.getById(documentDomain.getId());
			if (objectDomain == null) {
				throw new EIMException("EIM.ERROR.LOGIC.NODOCORFOLORTAG");
			}

			// オブジェクトタイプチェック
			if (!docObjTypeIds.contains(objectDomain.getType().getId())) {
				throw new EIMException("EIM.ERROR.LOGIC.NOTEXIST.DOCFOLWS");
			}

			// WF付きドキュメントの場合
			if ((objectDomain.getStatus() != null)
					&& (objectDomain.getAttribute(ConfigUtils.getByKey("ATTR_NAME_FOLDER_HIGHER_WF_FOLDER")) == null)) {
				long stsKind = objectDomain.getStatus().getType().getBase().getId();

				// ステータスが「編集中」でも「公開済」でもない場合
				if (stsKind != AppConstant.STATUS_TYPE_KIND_ID_EDITTING && stsKind != AppConstant.STATUS_TYPE_KIND_ID_PUBLIC) {
					// {0} は、承認中です。削除できません。
					throw new EIMException("EIM.ERROR.LOGIC.NODELETE.APPROVAL", new Object[] { objectDomain.getName() });
				}
			}

			if (objectDomain.isLatest()) {
				throw new EIMException("EIM.ERROR.LOGIC.REVISION.DELETE");
			}

			// 最新バージョンの１つ前のバージョンチェック
			List<ObjectDomain> objectDomains = Arrays.asList(revObjectMap.values().toArray(new ObjectDomain[0]));
			ObjectDomain formerObject = objectDomains.get(objectDomains.size() - 2);
			if (formerObject.getId() == objectDomain.getId()) {
				throw new EIMException("EIM.ERROR.LOGIC.NEXTNEW.REVISION.DELETE");
			}

			// ブランチコピー元チェック
			List<RelationDomain> relationDomainList = AppDocumentUtil.getChildRelationListByRelType(relTypeBranch, objectDomain, null);
			if (relationDomainList != null && relationDomainList.size() > 0) {
				throw new EIMException("EIM.ERROR.LOGIC.BRANCH.REVISION.DELETE");
			}

			// ドキュメント削除
			AppDocumentUtil.deleteObjectAll(objectDomain);

			// Access History - 「バージョン削除」
			AccessUtils.createAccess(formerObject, "EIM.ACCESS.TYPE.DELETEREVISION");

			//操作履歴 AOP利用
			//OperationHistoryUtils.createOperationHistory(objectDomain, 2, 2070, 2, 12, objectDomain.getId(), objectDomain.getName(),
			//											-1, -1, -1, null, objectDomain.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PASS")).getStringList().get(0)+",(Revision:"+objectDomain.getRevision()+")");
		}
	}

	/**
	 * 指定したドキュメントをPlaceDomainの下に移動します。
	 *
	 * @param documentDomain ドキュメントドメイン
	 * @param placeDomain ワークスペースドメイン又はフォルダドメイン
	 * @throws Exception
	 */
	public void move(DocumentDomain documentDomain, PlaceDomain placeDomain) throws Exception {
		if (documentDomain == null) {
			// 引数folderDomainとしてnullが入力されました。
			throw new EIMException("EIM.ERROR.LOGIC.OBJECT.VALUE.ILLEGAL");
		}

		if (placeDomain == null) {
			// 引数parentDomainとしてnullが入力されました。
			throw new EIMException("EIM.ERROR.LOGIC.OBJECT.VALUE.ILLEGAL");
		}

		// 先に処理対象ドキュメントの同一バージョン内のオブジェクト全てに行ロックをかける
		Map<Long, ObjectDomain> revObjectMap = VersionUtils.getVersion(documentDomain);
		if (!revObjectMap.isEmpty()) {
			AppDocumentUtil.lockObjects(revObjectMap);
		}else{
			// オブジェクトが存在しない、エラー処理
			throw new EIMException("EIM.ERROR.LOGIC.NODOCUMENTORFOLDER");
		}

		// オブジェクトの取得
		ObjectDomain objectDomain = objectService.getById(documentDomain.getId());
		if (objectDomain == null) {
			// オブジェクトが存在しない、エラー処理
			throw new EIMException("EIM.ERROR.LOGIC.NOTEXIST.DOCFOLWS");
		}

		// オブジェクトタイプの取得
		ObjectTypeDomain objectTypeDomain = objectDomain.getType();
		if (objectTypeDomain != null) {
			// オブジェクトタイプチェック
			if (!AppDocumentLogicUtil.isTypeOfDocument(objectTypeDomain)) {
				throw new EIMException("EIM.ERROR.LOGIC.NOTEXIST.DOCFOLWS");
			}
		}

		// 対象が最新のリビジョンかのチェック
		if (!objectDomain.isLatest()) {
			throw new EIMException("EIM.ERROR.LOGIC.NOCUTANDPASTEROLE");
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

		// 「ドキュメント」リレーションタイプ
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
			AccessRoleTypeDomain accessRoleType = new AccessRoleTypeDomain("READ");
			accessRoleType.setId(EIMAccessRole.READ);
			List<RelationDomain> parentRelList = AppDocumentUtil.getParentRelationByRelTypeAndChild(relationTypeDomain, objectDomain,
					accessRoleType);
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
			//重複チェックを修正
			//relationService.create(relationDomain, DuplicateCheckModeEnum.INHERITTYPE);
			RelationUtils.createRelation(relationDomain);

			if (isParentRecycleBox) {
				// ブランチ情報の復帰
				RelationTypeDomain relTypeBranch = relationTypeService.getByDefinitionName(ConfigUtils.getByKey("RELATION_TYPE_NAME_BRANCH"));
				// ドキュメントの場合
				AppDocumentLogicUtil.returnDocumentBranch(objectDomain, relTypeBranch);
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

		} catch (Exception e) {
			throw e;
		}

	}

	/**
	 * チェックアウト中のファイル原本を差し替えます。
	 *
	 * @param documentDomain ドキュメントドメイン
	 * @param inputStream InputStream
	 * @throws Exception
	 */
	public void replaceOriginalFile(DocumentDomain documentDomain, InputStream inputStream) throws Exception {

		if (documentDomain == null) {
			// 引数documentDomainとしてnullが入力されました。
			throw new EIMException("EIM.ERROR.LOGIC.OBJECT.VALUE.ILLEGAL");
		}
		// ファイル存在チェック
		if (inputStream == null) {
			throw new EIMException("EIM.ERROR.LOGIC.NOUPLOADFILE");
		}

		// ファイルサイズ判定(最大ファイルサイズ)
		long maxFileSize = Long.parseLong(ConfigUtils.getByKey("UPLOAD_FILE_SIZE_MAX"));
		if (inputStream.available() > maxFileSize) {
			throw new EIMException("EIM.ERROR.LOGIC.UPLOAD.FILE.SIZE.OVER", maxFileSize);
		}

		// 先に処理対象のドキュメントの同一バージョン内のオブジェクト全てに行ロックをかける
		Map<Long, ObjectDomain> revObjectMap = VersionUtils.getVersion(documentDomain);
		if (!revObjectMap.isEmpty()) {
			AppDocumentUtil.lockObjects(revObjectMap);
		}else{
			// オブジェクトが存在しない、エラー処理
			throw new EIMException("EIM.ERROR.LOGIC.NODOCUMENTORFOLDER");
		}

		// オブジェクトの取得
		ObjectDomain objectDomain = objectService.getById(documentDomain.getId());
		if (objectDomain == null) {
			// オブジェクトが存在しない、エラー処理
			throw new EIMException("EIM.ERROR.LOGIC.NODOCUMENTORFOLDER");
		}

		// ファイル名チェック
		if (StringUtils.isEmpty(documentDomain.getName())) {
			// オブジェクトの名称に、NULLまたは空文字を使用しています。
			throw new EIMException("EIM.ERROR.LOGIC.OBJECT.NAME.LENGTH.ILLEGAL");
		}

		String fileExt = StringUtils.getFileExt(documentDomain.getName());
		if (fileExt == null) {
			fileExt = "";
		}

		if(revObjectMap != null && revObjectMap.size() < 2){
			throw new EIMException("EIM.ERROR.LOGIC.NOT.TARGET.REPLACE.ORIGINAL.DOCUMENT");
		}

		// ステータスのチェック
		// WFなしドキュメントでもチェックインは出来るものとする
		if (!OptionConfData.getInstance().enableApproverCheckin) {
			if (objectDomain.getStatus() != null && objectDomain.getStatus().getType().getBase().getId() != AppConstant.STATUS_TYPE_KIND_ID_EDITTING
					|| (objectDomain.getStatus() == null && objectDomain.getLockUser() != null)) {
				throw new EIMException("EIM.ERROR.LOGIC.NOTUPDATING", objectDomain.getName());
			}
		}else{
			//WFなし、「編集中」、「承認依頼中」ドキュメントは、チェックインは出来るものとする
			if ((objectDomain.getStatus() != null && objectDomain.getStatus().getType().getBase().getId() !=  AppConstant.STATUS_TYPE_KIND_ID_EDITTING
					&& objectDomain.getStatus().getType().getBase().getId() != AppConstant.STATUS_TYPE_KIND_ID_APPROVE)
						|| (objectDomain.getStatus() == null && objectDomain.getLockUser() != null)) {
				throw new EIMException("EIM.ERROR.LOGIC.NOTUPDATING", objectDomain.getName());
			//「承認依頼中」ドキュメントにて直接編集が可能か判定する
			}else if (objectDomain.getStatus() != null && objectDomain.getStatus().getType().getBase().getId() == AppConstant.STATUS_TYPE_KIND_ID_APPROVE) {
				checkStatus(objectDomain);
			}
		}

		// 対象が最新のリビジョンかのチェック
		if (!objectDomain.isLatest()) {
			throw new EIMException("EIM.ERROR.LOGIC.NOROLE");
		}

		// Check Lock User
		if (objectDomain.getRevision() > 0) {
			List<ObjectDomain> objectDomains = Arrays.asList(revObjectMap.values().toArray(new ObjectDomain[0]));
			ObjectDomain lockObj = objectDomains.get(objectDomains.size() - 2);
			TransactionContext tx = EIMThreadContext.getTransactionContext();
			// オブジェクトがロックされている場合のみ、ユーザーの比較を行う
			// チェックアウト無しでチェックインする場合は、過去ドキュメントはロックされていない
			if (lockObj != null && lockObj.getLockUser() != null && lockObj.getLockUser().getId() != tx.getUser().getId()) {
				throw new EIMException("EIM.ERROR.LOGIC.NOCHECKOUTUSER");
			}else if(lockObj != null && !lockObj.isLatest()){
				throw new EIMException("EIM.ERROR.LOGIC.NOT.TARGET.REPLACE.ORIGINAL.DOCUMENT");
			}
		}

		// フォーマット、ディレクトリの取得
		FormatDomain formatDomain = formatService.getDefaultByObjectType(objectDomain.getType());
		if (formatDomain == null) {
			// デフォルトフォーマットが設定されていません。
			throw new EIMException("EIM.ERROR.LOGIC.NODEFAULTFORMAT");
		}
		DirectoryDomain dir = formatDomain.getOnlineDirectory();

		try {

			FileDomain file = fileDao.getByObjectAndFormat(objectDomain, formatDomain);
			if (file == null) {
				// 選択したドキュメントは存在しません。
				throw new EIMException("EIM.ERROR.LOGIC.NODOCUMENT");
			}

			// 「ドキュメント」リレーションタイプ
			RelationTypeDomain relationTypeDomain = AppDocumentLogicUtil.getDocumentRelType();
			ObjectDomain parentObject = null;
			AccessRoleTypeDomain accessRoleType = new AccessRoleTypeDomain("READ");
			accessRoleType.setId(EIMAccessRole.READ);
			List<RelationDomain> relList = AppDocumentUtil.getParentRelationByRelTypeAndChild(relationTypeDomain, objectDomain,
					accessRoleType);
			if (relList != null && relList.size() > 0) {
				parentObject = relList.get(0).getParent();
			}

			// Rename
			if (!documentDomain.getName().equals(objectDomain.getName())) {
				//  同じ親を持つ同名のオブジェクトが既に存在するかチェック
				EIMSession sess = jp.co.ctc_g.eim.framework.common.util.EIMThreadContext.getEIMSession();
				EIMSearchSelectEIMRelation relationSelectTarget = new EIMSearchSelectEIMRelation();
				EIMSearchSelectEIMRelation.SearchConditionBuildHelper h = new EIMSearchSelectEIMRelation.SearchConditionBuildHelper();

				relationSelectTarget.setSourceObjects(EIMSearchSelectEIMRelation.SourceObjects.CHILD);
				relationSelectTarget.setCondition(h.group(h.opAnd())
						.addCondition(
								h.eq(h.opAnd(),
										EIMSearchSelectEIMRelation.PsedoAttributeTypeEnum.PARENT,
										parentObject.getId())
						)
				);

				EIMSearchSelectEIMObject parentSelectTarget = new EIMSearchSelectEIMObject();
				parentSelectTarget.setRole(EIMAccessRole.NONE);
				EIMSearchSelectEIMObject childSelectTarget = new EIMSearchSelectEIMObject();
				childSelectTarget.setRole(EIMAccessRole.READ);
				EIMSearchConditionGroup allConds = new EIMSearchConditionGroup(EIMSearchOperatorEnum.AND);
				EIMSearchConditionCompare condition = new EIMSearchConditionCompare(
						EIMSearchOperatorEnum.OR
						, EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.NAME
						, EIMSearchOperatorEnum.EQ
						, documentDomain.getName()
					);
				allConds.addCondition(condition);
				childSelectTarget.setCondition(allConds);

				List objList = SearchUtils.searchRelations(sess,	relationSelectTarget, parentSelectTarget, childSelectTarget, null);

				if (objList.size() > 0) {
					// 同じ親を持つ同名のオブジェクトが既に存在します。
					throw new EIMException("EIM.ERROR.LOGIC.OBJTYPE.NAME.DUPLICATE");
				}

				// 名前変更する時の重複チェックする際、名前のみでチェックするように変更する
				objectService.updateName(objectDomain, parentObject, relationTypeDomain, documentDomain.getName(), DuplicateCheckModeEnum.TYPE);
				//ドキュメント名称の重複チェックが正しく処理されない為、コメントアウト
				RelationCriteria relCriteria = new RelationCriteria();
				relCriteria.setChildObjectId(objectDomain.getId());
				relCriteria.setParentObjectId(parentObject.getId());
				relCriteria.setRelationTypeId(relationTypeDomain.getId());;

				List<RelationDomain> relDomainList = relationService.getList(relCriteria);

				if(relDomainList.size() <= 0){
					// 選択したドキュメントは存在しません。
					throw new EIMException("EIM.ERROR.LOGIC.NODOCUMENT");
				}

				relationService.delete(relDomainList.get(0));

				RelationDomain relationDomain = new RelationDomain();
				relationDomain.setType(relationTypeDomain);
				relationDomain.setParent(parentObject);
				relationDomain.setChild(objectDomain);

				RelationUtils.createRelation(relationDomain);
			}

			// 取得した実ファイルをファイルサーバ上に配置する
			long fileSize = FileUtils.upload(dir.getPath() + objectDomain.getId() + fileExt, inputStream);
			// チェックイン実行（DBに登録）
			FileUtils.checkin(objectDomain, formatDomain, documentDomain.getName(), fileSize);

			// Access History - 「原本差し替え」
			AccessUtils.createAccess(objectDomain, "EIM.ACCESS.TYPE.REPLACEORIGINALFILE");

		} catch (Exception e) {
			throw e;
		}

	}

	/**
	 * 公開ファイルを差し替えます。
	 *
	 * @param documentDomain ドキュメントドメイン
	 * @param inputStream    InputStream
	 * @throws Exception
	 */
	public void replacePublicFile(DocumentDomain documentDomain, InputStream inputStream) throws Exception {

		// Object
		ObjectDomain object = objectService.getById(documentDomain.getId());
		if (object == null) {
			throw new EIMException("EIM.ERROR.LOGIC.NODOCUMENTORFOLDER");
		}

		// Format
		FormatDomain format = formatService.getByDefinitionName(ConfigUtils.getByKey("FORMAT_NAME_PUBLIC"));
		if (format == null) {
			throw new EIMException("EIM.ERROR.LOGIC.FORMAT.NOTFOUND");
		}

		// Directory
		DirectoryDomain dir = format.getOnlineDirectory();

		// File
		FileDomain file = fileDao.getByObjectAndFormat(object, format);
		if (file == null) {
			throw new EIMException("EIM.ERROR.LOGIC.NODOCUMENT");
		}

		// ファイル存在チェック
		if (inputStream == null) {
			throw new EIMException("EIM.ERROR.LOGIC.NOUPLOADFILE");
		}

		// ファイルサイズ判定(最大ファイルサイズ)
		long maxFileSize = Long.parseLong(ConfigUtils.getByKey("UPLOAD_FILE_SIZE_MAX"));
		if (inputStream.available() > maxFileSize) {
			throw new EIMException("EIM.ERROR.LOGIC.UPLOAD.FILE.SIZE.OVER", maxFileSize);
		}

		// File Ext
		String ext = StringUtils.getFileExt(documentDomain.getName());
		if (ext == null) {
			ext = "";
		}

		// File Size
		long size = FileUtils.upload(dir.getPath() + object.getId() + ext, inputStream);

		// Checkin
		FileUtils.checkin(object, format, documentDomain.getName(), size);

		// PDF変換実行済の公開ファイルであればPDF変換実行日時を更新する
		// (オブジェクトの更新日時＞PDF変換処理実行日時　となってしまうため）
		AttributeDomain attrDomain = object.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PDF_CONV_EXEC_DATE"));
		if (attrDomain != null) {
			// オブジェクトの再取得
			object = objectService.getById(documentDomain.getId());
			AttributeTypeDomain attrType = attributeTypeService.getByDefinitionName(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PDF_CONV_EXEC_DATE"));

			// PDF変換処理実行日時＞オブジェクトの更新日時　としたいので、+5秒する。
			Calendar calendar = Calendar.getInstance();
			calendar.setTime(object.getModificationDate());
			calendar.add(Calendar.SECOND, 5);

			List<Date> dateList = new ArrayList<>();
			dateList.add(calendar.getTime());
			AppDocumentUtil.setAttributeDates(object, attrType, dateList);
		}

		// Access History - 「公開差し替え」
		AccessUtils.createAccess(object, "EIM.ACCESS.TYPE.REPLACEPUBLICFILE");

		return;

	}
	/**
	 * 指定したドキュメントをPlaceDomainの下にコピーします。
	 *
	 * @param documentDomain ドキュメントドメイン
	 * @param placeDomain ワークスペースドメイン又はフォルダドメイン
	 * @throws Exception
	 */
	/*
	public void copy(DocumentDomain documentDomain, PlaceDomain placeDomain) throws Exception {
		if (documentDomain == null) {
			// 引数folderDomainとしてnullが入力されました。
			throw new EIMException("EIM.ERROR.LOGIC.OBJECT.VALUE.ILLEGAL");
		}

		if (placeDomain == null) {
			// 引数parentDomainとしてnullが入力されました。
			throw new EIMException("EIM.ERROR.LOGIC.OBJECT.VALUE.ILLEGAL");
		}

		// オブジェクトの取得
		ObjectDomain objectDomain = objectService.getById(documentDomain.getId());
		if (objectDomain == null) {
			// オブジェクトが存在しない、エラー処理
			throw new EIMException("EIM.ERROR.LOGIC.NOTEXIST.DOCFOLWS");
		}

		// オブジェクトタイプの取得
		ObjectTypeDomain objectTypeDomain = objectDomain.getType();
		if (objectTypeDomain != null) {
			// オブジェクトタイプチェック
			if (!AppLogicUtil.isTypeOfDocument(objectTypeDomain)) {
				throw new EIMException("EIM.ERROR.LOGIC.NOTEXIST.DOCFOLWS");
			}
		}

		// 対象が最新のリビジョンかのチェック
		if (!objectDomain.isLatest()) {
			throw new EIMException("EIM.ERROR.LOGIC.NOCUTANDPASTEROLE");
		}

		// 親オブジェクト
		ObjectDomain parentObject = objectService.getById(placeDomain.getId());
		if (parentObject == null) {
			// 選択したフォルダは存在しません。
			throw new EIMException("EIM.ERROR.LOGIC.NOFOLDER");
		}

		// 移動先がゴミ箱か否かをチェック
		if (AppObjectUtil.isObjectInRecycle(parentObject)) {
			throw new EIMException("EIM.ERROR.LOGIC.SELECTFOLDERDELETED");
		}

		// 親オブジェクトのパス
		String path = AppObjectUtil.getPath(parentObject);
		if (path == null) {
			// ワークスペースの場合、パス属性の値を保持していない
			path = "/";
		}
		path += parentObject.getName() + "/";

		// 「ドキュメント」リレーションタイプ
		RelationTypeDomain relationTypeDomain = AppLogicUtil.getDocumentRelType();

		// Format
		FormatDomain formatPublic = formatService.getByDefinitionName(ConfigUtils.getByKey("FORMAT_NAME_PUBLIC"));
		FormatDomain formatSignEnc = null;
		if (getSignAndEncrFlg()) {
			formatSignEnc = formatService.getByDefinitionName(ConfigUtils.getByKey("FORMAT_NAME_SIGNENCR"));
		}
		FormatDomain format = formatService.getDefaultByObjectType(objectDomain.getType());

		// New Object Name
		String newObjName = objectDomain.getName();

		// コピー先のステータス種別ID
		long stsKind = parentObject.getStatus() != null ? parentObject.getStatus().getType().getBase().getId() : AppConstant.STATUS_TYPE_KIND_ID_NONE;
		// 上位WFフォルダのステータスのチェック
		if ((AppLogicUtil.isTypeOfFolderWithWorkflow(parentObject) || AppLogicUtil.isTypeOfFolderUnderFolderWithWorkflow(parentObject))
				&& stsKind != AppConstant.STATUS_TYPE_KIND_ID_EDITTING) { // 上位WFのステータスが「編集中」以外の場合はエラー
			// コピー/貼付け権限がありません。
			throw new EIMException("EIM.ERROR.LOGIC.NOCOPYNDPASTEROLE");
		}

		try {

			// 指定されたドキュメントオブジェクトの作成
			ObjectDomain newObj = objectService.create(objectDomain, DuplicateCheckModeEnum.NONE);

			while (true) {
				try {
					// 親オブジェクトとのリレーションを作成
					RelationDomain relationDomain = new RelationDomain();
					relationDomain.setType(relationTypeDomain);
					relationDomain.setParent(parentObject);
					relationDomain.setChild(newObj);
					//ドキュメント名称の重複チェックが正しく処理されない為、コメントアウト
					//relationService.create(relationDomain, DuplicateCheckModeEnum.INHERITTYPE);
					RelationUtils.createRelation(relationDomain);

				} catch (EIMException ecp) {
					int errCode = ecp.getResultCode();
					// Resource2.ERR_OBJECT_NAME_DUPLICATE
					if (errCode == 925) {
						TransactionContext tx = EIMThreadContext.getTransactionContext();
						String langId = tx.getLangId();
						newObjName = ConfigUtils.getByKey("COPY_FILENAME_PREFIX_" + langId) + " - " + newObjName;

						objectService.updateName(newObj, newObjName);
						continue;
					}
				}
				break;
			}

			// 親の所属ワークスペースを引き継ぐ
			AttributeTypeDomain typeWS = attributeTypeService.getByDefinitionName(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_BELONGING_WS"));
			if (typeWS != null) {
				if (parentObject.getType().getDefinitionName().equals(ConfigUtils.getByKey("OBJECT_TYPE_NAME_WORKSPACE"))) {
					objectService.setAttributeSingleLong(newObj, typeWS, parentObject.getId());
				} else {
					AttributeDomain parentWS = parentObject.getAttribute(typeWS.getDefinitionName());
					objectService.setAttributeSingleLong(newObj, typeWS, parentWS.getLong());
				}
			}

			// 属性情報の更新処理
			newObj = objectService.getById(newObj.getId());
			AttributeUtil.updateAttributeForCopy(newObj, parentObject, path);

			// 物理ファイルのコピー
			// Inherit Files without Public
			FileUtils.inheritFile(objectDomain, newObj);
			FileDomain newFilePublic = fileDao.getByObjectAndFormat(newObj, formatPublic);
			if (newFilePublic != null) {
				fileDao.delete(newObj, newFilePublic);
			}

			newObj = objectService.getById(newObj.getId());
			FileDomain file = fileDao.getByObjectAndFormat(newObj, format);

			// WFなしドキュメントの場合、原本ファイルをコピーして公開ファイルとして登録
			if (!AppLogicUtil.isTypeOfDocumentWithWorkflow(newObj)) {
				File orgFile = new File(file.getDirectory().getPath() + newObj.getId() + file.getExt());
				File dstFile = new File(formatPublic.getOnlineDirectory().getPath() + newObj.getId() + file.getExt());
				FileUtils.copyFile(orgFile, dstFile);
				FileUtils.checkin(newObj, formatPublic, file.getName(), file.getSize());
			}

			// 署名・暗号化ファイルは継承しない
			if (getSignAndEncrFlg()) {
				FileDomain signEncFile = fileDao.getByObjectAndFormat(newObj, formatSignEnc);
				if (signEncFile != null) {
					File substance = new File(signEncFile.getDirectory().getPath() + newObj.getId() + StringUtils.nullToBlank(signEncFile.getExt()));
					if (substance.exists()) {
						substance.delete();
					}
					fileDao.delete(newObj, signEncFile);
				}
			}

			// Rename File
			fileDao.updateName(newObj, file, newObjName);

		} catch (Exception e) {
			throw e;
		}

	}
	*/

	/**
	 * 署名・暗号化オプション。
	 *
	 * @return trueまたはfalse
	 * @throws Exception
	 */
	private boolean getSignAndEncrFlg() throws Exception {
		boolean signAndEncrFlg = false;

		String optionStr = ConfigUtils.getByKey("OPTION_ARRAY");

		String[] optList = optionStr.split(",");
		for (int i = 0; i < optList.length; i++) {
			if (optList[i].equals("signature_and_encryption")) {
				signAndEncrFlg = true;
				break;
			}
		}
		return signAndEncrFlg;
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
	 * @param objectService セットします objectService
	 */
	public void setObjectService(ObjectService objectService) {
		this.objectService = objectService;
	}

	/**
	 * @return objectService
	 */
	public ObjectService getObjectService() {
		return objectService;
	}

	/**
	 * @return webdavObjectService
	 */
	public ObjectService getWebdavObjectService() {
		return webdavObjectService;
	}

	/**
	 * @param webdavObjectService セットします webdavObjectService
	 */
	public void setWebdavObjectService(ObjectService webdavObjectService) {
		this.webdavObjectService = webdavObjectService;
	}


	/**
	 * @return objectDao
	 */
	public ObjectDao getObjectDao() {
		return objectDao;
	}

	/**
	 * @param objectDao セットします objectDao
	 */
	public void setObjectDao(ObjectDao objectDao) {
		this.objectDao = objectDao;
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
	 * @return formatService
	 */
	public FormatService getFormatService() {
		return formatService;
	}

	/**
	 * @param formatService セットします formatService
	 */
	public void setFormatService(FormatService formatService) {
		this.formatService = formatService;
	}

	/**
	 * @return fileDao
	 */
	public FileDao getFileDao() {
		return fileDao;
	}

	/**
	 * @param fileDao セットします fileDao
	 */
	public void setFileDao(FileDao fileDao) {
		this.fileDao = fileDao;
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
	 * @param includeGarbageBox セットする includeGarbageBox
	 */
	public void setIncludeGarbageBox(boolean includeGarbageBox) {
		this.includeGarbageBox = includeGarbageBox;
	}

	/**
	 * @return createFileZEROSize
	 */
	public boolean isCreateFileZEROSize() {
		return createFileZEROSize;
	}

	/**
	 * @param createFileZEROSize セットします createFileZEROSize
	 */
	public void setCreateFileZEROSize(boolean createFileZEROSize) {
		this.createFileZEROSize = createFileZEROSize;
	}

	/**
	 * @return includeFileSizeOfDocumentDomain
	 */
	public boolean isIncludeFileSizeOfDocumentDomain() {
		return includeFileSizeOfDocumentDomain;
	}

	/**
	 * @param includeFileSizeOfDocumentDomain セットする includeFileSizeOfDocumentDomain
	 */
	public void setIncludeFileSizeOfDocumentDomain(boolean includeFileSizeOfDocumentDomain) {
		this.includeFileSizeOfDocumentDomain = includeFileSizeOfDocumentDomain;
	}

	/**
	 * @return fileUploadRule
	 */
	public String getFileUploadRule() {
		return fileUploadRule;
	}

	/**
	 * @param fileUploadRule セットする fileUploadRule
	 */
	public void setFileUploadRule(String fileUploadRule) {
		this.fileUploadRule = fileUploadRule;
	}

	/**
	 * WSのドキュメントドメインをFWのオブジェクトドメインに変換します
	 *
	 * @param objectDomain
	 * @return
	 * @throws Exception
	 */
	private DocumentDomain getDocumentByObjectDomain(ObjectDomain objectDomain) throws Exception {
		DocumentDomain documentDomain = new DocumentDomain();
		BeanUtils.copyProperties(documentDomain, objectDomain);
		//作成者の設定
		UserDomain objectUser = objectDomain.getCreationUser();
		AttributeDomain createUserAttDoamin = objectDomain.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_CREATE"));
		if(objectUser != null &&  createUserAttDoamin != null){

			if(createUserAttDoamin.getLong() > 0 && (objectUser.getId() != createUserAttDoamin.getLong())){
				//userIDからUserDomainを取得
				UserDomain getUserDomain = userService.getById(createUserAttDoamin.getLong());
				documentDomain.setCreateUser(getUserDomain);
			}else{
				//objectDomainの作成者をDocumentDomainの作成者に設定
				documentDomain.setCreateUser(objectUser);
			}
		}
		//ファイルサイズの設定
		if(isIncludeFileSizeOfDocumentDomain()){
			FormatDomain format = null;
			if(objectDomain.getType().getDefaultFormat() == null){
				format = formatService.getDefaultByObjectType(objectDomain.getType());
			}else{
				format = objectDomain.getType().getDefaultFormat();
			}

			FileDomain fileDomain = fileDao.getByObjectAndFormat(objectDomain, format);
			if(fileDomain != null ){
				documentDomain.setSize(fileDomain.getSize());
			}
		}

		return documentDomain;
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
		case USER:
			initialAttribute = attributeTypeObject.getAttribute(ConfigUtils.getByKey("ATTRIBUTE_TYPE_NAME_INITIAL_USER"));
			if (initialAttribute != null && (objAttr == null || objAttr.getUserList().size() == 0)) {
				attr = new AttributeDomain();
				attr.setAttributeType(attributeType);
				TransactionContext tx = EIMThreadContext.getTransactionContext();
				if (tx != null) attr.setUser(tx.getUser());
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

	/**
	 * 「承認中のチェックイン許可」設定が有効な場合の権限チェックを実行する。
	 *
	 * @param objectDomain
	 * @throws Exception
	 */
	private void checkStatus(ObjectDomain objectDomain) throws Exception{
		// WF取得
		WorkflowDomain workflow = workflowService.getByObject(objectDomain);

		// WFがない場合、エラー
		if (workflow == null) {
			throw new EIMException("EIM.ERROR.LOGIC.WORKFLOW.NOT.APPLIED");
		} else {
			//ワークフロー設定オブジェクト取得
			ObjectTypeDomain workflowSettingObjType = objectTypeService.getByDefinitionName(ConfigUtils.getByKey("OBJECT_TYPE_NAME_WFSETTING"));
			ObjectDomain workflowSettingObj = objectService.getByTypeAndName(workflowSettingObjType, String.valueOf(workflow.getId()));

			// 「チェックイン可能ステータス」属性を取得し、現在のステータスがチェックインを許可しているかをチェック
			boolean enableCheckStatus = false;
			AttributeDomain enableCheckinStatusAttr = workflowSettingObj.getAttribute(ConfigUtils.getByKey("ATTR_NAME_ENABLE_CHECKIN_STATUS"));
			if (enableCheckinStatusAttr != null) {
				List<Long> enableCheckinStatusList = enableCheckinStatusAttr.getLongList();
				for (long enableCheckinStatus : enableCheckinStatusList) {
					if (objectDomain.getStatus().getType().getId() == enableCheckinStatus) {
						enableCheckStatus = true;
					}
				}
			}
			// 現在のステータスがチェックインを許可していない場合、エラー
			if (!enableCheckStatus) {
				throw new EIMException("EIM.ERROR.LOGIC.DISABLE.APPROVER.CHECKIN",new Object[]{objectDomain.getName(), ResourceUtils.getByKey("EIM.ACCESS.TYPE.EDIT")});
			}

			TransactionContext tx = EIMThreadContext.getTransactionContext();
			UserDomain loginUser = tx.getUser();

			// ログインユーザが現ステータスのエントリに入っているかチェック
			if (!AppWorkFlowUtil.isUserEntriedApproverCheck(objectDomain.getStatus().getId(), loginUser.getId())) {
				throw new EIMException("EIM.ERROR.LOGIC.CANNOT.CHECKIN.ONLY.APPROVER",new Object[]{objectDomain.getName(), ResourceUtils.getByKey("EIM.ACCESS.TYPE.EDIT")});
			}
		}
	}
}
