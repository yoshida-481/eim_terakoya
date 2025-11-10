package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.beanutils.BeanUtils;
import org.apache.commons.lang3.StringUtils;

import common.util.AppObjectUtil;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMRelationType;
import eim.bo.EIMSecurity;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.EIMConstant;
import eim.util.ObjectUtils;
import eim.util.RelationUtils;
import eim.util.SecurityUtils;
import jp.co.ctc_g.eim.app.document.business.domain.DocumentDomain;
import jp.co.ctc_g.eim.app.document.business.domain.FolderDomain;
import jp.co.ctc_g.eim.app.document.business.domain.WorkspaceDomain;
import jp.co.ctc_g.eim.app.document.business.domain.criteria.WorkspaceCriteria;
import jp.co.ctc_g.eim.app.document.business.domain.criteria.WorkspaceCriteria.WorkspaceItemEnum;
import jp.co.ctc_g.eim.app.document.business.service.WorkspaceService;
import jp.co.ctc_g.eim.app.document.common.util.AppDocumentLogicUtil;
import jp.co.ctc_g.eim.app.document.common.util.AppDocumentUtil;
import jp.co.ctc_g.eim.app.document.common.util.AttributeUtil;
import jp.co.ctc_g.eim.app.document.common.util.OperationHistoryUtils;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.ObjectCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.RelationCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AccessRoleTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.RelationDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.RelationTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.SecurityDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework2.business.service.ObjectService;
import jp.co.ctc_g.eim.framework2.business.service.ObjectTypeService;
import jp.co.ctc_g.eim.framework2.business.service.RelationService;
import jp.co.ctc_g.eim.framework2.business.service.RelationTypeService;
import jp.co.ctc_g.eim.framework2.business.service.SecurityService;
import jp.co.ctc_g.eim.framework2.business.service.UserService;
import jp.co.ctc_g.eim.framework2.common.enumeration.DuplicateCheckModeEnum;
import jp.co.ctc_g.eim.framework2.common.exception.EIMException;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;
import jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils;

/**
 * 【ドキュメントAPI】
 * @see WorkspaceService
 */
public class WorkspaceServiceImpl implements WorkspaceService {

	/** オブジェクトサービス */
	private ObjectService objectService = null;

	/** オブジェクトタイプサービス */
	private ObjectTypeService objectTypeService = null;

	/** リレーションサービス */
	private RelationService relationService = null;

	/** リレーションタイプサービス */
	private RelationTypeService relationTypeService = null;

	/** ユーザサービス */
	private UserService userService = null;

	/** セキュリティサービス */
	private SecurityService securityService = null;

	/** WorkSpaceDomainを取得する時に使う	*/
	private List<WorkspaceItemEnum> itemListForSingleTargetMethod = null;

	/**
	 * ワークスペースIDを指定してワークスペースを取得します。
	 *
	 * @param id ワークスペースID
	 * @return ワークスペースドメイン
	 * @throws Exception
	 */
	public WorkspaceDomain getById(long id) throws Exception {
		if (id < 1) {
			return null;
		}
		// オブジェクトの取得
		ObjectDomain objectDomain = objectService.getById(id);

		if (objectDomain == null) {
			return null;
			// オブジェクトが存在しない、エラー処理
			//throw new EIMException("EIM.ERROR.LOGIC.NOWSFOL");
		}

		// オブジェクトタイプチェック
		if (!AppDocumentLogicUtil.isTypeOfWorkspace(objectDomain.getType())) {
			return null;
			//throw new EIMException("EIM.ERROR.LOGIC.NOTEXIST.DOCFOLWS");
		}

		WorkspaceDomain workSpaceDomain = getWorkSpaceByObjectDomain(objectDomain);
		// 子階層フォルダ、ドキュメントのデータを取得
		workSpaceDomain = getFolderAndDocumentList(workSpaceDomain, objectDomain);

		return workSpaceDomain;
	}

	/**
	 * ワークスペース名称を指定してワークスペースを取得します。
	 *
	 * @param workspaceName ワークスペース名
	 * @return ワークスペースドメイン
	 * @throws Exception
	 */
	public WorkspaceDomain getByName(String workspaceName) throws Exception {
		if (StringUtils.isEmpty(workspaceName)) {
			throw new EIMException("EIM.ERROR.LOGIC.NAME.VALUE.ILLEGAL");
		}
		// 取得リスト
		List<ObjectDomain> resultList = new ArrayList<ObjectDomain>();
		WorkspaceDomain resultDomain = null;

		ObjectCriteria objectCriteria = new ObjectCriteria();
		objectCriteria.setName(workspaceName);
		objectCriteria.setSubClasses(false);
		// オブジェクトタイプ指定
		ObjectTypeDomain wsObjTypeDomain = AppDocumentLogicUtil.getWorkspaceObjectType();
		long objTypeId = wsObjTypeDomain.getId();
		objectCriteria.setObjectTypeId(objTypeId);

		resultList = objectService.getList(objectCriteria);
		if (resultList != null && resultList.size() > 0) {

			// WSのワークスペースドメインをFWのオブジェクトドメインに変換する
			resultDomain = getWorkSpaceByObjectDomain(resultList.get(0));

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
	 * ワークスペース名称とワークスペースIDを指定してワークスペースを取得します。
	 *
	 * @param criteria ワークスペースクライテリア
	 * @return ワークスペースドメインリスト
	 * @throws Exception
	 */
	public List<WorkspaceDomain> getList(WorkspaceCriteria criteria) throws Exception {
		if (criteria == null) {
			// 引数criteriaとしてnullが入力されました。
			throw new EIMException("EIM.ERROR.LOGIC.CRITERIA.VALUE.ILLEGAL");
		}

		// オブジェクトの検索条件を指定
		ObjectCriteria objectCriteria = new ObjectCriteria();
		// ドキュメント名称（部分一致）
		if(criteria.getName() != null){
			objectCriteria.setName(criteria.getName());
		}
		// ID（複数）指定
		if(criteria.getIds() != null){
			objectCriteria.setIds(criteria.getIds());
		}
		// オブジェクトタイプ指定criteria
		ObjectTypeDomain wsObjTypeDomain = AppDocumentLogicUtil.getWorkspaceObjectType();
		long objTypeId = wsObjTypeDomain.getId();
		objectCriteria.setObjectTypeId(objTypeId);
		objectCriteria.setSubClasses(false);
		//権限チェック
		objectCriteria.setAccessRoleType(new AccessRoleTypeDomain("READ"));

		//最大取得件数 何も設定しない場合は無制限
		if(criteria.getLimit() == null){
			objectCriteria.setLimit(Integer.MAX_VALUE);
		}else{
			objectCriteria.setLimit(criteria.getLimit());
		}

		//最大取得件数を超えて取得した場合の設定をしない場合はデフォルトfalse
		if(criteria.isLimitCondition() == null){
			objectCriteria.setLimitCondition(false);
		}else{
			objectCriteria.setLimitCondition(criteria.isLimitCondition());
		}

		List<ObjectDomain> objectList = objectService.getList(objectCriteria);

		if (objectList != null && objectList.size() == 0) {
			return null;
		}
		// 取得リスト
		List<WorkspaceDomain> resultList = new ArrayList<WorkspaceDomain>();

		// WSのワークスペースドメインをFWのオブジェクトドメインに変換する
		for (ObjectDomain objectDomain : objectList) {
			WorkspaceDomain workSpaceDomain = getWorkSpaceByObjectDomain(objectDomain);
			resultList.add(workSpaceDomain);
		}

		return resultList;
	}

	/**
	 * ワークスペースを登録します。
	 *
	 * @param workspaceDomain ワークスペースドメイン
	 * @return ワークスペースドメイン
	 * @throws Exception
	 */
	public WorkspaceDomain create(WorkspaceDomain workspaceDomain) throws Exception {
		if (workspaceDomain == null) {
			// 引数workspaceDomainとしてnullが入力されました。
			throw new EIMException("EIM.ERROR.LOGIC.OBJECT.VALUE.ILLEGAL");
		}
		// Windows禁止文字チェック
		AppDocumentUtil.checkValidateFName(workspaceDomain.getName());

		// セキュリティチェック
		SecurityDomain securityDomain = workspaceDomain.getSecurity();
		if (securityDomain == null || (securityDomain.getId() == 0 && StringUtils.isEmpty(securityDomain.getDefinitionName()))) {
			// 指定されたセキュリティが存在しません。
			throw new EIMException("EIM.ERROR.LOGIC.SEC.NOTFOUND");
		}

		// ワークスペースタイプチェック
		ObjectTypeDomain objType = workspaceDomain.getType();
		// 「フォルダ」オブジェクトタイプ名称の取得
		if (objType != null) {
			if (!AppDocumentLogicUtil.isTypeOfWorkspace(objType)) {
				throw new EIMException("EIM.ERROR.LOGIC.NOWORKSPACETYPE");
			}
		} else {
			objType = AppDocumentLogicUtil.getWorkspaceObjectType();
			workspaceDomain.setType(objType);
		}

		ObjectDomain objectDomain = new ObjectDomain();
		WorkspaceDomain workSpace = new WorkspaceDomain();

		try {
			BeanUtils.copyProperties(objectDomain, workspaceDomain);

			//ワークスペース初期属性の除去
			List<AttributeDomain> attributeList = AppDocumentLogicUtil.removeInitAttributeForWorkspace(objectDomain.getAttributeList());
			objectDomain.setAttributeList(attributeList);
			//ワークスペース初期属性の設定
			List<AttributeDomain> initAttList = AppDocumentLogicUtil.getInitAttributeForWorkspace();
			objectDomain.getAttributeList().addAll(initAttList);

			// オブジェクト作成
			objectDomain = objectService.create(objectDomain, DuplicateCheckModeEnum.TYPE);

			/*----------------------------------------
			// ワークスペース固有ごみ箱の作成		 /
			----------------------------------------*/
			EIMSession sess = jp.co.ctc_g.eim.framework.common.util.EIMThreadContext.getEIMSession();

			// ワークスペース固有ごみ箱オブジェクトタイプ取得
			EIMObjectType wsRecycleObjType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_WORKSPACERECYCLE"));

			// ワークスペース固有ごみ箱オブジェクト作成
			EIMObject wsRecycleObject = ObjectUtils.createObject(jp.co.ctc_g.eim.framework.common.util.EIMThreadContext.getEIMSession(), wsRecycleObjType, "ごみ箱");

			// リレーションタイプ「ごみ箱」取得
			EIMRelationType relType = RelationUtils.getRelationTypeByName(sess, "ごみ箱");
			// ワークスペースとのリレーション作成
			RelationUtils.createRelation(sess, relType, ConvertUtils.toEIMObject(objectDomain), wsRecycleObject, EIMConstant.DEPU_CHECK_AVAILABLE);

			// ワークスペース固有ごみ箱のパス
			String path = "/" + objectDomain.getName() + "/";
			// パスの設定
			AppObjectUtil.setPath(sess, wsRecycleObject, path);

			// ワークスペース固有ごみ箱セキュリティ取得
			EIMSecurity wsRecycleSec = SecurityUtils.getSecurityByName(sess, EIMConfig.get("SECURITY_NAME_WORKSPACE_RECYCLE"));
			if(wsRecycleSec != null) {
				SecurityUtils.setSecurity(sess, wsRecycleObject, wsRecycleSec);
			}

			// WSのワークスペースドメインをFWのオブジェクトドメインに変換する
			//objectDomain = objectService.getById(objectDomain.getId());
			workSpace = getWorkSpaceByObjectDomain(objectDomain);

			//操作履歴 AOP利用
			//OperationHistoryUtils.createOperationHistory(objectDomain, 2, 2006, 1, 6, objectDomain.getId(), objectDomain.getName(),
			//											-1, -1, -1, null, null);

		} catch (Exception e) {
			throw e;
		}
		return workSpace;
	}

	/**
	 * ワークスペースの名称と属性、セキュリティを更新します。
	 *
	 * @param workspaceDomain ワークスペースドメイン
	 * @throws Exception
	 */
	public void update(WorkspaceDomain workspaceDomain) throws Exception {
		if (workspaceDomain == null) {
			// 引数workspaceDomainとしてnullが入力されました。
			throw new EIMException("EIM.ERROR.LOGIC.OBJECT.VALUE.ILLEGAL");
		}
		// オブジェクトの取得
		ObjectDomain objectDomain = objectService.getById(workspaceDomain.getId());
		if (objectDomain == null) {
			// オブジェクトが存在しない、エラー処理
			throw new EIMException("EIM.ERROR.LOGIC.NOTEXIST.DOCFOLWS");
		}

		boolean isChangeName = false;
		if (!StringUtils.isEmpty(workspaceDomain.getName()) && !workspaceDomain.getName().equals(objectDomain.getName())) {
			isChangeName = true;
		}

		// オブジェクトタイプの取得
		ObjectTypeDomain objectTypeDomain = objectDomain.getType();
		if (objectTypeDomain != null) {
			// オブジェクトタイプチェック
			if (!AppDocumentLogicUtil.isTypeOfWorkspace(objectTypeDomain)) {
				throw new EIMException("EIM.ERROR.LOGIC.NOTEXIST.DOCFOLWS");
			}
		}

		// 「ドキュメント」リレーションタイプ
		RelationTypeDomain relType = AppDocumentLogicUtil.getDocumentRelType();

		try {
			// オブジェクト名が変更された場合
			if (isChangeName) {
				objectDomain.setName(workspaceDomain.getName());
				// 改名処理
				AppDocumentLogicUtil.renameObjectForWorkSpace(objectDomain, relType);

				// ワークスペース固有ごみ箱のパスの更新
				String path = "/" + objectDomain.getName() + "/";
				RelationCriteria criteria = new RelationCriteria();
				criteria.setParentObjectId(objectDomain.getId());
				criteria.setRelationTypeId(AppDocumentLogicUtil.getRecycleRelType().getId());
				List<RelationDomain> wsRecycleRelList = relationService.getList(criteria);
				if(wsRecycleRelList == null || wsRecycleRelList.size() == 0){
					throw new EIMException("EIM.ERROR.LOGIC.NOWSRECYCLEBOX");
				}
				ObjectDomain wsRecycleObject = wsRecycleRelList.get(0).getChild();
				AppObjectUtil.setPath(jp.co.ctc_g.eim.framework.common.util.EIMThreadContext.getEIMSession(), ConvertUtils.toEIMObject(wsRecycleObject), path);

				//操作履歴
				OperationHistoryUtils.createOperationHistory(objectDomain, 1, 2008, 1, 6, objectDomain.getId(), objectDomain.getName(),
															-1, -1, -1, null, null);
			}

			SecurityDomain security = objectDomain.getSecurity();
			SecurityDomain newSecurity = workspaceDomain.getSecurity();
			if ((security == null && newSecurity != null) || (security != null && newSecurity == null)
					|| (security != null && newSecurity != null && security.getId() != newSecurity.getId())) {
				objectDomain.setSecurity(workspaceDomain.getSecurity());
				AppDocumentLogicUtil.applySecurity(relType, objectDomain, newSecurity);
			}

			// 更新対象属性をworkspaceDomain(引数値)から取得する
			List<AttributeDomain> updateAttributeList = AppDocumentLogicUtil.removeInitAttributeForWorkspace(workspaceDomain.getAttributeList());
			// objectDoman(DB値)から更新対象外の属性を取得する
			List<AttributeDomain> prohibitionUpdateAttributeList = AppDocumentLogicUtil.getInitAttributeForWorkspace(objectDomain.getAttributeList());

			List<AttributeDomain> margeAttributeList = AppDocumentLogicUtil.addAttributeDomain(updateAttributeList,prohibitionUpdateAttributeList);

			// 属性設定
			objectDomain.setAttributeList(margeAttributeList);

			// 属性情報の更新
			AttributeUtil.updateAttribute(objectDomain, relType);

			//操作履歴 AOP利用
			//OperationHistoryUtils.createOperationHistory(objectDomain, 1, 2008, 3, 12, objectDomain.getId(), objectDomain.getName(),
			//											-1, -1, -1, null, null);

		} catch (Exception e) {
			throw e;
		}
	}

	/**
	 * ワークスペースを削除します。ワークスペース名称もしくはオブジェクトIDを指定します。
	 *
	 * @param worksapceDomainList ワークスペースドメインリスト
	 * @throws Exception
	 */
	public void delete(List<WorkspaceDomain> worksapceDomainList) throws Exception {
		if (worksapceDomainList == null) {
			// 引数worksapceDomainListとしてnullが入力されました。
			throw new EIMException("EIM.ERROR.LOGIC.OBJECT.VALUE.ILLEGAL");
		}
		// 「ドキュメント」リレーションタイプ名称の取得
		RelationTypeDomain relationTypeDomain = AppDocumentLogicUtil.getDocumentRelType();

		// 「ごみ箱」リレーションタイプの取得
		RelationTypeDomain recycleRelationTypeDomain = AppDocumentLogicUtil.getRecycleRelType();

		// 「ワークスペース」オブジェクトタイプ名称の取得
		ObjectTypeDomain wsObjType = AppDocumentLogicUtil.getWorkspaceObjectType();

		// ワークスペース固有ごみ箱オブジェクトタイプ取得
		ObjectTypeDomain wsRecycleObjType = AppDocumentLogicUtil.getWsRecycleObjectType();

		for (int i = 0; i < worksapceDomainList.size(); i++) {

			// オブジェクトの取得
			ObjectDomain objectDomain = objectService.getById(worksapceDomainList.get(i).getId());

			if (objectDomain == null) {
				// オブジェクトが存在しない、エラー処理
				throw new EIMException("EIM.ERROR.LOGIC.NOWSFOL");
			}

			// オブジェクトタイプチェック
			if (wsObjType.getId() != objectDomain.getType().getId()) {
				throw new EIMException("EIM.ERROR.LOGIC.NOTEXIST.DOCFOLWS");
			}

			// 下位オブジェクトを取得する
			List<ObjectDomain> objList = AppDocumentUtil.getChildObjectByRelTypeAndParent(relationTypeDomain, objectDomain, null);
			if (objList.size() > 0) {
				throw new EIMException("EIM.ERROR.LOGIC.EXISTDATA", objectDomain.getName());
			}

			// ワークスペース固有ごみ箱オブジェクトの取得
			RelationCriteria criteria = new RelationCriteria();
			criteria.setParentObjectId(worksapceDomainList.get(i).getId());
			criteria.setRelationTypeId(recycleRelationTypeDomain.getId());
			List<RelationDomain> wsRecycleRelList = relationService.getList(criteria);
			if(wsRecycleRelList == null || wsRecycleRelList.size() == 0){
				throw new EIMException("EIM.ERROR.LOGIC.NOWSRECYCLEBOX");
			}
			ObjectDomain recycleObject = wsRecycleRelList.get(0).getChild();

			// ワークスペース固有ごみ箱配下の子オブジェクト取得
			List<ObjectDomain> objList2 = AppDocumentUtil.getChildObjectByRelTypeAndParent(relationTypeDomain, recycleObject, null);

			// ワークスペース固有ごみ箱内にデータが存在する場合
			if(objList2.size() > 0){
				throw new EIMException("EIM.ERROR.LOGIC.EXISTDATA", objectDomain.getName());
			}

			try {
				// オブジェクトを削除する
				objectService.delete(objectDomain);

				//操作履歴 AOP利用
				//OperationHistoryUtils.createOperationHistory(objectDomain, 1, 2008, 3, 12, objectDomain.getId(), objectDomain.getName(),
				//												-1, -1, -1, null, null);
			} catch (Exception e) {
				throw e;
			}
		}
	}

	/**
	 * WSのワークスペースドメインをFWのオブジェクトドメインに変換します
	 *
	 * @param objectDomain
	 * @return ワークスペースドメイン
	 * @throws Exception
	 */
	private WorkspaceDomain getWorkSpaceByObjectDomain(ObjectDomain objectDomain) throws Exception {
		WorkspaceDomain workspaceDomain = new WorkspaceDomain();
		BeanUtils.copyProperties(workspaceDomain, objectDomain);

		return workspaceDomain;
	}

	/**
	 * WSのフォルダドメインをFWのオブジェクトドメインに変換します
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
	 * WSのドキュメントドメインをFWのオブジェクトドメインに変換します
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
	 * 子階層フォルダ、ドキュメントのデータを取得します
	 *
	 * @param workspaceDomain
	 * @param objectDomain
	 * @return
	 * @throws Exception
	 */
	private WorkspaceDomain getFolderAndDocumentList(WorkspaceDomain workspaceDomain, ObjectDomain objectDomain) throws Exception {
		WorkspaceDomain result = workspaceDomain;

		if (itemListForSingleTargetMethod == null || itemListForSingleTargetMethod.contains(WorkspaceItemEnum.FOLDER_LIST)
				|| itemListForSingleTargetMethod.contains(WorkspaceItemEnum.DOCUMENT_LIST)) {

			// 「ドキュメント」リレーションタイプ
			RelationTypeDomain relationTypeDomain = AppDocumentLogicUtil.getDocumentRelType();
			List<ObjectDomain> objList = AppDocumentUtil.getChildObjectByRelTypeAndParent(relationTypeDomain, objectDomain,new AccessRoleTypeDomain("READ"));

			List<FolderDomain> folderObjList =  new ArrayList<FolderDomain>();
			List<DocumentDomain> documentObjList =  new ArrayList<DocumentDomain>();
			if (objList != null && objList.size() > 0) {
				ObjectTypeDomain folderObjType = AppDocumentLogicUtil.getFolderObjectType();
				List<Long> folderObjTypeIds = AppDocumentLogicUtil.getChildObjectTypeList(folderObjType.getId());

				ObjectTypeDomain docObjType = AppDocumentLogicUtil.getDocumentObjectType();
				List<Long> docObjTypeIds = AppDocumentLogicUtil.getChildObjectTypeList(docObjType.getId());

				for (ObjectDomain object : objList) {
					// 取得したオブジェクトを「ドキュメント」と「フォルダ」に分ける
					if (itemListForSingleTargetMethod == null || itemListForSingleTargetMethod.contains(WorkspaceItemEnum.FOLDER_LIST)) {
						if (folderObjTypeIds.contains(object.getType().getId())) {

							FolderDomain folderDomain = getFolderByObjectDomain(object);
							folderObjList.add(folderDomain);
							continue;
						}
					}
					if (itemListForSingleTargetMethod == null || itemListForSingleTargetMethod.contains(WorkspaceItemEnum.DOCUMENT_LIST)) {
						if (docObjTypeIds.contains(object.getType().getId())) {

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
	 * @param securityService セットします securityService
	 */
	public void setSecurityService(SecurityService securityService) {
		this.securityService = securityService;
	}

	/**
	 * @return securityService
	 */
	public SecurityService getSecrityService() {
		return securityService;
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

}
