package jp.co.ctc_g.eim.app.document.common.util;

import java.io.File;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.springframework.context.ApplicationContext;

import common.util.AppConstant;
import common.util.AppLogicUtil;
import common.util.AppObjectUtil;
import common.util.AppSqlUtil;
import common.util.AppUpdateNoticeUtils;
import eim.bo.EIMObject;
import eim.bo.EIMRelation;
import eim.bo.EIMSearchConditionIn;
import eim.bo.EIMSearchLimitCountCondition;
import eim.bo.EIMSearchResultList;
import eim.bo.EIMSearchSelectEIMObject;
import eim.bo.EIMSearchSelectEIMObject.SearchConditionBuildHelper;
import eim.net.EIMSession;
import eim.util.EIMConstant;
import eim.util.SearchUtils;
import jp.co.ctc_g.eim.app.document.business.domain.DocumentDomain;
import jp.co.ctc_g.eim.app.document.business.service.DocumentService;
import jp.co.ctc_g.eim.framework2.business.dao.FileDao;
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
import jp.co.ctc_g.eim.framework2.business.service.AttributeTypeService;
import jp.co.ctc_g.eim.framework2.business.service.FormatService;
import jp.co.ctc_g.eim.framework2.business.service.ObjectService;
import jp.co.ctc_g.eim.framework2.business.service.ObjectTypeService;
import jp.co.ctc_g.eim.framework2.business.service.RelationService;
import jp.co.ctc_g.eim.framework2.business.service.RelationTypeService;
import jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.common.context.TransactionContext;
import jp.co.ctc_g.eim.framework2.common.enumeration.DuplicateCheckModeEnum;
import jp.co.ctc_g.eim.framework2.common.exception.EIMException;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;
import jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils;
import jp.co.ctc_g.eim.framework2.integration.dao.plugin.DatabasePlugIn.RecursiveTableEnum;
import jp.co.ctc_g.eim.framework2.integration.dao.plugin.DatabasePlugInLoader;
import jp.co.ctc_g.eim.framework2.integration.util.internal.FileUtil;

/**
 * 【ドキュメントAPI】
 */
public class AppDocumentLogicUtil {

	private static ObjectTypeDomain _workspaceObjType;
	private static ObjectTypeDomain _folderObjType;
	private static ObjectTypeDomain _documentObjType;
	private static ObjectTypeDomain _recycleObjType;
	private static ObjectTypeDomain _wsRecycleObjType;
	private static RelationTypeDomain _documentRelType;
	private static RelationTypeDomain _recycleRelType;
	private static AttributeTypeDomain _docPathAttributeType;

	/**
	 * 指定オブジェクトタイプがフォルダ系タイプかどうかを返します。
	 *
	 * @param objectTypeDomain オブジェクトタイプ
	 * @return フォルダ系タイプならtrue
	 * @throws Exception
	 */
	public static boolean isTypeOfFolder(ObjectTypeDomain objectTypeDomain) throws Exception {

		boolean isTypeOfFolder = false;
		// オブジェクトタイプチェック
		if (objectTypeDomain != null && getChildObjectTypeList(getFolderObjectType().getId()).contains(objectTypeDomain.getId())) {
			isTypeOfFolder = true;
		}
		return isTypeOfFolder;
	}

	/**
	 * 指定オブジェクトタイプがドキュメント系タイプかどうかを返します。
	 *
	 * @param objectTypeDomain オブジェクトタイプ
	 * @return ドキュメント系タイプならtrue
	 * @throws Exception
	 */
	public static boolean isTypeOfDocument(ObjectTypeDomain objectTypeDomain) throws Exception {

		boolean isTypeOfDocument = false;
		// オブジェクトタイプチェック
		if (objectTypeDomain != null && getChildObjectTypeList(getDocumentObjectType().getId()).contains(objectTypeDomain.getId())) {
			isTypeOfDocument = true;
		}
		return isTypeOfDocument;
	}

	/**
	 * 指定オブジェクトタイプがワークスペースタイプかどうかを返します。
	 *
	 * @param objectTypeDomain オブジェクトタイプ
	 * @return ワークスペースタイプならtrue
	 * @throws Exception
	 */
	public static boolean isTypeOfWorkspace(ObjectTypeDomain objectTypeDomain) throws Exception {

		boolean isTypeOfWorkspace = false;
		// オブジェクトタイプチェック
		if (objectTypeDomain != null && objectTypeDomain.getId() == getWorkspaceObjectType().getId()) {
			isTypeOfWorkspace = true;
		}
		return isTypeOfWorkspace;
	}

	/**
	 * ワークフロー付きフォルダかどうかを返します。
	 *
	 * @param obj オブジェクト
	 * @return ワークフロー付きフォルダならtrue
	 * @throws Exception
	 */
	public static boolean isTypeOfFolderWithWorkflow(ObjectDomain obj) throws Exception {
		// ワークフロー付きフォルダとは、
		// 1)フォルダタイプで
		// 2)ワークフローがあり
		// 3)「上位ワークフローフォルダー」属性が無い
		// もの
		return (isTypeOfFolder(obj.getType()) //
				&& (obj.getStatus() != null)//
		&& (obj.getAttribute(ConfigUtils.getByKey("ATTR_NAME_FOLDER_HIGHER_WF_FOLDER")) == null)//
		);
	}

	/**
	 * ワークフロー付きフォルダの下のフォルダかどうかを返します。
	 *
	 * @param obj オブジェクト
	 * @return ワークフロー付きフォルダの下のフォルダならtrue
	 * @throws Exception
	 */
	public static boolean isTypeOfFolderUnderFolderWithWorkflow(ObjectDomain obj) throws Exception {
		return (isTypeOfFolder(obj.getType()) //
				&& (obj.getStatus() != null)//
		&& (obj.getAttribute(ConfigUtils.getByKey("ATTR_NAME_FOLDER_HIGHER_WF_FOLDER")) != null)//
		);
	}

	/**
	 * ワークフロー付きドキュメントかどうかを返します。
	 *
	 * @param obj オブジェクト
	 * @return ワークフロー付きドキュメントならtrue
	 * @throws Exception
	 */
	public static boolean isTypeOfDocumentWithWorkflow(ObjectDomain obj) throws Exception {
		// ワークフロー付きフォルダとは、
		// 1)ドキュメントタイプで
		// 2)ワークフローがあり
		// 3)「上位ワークフローフォルダー」属性が無い
		// もの
		return (isTypeOfDocument(obj.getType()) //
				&& (obj.getStatus() != null)//
		&& (obj.getAttribute(ConfigUtils.getByKey("ATTR_NAME_FOLDER_HIGHER_WF_FOLDER")) == null)//
		);
	}

	/**
	 * 指定オブジェクトタイプ系タイプのリストを返します。
	 * @param objTypeId
	 * @return
	 * @throws Exception
	 */
	public static List<Long> getChildObjectTypeList(Long objTypeId) throws Exception {

		List<Long> objTypeIds = new ArrayList<Long>();

		if(objTypeId < 0){
			return objTypeIds;
		}

		// Transaction
		TransactionContext tx = EIMThreadContext.getTransactionContext();
		// Connection
		Connection conn = tx.getDBConnection();
		// Statement
		CallableStatement cstmt = null;
		ResultSet rset = null;

		try{
			String[] columns = {"id", "parent"};
			String startCondition = String.format("id = %s", objTypeId);

			// SQL
			String sql = DatabasePlugInLoader.getPlugIn()
							.getQueryStringWithRecursive(RecursiveTableEnum.EIMOBJTYPE_CHILDREN, columns, startCondition, false) +
						DatabasePlugInLoader.getPlugIn()
							.getQueryStringSelectRecursive(RecursiveTableEnum.EIMOBJTYPE_CHILDREN, columns, startCondition, false) +
						" order by id";

			// Prepare
			cstmt = conn.prepareCall(sql);

			// Execute
			rset = cstmt.executeQuery();

			while (rset.next()) {
				objTypeIds.add(rset.getLong("id"));
			}

		}finally{
			if(rset != null){
				// Close ResultSet
				rset.close();
			}
			if(cstmt != null){
				// Close Statement
				cstmt.close();
			}
		}

		return objTypeIds;
	}

	/**
	 * 有効期限判定<BR>
	 * [備考]<BR>
	 * 有効期限切れ判定をします。
	 *
	 * @param date DBより取得した有効期限(GMT標準値)
	 * @return judgeFlg false:有効／true:期限切れ
	 * @throws Exception
	 */
	public static boolean judgeExpirationDate(Date date) throws Exception {

		boolean judgeFlg = false;
		/*
		 * 有効期限切れ判定
		 */
		Date nowDate = new Date();

		// 有効期限切れの場合
		if (!nowDate.before(date)) {
			judgeFlg = true;
		}

		return judgeFlg;
	}

	/**
	 * ドキュメントもしくはフォルダオブジェクトのパス属性(複数値)の先頭を入れ替えます。
	 * ※ ドキュメントの全バージョンについて行います。
	 *
	 * @param object ドキュメントもしくはフォルダオブジェクト
	 * @param path 設定するパス
	 * @throws Exception
	 */
	public static void replaceFirstPath(ObjectDomain object, String path) throws Exception {

		ApplicationContext context = ApplicationContextLoader.getApplicationContext();
		ObjectService objectService = (ObjectService) context.getBean("objectService4");

		// バージョンを取得
		Map<Long, ObjectDomain> revObjectMap = VersionUtils.getVersion(object);
		AttributeTypeDomain attributeType = AppDocumentLogicUtil.getDocPathAttributeType();
		// バージョン分繰り返す
		for (ObjectDomain repObj : revObjectMap.values()) {

			List<String> values = null;
			AttributeDomain attributeDomain = repObj.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PASS"));
			if (attributeDomain != null) {
				values = attributeDomain.getStringList();
			}

			if (values == null || values.size() == 0) {
				if (!path.equals("")) {
					values = new ArrayList<String>();
					values.add(path);
					AppDocumentUtil.setAttributeStrings(repObj, attributeType, values);
				}
			} else {
				if (path.equals("")) {
					objectService.removeAttribute(repObj, attributeType);
				} else {
					// パス属性の先頭のみ置き換える
					values.set(0, path);

					AppDocumentUtil.setAttributeStrings(repObj, attributeType, values);
				}
			}
		}
	}

	/**
	 * 指定ドキュメントに削除日時を設定します。
	 * ※全リビジョンに設定します。
	 *
	 * @param object ドキュメント
	 * @param dateList 日付
	 * @throws Exception
	 */
	public static void setAttrDeleteDate(ObjectDomain object, List<Date> dateList) throws Exception {

		ApplicationContext context = ApplicationContextLoader.getApplicationContext();
		AttributeTypeService attributeTypeService = (AttributeTypeService) context.getBean("attributeTypeService2");

		// バージョンを取得
		Map<Long, ObjectDomain> revObjectMap = VersionUtils.getVersion(object);
		AttributeTypeDomain attributeType = attributeTypeService.getByDefinitionName(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_DELETE_DATE"));
		// バージョン分繰り返す
		for (ObjectDomain repObj : revObjectMap.values()) {
			AppDocumentUtil.setAttributeDates(repObj, attributeType, dateList);
		}
	}

	/**
	 * 引数オブジェクトがワークフロー付きフォルダの下に居るかどうかを返します。
	 *
	 * @param obj
	 * @return 引数オブジェクトがワークフロー付きフォルダの下に居るならtrue
	 * @throws Exception
	 */
	public static boolean isUnderFolderWithWorkflow(ObjectDomain obj) throws Exception {
		// 上位WFフォルダ属性の属性名 -フォルダ/ドキュメント
		return (obj.getAttribute(ConfigUtils.getByKey("ATTR_NAME_FOLDER_HIGHER_WF_FOLDER")) != null);
	}

	/**
	 * 指定オブジェクトを起点にフォルダツリーをたどり、全オブジェクトを処理ウォーカーで処理します。<br>
	 *
	 * @param relType
	 * @param object 起点となるオブジェクト
	 * @throws Exception
	 */
	public static void processFolderTree(RelationTypeDomain relType, ObjectDomain object) throws Exception {

		Set<Long> alreadyProcessedObjSet = new HashSet<Long>();

		ObjectTypeDomain folderObjType = AppDocumentLogicUtil.getFolderObjectType();
		List<Long> folderObjTypeIds = AppDocumentLogicUtil.getChildObjectTypeList(folderObjType.getId());

		ObjectTypeDomain docObjType = AppDocumentLogicUtil.getDocumentObjectType();
		List<Long> docObjTypeIds = AppDocumentLogicUtil.getChildObjectTypeList(docObjType.getId());
		processFolderTreeInternal(relType, object, alreadyProcessedObjSet, folderObjTypeIds, docObjTypeIds);

		if (isTypeOfFolder(object.getType())) {

			// ドキュメントリンクを削除
			deleteDocumentLink(object);

			// フォルダ削除
			AppDocumentUtil.deleteObjectAll(object);
		}

	}

	/**
	 * 指定オブジェクトを起点にフォルダツリーをたどり、全オブジェクトを削除します。<br>
	 *
	 * @param relType
	 * @param object 起点となるオブジェクト
	 * @param alreadyProcessedObjSet 既に処理したオブジェクトのマップ
	 * @param folderObjTypeIds
	 * @param docObjTypeIds
	 * @throws Exception
	 */
	private static void processFolderTreeInternal(RelationTypeDomain relType, ObjectDomain object, Set<Long> alreadyProcessedObjSet,
			List<Long> folderObjTypeIds, List<Long> docObjTypeIds) throws Exception {
		if (object == null || alreadyProcessedObjSet.contains(object.getId()))
			return;
		// 削除可能か否かを判定
		AttributeUtil.checkDeleteEnable(object);
		ApplicationContext context = ApplicationContextLoader.getApplicationContext();
		DocumentService documentService = (DocumentService) context.getBean("documentService2");

		if (folderObjTypeIds.contains(object.getType().getId())) {
			// フォルダかワークスペース
			// 下位を取得し、それぞれを再起呼び出し
			List<ObjectDomain> childList = AppDocumentUtil.getChildObjectByRelTypeAndParent(relType, object, null);
			if (childList != null) {
				for (ObjectDomain child : childList) {
					processFolderTreeInternal(relType, child, alreadyProcessedObjSet, folderObjTypeIds, docObjTypeIds);
					if (folderObjTypeIds.contains(child.getType().getId())) {
						// ドキュメントリンクを削除
						deleteDocumentLink(child);
						AppDocumentUtil.deleteObjectAll(child);
					}
				}
			}
			return;
		} else if (docObjTypeIds.contains(object.getType().getId())) {
			// ドキュメント
			List<DocumentDomain> documentDomainList = new ArrayList<DocumentDomain>();
			DocumentDomain doc = new DocumentDomain();
			doc.setId(object.getId());
			documentDomainList.add(doc);
			documentService.delete(documentDomainList);
		} else {
			AppDocumentUtil.deleteObjectAll(object);
		}
		alreadyProcessedObjSet.add(object.getId());
	}

	/**
	 * ドキュメントリンクリレーションを削除します。<br>
	 * リンクリレーション削除とともに、リンク元オブジェクトの”パス”/”リンク先”属性から削除するリンクの情報をを除去します。
	 *
	 * @param parentFolderObject 親フォルダオブジェクト
	 * @throws Exception
	 */
	private static void deleteDocumentLink(ObjectDomain parentFolderObject) throws Exception {

		//フォルダに紐づくドキュメントリンクを削除します (削除または更新されている場合があるためDBから再取得)
		ApplicationContext context = ApplicationContextLoader.getApplicationContext();
		RelationTypeService relationTypeService = (RelationTypeService) context.getBean("relationTypeService2");

		RelationTypeDomain linRelationType = relationTypeService.getByDefinitionName(ConfigUtils.getByKey("RELATION_TYPE_NAME_LINK"));

		
		AppDocumentLogicUtil.getDocumentRelType();
		List<RelationDomain> childRelList = AppDocumentUtil.getChildRelationListByRelType(linRelationType, parentFolderObject, null);

		for (int i = 0; i < childRelList.size(); i++) {

			RelationDomain relation = (RelationDomain) childRelList.get(i);
			EIMObject parentFolderObj = ConvertUtils.toEIMObject(parentFolderObject);
			EIMObject childObj = ConvertUtils.toEIMObject(relation.getChild());
			EIMRelation rel = new EIMRelation(relation.getId(), ConvertUtils.toEIMRelationType(relation.getType()), parentFolderObj, childObj);
			EIMSession sess = ConvertUtils.toEIMSession(EIMThreadContext.getTransactionContext());
			AppLogicUtil.deleteDocLinkPath(sess, childObj, parentFolderObj.getId() );
			eim.util.RelationUtils.deleteRelation(sess, rel);

			// SearchFramework 更新通知 対象：配下ドキュメントリンク
			AppUpdateNoticeUtils.updateNoticeInsert(relation.getChild().getId(), "SEARCHFW_PHYSICALDEL_DOCUMENTLINK");

			//操作履歴(ドキュメントリンク)
			eim.util.OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, AppConstant.DOCLINK_DELETE,
						AppConstant.LINK_ORIGIN, EIMConstant.OBJECT, childObj,
						EIMConstant.TARGET_DELETE, EIMConstant.OBJECT, parentFolderObj, null);
		}

	}

	/**
	 * セキュリティーを設定します。
	 *
	 * @param relType リレーションタイプ
	 * @param object オブジェクト
	 * @param security セキュリティー
	 * @param helper
	 * @throws Exception
	 */
	public static void applySecurity(RelationTypeDomain relType, ObjectDomain object, SecurityDomain security) throws Exception {
		ApplicationContext context = ApplicationContextLoader.getApplicationContext();
		ObjectService objectService = (ObjectService) context.getBean("objectService4");

		// Apply Security
		objectService.setSecurity(object, security);

		// Child Relation
		List<ObjectDomain> childRelList = AppDocumentUtil.getChildObjectByRelTypeAndParent(relType, object, null);

		if (childRelList != null) {
			ObjectTypeDomain folderObjType = AppDocumentLogicUtil.getFolderObjectType();
			List<Long> folderObjTypeIds = AppDocumentLogicUtil.getChildObjectTypeList(folderObjType.getId());
			for (ObjectDomain childObj : childRelList) {

				if (folderObjTypeIds.contains(childObj.getType().getId())) {
					// Recurrenty
					applySecurity(relType, childObj, security);
				} else {
					// Apply Security
					objectService.setSecurity(childObj, security);
				}
			}
		}
	}

	/**
	 * ワークスペースの改名処理を行います。
	 *
	 * @param object 対象オブジェクト
	 * @param relType
	 * @throws Exception
	 */
	public static void renameObjectForWorkSpace(ObjectDomain object, RelationTypeDomain relType) throws Exception {

		ApplicationContext context = ApplicationContextLoader.getApplicationContext();
		ObjectService objectService = (ObjectService) context.getBean("objectService4");

		// Windows禁止文字チェック
		AppDocumentUtil.checkValidateFName(object.getName());

		// 元の名称取得
		ObjectDomain oldObjectDomain = objectService.getById(object.getId());

		// Rename Object
		objectService.updateName(object, object.getName(), DuplicateCheckModeEnum.TYPE);

		// 子オブジェクトのパスを変更する(改名することで必要となる)
		EIMSession sess = ConvertUtils.toEIMSession(EIMThreadContext.getTransactionContext());
		String path = "/";
		AppLogicUtil.setPath(sess, ConvertUtils.toEIMObject(object), path, oldObjectDomain.getName());
	}

	/**
	 * オブジェクトの改名処理を行います。
	 *
	 * @param object 対象オブジェクト
	 * @param relType
	 * @throws Exception
	 */
	public static void renameObject(ObjectDomain object, RelationTypeDomain relType) throws Exception {

		ApplicationContext context = ApplicationContextLoader.getApplicationContext();
		ObjectService objectService = (ObjectService) context.getBean("objectService4");
		FormatService formatService = (FormatService) context.getBean("formatService2");
		FileDao fileDao = (FileDao) context.getBean("fileDao2");

		RelationDomain docRel = null;

		List<RelationDomain> relList = AppDocumentUtil.getParentRelationByRelTypeAndChild(relType, object, new AccessRoleTypeDomain("READ"));

		if (relList != null && relList.size() > 0) {
			docRel = relList.get(0);
		}

		// ステータスチェック - WF付きフォルダ以下、かつ、編集中以外の場合、改名できない
		if (isUnderFolderWithWorkflow(object) && object.getStatus() != null
				&& object.getStatus().getType().getBase().getId() != AppConstant.STATUS_TYPE_KIND_ID_EDITTING) {
			// 改名権限がありません。
			throw new EIMException("EIM.ERROR.LOGIC.NORENAMEROLE");
		}

		// Windows禁止文字チェック
		AppDocumentUtil.checkValidateFName(object.getName());

		// 元の名称取得
		ObjectDomain oldObjectDomain = objectService.getById(object.getId());

		// Rename Object
		if (docRel != null) {
			objectService.updateName(object, docRel.getParent(), relType, object.getName(), DuplicateCheckModeEnum.INHERITTYPE);
		}

		// フォルダの場合のみ再帰的に「パス」を更新
		if (isTypeOfFolder(object.getType())) {
			EIMSession sess = ConvertUtils.toEIMSession(EIMThreadContext.getTransactionContext());
			String path = eim.util.StringUtils.nullToBlank(AppObjectUtil.getPath(ConvertUtils.toEIMObject(object)));
			AppLogicUtil.setPath(sess, ConvertUtils.toEIMObject(object), path, oldObjectDomain.getName());
		}
		// If Document Then Rename File
		else if (isTypeOfDocument(object.getType())) {
			// 拡張子の取得 ('.'付き)
			String fileExt = StringUtils.nullToBlank(StringUtils.getFileExt(object.getName()));

			// Format
			FormatDomain format = formatService.getDefaultByObjectType(object.getType());

			// 拡張子が変わる場合はファイルの拡張子も変更
			DirectoryDomain dir = format.getOnlineDirectory();
			FileDomain file = fileDao.getByObjectAndFormat(object, format);
			boolean isExtChanged = !fileExt.equals(StringUtils.nullToBlank(file.getExt())); // 拡張子変更判定

			if (isExtChanged) {
				File fromF = new File(dir.getPath() + FileUtil.getFileName(object, file));
				File toF = new File(dir.getPath() + object.getId() + fileExt);
				fromF.renameTo(toF);
				file.setName(object.getName());
			}

			// Rename File
			fileDao.updateName(object, file, object.getName());

			// 公開ドキュメント(PDF化ファイル)の改名
			FormatDomain formatPDF = formatService.getByDefinitionName(ConfigUtils.getByKey("FORMAT_NAME_PUBLIC"));
			FileDomain filePDF = fileDao.getByObjectAndFormat(object, formatPDF);
			if (filePDF != null) {
				fileDao.updateName(object, filePDF, StringUtils.getFileBody(object.getName()) + StringUtils.nullToBlank(filePDF.getExt()));

				//シンボリックリンク且つ拡張子が変わる場合、シンボリックリンクを再作成する。
				FileSystem fs = FileSystems.getDefault();
				File dstFile = new File(filePDF.getDirectory().getPath() + object.getId() + filePDF.getExt());
				boolean isSymbolicLink = Files.isSymbolicLink(fs.getPath(dstFile.getPath()));
				if(isExtChanged && isSymbolicLink) {
					dstFile.delete();
					FileUtils.createSymbolicLink(new File(dir.getPath() + object.getId() + fileExt), dstFile);
				}
			}
		}
	}

	/**
	 * ワークスペース作成時の初期属性を生成します。
	 *
	 * @throws Exception
	 */
	public static List<AttributeDomain> getInitAttributeForWorkspace() throws Exception {
		ApplicationContext context = ApplicationContextLoader.getApplicationContext();
		AttributeTypeService attributeTypeService = (AttributeTypeService) context.getBean("attributeTypeService2");

		List<AttributeDomain> attList = new ArrayList<AttributeDomain>();

		//使用可能ドキュメントタイプ絞込みフラグ：0
		AttributeTypeDomain limitDocTypeFlagAttTypeDomain = attributeTypeService.getByDefinitionName(ConfigUtils.getByKey("ATTR_NAME_WORKSPACE_LIMIT_DOCUMENT_TYPE_FLAG"));
		AttributeDomain limitDocTypeFlagAttDomain = new AttributeDomain();
		limitDocTypeFlagAttDomain.setAttributeType(limitDocTypeFlagAttTypeDomain);
		limitDocTypeFlagAttDomain.setLong(0);
		attList.add(limitDocTypeFlagAttDomain);

		//使用可能フォルダタイプ絞込みフラグ：0
		AttributeTypeDomain limitFolderTypeFlagAttTypeDomain = attributeTypeService.getByDefinitionName(ConfigUtils.getByKey("ATTR_NAME_WORKSPACE_LIMIT_FOLDER_TYPE_FLAG"));
		AttributeDomain limitFolderTypeFlagAttDomain = new AttributeDomain();
		limitFolderTypeFlagAttDomain.setAttributeType(limitFolderTypeFlagAttTypeDomain);
		limitFolderTypeFlagAttDomain.setLong(0);
		attList.add(limitFolderTypeFlagAttDomain);

		//使用可能タグタイプ絞込みフラグ：0
		AttributeTypeDomain limitTagFlagAttTypeDomain = attributeTypeService.getByDefinitionName(ConfigUtils.getByKey("ATTR_NAME_WORKSPACE_LIMIT_TAG_TYPE_FLAG"));
		AttributeDomain limitTagTypeFlagAttDomain = new AttributeDomain();
		limitTagTypeFlagAttDomain.setAttributeType(limitTagFlagAttTypeDomain);
		limitTagTypeFlagAttDomain.setLong(0);
		attList.add(limitTagTypeFlagAttDomain);

		//使用可能セキュリティ絞込みフラグ：0
		AttributeTypeDomain limitSecurityFlagAttTypeDomain = attributeTypeService.getByDefinitionName(ConfigUtils.getByKey("ATTR_NAME_WORKSPACE_LIMIT_SECURITY_FLAG"));
		AttributeDomain limitSecurityTypeFlagAttDomain = new AttributeDomain();
		limitSecurityTypeFlagAttDomain.setAttributeType(limitSecurityFlagAttTypeDomain);
		limitSecurityTypeFlagAttDomain.setLong(0);
		attList.add(limitSecurityTypeFlagAttDomain);

		return attList;
	}

	/**
	 * ワークスペース作成時の初期属性を除外します。
	 *
	 * @param attributeList
	 * @throws Exception
	 */
	public static List<AttributeDomain> removeInitAttributeForWorkspace(List<AttributeDomain> attributeList) throws Exception {

		List<AttributeDomain> attList = new ArrayList<AttributeDomain>();

		if(attributeList == null){
			return attList;
		}

		Iterator<AttributeDomain> ite = attributeList.iterator();
		while(ite.hasNext()){
			AttributeDomain attributeDomain = (AttributeDomain)ite.next();
			if(attributeDomain.getAttributeType().getDefinitionName().equals(ConfigUtils.getByKey("ATTR_NAME_WORKSPACE_LIMIT_DOCUMENT_TYPE_FLAG"))){
				//使用可能ドキュメントタイプ絞込みフラグ
				continue;
			}else if(attributeDomain.getAttributeType().getDefinitionName().equals(ConfigUtils.getByKey("ATTR_NAME_WORKSPACE_LIMIT_FOLDER_TYPE_FLAG"))){
				//使用可能フォルダタイプ絞込みフラグ
				continue;
			}else if(attributeDomain.getAttributeType().getDefinitionName().equals(ConfigUtils.getByKey("ATTR_NAME_WORKSPACE_LIMIT_TAG_TYPE_FLAG"))){
				//使用可能タグタイプ絞込みフラグ
				continue;
			}else if(attributeDomain.getAttributeType().getDefinitionName().equals(ConfigUtils.getByKey("ATTR_NAME_WORKSPACE_LIMIT_SECURITY_FLAG"))){
				//使用可能セキュリティ絞込みフラグ
				continue;
			}else if(attributeDomain.getAttributeType().getDefinitionName().equals(ConfigUtils.getByKey("ATTR_NAME_WORKSPACE_SELECTABLE_DOCUMENT_TYPE"))){
				//使用可能ドキュメントタイプ
				continue;
			}else if(attributeDomain.getAttributeType().getDefinitionName().equals(ConfigUtils.getByKey("ATTR_NAME_WORKSPACE_SELECTABLE_FOLDER_TYPE"))){
				//使用可能フォルダタイプ
				continue;
			}else if(attributeDomain.getAttributeType().getDefinitionName().equals(ConfigUtils.getByKey("ATTR_NAME_WORKSPACE_SELECTABLE_TAG_TYPE"))){
				//使用可能タグタイプ
				continue;
			}else if(attributeDomain.getAttributeType().getDefinitionName().equals(ConfigUtils.getByKey("ATTR_NAME_WORKSPACE_SELECTABLE_SECURITY"))){
				//使用可能セキュリティ
				continue;
			}else if(attributeDomain.getAttributeType().getDefinitionName().equals(ConfigUtils.getByKey("ATTR_NAME_FOLDER_TO_LOW_ATTR"))){
				//下位への引継属性
				continue;
			}else if(attributeDomain.getAttributeType().getDefinitionName().equals(ConfigUtils.getByKey("ATTR_NAME_FOLDER_LOW_FOLDER_SEC"))){
				//下位フォルダ管理セキュリティ
				continue;
			}else if(attributeDomain.getAttributeType().getDefinitionName().equals(ConfigUtils.getByKey("ATTR_NAME_WORKSPACE_NAME_ATTR"))){
				//名称割当て属性
				continue;
			}else if(attributeDomain.getAttributeType().getDefinitionName().equals(ConfigUtils.getByKey("ATTR_NAME_WORKSPACE_ADMINISTRATOR"))){
				//責任者
				continue;
			}else if(attributeDomain.getAttributeType().getDefinitionName().equals(ConfigUtils.getByKey("ATTR_NAME_WORKSPACE_ADMINISTRATOR_TYPE"))){
				//責任者種別
				continue;
			}else{
				attList.add(attributeDomain);
			}
		}

		return attList;
	}

	/**
	 * ワークスペース作成時の必須属性を取得します。
	 *
	 * @param attributeList
	 * @throws Exception
	 */
	public static List<AttributeDomain> getInitAttributeForWorkspace(List<AttributeDomain> attributeList) throws Exception {

		List<AttributeDomain> attList = new ArrayList<AttributeDomain>();

		if(attributeList == null){
			return attList;
		}

		Iterator<AttributeDomain> ite = attributeList.iterator();
		while(ite.hasNext()){
			AttributeDomain attributeDomain = (AttributeDomain)ite.next();
			if(attributeDomain.getAttributeType().getDefinitionName().equals(ConfigUtils.getByKey("ATTR_NAME_WORKSPACE_LIMIT_DOCUMENT_TYPE_FLAG"))){
				//使用可能ドキュメントタイプ絞込みフラグ
				attList.add(attributeDomain);
			}else if(attributeDomain.getAttributeType().getDefinitionName().equals(ConfigUtils.getByKey("ATTR_NAME_WORKSPACE_LIMIT_FOLDER_TYPE_FLAG"))){
				//使用可能フォルダタイプ絞込みフラグ
				attList.add(attributeDomain);
			}else if(attributeDomain.getAttributeType().getDefinitionName().equals(ConfigUtils.getByKey("ATTR_NAME_WORKSPACE_LIMIT_TAG_TYPE_FLAG"))){
				//使用可能タグタイプ絞込みフラグ
				attList.add(attributeDomain);
			}else if(attributeDomain.getAttributeType().getDefinitionName().equals(ConfigUtils.getByKey("ATTR_NAME_WORKSPACE_LIMIT_SECURITY_FLAG"))){
				//使用可能セキュリティ絞込みフラグ
				attList.add(attributeDomain);
			}else if(attributeDomain.getAttributeType().getDefinitionName().equals(ConfigUtils.getByKey("ATTR_NAME_WORKSPACE_SELECTABLE_DOCUMENT_TYPE"))){
				//使用可能ドキュメントタイプ
				attList.add(attributeDomain);
			}else if(attributeDomain.getAttributeType().getDefinitionName().equals(ConfigUtils.getByKey("ATTR_NAME_WORKSPACE_SELECTABLE_FOLDER_TYPE"))){
				//使用可能フォルダタイプ
				attList.add(attributeDomain);
			}else if(attributeDomain.getAttributeType().getDefinitionName().equals(ConfigUtils.getByKey("ATTR_NAME_WORKSPACE_SELECTABLE_TAG_TYPE"))){
				//使用可能タグタイプ
				attList.add(attributeDomain);
			}else if(attributeDomain.getAttributeType().getDefinitionName().equals(ConfigUtils.getByKey("ATTR_NAME_WORKSPACE_SELECTABLE_SECURITY"))){
				//使用可能セキュリティ
				attList.add(attributeDomain);
			}else if(attributeDomain.getAttributeType().getDefinitionName().equals(ConfigUtils.getByKey("ATTR_NAME_FOLDER_TO_LOW_ATTR"))){
				//下位への引継属性
				attList.add(attributeDomain);
			}else if(attributeDomain.getAttributeType().getDefinitionName().equals(ConfigUtils.getByKey("ATTR_NAME_FOLDER_LOW_FOLDER_SEC"))){
				//下位フォルダ管理セキュリティ
				attList.add(attributeDomain);
			}else if(attributeDomain.getAttributeType().getDefinitionName().equals(ConfigUtils.getByKey("ATTR_NAME_WORKSPACE_NAME_ATTR"))){
				//名称割当て属性
				attList.add(attributeDomain);
			}else if(attributeDomain.getAttributeType().getDefinitionName().equals(ConfigUtils.getByKey("ATTR_NAME_WORKSPACE_ADMINISTRATOR"))){
				//責任者
				attList.add(attributeDomain);
			}else if(attributeDomain.getAttributeType().getDefinitionName().equals(ConfigUtils.getByKey("ATTR_NAME_WORKSPACE_ADMINISTRATOR_TYPE"))){
				//責任者種別
				attList.add(attributeDomain);
			}else{
				continue;
			}
		}

		return attList;
	}

	/**
	 * 属性リストに属性リストで渡された属性を追加します。
	 *
	 * @param attributeList 追加元属性リスト
	 * @param addTargetAttributeList 追加対象属性リスト
	 * @throws Exception
	 */
	public static List<AttributeDomain> addAttributeDomain(List<AttributeDomain> attributeList,List<AttributeDomain> addTargetAttributeList) throws Exception {


		if(attributeList == null && addTargetAttributeList == null){
			return new ArrayList<AttributeDomain>();
		}else if(attributeList == null && addTargetAttributeList != null){
			return addTargetAttributeList;
		}else if(attributeList != null && addTargetAttributeList == null){
			return attributeList;
		}

		if(attributeList.size() == 0 && addTargetAttributeList.size() == 0){
			return new ArrayList<AttributeDomain>();
		}else if(attributeList.size() == 0 && addTargetAttributeList.size() > 0){
			return addTargetAttributeList;
		}else if(attributeList.size() > 0 && addTargetAttributeList.size() == 0){
			return attributeList;
		}

		List<AttributeDomain> baseAttributeList = null;
		List<AttributeDomain> addAttributeList = null;
		if(attributeList.size() > addTargetAttributeList.size()){
			baseAttributeList = attributeList;
			addAttributeList = addTargetAttributeList;
		}else{
			baseAttributeList = addTargetAttributeList;
			addAttributeList = attributeList;
		}

		Iterator<AttributeDomain> ite = addAttributeList.iterator();
		while(ite.hasNext()){
			AttributeDomain target = (AttributeDomain)ite.next();
			baseAttributeList.add(target);
		}

		return baseAttributeList;
	}


	/**
	 * ドキュメントのブランチ情報を論理削除から復帰します。
	 *
	 * @param object 対象のドキュメントオブジェクト
	 * @param relTypeBranch ブランチリレーションタイプ
	 * @throws Exception
	 */
	public static void returnDocumentBranch(ObjectDomain object, RelationTypeDomain relTypeBranch) throws Exception {
		ApplicationContext context = ApplicationContextLoader.getApplicationContext();
		RelationService relationService = (RelationService) context.getBean("relationService2");
		AttributeTypeService attributeTypeService = (AttributeTypeService) context.getBean("attributeTypeService2");

		// 一番古いバージョンのオブジェクトを取得
		List<ObjectDomain> objects = Arrays.asList(VersionUtils.getVersion(object).values().toArray(new ObjectDomain[0]));
		ObjectDomain rootObj = objects.get(0);
		// 一番古いバージョンのオブジェクトを子とするブランチリレーションを取得
		List<RelationDomain> relList = AppDocumentUtil.getParentRelationByRelTypeAndChild(relTypeBranch, rootObj, null);
		if (relList != null && relList.size() > 0) {
			// ブランチ先削除属性に「論理削除していない」を設定
			RelationDomain branchRel = relList.get(0);
			AttributeTypeDomain attrTypeBranchTargetDelete = attributeTypeService.getByDefinitionName(ConfigUtils
					.getByKey("ATTR_NAME_BRANCH_TARGET_DELETE"));

			relationService.setAttributeSingleLong(branchRel, attrTypeBranchTargetDelete, AppConstant.FLAG_OFF);
		}
	}

	/**
	 * フォルダ配下のドキュメントのブランチ情報を論理削除から復帰します。
	 *
	 * @param folderObj フォルダオブジェクト
	 * @param relTypeBranch ブランチリレーションタイプ
	 * @param relTypeDoc ドキュメントリレーションタイプ
	 * @throws Exception
	 */
	public static void returnFolderBranch(ObjectDomain folderObj, RelationTypeDomain relTypeBranch, RelationTypeDomain relTypeDoc) throws Exception {

		// フォルダ配下のリレーションリストを取得
		List<RelationDomain> childRelList = AppDocumentUtil.getChildRelationListByRelType(relTypeDoc, folderObj, new AccessRoleTypeDomain("READ"));
		if (childRelList != null) {
			ObjectTypeDomain folderObjType = AppDocumentLogicUtil.getFolderObjectType();
			List<Long> folderObjTypeIds = AppDocumentLogicUtil.getChildObjectTypeList(folderObjType.getId());

			for (RelationDomain rel : childRelList) {
				// Child Object
				ObjectDomain childObj = rel.getChild();
				// オブジェクトタイプがフォルダなら再起呼び出し
				if (folderObjTypeIds.contains(childObj.getType().getId())) {
					returnFolderBranch(childObj, relTypeBranch, relTypeDoc);
				}
				// ドキュメントの場合、ブランチを論理削除
				else {
					returnDocumentBranch(childObj, relTypeBranch);
				}
			}
		}
	}

	/**
	 * 対象ドキュメントリストから公開ドキュメントが存在するObjectIDをKeyに設定したHashMapを返却する。
	 *
	 * @param objectDomainList ドキュメントオブジェクトリスト
	 * @throws Exception
	 */
	public static HashMap<Long,String> getPublicDocumentMap(List<ObjectDomain> objectDomainList) throws Exception {

		HashMap<Long,String> result = new HashMap<Long,String>();

		if (objectDomainList.size() == 0) {
			return result;
		}

		//ステータスなしの「改定中 過去レビジョン全て」「改定なし」「初期登録」を取得
		//これに非該当のドキュメントは公開アイコンを表示しない
		List<Long> objectIdList = objectDomainList.stream().map(obj -> obj.getId()).collect(Collectors.toList());
		String sql = AppSqlUtil.getSqlNoStatusPublicIconObj(objectIdList);

		SearchConditionBuildHelper h = new SearchConditionBuildHelper();
		EIMSearchSelectEIMObject noSTEditObjSelect = new EIMSearchSelectEIMObject();

		noSTEditObjSelect.setCondition(
				h.group(h.opAnd()).addCondition(
						new EIMSearchConditionIn(h.opAnd(),EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.ID,h.opIn(),sql))
					);

		noSTEditObjSelect.setResultAttrs(new ArrayList<>());
		EIMSession sess = ConvertUtils.toEIMSession(EIMThreadContext.getTransactionContext());
		EIMSearchResultList noSTPublicObjList = SearchUtils.searchObjects(sess, noSTEditObjSelect, new EIMSearchLimitCountCondition(-1, false));

		for(int i = 0; i< noSTPublicObjList.size() ;i ++){
			result.put((long) ((EIMObject)noSTPublicObjList.get(i)).getId(),"");
		}

		return result;
	}


	/**
	 * オブジェクト(ドキュメント・フォルダ・タグ)、あるいはその配下にタグが付与されているかどうかを判定する。
	 *
	 * @param objectDomain 対象のフォルダオブジェクト
	 * @param relationType ドキュメントリレーションのタイプ
	 * @param isRecursive フォルダ配下のドキュメント・フォルダ・タグについて再帰処理を行うかどうか
	 * @return オブジェクトあるいはその配下にタグが付与されているかどうか
	 * @throws Exception
	 */
	public static boolean isTagAssignedObject(ObjectDomain objectDomain, RelationTypeDomain relationType, boolean isRecursive) throws Exception {

		if (isTypeOfDocument(objectDomain.getType())) {
			return isTagAssignedDocument(objectDomain);
		}
		// フォルダの場合
		else if (isTypeOfFolder(objectDomain.getType())) {
			if (isTagAssigned(objectDomain)) {
				return true;
			}
			// 再帰処理
			if (isRecursive) {
				// 配下オブジェクトの取得
				List<ObjectDomain> childList = AppDocumentUtil.getChildObjectByRelTypeAndParent(relationType, objectDomain, null);

				if (childList != null) {
					for (ObjectDomain tmpObj : childList) {
						if (isTagAssignedObject(tmpObj, relationType, true)) {
							return true;
						}
					}
				}
			}
		}
		return false;
	}

	/**
	 * ドキュメントにタグが付与されているかどうかを判定する。
	 * ※ 判定はドキュメントの全履歴について行う
	 *
	 * @param object 対象のドキュメントオブジェクト
	 * @return trueならタグが付与されている
	 * @throws Exception
	 */
	public static boolean isTagAssignedDocument(ObjectDomain object) throws Exception {
		// 削除対象オブジェクトを全バージョン取得
		Map<Long, ObjectDomain> revObjectMap = VersionUtils.getVersion(object);

		// 削除フラグの設定
		List<ObjectDomain> objList = Arrays.asList(revObjectMap.values().toArray(new ObjectDomain[0]));
		for (int i = 0; i < objList.size(); i++) {
			// タグが付与されたドキュメントの場合は削除できない
			if (isTagAssigned(objList.get(i))) {
				return true;
			}
		}
		return false;
	}

	/**
	 * オブジェクト(ドキュメント・フォルダ・タグ)にタグが付与されているかどうかを判定する
	 *
	 * @param object 対象のオブジェクト
	 * @return trueならタグが付与されている
	 * @throws Exception
	 */
	private static boolean isTagAssigned(ObjectDomain object) throws Exception {
		List<AttributeDomain> tmpAttrList = object.getAttributeList();
		String nameTag = ConfigUtils.getByKey("ATTR_NAME_FOLDER_TAG");
		for (int i = 0; i < tmpAttrList.size(); i++) {
			if (tmpAttrList.get(i).getAttributeType().getDefinitionName().equals(nameTag)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * 指定オブジェクトタイプがタグ系タイプかどうかを返します
	 *
	 * @param objectTypeDomain オブジェクトタイプ
	 * @return タグ系タイプならtrue
	 * @throws Exception
	 */
	public static boolean isTypeOfTag(ObjectTypeDomain objectTypeDomain) throws Exception {
		ApplicationContext context = ApplicationContextLoader.getApplicationContext();
		ObjectTypeService objectTypeService = (ObjectTypeService) context.getBean("objectTypeService2");

		boolean isTypeOfTag = false;
		// 「タグ」オブジェクトタイプ名称の取得
		String objectTypeName = ConfigUtils.getByKey("OBJECT_TYPE_NAME_TAG");
		ObjectTypeDomain objectType = objectTypeService.getByDefinitionName(objectTypeName);
		// オブジェクトタイプチェック
		if (objectTypeDomain != null && getChildObjectTypeList(objectType.getId()).contains(objectTypeDomain.getId())) {
			isTypeOfTag = true;
		}
		return isTypeOfTag;
	}

	/**
	 * フォルダ配下のタグを物理削除します。
	 *
	 * @param objectDomain 対象のフォルダオブジェクト
	 * @param relationTypeDomain ドキュメントリレーションのタイプ
	 * @throws Exception
	 */
	public static void physicalDeleteTagUnderFolder(ObjectDomain objectDomain, RelationTypeDomain relTypeDoc) throws Exception {
		// 配下オブジェクトの取得
		List<ObjectDomain> childList = AppDocumentUtil.getChildObjectByRelTypeAndParent(relTypeDoc, objectDomain, null);

		if (childList != null) {
			for (ObjectDomain tmpObj : childList) {
				// タグであれば
				if (isTypeOfTag(tmpObj.getType())) {
					// タグの物理削除
					physicalDeleteTag(tmpObj, objectDomain, relTypeDoc);
				}
				// フォルダであれば
				else if (isTypeOfFolder(tmpObj.getType())) {
					// 再帰処理
					physicalDeleteTagUnderFolder(tmpObj, relTypeDoc);
				}
			}
		}
	}

	/**
	 * タグを物理削除します。
	 * ※ タグが有効なものであった場合(有効期限が設定されており、かつ有効期限内)例外を返却します
	 *
	 * @param object  対象のタグオブジェクト
	 * @param parentObj タグの親オブジェクト
	 * @param relTypeDoc ドキュメントリレーションのタイプ
	 * @throws Exception
	 */
	private static void physicalDeleteTag(ObjectDomain object, ObjectDomain parentObj, RelationTypeDomain relTypeDoc) throws Exception {
		ApplicationContext context = ApplicationContextLoader.getApplicationContext();
		RelationService relationService = (RelationService) context.getBean("relationService2");

		// 有効期限のチェック
		AttributeDomain attributeDomain = object.getAttribute(ConfigUtils.getByKey("ATTR_NAME_FOLDER_EFFECT_DATE"));
		if (attributeDomain != null) {
			Date effectDate = attributeDomain.getDate();
			if (effectDate != null) {
				// システム日付 ＜＝ 対象オブジェクトの属性値「有効期限」の場合はエラー
				if (!AppDocumentLogicUtil.judgeExpirationDate(effectDate)) {
					// 有効なドキュメント、またはフォルダは削除できません。
					throw new EIMException("EIM.ERROR.LOGIC.NOTDEL.EFFECTIVE.DOCFOL");
				}
			}
		}

		List<ObjectDomain> objList = TagUtil.getTagGivenObj(object);
		if (objList != null) {
			for (ObjectDomain obj : objList) {
				// タグの解除
				TagUtil.removeTag(obj, object);
			}
		}

		// ドキュメントリレーションの削除
		RelationDomain rel = relationService.getByTypeAndParentAndChild(relTypeDoc, parentObj, object);
		relationService.delete(rel);

		// タグオブジェクトの削除
		AppDocumentUtil.deleteObjectAll(object);
	}

	/**
	 * WFなしドキュメントがチェックアウト可能か否かの判定結果を返します。
	 *
	 * @param objectDomain チェックアウトするドキュメントのオブジェクト
	 * @return チェックアウト可能か否かの判定結果
	 */
	public static boolean isCheckoutEnabledNoWFDoc(ObjectDomain objectDomain, List<ObjectDomain> objectDomains) {
		// 最新履歴フラグが真、かつ、ロックユーザがいない、かつ、履歴が複数ある場合
		if (objectDomain.isLatest() && objectDomain.getLockUser() == null && objectDomain.getRevision() > 0) {
			// 最新から1つ前の履歴のオブジェクトを取得
			ObjectDomain prevObj = objectDomains.get(objectDomains.size() - 2);
			// 1つ前のオブジェクトの最新履歴フラグが真の場合(＝まだチェックインしてない)はチェックアウトできない
			if (prevObj.isLatest()) {
				return false;
			}
		}
		return true;
	}

	/*
	 * 「ワークスペース」オブジェクトタイプを取得
	 */
	public static ObjectTypeDomain getWorkspaceObjectType() throws Exception{
		if(_workspaceObjType==null) {
			ApplicationContext context = ApplicationContextLoader.getApplicationContext();
			ObjectTypeService objectTypeService = (ObjectTypeService) context.getBean("objectTypeService2");

			String objectTypeName = ConfigUtils.getByKey("OBJECT_TYPE_NAME_WORKSPACE");
			_workspaceObjType = objectTypeService.getByDefinitionName(objectTypeName);
		}

		return _workspaceObjType;
	}

	/*
	 * 「フォルダ」オブジェクトタイプを取得
	 */
	public static ObjectTypeDomain getFolderObjectType() throws Exception{
		if(_folderObjType==null) {
			ApplicationContext context = ApplicationContextLoader.getApplicationContext();
			ObjectTypeService objectTypeService = (ObjectTypeService) context.getBean("objectTypeService2");

			String objectTypeName = ConfigUtils.getByKey("OBJECT_TYPE_NAME_FOLDER");
			_folderObjType = objectTypeService.getByDefinitionName(objectTypeName);
		}

		return _folderObjType;
	}

	/*
	 * 「ドキュメント」オブジェクトタイプを取得
	 */
	public static ObjectTypeDomain getDocumentObjectType() throws Exception{
		if(_documentObjType==null) {
			ApplicationContext context = ApplicationContextLoader.getApplicationContext();
			ObjectTypeService objectTypeService = (ObjectTypeService) context.getBean("objectTypeService2");

			String objectTypeName = ConfigUtils.getByKey("OBJECT_TYPE_NAME_DOCUMENT");
			_documentObjType = objectTypeService.getByDefinitionName(objectTypeName);
		}

		return _documentObjType;
	}

	/*
	 * 「システムごみ箱」オブジェクトタイプを取得
	 */
	public static ObjectTypeDomain getRecycleObjectType() throws Exception{
		if(_recycleObjType==null) {
			ApplicationContext context = ApplicationContextLoader.getApplicationContext();
			ObjectTypeService objectTypeService = (ObjectTypeService) context.getBean("objectTypeService2");

			String objectTypeName = ConfigUtils.getByKey("OBJECT_TYPE_NAME_RECYCLE");
			_recycleObjType = objectTypeService.getByDefinitionName(objectTypeName);
		}
		return _recycleObjType;
	}

	/*
	 * 「ワークスペースごみ箱」オブジェクトタイプを取得
	 */
	public static ObjectTypeDomain getWsRecycleObjectType() throws Exception{
		if(_wsRecycleObjType==null) {
			ApplicationContext context = ApplicationContextLoader.getApplicationContext();
			ObjectTypeService objectTypeService = (ObjectTypeService) context.getBean("objectTypeService2");

			String objectTypeName = ConfigUtils.getByKey("OBJECT_TYPE_NAME_WORKSPACERECYCLE");
			_wsRecycleObjType = objectTypeService.getByDefinitionName(objectTypeName);
		}
		return _wsRecycleObjType;
	}

	/*
	 * 「ドキュメント」リレーションタイプを取得
	 */
	public static RelationTypeDomain getDocumentRelType() throws Exception{
		if(_documentRelType==null) {
			ApplicationContext context = ApplicationContextLoader.getApplicationContext();
			RelationTypeService relTypeService = (RelationTypeService) context.getBean("relationTypeService2");

			String relTypeName = ConfigUtils.getByKey("RELATION_TYPE_NAME_DOCUMENT");
			_documentRelType = relTypeService.getByDefinitionName(relTypeName);
		}

		return _documentRelType;
	}

	/*
	 * 「ごみ箱」リレーションタイプを取得
	 */
	public static RelationTypeDomain getRecycleRelType() throws Exception{
		if(_recycleRelType==null) {
			ApplicationContext context = ApplicationContextLoader.getApplicationContext();
			RelationTypeService relTypeService = (RelationTypeService) context.getBean("relationTypeService2");

			String relTypeName = ConfigUtils.getByKey("RELATION_TYPE_NAME_RECYCLE");
			_recycleRelType = relTypeService.getByDefinitionName(relTypeName);
		}

		return _recycleRelType;
	}

	/*
	 * ドキュメントの「パス」属性タイプタイプを取得
	 */
	public static AttributeTypeDomain getDocPathAttributeType() throws Exception{
		if(_docPathAttributeType==null) {
			ApplicationContext context = ApplicationContextLoader.getApplicationContext();
			AttributeTypeService attributeTypeService = (AttributeTypeService) context.getBean("attributeTypeService2");

			String attTypeName = ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PASS");
			_docPathAttributeType = attributeTypeService.getByDefinitionName(attTypeName);
		}

		return _docPathAttributeType;
	}
}
