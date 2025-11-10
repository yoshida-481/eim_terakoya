package common.util;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.context.ApplicationContext;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;

import eim.bo.EIMAccessRole;
import eim.bo.EIMAttributeType;
import eim.bo.EIMException;
import eim.bo.EIMObject;
import eim.bo.EIMRelation;
import eim.bo.EIMResource;
import eim.net.EIMSession;
import eim.util.DateUtils;
import eim.util.EIMConfig;
import eim.util.RelationUtils;
import eim.util.StringUtils;
import eim.util.VersionUtils;
import jp.co.ctc_g.eim.app.document.business.domain.DocumentDomain;
import jp.co.ctc_g.eim.app.document.business.domain.PlaceDomain;
import jp.co.ctc_g.eim.app.document.business.domain.WorkspaceDomain;
import jp.co.ctc_g.eim.app.document.business.service.DocumentService;
import jp.co.ctc_g.eim.app.document.common.util.AppDocumentUtil;
import jp.co.ctc_g.eim.app.document.presentation.dto.BoxDocumentCreateDTO;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.AttributeTypeCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.CodeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.OperationHistoryDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework2.business.service.AttributeTypeService;
import jp.co.ctc_g.eim.framework2.business.service.ObjectService;
import jp.co.ctc_g.eim.framework2.business.service.OperationHistoryService;
import jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework2.common.enumeration.ValueTypeEnum;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;
import jp.co.ctc_g.eim.framework2.common.util.ResourceUtils;
import jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.AttributeMapDomain;
import jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils;

public class CreateBoxDocumentObjectHelper {

	/** 一時格納フォルダオブジェクトのオブジェクトID **/
	private final String _tmpFolderObjId;
	/** 登録ユーザ **/
	private final UserDomain _createUser;
	/** ドキュメントオブジェクトタイプ **/
	private final ObjectTypeDomain _docObjType;
	/** Boxパス **/
	private final String _boxPath;
	/** ヘルパー **/
	private AppObjectConditionHelper _helper;

	/** ドキュメントサービス **/
	private DocumentService documentService;
	/** オブジェクトサービス **/
	private ObjectService objectService;

	/** ドキュメントオブジェクト関連の文字列 **/
	private static final String DOCUMENT_OBJ_TYPE = EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT");
	private static final String PROPERTY = EIMConfig.get("ATTR_NAME_DOCUMENT_PROP");
	private static final String HIGHER_WF_FOLDER = EIMConfig.get("ATTR_NAME_DOCUMENT_HIGHER_WF_FOLDER");
	private static final String EXPIRE_DATE = EIMConfig.get("ATTR_NAME_DOCUMENT_EFFECT_DATE");

	/** フォルダ／ワークスペースオブジェクト関連の文字列 **/
	private static final String FOLDER_OBJ_TYPE = EIMConfig.get("OBJECT_TYPE_NAME_FOLDER");
	private static final String DOCUMENT_CREATER = EIMConfig.get("ATTR_NAME_DOCUMENT_CREATE");
	private static final String LOW_FOLDER_SECURITY = EIMConfig.get("ATTR_NAME_WORKSPACE_LOW_FOLDER_SEC");

	/** その他 **/
	private static final String DOCUMENT_RELATION_TYPE = EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT");
	private static final String REGIST_PATH = EIMConfig.get("ATTR_NAME_REGIST_PATH");
	private static final String TMP_FOLDER_PATH = EIMConfig.get("TEMP");
	private static final String FORMAT_PUBLIC = EIMConfig.get("FORMAT_NAME_PUBLIC");

	/**
	 * コンストラクタ
	 * @param _dType ドキュメントのオブジェクトタイプ
	 * @param _oId
	 *            一時格納フォルダオブジェクトのオブジェクトID
	 * @param _uId
	 *            登録ユーザID
	 * @param _bPath
	 *            Boxパス
	 *
	 */
	public CreateBoxDocumentObjectHelper( ObjectTypeDomain _dType,String _oId, UserDomain _u, String _bPath) {
		_docObjType = _dType;
		_tmpFolderObjId = _oId;
		_createUser = _u;
		_boxPath = _bPath;
	}

	/**
	 * デフォルトコンストラクタ
	 */
    public CreateBoxDocumentObjectHelper() {
        this._tmpFolderObjId = null;
        this._createUser = null;
        this._boxPath = null;
        this._docObjType = null;
    }

	/**
	 * ドキュメントを登録する
	 *
	 * @param pathMap フルパスとフォルダオブジェクトのマップ
	 * @param fileName ドキュメント名
	 * @param path パス
	 * @return 登録したドキュメント
	 */
	public BoxDocumentCreateDTO createDocument(HashMap pathMap, String fileName, String path, Object attributeList)
			throws Exception {
		String errorMessage = null;
		BoxDocumentCreateDTO createResult = new BoxDocumentCreateDTO();

		//	セッションの取得
		EIMSession _sess = null;
		_sess = EIMThreadContext.getEIMSession();
		_helper = new AppObjectConditionHelper(_sess);

		// 親オブジェクト取得
		ObjectDomain parentObj = getParentObject(pathMap, getParentPath(path));
		// 同名オブジェクト取得。同名オブジェクトが存在する場合、ドキュメントとドキュメントリンクの判定も行う。
		// ドキュメントとドキュメントリンクの両方が存在する場合は両方ともtrue。
		ObjectDomain sameNameObj = ConvertUtils.toObjectDomain(AppObjectUtil.getObjListByFullPass(_sess, path + fileName, DOCUMENT_OBJ_TYPE), new AttributeMapDomain());
		boolean isDocLink = false;
		boolean isDocument = false;
		if (sameNameObj != null) {
			isDocLink = _helper.isDocumentLink(ConvertUtils.toEIMObject(parentObj), ConvertUtils.toEIMObject(sameNameObj));
			isDocument = _helper.isDocument(ConvertUtils.toEIMObject(parentObj), ConvertUtils.toEIMObject(sameNameObj));
		}
		try {
			if (sameNameObj == null || (isDocLink && isDocument == false)) {
				/* 同名のファイルが登録されていない場合、またはドキュメントリンクのみ存在している場合 */

				// 戻り値にファイル名を設定
				createResult.setObjName(fileName);

				List<AttributeDomain> attributeDomains = new ArrayList<>();

				DocumentDomain documentDomain = new DocumentDomain();
				documentDomain.setName(fileName);
				documentDomain.setType(_docObjType);
				documentDomain.setCreateUser(_createUser);
				PlaceDomain parentFolderDomain = new WorkspaceDomain();
				parentFolderDomain.setId(parentObj.getId());
				parentFolderDomain.setName(parentObj.getName());

				// Object を List<LinkedHashMap<String, Object>> にキャスト
				if (attributeList instanceof List<?>) {
					List<?> rawList = (List<?>) (attributeList);

					// ObjectMapper を準備
					ObjectMapper objectMapper = new ObjectMapper();
					// AttributeDomain に定義されていないフィールドがあってもエラーにしない設定
					objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);

					// 各オブジェクトを変換
					for (Object obj : rawList) {
						if (obj instanceof LinkedHashMap) {
							// LinkedHashMap にキャスト
							LinkedHashMap<String, Object> map = (LinkedHashMap<String, Object>) obj;

							// 変換処理
							AttributeDomain attributeDomain = objectMapper.convertValue(map, AttributeDomain.class);
							attributeDomains.add(attributeDomain);
						}
					}
				}

				// 属性値を設定
				documentDomain.setAttributeList(attributeDomains);

				// コード型として保持されているリスト値を本来のデータ型に再設定する
				DocumentDomain convertedDocumentDomain = convertCodeToOriginalValue(documentDomain);

				//一時フォルダ内の指定ファイルの存在を確認
				String tmpFolderPath = TMP_FOLDER_PATH + _tmpFolderObjId;
				String tmpPath = tmpFolderPath + "/" + fileName;
				File tmpFolderFile = new File(tmpPath);
				if (tmpFolderFile.exists() == false) {
					throw new Exception();
				}

				InputStream inputStream = new FileInputStream(tmpFolderFile);

				ApplicationContext context = ApplicationContextLoader.getApplicationContext();
				DocumentService documentService = (DocumentService) context.getBean("documentService2");

				//ドキュメント登録
				DocumentDomain newDocumentDomain =documentService.create(convertedDocumentDomain , parentFolderDomain, inputStream);

				documentDomain.setId(newDocumentDomain.getId());

				// 色情報を保存する
				updateDisplayColor(documentDomain);

				// 戻り値に新しいオブジェクトIDをセット
				createResult.setObjId(newDocumentDomain.getId());

				// 操作履歴を作成
				OperationHistoryService operationHistoryService = (OperationHistoryService) ApplicationContextLoader
						.getApplicationContext().getBean("operationHistoryService2");

				OperationHistoryDomain operationHistory = new OperationHistoryDomain();
				operationHistory.setApplicationTypeId(2);
				operationHistory.setOperationTypeId(2401);
				operationHistory.setRecordInfoIdA(1);
				operationHistory.setRecordInfoIdB(36);
				operationHistory.setRecordObjectA(fileName);
				operationHistory.setRecordObjectB(_boxPath);
				operationHistory.setDetail(path);
				operationHistoryService.create(operationHistory);

				// SearchFramework 検索FW更新通知 対象：オブジェクト
				AppUpdateNoticeUtils.updateNoticeInsert(newDocumentDomain.getId(), "SEARCHFW_CREATE_DOCUMENT");

			} else {
				/* 同名のファイルが既に登録されている場合 */

				// 戻り値にファイル名を設定
				createResult.setObjName(fileName);

				// 同名ファイルと同名ドキュメントリンクが同時に存在している場合、確実にドキュメントを
				// 取得できるよう、再度取得しなおす
				if (isDocument && isDocLink) {
					EIMObject parentEIMObj = ConvertUtils.toEIMObject(parentObj);
					List rels = RelationUtils.getChildRelationListByRelType(_sess, parentEIMObj,
							_helper.getRelationTypeOfDocument(), EIMAccessRole.READ);
					for (int ii = 0; ii < rels.size(); ii++) {
						EIMObject obj = ((EIMRelation) rels.get(ii)).getChild();
						if (sameNameObj.getName().equals(obj.getName())) {
							sameNameObj = ConvertUtils.toObjectDomain((EIMObject) obj, new AttributeMapDomain());
							break;
						}
					}
				}

				// 先に処理対象ドキュメントの同一バージョン内のオブジェクト全てに行ロックをかける
				List<EIMObject> objListInVersion = VersionUtils.getVersion(_sess, ConvertUtils.toEIMObject(sameNameObj)).getList();
				long[] objIds = new long[objListInVersion.size()];
				for (int i = 0; i < objListInVersion.size(); i++) {
					objIds[i] = objListInVersion.get(i).getId();
				}
				AppObjectUtil.lockObjectById(_sess, objIds);

				//一時フォルダ内の指定ファイルの存在を確認
				String tmpFolderPath = TMP_FOLDER_PATH + _tmpFolderObjId;
				String tmpPath = tmpFolderPath + "/" + fileName;
				File tmpFolderFile = new File(tmpPath);
				if (tmpFolderFile.exists() == false) {
					throw new Exception();
				}

				InputStream inputStream = new FileInputStream(tmpFolderFile);

				ApplicationContext context = ApplicationContextLoader.getApplicationContext();
				DocumentService documentService = (DocumentService) context.getBean("documentService2");
				DocumentDomain documentDomain = documentService.getById(sameNameObj.getId());
				DocumentDomain newDocumentDomain = new DocumentDomain();

				//チェックアウト
				newDocumentDomain = documentService.checkout(documentDomain);

				documentDomain.setId(newDocumentDomain.getId());
				documentDomain.setName(fileName);

				//チェックイン
				documentService.checkin(documentDomain, inputStream);

				ObjectService objectService = (ObjectService) context.getBean("objectService2");

				List<AttributeDomain> attributeDomains = new ArrayList<>();

				// Object を List<LinkedHashMap<String, Object>> にキャスト
				if (attributeList instanceof List<?>) {
					List<?> rawList = (List<?>) (attributeList);

					// ObjectMapper を準備
					ObjectMapper objectMapper = new ObjectMapper();
					objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);

					// 各オブジェクトを変換
					for (Object obj : rawList) {
						if (obj instanceof LinkedHashMap) {
							LinkedHashMap<String, Object> map = (LinkedHashMap<String, Object>) obj;

							// 変換処理
							AttributeDomain attributeDomain = objectMapper.convertValue(map, AttributeDomain.class);
							attributeDomains.add(attributeDomain);

						}
					}
				}

				// 属性値を設定
				documentDomain.setAttributeList(attributeDomains);

				// コード型として保持されているリスト値を本来のデータ型に再設定する
				DocumentDomain convertedDocumentDomain = convertCodeToOriginalValue(documentDomain);

				// 属性値を更新
				objectService.update(convertedDocumentDomain);

				// 色情報を保存する
				updateDisplayColor(documentDomain);

				// 作成者を設定
				AppDocumentUtil.setAttrLong(objectService.getById(documentDomain.getId()), ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_CREATE"), _createUser.getId());

				// 戻り値に新しいオブジェクトIDをセット
				createResult.setObjId(newDocumentDomain.getId());

				// 操作履歴を作成
				OperationHistoryService operationHistoryService = (OperationHistoryService) ApplicationContextLoader
						.getApplicationContext().getBean("operationHistoryService2");

				OperationHistoryDomain operationHistory = new OperationHistoryDomain();
				operationHistory.setApplicationTypeId(2);
				operationHistory.setOperationTypeId(2401);
				operationHistory.setRecordInfoIdA(1);
				operationHistory.setRecordInfoIdB(36);
				operationHistory.setRecordObjectA(fileName);
				operationHistory.setRecordObjectB(_boxPath);
				operationHistory.setDetail(path);
				operationHistoryService.create(operationHistory);

				// SearchFramework 検索FW更新通知 対象：新しいオブジェクト + 古い同名のオブジェクト
				AppUpdateNoticeUtils.updateNoticeInsert(newDocumentDomain.getId(),
						"SEARCHFW_UPLOAD_CREATE_DOCUMENT");
				AppUpdateNoticeUtils.updateNoticeInsert(sameNameObj.getId(), "SEARCHFW_UPLOAD_OLD_DOCUMENT");

			}
		} catch (Exception e) {
			Log log = LogFactory.getLog(this.getClass().getName());
			log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
			errorMessage = EIMResource.getMessage("EIM.ERROR.SYSTEMERROR");
			createResult.setMessage(errorMessage);
		}



		return createResult;
	}

	/**
	 * 値リストに対応するためにコード型として格納されている属性値を本来のデータ型に変換します。
	 * @param attributeLists 属性ドメインリスト
	 * @param _docObjType ドキュメントタイプ
	 * @return
	 * @throws Exception
	 */
	private DocumentDomain convertCodeToOriginalValue(DocumentDomain documentDomain) throws Exception {

		DocumentDomain newDocumentDomain = (DocumentDomain) documentDomain.clone();

		// 属性タイプ一覧をDBから取得しMapに格納する
		AttributeTypeCriteria attributeTypeCriteria = new AttributeTypeCriteria();
		attributeTypeCriteria.setObjectTypeId( _docObjType.getId());
		attributeTypeCriteria.setIncludingParents(true);

		ApplicationContext context = ApplicationContextLoader.getApplicationContext();
		AttributeTypeService attrTypeService = (AttributeTypeService) context.getBean("attributeTypeService2");


		List<AttributeTypeDomain> attributeTypeList = attrTypeService.getList(attributeTypeCriteria);
		Map<Long, AttributeTypeDomain> attributeTypeMap = new HashMap<Long, AttributeTypeDomain>();

		for (AttributeTypeDomain attributeType : attributeTypeList) {
			// 属性タイプIDをキーにMapに格納する
			attributeTypeMap.put(new Long(attributeType.getId()), attributeType);
		}

		EIMSession sess = EIMThreadContext.getEIMSession();

		// コード型からオリジナルのバリュータイプに変換する
		for (AttributeDomain attribute : newDocumentDomain.getAttributeList()) {
			if (attribute.getAttributeType().getValueType() == ValueTypeEnum.CODE) {
				// オリジナルを取得する
				AttributeTypeDomain originalAttributeType = attributeTypeMap.get(attribute.getAttributeType().getId());

				// バリュータイプを設定する
				attribute.getAttributeType().setValueType(originalAttributeType.getValueType());

				// 属性値を設定する
				for (CodeDomain code : attribute.getCodeList()) {
					switch(attribute.getAttributeType().getValueType()) {
						case LONG:
							attribute.getLongList().add(Long.valueOf(code.getCode()));
							break;
						case STRING:
							attribute.getStringList().add(code.getCode());
							break;
						case DATE:
							// 文字列→Date変換
							Date dateValue = StringUtils.getDateFromString(code.getCode(), ResourceUtils.getByKey("EIM.FORMAT.DATE"));
							Date convertedDateValue = new Date(DateUtils.convCLTzToDBTzTime(sess, dateValue));
							attribute.getDateList().add(convertedDateValue);
							break;
						case TEXT:
							attribute.getTextList().add(code.getCode());
							break;
						case DOUBLE:
							attribute.getDoubleList().add(Double.valueOf(code.getCode()));
							break;
					}
				}

				// コード型属性を削除する
				attribute.getCodeList().clear();
			}
		}

		return newDocumentDomain;
	}

	/**
	 * 値リストに対応した色オブジェクトを更新します。
	 * @param form
	 * @throws Exception
	 */
	private void updateDisplayColor(DocumentDomain documentDomain) throws Exception {
		EIMSession sess = EIMThreadContext.getEIMSession();

		for (AttributeDomain attribute :documentDomain.getAttributeList()) {
			if (attribute.getAttributeType().getValueType() == ValueTypeEnum.CODE) {
				// 色オブジェクトを更新する
				EIMObject obj = new EIMObject(documentDomain.getId(), null, null, 0, false, null, null, null, null, null, null, false, false, null);
				EIMAttributeType attrType = new EIMAttributeType(attribute.getAttributeType().getId(), null, null);
				String value = null;
				CodeDomain code =attribute.getCode();
				if (code != null)
					value = code.getCode();

				DisplayColorUtil.updateDisplayColor(sess, obj, attrType, value);
			}
		}
	}

	/**
	 * 親オブジェクトを取得する。 登録時に作成したフォルダは属性リストのサイズが0であるため、再度DBから取得しなおしマップに登録し直す。
	 *
	 * @param pathMap フルパスとフォルダオブジェクトのマップ
	 * @param path 親オブジェクトのパス
	 */
	private ObjectDomain getParentObject(HashMap pathMap, String path) throws EIMException, Exception {
		ObjectDomain parentObj = ConvertUtils.toObjectDomain((EIMObject)pathMap.get(path), new AttributeMapDomain());

		if (parentObj == null) {
			throw new Exception("EIM.ERROR.LOGIC.NOFOLDER");
		} else if (parentObj.getAttributeList().size() == 0) {
			// 登録時に作成したフォルダは属性リストのサイズが0。
			// その場合、もう一度DBから取り直し再度マップに登録する。
			parentObj = objectService.getById(parentObj.getId());
			if (parentObj == null) {
				throw new Exception("EIM.ERROR.LOGIC.NOFOLDER");
			}
			pathMap.put(path, parentObj);
		}

		return parentObj;
	}

	/**
	 * 親フォルダのパスを取得する（最後の"/"はなし）
	 *
	 * @path パス
	 */
	private String getParentPath(String path) {
		return path.substring(0, path.lastIndexOf("/"));
	}


	public DocumentService getDocumentService() {
		return documentService;
	}

	public void setDocumentService(DocumentService documentService) {
		this.documentService = documentService;
	}

	public ObjectService getObjectService() {
		return objectService;
	}

	public void setObjectService(ObjectService objectService) {
		this.objectService = objectService;
	}

}
