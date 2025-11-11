package common.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.springframework.context.ApplicationContext;

import app.document.search.EIMDocSearchType;
import eim.bo.EIMAccessRole;
import eim.bo.EIMAttributeType;
import eim.bo.EIMException;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMRelation;
import eim.bo.EIMRelationType;
import eim.bo.EIMSearchConditionGroup;
import eim.bo.EIMSearchLimitCountCondition;
import eim.bo.EIMSearchSelectEIMObject;
import eim.bo.EIMSearchSelectEIMObject.PsedoAttributeTypeEnum;
import eim.bo.EIMSearchSelectEIMRelation;
import eim.bo.EIMSecurity;
import eim.bo.EIMUser;
import eim.bo.EIMVersion;
import eim.net.EIMSession;
import eim.util.AttributeUtils;
import eim.util.EIMConfig;
import eim.util.ObjectAttributeUtils;
import eim.util.ObjectUtils;
import eim.util.RelationUtils;
import eim.util.SearchUtils;
import eim.util.SecurityUtils;
import eim.util.StringUtils;
import eim.util.TypeConvertUtils;
import eim.util.VersionUtils;
import jp.co.ctc_g.eim.app.document.common.util.AppDocumentUtil;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.AttributeTypeCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AccessRoleTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.RelationDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.RelationTypeDomain;
import jp.co.ctc_g.eim.framework2.business.service.AttributeTypeService;
import jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;
import jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils;

/**
 * ドキュメント管理オブジェクト判定の支援クラス<br>
 * 各メソッドでDBから取得した結果は、可能な限りキャッシュします。本インスタンスをループ内・ネスト処理内で使用すると、DB問い合わせが減ってパフォーマンスが向上する可能性があります。
 */
public class AppObjectConditionHelper {
	/** フォルダタイプ名のXMLエスケープ済み文字列 */
	private static final String OBJTYPENAM_FOLDER_XML_ESCAPED = StringUtils.xmlEncode(EIMConfig.get("OBJECT_TYPE_NAME_FOLDER"));

	/** タグタイプ名のXMLエスケープ済み文字列 */
	private static final String OBJTYPENAM_TAG_XML_ESCAPED = StringUtils.xmlEncode(EIMConfig.get("OBJECT_TYPE_NAME_TAG"));

	/** ワークスペース固有ごみ箱タイプ名のXMLエスケープ済み文字列 */
	private static final String OBJTYPENAM_WORKSPACERECYCLE_XML_ESCAPED = StringUtils.xmlEncode(EIMConfig.get("OBJECT_TYPE_NAME_WORKSPACERECYCLE"));

	/** 名称割り当て属性の属性名 -ワークスペース/フォルダ */
	private static final String ATTR_NAME_OF_NAME_ATTR = EIMConfig.get("ATTR_NAME_WORKSPACE_NAME_ATTR");

	/** 下位引き継ぎ属性の属性名 -ワークスペース/フォルダ */
	private static final String ATTR_NAME_OF_TO_LOW_ATTR = EIMConfig.get("ATTR_NAME_WORKSPACE_TO_LOW_ATTR");

	/** 改訂内容の属性名 - ドキュメント */
	private static final String ATTR_NAME_OF_DOCUMENT_REV_CONTENT = EIMConfig.get("ATTR_NAME_DOCUMENT_REV_CONTENT");

	/** パス属性の属性名 -ワークスペース/フォルダ/ドキュメント */
	private static final String ATTR_NAME_OF_PATH = EIMConfig.get("ATTR_NAME_WORKSPACE_PASS");

	/** 上位からの引継ぎ属性 -フォルダ/ドキュメント */
	private static final String ATTR_NAME_OF_FROM_HIGH_ATTR = EIMConfig.get("ATTR_NAME_FOLDER_FROM_HIGH_ATTR");

	/** 上位WFフォルダ属性の属性名 -フォルダ/ドキュメント */
	private static final String ATTR_NAME_OF_HIGHER_WF_FOLDER = EIMConfig.get("ATTR_NAME_FOLDER_HIGHER_WF_FOLDER");

	/** 有効期限属性の属性名 -フォルダ/ドキュメント */
	private static final String ATTR_NAME_OF_EFFECT_DATE = EIMConfig.get("ATTR_NAME_FOLDER_EFFECT_DATE");

	/** PDF変換処理失敗属性の属性名 -ドキュメント */
	private static final String ATTR_NAME_OF_PUB_PROC_FAIL = EIMConfig.get("ATTR_NAME_DOCUMENT_PUB_PROC_FAIL");

	/** PDF分割状態属性の属性名 -ドキュメント */
	private static final String ATTR_NAME_OF_PDF_DIVIDE_STATUS = EIMConfig.get("ATTR_NAME_DOCUMENT_PDF_DIVIDE_STATUS");

	/** PDF結合処理失敗属性の属性名 -ドキュメント */
	private static final String ATTR_NAME_OF_PDF_JOIN_FAIL = EIMConfig.get("ATTR_NAME_DOCUMENT_PDF_JOIN_FAIL");

	/** 署名・暗号化状態の属性名 -ドキュメント/タグ */
	private static final String ATTR_NAME_OF_SIGN_ENC_STATUS = EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_STATUS");

	/** セッション */
	private EIMSession _sess;

	/** 読み取りのみセキュリティのマップ */
	private Map _readOnlySecIdMap;

	/** 読み取りのみセキュリティのマップ(ユーザ定義グループも含む) (<ユーザID<OBJECTID,Boolean>>) */
	private HashMap<Long,HashMap<Long,Boolean>> _readOnlyUserMap;

	/** ドキュメント系タイプのマップ */
	private Map _docTypeIdMap;

	/** フォルダ系タイプのマップ */
	private Map _folderTypeIdMap;

	/** タグ系タイプのマップ */
	private Map _tagTypeIdMap;

	/** 上位へ向かうフォルダ階層のマップ */
	private Map _upperFoldersMap = new HashMap();

	/** ワークスペースタイプ */
	private EIMObjectType _typeOfWorkspace;

	/** フォルダタイプ */
	private EIMObjectType _typeOfFolder;

	/** ごみ箱タイプ */
	private EIMObjectType _typeOfRecycle;

	/** ワークスペース固有ごみ箱タイプ */
	private EIMObjectType _typeOfWSRecycle;

	/** マイドキュメントタイプ */
	private EIMObjectType _typeOfMyDocument;

	/** ドキュメントリレーションタイプ */
	private EIMRelationType _reltypeOfDocument;

	/** ブランチリレーションタイプ */
	private EIMRelationType _reltypeOfBranch;

	/** (ドキュメント)リンクリレーションタイプ */
	private EIMRelationType _reltypeOfDocLink;
	
	/** ごみ箱リレーションタイプ */
	private EIMRelationType _reltypeOfRecycle;

	/** 属性タイプをキャッシュするMap<String attrDefName> */
	private Map _attrTypeMap;

	/** オブジェクトの一括ロードを行うローダークラス */
	private BulkLoader bulkLoader = new BulkLoader();

	/** オブジェクトタイプIDと適用されている属性タイプ一覧のMap */
	private Map<Long, List<EIMAttributeType>> objTypeId_AttributeListMap = new HashMap<>();

	/** オブジェクトタイプIDと適用されている属性タイプ一覧のMap */
	private Map<Long, List<AttributeTypeDomain>> objTypeId_AttributeListMap2 = new HashMap<>();

	/** パスとフォルダのMap */
	private Map<String, EIMObject> path_folderMap = new HashMap<>();

	/** 親オブジェクトと属性表示色オブジェクトMapのMap */
	private Map<Long, HashMap<String, EIMObject>> parentObjId_displayColorMapMap = new HashMap<>();

	/** オブジェクトIDとパス属性値配列のMap */
	private Map<Long, String[]> objId_pathArray = new HashMap<>();

	/** オブジェクトIDとリンク先属性値配列のMap */
	private Map<Long, long[]> objId_toLinkArray = new HashMap<>();

	/**
	 * コンストラクタ
	 *
	 * @param sess セッション
	 */
	public AppObjectConditionHelper(EIMSession sess) {
		this._sess = sess;
	}

	/**
	 * 指定属性名の属性タイプをキャッシュして返します
	 *
	 * @param attrTypeDefName 属性名のデフォルト名称
	 * @return 属性タイプ
	 * @throws Exception
	 */
	public EIMAttributeType getAttrType(String attrTypeDefName) throws Exception {
		if (_attrTypeMap == null)
			_attrTypeMap = new HashMap();
		EIMAttributeType attrType = (EIMAttributeType) _attrTypeMap.get(attrTypeDefName);
		if (attrType == null) {
			attrType = AttributeUtils.getAttributeTypeByName(_sess, attrTypeDefName);
			_attrTypeMap.put(attrTypeDefName, attrType);
		}
		return attrType;
	}

	/**
	 * 指定属性名の属性タイプをキャッシュして返します
	 *
	 * @param attrTypeDefName 属性名のデフォルト名称
	 * @return 属性タイプ
	 * @throws Exception
	 */
	public AttributeTypeDomain getAttrType2(String attrTypeDefName) throws Exception {
		if (_attrTypeMap == null)
			_attrTypeMap = new HashMap();
		AttributeTypeDomain attrType = (AttributeTypeDomain) _attrTypeMap.get(attrTypeDefName);
		if (attrType == null) {
			ApplicationContext context = ApplicationContextLoader.getApplicationContext();
			AttributeTypeService attributeTypeService = (AttributeTypeService) context.getBean("attributeTypeService2");
			attrType =  attributeTypeService.getByDefinitionName(attrTypeDefName);
			_attrTypeMap.put(attrTypeDefName, attrType);
		}
		return attrType;
	}
	
	/**
	 * ワークフロー付きフォルダかどうかを返す
	 *
	 * @param obj オブジェクト
	 * @return ワークフロー付きフォルダならtrue
	 * @throws Exception
	 */
	public boolean isTypeOfFolderWithWorkflow(EIMObject obj) throws Exception {
		// ワークフロー付きフォルダとは、
		// 1)フォルダタイプで
		// 2)ワークフローがあり
		// 3)「上位ワークフローフォルダー」属性が無い
		// もの
		return (isTypeOfFolder(obj.getType()) //
				&& (obj.getStatus() != null)//
		&& (obj.getAttribute(ATTR_NAME_OF_HIGHER_WF_FOLDER) == null)//
		);
	}

	/**
	 * ワークフロー付きドキュメントかどうかを返す
	 *
	 * @param obj オブジェクト
	 * @return ワークフロー付きドキュメントならtrue
	 * @throws Exception
	 */
	public boolean isTypeOfDocumentWithWorkflow(EIMObject obj) throws Exception {
		// ワークフロー付きフォルダとは、
		// 1)ドキュメントタイプで
		// 2)ワークフローがあり
		// 3)「上位ワークフローフォルダー」属性が無い
		// もの
		return (isTypeOfDocument(obj.getType()) //
				&& (obj.getStatus() != null)//
		&& (obj.getAttribute(ATTR_NAME_OF_HIGHER_WF_FOLDER) == null)//
		);
	}

	/**
	 * ワークフロー付きフォルダの下のフォルダかどうかを返す
	 *
	 * @param obj オブジェクト
	 * @return ワークフロー付きフォルダの下のフォルダならtrue
	 * @throws Exception
	 */
	public boolean isTypeOfFolderUnderFolderWithWorkflow(EIMObject obj) throws Exception {
		return (isTypeOfFolder(obj.getType()) //
				&& (obj.getStatus() != null)//
		&& (obj.getAttribute(ATTR_NAME_OF_HIGHER_WF_FOLDER) != null)//
		);
	}

	/**
	 * 引数オブジェクトがワークフロー付きフォルダの下に居るかどうかを返す。
	 *
	 * @param obj
	 * @return 引数オブジェクトがワークフロー付きフォルダの下に居るならtrue
	 * @throws Exception
	 */
	public boolean isUnderFolderWithWorkflow(EIMObject obj) throws Exception {
		return (obj.getAttribute(getAttrNameDocumentHigherWFFolder()) != null);
	}

	/**
	 * 引数オブジェクトがワークフロー付きフォルダの下に居る場合、上位ワークフロー付きフォルダのオブジェクトIDを返します。
	 *
	 * @param obj オブジェクト
	 * @return 上位ワークフロー付きフォルダのオブジェクトID。ワークフロー付きフォルダ外なら-1
	 * @throws Exception
	 */
	public long getUpperFolderWithWorkflowObjId(EIMObject obj) throws Exception {
		return AppObjectUtil.getIntAttr(_sess, obj, getAttrNameDocumentHigherWFFolder(), -1);
	}

	/**
	 * 引数オブジェクトに対してログインユーザーがアクセス可能かを判断します。
	 *
	 * @param obj オブジェクト
	 * @param doThrowException アクセス不可の時に例外を発生させるかどうか。trueの時例外発生
	 * @return アクセス権があればtrue
	 * @throws EIMException アクセス不可の時にmessageKey:"EIM.ERROR.LOGIC.NOACCESS"(「アクセス権がありません」)の例外を発生させます
	 * @throws Exception
	 */
	public boolean checkAccessibleStatusSelf(EIMObject obj, boolean doThrowException)
			throws Exception {
		// WFなしの場合は常にアクセス可。
		if ((obj.getStatus() == null) || !isReadOnlyAccess(obj))
			return true;
		// (WFあり、)(参照権限のみのユーザーで、)の場合、
		// ドキュメントもフォルダも「公開済」のみアクセス不可。
		if (obj.getStatus().getType().getKind() == AppConstant.STATUS_TYPE_KIND_ID_PUBLIC)//
			return true;
		if (doThrowException) {
			throw new EIMException(_sess, "EIM.ERROR.LOGIC.NOACCESS");// 「アクセス権がありません」
		} else {
			return false;
		}
	}

	/**
	 * 指定フォルダに対して、ログインユーザーがフォルダ操作を行う権限を持つかどうかを判断して返します。<br>
	 * 以下2条件を満たせばtrueを返します。
	 * <ul>
	 * <li>引数フォルダに引数アクセスロール権限がある
	 * <li>引数フォルダ構成管理属性が指定されている場合は、その値が指し示すセキュリティにUPDATE権限も必要
	 * </ul>
	 * ★ワークフローステータスは判断に含めません。
	 *
	 * @param obj 対象オブジェクト
	 * @param accessRole アクセスロール。無指定のときは負の値。
	 * @param doThrowException
	 * @return 権限があればtrue
	 * @throws Exception
	 */
	public boolean checkUpdatableToFolder(EIMObject obj, int accessRole) throws Exception {
		// フォルダタイプか？
		if (!isTypeOfFolder(obj.getType()) && !isTypeOfWorkspace(obj.getType()))
			return false;

		// 指定のアクセスロールを持つか？
		if (accessRole > 0 && obj.getSecurity() != null
				&& !SecurityUtils.authorized(_sess, obj,_sess.getUser(), accessRole))
			return false;

		// 下位フォルダ管理セキュリティ
		long lowFolderSecId = AppObjectUtil.getIntAttr(_sess, obj,
			EIMConfig.get("ATTR_NAME_FOLDER_LOW_FOLDER_SEC"), -1);
		if (lowFolderSecId > 0
				&& !AppSecurityUtils.authorizedLowFolderSecurity(_sess, obj, _sess.getUser(), EIMAccessRole.UPDATE))
			return false;

		return true;
	}

	/**
	 * 引数オブジェクトを「ドキュメント」リレーションで上方にたどり、親階層のオブジェクトを返します。 <br>
	 * 親はフォルダ・ワークスペース・ゴミ箱・マイドキュメントのどれかです。
	 *
	 * @param obj
	 * @return 親階層のオブジェクト
	 * @throws Exception
	 */
	public EIMObject getUpperObject(EIMObject obj) throws Exception {
		List rels;
		if(isTypeOfWorkspaceRecycle(obj.getType())){
			rels = RelationUtils.getParentRelationListByRelType(_sess, obj,
					getRelationTypeOfRecycle(),EIMAccessRole.READ);
		} else {
			
			rels = RelationUtils.getParentRelationListByRelType(_sess, obj,
					getRelationTypeOfDocument(),EIMAccessRole.READ);
		}
		
		// ドキュメントリレーションによるフォルダツリーは親が1つ、が前提
		return (rels.size() > 0) ? ((EIMRelation) rels.get(0)).getParent() : null;
	}

	/**
	 * 引数オブジェクトの親階層にあるオブジェクトの全てを返します。
	 *
	 * @param obj オブジェクト
	 * @return 親階層にあるオブジェクトのリスト。引数オブジェクトの直上が[0]に、最上位親が末端に入ります。
	 * @throws Exception
	 */
	public List getUpperObjectsToToplevel(EIMObject obj) throws Exception {
		Long key = new Long(obj.getId());
		List uppers = (List) _upperFoldersMap.get(key);
		if (uppers != null)
			return uppers;

		uppers = new ArrayList();
		List rels;
		if(isTypeOfWorkspaceRecycle(obj.getType())){
			rels = RelationUtils.getParentRelationListByRelType(_sess, obj,
					getRelationTypeOfRecycle(),EIMAccessRole.READ);
		} else {
			rels = RelationUtils.getParentRelationListByRelType(_sess, obj,
					getRelationTypeOfDocument(),EIMAccessRole.READ);
		}

		if (rels.size() > 0) {
			obj = ((EIMRelation) rels.get(0)).getParent();
			uppers.add(obj);// ドキュメントリレーションによるフォルダツリーは親が1つ、が前提
			uppers.addAll(getUpperObjectsToToplevel(obj));
		}
		_upperFoldersMap.put(key, uppers);
		return uppers;
	}

	/**
	 * 指定オブジェクトタイプがドキュメント系タイプかどうかを返します
	 *
	 * @param objType オブジェクトタイプ
	 * @return ドキュメント系タイプならtrue
	 * @throws Exception
	 */
	public boolean isTypeOfDocument(EIMObjectType objType) throws Exception {
		return getDocumentTypeIdMap().containsKey(new Long(objType.getId()));
	}

	/**
	 * 指定オブジェクトタイプがフォルダ系タイプかどうかを返します
	 *
	 * @param objType オブジェクトタイプ
	 * @return フォルダ系タイプならtrue
	 * @throws Exception
	 */
	public boolean isTypeOfFolder(EIMObjectType objType) throws Exception {
		return getFolderTypeIdMap().containsKey(new Long(objType.getId()));
	}

	/**
	 * 指定オブジェクトタイプがタグ系タイプかどうかを返します
	 *
	 * @param objType オブジェクトタイプ
	 * @return タグ系タイプならtrue
	 * @throws Exception
	 */
	public boolean isTypeOfTag(EIMObjectType objType) throws Exception {
		return getTagTypeIdMap().containsKey(new Long(objType.getId()));
	}

	/**
	 * 指定オブジェクトタイプがワークスペースタイプかどうかを返します
	 *
	 * @param objType オブジェクトタイプ
	 * @return ワークスペースタイプならtrue
	 * @throws Exception
	 */
	public boolean isTypeOfWorkspace(EIMObjectType objType) throws Exception {
		return objType.getId() == getTypeOfWorkspace().getId();
	}

	/**
	 * 指定オブジェクトタイプがワークスペースタイプまたはフォルダタイプかどうかを返します
	 *
	 * @param objType オブジェクトタイプ
	 * @return ワークスペースタイプまたはフォルダ対応ならtrue
	 * @throws Exception
	 */
	public boolean isTypeOfWorkspaceOrFolder(EIMObjectType objType) throws Exception {
		return isTypeOfWorkspace(objType) || isTypeOfFolder(objType);
	}

	/**
	 * ワークスペースタイプを返します
	 *
	 * @return ワークスペースタイプ
	 * @throws Exception
	 */
	public EIMObjectType getTypeOfWorkspace() throws Exception {
		if (_typeOfWorkspace == null) {
			_typeOfWorkspace = ObjectUtils.getObjectTypeByName(_sess,
				EIMConfig.get("OBJECT_TYPE_NAME_WORKSPACE"));
		}
		return _typeOfWorkspace;
	}

	/**
	 * 指定オブジェクトタイプがごみ箱タイプかどうかを返します
	 *
	 * @param objType オブジェクトタイプ
	 * @return ごみ箱タイプならtrue
	 * @throws Exception
	 */
	public boolean isTypeOfRecycle(EIMObjectType objType) throws Exception {
		return objType.getId() == getTypeOfRecycle().getId();
	}

	/**
	 * ごみ箱タイプを返します
	 *
	 * @return ごみ箱タイプ
	 * @throws Exception
	 */
	public EIMObjectType getTypeOfRecycle() throws Exception {
		if (_typeOfRecycle == null) {
			_typeOfRecycle = ObjectUtils.getObjectTypeByName(_sess,
				EIMConfig.get("OBJECT_TYPE_NAME_RECYCLE"));
		}
		return _typeOfRecycle;
	}

	/**
	 * 指定オブジェクトタイプがワークスペース固有ごみ箱タイプかどうかを返します
	 *
	 * @param objType オブジェクトタイプ
	 * @return ワークスペース固有ごみ箱タイプならtrue
	 * @throws Exception
	 */
	public boolean isTypeOfWorkspaceRecycle(EIMObjectType objType) throws Exception {
		return objType.getId() == getTypeOfWorkspaceRecycle().getId();
	}

	/**
	 * ワークスペース固有ごみ箱タイプを返します
	 *
	 * @return ワークスペース固有ごみ箱タイプ
	 * @throws Exception
	 */
	public EIMObjectType getTypeOfWorkspaceRecycle() throws Exception {
		if (_typeOfWSRecycle == null) {
			_typeOfWSRecycle = ObjectUtils.getObjectTypeByName(_sess,
				EIMConfig.get("OBJECT_TYPE_NAME_WORKSPACERECYCLE"));
		}
		return _typeOfWSRecycle;
	}


	/**
	 * 指定オブジェクトタイプがマイドキュメントタイプかどうかを返します
	 *
	 * @param objType オブジェクトタイプ
	 * @return マイドキュメントタイプならtrue
	 * @throws Exception
	 */
	public boolean isTypeOfMyDocument(EIMObjectType objType) throws Exception {
		return objType.getId() == getTypeOfMyDocument().getId();
	}

	/**
	 * マイドキュメントタイプを返します
	 *
	 * @return マイドキュメントタイプ
	 * @throws Exception
	 */
	public EIMObjectType getTypeOfMyDocument() throws Exception {
		if (_typeOfMyDocument == null) {
			_typeOfMyDocument = ObjectUtils.getObjectTypeByName(_sess,
				EIMConfig.get("OBJECT_TYPE_NAME_MYDOCUMENT"));
		}
		return _typeOfMyDocument;
	}

	/**
	 * フォルダタイプを返します
	 *
	 * @return フォルダタイプ
	 * @throws Exception
	 */
	public EIMObjectType getTypeOfFolder() throws Exception {
		if (_typeOfFolder == null) {
			_typeOfFolder = ObjectUtils.getObjectTypeByName(_sess,
				EIMConfig.get("OBJECT_TYPE_NAME_FOLDER"));
		}
		return _typeOfFolder;
	}

	/**
	 * ドキュメントリレーションタイプを返します
	 *
	 * @return ドキュメントリレーションタイプ
	 * @throws Exception
	 */
	public EIMRelationType getRelationTypeOfDocument() throws Exception {
		if (_reltypeOfDocument == null) {
			_reltypeOfDocument = RelationUtils.getRelationTypeByName(_sess,
				EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));
		}
		return _reltypeOfDocument;
	}

	/**
	 * ブランチリレーションタイプを返します
	 * 
	 * @return ドキュメントリレーションタイプ
	 * @throws Exception
	 */
	public EIMRelationType getRelationTypeOfBranch() throws Exception {
		if (_reltypeOfBranch == null) {
			_reltypeOfBranch = RelationUtils.getRelationTypeByName(_sess,
				EIMConfig.get("RELATION_TYPE_NAME_BRANCH"));
		}
		return _reltypeOfBranch;
	}

	/**
	 * (ドキュメント)リンクリレーションタイプを返します
	 *
	 * @return (ドキュメント)リンクリレーションタイプ
	 * @throws Exception
	 */
	public EIMRelationType getRelationTypeOfDocLink() throws Exception {
		if (_reltypeOfDocLink == null) {
			_reltypeOfDocLink = RelationUtils.getRelationTypeByName(_sess,
				EIMConfig.get("RELATION_TYPE_NAME_LINK"));
		}
		return _reltypeOfDocLink;
	}
	/**
	 * ごみ箱リレーションタイプを返します
	 *
	 * @return ごみ箱リレーションタイプ
	 * @throws Exception
	 */
	public EIMRelationType getRelationTypeOfRecycle() throws Exception {
		if (_reltypeOfRecycle == null) {
			_reltypeOfRecycle = RelationUtils.getRelationTypeByName(_sess,
				EIMConfig.get("RELATION_TYPE_NAME_RECYCLE"));
		}
		return _reltypeOfRecycle;
	}

	/**
	 * 引数オブジェクトタイプが、フォルダツリー上最上位階層を受け持つタイプかどうかを返します
	 *
	 * @param objType 対象オブジェクトタイプ
	 * @return フォルダツリー上最上位階層を受け持つタイプならtrue
	 * @throws Exception
	 */
	public boolean isTypeOfToplevelTypes(EIMObjectType objType) throws Exception {
		long typeId = objType.getId();
		return (typeId == getTypeOfWorkspace().getId())//
				|| (typeId == getTypeOfRecycle().getId())//
				|| (typeId == getTypeOfMyDocument().getId());
	}

	/**
	 * ドキュメント系タイプのマップを返します
	 *
	 * @return ドキュメント系タイプのマップ
	 * @throws Exception
	 */
	private Map getDocumentTypeIdMap() throws Exception {
		if (_docTypeIdMap == null) {
			_docTypeIdMap = AppObjectTypeUtil.getObjTypeMap(_sess,
				EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT"));
		}
		return _docTypeIdMap;
	}

	/**
	 * フォルダ系タイプのマップを返します
	 *
	 * @return フォルダ系タイプのマップ
	 * @throws Exception
	 */
	private Map getFolderTypeIdMap() throws Exception {
		if (_folderTypeIdMap == null) {
			_folderTypeIdMap = AppObjectTypeUtil.getObjTypeMap(_sess,
				EIMConfig.get("OBJECT_TYPE_NAME_FOLDER"));
		}
		return _folderTypeIdMap;
	}

	/**
	 * タグ系タイプのマップを返します
	 *
	 * @return タグ系タイプのマップ
	 * @throws Exception
	 */
	private Map getTagTypeIdMap() throws Exception {
		if (_tagTypeIdMap == null) {
			_tagTypeIdMap = AppObjectTypeUtil.getObjTypeMap(_sess,
				EIMConfig.get("OBJECT_TYPE_NAME_TAG"));
		}
		return _tagTypeIdMap;
	}

	/**
	 * 指定オブジェクトに対し、ログインユーザーが公開読取権「しか」持たないかどうかを返します
	 *
	 * @param obj 対象オブジェクト
	 * @return 指定オブジェクトに対し、ログインユーザーが公開読取権「しか」持たないならtrue
	 * @throws Exception
	 */
	public boolean isReadOnlyAccess(EIMObject obj) throws Exception {
		EIMUser user = _sess.getUser();
		boolean authorizedFlag = false;
		if(SecurityUtils.authorized(_sess, obj, user, EIMAccessRole.READ)
				&& !SecurityUtils.authorized(_sess, obj, _sess.getUser(), AppConstant.ACCESS_ROLE_ALWAYS_READ)){
			authorizedFlag = true;
		}
		return authorizedFlag;
	}

	/**
	 * ログインユーザーが公開読取権限しか持たないセキュリティのマップを返します。
	 *
	 * @return ログインユーザーが公開読取権限しか持たないセキュリティのマップ
	 * @throws Exception
	 */
	public Map getReadOnlySecurities() throws Exception {
		if (_readOnlySecIdMap != null)
			return _readOnlySecIdMap;

		List readOnlySecs = SecurityUtils.getReadOnlySecurityList(_sess);
		_readOnlySecIdMap = new HashMap();
		for (Iterator i = readOnlySecs.iterator(); i.hasNext();) {
			EIMSecurity sec = (EIMSecurity) i.next();
			_readOnlySecIdMap.put(new Long(sec.getId()), sec);
		}
		return _readOnlySecIdMap;
	}

	/**
	 * 指定オブジェクトの下位にあり、ログインユーザーがアクセス可能なオブジェクトのリストを返します。
	 *
	 * @param parentObj 対象オブジェクト
	 * @param doReturnOnlyFolderType フォルダだけに絞るかどうか。trueならフォルダだけを返却します
	 * @return オブジェクトのリスト
	 * @throws Exception
	 */
	public List getChildObjectsInAccessibleStatus(EIMObject parentObj,
			boolean doReturnOnlyFolderType) throws Exception {

		List rels = RelationUtils.getChildRelationListByRelType(_sess, parentObj,
			getRelationTypeOfDocument(),EIMAccessRole.READ);

		// アクセス権限有りのEIMObjectリスト取得
		List objectList = getAuthorizedAccessEIMObjectList(rels);

		List<EIMObject> objs = new ArrayList<EIMObject>();
		for (Iterator i = objectList.iterator(); i.hasNext();) {
			EIMObject obj = (EIMObject) i.next();
			if (!doReturnOnlyFolderType || isTypeOfFolder(obj.getType()))
				objs.add(obj);
		}
		return objs;
	}

	/**
	 * 指定オブジェクトの下位にあるオブジェクトのリストを、アクセス権限に関係なく返します。
	 * loadメソッドによって事前にメモリ上にリレーションをロードすることができます。
	 * ロード済のリレーション一覧が存在する場合は、DBアクセスは行わずそれを返却します。
	 *
	 * @param parentObj 対象オブジェクト
	 * @param doReturnOnlyFolderType フォルダだけに絞るかどうか。trueならフォルダだけを返却します
	 * @return オブジェクトのリスト
	 * @throws Exception
	 */
	public List getChildObjectsWithoutAnyConditions(EIMObject parentObj,
			boolean doReturnOnlyFolderType) throws Exception {
		List<EIMRelation> rels = getChildRelationListByRelType(parentObj, getRelationTypeOfDocument(), EIMAccessRole.NONE);

		// アクセス権限有りのEIMObjectリスト取得
		List objectList = getAuthorizedAccessEIMObjectList(rels);

		List<EIMObject> objs = new ArrayList<EIMObject>();
		for (Iterator i = objectList.iterator(); i.hasNext();) {
			EIMObject obj = (EIMObject) i.next();
			if (!doReturnOnlyFolderType || isTypeOfFolder(obj.getType()))
				objs.add(obj);
		}
		return objs;
	}

	/**
	 * 指定オブジェクトの下位にあり、ログインユーザーがアクセス可能なフォルダとタグのリストを返します。
	 *
	 * <li>第1ソートは「タグ、フォルダ、ドキュメント」の順、第2ソートは「オブジェクト名」昇順、第3ソートは「オブジェクトID」昇順とします。
	 *
	 * @param parentObj 対象オブジェクト
	 * @param doReturnTag タグタイプを取得するか否か
	 * @param doReturnFolder フォルダタイプを取得するか否か
	 * @param doReturnDocument ドキュメントタイプを取得するか否か
	 * @return オブジェクトのリスト
	 * @throws Exception
	 */
	public List getChildFolderTagsInAccessibleStatus(EIMObject parentObj,
			boolean doReturnTag, boolean doReturnFolder, boolean doReturnDocument) throws Exception {

		List tagList = new ArrayList();
		List folList = new ArrayList();
		List docList = new ArrayList();

		List rels = RelationUtils.getChildRelationListByRelType(_sess, parentObj, getRelationTypeOfDocument(),EIMAccessRole.READ);

		if (rels != null) {

			for (Iterator iter = rels.iterator(); iter.hasNext();) {
				EIMObject obj = ((EIMRelation) iter.next()).getChild();

				// オブジェクトタイプ毎に仕分け
				if (doReturnTag && isTypeOfTag(obj.getType())) {
					tagList.add(obj);
				} else if (doReturnFolder && isTypeOfFolder(obj.getType())) {
					folList.add(obj);
				} else if (doReturnDocument && isTypeOfDocument(obj.getType())) {
					docList.add(obj);
				}
			}

			// 各リストをオブジェクト名昇順でソート (オブジェクトIDのソートは取得時に済んでいるはず)
			if (tagList.size() > 1) {
				tagList = AppObjectUtil.getStrSortedList(tagList, "getName", true);
			}
			if (folList.size() > 1) {
				folList = AppObjectUtil.getStrSortedList(folList, "getName", true);
			}
			if (docList.size() > 1) {
				docList = AppObjectUtil.getStrSortedList(docList, "getName", true);
			}
			// 各リストを連結
			tagList.addAll(folList);
			tagList.addAll(docList);
		}
		return tagList;
	}

	/**
	 * 指定オブジェクトの下位にあり、ログインユーザーがアクセス可能な「ドキュメントリンクを含む」オブジェクトのリストを返します。
	 * (上限指定なし）
	 * @param parentObj 対象オブジェクト
	 * @param isDocumentLinkList EIMObjectをKey・ドキュメントリンクかどうかを示す値("true"/"false":String)をValueとするHashMap
	 * @return オブジェクトのリスト
	 * @throws Exception
	 */
	public List getChildObjectsWithDocLinkInAccessibleStatus(EIMObject parentObj, HashMap isDocumentLinkMap) throws Exception {
		return getChildObjectsWithDocLinkInAccessibleStatus(parentObj, isDocumentLinkMap, null);
	}

	/**
	 * 指定オブジェクトの下位にあり、ログインユーザーがアクセス可能な「ドキュメントリンクを含む」オブジェクトのリストを返します。
	 * (上限指定あり）
	 * @param parentObj 対象オブジェクト
	 * @param isDocumentLinkList EIMObjectをKey・ドキュメントリンクかどうかを示す値("true"/"false":String)をValueとするHashMap
	 * 	@param  limit 検索上限
	 * @return オブジェクトのリスト
	 * @throws Exception
	 */
	public List getChildObjectsWithDocLinkInAccessibleStatus(EIMObject parentObj, HashMap isDocumentLinkMap, EIMSearchLimitCountCondition limitCond) throws Exception {


		if( isDocumentLinkMap != null)
			isDocumentLinkMap.clear();

		List<EIMObject> objs = new ArrayList<EIMObject>();

		// 配下ドキュメント＆フォルダの獲得

		EIMRelationType docRelationType = getRelationTypeOfDocument();
		EIMRelationType linkRelationType = getRelationTypeOfDocLink();

		EIMSearchSelectEIMRelation
		relationSelectTarget = new EIMSearchSelectEIMRelation();
		EIMSearchSelectEIMRelation.SearchConditionBuildHelper
			h = new EIMSearchSelectEIMRelation.SearchConditionBuildHelper();

		relationSelectTarget.setSourceObjects(
				EIMSearchSelectEIMRelation.SourceObjects.CHILD);

		relationSelectTarget.setCondition(h.group(h.opAnd())
				.addCondition(
						h.eq(h.opAnd(),
								EIMSearchSelectEIMRelation.PsedoAttributeTypeEnum.PARENT,
								parentObj.getId())
				)
				.addCondition(h.group(h.opAnd())
						.addCondition(
							h.eq(h.opAnd(),
									EIMSearchSelectEIMRelation.PsedoAttributeTypeEnum.TYPE,
									docRelationType.getId())
						)
						.addCondition(
							h.eq(h.opOr(),
									EIMSearchSelectEIMRelation.PsedoAttributeTypeEnum.TYPE,
									linkRelationType.getId())
						)
				)
		);

		EIMSearchSelectEIMObject
			parentSelectTarget = new EIMSearchSelectEIMObject();
		parentSelectTarget.setRole(EIMAccessRole.NONE);

		EIMSearchSelectEIMObject
			childSelectTarget = new EIMSearchSelectEIMObject();
		if(EIMAccessRole.READ != EIMAccessRole.OLD_AUTHORITY_CHECK)
			childSelectTarget.setRole(EIMAccessRole.READ);

		List rels = SearchUtils.searchRelations(_sess,
					relationSelectTarget, parentSelectTarget, childSelectTarget, limitCond);

		// アクセス権限有りのEIMObjectリスト取得

		// 取得したリレーションを「ドキュメント」と「リンク」に分ける
		List<EIMRelation> docRelationList = new ArrayList<EIMRelation>();
		List<EIMRelation> linkRelationList = new ArrayList<EIMRelation>();
		for(int ii = 0; ii < rels.size(); ii++) {
			EIMRelation r = (EIMRelation)rels.get(ii);
			String relName = r.getType().getDefName();
			if(relName.equals(docRelationType.getDefName())) {
				docRelationList.add(r);
			}
			else if(relName.equals(linkRelationType.getDefName())) {
				linkRelationList.add(r);
			}
		}

		List docList = getAuthorizedAccessEIMObjectList(docRelationList);
		for (Iterator i = docList.iterator(); i.hasNext();) {
		EIMObject obj = (EIMObject) i.next();
			objs.add(obj);
			if( isDocumentLinkMap != null )
				isDocumentLinkMap.put(obj, "false");	// HashMapの作成
		}

		List linkList = getAuthorizedAccessEIMObjectList(linkRelationList);
		for (Iterator i = linkList.iterator(); i.hasNext();) {
			EIMObject obj = (EIMObject) i.next();
			objs.add(obj);
			if( isDocumentLinkMap != null )
				isDocumentLinkMap.put(obj, "true");	// HashMapの作成
		}

		// オブジェクト名称でソートする
		Collections.sort(objs, new Comparator(){
			public int compare(Object obj1, Object obj2){
			        EIMObject ent1 =(EIMObject)obj1;
			        EIMObject ent2 =(EIMObject)obj2;
			        String val1 = (String) ent1.getName();
			        String val2 = (String) ent2.getName();
			        return val1.compareTo(val2);
			}
		});

		return objs;
	}

	/**
	 * 指定オブジェクトの下位にあり、ログインユーザーがアクセス可能な「ドキュメントリンクを含む」オブジェクトのリストを返します。
	 *
	 * @param parentObj 対象オブジェクト
	 * @param isDocumentLinkList EIMObjectをKey・ドキュメントリンクかどうかを示す値("true"/"false":String)をValueとするHashMap
	 * @return オブジェクトのリスト
	 * @throws Exception
	 */
	public List getChildObjectsWithDocLinkInAccessibleStatusByCondMaker(EIMObject parentObj, HashMap isDocumentLinkMap, EIMDocSearchType type) throws Exception {
		return getChildObjectsWithDocLinkInAccessibleStatusByCondMakerInternal(parentObj, isDocumentLinkMap, type, null);
	}

	/**
	 * 指定オブジェクトの下位にあり、ログインユーザーがアクセス可能な「ドキュメントリンクを含む」オブジェクトのリストを返します。
	 *
	 * @param parentObj 対象オブジェクト
	 * @param isDocumentLinkList EIMObjectをKey・ドキュメントリンクかどうかを示す値("true"/"false":String)をValueとするHashMap
	 * @param linkUpdateTimingMap オブジェクトIDをKey、リンク更新タイミングの値をValueとするHashMap
	 * @return オブジェクトのリスト
	 * @throws Exception
	 */
	public List getChildObjectsWithDocLinkInAccessibleStatusByCondMaker(
			EIMObject parentObj, HashMap isDocumentLinkMap, EIMDocSearchType type,
			HashMap<Long, Long> linkUpdateTimingMap) throws Exception {

		return getChildObjectsWithDocLinkInAccessibleStatusByCondMakerInternal(parentObj, isDocumentLinkMap, type, linkUpdateTimingMap);

	}

	/**
	 * 指定オブジェクトの下位にあり、ログインユーザーがアクセス可能な「ドキュメントリンクを含む」オブジェクトのリストを返します。
	 *
	 * @param parentObj 対象オブジェクト
	 * @param isDocumentLinkList EIMObjectをKey・ドキュメントリンクかどうかを示す値("true"/"false":String)をValueとするHashMap
	 * @param linkUpdateTimingMap オブジェクトIDをKey、リンク更新タイミングの値をValueとするHashMap
	 * @return オブジェクトのリスト
	 * @throws Exception
	 */
	private List getChildObjectsWithDocLinkInAccessibleStatusByCondMakerInternal(
			EIMObject parentObj, HashMap isDocumentLinkMap, EIMDocSearchType type,
			HashMap<Long, Long> linkUpdateTimingMap) throws Exception {

		if( isDocumentLinkMap != null)
			isDocumentLinkMap.clear();

		if( linkUpdateTimingMap != null)
			linkUpdateTimingMap.clear();

		List<EIMObject> objs = new ArrayList<EIMObject>();

		// 配下ドキュメント＆フォルダ＆ワークスペース固有ごみ箱の獲得

		EIMRelationType docRelationType = getRelationTypeOfDocument();
		EIMRelationType linkRelationType = getRelationTypeOfDocLink();
		EIMRelationType recycleRelationType = getRelationTypeOfRecycle();

		int accessRoleChild;
		if(EIMAccessRole.READ != EIMAccessRole.OLD_AUTHORITY_CHECK)
			accessRoleChild = EIMAccessRole.READ;

		List rels = AppSearchUtils.searchRelationByConditionMaker(_sess, type, EIMAccessRole.NONE, accessRoleChild, null, parentObj);

		// アクセス権限有りのEIMObjectリスト取得

		// 取得したリレーションを「ドキュメント」と「リンク」と「ごみ箱」に分ける
		List<EIMRelation> docRelationList = new ArrayList<EIMRelation>();
		List<EIMRelation> linkRelationList = new ArrayList<EIMRelation>();
		List<EIMRelation> recycleRelationList = new ArrayList<EIMRelation>();
		for(int ii = 0; ii < rels.size(); ii++) {
			EIMRelation r = (EIMRelation)rels.get(ii);
			String relName = r.getType().getDefName();
			if(relName.equals(docRelationType.getDefName())) {
				docRelationList.add(r);
			}
			else if(relName.equals(linkRelationType.getDefName())) {
				linkRelationList.add(r);
				if( linkUpdateTimingMap != null )
					linkUpdateTimingMap.put(new Long(r.getChild().getId()), new Long(r.getAttributeByName(EIMConfig.get("ATTR_NAME_LINK_UPDATE_TIMING")).getInt()));
			}
			else if(relName.equals(recycleRelationType.getDefName())) {
				recycleRelationList.add(r);
			}
		}

		List docList = getAuthorizedAccessEIMObjectList(docRelationList);
		for (Iterator i = docList.iterator(); i.hasNext();) {
		EIMObject obj = (EIMObject) i.next();
			objs.add(obj);
			if( isDocumentLinkMap != null )
				isDocumentLinkMap.put(obj, "false");	// HashMapの作成
		}

		List linkList = getAuthorizedAccessEIMObjectList(linkRelationList);
		for (Iterator i = linkList.iterator(); i.hasNext();) {
			EIMObject obj = (EIMObject) i.next();
			objs.add(obj);
			if( isDocumentLinkMap != null )
				isDocumentLinkMap.put(obj, "true");	// HashMapの作成
		}
		List recycleList = getAuthorizedAccessEIMObjectList(recycleRelationList);
		if(recycleList.size() > 0) {
			EIMObject obj = (EIMObject) recycleList.get(0);
			objs.add(obj);
			if( isDocumentLinkMap != null )
				isDocumentLinkMap.put(obj, "false");	// HashMapの作成
		}

		// オブジェクト名称でソートする
		Collections.sort(objs, new Comparator(){
			public int compare(Object obj1, Object obj2){
			        EIMObject ent1 =(EIMObject)obj1;
			        EIMObject ent2 =(EIMObject)obj2;
			        String val1 = (String) ent1.getName();
			        String val2 = (String) ent2.getName();
			        return val1.compareTo(val2);
			}
		});

		return objs;
	}

	/**
	 * 表示可能なオブジェクトリンクの親IDの一覧を返す。
	 *
	 * @param parents
	 * @param type
	 * @return
	 * @throws Exception
	 */
	public List getDocumentLinkRilationParentIdListByCondMaker(long[] parents, EIMDocSearchType type) throws Exception {

		List<EIMObject> objects = AppSearchUtils.searchObjectsByConditionMaker(_sess, type, EIMAccessRole.READ, null, parents);

		List<Long> result = new ArrayList<Long>();

		for (EIMObject obj : objects) {
			long id = obj.getId();
			result.add(id);
		}

		return result;
	}

	/**
	 * 指定オブジェクトの下位にあり、ログインユーザーがアクセス可能なタグのリストを返します。
	 *
	 * @param parentObj 対象オブジェクト
	 * @return オブジェクトのリスト
	 * @throws Exception
	 */
	public List getChildTagsInAccessibleStatus(EIMObject parentObj) throws Exception {
		List rels = RelationUtils.getChildRelationListByRelType(_sess, parentObj,
			getRelationTypeOfDocument(),EIMAccessRole.READ);

		// アクセス権限有りのEIMObjectリスト取得
		List objectList = getAuthorizedAccessEIMObjectList(rels);

		List<EIMObject> objs = new ArrayList<EIMObject>();
		for (Iterator i = objectList.iterator(); i.hasNext();) {
			EIMObject obj = (EIMObject) i.next();
			if (isTypeOfTag(obj.getType()))
				objs.add(obj);
		}
		return objs;
	}

	/**
	 * 指定オブジェクトの下位にあり、ログインユーザーがアクセス可能なフォルダ（およびタグ）のリストを返します
	 * @param parentObj 対象オブジェクト
	 * @param includeTag タグのリストを返却値に含めるかどうかのフラグ
	 * @return オブジェクトのリストを格納しているマップ（タグのキー："tags"、フォルダのキー："folders"）
	 * @throws Exception
	 */
	public Map<String, List> getChildTagsAndFoldersInAccessibleStatus(EIMObject parentObj,
			boolean includeTag) throws Exception
	{
		EIMRelationType docRelationType = getRelationTypeOfDocument();

		EIMSearchSelectEIMRelation
		relationSelectTarget = new EIMSearchSelectEIMRelation();
		EIMSearchSelectEIMRelation.SearchConditionBuildHelper
			h = new EIMSearchSelectEIMRelation.SearchConditionBuildHelper();

		relationSelectTarget.setSourceObjects(
				EIMSearchSelectEIMRelation.SourceObjects.CHILD);

		relationSelectTarget.setCondition(h.group(h.opAnd())
				.addCondition(
						h.eq(h.opAnd(),
								EIMSearchSelectEIMRelation.PsedoAttributeTypeEnum.PARENT,
								parentObj.getId())
				)
				.addCondition(
						h.eq(h.opAnd(),
								EIMSearchSelectEIMRelation.PsedoAttributeTypeEnum.TYPE,
								docRelationType.getId())
				)
		);

		EIMSearchSelectEIMObject
			parentSelectTarget = new EIMSearchSelectEIMObject();
		parentSelectTarget.setRole(EIMAccessRole.NONE);

		EIMSearchSelectEIMObject
			childSelectTarget = new EIMSearchSelectEIMObject();
		if(EIMAccessRole.READ != EIMAccessRole.OLD_AUTHORITY_CHECK)
			childSelectTarget.setRole(EIMAccessRole.READ);

		List rels = SearchUtils.searchRelations(_sess,
					relationSelectTarget, parentSelectTarget, childSelectTarget, null);

		// アクセス権限有りのEIMObjectリスト取得
		List objectList = getAuthorizedAccessEIMObjectList(rels);

		List<EIMObject> folderList = new ArrayList<EIMObject>();
		List<EIMObject> tagList = new ArrayList<EIMObject>();
		for (Iterator i = objectList.iterator(); i.hasNext();) {
			EIMObject obj = (EIMObject) i.next();
			if (includeTag && isTypeOfTag(obj.getType())) {
				tagList.add(obj);
			}
			else if(isTypeOfFolder(obj.getType())) {
				folderList.add(obj);
			}
		}

		Map<String, List> map = new HashMap<String, List>();
		map.put("tags", tagList);
		map.put("folders", folderList);

		return map;
	}

	/**
	 * 指定フォルダ配下のドキュメントにドキュメントリンクが存在するかどうかを再帰的にチェックします<br>
	 * loadメソッドによって事前にメモリ上にオブジェクト一覧とアクセス権限情報をロードすることができます。
	 * ロード済のオブジェクト一覧とアクセス権限情報が存在する場合は、DBアクセスは行わずそれを返却します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param object 対象オブジェクト(フォルダ)
	 * @return ドキュメントリンクが存在するドキュメントがあれがtrue、なければfalse
	 * @throws Exception
	 */
	public boolean isExistDocumentWithLinkUnderFolder(EIMSession sess, EIMObject object) throws Exception {

		boolean bRet = false;

		//Relation Type
		EIMRelationType relType = getRelationTypeOfDocument();
		//Child Relation
		List<EIMRelation> childRelList = getChildRelationListByRelType(object, relType, EIMAccessRole.READ);

		for(int i = 0; i < childRelList.size(); i++)
		{
			//Relation
			EIMRelation relation = (EIMRelation)childRelList.get(i);

			//Child Object
			EIMObject childObj = relation.getChild();

			// オブジェクトタイプがフォルダなら再起呼び出し
			if( isTypeOfFolder( childObj.getType()) ) {
				if( isExistDocumentWithLinkUnderFolder( sess, childObj ) )
					bRet = true;
			}
			else {
				// 「ドキュメントリンク」属性の取得
				if( AppObjectUtil.getIntAttr(sess, childObj, EIMConfig.get("ATTR_NAME_DOCUMENT_DOCUMENT_LINK"), 0) == 1 )
					bRet = true;
				else {
					// 過去バージョンについてもドキュメントリンク有無のチェック
					EIMVersion version = getVersion(childObj);
					for (Iterator j = version.getList().iterator(); j.hasNext();) {
						EIMObject pastObj = (EIMObject) j.next();
						if (pastObj.getId() == childObj.getId())
							continue;	// チェック済み
						if( AppObjectUtil.getIntAttr(sess, pastObj, EIMConfig.get("ATTR_NAME_DOCUMENT_DOCUMENT_LINK"), 0) == 1 ) {
							bRet = true;
							break;
						}
					}
				}
			}
			if( bRet )
				break;
		}
		return bRet;
	}

	/**
	 * フォルダタイプ名のXMLエスケープ済み文字列を返します
	 *
	 * @return フォルダタイプ名のXMLエスケープ済み文字列
	 */
	public String getObjTypeNameFolderXmlEscaped() {
		return OBJTYPENAM_FOLDER_XML_ESCAPED;
	}

	/**
	 * タグタイプ名のXMLエスケープ済み文字列を返します
	 *
	 * @return タグタイプ名のXMLエスケープ済み文字列
	 */
	public String getObjTypeNameTagXmlEscaped() {
		return OBJTYPENAM_TAG_XML_ESCAPED;
	}

	/**
	 * ワークスペース固有ごみ箱タイプ名のXMLエスケープ済み文字列を返します
	 *
	 * @return ワークスペース固有ごみ箱タイプ名のXMLエスケープ済み文字列
	 */
	public String getObjTypeNameWorkspaceRecycleXmlEscaped() {
		return OBJTYPENAM_WORKSPACERECYCLE_XML_ESCAPED;
	}

	/**
	 * 上位WFフォルダ属性のデフォルト名称を返します
	 *
	 * @return 上位WFフォルダ属性のデフォルト名称
	 */
	public String getAttrNameDocumentHigherWFFolder() {
		return ATTR_NAME_OF_HIGHER_WF_FOLDER;
	}

	/**
	 * PDF変換失敗属性のデフォルト名称を返します
	 *
	 * @return PDF変換失敗属性のデフォルト名称
	 */
	public String getAttrNameOfPubProcFail() {
		return ATTR_NAME_OF_PUB_PROC_FAIL;
	}

	/**
	 * PDF分割状態属性のデフォルト名称を返します
	 *
	 * @return PDF分割状態属性のデフォルト名称
	 */
	public String getAttrNameOfPdfDivideStatus() {
		return ATTR_NAME_OF_PDF_DIVIDE_STATUS;
	}

	/**
	 * PDF結合処理失敗属性のデフォルト名称を返します
	 *
	 * @return PDF結合処理失敗属性のデフォルト名称
	 */
	public String getAttrNameOfPDFJoinFail() {
		return ATTR_NAME_OF_PDF_JOIN_FAIL;
	}

	/**
	 * 名称割当属性のデフォルト名称を返します
	 *
	 * @return 名称割当属性のデフォルト名称
	 */
	public String getAttrNameOfNameAttr() {
		return ATTR_NAME_OF_NAME_ATTR;
	}

	/**
	 * パス属性のデフォルト名称を返します
	 *
	 * @return パス属性のデフォルト名称
	 */
	public String getAttrNameOfPath() {
		return ATTR_NAME_OF_PATH;
	}

	/**
	 * 下位引き継ぎ属性のデフォルト名称を返します
	 *
	 * @return パス属性のデフォルト名称
	 */
	public String getAttrNameOfToLowAttr() {
		return ATTR_NAME_OF_TO_LOW_ATTR;
	}

	/**
	 * 有効期限属性のデフォルト名称を返します
	 *
	 * @return 有効期限属性のデフォルト名称
	 */
	public String getAttrNameOfEffectDate() {
		return ATTR_NAME_OF_EFFECT_DATE;
	}

	/**
	 * 上位WFフォルダ属性タイプを返します
	 *
	 * @return 上位WFフォルダ属性タイプ
	 * @throws Exception
	 */
	public EIMAttributeType getAttrTypeOfHigherWFFolder() throws Exception {
		return getAttrType(getAttrNameDocumentHigherWFFolder());
	}

	/**
	 * 上位からの引継ぎ属性のデフォルト名称を返します
	 *
	 * @return 上位からの引継ぎ属性のデフォルト名称
	 */
	public String getAttrNameOfFromHighAttr() {
		return ATTR_NAME_OF_FROM_HIGH_ATTR;
	}

	/**
	 * 改訂内容のデフォルト名称を返します
	 *
	 * @return 改訂内容のデフォルト名称
	 */
	public String getAttrNameOfUpdateRevComment() {
		return ATTR_NAME_OF_DOCUMENT_REV_CONTENT;
	}

	/**
	 * 上位からの引継ぎ属性タイプを返します
	 *
	 * @return 上位からの引継ぎ属性タイプ
	 * @throws Exception
	 */
	public EIMAttributeType getAttrTypeOfFromHighAttr() throws Exception {
		return getAttrType(getAttrNameOfFromHighAttr());
	}

	/**
	 * 署名・暗号化状態のデフォルト名称を返します
	 *
	 * @return 署名・暗号化状態のデフォルト名称
	 */
	public String getAttrNameOfSignEncStatus() {
		return ATTR_NAME_OF_SIGN_ENC_STATUS;
	}

	/**
	 * このインスタンスが持つセッションを返します
	 *
	 * @return セッション
	 */
	public EIMSession getSession() {
		return _sess;
	}

	/**
	 * 指定オブジェクトがドキュメントかどうかを返す
	 * @param parentObj 親オブジェクト
	 * @param targetObj 指定オブジェクト
	 */
	public boolean isDocument(EIMObject parentObj, EIMObject targetObj) throws Exception
	{
		List rels = RelationUtils.getChildRelationListByRelType(_sess, parentObj, getRelationTypeOfDocument(),EIMAccessRole.READ);
		for(int ii = 0; ii < rels.size(); ii++) {
			EIMObject obj = ((EIMRelation)rels.get(ii)).getChild();
			if(targetObj.getName().equals(obj.getName())) {
				return true;
			}
		}
		return false;
	}

	/**
	 * 指定オブジェクトがドキュメントリンクかどうかを返す
	 * @param parentObj 親オブジェクト
	 * @param targetObj 指定オブジェクト
	 */
	public boolean isDocumentLink(EIMObject parentObj, EIMObject targetObj) throws Exception
	{
		List rels = RelationUtils.getChildRelationListByRelType(_sess, parentObj, getRelationTypeOfDocLink(),EIMAccessRole.READ);
		for(int ii = 0; ii < rels.size(); ii++) {
			EIMObject obj = ((EIMRelation)rels.get(ii)).getChild();
			if(targetObj.getName().equals(obj.getName())) {
				return true;
			}
		}
		return false;
	}

	/**
	 * アクセス権有りのEIMObjectリスト取得
	 *
	 * ※｢アクセス権有り｣の条件は以下のいずれかの条件を満たした場合とする。
	 * 　・WFなし(ステータスタイプがNULL)
	 * 　・｢アクセス権限が公開読取のみ｣以外
	 * 　・上記以外で、ステータスタイプが｢公開済｣の場合
	 *
	 * @param rels EIMRelation格納領域
	 * @return
	 * @throws Exception
	 */
	private List getAuthorizedAccessEIMObjectList(List rels) throws Exception {

		List<EIMObject> notReadOnryEIMObjectList = new ArrayList<EIMObject>();
		if(rels == null || rels.size() == 0){
			return notReadOnryEIMObjectList;
		}

		//===============================================================
		// リレーションリストからEIMObject配列を取得
		//===============================================================
		EIMObject[] objects = getEIMObjects(rels);

		//===============================================================
		// アクセス権有りのEIMObject配列を取得
		//===============================================================
		EIMUser user = _sess.getUser();
		// アクセス権限｢公開読取｣のEIMObjectリストを取得
		List readEIMObjectList = getAuthorizedObjects(_sess, objects, user, EIMAccessRole.READ);

		// アクセス権限｢常時読取｣のEIMObjectリストを取得
		List readAlwaysEIMObjectList = getAuthorizedObjects(_sess, objects, user, AppConstant.ACCESS_ROLE_ALWAYS_READ);

		if(readEIMObjectList == null){
			return notReadOnryEIMObjectList;
		}

		// アクセス権限有りのEIMObjectリストを作成する
		for (Iterator readItr = readEIMObjectList.iterator(); readItr.hasNext();) {
			boolean accessFlg = false;		// アクセス権判定フラグ
			EIMObject readObj = (EIMObject)readItr.next();

			// WFなしの場合は常にアクセス権有り。
			if (readObj.getStatus() == null) {
				notReadOnryEIMObjectList.add(readObj);
				continue;
			}

			// 参照権限(アクセス権｢公開読取｣のみ)判定
			// ⇒ 参照権限以外はアクセス権有り
			if(readAlwaysEIMObjectList != null){
				for (Iterator readAlItr = readAlwaysEIMObjectList.iterator(); readAlItr.hasNext();) {
					EIMObject readAlObj = (EIMObject)readAlItr.next();
					if(readObj.getId() == readAlObj.getId()){
						accessFlg = true;
						break;
					}
				}
			}
			if(accessFlg){
				notReadOnryEIMObjectList.add(readObj);
				continue;
			}

			// (WFあり)(参照権限のみのユーザー)の場合、
			// ドキュメントもフォルダも「公開済」の場合はアクセス権有り。
			if (readObj.getStatus().getType().getKind() == AppConstant.STATUS_TYPE_KIND_ID_PUBLIC) {
				notReadOnryEIMObjectList.add(readObj);
				continue;
			}

			// 重複しない場合はアクセス権限が｢公開読取｣のみなので何もしない
		}
		return notReadOnryEIMObjectList;
	}

	/**
	 * EIMRelationリスト(List)からEIMObjectを取り出し、EIMObject配列を取得する。
	 *
	 * @param rels EIMRelation格納領域
	 * @return EIMObject
	 * @throws Exception
	 */
	private EIMObject[] getEIMObjects(List rels) throws Exception {

		if(rels == null || rels.size() == 0){
			return null;
		}

		EIMObject[] objects = new EIMObject[rels.size()];

		for(int i = 0; i < rels.size(); i++) {
			EIMObject obj = ((EIMRelation)rels.get(i)).getChild();
			objects[i] = obj;
		}
		return objects;
	}

	/**
	 * IDと権限ロールIDを元に該当するオブジェクト配列を取得。<br>
	 * loadメソッドによって事前にメモリ上にオブジェクト一覧とアクセス権限情報をロードすることができます。
	 * ロード済のオブジェクト一覧とアクセス権限情報が存在する場合は、DBアクセスは行わずそれを返却します。
	 * @param sess
	 * @param objects
	 * @param user
	 * @param roleIdList
	 * @return
	 * @throws Exception
	 */
	public List getAuthorizedObjects(EIMSession sess, EIMObject[] objects, EIMUser user, int roleId) throws Exception
	{
		List<EIMObject> loadedResultList = new ArrayList<>();
		List<EIMObject> notLoadedObjectList = new ArrayList<>();
		EIMObject[] notLoadedObjects = null;

		// 取得済みの情報で権限チェック
		if (bulkLoader.objId_accessRoleIdListMap.size() > 0 && bulkLoader.objId_childObjectRecursiveMap.size() > 0) {
			for (int i = 0; i < objects.length; i ++) {
				List<Integer> accessRoleList = bulkLoader.objId_accessRoleIdListMap.get(new Long(objects[i].getId()));
				if (accessRoleList == null) {
					// 未取得の場合
					notLoadedObjectList.add(objects[i]);
				} else if (accessRoleList.contains(roleId)) {
					// 権限がある場合
					EIMObject object = bulkLoader.objId_childObjectRecursiveMap.get(new Long(objects[i].getId()));
					if (object == null) {
						// 未取得の場合
						notLoadedObjectList.add(objects[i]);
					} else {
						// 取得済みの情報を結果に設定
						loadedResultList.add(object);
					}
				}
			}
			// 未取得のオブジェクトを検索対象とする
			notLoadedObjects = notLoadedObjectList.toArray(new EIMObject[0]);
		} else {
			// 取得済のオブジェクトが存在しない場合は引数をそのまま検索対象とする
			notLoadedObjects = objects;
		}

		if (notLoadedObjects.length == 0) {
			return loadedResultList;
		}

		//SelectTargetとヘルパを確保
		EIMSearchSelectEIMObject selectTarget = new EIMSearchSelectEIMObject();
		EIMSearchSelectEIMObject.SearchConditionBuildHelper
			h = new EIMSearchSelectEIMObject.SearchConditionBuildHelper();

		//オブジェクトのIDを検索条件として登録
		EIMSearchConditionGroup g = h.group(h.opAnd());
		
		long[] eimObjIdList = new long[notLoadedObjects.length];
		for (int i=0; i<notLoadedObjects.length; i++) {
			eimObjIdList[i] = notLoadedObjects[i].getId();		// EIMOblectIdを格納
		}
		
		g.addCondition(
				h.in(h.opAnd(),
						PsedoAttributeTypeEnum.ID,
						h.opIn(),
						TypeConvertUtils.convertToBuildTypeArray(eimObjIdList)
				)
		);
		selectTarget.setCondition(g);

		//判定対象の権限を設定
		selectTarget.setRole(roleId);
		selectTarget.setUser(user);

		//検索実行
		@SuppressWarnings("unchecked")
		List<EIMObject> resultList = SearchUtils.searchObjects(sess, selectTarget,
				new EIMSearchLimitCountCondition(EIMSearchLimitCountCondition.UNLIMITED, false));

		// 検索結果に取得済み情報を追加
		resultList.addAll(loadedResultList);

		if(resultList == null || resultList.size() == 0){
			return null;
		}

		return resultList;

	}

	/**
	 * オブジェクトタイプに適用されている属性タイプの一覧を取得します。このとき、親オブジェクトタイプに適用されている属性タイプも合わせて取得します。<br>
	 * DBから取得したオブジェクトはメモリにロードされ、再度呼び出された場合、再利用されます。
	 * @param objType オブジェクトタイプ
	 * @return 属性タイプ一覧
	 * @throws Exception
	 */
	@SuppressWarnings("unchecked")
	public List<EIMAttributeType> getAttributeTypeList(EIMObjectType objType) throws Exception {
		List<EIMAttributeType> attributeTypeList = objTypeId_AttributeListMap.get(new Long(objType.getId()));
		if (attributeTypeList == null) {
			// 未取得の場合
			attributeTypeList = ObjectAttributeUtils.getAttributeTypeList(_sess, objType);
			objTypeId_AttributeListMap.put(new Long(objType.getId()), attributeTypeList);
		}
		return attributeTypeList;
	}

	/**
	 * オブジェクトタイプに適用されている属性タイプの一覧を取得します。このとき、親オブジェクトタイプに適用されている属性タイプも合わせて取得します。<br>
	 * DBから取得したオブジェクトはメモリにロードされ、再度呼び出された場合、再利用されます。
	 * @param objType オブジェクトタイプ
	 * @return 属性タイプ一覧
	 * @throws Exception
	 */
	@SuppressWarnings("unchecked")
	public List<AttributeTypeDomain> getAttributeTypeList(ObjectTypeDomain objType) throws Exception {
		List<AttributeTypeDomain> attributeTypeList = objTypeId_AttributeListMap2.get(new Long(objType.getId()));
		if (attributeTypeList == null) {
			// 未取得の場合
			ApplicationContext context = ApplicationContextLoader.getApplicationContext();
			AttributeTypeService attributeTypeService = (AttributeTypeService) context.getBean("attributeTypeService2");
			AttributeTypeCriteria attributeTypeCriteria = new AttributeTypeCriteria();
			attributeTypeCriteria.setObjectTypeId(objType.getId());
			attributeTypeList = attributeTypeService.getList(attributeTypeCriteria);
			objTypeId_AttributeListMap2.put(new Long(objType.getId()), attributeTypeList);
		}
		return attributeTypeList;
	}
	
	/**
	 * フルパスを元にオブジェクトを取得します。 
	 * <ul>
	 * <li>フルパス文字列、タイプ文字列が不正な場合はnullを返します。
	 * </ul>
	 * DBから取得したオブジェクトはメモリにロードされ、再度呼び出された場合、再利用されます。
	 * @param parentPath パス
	 * @param strObjType オブジェクトのタイプ(ドキュメント or フォルダ or タグ)
	 * @return 取得したオブジェクト
	 * @throws Exception
	 */
	public EIMObject getObjectByFullPass(String parentPath, String strObjType) throws Exception {
		EIMObject parentObj = path_folderMap.get(parentPath);
		if (parentObj == null) {
			// 未取得の場合
			parentObj = AppObjectUtil.getObjListByFullPass(getSession(), parentPath, strObjType);
			path_folderMap.put(parentPath, parentObj);
		}
		return parentObj;
	}

	/**
	 * 指定されたオブジェクトに関連する属性表示色オブジェクトのマップを取得する。
	 * DBから取得したオブジェクトはメモリにロードされ、再度呼び出された場合、再利用されます。
	 * @param parentObj 親オブジェクト
	 * @return 属性表示色オブジェクトMap
	 * @throws Exception
	 */
	public HashMap<String, EIMObject> getDisplayColorSet(EIMObject parentObj) throws Exception {
		HashMap<String, EIMObject> displayColorMap = parentObjId_displayColorMapMap.get(new Long(parentObj.getId()));
		if (displayColorMap == null) {
			// 未取得の場合
			displayColorMap = DisplayColorUtil.getDisplayColorSet(_sess, Arrays.asList(parentObj));
			parentObjId_displayColorMapMap.put(new Long(parentObj.getId()), displayColorMap);
		}

		return displayColorMap;
	}

	/**
	 * パス属性を配列で取得します。 setAttrPath()で設定された値があればそれを、なければ引数objから値を取得します。
	 * @param obj 対象オブジェクト
	 * @return パス属性(文字列)配列
	 * @throws Exception
	 */
	public String[] getStrAttrsPath(EIMObject obj) throws Exception {
		// 更新された値を取得
		String[] pathArray = objId_pathArray.get(new Long(obj.getId()));
		if (pathArray == null) {
			pathArray = AppObjectUtil.getStrAttrs(_sess, obj, EIMConfig.get("ATTR_NAME_DOCUMENT_PASS"));
		}
		return pathArray;
	}

	/**
	 * パス属性を配列で取得します。 setAttrPath()で設定された値があればそれを、なければ引数objから値を取得します。
	 * @param obj 対象オブジェクト
	 * @return パス属性(文字列)配列
	 * @throws Exception
	 */
	public List<String> getStrAttrsPath(ObjectDomain obj) throws Exception {
		// 更新された値を取得
		String[] pathArray = objId_pathArray.get(new Long(obj.getId()));
		List<String> pathList = new ArrayList<>();
		if (pathArray == null) {
			AttributeDomain attributeDomain = obj.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PASS"));
			if (attributeDomain != null) {
				pathList = attributeDomain.getStringList();
			}
		} else {
			pathList = Arrays.asList(pathArray);
		}
		return pathList;
	}
	
	/**
	 * パス属性値配列を設定します。設定した値はメモリ上に保持され、getStrAttrsPath()で再取得することができます。
	 * @param obj 対象オブジェクト
	 * @param pathArray パス属性(文字列)配列
	 * @throws Exception
	 */
	public void setAttrPath(EIMObject obj, String[] pathArray) throws Exception {
		ObjectAttributeUtils.setAttribute(_sess, obj, getAttrType(EIMConfig.get("ATTR_NAME_DOCUMENT_PASS")), pathArray);
		// 更新された値を保管
		objId_pathArray.put(new Long(obj.getId()), pathArray);
	}

	/**
	 * リンク先属性を配列で取得します。 setAttrToLink()で設定された値があればそれを、なければ引数objから値を取得します。
	 * @param obj 対象オブジェクト
	 * @return リンク先属性(数値)配列
	 * @throws Exception
	 */
	public long[] getIntAttrsToLink(EIMObject obj) throws Exception {
		// 更新された値を取得
		long[] pathArray = objId_toLinkArray.get(new Long(obj.getId()));
		if (pathArray == null) {
			pathArray = AppObjectUtil.getIntAttrs(_sess, obj, EIMConfig.get("ATTR_NAME_DOCUMENT_TARGET_TO_LINK"));
		}
		return pathArray;
	}

	/**
	 * リンク先属性値配列を設定します。設定した値はメモリ上に保持され、getIntAttrsToLink()で再取得することができます。
	 * @param obj 対象オブジェクト
	 * @param toLink リンク先属性(数値)配列
	 * @throws Exception
	 */
	public void setAttrToLink(EIMObject obj, long[] toLink) throws Exception {
		ObjectAttributeUtils.setAttribute(_sess, obj, getAttrType(EIMConfig.get("ATTR_NAME_DOCUMENT_TARGET_TO_LINK")), TypeConvertUtils.convertToBuildTypeArray(toLink));
		// 更新された値を保管
		objId_toLinkArray.put(new Long(obj.getId()), toLink);
	}

	/**
	 * リレーションタイプを指定して子リレーション一覧を取得します。当メソッドは権限ロールIDでオブジェクトの権限判定を行います。<br>
	 * loadメソッドによって事前にメモリ上にリレーションをロードすることができます。
	 * ロード済のリレーション一覧が存在する場合は、DBアクセスは行わずそれを返却します。
	 * @param parentObj 親オブジェクト
	 * @param relType リレーションタイプ(ドキュメント or リンク or ブランチ)
	 * @param roleId
	 * @return 子リレーション一覧
	 * @throws Exception
	 */
	@SuppressWarnings("unchecked")
	public List<EIMRelation> getChildRelationListByRelType(EIMObject parentObj, EIMRelationType relType, int roleId) throws Exception {

		List<EIMRelation> loadedRelationList = null;
		if (relType.getId() == getRelationTypeOfDocument().getId()) {
			// ドキュメントリレーション
			loadedRelationList = bulkLoader.parentObjId_childRelationOfDocListMap.get(new Long(parentObj.getId()));
		} else if (relType.getId() == getRelationTypeOfDocLink().getId()) {
			// リンクリレーション
			loadedRelationList = bulkLoader.parentObjId_childRelationOfLinkListMap.get(new Long(parentObj.getId()));
		} else if (relType.getId() == getRelationTypeOfBranch().getId()) {
			// ブランチリレーション
			loadedRelationList = bulkLoader.parentObjId_childRelationOfBranchListMap.get(new Long(parentObj.getId()));
		}

		List<EIMRelation> childRelationList = null;
		if (loadedRelationList == null) {
			// 未ロードの場合
			childRelationList = RelationUtils.getChildRelationListByRelType(_sess, parentObj, relType, roleId);
		} else {
			// ロード済みの場合
			if (roleId == EIMAccessRole.NONE) {
				// 権限判定なしの場合
				childRelationList = loadedRelationList;
			} else {
				// 権限判定ありの場合
				childRelationList = new ArrayList<>();
				for (EIMRelation rel : loadedRelationList) {
					List<Integer> accessRoleList = bulkLoader.objId_accessRoleIdListMap.get(new Long(rel.getChild().getId()));
					if (accessRoleList != null && accessRoleList.contains(roleId)) {
						// 権限がある場合
						childRelationList.add(rel);
					}
				}
			}
		}

		return childRelationList;
	}

	
	/**
	 * リレーションタイプを指定して親リレーション一覧を取得します。当メソッドは権限ロールIDでオブジェクトの権限判定を行います。<br>
	 * loadメソッドによって事前にメモリ上にリレーションをロードすることができます。
	 * ロード済のリレーション一覧が存在する場合は、DBアクセスは行わずそれを返却します。
	 * @param parentObj 子オブジェクト
	 * @param relType リレーションタイプ(ドキュメント or リンク or ブランチ)
	 * @param roleId
	 * @return 子リレーション一覧
	 * @throws Exception
	 */
	@SuppressWarnings("unchecked")
	public List<EIMRelation> getParentRelationListByRelType(EIMObject childObj, EIMRelationType relType, int roleId) throws Exception {
		List<EIMRelation> loadedRelationList = null;
		if (relType.getId() == getRelationTypeOfBranch().getId()) {
			// ブランチリレーション
			loadedRelationList = bulkLoader.childObjId_parentRelationOfBranchListMap.get(new Long(childObj.getId()));
		}
		// V6.17時点ではドキュメントリレーション、リンクリレーションの保管情報はない

		List<EIMRelation> parentRelationList = null;
		if (loadedRelationList == null) {
			// 未ロードの場合
			parentRelationList = RelationUtils.getParentRelationListByRelType(getSession(), childObj, relType, roleId);
		} else {
			// ロード済みの場合
			if (roleId == EIMAccessRole.NONE) {
				// 権限判定なしの場合
				parentRelationList = loadedRelationList;
			} else {
				// 権限判定ありの場合
				parentRelationList = new ArrayList<>();
				for (EIMRelation rel : loadedRelationList) {
					List<Integer> accessRoleList = bulkLoader.objId_accessRoleIdListMap.get(new Long(rel.getParent().getId()));
					if (accessRoleList != null && accessRoleList.contains(roleId)) {
						// 権限がある場合
						parentRelationList.add(rel);
					}
				}
			}
		}

		return parentRelationList;
	}

	/**
	 * リレーションタイプを指定して親リレーション一覧を取得します。<br>
	 * loadメソッドによって事前にメモリ上にリレーションをロードすることができます。
	 * @param relType
	 * @param childObj
	 * @param accessRoleTypeDomain
	 * @return 子リレーション一覧
	 * @throws Exception
	 */
	@SuppressWarnings("unchecked")
	public List<RelationDomain> getParentRelationListByRelTypeAndChild(RelationTypeDomain relType, ObjectDomain childObj,
			AccessRoleTypeDomain accessRoleTypeDomain) throws Exception {
		List<RelationDomain> loadedRelationList = null;
		if (relType.getId() == getRelationTypeOfBranch().getId()) {
			// ブランチリレーション
			loadedRelationList = bulkLoader.childObjId_parentRelationOfBranchListMap2.get(new Long(childObj.getId()));
		}
		// V6.17時点ではドキュメントリレーション、リンクリレーションの保管情報はない

		List<RelationDomain> parentRelationList = null;
		if (loadedRelationList == null) {
			// 未ロードの場合
			parentRelationList = AppDocumentUtil.getParentRelationByRelTypeAndChild(relType, childObj, accessRoleTypeDomain);
		} else {
			// ロード済みの場合
			if (accessRoleTypeDomain == null) {
				// 権限判定なしの場合
				parentRelationList = loadedRelationList;
			} else {
				// 権限判定ありの場合
				parentRelationList = new ArrayList<>();
				for (RelationDomain rel : loadedRelationList) {
					List<Integer> accessRoleList = bulkLoader.objId_accessRoleIdListMap.get(new Long(rel.getParent().getId()));
					if (accessRoleList != null && accessRoleList.contains(accessRoleTypeDomain.getId())) {
						// 権限がある場合
						parentRelationList.add(rel);
					}
				}
			}
		}

		return parentRelationList;
	}
	
	/**
	 * オブジェクトのバージョンを取得します。<br>
	 * loadメソッドによって事前にメモリ上にバージョン情報をロードすることができます。
	 * ロード済のバージョン情報が存在する場合は、DBアクセスは行わずそれを返却します。
	 * @param object オブジェクト
	 * @return バージョン
	 * @throws Exception
	 */
	public EIMVersion getVersion(EIMObject object) throws Exception {
		EIMVersion version = bulkLoader.objId_versionMap.get(new Long(object.getId()));
		if (version == null) {
			// 未ロードの場合
			version = VersionUtils.getVersion(_sess, object);
		}
		return version;
	}

	/**
	 * オブジェクトのバージョンを取得します。<br>
	 * loadメソッドによって事前にメモリ上にバージョン情報をロードすることができます。
	 * ロード済のバージョン情報が存在する場合は、DBアクセスは行わずそれを返却します。
	 * @param object オブジェクト
	 * @return バージョン
	 * @throws Exception
	 */
	public List<ObjectDomain> getVersion(ObjectDomain object) throws Exception {
		List<ObjectDomain> version = bulkLoader.objId_versionMap2.get(object.getId());
		if (version == null) {
			// 未ロードの場合
			version = new ArrayList<ObjectDomain>();
			List<Object> objects =  Arrays.asList(jp.co.ctc_g.eim.app.document.common.util.VersionUtils.getVersion(object).values().toArray());
	
			for (Object obj : objects) {
				 version.add((ObjectDomain) obj);
			}
			bulkLoader.objId_versionMap2.put(object.getId(), version);
		}
		return version;
	}
	/**
	 * 指定したオブジェクトに対してアクセス権限があるかをチェックします。<br>
	 * loadメソッドによって事前にメモリ上にアクセス権限情報をロードすることができます。
	 * ロード済のアクセス権限情報が存在する場合は、DBアクセスは行わずそれを使用して権限判定を行います。
	 * @param object オブジェクト
	 * @param user ユーザ
	 * @param roleId アクセス権限ID
	 * @return 権限があるか
	 * @throws Exception
	 */
	public boolean authorized(EIMObject object, EIMUser user, int roleId) throws Exception {
		List<Integer> accessRoleList = bulkLoader.objId_accessRoleIdListMap.get(new Long(object.getId()));
		if (accessRoleList != null) {
			// ロード済みの場合
			return accessRoleList.contains(roleId);
		}

		// 未ロードの場合
		return SecurityUtils.authorized(getSession(), object, getSession().getUser(), EIMAccessRole.READ);
	}

	/**
	 * 指定したフォルダ配下のドキュメントリレーションおよびドキュメントリンクリレーションを再帰的に探索し、メモリに読み込みます。<br>
	 * オブジェクト、アクセス権限情報の他にモードの指定によってバージョン情報、ブランチリレーションを読み込みます。
	 * @param folder 起点となるフォルダ
	 * @param roleIdList 取得対象のアクセス権限ID一覧
	 * @param mode ロードモード
	 * @throws Exception
	 */
	public void loadChildObjectsRecursive(EIMObject folder, List<Integer> roleIdList, LoadingModeEnum mode) throws Exception {
		bulkLoader = new BulkLoader();
		bulkLoader.loadChildObjectsRecursive(folder, roleIdList, mode);
	}

	/**
	 * 再帰的データロード処理の実行モードです。
	 */
	public enum LoadingModeEnum {
		/**
		 * 指定をフォルダを含む配下のフォルダ、ドキュメント全てのバージョン情報をロードします。
		 */
		VERSION,
		/**
		 * 指定をフォルダを含む配下のフォルダ、ドキュメント全てのバージョン情報と、
		 * 配下のドキュメントのブランチリレーションをロードします。
		 */
		VERSION_AND_BRANCH;
	}

	/**
	 * オブジェクトの一括ロードを行います。
	 * AppObjectConditionHelperを介してこれを取得することでDBアクセスの回数を減らし、性能改善効果が期待できます。
	 */
	private class BulkLoader {

		/** オブジェクトIDと再帰取得した子オブジェクトのMap */
		private Map<Long, EIMObject> objId_childObjectRecursiveMap = new HashMap<>();

		/** 子オブジェクトIDと親ブランチリレーション */
		private Map<Long, List<EIMRelation>> childObjId_parentRelationOfBranchListMap = new HashMap<>();

		/** 子オブジェクトIDと親ブランチリレーション */
		private Map<Long, List<RelationDomain>> childObjId_parentRelationOfBranchListMap2 = new HashMap<>();
		
		/** 親オブジェクトIDと子ドキュメントリレーション一覧のMap */
		private Map<Long, List<EIMRelation>> parentObjId_childRelationOfDocListMap = new HashMap<>();

		/** 親オブジェクトIDと子リンクリレーション一覧のMap */
		private Map<Long, List<EIMRelation>> parentObjId_childRelationOfLinkListMap = new HashMap<>();

		/** 親オブジェクトIDと子ブランチリレーション一覧のMap */
		private Map<Long, List<EIMRelation>> parentObjId_childRelationOfBranchListMap = new HashMap<>();
		
		/** 親オブジェクトIDと子オブジェクト一覧のMap */
		private Map<Long, List<ObjectDomain>> parentObjId_childObjectListMap = new HashMap<>();

		/** オブジェクトIDとアクセス権限ID一覧のMap */
		private Map<Long, List<Integer>> objId_accessRoleIdListMap = new HashMap<>();

		/** バージョンに含まれる全てのオブジェクトIDとバージョンのMap */
		private Map<Long, EIMVersion> objId_versionMap = new HashMap<>();

		/** バージョンに含まれる全てのオブジェクトIDとバージョンのMap */
		private Map<Long, List<ObjectDomain>> objId_versionMap2 = new HashMap<>();
		
		/**
		 * 指定したフォルダ配下のドキュメントリレーションおよびドキュメントリンクリレーションを再帰的に探索し、メモリに読み込みます。
		 * @param folder フォルダ
		 * @return 再帰的にロードしたフォルダ配下のオブジェクト一覧
		 * @throws Exception
		 */
		private void loadChildObjectsRecursive(EIMObject folder, List<Integer> roleIdList, LoadingModeEnum mode) throws Exception {
			// 対象はフォルダのみ
			if (!isTypeOfFolder(folder.getType())) {
				return;
			}

			// フォルダの子階層を再帰的に探索
			List<EIMObject> parentList = Arrays.asList(folder);
			loadChildObjectsRecursive(parentList, folder);

			// ロードされたオブジェクト (以降の処理は指定された親フォルダを含める)
			List<EIMObject> loadedObjectList = new ArrayList<>(objId_childObjectRecursiveMap.values());
			loadedObjectList.add(folder);

			// バージョン一覧を取得
			List<EIMVersion> versionList = AppObjectUtil.getVersionListByObjects(getSession(), loadedObjectList);

			// EIMVersionをMapに保管
			for (EIMVersion version : versionList) {
				for (Object obj : version.getList()) {
					EIMObject eimObj = (EIMObject) obj;
					objId_versionMap.put(new Long(eimObj.getId()), version);
				}
			}

			// アクセス権限
			loadAccessRoleList(loadedObjectList, roleIdList);

			// ブランチリレーション一括取得
			if (mode == LoadingModeEnum.VERSION_AND_BRANCH) {
				List<EIMObject> allRevisionDocumentList = new ArrayList<>();
				for (EIMVersion version : versionList) {
					for (Object obj :  version.getList()) {
						if (isTypeOfDocument(((EIMObject) obj).getType())) {
							// ドキュメントの全リビジョンが対応
							allRevisionDocumentList.add((EIMObject) obj);
						}
					}
				}
				loadBranchRelation(allRevisionDocumentList);
			}

		}

		/**
		 * 指定したフォルダ一覧配下のドキュメントリレーションおよびドキュメントリンクリレーションを再帰的に探索し、メモリに読み込みます。
		 * @param folderList フォルダ一覧
		 * @param startFolder 再帰処理の起点となった最上位のフォルダ
		 * @throws Exception
		 */
		private void loadChildObjectsRecursive(List<EIMObject> folderList, EIMObject startFolder)
				throws Exception {

			if (folderList == null || folderList.size() == 0) {
				return;
			}

			// 子リレーションリストの保管用Mapの設定
			for (EIMObject parent : folderList) {
				parentObjId_childRelationOfDocListMap.put(new Long(parent.getId()), new ArrayList<>());
				parentObjId_childRelationOfLinkListMap.put(new Long(parent.getId()), new ArrayList<>());
			}

			EIMSearchSelectEIMRelation.SearchConditionBuildHelper h = new EIMSearchSelectEIMRelation.SearchConditionBuildHelper();

			// リレーション条件
			EIMSearchSelectEIMRelation relationTarget = new EIMSearchSelectEIMRelation();
			// 親オブジェクトID
			long[] ids = folderList.stream().mapToLong(obj -> obj.getId()).toArray();
			// ドキュメントリレーション or リンクリレーション
			long[] types = Arrays.asList(getRelationTypeOfDocument(), getRelationTypeOfDocLink()).stream()
					.mapToLong(type -> type.getId()).toArray();
			relationTarget.setCondition(h.group(h.opAnd())
					// 親オブジェクト
					.addCondition(
							h.in(h.opAnd(),
									EIMSearchSelectEIMRelation.PsedoAttributeTypeEnum.PARENT,
									h.opIn(),
									TypeConvertUtils.convertToBuildTypeArray(ids)))
					// リレーションタイプ
					.addCondition(
							h.in(h.opAnd(),
									EIMSearchSelectEIMRelation.PsedoAttributeTypeEnum.TYPE,
									h.opIn(),
									TypeConvertUtils.convertToBuildTypeArray(types))));

			// 親オブジェクト条件
			EIMSearchSelectEIMObject parentTarget = new EIMSearchSelectEIMObject();
			parentTarget.setRole(EIMAccessRole.NONE);
			parentTarget.setResultAttrs(new ArrayList<>());

			// 子オブジェクト条件
			EIMSearchSelectEIMObject childTarget = new EIMSearchSelectEIMObject();
			childTarget.setRole(EIMAccessRole.NONE);

			// リレーション検索
			@SuppressWarnings("unchecked")
			List<EIMRelation> relationList = SearchUtils.searchRelations(getSession(), relationTarget, parentTarget,
						childTarget, new EIMSearchLimitCountCondition(EIMSearchLimitCountCondition.UNLIMITED, false));

			// 検索結果を保管
			List<EIMObject> childFolderList = new ArrayList<>();
			for (EIMRelation relation : relationList) {
				EIMRelationType relType = relation.getType();
				EIMObject parent = relation.getParent();
				EIMObject child = relation.getChild();

				if (relType.getId() == getRelationTypeOfDocument().getId()) {
					// 子リレーション(ドキュメント)MAPに保管
					parentObjId_childRelationOfDocListMap.get(new Long(parent.getId())).add(relation);
				} else if (relType.getId() == getRelationTypeOfDocLink().getId()) {
					// 子リレーション(リンク)MAPに保管
					parentObjId_childRelationOfLinkListMap.get(new Long(parent.getId())).add(relation);
				}

				// オブジェクトMapにオブジェクトを保管
				objId_childObjectRecursiveMap.putIfAbsent(new Long(child.getId()), child);

				// 子フォルダを保管
				if (isTypeOfFolder(child.getType())) {
					childFolderList.add(child);
				}
			}

			// 再帰呼び出し
			loadChildObjectsRecursive(childFolderList, startFolder);
		}

		/**
		 * ログインユーザが、指定したオブジェクト一覧に対して、指定したアクセス権限を有するかチェックし、メモリに読み込みます。
		 * @param objectListオブジェクト一覧
		 * @param roleIdList アクセス権限一覧
		 * @throws Exception
		 */
		private void loadAccessRoleList(List<EIMObject> objectList, List<Integer> roleIdList) throws Exception {
			if (roleIdList == null || roleIdList.size() == 0) {
				return;
			}

			//SelectTargetとヘルパを確保
			EIMSearchSelectEIMObject selectTarget = new EIMSearchSelectEIMObject();
			EIMSearchSelectEIMObject.SearchConditionBuildHelper h = new EIMSearchSelectEIMObject.SearchConditionBuildHelper();

			//オブジェクトのIDを検索条件として登録
			EIMSearchConditionGroup g = h.group(h.opAnd());
			selectTarget.setCondition(g);

			// ID
			long[] eimObjIdList = objectList.stream().mapToLong(obj -> new Long(obj.getId())).toArray();
			g.addCondition(
					h.in(h.opAnd(),
							PsedoAttributeTypeEnum.ID,
							h.opIn(),
							TypeConvertUtils.convertToBuildTypeArray(eimObjIdList)));

			// User
			selectTarget.setUser(_sess.getUser());

			// 取得項目設定
			selectTarget.setResultAttrs(new ArrayList<>());

			// 保管用Map初期化
			for (EIMObject object : objectList) {
				objId_accessRoleIdListMap.put(new Long(object.getId()), new ArrayList<>());
			}

			for (Integer roleId : roleIdList) {
				//判定対象の権限を設定
				selectTarget.setRole(roleId);

				//検索実行
				@SuppressWarnings("unchecked")
				List<EIMObject> resultList = SearchUtils.searchObjects(_sess, selectTarget,
						new EIMSearchLimitCountCondition(EIMSearchLimitCountCondition.UNLIMITED, false));

				// アクセス権限保管リストに追加
				for (EIMObject result : resultList) {
					objId_accessRoleIdListMap.get(new Long(result.getId())).add(roleId);
				}
			}
		}

		/**
		 * フォルダ一覧配下のドキュメントを保持するドキュメントブランチリレーションをロードします。
		 * @param objectList フォルダ一覧
		 * @throws Exception
		 */
		private void loadBranchRelation(List<EIMObject> objectList)
				throws Exception {

			if (objectList == null || objectList.size() == 0) {
				return;
			}

			// 子リレーションリストの保管用Mapの設定
			for (EIMObject object : objectList) {
				parentObjId_childRelationOfBranchListMap.put(new Long(object.getId()), new ArrayList<>());
				childObjId_parentRelationOfBranchListMap.put(new Long(object.getId()), new ArrayList<>());
				childObjId_parentRelationOfBranchListMap2.put(new Long(object.getId()), new ArrayList<>());
			}

			EIMSearchSelectEIMRelation.SearchConditionBuildHelper h = new EIMSearchSelectEIMRelation.SearchConditionBuildHelper();

			// ---------------
			// 子リレーション
			// ---------------

			// リレーション条件
			EIMSearchSelectEIMRelation relationTarget = new EIMSearchSelectEIMRelation();
			long[] ids = objectList.stream().mapToLong(obj -> obj.getId()).toArray();
			relationTarget.setCondition(h.group(h.opAnd())
					// 親オブジェクト
					.addCondition(
							h.in(h.opAnd(),
									EIMSearchSelectEIMRelation.PsedoAttributeTypeEnum.PARENT,
									h.opIn(),
									TypeConvertUtils.convertToBuildTypeArray(ids)))
					// リレーションタイプ
					.addCondition(
							h.eq(h.opAnd(),
									EIMSearchSelectEIMRelation.PsedoAttributeTypeEnum.TYPE,
									getRelationTypeOfBranch().getId())));

			// 親オブジェクト条件
			EIMSearchSelectEIMObject parentObjTarget = new EIMSearchSelectEIMObject();
			parentObjTarget.setRole(EIMAccessRole.NONE);
			parentObjTarget.setResultAttrs(new ArrayList<>());

			// 子オブジェクト条件
			EIMSearchSelectEIMObject childObjTarget = new EIMSearchSelectEIMObject();
			childObjTarget.setRole(EIMAccessRole.NONE);

			// リレーション検索
			@SuppressWarnings("unchecked")
			List<EIMRelation> childRelationList = SearchUtils.searchRelations(getSession(), relationTarget,
					parentObjTarget, childObjTarget,
					new EIMSearchLimitCountCondition(EIMSearchLimitCountCondition.UNLIMITED, false));

			// 検索結果を保管
			for (EIMRelation relation : childRelationList) {
				// 子リレーション(ブランチ)MAPに保管
				parentObjId_childRelationOfBranchListMap.get(new Long(relation.getParent().getId())).add(relation);
			}

			// ---------------
			// 親リレーション
			// ---------------

			// リレーション条件
			relationTarget = new EIMSearchSelectEIMRelation();
			relationTarget.setCondition(h.group(h.opAnd())
					// 親オブジェクト
					.addCondition(
							h.in(h.opAnd(),
									EIMSearchSelectEIMRelation.PsedoAttributeTypeEnum.CHILD,
									h.opIn(),
									TypeConvertUtils.convertToBuildTypeArray(ids)))
					// リレーションタイプ
					.addCondition(
							h.eq(h.opAnd(),
									EIMSearchSelectEIMRelation.PsedoAttributeTypeEnum.TYPE,
									getRelationTypeOfBranch().getId())));

			// 親オブジェクト条件
			parentObjTarget = new EIMSearchSelectEIMObject();
			parentObjTarget.setRole(EIMAccessRole.NONE);

			// 子オブジェクト条件
			childObjTarget = new EIMSearchSelectEIMObject();
			childObjTarget.setRole(EIMAccessRole.NONE);
			childObjTarget.setResultAttrs(new ArrayList<>());

			// リレーション検索
			@SuppressWarnings("unchecked")
			List<EIMRelation> parentRelationList = SearchUtils.searchRelations(getSession(), relationTarget,
					parentObjTarget, childObjTarget,
					new EIMSearchLimitCountCondition(EIMSearchLimitCountCondition.UNLIMITED, false));

			// 検索結果を保管
			for (EIMRelation relation : parentRelationList) {
				// 親リレーション(ブランチ)MAPに保管
				childObjId_parentRelationOfBranchListMap.get(new Long(relation.getChild().getId())).add(relation);
				childObjId_parentRelationOfBranchListMap2.get(new Long(relation.getChild().getId())).add(ConvertUtils.toRelationDomain(relation));
			}
		}

	}

}