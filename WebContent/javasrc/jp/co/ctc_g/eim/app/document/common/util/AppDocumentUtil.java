package jp.co.ctc_g.eim.app.document.common.util;

import java.io.File;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import org.springframework.context.ApplicationContext;

import common.util.AppConstant;
import eim.util.EIMConfig;
import jp.co.ctc_g.eim.framework2.business.dao.FileDao;
import jp.co.ctc_g.eim.framework2.business.dao.ObjectDao;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.ObjectCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.RelationCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AccessRoleTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.FileDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.FormatDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.RelationDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.RelationTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchConditionCompare;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchConditionGroup;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchOperatorEnum;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchSelectObject;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchSelectObject.SearchConditionBuildHelper;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchSelectRelation;
import jp.co.ctc_g.eim.framework2.business.service.AttributeTypeService;
import jp.co.ctc_g.eim.framework2.business.service.FormatService;
import jp.co.ctc_g.eim.framework2.business.service.ObjectService;
import jp.co.ctc_g.eim.framework2.business.service.ObjectTypeService;
import jp.co.ctc_g.eim.framework2.business.service.RelationService;
import jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework2.common.enumeration.DuplicateCheckModeEnum;
import jp.co.ctc_g.eim.framework2.common.enumeration.LockModeEnum;
import jp.co.ctc_g.eim.framework2.common.enumeration.ValueTypeEnum;
import jp.co.ctc_g.eim.framework2.common.exception.EIMException;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;
import jp.co.ctc_g.eim.framework2.integration.util.internal.FileUtil;

/**
* 【ドキュメントAPI】
* (ドキュメント管理用)オブジェクト関連クラス
*
*/
public class AppDocumentUtil {

	/**
	 * 入力文字列にWindows禁止文字が含まれる場合にはEIMExceptionを返します。
	 *
	 * @param name ファイル名、または、フォルダ名
	 * @throws Exception
	 */
	public static void checkValidateFName(String name) throws Exception {
		if (!StringUtils.isEmpty(name)) {
			for (int i = 0; AppConstant.INVALID_NAME_CHAR.length > i; i++) {
				if (name.indexOf(AppConstant.INVALID_NAME_CHAR[i]) != -1) {
					// 名前には次の文字は使えません。\n \ / : * ? " < > |
					throw new EIMException("EIM.WARN.LOGIC.INVALIDNAME");
				}
			}
		}
	}

	/**
	 * オブジェクト属性(数値)を取得します。
	 *
	 * @param objectDomain 属性を取得するオブジェクト
	 * @param attrName 属性名称
	 * @return 属性値
	 */
	public static long getIntAttr(ObjectDomain objectDomain, String attrName) {
		if (objectDomain.getAttribute(attrName) != null) {
			return objectDomain.getAttribute(attrName).getLong();
		} else {
			return 0;
		}
	}

	/**
	 * オブジェクト属性(数値)を配列で取得します。
	 *
	 * @param obj 属性を取得するオブジェクト
	 * @param attrName 属性名称
	 * @return 属性値を格納した配列
	 * @throws Exception
	 */
	public static List<Long> getIntAttrs(ObjectDomain obj, String attrName) throws Exception {
		List<Long> ret = null;
		AttributeDomain attr = obj.getAttribute(attrName);
		if (attr != null) {
			ret = attr.getLongList();
		}
		return ret;
	}

	/**
	 * オブジェクト属性(文字列)を配列で取得します。
	 *
	 * @param obj 属性を取得するオブジェクト
	 * @param attrName 属性名称
	 * @return 属性値を格納した配列
	 * @throws Exception
	 */
	public static List<String> getStrAttrs(ObjectDomain obj, String attrName) throws Exception {
		List<String> ret = null;
		AttributeDomain attr = obj.getAttribute(attrName);
		if (attr != null) {
			ret = attr.getStringList();
		}
		return ret;
	}

	/**
	 * オブジェクト属性(日付)を配列で取得します。
	 *
	 * @param obj 属性を取得するオブジェクト
	 * @param attrName 属性名称
	 * @return 属性値を格納した配列
	 * @throws Exception
	 */
	public static List<Date> getDateAttrs(ObjectDomain obj, String attrName) throws Exception {
		List<Date> ret = null;
		AttributeDomain attr = obj.getAttribute(attrName);
		if (attr != null) {
			ret = attr.getDateList();
		}
		return ret;
	}

	/**
	 * オブジェクト属性(double数値)を配列で取得します。
	 *
	 * @param obj 属性を取得するオブジェクト
	 * @param attrName 属性名称
	 * @return 属性値を格納した配列
	 * @throws Exception
	 */
	public static List<Double> getDoubleAttrs(ObjectDomain obj, String attrName) throws Exception {
		List<Double> ret = null;
		AttributeDomain attr = obj.getAttribute(attrName);
		if (attr != null) {
			ret = attr.getDoubleList();
		}
		return ret;
	}

	/**
	 * オブジェクト属性(文字列)を配列で取得します。
	 *
	 * @param obj 属性を取得するオブジェクト
	 * @param attrName 属性名称
	 * @return 属性値を格納した配列
	 * @throws Exception
	 */
	public static List<String> getTextAttrs(ObjectDomain obj, String attrName) throws Exception {
		List<String> ret = null;
		AttributeDomain attr = obj.getAttribute(attrName);
		if (attr != null) {
			ret = attr.getTextList();
		}
		return ret;
	}

	/**
	 * オブジェクト属性(数値)を作成します。
	 *
	 * @param objectDomain 属性を付加するオブジェクト
	 * @param attrName 属性名称
	 * @param value 属性値
	 * @throws Exception
	 */
	public static void setAttrLong(ObjectDomain objectDomain, String attrName, long value) throws Exception {

		ApplicationContext context = ApplicationContextLoader.getApplicationContext();
		ObjectService objectService = (ObjectService) context.getBean("objectService4");
		AttributeTypeService attributeTypeService = (AttributeTypeService) context.getBean("attributeTypeService2");

		AttributeTypeDomain attributeTypeDomain = attributeTypeService.getByDefinitionName(attrName);
		objectService.setAttributeSingleLong(objectDomain, attributeTypeDomain, value);

	}

	/**
	 * オブジェクト属性(数値)を複数値で作成します。
	 *
	 * @param objectDomain 属性を付加するオブジェクト
	 * @param attrName 属性名称
	 * @param values 属性値の配列(明細キー順に格納する)
	 * @throws Exception
	 */
	public static void setAttrLongs(ObjectDomain objectDomain, String attrName, List<Long> values) throws Exception {

		ApplicationContext context = ApplicationContextLoader.getApplicationContext();
		ObjectService objectService = (ObjectService) context.getBean("objectService4");
		AttributeTypeService attributeTypeService = (AttributeTypeService) context.getBean("attributeTypeService2");

		AttributeTypeDomain attributeTypeDomain = attributeTypeService.getByDefinitionName(attrName);
		objectService.setAttributeMultipleLong(objectDomain, attributeTypeDomain, values);
	}

	/**
	 * オブジェクト属性(文字列)を複数値で作成します。
	 *
	 * @param objectDomain 属性を付加するオブジェクト
	 * @param attrName 属性名称
	 * @param values 属性値
	 * @throws Exception
	 */
	public static void setAttrStrings(ObjectDomain objectDomain, String attrName, List<String> values) throws Exception {

		ApplicationContext context = ApplicationContextLoader.getApplicationContext();
		ObjectService objectService = (ObjectService) context.getBean("objectService4");
		AttributeTypeService attributeTypeService = (AttributeTypeService) context.getBean("attributeTypeService2");

		AttributeTypeDomain attributeTypeDomain = attributeTypeService.getByDefinitionName(attrName);
		objectService.setAttributeMultipleString(objectDomain, attributeTypeDomain, values);
	}

	/**
	 * 指定のオブジェクトがシステムごみ箱、または、ワークスペースごみ箱に属するか否かを判定します。
	 *
	 * @param obj 指定のオブジェクト
	 * @return ごみ箱に属するか否か
	 * @throws Exception
	 */
	public static boolean isObjectInRecycle(ObjectDomain obj) throws Exception {
		if (obj == null) {
			return true;
		}

		// システムごみ箱のオブジェクトタイプ
		ObjectTypeDomain sysRecycleObjType = AppDocumentLogicUtil.getRecycleObjectType();
		// ワークスペースごみ箱のオブジェクトタイプ
		ObjectTypeDomain wsRecycleObjType = AppDocumentLogicUtil.getWsRecycleObjectType();

		// 指定のオブジェクト自身がごみ箱の場合
		if (obj.getType().getId() == sysRecycleObjType.getId() || obj.getType().getId() == wsRecycleObjType.getId()) {
			return true;
		}

		// ドキュメントのリレーションタイプ
		RelationTypeDomain relType = AppDocumentLogicUtil.getDocumentRelType();
		// ルートオブジェクト
		ObjectDomain rootObj = getRootObject(relType, obj);

		// 指定のオブジェクトのルートに存在するオブジェクトがごみ箱の場合
		if (rootObj != null && sysRecycleObjType.getId() == rootObj.getType().getId()) {
			return true;
		}

		// ワークスペース固有ごみ箱の判定
		if (isObjectInWsRecycle(obj)) {
			return true;
		}

		return false;
	}

	/**
	 * 指定のオブジェクトがシステムごみ箱、または、ワークスペースごみ箱に属するか否かを判定します。
	 *
	 * @param obj 指定のオブジェクト
	 * @return ごみ箱に属するか否か
	 * @throws Exception
	 */
	public static boolean isObjectInRecycleWithoutRecycle(ObjectDomain obj) throws Exception {
		if (obj == null) {
			return true;
		}

		// ごみ箱のオブジェクトタイプ
		ObjectTypeDomain recycleObjType = AppDocumentLogicUtil.getRecycleObjectType();

		// ドキュメントのリレーションタイプ
		RelationTypeDomain relType = AppDocumentLogicUtil.getDocumentRelType();
		// ルートオブジェクト
		ObjectDomain rootObj = getRootObject(relType, obj);

		// 指定のオブジェクトのルートに存在するオブジェクトがごみ箱の場合
		if (rootObj != null && recycleObjType.getId() == rootObj.getType().getId()) {
			return true;
		}

		// ワークスペース固有ごみ箱の判定
		if (isObjectInWsRecycle(obj)) {
			return true;
		}

		return false;
	}

	/**
	 * 指定のオブジェクトがワークスペースごみ箱に属するか否かを判定します。
	 *
	 * @param obj 指定のオブジェクト
	 * @return ごみ箱に属するか否か
	 * @throws Exception
	 */
	public static boolean isObjectInWsRecycle(ObjectDomain obj) throws Exception {
		if (obj == null) {
			return true;
		}

		if (obj.getType() != null && obj.getType().getDefinitionName().equals(EIMConfig.get("OBJECT_TYPE_NAME_WORKSPACE"))) {
			// ワークスペースの場合は属さないためfalseを返す
			return false;
		}
		// パス属性値の取得
		String path = obj.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_PASS")).getStringList().get(0);

		// 「ごみ箱」より前のパスを取得
		int index = path.indexOf("ごみ箱");

		if (index != -1) {
			String pathBeforeRecycle = path.substring(0, index);

			int count = 0;
			// 「/」の個数をカウントする
			String[] ary = pathBeforeRecycle.split("");
			for(int i = 0; i < ary.length; i++){
				if ("/".equals(ary[i])){
					count++;
				}
			}

			// 属性「削除日時」の取得
			AttributeDomain deleteDateAttr = obj.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_DELETE_DATE"));
			
			if (deleteDateAttr == null) {
				return false;
			}
			// 属性「削除日時」の日付型の属性値の取得
			Date deleteDate = deleteDateAttr.getDate();

			if (count == 2 && deleteDate != null) {
				// 「/」が2個なら第2階層が「ごみ箱」となる
				// ごみ箱に属していれば、削除日時を持っている
				return true;
			}
		}
		return false;
	}

	/**
	 * 指定のオブジェクトがシステムごみ箱に属するか否かを判定します。
	 *
	 * @param obj 指定のオブジェクト
	 * @return ごみ箱に属するか否か
	 * @throws Exception
	 */
	public static boolean isObjectInSysRecycle(ObjectDomain obj) throws Exception {
		if (obj == null) {
			return true;
		}

		// ごみ箱のオブジェクトタイプ
		ObjectTypeDomain recycleObjType = AppDocumentLogicUtil.getRecycleObjectType();

		// ドキュメントのリレーションタイプ
		RelationTypeDomain relType = AppDocumentLogicUtil.getDocumentRelType();
		// ルートオブジェクト
		ObjectDomain rootObj = getRootObject(relType, obj);

		// 指定のオブジェクトのルートに存在するオブジェクトがごみ箱の場合
		if (rootObj != null && recycleObjType.getId() == rootObj.getType().getId()) {
			return true;
		}

		return false;
	}
	/**
	 * ルートオブジェクトを取得します。
	 *
	 * @param relType リレーションタイプ
	 * @param childObj 子オブジェクト
	 * @return
	 * @throws Exception
	 */
	private static ObjectDomain getRootObject(RelationTypeDomain relType, ObjectDomain childObj) throws Exception {

		ApplicationContext context = ApplicationContextLoader.getApplicationContext();
		RelationService relationService = (RelationService) context.getBean("relationService2");

		// ルートオブジェクト
		ObjectDomain rootObj = null;
		RelationCriteria relationCriteria = new RelationCriteria();
		// リレーションタイプ
		relationCriteria.setRelationTypeId(relType.getId());
		// 子オブジェクトID
		relationCriteria.setChildObjectId(childObj.getId());

		List<RelationDomain> relationList = relationService.getList(relationCriteria);
		while (relationList != null && relationList.size() > 0) {
			// リレーションドメイン
			RelationDomain relationDomain = relationList.get(0);
			rootObj = relationDomain.getParent();
			// 子オブジェクトID
			relationCriteria.setChildObjectId(rootObj.getId());
			relationList = relationService.getList(relationCriteria);
		}
		return rootObj;
	}

	/**
	 * リレーションタイプを指定して親リレーション一覧を取得します。
	 *
	 * @param relType
	 * @param childObj
	 * @return 親オブジェクト
	 * @throws Exception
	 */
	public static List<RelationDomain> getParentRelationByRelTypeAndChild(RelationTypeDomain relType, ObjectDomain childObj,
			AccessRoleTypeDomain accessRoleTypeDomain) throws Exception {

		ApplicationContext context = ApplicationContextLoader.getApplicationContext();
		RelationService relationService = (RelationService) context.getBean("relationService2");

		SearchSelectRelation selectTarget = new SearchSelectRelation();
		// リレーションタイプ
		SearchConditionGroup conditions = new SearchConditionGroup(SearchOperatorEnum.AND);
		conditions.addCondition(new SearchConditionCompare(SearchOperatorEnum.AND, SearchSelectRelation.PsedoAttributeTypeEnum.TYPE,
				SearchOperatorEnum.EQ, relType.getId()));
		selectTarget.setCondition(conditions);

		// parentObjectSelectTargetを作成する。
		SearchSelectObject parentSearchSelect = new SearchSelectObject();
		// 権限ロールID
		if (accessRoleTypeDomain != null) {
			parentSearchSelect.setAccessRoleType(accessRoleTypeDomain);
		}

		// childObjectSelectTargetを作成する。
		SearchSelectObject childSearchSelect = new SearchSelectObject();
		SearchConditionGroup childConditions = new SearchConditionGroup(SearchOperatorEnum.AND);
		childConditions.addCondition(new SearchConditionCompare(SearchOperatorEnum.AND, SearchSelectObject.PsedoAttributeTypeEnum.ID,
				SearchOperatorEnum.EQ, childObj.getId()));
		childSearchSelect.setCondition(childConditions);

		List<RelationDomain> relationList = relationService.getList(selectTarget, parentSearchSelect, childSearchSelect, null);

		return relationList;
	}

	/**
	 * 指定オブジェクトの下位にある全てのオブジェクトのリストを返します。（アクセス権限の指定はしない）
	 *
	 * @param relType
	 * @param parentObj
	 * @return 子オブジェクトのリスト
	 * @throws Exception
	 */
	public static List<ObjectDomain> getChildObjectByRelTypeAndParent(RelationTypeDomain relType, ObjectDomain parentObj, AccessRoleTypeDomain accessRoleTypeDomain) throws Exception {

		List<RelationDomain> relationList = getChildRelationListByRelType(relType, parentObj, accessRoleTypeDomain);
		List<ObjectDomain> childObjs = new ArrayList<ObjectDomain>();
		if (relationList != null && relationList.size() > 0) {
			for (RelationDomain relationDomain : relationList) {
				childObjs.add(relationDomain.getChild());
			}
		}

		return childObjs;
	}

	/**
	 * 指定オブジェクトの下位にあり、ログインユーザーがアクセス可能なリレーションのリストを返します。
	 *
	 * @param relType
	 * @param parentObj
	 * @return 子リレーションのリスト
	 * @throws Exception
	 */
	public static List<RelationDomain> getChildRelationListByRelType(RelationTypeDomain relType, ObjectDomain parentObj,
			AccessRoleTypeDomain accessRoleTypeDomain) throws Exception {

		ApplicationContext context = ApplicationContextLoader.getApplicationContext();
		RelationService relationService = (RelationService) context.getBean("relationService2");

		SearchSelectRelation selectTarget = new SearchSelectRelation();
		// リレーションタイプ
		SearchConditionGroup conditions = new SearchConditionGroup(SearchOperatorEnum.AND);
		conditions.addCondition(new SearchConditionCompare(SearchOperatorEnum.AND, SearchSelectRelation.PsedoAttributeTypeEnum.TYPE,
				SearchOperatorEnum.EQ, relType.getId()));
		selectTarget.setCondition(conditions);

		// parentObjectSelectTargetを作成する。
		SearchSelectObject parentSearchSelect = new SearchSelectObject();
		SearchConditionGroup parentConditions = new SearchConditionGroup(SearchOperatorEnum.AND);
		parentConditions.addCondition(new SearchConditionCompare(SearchOperatorEnum.AND, SearchSelectObject.PsedoAttributeTypeEnum.ID,
				SearchOperatorEnum.EQ, parentObj.getId()));
		parentSearchSelect.setCondition(parentConditions);

		// childObjectSelectTargetを作成する。
		SearchSelectObject childSearchSelect = new SearchSelectObject();
		// 権限ロールID
		if (accessRoleTypeDomain != null) {
			childSearchSelect.setAccessRoleType(accessRoleTypeDomain);
		}

		List<RelationDomain> relationList = relationService.getList(selectTarget, parentSearchSelect, childSearchSelect, null);

		return relationList;
	}

	/**
	 * ドキュメントもしくはフォルダオブジェクトにパス属性を設定します。
	 *
	 * @param object ドキュメントもしくはフォルダオブジェクト
	 * @return 設定するパス
	 * @throws Exception
	 */
	public static String getPath(ObjectDomain object) throws Exception {

		String path = null;

		if (object.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PASS")) != null) {
			// 先頭のパス属性を返却する
			path = object.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PASS")).getStringList().get(0);
		}
		return path;
	}

	/**
	 * ドキュメントもしくはフォルダオブジェクトにパス属性を設定します。
	 *
	 * @param objectDomain ドキュメントもしくはフォルダオブジェクト
	 * @param path 設定するパス
	 * @throws Exception
	 */
	public static void setPath(ObjectDomain objectDomain, String path) throws Exception {

		List<String> values = new ArrayList<String>();
		values.add(path);

		AttributeTypeDomain attributeType = AppDocumentLogicUtil.getDocPathAttributeType();
		setAttributeStrings(objectDomain, attributeType, values);
	}

	/**
	 * オブジェクト属性を作成します。
	 *
	 * @param objectDomain  属性を付加するオブジェクト
	 * @param attr 属性
	 * @throws Exception
	 */
	public static void setAttr(ObjectDomain objectDomain, AttributeDomain attr) throws Exception {

		ApplicationContext context = ApplicationContextLoader.getApplicationContext();
		ObjectService objectService = (ObjectService) context.getBean("objectService4");

		AttributeTypeDomain attributeType = attr.getAttributeType();
		switch (attributeType.getValueType()) {
		case LONG:
			if (!attributeType.isMultiple()) {
				objectService.setAttributeSingleLong(objectDomain, attributeType, attr.getLong());
			} else {
				objectService.setAttributeMultipleLong(objectDomain, attributeType, attr.getLongList());
			}
			break;
		case STRING:
			if (!attributeType.isMultiple()) {
				objectService.setAttributeSingleString(objectDomain, attributeType, attr.getString());
			} else {
				objectService.setAttributeMultipleString(objectDomain, attributeType, attr.getStringList());
			}
			break;
		case DATE:
			if (!attributeType.isMultiple()) {
				objectService.setAttributeSingleDate(objectDomain, attributeType, attr.getDate());
			} else {
				objectService.setAttributeMultipleDate(objectDomain, attributeType, attr.getDateList());
			}
			break;
		case TEXT:
			if (!attributeType.isMultiple()) {
				objectService.setAttributeSingleText(objectDomain, attributeType, attr.getText());
			} else {
				objectService.setAttributeMultipleText(objectDomain, attributeType, attr.getTextList());
			}
			break;
		case DOUBLE:
			if (!attributeType.isMultiple()) {
				objectService.setAttributeSingleDouble(objectDomain, attributeType, attr.getDouble());
			} else {
				objectService.setAttributeMultipleDouble(objectDomain, attributeType, attr.getDoubleList());
			}
			break;
		case USER:
			if (!attributeType.isMultiple()) {
				objectService.setAttributeSingleUser(objectDomain, attributeType, attr.getUser());
			} else {
				objectService.setAttributeMultipleUser(objectDomain, attributeType, attr.getUserList());
			}
			break;
		}
	}

	/**
	 * 対象オブジェクトを行ロックします。
	 *
	 * @param revObjectMap 行ロックするキー：履歴、値：ObjectDomainのマップ
	 * @throws Exception
	 */
	public static void lockObjects(Map<Long, ObjectDomain> revObjectMap) throws Exception {
		List<Long> objectIdList = new ArrayList<Long>();
		for(Map.Entry<Long, ObjectDomain> entry : revObjectMap.entrySet()){
			ObjectDomain objectDomain = entry.getValue();
			objectIdList.add(objectDomain.getId());
		}

		AppDocumentUtil.lockObjects(objectIdList.toArray(new Long[1]));
	}

	/**
	 * 対象オブジェクトを行ロックします。
	 *
	 * @param objIds 行ロックするオブジェクトのIDの配列
	 * @throws Exception
	 */
	public static void lockObjects(Long[] objIds) throws Exception {
		ApplicationContext context = ApplicationContextLoader.getApplicationContext();
		ObjectService objectService = (ObjectService) context.getBean("objectService4");

		// SearchUtils.searchObjects()を使用して、排他ロックをかける
		SearchSelectObject searchSelectObject = new SearchSelectObject();
		searchSelectObject.setLockMode(LockModeEnum.ROWSHARE);
		SearchConditionBuildHelper helper = new SearchConditionBuildHelper();
		SearchConditionGroup searchConditionGroup = helper.group(helper.opOr());

		// IN句生成
		searchConditionGroup
				.addCondition(helper.inArray(helper.opAnd(), SearchSelectObject.PsedoAttributeTypeEnum.ID, SearchOperatorEnum.IN, objIds));
		searchSelectObject.setResultAttrs(new ArrayList<AttributeTypeDomain>());
		searchSelectObject.setCondition(searchConditionGroup);

		objectService.getList(searchSelectObject, null);
	}

	/**
	 * 属性値を削除します。
	 *
	 * @param objectDomain 属性を削除するオブジェクト
	 * @param attrName 属性名称
	 * @throws Exception
	 */
	public static void deleteAttribute(ObjectDomain objectDomain, String attrName) throws Exception {

		ApplicationContext context = ApplicationContextLoader.getApplicationContext();
		ObjectService objectService = (ObjectService) context.getBean("objectService4");
		AttributeTypeService attributeTypeService = (AttributeTypeService) context.getBean("attributeTypeService2");

		AttributeTypeDomain attributeTypeDomain = attributeTypeService.getByDefinitionName(attrName);
		objectService.removeAttribute(objectDomain, attributeTypeDomain);
	}

	/**
	 * 指定したオブジェクトをロックを解除します。
	 * ロックを行ったユーザ又はシステム管理者のみロック解除可能です。
	 *
	 * @param objectDomain
	 * @param authorityCheck
	 * @return
	 * @throws Exception
	 */
	public static ObjectDomain unLock(ObjectDomain objectDomain) throws Exception {

		try {
			ApplicationContext context = ApplicationContextLoader.getApplicationContext();
			ObjectDao objectDao = (ObjectDao) context.getBean("objectDaoForUtilForLock");

			// 対象オブジェクトの取得
			ObjectDomain targetObject = objectDao.getById(objectDomain.getId());
			if (targetObject == null) {
				return null;
			}

			// ロックユーザーの設定
			targetObject.setLockUser(null);

			// オブジェクト更新
			objectDao.update(targetObject, DuplicateCheckModeEnum.NONE);

			// Result
			ObjectService objectService = (ObjectService) context.getBean("objectService4");
			return objectService.getById(objectDomain.getId());
		} catch (jp.co.ctc_g.eim.framework2.common.exception.EIMException e) {
			// Daoの例外をUtilの例外に変換
			EIMException ee = new EIMException(e.getMessageKey());
			ee.initCause(e);
			throw ee;
		}
	}

	/**
	 * オブジェクト属性(文字列)を強制的に設定します。
	 *
	 * @param objectDomain 属性を付加するオブジェクト
	 * @param attributeType 属性タイプ
	 * @param values 属性値の配列
	 * @throws Exception
	 */
    public static void setAttributeStrings(ObjectDomain objectDomain, AttributeTypeDomain attributeType, List<String> values) throws Exception {

    	ApplicationContext context = ApplicationContextLoader.getApplicationContext();
		ObjectService objectService = (ObjectService) context.getBean("objectService4");

		if (values == null || values.size() == 0) {
			objectService.removeAttribute(objectDomain, attributeType);
			return;
		}

		if (attributeType.isMultiple()) {
			objectService.removeAttribute(objectDomain, attributeType);
			// Which String or Text, Call each Procedure
			ValueTypeEnum ValueTypeId = attributeType.getValueType();
			if (ValueTypeId == ValueTypeEnum.STRING)
				objectService.setAttributeMultipleString(objectDomain, attributeType, values);
			else if (ValueTypeId == ValueTypeEnum.TEXT)
				objectService.setAttributeMultipleText(objectDomain, attributeType, values);
		} else {
			if (values.size() > 1) {
				throw new EIMException("EIM.ERROR.LOGIC.SINGLE.ATTRIBUTE.TYPE", objectDomain.getName());
			}
			// Which String or Text, Call each Procedure
			ValueTypeEnum ValueTypeId = attributeType.getValueType();
			if (ValueTypeId == ValueTypeEnum.STRING)
				objectService.setAttributeSingleString(objectDomain, attributeType, values.get(0));
			else if (ValueTypeId == ValueTypeEnum.TEXT)
				objectService.setAttributeSingleText(objectDomain, attributeType, values.get(0));
		}
	}

    /**
	 * オブジェクト属性(数値)を強制的に設定します。
	 *
	 * @param objectDomain 属性を付加するオブジェクト
	 * @param attributeType 属性タイプ
	 * @param values 属性値の配列
	 * @throws Exception
	 */
    public static void setAttributeLongs(ObjectDomain objectDomain, AttributeTypeDomain attributeType, List<Long> values) throws Exception {

    	ApplicationContext context = ApplicationContextLoader.getApplicationContext();
		ObjectService objectService = (ObjectService) context.getBean("objectService4");

		if (values == null || values.size() == 0) {
			objectService.removeAttribute(objectDomain, attributeType);
			return;
		}

		if (attributeType.isMultiple()) {
			objectService.removeAttribute(objectDomain, attributeType);

			objectService.setAttributeMultipleLong(objectDomain, attributeType, values);

		} else {
			if (values.size() > 1) {
				throw new EIMException("EIM.ERROR.LOGIC.SINGLE.ATTRIBUTE.TYPE", objectDomain.getName());
			}

			objectService.setAttributeSingleLong(objectDomain, attributeType, values.get(0));

		}
	}

    /**
	 * オブジェクト属性(日付)を強制的に設定します。
	 *
	 * @param objectDomain 属性を付加するオブジェクト
	 * @param attributeType 属性タイプ
	 * @param values 属性値の配列
	 * @throws Exception
	 */
    public static void setAttributeDates(ObjectDomain objectDomain, AttributeTypeDomain attributeType, List<Date> values) throws Exception {
    	ApplicationContext context = ApplicationContextLoader.getApplicationContext();
		ObjectService objectService = (ObjectService) context.getBean("objectService4");

		if (values == null || values.size() == 0) {
			objectService.removeAttribute(objectDomain, attributeType);
			return;
		}

		if (attributeType.isMultiple()) {
			objectService.removeAttribute(objectDomain, attributeType);

			objectService.setAttributeMultipleDate(objectDomain, attributeType, values);

		} else {
			if (values.size() > 1) {
				throw new EIMException("EIM.ERROR.LOGIC.SINGLE.ATTRIBUTE.TYPE", objectDomain.getName());
			}
			objectService.setAttributeSingleDate(objectDomain, attributeType, values.get(0));

		}
	}

    /**
	 * オブジェクト属性(double数値)を強制的に設定します。
	 *
	 * @param objectDomain 属性を付加するオブジェクト
	 * @param attributeType 属性タイプ
	 * @param values 属性値の配列
	 * @throws Exception
	 */
    public static void setAttributeDoubles(ObjectDomain objectDomain, AttributeTypeDomain attributeType, List<Double> values) throws Exception {

    	ApplicationContext context = ApplicationContextLoader.getApplicationContext();
		ObjectService objectService = (ObjectService) context.getBean("objectService4");

		if (values == null || values.size() == 0) {
			objectService.removeAttribute(objectDomain, attributeType);
			return;
		}

		if (attributeType.isMultiple()) {
			objectService.removeAttribute(objectDomain, attributeType);

			objectService.setAttributeMultipleDouble(objectDomain, attributeType, values);

		} else {
			if (values.size() > 1) {
				throw new EIMException("EIM.ERROR.LOGIC.SINGLE.ATTRIBUTE.TYPE", objectDomain.getName());
			}
			objectService.setAttributeSingleDouble(objectDomain, attributeType, values.get(0));

		}
	}

    /**
     * オブジェクトを削除します(物理削除)。
     *
     * @param object
     * @throws Exception
     */
	public static void deleteObjectAll(ObjectDomain object) throws Exception {

		ApplicationContext context = ApplicationContextLoader.getApplicationContext();
		ObjectService objectService = (ObjectService) context.getBean("objectService4");

		// ステータスを持つものは、ワークフロー関連のオブジェクトを削除
		// ただしワークフロー付きフォルダ下を除く
		if (object.getStatus() != null && !AppDocumentLogicUtil.isUnderFolderWithWorkflow(object)) {
			AppDocumentUtil.deleteWFRelatedObject(object);
		}

		// 物理削除
		deleteFiles(object);
		objectService.delete(object);

	}

	/**
	 * 対象オブジェクトのワークフロー関連のオブジェクトを削除します。
	 * <br>
	 * 対象オブジェクトがnullの場合は何もしません。<br>
	 * 以下のオブジェクトを削除します。
	 * <li>承認依頼
	 * <li>承認依頼通知
	 * <li>公開通知
	 * <li>通知先
	 * <li>受信確認
	 *
	 * @param object 対象オブジェクト
	 * @throws Exception
	 */
	public static void deleteWFRelatedObject(ObjectDomain object) throws Exception {

		if (object == null) {
			return;
		}

		ApplicationContext context = ApplicationContextLoader.getApplicationContext();
		ObjectService objectService = (ObjectService) context.getBean("objectService4");
		ObjectTypeService objectTypeService = (ObjectTypeService) context.getBean("objectTypeService2");

		String objId = String.valueOf(object.getId());
		// 下記4行はオブジェクトタイプ自体が存在しないため削除できる
//		deleteObject(ConfigUtils.getByKey("OBJECT_TYPE_NAME_REQUEST"), objId); // 承認依頼
//		deleteObject(ConfigUtils.getByKey("OBJECT_TYPE_NAME_REQUESTMAIL"), objId); // 承認依頼通知
//		deleteObject(ConfigUtils.getByKey("OBJECT_TYPE_NAME_PUBLICMAIL"), objId); // 公開通知
//		deleteObject(ConfigUtils.getByKey("OBJECT_TYPE_NAME_NOTIFY_TO"), objId); // 通知先
		// 受信確認
		ObjectTypeDomain objectTypeDomain = objectTypeService.getByDefinitionName(ConfigUtils.getByKey("OBJECT_TYPE_NAME_RECEIVE"));
		ObjectCriteria objectCriteria = new ObjectCriteria();
		// オブジェクトタイプ
		objectCriteria.setObjectTypeId(objectTypeDomain.getId());
		// オブジェクト名称
		objectCriteria.setName(objId + ".%");
		// デフォルト設定
		objectCriteria.setLatest(true);
		objectCriteria.setSubClasses(false);
		objectCriteria.setAccessRoleType(new AccessRoleTypeDomain("READ"));

		List<ObjectDomain> objList = objectService.getList(objectCriteria);
		if (objList != null) {
			for (ObjectDomain obj : objList) {
				deleteFiles(obj);
				objectService.delete(obj);
			}
		}
	}

	/**
	 * ファイルを削除します。
	 *
	 * @param object
	 * @throws Exception
	 */
	private static void deleteFiles(ObjectDomain object) throws Exception {
		ApplicationContext context = ApplicationContextLoader.getApplicationContext();
		FormatService formatService = (FormatService) context.getBean("formatService2");
		FileDao fileDao = (FileDao) context.getBean("fileDao2");

		// Format List
		List<FormatDomain> formatList = new ArrayList<FormatDomain>();

		// 公開ドキュメント
		FormatDomain publicDocumentFormat = formatService.getByDefinitionName(ConfigUtils.getByKey("FORMAT_NAME_PUBLIC"));
		formatList.add(publicDocumentFormat);
		// サムネイル
		FormatDomain thumbnailFormat = formatService.getByDefinitionName(ConfigUtils.getByKey("FORMAT_NAME_THUMBNAIL"));
		formatList.add(thumbnailFormat);
		// プレビュー
		FormatDomain previewFormat = formatService.getByDefinitionName(ConfigUtils.getByKey("FORMAT_NAME_PREVIEW"));
		formatList.add(previewFormat);

		ObjectTypeDomain objectTypeDomain = object.getType();
		while (objectTypeDomain != null) {
			FormatDomain formatDomain = formatService.getDefaultByObjectType(objectTypeDomain);
			if (formatDomain != null) {
				formatList.add(formatDomain);
			}
			objectTypeDomain = objectTypeDomain.getParent();
		}

		// File List
		List<File> fileList = new ArrayList<File>();
		for (FormatDomain format : formatList) {
			// File
			FileDomain file = fileDao.getByObjectAndFormat(object, format);
			if (file != null)
				fileList.add(new File(file.getDirectory().getPath() + FileUtil.getFileName(object, file)));
		}

		// Delete Files
		for (int i = 0; i < fileList.size(); i++) {
			File file = (File) fileList.get(i);
			file.delete();
		}
	}

//	/**
//	 * オブジェクトを属性も含めて削除します。
//	 *
//	 * @param typeName オブジェクトタイプ名称
//	 * @param name オブジェクト名称
//	 * @throws Exception
//	 */
//	private static void deleteObject(String typeName, String name) throws Exception {
//
//		ApplicationContext context = ApplicationContextLoader.getApplicationContext();
//		ObjectService objectService = (ObjectService) context.getBean("objectService2");
//		ObjectTypeService objectTypeService = (ObjectTypeService) context.getBean("objectTypeService2");
//
//		ObjectTypeDomain objectTypeDomain = objectTypeService.getByDefinitionName(typeName);
//
//		ObjectCriteria objectCriteria = new ObjectCriteria();
//		// オブジェクトタイプ
//		objectCriteria.setObjectTypeId(objectTypeDomain.getId());
//		// オブジェクト名称
//		objectCriteria.setName(name);
//		// デフォルト設定
//		objectCriteria.setLatest(true);
//		objectCriteria.setSubClasses(false);
//		objectCriteria.setAccessRoleType(new AccessRoleTypeDomain("READ"));
//
//		List<ObjectDomain> objList = objectService.getList(objectCriteria);
//		if (objList != null) {
//			for (ObjectDomain obj : objList) {
//				objectService.delete(obj);
//			}
//		}
//	}

	/**
	 * 比較元フォルダが探索対象フォルダに属するか否かを判定します。
	 * <li>再帰的に探索します。
	 *
	 * @param relType
	 * @param baseFolder 比較元フォルダ
	 * @param targetFolder 探索対象フォルダ
	 * @return 比較元フォルダが探索対象フォルダに属する場合はtrue、属しない場合はfalse
	 * @throws Exception
	 */
	public static boolean isChildFolder(RelationTypeDomain relType, ObjectDomain baseFolder, ObjectDomain targetFolder) throws Exception {

		// 対象がフォルダの場合のみ判定
		if (baseFolder != null && targetFolder != null && targetFolder.getType() != null
				&& targetFolder.getType().getDefinitionName().equals(ConfigUtils.getByKey("OBJECT_TYPE_NAME_FOLDER"))) {

			// 子フォルダ一覧の取得
			List<ObjectDomain> childFolders = getChildObjectByRelTypeAndParent(relType, targetFolder, null);
			if (childFolders.size() > 0) {
				for (int i = 0; i < childFolders.size(); i++) {

					// 子フォルダとIDが一致したらtrueを返却、そうでない場合は再帰的に子フォルダを判定
					if (childFolders.get(i).getId() == baseFolder.getId() || isChildFolder(relType, baseFolder, childFolders.get(i))) {
						return true;
					}
				}
			}
		}
		return false;
	}

}
