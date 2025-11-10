package jp.co.ctc_g.eim.app.document.common.util;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.springframework.context.ApplicationContext;

import jp.co.ctc_g.eim.framework2.business.domain.entity.AccessRoleTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchConditionGroup;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchSelectObject;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchSelectObject.SearchConditionBuildHelper;
import jp.co.ctc_g.eim.framework2.business.service.AttributeTypeService;
import jp.co.ctc_g.eim.framework2.business.service.ObjectService;
import jp.co.ctc_g.eim.framework2.business.service.ObjectTypeService;
import jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;

/**
 * 【ドキュメントAPI】
 * タグ関連共通クラス
 *
 */
public class TagUtil {

	/**
	 * 指定タグの付与された全てのObjectDomainを返却します。
	 *
	 * @param object 「タグ」オブジェクト
	 * @return 指定タグが付与された全てのObjectDomainのリスト
	 * @throws Exception
	 */
	public static List<ObjectDomain> getTagGivenObj(ObjectDomain object) throws Exception {

		ApplicationContext context = ApplicationContextLoader.getApplicationContext();
		AttributeTypeService attributeTypeService = (AttributeTypeService) context.getBean("attributeTypeService2");
		ObjectService objectService = (ObjectService) context.getBean("objectService2");
		ObjectTypeService objectTypeService = (ObjectTypeService) context.getBean("objectTypeService2");

		SearchSelectObject searchSelectObject = new SearchSelectObject();
		SearchConditionBuildHelper helper = new SearchConditionBuildHelper();
		SearchConditionGroup searchConditionGroup = helper.group(helper.opAnd());

		// 検索条件１：オブジェクトタイプが「ドキュメント」「フォルダ」「タグ」のいずれか
		ObjectTypeDomain docObjType = AppDocumentLogicUtil.getDocumentObjectType();
		List<Long> docObjTypeIds = AppDocumentLogicUtil.getChildObjectTypeList(docObjType.getId());

		ObjectTypeDomain folderObjType = AppDocumentLogicUtil.getFolderObjectType();
		List<Long> folderObjTypeIds = AppDocumentLogicUtil.getChildObjectTypeList(folderObjType.getId());

		ObjectTypeDomain tagObjType = objectTypeService.getByDefinitionName(ConfigUtils.getByKey("OBJECT_TYPE_NAME_TAG"));
		List<Long> tagObjTypeIds = AppDocumentLogicUtil.getChildObjectTypeList(tagObjType.getId());

		List<Long> objTypeIds = new ArrayList<Long>();
		objTypeIds.addAll(docObjTypeIds);
		objTypeIds.addAll(folderObjTypeIds);
		objTypeIds.addAll(tagObjTypeIds);

		searchConditionGroup.addCondition(helper.inArray(helper.opAnd(), SearchSelectObject.PsedoAttributeTypeEnum.TYPE, helper.opIn(), objTypeIds
				.toArray()));

		// 「タグ」属性条件設定
		AttributeTypeDomain attributeType = attributeTypeService.getByDefinitionName(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_TAG"));
		// 検索条件２：属性「タグ」が<オブジェクトID>
		searchConditionGroup.addCondition(helper.eq(helper.opAnd(), attributeType, object.getId()));

		searchSelectObject.setCondition(searchConditionGroup);
		searchSelectObject.setAccessRoleType(new AccessRoleTypeDomain("READ"));

		// タグが割り当てられているオブジェクト一覧を取得
		List<ObjectDomain> resultList = objectService.getList(searchSelectObject, null);
		return resultList;
	}

	/**
	 * 指定されたオブジェクトからタグを解除する。
	 *
	 * @param targetObj タグ解除対象のオブジェクト
	 * @param tagObj 付与するタグのオブジェクト
	 * @throws Exception
	 */
	public static void removeTag(ObjectDomain targetObj, ObjectDomain tagObj) throws Exception {
		ApplicationContext context = ApplicationContextLoader.getApplicationContext();
		AttributeTypeService attributeTypeService = (AttributeTypeService) context.getBean("attributeTypeService2");

		AttributeTypeDomain attrTag = attributeTypeService.getByDefinitionName(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_TAG"));
		AttributeTypeDomain attrUser = attributeTypeService.getByDefinitionName(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_TAG_GIVER"));
		AttributeTypeDomain attrDate = attributeTypeService.getByDefinitionName(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_TAG_GIVENDATE"));

		removeTagRecursiveInternal(targetObj, tagObj, attrTag, attrUser, attrDate);
	}

	/**
	 * 指定されたオブジェクトからタグを解除する。
	 *
	 * @param targetObj タグ解除対象のオブジェクト
	 * @param tagObj 付与するタグのオブジェクト
	 * @param attrTag 「タグ」属性
	 * @param attrUser 「タグ付与者」属性
	 * @param attrDate 「タグ付与日」属性
	 * @throws Exception
	 */
	private static void removeTagRecursiveInternal(ObjectDomain targetObj, ObjectDomain tagObj, AttributeTypeDomain attrTag,
			AttributeTypeDomain attrUser, AttributeTypeDomain attrDate) throws Exception {
		ApplicationContext context = ApplicationContextLoader.getApplicationContext();
		ObjectService objectService = (ObjectService) context.getBean("objectService2");

		// タグ付与対象のオブジェクトから「タグ」「タグ付与者」「タグ付与日」属性を取得
		List<Long> tagList = AppDocumentUtil.getIntAttrs(targetObj, ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_TAG"));
		List<Long> userList = AppDocumentUtil.getIntAttrs(targetObj, ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_TAG_GIVER"));
		List<Date> dateList = AppDocumentUtil.getDateAttrs(targetObj, ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_TAG_GIVENDATE"));

		// この条件に合致しない場合は削除を行わない
		if (tagList != null && userList != null && dateList != null && tagList.size() == userList.size() && userList.size() == dateList.size()) {
			// 削除対象のタグの位置を探す
			int nExistKey = -1;
			for (int i = 0; i < tagList.size(); i++) {
				if (tagList.get(i) == tagObj.getId()) {
					nExistKey = i;
					break;
				}
			}

			if (nExistKey == 0 && tagList.size() == 1) { // 最後のタグ属性値
				// 属性の削除
				objectService.removeAttribute(targetObj, attrTag);
				objectService.removeAttribute(targetObj, attrUser);
				objectService.removeAttribute(targetObj, attrDate);
			} else if (nExistKey != -1) {
				tagList.remove(nExistKey);
				userList.remove(nExistKey);
				dateList.remove(nExistKey);

				// 属性の再設定
				AppDocumentUtil.setAttributeLongs(targetObj, attrTag, tagList);
				AppDocumentUtil.setAttributeLongs(targetObj, attrUser, userList);
				AppDocumentUtil.setAttributeDates(targetObj, attrDate, dateList);
			}
		}
	}

}
