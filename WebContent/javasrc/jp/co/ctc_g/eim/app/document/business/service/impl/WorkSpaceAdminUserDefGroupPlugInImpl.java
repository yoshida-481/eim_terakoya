package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.util.ArrayList;
import java.util.List;

import eim.util.EIMConfig;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.RelationDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchConditionCompare;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchConditionGroup;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchConditionIn;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchOperatorEnum;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchSelectObject;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchSelectObject.PsedoAttributeTypeEnum;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchSelectObject.SearchConditionBuildHelper;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchSelectRelation;
import jp.co.ctc_g.eim.framework2.business.plugin.impl.AbstractUserDefGroupPlugInImpl;
import jp.co.ctc_g.eim.framework2.business.service.AttributeTypeService;
import jp.co.ctc_g.eim.framework2.business.service.ObjectTypeService;
import jp.co.ctc_g.eim.framework2.business.service.RelationService;
import jp.co.ctc_g.eim.framework2.common.exception.EIMException;

/**
 * ユーザ定義グループ「ワークスペース管理者」の実装<br>
 *
 * ワークスペース毎のワークスペース管理者を取得する。
 *
 * @see jp.co.ctc_g.eim.framework2.business.plugin.impl.AbstractUserDefGroupPlugInImpl
 * @since Ver1.0
 */
public class WorkSpaceAdminUserDefGroupPlugInImpl extends AbstractUserDefGroupPlugInImpl{

	/** オブジェクトタイプサービス */
	private ObjectTypeService objectTypeService;

	/** 属性タイプサービスタイプサービス */
	private AttributeTypeService attributeTypeService;

	/** リレーションサービス */
	private RelationService relationService;


	@Override
	/**
	 * @see jp.co.ctc_g.eim.framework2.business.plugin.impl.AbstractUserDefGroupPlugInImpl#getUserListByObject(jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain)
	 * @since Ver 1.0
	 */
	public List<UserDomain> getUserListByObject(ObjectDomain object) throws Exception {
		return null;
	}

	@Override
	/**
	 * @see jp.co.ctc_g.eim.framework2.business.plugin.impl.AbstractUserDefGroupPlugInImpl#getSearchConditionByUser(jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain)
	 * @since Ver 1.0
	 */
	public SearchConditionGroup getSearchConditionByUser(UserDomain user) throws Exception {
		// パラメータチェック
		if (user == null) {

			throw new EIMException("EIM.ERROR.LOGIC.USER.VALUE.ILLEGAL");
		}

		// 検索条件構築のヘルパー
		SearchConditionBuildHelper h = new SearchConditionBuildHelper();

		SearchSelectRelation selectTarget = new SearchSelectRelation();
		selectTarget.setResultAttrs(new ArrayList<>());

		// parentObjectSelectTargetを作成する
		// オブジェクトタイプがワークスペース かつ 責任者属性がログインユーザーのIDと一致
		ObjectTypeDomain wsOjectTypeDomain = objectTypeService.getByDefinitionName(EIMConfig.get("OBJECT_TYPE_NAME_WORKSPACE"));
		long wsObjTypeId = wsOjectTypeDomain.getId();

		AttributeTypeDomain wsAdminAttrType = attributeTypeService.getByDefinitionName(EIMConfig.get("ATTR_NAME_WORKSPACE_ADMINISTRATOR"));

		SearchSelectObject parentSearchSelect = new SearchSelectObject();
		SearchConditionGroup parentConditions = new SearchConditionGroup(SearchOperatorEnum.AND);
		parentConditions.addCondition(new SearchConditionCompare(SearchOperatorEnum.AND, SearchSelectObject.PsedoAttributeTypeEnum.TYPE,
				SearchOperatorEnum.EQ, wsObjTypeId));
		parentConditions.addCondition(new SearchConditionCompare(SearchOperatorEnum.AND, wsAdminAttrType, SearchOperatorEnum.LIKE, "user.getId()"));
		parentSearchSelect.setCondition(parentConditions);
		parentSearchSelect.setResultAttrs(new ArrayList<>());

		// childObjectSelectTargetを作成する
		// オブジェクトタイプがワークスペース固有ごみ箱
		ObjectTypeDomain wsRecycleObjType = objectTypeService.getByDefinitionName(EIMConfig.get("OBJECT_TYPE_NAME_WORKSPACERECYCLE"));

		SearchSelectObject childSearchSelect = new SearchSelectObject();
		SearchConditionGroup childConditions = new SearchConditionGroup(SearchOperatorEnum.AND);
		childConditions.addCondition(new SearchConditionCompare(SearchOperatorEnum.AND, SearchSelectObject.PsedoAttributeTypeEnum.TYPE,
				SearchOperatorEnum.EQ, wsRecycleObjType.getId()));
		childSearchSelect.setCondition(childConditions);
		childSearchSelect.setResultAttrs(new ArrayList<>());

		// 自身が管理者となっているワークスペースとごみ箱のリレーション一覧を取得
		List<RelationDomain> wsRecycleRelationList = relationService.getList(selectTarget, parentSearchSelect, childSearchSelect, null);

		// 管理対象のワークスペースが一件もない場合
		if(wsRecycleRelationList.size() <= 0){
			// 常に偽となる検索条件を返却する
			return h.group(h.opAnd()).addCondition(h.eq(h.opAnd(), PsedoAttributeTypeEnum.ID, 0));
		}

		// ワークスペース固有ごみ箱のIDを取得
		long[] wsRecycleObjIds = new long[wsRecycleRelationList.size()];
		for(int i = 0; i < wsRecycleRelationList.size(); i++){
			RelationDomain wsRecycleRelation = wsRecycleRelationList.get(i);
			wsRecycleObjIds[i] = wsRecycleRelation.getChild().getId();
		}

		// ワークスペース固有ごみ箱オブジェクトのIDを返却
		return h.group(h.opAnd()).addCondition(
				new SearchConditionIn(h.opAnd(), PsedoAttributeTypeEnum.ID, SearchOperatorEnum.IN, wsRecycleObjIds));
	}

	/**
	 * オブジェクトタイプサービスを取得します。
	 * @return オブジェクトタイプサービスクラス
	 */
	public ObjectTypeService getobjectTypeService() {
	    return objectTypeService;
	}

	/**
	 * オブジェクトタイプサービスを設定します。
	 * @param objectTypeService オブジェクトタイプサービスクラス
	 */
	public void setObjectTypeService (ObjectTypeService objectTypeService) {
	    this.objectTypeService = objectTypeService;
	}

	/**
	 * 属性タイプサービスを取得します。
	 * @return 属性タイプサービスクラス
	 */
	public AttributeTypeService getattributeTypeService() {
	    return attributeTypeService;
	}

	/**
	 * 属性タイプサービスを設定します。
	 * @param attributeTypeService 属性タイプサービスクラス
	 */
	public void setAttributeTypeService (AttributeTypeService attributeTypeService) {
	    this.attributeTypeService = attributeTypeService;
	}

	/**
	 * リレーションサービスを取得します。
	 * @return リレーションサービスクラス
	 */
	public RelationService getRelationService() {
	    return relationService;
	}

	/**
	 * リレーションサービスを設定します。
	 * @param relationService リレーションサービスクラス
	 */
	public void setRelationService (RelationService relationService) {
	    this.relationService = relationService;
	}



}
