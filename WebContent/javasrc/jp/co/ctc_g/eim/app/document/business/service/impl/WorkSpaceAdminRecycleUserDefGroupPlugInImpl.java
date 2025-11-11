package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.util.ArrayList;
import java.util.List;

import eim.bo.EIMComp;
import eim.bo.EIMUser;
import eim.net.EIMSession;
import eim.util.CompUtils;
import eim.util.EIMConfig;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.GroupDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.RelationDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.RoleDomain;
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
import jp.co.ctc_g.eim.framework2.business.service.UserService;
import jp.co.ctc_g.eim.framework2.common.exception.EIMException;

/**
 * ユーザ定義グループ「ワークスペース管理者」(ごみ箱表示用)の実装<br>
 *
 * ワークスペース毎のワークスペース管理者を取得する。
 *
 * @see jp.co.ctc_g.eim.framework2.business.plugin.impl.AbstractUserDefGroupPlugInImpl
 * @since Ver1.0
 */
public class WorkSpaceAdminRecycleUserDefGroupPlugInImpl extends AbstractUserDefGroupPlugInImpl{

	/** オブジェクトタイプサービス */
	private ObjectTypeService objectTypeService;

	/** 属性タイプサービスタイプサービス */
	private AttributeTypeService attributeTypeService;

	/** リレーションサービス */
	private RelationService relationService;

	/** ユーザサービス */
	private UserService userService;

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
		EIMSession sess = EIMThreadContext.getEIMSession();
		// 検索条件構築のヘルパー
		SearchConditionBuildHelper h = new SearchConditionBuildHelper();

		SearchSelectRelation selectTarget = new SearchSelectRelation();
		selectTarget.setResultAttrs(new ArrayList<>());

		// 自身が管理者となっているワークスペースとごみ箱のリレーション一覧を取得するための検索条件を作成
		// parentObjectSelectTargetを作成する
		SearchSelectObject parentSearchSelect = new SearchSelectObject();

		// オブジェクトタイプがワークスペース かつ
		// 責任者属性がログインユーザーのID または 所属するグループのID または 所属するロールのID または複合グループのIDと一致
		ObjectTypeDomain wsOjectType = objectTypeService.getByDefinitionName(EIMConfig.get("OBJECT_TYPE_NAME_WORKSPACE"));

		SearchConditionGroup parentConditions = new SearchConditionGroup(SearchOperatorEnum.AND);
		parentConditions.addCondition(new SearchConditionCompare(SearchOperatorEnum.AND, SearchSelectObject.PsedoAttributeTypeEnum.TYPE,
				SearchOperatorEnum.EQ, wsOjectType.getId()));

		// 属性タイプが責任者属性
		AttributeTypeDomain wsAdminAttrType = attributeTypeService.getByDefinitionName(EIMConfig.get("ATTR_NAME_WORKSPACE_ADMINISTRATOR"));

		// ログインユーザのユーザ、グループ、ロール情報を取得
		UserDomain loginUser =  userService.getById(user.getId());

		// ワークスペース管理者にユーザが設定されている場合
		SearchConditionGroup adminAttrTypeConditions = new SearchConditionGroup(SearchOperatorEnum.AND);
		adminAttrTypeConditions.addCondition(new SearchConditionCompare(SearchOperatorEnum.AND, wsAdminAttrType,
				SearchOperatorEnum.EQ, user.getId()));

		// ワークスペース管理者にグループが設定されている場合
		List<GroupDomain> groupList = loginUser.getGroupList();
		// 所属するグループが存在する場合
		if (groupList.size() > 0) {
			for(GroupDomain adminGroup : groupList) {
				adminAttrTypeConditions.addCondition(new SearchConditionCompare(SearchOperatorEnum.OR, wsAdminAttrType,
						SearchOperatorEnum.EQ, adminGroup.getId()));
			}
		}

		// ワークスペース管理者にロールが設定されている場合
		List<RoleDomain> roleList = loginUser.getRoleList();
		// 所属するロールが存在する場合
		if (roleList.size() > 0){
			for(RoleDomain adminRole: roleList) {
				adminAttrTypeConditions.addCondition(new SearchConditionCompare(SearchOperatorEnum.OR, wsAdminAttrType,
						SearchOperatorEnum.EQ, adminRole.getId()));
			}
		}

		// 責任者属性が複合グループの場合
		List<EIMComp> allCompList = CompUtils.getCompList(sess);
		List<EIMComp> compList = new ArrayList<EIMComp>();

		for (EIMComp comp : allCompList){
			List <EIMUser> compUserList = CompUtils.getUserList(sess, comp);
			for (EIMUser compUser : compUserList){
				if(compUser.getId() == loginUser.getId()){
					compList.add(comp);
				}
			}
		}
		if (compList.size() > 0){
			for (EIMComp adminComp : compList) {
				adminAttrTypeConditions.addCondition(new SearchConditionCompare(SearchOperatorEnum.OR, wsAdminAttrType,
						SearchOperatorEnum.EQ, adminComp.getId()));
			}
		}

		parentConditions.addCondition(adminAttrTypeConditions);
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

		// ワークスペース固有ごみ箱オブジェクトのIDを検索条件として返却
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

	/**
	 * ユーザサービスを取得します。
	 * @param userService ユーザサービスクラス
	 */
	public  UserService getUserService() {
		return userService;
	}

	/**
	 * ユーザサービスを設定します。
	 * @param userService ユーザサービスクラス
	 */
	public  void setUserService(UserService userService) {
		this.userService = userService;
	}


}
