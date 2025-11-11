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
import jp.co.ctc_g.eim.framework2.business.domain.entity.RoleDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchConditionCompare;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchConditionGroup;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchConditionLike;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchOperatorEnum;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchSelectObject;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchSelectObject.PsedoAttributeTypeEnum;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchSelectObject.SearchConditionBuildHelper;
import jp.co.ctc_g.eim.framework2.business.plugin.impl.AbstractUserDefGroupPlugInImpl;
import jp.co.ctc_g.eim.framework2.business.service.AttributeTypeService;
import jp.co.ctc_g.eim.framework2.business.service.ObjectService;
import jp.co.ctc_g.eim.framework2.business.service.ObjectTypeService;
import jp.co.ctc_g.eim.framework2.business.service.UserService;
import jp.co.ctc_g.eim.framework2.common.exception.EIMException;

/**
 * ユーザ定義グループ「ワークスペース管理者」(ごみ箱配下表示用)の実装<br>
 *
 * ワークスペース毎のワークスペース管理者を取得する。
 *
 * @see jp.co.ctc_g.eim.framework2.business.plugin.impl.AbstractUserDefGroupPlugInImpl
 * @since Ver1.0
 */
public class WorkSpaceAdminInRecycleUserDefGroupPlugInImpl extends AbstractUserDefGroupPlugInImpl{

	/** オブジェクトタイプサービス */
	private ObjectTypeService objectTypeService;

	/** 属性タイプサービスタイプサービス */
	private AttributeTypeService attributeTypeService;

	/** オブジェクトサービス */
	private ObjectService objectService;

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

		// 自身が管理者となっているワークスペースを取得するための検索条件を作成
		SearchSelectObject searchSelect = new SearchSelectObject();

		// オブジェクトタイプがワークスペース かつ
		// 責任者属性がログインユーザーのID または 所属するグループのID または 所属するロールのID または複合グループのIDと一致
		ObjectTypeDomain wsOjectType = objectTypeService.getByDefinitionName(EIMConfig.get("OBJECT_TYPE_NAME_WORKSPACE"));

		SearchConditionGroup conditions = new SearchConditionGroup(SearchOperatorEnum.AND);
		conditions.addCondition(new SearchConditionCompare(SearchOperatorEnum.AND, SearchSelectObject.PsedoAttributeTypeEnum.TYPE,
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
			for(GroupDomain adminGroup : groupList){
				adminAttrTypeConditions.addCondition(new SearchConditionCompare(SearchOperatorEnum.OR, wsAdminAttrType,
						SearchOperatorEnum.EQ, adminGroup.getId()));
			}
		}

		// ワークスペース管理者にロールが設定されている場合
		List<RoleDomain> roleList = loginUser.getRoleList();
		// 所属するロールが存在する場合
		if (roleList.size() > 0){
			for(RoleDomain adminRole : roleList) {
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

		conditions.addCondition(adminAttrTypeConditions);
		searchSelect.setCondition(conditions);
		// 属性タイプのみ取得
		List<AttributeTypeDomain> resultAttrs = new ArrayList<AttributeTypeDomain>();
		resultAttrs.add(PsedoAttributeTypeEnum.NAME);
		searchSelect.setResultAttrs(resultAttrs);

		// 自身が管理者となっているワークスペースの一覧を取得
		List<ObjectDomain> wsList = objectService.getList(searchSelect,null);

		// 管理対象のワークスペースが一件もない場合
		if(wsList.size() <= 0){
			// 常に偽となる検索条件を返却する
			return h.group(h.opAnd()).addCondition(h.eq(h.opAnd(), PsedoAttributeTypeEnum.ID, 0));
		}

		// オブジェクトタイプがパス属性
		AttributeTypeDomain pathAttrType = attributeTypeService.getByDefinitionName(EIMConfig.get("ATTR_NAME_DOCUMENT_PASS"));

		SearchConditionGroup pathAttrConditionGroup = h.group(h.opOr()).addCondition(null);
		// ワークスペース固有ごみ箱のパスを設定
		for(int i = 0; i < wsList.size(); i++){
			String wsRecyclePath= "/" + wsList.get(i).getName() + "/ごみ箱/*";
			pathAttrConditionGroup.addCondition(new SearchConditionLike(h.opOr(), pathAttrType, SearchOperatorEnum.LIKE, wsRecyclePath));
		}

		// パス属性を検索条件として返却
		return pathAttrConditionGroup;
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
	 * オブジェクトサービスを取得します。
	 * @return オブジェクトサービスクラス
	 */
	public ObjectService getObjectService() {
	    return objectService;
	}

	/**
	 * オブジェクトサービスを設定します。
	 * @param objectService オブジェクトサービスクラス
	 */
	public void setObjectService (ObjectService objectService) {
	    this.objectService = objectService;
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
	public void setUserService(UserService userService) {
		this.userService = userService;
	}



}
