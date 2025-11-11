package jp.co.ctc_g.eim.app.document.business.service.search.plugin.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import eim.bo.EIMAttribute;
import eim.bo.EIMObject;
import eim.util.EIMConfig;
import jp.co.ctc_g.eim.framework.business.domain.UserDefGroupDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectTypeDomain;
import jp.co.ctc_g.eim.framework2.business.service.AttributeTypeService;
import jp.co.ctc_g.eim.framework2.business.service.ObjectService;
import jp.co.ctc_g.eim.framework2.business.service.ObjectTypeService;
import jp.co.ctc_g.eim.framework2.common.util.ResourceUtils;
import jp.co.ctc_g.eim.search.core.common.business.domain.AccessControlDomain;
import jp.co.ctc_g.eim.search.core.common.eim.util.EIMSearchConstant;
import jp.co.ctc_g.eim.search.core.common.util.SearchConstant;
import jp.co.ctc_g.eim.search.core.indexBase.business.domain.AccessRoleListDomain;
import jp.co.ctc_g.eim.search.core.indexBase.eim.business.service.plugin.impl.DisabledEIMSecurityPlugInImpl;
import jp.co.ctc_g.eim.search.core.indexBase.eim.business.service.plugin.impl.EIMUserDefGroupPlugInImpl;

/**
 * ワークスペース管理者用ユーザ定義グループ取得実装クラス
 * @see jp.co.ctc_g.eim.search.core.indexBase.eim.business.service.plugin.impl.EIMUserDefGroupPlugIn
 */
public class WorkSpaceAdminSecurityPlugInImpl extends EIMUserDefGroupPlugInImpl {


	/** Logger */
	private static final Log log = LogFactory.getLog(DisabledEIMSecurityPlugInImpl.class);

	/** オブジェクトタイプサービス */
	private ObjectTypeService objectTypeService;

	/** オブジェクトサービス */
	private ObjectService objectService;

	/** 属性タイプサービスタイプサービス */
	private AttributeTypeService attributeTypeService;


	/**
	 * ワークスペース管理者用のアクセス権限情報を返却します。
	 * @see jp.co.ctc_g.eim.search.core.indexBase.eim.business.service.plugin.impl.EIMSecurityPlugInImpl#getSecurityList(java.util.List)
	 */
	@Override
	public List<AccessRoleListDomain> getUserDefGroupAccessRoleList(List<EIMObject> objectList, UserDefGroupDomain userDefGroup)
	throws Exception {

		// 返却用アクセス権限一覧のリスト
		List<AccessRoleListDomain> resultAccessRoleListDomainList = new ArrayList<AccessRoleListDomain>();

		// 引数のオブジェクトリストがNULL又は空リストの場合空リストを返す。
		if(objectList == null || objectList.isEmpty()){
			return resultAccessRoleListDomainList;
		}

		// オブジェクトタイプがワークスペース
		ObjectTypeDomain wsOjectType = objectTypeService.getByDefinitionName(EIMConfig.get("OBJECT_TYPE_NAME_WORKSPACE"));

		// 属性タイプがパス
		AttributeTypeDomain pathAttrType = attributeTypeService.getByDefinitionName(EIMConfig.get("ATTR_NAME_DOCUMENT_PASS"));

		// 取得済みのワークスペースを保管するMap
		Map<String, ObjectDomain> nameToWorkspaceObjectMap = new HashMap<>();

		// アクセス権限情報一覧を設定
		for(EIMObject eimObject : objectList){

			// オブジェクトID
			String eimObjId = String.valueOf(eimObject.getId());
			AccessRoleListDomain objAccessRoleListDomain = new AccessRoleListDomain(eimObjId);

			List<AccessControlDomain> accessControlDomainList = new ArrayList<AccessControlDomain>();

			// 対象オブジェクトのパスを取得
			EIMAttribute pathAttribute = eimObject.getAttribute(pathAttrType.getDefinitionName());
			// パス属性が取得できない場合(データ不正)はスキップ
			if (pathAttribute == null) {
				// オブジェクトのパス属性が取得できません。
				log.warn(ResourceUtils.getByKey("EIM.ERROR.LOGIC.NO.PATH.ATTRIBUTE", eimObject.getId()));
				continue;
			}
			String eimObjPath = pathAttribute.getStrings()[0];

			// 対象のワークスペース名を取得
			String [] strs = eimObjPath.split("/");
			String wsObjName = strs[1];

			// 対象のワークスペースオブジェクトを取得
			ObjectDomain wsObj = null;
			if (nameToWorkspaceObjectMap.containsKey(wsObjName)) {
				// 取得済みのワークスペースをMapから取得
				wsObj = nameToWorkspaceObjectMap.get(wsObjName);
			} else {
				// 未取得の場合DBから取得
				wsObj = objectService.getByTypeAndName(wsOjectType, wsObjName);

				// 取得済みのワークスペースをMapに保管
				nameToWorkspaceObjectMap.put(wsObjName, wsObj);
			}

			// ワークスペースが取得できない場合(データ不正)はスキップ
			if (wsObj == null) {
				// オブジェクトが取得できません。
				log.warn(ResourceUtils.getByKey("EIM.ERROR.OBJECT.NOTFOUND.DETAIL", wsObjName));
				continue;
			}

			// ワークスペース管理者が設定されていない場合スキップ
			if (wsObj.getAttribute(EIMConfig.get("ATTR_NAME_WORKSPACE_ADMINISTRATOR")) == null) {
				continue;
			}

			// ワークスペースの責任者属性を取得
			List<Long> wsAdminIdList = wsObj.getAttribute(EIMConfig.get("ATTR_NAME_WORKSPACE_ADMINISTRATOR")).getLongList();
			// ワークスペースの責任者種別を取得
			List<Long> wsAdminTypeList = wsObj.getAttribute(EIMConfig.get("ATTR_NAME_WORKSPACE_ADMINISTRATOR_TYPE")).getLongList();

			for (int i = 0; i < wsAdminIdList.size(); i ++){
				// 責任者属性のIDをlong型→Stringに変換
				String wsAdminId = String.valueOf(wsAdminIdList.get(i));
				// 責任者種別のIDをlong型→Stringに変換
				String wsAdminType = String.valueOf(wsAdminTypeList.get(i));

				// エントリー種別ID(1:ユーザ、2:グループ、3:ロール、4:複合グループ)
				switch (wsAdminType){
				case "1":
					// ワークスペース管理者（種別：ユーザ）のアクセス権限情報
					accessControlDomainList.add(new AccessControlDomain(wsAdminId,SearchConstant.ACCESS_CONTROL_TYPE_USER));
					break;
				case "2":
					// ワークスペース管理者（種別：グループ）のアクセス権限情報
					accessControlDomainList.add(new AccessControlDomain(wsAdminId,EIMSearchConstant.ACCESS_CONTROL_TYPE_GROUP));
					break;
				case "3":
					// ワークスペース管理者（種別：ロール）のアクセス権限情報
					accessControlDomainList.add(new AccessControlDomain(wsAdminId,EIMSearchConstant.ACCESS_CONTROL_TYPE_ROLE));
					break;
				case "4":
					// ワークスペース管理者（種別：複合グループ）のアクセス権限情報
					accessControlDomainList.add(new AccessControlDomain(wsAdminId,EIMSearchConstant.ACCESS_CONTROL_TYPE_COMP));
					break;
				}

				objAccessRoleListDomain.setAccessControlList(accessControlDomainList);
				resultAccessRoleListDomainList.add(objAccessRoleListDomain);
			}
		}

		return resultAccessRoleListDomainList;
	}

	/**
	 * オブジェクトサービスを取得します。
	 * @return オブジェクトサービスクラス
	 */
	public ObjectService getobjectService() {
	    return objectService;
	}

	/**
	 * オブジェクトサービスを設定します。
	 * @param objectService オブジェクトサービスクラス
	 */
	public void setobjectService (ObjectService objectService) {
	    this.objectService = objectService;
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

}
