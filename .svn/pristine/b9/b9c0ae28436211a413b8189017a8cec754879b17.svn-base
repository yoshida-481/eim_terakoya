/**
 * 
 */
package app.document.search.condition.impl;

import app.document.search.EIMDocSearchType;
import app.document.search.condition.EIMDocSearchConditionMaker;
import common.util.AppSearchUtils;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMSearchConditionGroup;
import eim.bo.EIMSearchSelectEIMObject;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.ObjectUtils;
import eim.util.TypeConvertUtils;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * オブジェクトタイプ「フォルダ」指定の検索条件定義クラス
 *
 */
public class ObjTypeFolderOrWsRecycleConditionMaker extends EIMDocSearchConditionMaker {
	
	/** 対象オブジェクト */
	private EIMObject object;

	/**
	 * @param type
	 * @param userData
	 */
	public ObjTypeFolderOrWsRecycleConditionMaker(EIMDocSearchType type, Object userData) {
		super(type, userData);
	}

	/**
	 * @see app.document.search.condition.EIMDocSearchConditionMaker#getDefaultCondition()
	 */
	@Override
	public EIMSearchConditionGroup getDefaultCondition() throws Exception {
		
		// 検索条件ヘルパー生成
		EIMSearchSelectEIMObject.SearchConditionBuildHelper helper = 
			new EIMSearchSelectEIMObject.SearchConditionBuildHelper();
		
		EIMSession sess = EIMThreadContext.getEIMSession();
		
		// 検索条件グループ作成
		EIMSearchConditionGroup conds = helper.group(helper.opAnd());
		
		// 検索条件：オブジェクトタイプが「フォルダ」。そのサブクラスも対象。
		conds.addCondition(AppSearchUtils.getTreeViewItemFolderObjTypeCondition(sess));
		
		// 検索条件：オブジェクトタイプが「ワークスペース固有ごみ箱」
		EIMObjectType objTypeWsRecycle = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_WORKSPACERECYCLE"));
		long[] types = {objTypeWsRecycle.getId()};

		conds.addCondition(helper.in(helper.opOr(), EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.TYPE, 
				helper.opIn(), TypeConvertUtils.convertToBuildTypeArray(types)));
		
		return conds;
	}

	/**
	 * @see app.document.search.condition.EIMDocSearchConditionMaker#parseUserData(java.lang.Object)
	 */
	@Override
	public void parseUserData(Object userData) throws Exception {
		// 取得対象EIMオブジェクトをセット
		this.setObject((EIMObject)userData);
	}

	/**
	 * @return the object
	 */
	public EIMObject getObject() {
		return object;
	}

	/**
	 * @param object the object to set
	 */
	public void setObject(EIMObject object) {
		this.object = object;
	}
	
}
