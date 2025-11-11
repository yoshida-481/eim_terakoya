/**
 * 
 */
package app.document.search.condition.impl;

import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import eim.bo.EIMObjectType;
import eim.bo.EIMSearchConditionGroup;
import eim.bo.EIMSearchSelectEIMObject;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.ObjectUtils;
import app.document.search.EIMDocSearchType;
import app.document.search.condition.EIMDocSearchConditionMaker;

/**
 * オブジェクトタイプ取得に関する検索条件定義
 *
 */
public class DspFixedFormConditionMaker extends EIMDocSearchConditionMaker {
	
	/** 検索対象オブジェクトタイプのオブジェクトID */
	private String typeIds[];

	/**
	 * @param type 検索種別
	 * @param userData 取得する定型ドキュメントのオブジェクトID
	 */
	public DspFixedFormConditionMaker(EIMDocSearchType type, Object userData) {
		super(type, userData);
	}

	/**
	 * @see app.document.search.condition.EIMDocSearchConditionMaker#getDefaultCondition()
	 */
	@Override
	public EIMSearchConditionGroup getDefaultCondition() throws Exception {
		
		EIMSearchSelectEIMObject.SearchConditionBuildHelper helper
		 = new EIMSearchSelectEIMObject.SearchConditionBuildHelper();
		EIMSession sess = EIMThreadContext.getEIMSession();
		EIMObjectType objType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_OBJECTTYPE"));
		
		// 検索条件１：オブジェクトタイプは「オブジェクトタイプ」
		EIMSearchConditionGroup conds = helper.group(helper.opAnd());
		conds.addCondition(helper.eq(helper.opAnd(),
				EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.TYPE,
				objType.getId()));
		
		// 検索条件２：対象オブジェクトは指定したオブジェクトタイプオブジェクトのみ
		conds.addCondition(helper.in(helper.opAnd(), 
				EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.NAME, helper.opIn(), this.getTypeIds()));
		
		return conds;
	}

	/**
	 * @see app.document.search.condition.EIMDocSearchConditionMaker#parseUserData(java.lang.Object)
	 */
	@Override
	public void parseUserData(Object userData) throws Exception {
		this.setTypeIds((String[])userData);
	}

	/**
	 * @return the typeIds
	 */
	public String[] getTypeIds() {
		return typeIds;
	}

	/**
	 * @param typeIds the typeIds to set
	 */
	public void setTypeIds(String[] typeIds) {
		this.typeIds = typeIds;
	}
	
	

}
