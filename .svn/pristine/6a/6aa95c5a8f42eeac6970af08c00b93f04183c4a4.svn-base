/**
 * 
 */
package app.document.search.condition.impl;

import common.util.AppSearchUtils;

import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import eim.bo.EIMAttributeType;
import eim.bo.EIMObject;
import eim.bo.EIMSearchConditionCompare;
import eim.bo.EIMSearchConditionGroup;
import eim.bo.EIMSearchOperatorEnum;
import eim.bo.EIMSearchSelectEIMObject;
import eim.net.EIMSession;
import eim.util.AttributeUtils;
import eim.util.EIMConfig;
import app.document.search.EIMDocSearchType;
import app.document.search.condition.EIMDocSearchConditionMaker;

/**
 * タグ配下のオブジェクトを取得する検索条件の定義
 */
public class DspTagTreeConditionMaker extends EIMDocSearchConditionMaker {
	
	/** 選択されたタグ */
	private EIMObject object;

	/**
	 * @param type 検索種別
	 * @param userData ユーザデータ(選択されたタグ)
	 */
	public DspTagTreeConditionMaker(EIMDocSearchType type, Object userData) {
		super(type, userData);
	}

	/**
	 * @see app.document.search.condition.EIMDocSearchConditionMaker#getDefaultCondition()
	 */
	@Override
	public EIMSearchConditionGroup getDefaultCondition() throws Exception {
		
		EIMSession sess = EIMThreadContext.getEIMSession();
		
		// 検索条件ヘルパー生成
		EIMSearchSelectEIMObject.SearchConditionBuildHelper h = new EIMSearchSelectEIMObject.SearchConditionBuildHelper();
		
		// 検索条件グループ作成
		EIMSearchConditionGroup conds = new EIMSearchConditionGroup(EIMSearchOperatorEnum.AND);
		
		// 検索条件１：オブジェクトタイプが「ドキュメント」「フォルダ」「タグ」のいずれか
		conds.addCondition(AppSearchUtils.getListViewItemObjTypeCondition(sess));

		// 検索条件２：属性「タグ」が<オブジェクトID>
		EIMAttributeType attrTag = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_TAG"));
		EIMSearchConditionCompare attrTagCond = h.eq(h.opAnd(), attrTag, this.getObject().getId());
		conds.addCondition(attrTagCond);
		
		return conds;
	}

	/**
	 * @see app.document.search.condition.EIMDocSearchConditionMaker#parseUserData(java.lang.Object)
	 */
	@Override
	public void parseUserData(Object userData) throws Exception {
		
		// 呼び出し元から渡されたパラメータを格納
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
