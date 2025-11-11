/**
 * 
 */
package app.document.search.condition.impl;

import java.sql.ResultSet;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import app.document.search.EIMDocSearchType;
import app.document.search.condition.EIMDocSearchConditionMaker;
import common.util.AppSearchUtils;
import eim.bo.EIMSearchConditionGroup;
import eim.bo.EIMSearchSelectEIMObject;
import eim.net.EIMSession;
import eim.util.TypeConvertUtils;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * 属性ツリービューの属性値表示
 */
public class DspAttrValueTreeConditionMaker extends EIMDocSearchConditionMaker {
	
	/** 属性値取得結果 */
	private ResultSet resultSet;

	/**
	 * @param type 検索種別
	 * @param userData ユーザ情報
	 */
	public DspAttrValueTreeConditionMaker(EIMDocSearchType type, Object userData) {
		super(type, userData);
	}

	/**
	 * @see app.document.search.condition.EIMDocSearchConditionMaker#getDefaultCondition()
	 */
	@Override
	public EIMSearchConditionGroup getDefaultCondition() throws Exception {
		
		EIMSearchSelectEIMObject.SearchConditionBuildHelper helper =
			new EIMSearchSelectEIMObject.SearchConditionBuildHelper();
		EIMSession sess = EIMThreadContext.getEIMSession();
		
		// 検索条件１：対象オブジェクトタイプが「フォルダ」「ドキュメント」
		EIMSearchConditionGroup conds = helper.group(helper.opAnd());
		conds.addCondition(AppSearchUtils.getAttrTreeItemObjTypeCondition(sess));
		
		// 検索条件２：属性値取得結果で取れたオブジェクトのID
		Set<Long> dspIdSet = new HashSet<Long>();
		while (this.getResultSet().next()) {
			ResultSet r = this.getResultSet();
			long id = r.getLong("id");
			dspIdSet.add(id);
		}
		long[] dspIds = new long[dspIdSet.size()];
		int i = 0;
		Iterator<Long> iterator = dspIdSet.iterator();
		while (iterator.hasNext()) {
			dspIds[i++] = iterator.next();
		}
		conds.addCondition(helper.in(helper.opAnd(), 
				EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.ID, helper.opIn(), TypeConvertUtils.convertToBuildTypeArray(dspIds)));
		
		return conds;
	}

	/**
	 * @see app.document.search.condition.EIMDocSearchConditionMaker#parseUserData(java.lang.Object)
	 */
	@Override
	public void parseUserData(Object userData) throws Exception {
		
		// 表示対象属性の検索結果を取得
		this.setResultSet((ResultSet)userData);

	}

	/**
	 * @return the resultSet
	 */
	public ResultSet getResultSet() {
		return resultSet;
	}

	/**
	 * @param resultSet the resultSet to set
	 */
	public void setResultSet(ResultSet resultSet) {
		this.resultSet = resultSet;
	}
	
	

}
