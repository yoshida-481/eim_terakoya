/**
 * 
 */
package app.document.search.condition.impl;

import common.util.AppSearchUtils;

import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import eim.bo.EIMSearchConditionGroup;
import eim.bo.EIMSearchSelectEIMObject;
import eim.bo.EIMUser;
import eim.net.EIMSession;
import app.document.search.EIMDocSearchType;
import app.document.search.condition.EIMDocSearchConditionMaker;

/**
 * チェックアウト一覧表示用検索条件定義
 *
 */
public class DspCheckoutListConditionMaker extends EIMDocSearchConditionMaker {

	/**
	 * @param type
	 * @param userData
	 */
	public DspCheckoutListConditionMaker(EIMDocSearchType type, Object userData) {
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
		EIMUser user = sess.getUser();
		
		EIMSearchConditionGroup conds = helper.group(helper.opAnd());
		
		// 検索条件１：オブジェクトタイプ(=ドキュメントとその配下のオブジェクトタイプ)
		conds.addCondition(AppSearchUtils.getCheckoutItemObjTypeCondition(sess));
		
		// 検索条件２：ユーザがロックしている
		conds.addCondition(helper.eq(helper.opAnd(),
				EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.LUSER,
				user.getId()));
		
		// 検索条件３：LATESTフラグが1
		conds.addCondition(helper.eq(helper.opAnd(),
				EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.LATEST,
				1));
		
		return conds;
		
		
	}

	/**
	 * @see app.document.search.condition.EIMDocSearchConditionMaker#parseUserData(java.lang.Object)
	 */
	@Override
	public void parseUserData(Object userData) throws Exception {
		// パラメータ無し

	}

}
