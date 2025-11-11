/**
 *
 */
package app.document.search.condition.impl;

import java.util.ArrayList;
import java.util.List;

import app.document.search.EIMDocSearchType;
import app.document.search.condition.EIMDocSearchConditionMaker;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMSearchConditionGroup;
import eim.bo.EIMSearchConditionIn;
import eim.bo.EIMSearchSelectEIMObject;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.ObjectUtils;
import eim.util.TypeConvertUtils;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * ワークスペース表示時の検索条件定義
 *
 */
public class DspWorkspaceConditionMaker extends EIMDocSearchConditionMaker {

	/** 取得対象のワークスペースID（取得するワークスペースを絞る際に使用する） */
	private List<Long> workspaceObjectIdList = null;

	/**
	 * コンストラクタ
	 * @param type 検索種別
	 */
	public DspWorkspaceConditionMaker(EIMDocSearchType type, Object userData) {
		super(type, userData);
	}

	/**
	 * @see app.document.search.condition.EIMDocSearchConditionMaker#parseUserData(Object userData)
	 */
	@Override
	public void parseUserData(Object userData) throws Exception{

		if (org.apache.commons.lang3.ObjectUtils.isEmpty(userData)) {
			return;
		}

		@SuppressWarnings("unchecked")
		List<EIMObject> objList = (List<EIMObject>)userData;
		this.workspaceObjectIdList = new ArrayList<Long>();

		for (EIMObject obj : objList) {
			this.workspaceObjectIdList.add(new Long(obj.getId()));
		}

	}

	/**
	 * @see app.document.search.condition.EIMDocSearchConditionMaker#getDefaultCondition()
	 */
	@Override
	public EIMSearchConditionGroup getDefaultCondition() throws Exception{

		EIMSearchSelectEIMObject.SearchConditionBuildHelper helper =
			new EIMSearchSelectEIMObject.SearchConditionBuildHelper();

		// 検索条件：オブジェクトタイプがワークスペースの読み取り可なオブジェクト
		EIMSession sess = EIMThreadContext.getEIMSession();
		EIMObjectType objType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_WORKSPACE"));

		EIMSearchConditionGroup conds = helper.group(helper.opAnd());

		// 検索条件１：オブジェクトタイプ(=ワークスペース)
		conds.addCondition(helper.eq(helper.opAnd(),
				EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.TYPE,
				objType.getId()));

		// 検索条件２：LATEST(=最新)
		conds.addCondition(helper.latest(helper.opAnd()));

		// 検索条件３：オブジェクトID（オプション）
		// 自身に承認のあるオブジェクトがある時のみ検索を行う
		if (this.workspaceObjectIdList != null && this.workspaceObjectIdList.size() > 0) {

			long[] workspaceObjectIds = new long[this.workspaceObjectIdList.size()];
			for (int i = 0 ; i < this.workspaceObjectIdList.size() ; i++) {
				workspaceObjectIds[i] = this.workspaceObjectIdList.get(i).longValue();
			}
			conds.addCondition(new EIMSearchConditionIn(helper.opAnd(),
					EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.ID, helper.opIn(), TypeConvertUtils.convertToBuildTypeArray(workspaceObjectIds)));
		}

		return conds;
	}

}
