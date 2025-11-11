package app.document.search.condition;

import java.util.ArrayList;
import java.util.List;

import app.document.search.EIMDocSearchType;
import eim.bo.EIMAttributeType;
import eim.bo.EIMSearchConditionCompare;
import eim.bo.EIMSearchConditionGroup;
import eim.bo.EIMSearchConditionIn;
import eim.bo.EIMSearchSelectEIMObject;
import eim.bo.EIMSearchSelectEIMRelation;
import eim.util.TypeConvertUtils;
import eim.util.internal.search.SearchCondition;
import jp.co.ctc_g.eim.framework.search.impl.SearchConditionPlugInManager;

/**
 * 標準品の検索条件の定義を行うストラテジのベースクラス
 * 検索条件追加プラグインの呼び出しを共通とし、標準品デフォルトの
 * 検索条件定義を抽象メソッドで定義
 */
public abstract class EIMDocSearchConditionMaker {
	
	/** 検索種別 */
	private EIMDocSearchType type;
	
	/** 検索条件定義時に使用するデータ */
	private Object userData;
	
	/** 検索条件の設定対象 */
	private int conditionTarget = 1;
	
	/** リレーション検索の際のリレーション部分の検索条件 */
	private EIMSearchConditionGroup mainCondition;
	
	/**
	 * コンストラクタ
	 * @param type 検索種別
	 * @param userData 検索条件定義に使用する任意のデータ
	 */
	public EIMDocSearchConditionMaker(EIMDocSearchType type, Object userData) {
		this.setType(type);
		this.setUserData(userData);
	}
	
	/**
	 * ユーザデータの解析処理
	 * @param userData 検索条件定義プラグイン作成時に、呼び出し元から渡された任意のデータ
	 * @throws Exception
	 */
	public abstract void parseUserData(Object userData) throws Exception;
	
	/**
	 * 標準品デフォルトの検索条件定義
	 * @param userData 検索条件の定義に使用する任意のデータ
	 * @return デフォルトの検索条件
	 */
	public abstract EIMSearchConditionGroup getDefaultCondition() throws Exception;
	
	
	/**
	 * 標準品の検索条件定義を行う
	 * @return 操作別検索条件
	 */
	@SuppressWarnings("unchecked")
	public EIMSearchConditionGroup getCondition() throws Exception
	{
		// ユーザデータの解析
		this.parseUserData(this.getUserData());
		
		// デフォルトの検索条件の取得
		EIMSearchConditionGroup defaultCond = this.getDefaultCondition();
		
		// ここに拡張可能な検索条件追加文が入る
		SearchConditionPlugInManager cndManager = SearchConditionPlugInManager.getInstance();
		EIMSearchConditionGroup addCond = null;
		List<Long> typeList = new ArrayList<Long>();
		if (conditionTarget == 1 || conditionTarget == 2) {
			typeList = recursiveSearch(defaultCond);
		} else if (conditionTarget == 3 || conditionTarget == 4) {
			typeList = recursiveSearch(mainCondition);
		}
		
		if (typeList.size() > 0) {
			typeList.add(0L);
		}
		
		for (long type : typeList) {
			if (conditionTarget == 1) {
				addCond = cndManager.getObjectSearchCondition(type);
			} else if (conditionTarget == 2) {
				addCond = cndManager.getRelationSearchCondition(type);
			} else if (conditionTarget == 3) {
				addCond = cndManager.getParentSearchCondition(type);
			} else if (conditionTarget == 4) {
				addCond = cndManager.getChildSearchCondition(type);
			}
			
			if (addCond != null) {
				if (defaultCond == null) {
					defaultCond = addCond;
				} else {
					defaultCond.addCondition(addCond);
				}
			}
		}
		
		
		return defaultCond;
	}
	
	
	/**
	 * オブジェクトおよびリレーションタイプの検索条件の値を取得する
	 * @param cond
	 * @return 検索値の配列
	 */
	@SuppressWarnings("unchecked")
	private List<Long> recursiveSearch(SearchCondition cond) {
		
		List<Long> result = new ArrayList<Long>();
		
		if (cond instanceof EIMSearchConditionGroup ) {
			List<SearchCondition> condList = ((EIMSearchConditionGroup) cond).getConditions();
			for (SearchCondition tmp : condList) {
				List<Long> tmpResult = recursiveSearch(tmp);
				if (tmpResult.size() > 0) {
					result.addAll(tmpResult);
				}
			}
		} else if (cond instanceof EIMSearchConditionCompare) {
			EIMAttributeType attrType = ((EIMSearchConditionCompare)cond).getAttTypeLeft();
			long typeId = attrType.getId();
			if (typeId == EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.TYPE.getId() ||
					typeId == EIMSearchSelectEIMRelation.PsedoAttributeTypeEnum.TYPE.getId()) {
				long[] ids = {((EIMSearchConditionCompare)cond).getInt()};
				for (long id : ids) {
					result.add(id);
				}
			}
		} else if (cond instanceof EIMSearchConditionIn) {
			EIMAttributeType attrType = ((EIMSearchConditionIn)cond).getAttTypeLeft();
			long typeId = attrType.getId();
			if (typeId == EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.TYPE.getId() ||
					typeId == EIMSearchSelectEIMRelation.PsedoAttributeTypeEnum.TYPE.getId()) {
				long[] ids = TypeConvertUtils.convertToLongArray(((EIMSearchConditionIn)cond).getInts());
				for (long id : ids) {
					result.add(id);
				}
			}
		}
		
		return result;
	}


	/**
	 * @return the type
	 */
	public EIMDocSearchType getType() {
		return type;
	}


	/**
	 * @param type the type to set
	 */
	public void setType(EIMDocSearchType type) {
		this.type = type;
	}


	/**
	 * @param userData the userData to set
	 */
	public void setUserData(Object userData) {
		this.userData = userData;
	}


	/**
	 * @return the userData
	 */
	public Object getUserData() {
		return userData;
	}
	
	
	/**
	 * 検索条件の対象を設定する
	 * 何も設定しない場合はObjectSearch
	 * @param target 検索条件の対象
	 *               Object : 1
	 *               Relation : 2
	 *               Parent : 3
	 *               Child : 4
	 */
	public void setConditionTarget(int target) {
		conditionTarget = target;
	}
	
	
	public void setMainCondition(EIMSearchConditionGroup cond) {
		mainCondition = cond;
	}

}
