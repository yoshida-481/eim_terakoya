package jp.co.ctc_g.eim.framework.search.impl;

import eim.bo.EIMSearchConditionGroup;

/**
 * 検索条件追加プラグインのインターフェース
 * 
 */
public interface EIMSearchConditionPlugIn {
	
	/**
	 * 各プラグイン内で定義された検索条件を返します。
	 * @return 検索条件
	 */
	public abstract EIMSearchConditionGroup getCondition() throws Exception;
	
}