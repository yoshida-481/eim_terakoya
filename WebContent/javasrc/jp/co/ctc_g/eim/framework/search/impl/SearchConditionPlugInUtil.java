package jp.co.ctc_g.eim.framework.search.impl;

import java.util.List;

import jp.co.ctc_g.eim.framework.common.util.ApplicationContextLoader;

import org.springframework.context.ApplicationContext;

/**
 * 
 * 検索条件追加プラグイン関連クラス
 * 
 */
public class SearchConditionPlugInUtil
{
	/**
	 * 検索条件追加プラグインが有効かどうか判定します。
	 * 
	 * @return true:有効、false:無効
	 * @throws Exception
	 */
	public static boolean isPlugInEffective() throws Exception
	{
		ApplicationContext context = ApplicationContextLoader.getContext();
		
		ObjectSearchConditionPlugInImpl objSearchCondition = (ObjectSearchConditionPlugInImpl)context.getBean("ObjectSearchConditionPlugin");
		List<SearchConditionBase> objPlugInList = objSearchCondition.getPlugins();
		
		RelationSearchConditionPlugInImpl relSearchCondition = (RelationSearchConditionPlugInImpl)context.getBean("RelationSearchConditionPlugin");
		List<SearchConditionBase> relPlugInList = relSearchCondition.getPlugins();
		
		if(objPlugInList != null && objPlugInList.size() != 0)
		{
			return true;
		}
		if(relPlugInList != null && relPlugInList.size() != 0)
		{
			return true;
		}
		return false;
	}
}