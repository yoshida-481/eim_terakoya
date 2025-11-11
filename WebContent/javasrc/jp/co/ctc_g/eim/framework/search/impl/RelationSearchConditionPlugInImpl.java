package jp.co.ctc_g.eim.framework.search.impl;

import java.util.List;

import eim.bo.EIMSearchConditionGroup;

public class RelationSearchConditionPlugInImpl {
	
	
	private List<SearchConditionBase> plugins;
	
	public List<SearchConditionBase> getPlugins() {
		return plugins;
	}
	
	public void setPlugins(List<SearchConditionBase> plugins) {
		this.plugins = plugins;
	}
	
	public EIMSearchConditionGroup getRelationCondition(long id) throws Exception{
		EIMSearchConditionGroup condition = null;
		
		for (SearchConditionBase tmp : plugins) {
			if (tmp.getTarget().equals("RELATION") || tmp.getTarget().equals("")) {
				tmp.init(SearchConditionBase.CONDITION_MODE_RELATION);
				if(id == tmp.getiType()){
					condition = tmp.createCondition();
					break;
				}
			}
		}
		
		return condition;
	}
	
	public EIMSearchConditionGroup getParentCondition(long id) throws Exception{
		EIMSearchConditionGroup condition = null;
		
		for (SearchConditionBase tmp : plugins) {
			if (tmp.getTarget().equals("PARENT") || tmp.getTarget().equals("")) {
				tmp.init(SearchConditionBase.CONDITION_MODE_RELATION);
				if(id == tmp.getiType()){
					condition = tmp.createCondition();
					break;
				}
			}
		}
		
		return condition;
	}
	
	public EIMSearchConditionGroup getChildCondition(long id) throws Exception{
		EIMSearchConditionGroup condition = null;
		
		for (SearchConditionBase tmp : plugins) {
			if (tmp.getTarget().equals("CHILD") || tmp.getTarget().equals("")) {
				tmp.init(SearchConditionBase.CONDITION_MODE_RELATION);
				if(id == tmp.getiType()){
					condition = tmp.createCondition();
					break;
				}
			}
		}
		
		return condition;
	}
}