package jp.co.ctc_g.eim.framework.search.impl;

import java.util.List;

import eim.bo.EIMSearchConditionGroup;

public class ObjectSearchConditionPlugInImpl {
	
	
	private List<SearchConditionBase> plugins;
	
	public List<SearchConditionBase> getPlugins() {
		return plugins;
	}
	
	public void setPlugins(List<SearchConditionBase> plugins) {
		this.plugins = plugins;
	}
	
	public EIMSearchConditionGroup getCondition(long objTypeId) throws Exception{
		EIMSearchConditionGroup resultCondition = null;
		
		for(SearchConditionBase pluginImpl : plugins){
			pluginImpl.init(SearchConditionBase.CONDITION_MODE_OBJECT);
			if(pluginImpl.getiType() == objTypeId){
				resultCondition = pluginImpl.createCondition();
				break;
			}
		}
		
		return resultCondition;
	}
}

