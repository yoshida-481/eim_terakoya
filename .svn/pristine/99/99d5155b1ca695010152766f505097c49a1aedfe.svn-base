package jp.co.ctc_g.eim.framework.search.impl;

import java.util.List;

import eim.bo.EIMObjectType;
import eim.bo.EIMRelationType;
import eim.bo.EIMSearchConditionGroup;
import eim.net.EIMSession;
import eim.util.ObjectUtils;
import eim.util.RelationUtils;
import eim.util.internal.search.SearchCondition;
import jp.co.ctc_g.eim.framework.common.exception.EIMAppException;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

public class SearchConditionBase {
	
	private List<EIMSearchConditionPlugIn> plugins;
	
	private String type;
	
	private String target;
	
	private long iType = 0;
	
	public static int CONDITION_MODE_OBJECT = 1;
	public static int CONDITION_MODE_RELATION = 2;
	
	
	public List<EIMSearchConditionPlugIn> getPlugins() {
		return plugins;
	}
	
	public void setPlugins(List<EIMSearchConditionPlugIn> plugins) {
		this.plugins = plugins;
	}
	
	public String getType() {
		return type;
	}
	
	public void setType(String type) {
		this.type = type;
	}
	
	public String getTarget() {
		return target;
	}
	
	public void setTarget(String target) {
		this.target = target;
	}
	
	public long getiType() {
		return iType;
	}
	
	public void init(int mode) throws Exception{
		if (type != null) {
			
			EIMSession sess = EIMThreadContext.getEIMSession();
			if (mode == CONDITION_MODE_OBJECT) {
				EIMObjectType objType = ObjectUtils.getObjectTypeByName(sess, type);
				if(objType != null){
					iType = objType.getId();
				}
			} else if (mode == CONDITION_MODE_RELATION) {
				EIMRelationType relType = RelationUtils.getRelationTypeByName(sess, type);
				if(relType != null){
					iType = relType.getId();
				}
			}
		}
	}
	
	
	@SuppressWarnings("unchecked")
	public EIMSearchConditionGroup createCondition() throws Exception {
		
		if (plugins == null) {
			throw new EIMAppException();
		}
		
		EIMSearchConditionGroup group = null;
		
		if (target != null && !checkTarget()) {
			return group;
		}
		
		for (EIMSearchConditionPlugIn tmp : plugins) {
			
			if (group == null) {
				group = tmp.getCondition();
			} else {
				List<SearchCondition> condList = tmp.getCondition().getConditions();
				for (SearchCondition tmpCond : condList) {
					group.addCondition(tmpCond);
				}
			}
			
		}
		
		return group;
	}
	
	private boolean checkTarget() {
		
		boolean result = true;
		
		if (target == null) {
			return result;
		}
		
		target = target.toUpperCase();
		
		if (target.equals("RELATION")) {
			result = true;
		} else if (target.equals("PARENT")) {
			result = true;
		} else if (target.equals("CHILD")) {
			result = true;
		} else {
			result = false;
		}
		
		return result;
	}
}