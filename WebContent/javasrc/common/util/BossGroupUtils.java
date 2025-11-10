package common.util;

import java.util.ArrayList;
import java.util.List;

import eim.bo.EIMGroup;
import eim.bo.EIMUser;
import eim.net.EIMSession;
import eim.util.GroupUtils;

public class BossGroupUtils {
	
	private List<EIMGroup> groupList = null;
	
	public BossGroupUtils(EIMSession sess, EIMUser user) throws Exception
	{
		List<EIMGroup> userGroupList = GroupUtils.getGroupByUser(sess, user);
		if (userGroupList.size() == 0) return;

		groupList = new ArrayList<EIMGroup>();
		groupList.addAll(userGroupList);
		for(int i = 0 ; i < userGroupList.size() ; i++)
		{
			getParentGroupReflexive((EIMGroup)userGroupList.get(i), groupList);
		}
	}
	
	private void getParentGroupReflexive(EIMGroup child, List<EIMGroup> parentList) throws Exception
	{
		EIMGroup parent = child.getParent();
		if (parent == null) return;
		parentList.add(parent);
		getParentGroupReflexive(parent, parentList);
	}
	
	public List<EIMGroup> get() throws Exception
	{
		return(groupList);
	}

	public boolean isInGroup(EIMGroup group) throws Exception
	{
		if (groupList == null) return(true);
		
		for (int i = 0 ; i < groupList.size() ; i++)
		{
			if (((EIMGroup)groupList.get(i)).getId() == group.getId()) return(true);
		}
		return(false);
	}
	
	public boolean isInGroup(List<EIMGroup> groups) throws Exception
	{
		for (int i = 0 ; i < groups.size() ; i++)
		{
			if (isInGroup((EIMGroup)groups.get(i))) return(true);
		}
		return(false);
	}

}
