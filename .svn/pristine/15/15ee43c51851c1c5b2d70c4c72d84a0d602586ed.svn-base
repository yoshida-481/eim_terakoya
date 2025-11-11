package common.util;

import java.util.ArrayList;
import java.util.List;

import eim.bo.EIMUser;
import eim.bo.EIMRole;
import eim.net.EIMSession;
import eim.util.RoleUtils;

public class BossRoleUtils {
	private List<EIMRole> roleList = null;
	
	public BossRoleUtils(EIMSession sess, EIMUser user) throws Exception
	{
		List<EIMRole> userRoleList = RoleUtils.getRoleByUser(sess, user);
		if (userRoleList.size() == 0) return;
		
		roleList = new ArrayList<EIMRole>();
		for(int i = 0 ; i < userRoleList.size() ; i++)
		{
			roleList.addAll(RoleUtils.getParentRoleList(sess, (EIMRole)userRoleList.get(i)));
		}
	}

	public List<EIMRole> get() throws Exception
	{
		return(roleList);
	}

	public boolean isInRole(EIMRole role) throws Exception
	{
		if (roleList == null) return(true);
		
		for (int i = 0 ; i < roleList.size() ; i++)
		{
			if (((EIMRole)roleList.get(i)).getId() == role.getId()) return(true);
		}
		return(false);
	}
	
	public boolean isInRole(List<EIMRole> roles) throws Exception
	{
		for (int i = 0 ; i < roles.size() ; i++)
		{
			if (isInRole((EIMRole)roles.get(i))) return(true);
		}
		return(false);
	}

}
