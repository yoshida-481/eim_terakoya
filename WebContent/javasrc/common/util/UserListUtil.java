package common.util;

import java.util.List;

import jp.co.ctc_g.eim.framework2.business.domain.entity.GroupDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.RoleDomain;

/**
 * ユーザ検索時のユーティリティクラスです。<br>
 */
public class UserListUtil {
	
	/**
	 * グループ名称をカンマ区切り文字列に変換します。
	 * 
	 * @param groupList 対象グループのリスト
	 * @return グループ名称(カンマ区切り)
	 * @throws Exception 
	 */
	@SuppressWarnings("unchecked")
	public static String toSeparatedGroupNames(List<GroupDomain> groupList) {
		List<GroupDomain> sortedGroupList = AppObjectUtil.getStrSortedList(groupList, "getName", true);
		
		StringBuffer groupNameBuffer = new StringBuffer();
		for (int j = 0; j < sortedGroupList.size(); j++) 	{
			GroupDomain group = sortedGroupList.get(j);
			if(j != 0) {
				groupNameBuffer.append(",");
			}
			groupNameBuffer.append(group.getName());
		}
		return groupNameBuffer.toString();
	}


	/**
	 * グループIDをカンマ区切り文字列に変換します。
	 *
	 * @param groupList 対象グループのリスト
	 * @return グループID(カンマ区切り)
	 * @throws Exception
	 */
	@SuppressWarnings("unchecked")
	public static String toSeparatedGroupIds(List<GroupDomain> groupList) {
		List<GroupDomain> sortedGroupList = AppObjectUtil.getStrSortedList(groupList, "getName", true);

		StringBuffer groupIdBuffer = new StringBuffer();
		for (int j = 0; j < sortedGroupList.size(); j++) 	{
			GroupDomain group = sortedGroupList.get(j);
			if(j != 0) {
				groupIdBuffer.append(",");
			}
			groupIdBuffer.append(group.getId());
		}
		return groupIdBuffer.toString();
	}

	/**
	 * ロール名称をカンマ区切り文字列に変換します。
	 * 
	 * @param roleList 対象ロールのリスト
	 * @return ロール名称(カンマ区切り)
	 * @throws Exception 
	 */
	@SuppressWarnings("unchecked")
	public static String toSeparatedRoleNames(List<RoleDomain> roleList) {
		List<RoleDomain> sortedRoleList = AppObjectUtil.getStrSortedList(roleList, "getName", true);
		
		StringBuffer roleNameBuffer = new StringBuffer();
		for (int j = 0; j < sortedRoleList.size(); j++) 	{
			RoleDomain role = sortedRoleList.get(j);
			if(j != 0) {
				roleNameBuffer.append(",");
			}
			roleNameBuffer.append(role.getName());
		}
		return roleNameBuffer.toString();
	}

	/**
	 * ロールIDをカンマ区切り文字列に変換します。
	 *
	 * @param roleList 対象ロールのリスト
	 * @return ロールID(カンマ区切り)
	 * @throws Exception
	 */
	@SuppressWarnings("unchecked")
	public static String toSeparatedRoleIds(List<RoleDomain> roleList) {
		List<RoleDomain> sortedRoleList = AppObjectUtil.getStrSortedList(roleList, "getName", true);

		StringBuffer roleIdBuffer = new StringBuffer();
		for (int j = 0; j < sortedRoleList.size(); j++) 	{
			RoleDomain role = sortedRoleList.get(j);
			if(j != 0) {
				roleIdBuffer.append(",");
			}
			roleIdBuffer.append(role.getId());
		}
		return roleIdBuffer.toString();
	}
}
