package common.util;

import java.util.List;

import eim.bo.EIMGroup;
import eim.bo.EIMRole;
import eim.bo.EIMUser;
import eim.net.EIMSession;
import eim.util.GroupUtils;
import eim.util.RoleUtils;

/**
 * 
 * (ドキュメント管理用)ユーザ関連クラス
 *
 */
public class AppUserUtil {

	/**
	 * ユーザの所属するグループ名称をカンマ区切り文字列で取得します。
	 * 
	 * <li>引数のEIMUserがnullの場合はブランクを返します。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param user 対象ユーザ
	 * @return 所属するグループ名称(カンマ区切り)
	 * @throws Exception
	 */
	public static String getBelongGroupName(EIMSession sess, EIMUser user) throws Exception {
		
		String groupName = "";
		if (user != null) {
			List groupList = GroupUtils.getGroupByUser(sess, user);
			for (int i = 0; i < groupList.size(); i++) 	{
				EIMGroup group = (EIMGroup)groupList.get(i);
				if(i == 0) {
					groupName += group.getName();
				} else {
					groupName += "," + group.getName();
				}
			}
		}
		return groupName;
	}

	/**
	 * ユーザの所属するグループID、グループ名称をカンマ区切り文字列で取得します。
	 * 
	 * <li>引数のEIMUserがnullの場合はブランクを返します。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param user 対象ユーザ
	 * @return 所属するグループID、グループ名称(カンマ区切り)
	 * @throws Exception
	 */
	public static String[] getBelongGroupIdName(EIMSession sess, EIMUser user) throws Exception {
		String[] groupIdName = new String[] { "", "" };
		if (user != null) {
			List<?> groupList = GroupUtils.getGroupByUser(sess, user);
			for (int i = 0; i < groupList.size(); i++) {
				EIMGroup group = (EIMGroup) groupList.get(i);
				if (i == 0) {
					groupIdName[0] += group.getId();
					groupIdName[1] += group.getName();
				} else {
					groupIdName[0] += "," + group.getId();
					groupIdName[1] += "," + group.getName();
				}
			}
		}
		return groupIdName;
	}
	
	/**
	 * ユーザの所属するロール名称をカンマ区切り文字列で取得します。
	 * 
	 * <li>引数のEIMUserがnullの場合はブランクを返します。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param user 対象ユーザ
	 * @return 所属するロール名称(カンマ区切り)
	 * @throws Exception
	 */
	public static String getBelongRoleName(EIMSession sess, EIMUser user) throws Exception {
	
		String roleName = "";
		if (user != null) {
			List roleList = RoleUtils.getRoleByUser(sess, user);
			for (int i = 0; i < roleList.size(); i++) {
				EIMRole role = (EIMRole)roleList.get(i);
				if(i == 0) {
					roleName += role.getName();
				} else {
					roleName += "," + role.getName();
				}
			}
		}
		return roleName;
	}

	/**
	 * ユーザの所属するロールID、ロール名称をカンマ区切り文字列で取得します。
	 * 
	 * <li>引数のEIMUserがnullの場合はブランクを返します。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param user 対象ユーザ
	 * @return 所属するロールID、ロール名称(カンマ区切り)
	 * @throws Exception
	 */
	public static String[] getBelongRoleIdName(EIMSession sess, EIMUser user) throws Exception {
		String[] roleIdName = new String[] { "", "" };
		if (user != null) {
			List<?> roleList = RoleUtils.getRoleByUser(sess, user);
			for (int i = 0; i < roleList.size(); i++) {
				EIMRole role = (EIMRole) roleList.get(i);
				if (i == 0) {
					roleIdName[0] += role.getId();
					roleIdName[1] += role.getName();
				} else {
					roleIdName[0] += "," + role.getId();
					roleIdName[1] += "," + role.getName();
				}
			}
		}
		return roleIdName;
	}
}