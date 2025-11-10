package common.util;

import jp.co.ctc_g.eim.framework2.business.domain.entity.GroupDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.RoleDomain;
import eim.bo.EIMGroup;

/**
 * エントリー関連クラス
 *
 */
public class EntryUtil {
	
	/**
	 * 親グループをフルパスで取得します(EIMGroup)。
	 * ※引数のグループ名はフルパスに含みません。
	 * 
	 * @param group EIMグループ
	 * @return 親グループのフルパス文字列(String)
	 */
	public static String getParentGroupName(EIMGroup group) {
		EIMGroup parent = group.getParent();
		if(parent != null) {
		String path = getParentGroupName(parent);
			if(path.equals("/")) {
				return "/" + parent.getName();
			} else {
				return path + "/" + parent.getName();
			}
		} else {
			return "/";
		}
	}
	
	/**
	 * グループ名をフルパスで取得します(EIMGroup)。
	 * ※引数のグループ名もフルパスに含みます。
	 * 
	 * @param group EIMグループ
	 * @return グループのフルパス文字列(String)
	 */
	public static String getFullpathGroupName(EIMGroup group) {
		EIMGroup parent = group.getParent();
		String path = group.getName();
		if(parent != null) {
			return  getFullpathGroupName(parent) + "/" + path;
		} else {
			return  "/" + path;
		}
	}
	
	/**
	 * 親グループをフルパスで取得します(GroupDomain)。
	 * ※引数のグループ名はフルパスに含みません。
	 * 
	 * @param group グループドメイン
	 * @return 親グループのフルパス文字列(String)
	 */
	public static String getParentGroupName(GroupDomain group) {
		GroupDomain parent = group.getParent();
			if(parent != null) {
			String path = getParentGroupName(parent);
				if(path.equals("/")) {
					return "/" + parent.getName();
				} else {
					return path + "/" + parent.getName();
				}
			} else {
				return "/";
			}
		}
	
	/**
	 * グループ名をフルパスで取得(GroupDomain)。
	 * ※引数のグループ名もフルパスに含みます。
	 * 
	 * @param group グループドメイン
	 * @return グループのフルパス文字列(String)
	 */
	public static String getFullpathGroupName(GroupDomain group) {
		GroupDomain parent = group.getParent();
		String path = group.getName();
		if(parent != null) {
			return  getFullpathGroupName(parent) + "/" + path;
		} else {
			return  "/" + path;
		}
	}
	
	/**
	 * 親ロール名をフルパスで取得(RoleDomain)。
	 * ※引数のロール名はフルパスに含みません。
	 * 
	 * @param role ロールドメイン
	 * @return 親ロールのフルパス文字列(String)
	 */
	public static String getParentRoleName(RoleDomain role) {
		RoleDomain parent = role.getParent();
			if(parent != null) {
			String path = getParentRoleName(parent);
				if(path.equals("/")) {
					return "/" + parent.getName();
				} else {
					return path + "/" + parent.getName();
				}
			} else {
				return "/";
			}
		}
	
	/**
	 * ロール名をフルパスで取得(RoleDomain)。
	 * ※引数のロール名もフルパスに含みます。
	 * 
	 * @param role ロールドメイン
	 * @return ロールのフルパス文字列(String)
	 */
	public static String getFullpathRoleName(RoleDomain role) {
		RoleDomain parent = role.getParent();
		String path = role.getName();
		if(parent != null) {
			return  getFullpathRoleName(parent) + "/" + path;
		} else {
			return  "/" + path;
		}
	}
}
