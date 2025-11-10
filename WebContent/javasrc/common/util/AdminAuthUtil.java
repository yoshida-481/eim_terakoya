package common.util;

import eim.bo.*;
import eim.util.*;

/**
 * 管理者権限関連　共通クラス
 * <P>
 * [機能]<BR>
 *		管理者権限設定ファイルの操作に関するクラス
 *
 * @author kumasaka
 * @version 1.0
 */
public class AdminAuthUtil {

	/**
	 * ユーザが指定された権限、またはシステム管理（汎用）の権限の
	 * どちらかを持っているかを判定します。
	 *
	 * @param user
	 * @param prmAuthId ユーザが持っているかどうか判定する対象の管理者権限ID
	 * @return 権限があれば true 、さもなくば false
	 * @throws Exception
	 */
	public static boolean hasSpecifiedOrGeneralAuth(EIMUser user, String prmAuthId) throws Exception {

		if (EIMXmlConfigAdminAuth.hasSpecifiedAuth(user, prmAuthId)
		 || EIMXmlConfigAdminAuth.hasSpecifiedAuth(user, AppConstant.ADMIN_AUTH_ID_GENERAL)
		 || EIMXmlConfigAdminAuth.hasSpecifiedAuth(user, AppConstant.ADMIN_AUTH_ID_FORM)
		 || EIMXmlConfigAdminAuth.hasSpecifiedAuth(user, AppConstant.ADMIN_AUTH_ID_TASK)) {
			return true;
		}
		return false;
	}

	/**
	 * ユーザがなんらかのシステム管理権限を持っているかを判定します。
	 *
	 * @param user
	 * @return 権限があれば true 、さもなくば false
	 * @throws Exception
	 */
	public static boolean hasAnyAuth(EIMUser user) throws Exception {

		if (EIMXmlConfigAdminAuth.hasAnyAuthInSpecifiedAdminApp(user, AppConstant.ADMIN_APP_ID_GENERAL)
		 || EIMXmlConfigAdminAuth.hasAnyAuthInSpecifiedAdminApp(user, AppConstant.ADMIN_APP_ID_DOCUMENT)
		 || EIMXmlConfigAdminAuth.hasAnyAuthInSpecifiedAdminApp(user, AppConstant.ADMIN_APP_ID_FORM)
		 || EIMXmlConfigAdminAuth.hasAnyAuthInSpecifiedAdminApp(user, AppConstant.ADMIN_APP_ID_TASK)) {
			return true;
		}
		return false;
	}
}
