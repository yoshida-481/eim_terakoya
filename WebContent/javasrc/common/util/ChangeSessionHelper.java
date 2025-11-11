package common.util;

import eim.bo.EIMUser;
import eim.net.EIMSession;
import eim.util.UserUtils;

/**
 * EIMSessionの付け替え支援クラス
 * 
 */
public class ChangeSessionHelper {
	
	/** ログインユーザ */
	private EIMUser _loginUser = null;
	
	/** systemユーザ */
	private EIMUser _systemUser = null;
	
	/**
	 * コンストラクタ
	 * 
	 * @param sess EIMSessionインスタンス
	 * @throws Exception 
	 */
	public ChangeSessionHelper(EIMSession sess) throws Exception {
		
		this._loginUser = sess.getUser();
		this._systemUser = UserUtils.getUserById(sess, AppConstant.SYSYEM_USER_ID);	// systemユーザ
	}
	
	/**
	 * セッションをsystemユーザに変更する
	 * 
	 * @param sess EIMSessionインスタンス
	 * @throws Exception
	 */
	public void setSystemSession(EIMSession sess) throws Exception {
		
		sess.setUser(this._systemUser);
		sess.setAttribute(EIMSession.USER, this._systemUser);
	}
	
	/**
	 * セッションをログインユーザに戻す
	 * 
	 * @param sess EIMSessionインスタンス
	 * @throws Exception
	 */
	public void setLoginUserSession(EIMSession sess) throws Exception {
		
		sess.setUser(this._loginUser);
		sess.setAttribute(EIMSession.USER, this._loginUser);
	}
	
	/**
	 * セッションを指定のユーザに変更する(コンソールモード用)
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param id ユーザID
	 * @throws Exception
	 */
	public void setUserSessionConsole(EIMSession sess, long id) throws Exception {
		
		EIMUser user = UserUtils.getUserById(sess, id);
		sess.setUser(user);
	}
	
	/**
	 * セッションをSystemユーザに戻す(コンソールモード用)
	 * 
	 * @param sess EIMSessionインスタンス
	 * @throws Exception
	 */
	public void setSystemUserSessionConsole(EIMSession sess) throws Exception {
		
		sess.setUser(this._systemUser);
	}
	
}