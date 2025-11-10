package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.util.HashSet;
import java.util.Set;

import jp.co.ctc_g.eim.framework.business.service.AdminAuthCheckPlugIn;
import eim.bo.EIMUser;
import eim.util.EIMXmlConfigAdminAuth;

/**
 * システム管理権限を持つかどうかを判定します。
 *
 */
public class ArbitraryAdminAuthCheckPlugInImp implements AdminAuthCheckPlugIn {
	
	private Set<String> authList = new HashSet<String>();

	/**
	 * 指定されたユーザがシステム管理者権限を持つかどうか判定します。
	 * @see jp.co.ctc_g.eim.framework.business.service.AdminAuthCheckPlugIn#hasAuth(eim.bo.EIMUser)
	 */
	public boolean hasAuth(EIMUser user) throws Exception {
		Set<String> aList = getAuthList();
		for(String auth:aList) {
			if(EIMXmlConfigAdminAuth.hasSpecifiedAuth( user, auth)) {
				return true;
			}
		}
		return false;
	}

	public Set<String> getAuthList() {
		return authList;
	}

	public void setAuthList(Set<String> authList) {
		this.authList = authList;
	}
	
	public void addAuth(String auth) {
		getAuthList().add(auth);
	}
	
	public void removeAuth(String auth) {
		if(authList.contains(auth)) {
			authList.remove(auth);
		}
	}

}
