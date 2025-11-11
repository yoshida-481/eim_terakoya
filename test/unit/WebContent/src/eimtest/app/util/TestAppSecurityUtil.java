package eimtest.app.util;

import java.util.Iterator;
import java.util.List;

import common.util.AppConstant;

import eim.bo.EIMAccessEntry;
import eim.bo.EIMAccessEntryType;
import eim.bo.EIMAccessRole;
import eim.bo.EIMException;
import eim.bo.EIMSecurity;
import eim.bo.EIMUser;
import eim.net.EIMSession;
import eim.util.SecurityUtils;

/** */
public class TestAppSecurityUtil {
	/**
	 * 
	 * @param sess
	 * @param secName
	 * @param users
	 * @param roleTypes
	 * @return o
	 * @throws Exception
	 */
	public static EIMSecurity setEntry(EIMSession sess, String secName, EIMUser[] users,
			int[] roleTypes) throws Exception {
		try {
			SecurityUtils.createSecurity(sess, secName);
		} catch (EIMException e) {
			if (e.getCode() != 1402)
				throw e;
		}
		EIMSecurity sec = SecurityUtils.getSecurityByName(sess, secName);
		List entries = SecurityUtils.getAccessEntryList(sess, sec);
		for (Iterator i = entries.iterator(); i.hasNext();) {
			SecurityUtils.deleteAccessEntry(sess, (EIMAccessEntry) i.next());
		}
		EIMAccessRole[][] roles = {//
				{ new EIMAccessRole(11), new EIMAccessRole(12), new EIMAccessRole(13),
						new EIMAccessRole(14), new EIMAccessRole(15), new EIMAccessRole(21),
						new EIMAccessRole(22), new EIMAccessRole(31), new EIMAccessRole(32),
						new EIMAccessRole(41), new EIMAccessRole(42), new EIMAccessRole(51),
						new EIMAccessRole(61), new EIMAccessRole(62), new EIMAccessRole(63),
						new EIMAccessRole(101)//
						, new EIMAccessRole(AppConstant.ACCESS_ROLE_ALWAYS_READ) }//
				,
				{ new EIMAccessRole(EIMAccessRole.READ),
						new EIMAccessRole(EIMAccessRole.STATUS_UP),
						new EIMAccessRole(EIMAccessRole.STATUS_DOWN),
						new EIMAccessRole(AppConstant.ACCESS_ROLE_ALWAYS_READ) }//
				,
				{ new EIMAccessRole(EIMAccessRole.READ),
						new EIMAccessRole(AppConstant.ACCESS_ROLE_ALWAYS_READ) }//
				, { new EIMAccessRole(EIMAccessRole.READ) } //
				, {} };

		for (int i = 0; i < users.length; i++) {
			EIMAccessEntry ent = SecurityUtils.createAccessEntry(sess, sec, new EIMAccessEntryType(
					EIMAccessEntryType.USER), users[i]);
			for (int j = 0; j < roles[roleTypes[i]].length; j++) {
				SecurityUtils.updateAccessRole(sess, ent, roles[roleTypes[i]][j], 1);
			}
		}
		return sec;
	}
}
