package app.document.user;

import java.util.Iterator;
import java.util.List;

import junit.extensions.TestSetup;
import junit.framework.Test;
import junit.framework.TestSuite;

import org.w3c.dom.Document;

import app.document.folder.TestFolderTreeUtil;

import common.util.AppConstant;

import eim.bo.EIMAccessEntry;
import eim.bo.EIMAccessRole;
import eim.bo.EIMObject;
import eim.bo.EIMSecurity;
import eim.util.SecurityUtils;
import eimtest.app.util.JSPTestCase;
import eimtest.app.util.TestAppMisc;
import eimtest.app.util.TestAppObjectUtil;

/** */
public class DspFavoriteListTest extends JSPTestCase {
	/**
	 * 
	 * @return o
	 * @throws Exception
	 */
	public static Test suite() throws Exception {
		TestSuite suite = new TestSuite(DspFavoriteListTest.class);
		TestSetup w = new TestSetup(suite) {

			/** */
			public void setUp() throws Exception {
				if (true) {
					TestFolderTreeUtil.setupDBData();
				}
			}
		};
		return w;
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testMe() throws Exception {
		ds.switchUser("tu1");
		EIMObject f1 = TestAppObjectUtil.getObj(sess, "F1");
		EIMObject f6 = TestAppObjectUtil.getObj(sess, "F6");
		ds.user_actCreateFavorite(f1.getId());
		ds.user_actCreateFavorite(f6.getId());
		Document xdoc = xu.toDOM(ds.user_dspFavoriteList());
		assertObjValues(xdoc, f1.getId(), new String[][] {//
			{ "true", "isWorkflowFolder" } });
		assertObjValues(xdoc, f6.getId(), new String[][] {//
			{ "false", "isWorkflowFolder" } });
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testAccessRole() throws Exception {
		// 書き込み権
		// F1はtestMeでお気に入り登録ずみ
		ds.switchUser("tu1");
		EIMObject f1 = TestAppObjectUtil.getObj(sess, "F1");
		Document xdoc = xu.toDOM(ds.user_dspFavoriteList());
		assertObjValues(xdoc, f1.getId(), new String[][] {//
			{ "true", "isWorkflowFolder" } });// 見える

		// ステータス変更
		// tu1をステータス変更権限に変える
		updateAccessRole("tu1", 1);
		sess.commit();
		ds.switchUser("tu1");
		xdoc = xu.toDOM(ds.user_dspFavoriteList());
		assertObjValues(xdoc, f1.getId(), new String[][] {//
			{ "true", "isWorkflowFolder" } });// 見える

		// 常時読取
		// tu1を常時読み取り権限に変える
		updateAccessRole("tu1", 2);
		sess.commit();
		ds.switchUser("tu1");
		xdoc = xu.toDOM(ds.user_dspFavoriteList());
		assertObjValues(xdoc, f1.getId(), new String[][] {//
			{ "true", "isWorkflowFolder" } });// 見える

		// 公開読取
		// tu1を公開権限に変える
		updateAccessRole("tu1", 3);
		sess.commit();
		ds.switchUser("tu1");
		xdoc = xu.toDOM(ds.user_dspFavoriteList());
		assertObjValues(xdoc, f1.getId(), new String[][] {//
			{ "", "isWorkflowFolder" } });// 見えない
	}

	/** */
	static EIMAccessRole[][] roleDefs = {//
			{ new EIMAccessRole(11), new EIMAccessRole(12), new EIMAccessRole(13),
					new EIMAccessRole(14), new EIMAccessRole(15), new EIMAccessRole(21),
					new EIMAccessRole(22), new EIMAccessRole(31), new EIMAccessRole(32),
					new EIMAccessRole(41), new EIMAccessRole(42), new EIMAccessRole(51),
					new EIMAccessRole(61), new EIMAccessRole(62), new EIMAccessRole(63),
					new EIMAccessRole(101)//
					, new EIMAccessRole(AppConstant.ACCESS_ROLE_ALWAYS_READ) }//
			,
			{ new EIMAccessRole(EIMAccessRole.READ), new EIMAccessRole(EIMAccessRole.STATUS_UP),
					new EIMAccessRole(EIMAccessRole.STATUS_DOWN),
					new EIMAccessRole(AppConstant.ACCESS_ROLE_ALWAYS_READ) }//
			,
			{ new EIMAccessRole(EIMAccessRole.READ),
					new EIMAccessRole(AppConstant.ACCESS_ROLE_ALWAYS_READ) }//
			, { new EIMAccessRole(EIMAccessRole.READ) } //
	};

	/**
	 * 
	 * @param userCd
	 * @param roleLevel
	 * @throws Exception
	 */
	void updateAccessRole(String userCd, int roleLevel) throws Exception {
		EIMSecurity sec = SecurityUtils.getSecurityByName(sess, "ツリーテスト用セキュリティ");
		List ents = SecurityUtils.getAccessEntryList(sess, sec);
		for (Iterator i = ents.iterator(); i.hasNext();) {
			EIMAccessEntry ent = (EIMAccessEntry) i.next();
			if (ent.getUser().getCode().equals(userCd)) {
				List roles = SecurityUtils.getAccessRoleList(sess, ent);
				for (Iterator j = roles.iterator(); j.hasNext();) {
					EIMAccessRole role = (EIMAccessRole) j.next();
					SecurityUtils.updateAccessRole(sess, ent, role, 0);
				}
				for (int j = 0; j < roleDefs[roleLevel].length; j++) {
					SecurityUtils.updateAccessRole(sess, ent, roleDefs[roleLevel][j], 1);
				}
			}
		}
	}

	/**
	 * 
	 * @param xdoc
	 * @param objId
	 * @param values
	 * @throws Exception
	 */
	void assertObjValues(Document xdoc, int objId, String[][] values) throws Exception {
		for (int i = 0; i < values.length; i++)
			assertEquals("[" + values[i][0] + "]", TestAppMisc.getNodeValues(xdoc,
				"//objList/object[@objId='" + objId + "']/@" + values[i][1]).toString());
	}

}
