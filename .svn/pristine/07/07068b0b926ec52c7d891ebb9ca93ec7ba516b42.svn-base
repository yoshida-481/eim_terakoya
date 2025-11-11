package eimtest.app.util;

import java.util.List;

import junit.framework.TestCase;
import admin.AdminService;
import app.document.DocumentService;
import eim.bo.EIMUser;
import eim.net.EIMSession;
import eim.util.UserUtils;
import eimtest.util.TestSessionUtil;

/** */
public class JSPTestCase extends TestCase {
	/** */
	protected DocumentService ds;

	/** */
	protected AdminService as;

	/** */
	protected XMLUtil xu;

	/** */
	protected EIMSession sess;

	/** */
	protected EIMUser u1;

	/** */
	protected EIMUser u2;

	/** */
	protected EIMUser u3;

	/** */
	protected EIMUser u4;

	/** */
	protected EIMUser tu1;

	/** */
	protected EIMUser tu2;

	/** */
	protected EIMUser tu3;

	/** */
	protected EIMUser tu4;

	/** */
	protected EIMUser system;

	/** */
	protected List objs;

	public void setUp() throws Exception {
		ds = new DocumentService();
		as = new AdminService();
		xu = new XMLUtil();
		sess = TestSessionUtil.createEIMSession();
		u1 = UserUtils.getUserByCode(sess, "u1");
		u2 = UserUtils.getUserByCode(sess, "u2");
		u3 = UserUtils.getUserByCode(sess, "u3");
		u4 = UserUtils.getUserByCode(sess, "u4");
		tu1 = UserUtils.getUserByCode(sess, "tu1");
		tu2 = UserUtils.getUserByCode(sess, "tu2");
		tu3 = UserUtils.getUserByCode(sess, "tu3");
		tu4 = UserUtils.getUserByCode(sess, "tu4");
		system = UserUtils.getUserByCode(sess, "system");
		objs = TestAppObjectUtil.getFolderAndDocObjs(sess);
	}

	protected void tearDown() throws Exception {
		sess.close();
	}

	/** */
	public void testNone() {
	}
}
