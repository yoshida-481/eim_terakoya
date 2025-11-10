package app.document.folder;

import app.document.attrTree.TestAttrTreeUtil;
import eim.bo.EIMObject;
import eim.net.EIMSession;
import eim.util.ObjectUtils;
import eimtest.app.util.TestAppDBUtil;
import eimtest.app.util.TestAppObjectUtil;
import eimtest.util.TestSessionUtil;

/** */
public class TestFolderTreeUtil {
	/**
	 * 
	 * @throws Exception
	 */
	public static void setupDBData() throws Exception {
		TestAppDBUtil.loadPrimitiveData();
		EIMSession sess = TestSessionUtil.createEIMSession();
		EIMObject obj = TestAppObjectUtil.getObj(sess, "F1");
		ObjectUtils.rename(sess, obj, "WS1F1");
		TestAttrTreeUtil.createFolderTree(sess);
		TestAttrTreeUtil.createAttrTree1(sess);
		TestAttrTreeUtil.createAttrTree2(sess);
		obj = TestAppObjectUtil.getObj(sess, "%-proj-X");
		ObjectUtils.rename(sess, obj, "A-proj-X");
		sess.commit();
	}
}
