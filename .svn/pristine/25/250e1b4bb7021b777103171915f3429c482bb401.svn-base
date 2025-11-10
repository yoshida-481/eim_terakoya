package common.util;

import junit.extensions.TestSetup;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import eim.bo.EIMObject;
import eim.net.EIMSession;
import eim.util.ObjectUtils;
import eimtest.app.util.CreateFolderTreeData;
import eimtest.app.util.TestAppDBUtil;
import eimtest.app.util.TestAppObjectUtil;
import eimtest.util.TestSessionUtil;

/** */
public class AppObjectUtilTest extends TestCase {
	/**
	 * 
	 * @return o
	 * @throws Exception
	 */
	public static Test suite() throws Exception {
		TestSuite suite = new TestSuite(AppObjectUtilTest.class);
		TestSetup w = new TestSetup(suite) {

			/** */
			public void setUp() throws Exception {
				if (true) {
					TestAppDBUtil.loadPrimitiveData();
					EIMSession sess = TestSessionUtil.createEIMSession();
					new CreateFolderTreeData(sess, null).process();// テスト用フォルダツリーを作る
					sess.commit();
				}
			}
		};
		return w;
	}

	/** */
	EIMSession sess;

	public void setUp() throws Exception {
		sess = TestSessionUtil.createEIMSession(null);
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testSetAttrEIMSessionEIMObjectEIMAttribute() throws Exception {
		EIMObject o = TestAppObjectUtil.getObj(sess, "F編集中");
		EIMObject newo = ObjectUtils.createObject(sess, o.getType(), "hoge");
		AppObjectUtil.setAttr(sess, newo, o.getAttribute("t数値N"));
		AppObjectUtil.setAttr(sess, newo, o.getAttribute("t日付N"));
		AppObjectUtil.setAttr(sess, newo, o.getAttribute("t文字N"));
		AppObjectUtil.setAttr(sess, newo, o.getAttribute("tテキストN"));
		newo = TestAppObjectUtil.getObj(sess, newo.getName());
		TestAppObjectUtil.assertEqualsAttr(o, newo, new String[] {//
			"t数値N"//
					, "t日付N"//
					, "t文字N"//
					, "tテキストN"//
			});
	}
}
