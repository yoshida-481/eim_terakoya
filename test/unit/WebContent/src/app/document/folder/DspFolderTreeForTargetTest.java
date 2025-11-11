package app.document.folder;

import junit.extensions.TestSetup;
import junit.framework.Test;
import junit.framework.TestSuite;
import eim.bo.EIMObject;
import eimtest.app.util.JSPTestCase;
import eimtest.app.util.TestAppMisc;
import eimtest.app.util.TestAppObjectUtil;

/** */
public class DspFolderTreeForTargetTest extends JSPTestCase {
	/**
	 * 
	 * @return o
	 * @throws Exception
	 */
	public static Test suite() throws Exception {
		TestSuite suite = new TestSuite(DspFolderTreeForTargetTest.class);
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
		EIMObject obj = TestAppObjectUtil.getObj(sess, "F11");

		// systemユーザー
		String assertText = TestAppMisc.loadAppTextFile(this, "_01.txt");
		assertText = TestAppMisc.replaceAssertText(assertText, objs);
		assertEquals(assertText, xu.toStr(xu.toDOM(ds.folder_dspFolderTreeForTarget(obj.getId()))));

		// 書込権限
		ds.switchUser("tu1");
		assertText = TestAppMisc.loadAppTextFile(this, "_02.txt");
		assertText = TestAppMisc.replaceAssertText(assertText, objs);
		assertEquals(assertText, xu.toStr(xu.toDOM(ds.folder_dspFolderTreeForTarget(obj.getId()))));

		// ステータス変更権限
		ds.switchUser("tu2");
		assertEquals(assertText, xu.toStr(xu.toDOM(ds.folder_dspFolderTreeForTarget(obj.getId()))));

		// 常時読取権限
		ds.switchUser("tu3");
		assertEquals(assertText, xu.toStr(xu.toDOM(ds.folder_dspFolderTreeForTarget(obj.getId()))));

		// 公開読取権限
		ds.switchUser("tu4");
		assertText = TestAppMisc.loadAppTextFile(this, "_03.txt");
		assertText = TestAppMisc.replaceAssertText(assertText, objs);
		assertEquals(assertText, xu.toStr(xu.toDOM(ds.folder_dspFolderTreeForTarget(obj.getId()))));
	}
}
