package app.document.folder;

import junit.extensions.TestSetup;
import junit.framework.Test;
import junit.framework.TestSuite;
import eimtest.app.util.JSPTestCase;
import eimtest.app.util.TestAppMisc;

/** */
public class DspFolderTreeTest extends JSPTestCase {
	/**
	 * 
	 * @return o
	 * @throws Exception
	 */
	public static Test suite() throws Exception {
		TestSuite suite = new TestSuite(DspFolderTreeTest.class);
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
		// systemユーザー
		String assertText = TestAppMisc.loadAppTextFile(this, "_01.txt");
		assertText = TestAppMisc.replaceAssertText(assertText, objs);
		assertEquals(assertText, xu.toStr(xu.toDOM(ds.folder_dspFolderTree())));

		// 書込権限
		ds.switchUser("tu1");
		assertText = TestAppMisc.loadAppTextFile(this, "_02.txt");
		assertText = TestAppMisc.replaceAssertText(assertText, objs);
		assertEquals(assertText, xu.toStr(xu.toDOM(ds.folder_dspFolderTree())));

		// ステータス変更権限
		ds.switchUser("tu2");
		assertEquals(assertText, xu.toStr(xu.toDOM(ds.folder_dspFolderTree())));

		// 常時読取権限
		ds.switchUser("tu3");
		assertEquals(assertText, xu.toStr(xu.toDOM(ds.folder_dspFolderTree())));

		// 公開読取権限
		ds.switchUser("tu4");
		assertText = TestAppMisc.loadAppTextFile(this, "_03.txt");
		assertText = TestAppMisc.replaceAssertText(assertText, objs);
		assertEquals(assertText, xu.toStr(xu.toDOM(ds.folder_dspFolderTree())));
	}
}
