package app.document.object;

import junit.extensions.TestSetup;
import junit.framework.Test;
import junit.framework.TestSuite;

import org.w3c.dom.Document;

import eim.bo.EIMObject;
import eim.bo.EIMRelation;
import eim.bo.EIMResource;
import eim.net.EIMSession;
import eim.util.ObjectUtils;
import eim.util.RelationUtils;
import eimtest.app.util.CreateFolderTreeData;
import eimtest.app.util.JSPTestCase;
import eimtest.app.util.TestAppDBUtil;
import eimtest.app.util.TestAppMisc;
import eimtest.app.util.TestAppObjectUtil;
import eimtest.app.util.net.EIMServerResponseError;
import eimtest.util.TestSessionUtil;

/** */
public class DspParentFolderTest extends JSPTestCase {
	/**
	 * 
	 * @return o
	 * @throws Exception
	 */
	public static Test suite() throws Exception {
		TestSuite suite = new TestSuite(DspParentFolderTest.class);
		TestSetup w = new TestSetup(suite) {

			/** */
			public void setUp() throws Exception {
				if (true) {
					TestAppDBUtil.loadPrimitiveData();
					EIMSession sess = TestSessionUtil.createEIMSession();
					EIMObject obj = TestAppObjectUtil.getObj(sess, "F1");
					ObjectUtils.rename(sess, obj, "WS1F1");
					new CreateFolderTreeData(sess, null).process();
					obj = TestAppObjectUtil.getObj(sess, "%-WFステータステスト");
					ObjectUtils.rename(sess, obj, "A-WFステータステスト");
					sess.commit();
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
		Document xdoc = xu.toDOM(ds.object_dspParentFolder(
			TestAppObjectUtil.getObj(sess, "プロジェクトX").getId(), null));
		assertEquals("[" + TestAppObjectUtil.getObj(sess, "A-WFステータステスト").getId() + "]",
			TestAppMisc.getNodeValues(xdoc, "//object/@objId").toString());

		// アクセス権チェック
		ds.switchUser("tu1");
		xdoc = xu.toDOM(ds.object_dspParentFolder(
			TestAppObjectUtil.getObj(sess, "F編集中2.txt").getId(), null));
		assertEquals("[" + TestAppObjectUtil.getObj(sess, "F編集中").getId() + "]",
			TestAppMisc.getNodeValues(xdoc, "//object/@objId").toString());

		ds.switchUser("tu2");
		xdoc = xu.toDOM(ds.object_dspParentFolder(
			TestAppObjectUtil.getObj(sess, "F編集中2.txt").getId(), null));
		assertEquals("[" + TestAppObjectUtil.getObj(sess, "F編集中").getId() + "]",
			TestAppMisc.getNodeValues(xdoc, "//object/@objId").toString());

		ds.switchUser("tu3");
		xdoc = xu.toDOM(ds.object_dspParentFolder(
			TestAppObjectUtil.getObj(sess, "F編集中2.txt").getId(), null));
		assertEquals("[" + TestAppObjectUtil.getObj(sess, "F編集中").getId() + "]",
			TestAppMisc.getNodeValues(xdoc, "//object/@objId").toString());

		// アクセス権なしエラー
		try {
			ds.switchUser("tu4");
			xdoc = xu.toDOM(ds.object_dspParentFolder(
				TestAppObjectUtil.getObj(sess, "F編集中2.txt").getId(), null));
			fail();
		} catch (EIMServerResponseError e) {
			assertEquals(EIMResource.getMessage("JA", "EIM.ERROR.LOGIC.NOACCESS"), e.getMessage());
		}

		// 指定オブジェクトなしエラー
		try {
			ds.switchUser("tu4");
			xdoc = xu.toDOM(ds.object_dspParentFolder(-1234, null));
			fail();
		} catch (EIMServerResponseError e) {
			assertEquals(EIMResource.getMessage("JA", "EIM.ERROR.LOGIC.NOTEXIST.DOCFOL"),
				e.getMessage());
		}

		// ごみ箱配下
		ds.switchUser("system", "manager");
		// F承認依頼中フォルダをごみ箱に
		EIMObject o = TestAppObjectUtil.getObj(sess, "F承認依頼中");
		EIMObject recycle = TestAppObjectUtil.getObj(sess, "ごみ箱");
		RelationUtils.replaceParentRelation(sess,
			(EIMRelation) RelationUtils.getParentRelationList(sess, o).get(0), recycle);
		sess.commit();
		xdoc = xu.toDOM(ds.object_dspParentFolder(
			TestAppObjectUtil.getObj(sess, "F承認依頼中3.txt").getId(), null));
		assertEquals("[" + recycle.getId() + "]",
			TestAppMisc.getNodeValues(xdoc, "//object/@objId").toString());

		// isURLLogin
		ds.switchUser("tu1");
		xdoc = xu.toDOM(ds.object_dspParentFolder(TestAppObjectUtil.getObj(sess, "F編集中").getId(),
			"true"));
		assertEquals("[" + TestAppObjectUtil.getObj(sess, "F編集中").getId() + "]",
			TestAppMisc.getNodeValues(xdoc, "//object/@objId").toString());

		// 念のため、isURLLogin無指定なら親が取れること
		ds.switchUser("tu1");
		xdoc = xu.toDOM(ds.object_dspParentFolder(TestAppObjectUtil.getObj(sess, "F編集中").getId(),
			null));
		assertEquals("[" + TestAppObjectUtil.getObj(sess, "プロジェクトX").getId() + "]",
			TestAppMisc.getNodeValues(xdoc, "//object/@objId").toString());
	}
}