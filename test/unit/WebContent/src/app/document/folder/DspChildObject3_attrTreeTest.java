package app.document.folder;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import junit.extensions.TestSetup;
import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.commons.lang.ArrayUtils;
import org.w3c.dom.Document;

import app.document.attrTree.TestAttrTreeUtil;
import app.document.search.ActSearchTestUtil;

import common.bo.AttributeTree;
import common.bo.AttributeTreeItem;
import common.util.AttributeTreeUtil;

import eim.bo.EIMAttributeType;
import eim.bo.EIMResource;
import eim.bo.EIMValueType;
import eim.net.EIMSession;
import eim.util.AttributeUtils;
import eimtest.app.util.JSPTestCase;
import eimtest.app.util.TestAppDBUtil;
import eimtest.app.util.TestAppMisc;
import eimtest.app.util.TestAppObjectUtil;
import eimtest.app.util.net.EIMServerResponseError;
import eimtest.util.TestSessionUtil;

/** */
public class DspChildObject3_attrTreeTest extends JSPTestCase {
	/**
	 * 
	 * @return o
	 * @throws Exception
	 */
	public static Test suite() throws Exception {
		TestSuite suite = new TestSuite(DspChildObject3_attrTreeTest.class);
		TestSetup w = new TestSetup(suite) {

			/** */
			public void setUp() throws Exception {
				if (true) {
					TestAppDBUtil.loadPrimitiveData();
					EIMSession sess = TestSessionUtil.createEIMSession();
					TestAttrTreeUtil.createFolderTree(sess);
					TestAttrTreeUtil.createAttrTree1(sess);
					TestAttrTreeUtil.createAttrTree2(sess);
					sess.commit();
				}
			}
		};
		return w;
	}

	/** */
	AttributeTree attrt1;

	/** */
	String attrt1settings;

	/** */
	String attrt2settings;

	/** */
	AttributeTree attrt2;

	public void setUp() throws Exception {
		super.setUp();
		List attrts = AttributeTreeUtil.getAttributeTreeList(sess);
		for (Iterator i = attrts.iterator(); i.hasNext();) {
			AttributeTree attrt = (AttributeTree) i.next();
			if (attrt.getName().equals("t数値-t日付"))
				attrt1 = attrt;
			else if (attrt.getName().equals("t文字-tテキスト"))
				attrt2 = attrt;
		}
		attrt1settings = AttributeTreeUtil.getAttrTreeSettingsStr(sess, attrt1);
		attrt2settings = AttributeTreeUtil.getAttrTreeSettingsStr(sess, attrt2);
		objs = TestAppObjectUtil.getFolderAndDocObjs(sess);
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testAccessRole() throws Exception {
		// フォルダ分類
		// TestCase:書込権限
		ds.switchUser("tu1", null);
		long[] assertObjIds = { //
		TestAppObjectUtil.getObj(sess, "F21").getId(),
				TestAppObjectUtil.getObj(sess, "F6").getId(), };
		Document xdoc = xu.toDOM(ds.folder_dspChildObject(null, attrt1.getId()//
			, new String[] { "2", "1025449200000" }, attrt1settings, "d/e/f"));
		List objIds = ActSearchTestUtil.getObjIdsFromResult(xdoc);
		assertEquals(Arrays.asList(ArrayUtils.toObject(assertObjIds)).toString(), objIds.toString());

		// TestCase:ステータス変更権限
		ds.switchUser("tu2", null);
		xdoc = xu.toDOM(ds.folder_dspChildObject(null, attrt1.getId()//
			, new String[] { "2", "1025449200000" }, attrt1settings, "d/e/f"));
		objIds = ActSearchTestUtil.getObjIdsFromResult(xdoc);
		assertEquals(Arrays.asList(ArrayUtils.toObject(assertObjIds)).toString(), objIds.toString());

		// TestCase:常時読取権限
		ds.switchUser("tu3", null);
		xdoc = xu.toDOM(ds.folder_dspChildObject(null, attrt1.getId()//
			, new String[] { "2", "1025449200000" }, attrt1settings, "d/e/f"));
		objIds = ActSearchTestUtil.getObjIdsFromResult(xdoc);
		assertEquals(Arrays.asList(ArrayUtils.toObject(assertObjIds)).toString(), objIds.toString());

		// TestCase:公開読取権限
		ds.switchUser("tu4", null);
		assertObjIds = new long[] { //				
		TestAppObjectUtil.getObj(sess, "F6").getId(), };
		xdoc = xu.toDOM(ds.folder_dspChildObject(null, attrt1.getId()//
			, new String[] { "2", "1025449200000" }, attrt1settings, "d/e/f"));
		objIds = ActSearchTestUtil.getObjIdsFromResult(xdoc);
		assertEquals(Arrays.asList(ArrayUtils.toObject(assertObjIds)).toString(), objIds.toString());

		// ドキュメント分類
		// TestCase:書込権限
		ds.switchUser("tu1", null);
		assertObjIds = new long[] { //
		TestAppObjectUtil.getObj(sess, "t3_1.txt").getId() };
		xdoc = xu.toDOM(ds.folder_dspChildObject(null, attrt2.getId()//
			, new String[] { "c", "お\nか" }, attrt2settings, "d/e/f"));
		objIds = ActSearchTestUtil.getObjIdsFromResult(xdoc);
		assertEquals(Arrays.asList(ArrayUtils.toObject(assertObjIds)).toString(), objIds.toString());

		// TestCase:ステータス変更権限
		ds.switchUser("tu2", null);
		xdoc = xu.toDOM(ds.folder_dspChildObject(null, attrt2.getId()//
			, new String[] { "c", "お\nか" }, attrt2settings, "d/e/f"));
		objIds = ActSearchTestUtil.getObjIdsFromResult(xdoc);
		assertEquals(Arrays.asList(ArrayUtils.toObject(assertObjIds)).toString(), objIds.toString());

		// TestCase:常時読取権限
		ds.switchUser("tu3", null);
		xdoc = xu.toDOM(ds.folder_dspChildObject(null, attrt2.getId()//
			, new String[] { "c", "お\nか" }, attrt2settings, "d/e/f"));
		objIds = ActSearchTestUtil.getObjIdsFromResult(xdoc);
		assertEquals(Arrays.asList(ArrayUtils.toObject(assertObjIds)).toString(), objIds.toString());

		// TestCase:公開読取権限
		ds.switchUser("tu4", null);
		xdoc = xu.toDOM(ds.folder_dspChildObject(null, attrt2.getId()//
			, new String[] { "c", "お\nか" }, attrt2settings, "d/e/f"));
		objIds = ActSearchTestUtil.getObjIdsFromResult(xdoc);
		assertEquals("[]", objIds.toString());
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testClassify() throws Exception {
		long[] assertObjIds = { //
		TestAppObjectUtil.getObj(sess, "F21").getId(),
				TestAppObjectUtil.getObj(sess, "F6").getId(), };
		Document xdoc = xu.toDOM(ds.folder_dspChildObject(null, attrt1.getId()//
			, new String[] { "2", "1025449200000" }, attrt1settings, "d/e/f"));
		List objIds = ActSearchTestUtil.getObjIdsFromResult(xdoc);
		assertEquals(Arrays.asList(ArrayUtils.toObject(assertObjIds)).toString(), objIds.toString());

		assertObjIds = new long[] { //
		TestAppObjectUtil.getObj(sess, "t3_1.txt").getId() };
		xdoc = xu.toDOM(ds.folder_dspChildObject(null, attrt2.getId()//
			, new String[] { "c", "お\nか" }, attrt2settings, "d/e/f"));
		objIds = ActSearchTestUtil.getObjIdsFromResult(xdoc);
		assertEquals(Arrays.asList(ArrayUtils.toObject(assertObjIds)).toString(), objIds.toString());

	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testAbnormal2() throws Exception {
		ds.switchUser("tu1");
		// TestCase:属性ツリーの初段が最新では変更
		String assertBaseText = TestAppMisc.loadAppTextFile(this, "_a01.txt");
		String assertText = TestAttrTreeUtil.replaceAssertText(sess, assertBaseText, attrt1,
			attrt2, null).replaceAll("\\[val\\_1\\]", "999").replaceAll("\\[val\\_2\\]",
			"1025449200000");
		assertEquals(assertText, xu.toStr(xu.toDOM(ds.folder_dspChildObject(null, attrt1.getId(),
			new String[] { "999", "1025449200000" }, attrt1settings, "a/b/c"))));

		// TestCase:属性ツリーの2段目が最新では変更
		assertText = TestAttrTreeUtil.replaceAssertText(sess, assertBaseText, attrt1, attrt2, null).replaceAll(
			"\\[val\\_1\\]", "2").replaceAll("\\[val\\_2\\]", "999");
		assertEquals(assertText, xu.toStr(xu.toDOM(ds.folder_dspChildObject(null, attrt1.getId(),
			new String[] { "2", "999" }, attrt1settings, "a/b/c"))));

		// TestCase:AttrTreeValueNが多すぎてエラー
		try {
			ds.folder_dspChildObject(null, attrt1.getId(), new String[] { "2", "1025449200000",
					"999" }, attrt1settings, "a/b/c");
			fail();
		} catch (EIMServerResponseError e) {
			assertEquals(EIMResource.getMessage("JA",
				"EIM.ERROR.LOGIC.ATTRTREE.MANY_OR_LESS.VALUES"), e.getMessage());
		}

		// TestCase:AttrTreeValueNが少なくてエラー
		try {
			ds.folder_dspChildObject(null, attrt1.getId(), new String[] { "2" }, attrt1settings,
				"a/b/c");
			fail();
		} catch (EIMServerResponseError e) {
			assertEquals(EIMResource.getMessage("JA",
				"EIM.ERROR.LOGIC.ATTRTREE.MANY_OR_LESS.VALUES"), e.getMessage());
		}

	}

	/**
	 * 異常系
	 * 
	 * @throws Exception
	 */
	public void testAbnormal() throws Exception {
		// TestCase:ログインエラーを検証
		ds.br.eimLogout();
		try {
			ds.folder_dspChildObject(null, -1, null, null, null);
			fail();
		} catch (EIMServerResponseError e) {
			assertEquals(EIMResource.getMessage("JA", "EIM.ERROR.SESSIONTIMEOUT"), e.getMessage());
		}

		ds.switchUser("tu1");
		// TestCase:属性ツリーが存在しないエラーを検証
		try {
			ds.folder_dspChildObject(null, 1234, null, null, null);
			fail();
		} catch (EIMServerResponseError e) {
			assertEquals(EIMResource.getMessage("JA", "EIM.ERROR.LOGIC.NOATTRTREE"), e.getMessage());
		}

		// TestCase:属性ツリーで使用する属性が存在しないエラーを検証
		EIMAttributeType attrHoge = AttributeUtils.createAttributeType(sess, "attrHoge",
			new EIMValueType(sess, EIMValueType.INTEGER));
		List items = new ArrayList(attrt1.getTreeItemList());
		items.add(new AttributeTreeItem(-1, attrHoge, false));
		AttributeTreeUtil.setTreeItems(sess, attrt1, items);
		attrt1 = AttributeTreeUtil.getAttributeTreeById(sess, attrt1.getId());
		AttributeUtils.deleteAttributeType(sess, attrHoge);
		sess.commit();
		try {
			ds.folder_dspChildObject(null, attrt1.getId(), null, null, null);
			fail();
		} catch (EIMServerResponseError e) {
			assertEquals(EIMResource.getMessage("JA", "EIM.ERROR.LOGIC.NOATTRTYPE"), e.getMessage());
		}
		items.remove(items.size() - 1);
		AttributeTreeUtil.setTreeItems(sess, attrt1, items);
		attrt1 = AttributeTreeUtil.getAttributeTreeById(sess, attrt1.getId());
		sess.commit();

		// TestCase:属性ツリーの定義が変更されている
		// 1番目の項目の属性値なし表示フラグを反転
		AttributeTreeItem item = (AttributeTreeItem) items.get(0);
		items.set(0, new AttributeTreeItem(-1, item.getType(), !item.isViewNoValues()));
		AttributeTreeUtil.setTreeItems(sess, attrt1, items);
		attrt1 = AttributeTreeUtil.getAttributeTreeById(sess, attrt1.getId());
		sess.commit();
		try {
			ds.folder_dspChildObject(null, attrt1.getId(), null, attrt1settings, "a/b/c");
			fail();
		} catch (EIMServerResponseError e) {
			assertEquals(EIMResource.getMessage("JA", "EIM.ERROR.LOGIC.ATTRTREE.REG.CHANGED"),
				e.getMessage());
		}
		items.set(0, item);
		AttributeTreeUtil.setTreeItems(sess, attrt1, items);
		attrt1 = AttributeTreeUtil.getAttributeTreeById(sess, attrt1.getId());
		sess.commit();
	}

}
