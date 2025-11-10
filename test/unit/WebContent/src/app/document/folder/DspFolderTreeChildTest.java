package app.document.folder;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import junit.extensions.TestSetup;
import junit.framework.Test;
import junit.framework.TestSuite;
import app.document.attrTree.TestAttrTreeUtil;

import common.bo.AttributeTree;
import common.bo.AttributeTreeItem;
import common.util.AttributeTreeUtil;

import eim.bo.EIMAttributeType;
import eim.bo.EIMObject;
import eim.bo.EIMResource;
import eim.bo.EIMValueType;
import eim.util.AttributeUtils;
import eimtest.app.util.JSPTestCase;
import eimtest.app.util.TestAppMisc;
import eimtest.app.util.TestAppObjectUtil;
import eimtest.app.util.net.EIMServerResponseError;

/** */
public class DspFolderTreeChildTest extends JSPTestCase {
	/**
	 * 
	 * @return o
	 * @throws Exception
	 */
	public static Test suite() throws Exception {
		TestSuite suite = new TestSuite(DspFolderTreeChildTest.class);
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

	/** */
	AttributeTree attrt1;

	/** */
	String attrt1settings;

	/** */
	String attrt2settings;

	/** */
	AttributeTree attrt2;

	/** */
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
	}

	protected void tearDown() throws Exception {
		sess.close();
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testAbnormal2() throws Exception {
		EIMObject f21 = TestAppObjectUtil.getObj(sess, "F21");
		ds.switchUser("tu1");
		// TestCase:属性ツリーの初段が最新では変更
		try {
			ds.folder_dspFolderTreeChild(f21.getId(), attrt1.getId(), new String[] { "999",
					"1025449200000" }, attrt1settings);
			fail();
		} catch (EIMServerResponseError e) {
			assertEquals(EIMResource.getMessage("JA", "EIM.ERROR.LOGIC.ATTRTREE.NOTEXIST.FOLDAER"),
				e.getMessage());
		}

		// TestCase:属性ツリーの2段目が最新では変更
		try {
			ds.folder_dspFolderTreeChild(f21.getId(), attrt1.getId(), new String[] { "2", "999" },
				attrt1settings);
			fail();
		} catch (EIMServerResponseError e) {
			assertEquals(EIMResource.getMessage("JA", "EIM.ERROR.LOGIC.ATTRTREE.NOTEXIST.FOLDAER"),
				e.getMessage());
		}

		// TestCase:属性ツリーの3段目のフォルダが最新では存在せず
		try {
			ds.folder_dspFolderTreeChild(f21.getId(), attrt1.getId(), new String[] { "2",
					"1025449200000", "999" }, attrt1settings);
			fail();
		} catch (EIMServerResponseError e) {
			assertEquals(EIMResource.getMessage("JA", "EIM.ERROR.LOGIC.ATTRTREE.NOTEXIST.FOLDAER"),
				e.getMessage());
		}

		// TestCase:属性ツリーの3段目のフォルダが最新では別の位置
		try {
			ds.folder_dspFolderTreeChild(f21.getId(), attrt1.getId(),
				new String[] { "2", "1025449200000",
						String.valueOf(TestAppObjectUtil.findObj("F3", objs).getId()) },
				attrt1settings);
			fail();
		} catch (EIMServerResponseError e) {
			assertEquals(EIMResource.getMessage("JA", "EIM.ERROR.LOGIC.ATTRTREE.NOTEXIST.FOLDAER"),
				e.getMessage());
		}

		// TestCase:属性ツリーの4段目のフォルダが最新では存在せず
		try {
			ds.folder_dspFolderTreeChild(f21.getId(), attrt1.getId(), new String[] { "2",
					"1025449200000",
					String.valueOf(TestAppObjectUtil.findObj("F21", objs).getId()), "999" },
				attrt1settings);
			fail();
		} catch (EIMServerResponseError e) {
			assertEquals(EIMResource.getMessage("JA", "EIM.ERROR.LOGIC.ATTRTREE.NOTEXIST.FOLDAER"),
				e.getMessage());
		}

		// TestCase:属性ツリーの4段目のフォルダが最新では別の位置
		try {
			ds.folder_dspFolderTreeChild(f21.getId(), attrt1.getId(), new String[] { "2",
					"1025449200000",
					String.valueOf(TestAppObjectUtil.findObj("F21", objs).getId()),
					String.valueOf(TestAppObjectUtil.findObj("F3", objs).getId()) }, attrt1settings);
			fail();
		} catch (EIMServerResponseError e) {
			assertEquals(EIMResource.getMessage("JA", "EIM.ERROR.LOGIC.ATTRTREE.NOTEXIST.FOLDAER"),
				e.getMessage());
		}
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testMe() throws Exception {
		// TestCase:attr1の末端フォルダF21を展開
		String assertText = TestAppMisc.loadAppTextFile(this, "_01.txt");
		assertText = TestAttrTreeUtil.replaceAssertText(sess, assertText, attrt1, attrt2, objs);
		EIMObject f21 = TestAppObjectUtil.getObj(sess, "F21");
		assertEquals(assertText, xu.toStr(xu.toDOM(ds.folder_dspFolderTreeChild(f21.getId(),
			attrt1.getId()//
			, new String[] { "2", "1025449200000", String.valueOf(f21.getId()) }, attrt1settings))));

		// TestCase:attr1の末端フォルダF21のF211を展開
		assertText = TestAppMisc.loadAppTextFile(this, "_02.txt");
		assertText = TestAttrTreeUtil.replaceAssertText(sess, assertText, attrt1, attrt2, objs);
		EIMObject f211 = TestAppObjectUtil.getObj(sess, "F211");
		assertEquals(assertText, xu.toStr(xu.toDOM(ds.folder_dspFolderTreeChild(f211.getId(),
			attrt1.getId()//
			, new String[] { "2", "1025449200000", String.valueOf(f21.getId()),
					String.valueOf(f211.getId()) }, attrt1settings))));

		// TestCase:通常動作
		assertText = TestAppMisc.loadAppTextFile(this, "_03.txt");
		assertText = TestAttrTreeUtil.replaceAssertText(sess, assertText, attrt1, attrt2, objs);
		EIMObject projX = TestAppObjectUtil.getObj(sess, "A-proj-X");
		assertEquals(assertText, xu.toStr(xu.toDOM(ds.folder_dspFolderTreeChild(projX.getId(), -1//
			, null, null))));
	}

	/**
	 * 異常系
	 * 
	 * @throws Exception
	 */
	public void testAbnormal() throws Exception {
		EIMObject f21 = TestAppObjectUtil.getObj(sess, "F21");
		// TestCase:ログインエラーを検証
		ds.br.eimLogout();
		try {
			ds.folder_dspFolderTreeChild(f21.getId(), -1, null, null);
			fail();
		} catch (EIMServerResponseError e) {
			assertEquals(EIMResource.getMessage("JA", "EIM.ERROR.SESSIONTIMEOUT"), e.getMessage());
		}

		ds.switchUser("tu1");
		// TestCase:属性ツリーが存在しないエラーを検証
		try {
			ds.folder_dspFolderTreeChild(f21.getId(), 1234, null, null);
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
			ds.folder_dspFolderTreeChild(f21.getId(), attrt1.getId(), null, null);
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
			ds.folder_dspFolderTreeChild(f21.getId(), attrt1.getId(), new String[] { "2",
					"1025449200000", String.valueOf(f21.getId()) }, attrt1settings);
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
