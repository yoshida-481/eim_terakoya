package app.document.attrTree;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import junit.extensions.TestSetup;
import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.commons.lang.ClassUtils;

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
import eimtest.app.util.TestAppObjectUtil;
import eimtest.app.util.net.EIMServerResponseError;
import eimtest.util.Misc;
import eimtest.util.TestSessionUtil;

/** */
public class DspAttrTreeChildTest extends JSPTestCase {
	/**
	 * 
	 * @return o
	 * @throws Exception
	 */
	public static Test suite() throws Exception {
		TestSuite suite = new TestSuite(DspAttrTreeChildTest.class);
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
		ds.switchUser("tu4");// 公開読取
		// TestCase:アクセス権
		// 検証用雛形XMLを作成
		String assertBaseText = Misc.loadFile(this.getClass().getResource(
			ClassUtils.getShortClassName(this.getClass()) + "a_01.txt").getPath());
		String assertText = TestAttrTreeUtil.replaceAssertText(sess, assertBaseText, attrt1,
			attrt2, objs);
		// JSP返却結果XMLを検証
		assertEquals(assertText, xu.toStr(xu.toDOM(ds.attrTree_dspAttrTreeChild(attrt1.getId(),
			new String[] { "2", "1025449200000" }, attrt1settings))));

		ds.switchUser("tu3");// 常時読取
		// TestCase:アクセス権
		// 検証用雛形XMLを作成
		assertBaseText = Misc.loadFile(this.getClass().getResource(
			ClassUtils.getShortClassName(this.getClass()) + "a_02.txt").getPath());
		assertText = TestAttrTreeUtil.replaceAssertText(sess, assertBaseText, attrt1, attrt2, objs);
		// JSP返却結果XMLを検証
		assertEquals(assertText, xu.toStr(xu.toDOM(ds.attrTree_dspAttrTreeChild(attrt1.getId(),
			new String[] { "2", "1025449200000" }, attrt1settings))));

		ds.switchUser("tu2");// ステータス変更
		// TestCase:アクセス権
		// JSP返却結果XMLを検証
		assertEquals(assertText, xu.toStr(xu.toDOM(ds.attrTree_dspAttrTreeChild(attrt1.getId(),
			new String[] { "2", "1025449200000" }, attrt1settings))));

		ds.switchUser("tu1");// 書込
		// TestCase:アクセス権
		// JSP返却結果XMLを検証
		assertEquals(assertText, xu.toStr(xu.toDOM(ds.attrTree_dspAttrTreeChild(attrt1.getId(),
			new String[] { "2", "1025449200000" }, attrt1settings))));
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testMe() throws Exception {
		ds.switchUser("tu1");

		// TestCase:掘り下げ1段目
		// 検証用雛形XMLを作成
		String assertBaseText = Misc.loadFile(this.getClass().getResource(
			ClassUtils.getShortClassName(this.getClass()) + "_01.txt").getPath());
		String assertText = TestAttrTreeUtil.replaceAssertText(sess, assertBaseText, attrt1,
			attrt2, objs);
		// JSP返却結果XMLを検証
		assertEquals(assertText, xu.toStr(xu.toDOM(ds.attrTree_dspAttrTreeChild(attrt1.getId(),
			null, attrt1settings))));

		// TestCase:掘り下げ2段目
		// 検証用雛形XMLを作成
		assertBaseText = Misc.loadFile(this.getClass().getResource(
			ClassUtils.getShortClassName(this.getClass()) + "_02.txt").getPath());
		assertText = TestAttrTreeUtil.replaceAssertText(sess, assertBaseText, attrt1, attrt2, objs);
		// JSP返却結果XMLを検証
		assertEquals(assertText, xu.toStr(xu.toDOM(ds.attrTree_dspAttrTreeChild(attrt1.getId(),
			new String[] { "2" }, attrt1settings))));

		// TestCase:掘り下げ3段目のフォルダ
		// 検証用雛形XMLを作成
		assertBaseText = Misc.loadFile(this.getClass().getResource(
			ClassUtils.getShortClassName(this.getClass()) + "_03.txt").getPath());
		assertText = TestAttrTreeUtil.replaceAssertText(sess, assertBaseText, attrt1, attrt2, objs);
		// JSP返却結果XMLを検証
		assertEquals(assertText, xu.toStr(xu.toDOM(ds.attrTree_dspAttrTreeChild(attrt1.getId(),
			new String[] { "2", "1025449200000" }, attrt1settings))));

		// TestCase:属性値なしを掘り下げ
		// 検証用雛形XMLを作成
		assertBaseText = Misc.loadFile(this.getClass().getResource(
			ClassUtils.getShortClassName(this.getClass()) + "_04.txt").getPath());
		assertText = TestAttrTreeUtil.replaceAssertText(sess, assertBaseText, attrt1, attrt2, objs);
		// JSP返却結果XMLを検証
		assertEquals(assertText, xu.toStr(xu.toDOM(ds.attrTree_dspAttrTreeChild(attrt1.getId(),
			new String[] { "", "" }, attrt1settings))));

		// TestCase:attrt2を掘り下げ1段目
		// 検証用雛形XMLを作成
		assertBaseText = Misc.loadFile(this.getClass().getResource(
			ClassUtils.getShortClassName(this.getClass()) + "_05.txt").getPath());
		assertText = TestAttrTreeUtil.replaceAssertText(sess, assertBaseText, attrt1, attrt2, objs);
		// JSP返却結果XMLを検証
		assertEquals(assertText, xu.toStr(xu.toDOM(ds.attrTree_dspAttrTreeChild(attrt2.getId(),
			null, attrt2settings))));

		// TestCase:attrt2を掘り下げ2段目
		// 検証用雛形XMLを作成
		assertBaseText = Misc.loadFile(this.getClass().getResource(
			ClassUtils.getShortClassName(this.getClass()) + "_06.txt").getPath());
		assertText = TestAttrTreeUtil.replaceAssertText(sess, assertBaseText, attrt1, attrt2, objs);
		// JSP返却結果XMLを検証
		assertEquals(assertText, xu.toStr(xu.toDOM(ds.attrTree_dspAttrTreeChild(attrt2.getId(),
			new String[] { "b" }, attrt2settings))));

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
			ds.attrTree_dspAttrTreeChild(-1, null, null);
			fail();
		} catch (EIMServerResponseError e) {
			assertEquals(EIMResource.getMessage("JA", "EIM.ERROR.SESSIONTIMEOUT"), e.getMessage());
		}

		ds.switchUser("tu1");
		// TestCase:属性ツリーが存在しないエラーを検証
		try {
			ds.attrTree_dspAttrTreeChild(1234, null, null);
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
			ds.attrTree_dspAttrTreeChild(attrt1.getId(), null, null);
			fail();
		} catch (EIMServerResponseError e) {
			assertEquals(EIMResource.getMessage("JA", "EIM.ERROR.LOGIC.NOATTRTYPE"), e.getMessage());
		}
		items.remove(items.size() - 1);
		AttributeTreeUtil.setTreeItems(sess, attrt1, items);
		attrt1 = AttributeTreeUtil.getAttributeTreeById(sess, attrt1.getId());
		sess.commit();

		// TestCase:属性ツリーの定義が変更されている
		ds.attrTree_dspAttrTreeChild(attrt1.getId(), null, attrt1settings);// 念のため。エラーは発生しない。
		// 1番目の項目の属性値なし表示フラグを反転
		AttributeTreeItem item = (AttributeTreeItem) items.get(0);
		items.set(0, new AttributeTreeItem(-1, item.getType(), !item.isViewNoValues()));
		AttributeTreeUtil.setTreeItems(sess, attrt1, items);
		attrt1 = AttributeTreeUtil.getAttributeTreeById(sess, attrt1.getId());
		sess.commit();
		try {
			ds.attrTree_dspAttrTreeChild(attrt1.getId(), null, attrt1settings);
			fail();
		} catch (EIMServerResponseError e) {
			assertEquals(EIMResource.getMessage("JA", "EIM.ERROR.LOGIC.ATTRTREE.REG.CHANGED"),
				e.getMessage());
		}
		items.set(0, item);
		AttributeTreeUtil.setTreeItems(sess, attrt1, items);
		attrt1 = AttributeTreeUtil.getAttributeTreeById(sess, attrt1.getId());
		sess.commit();

		// TestCase:attrt2はドキュメント分類なのに掘り下げ3段目でエラー
		// 検証用雛形XMLを作成
		try {
			ds.attrTree_dspAttrTreeChild(attrt2.getId(), new String[] { "b", "う\nえ" },
				attrt2settings);
			fail();
		} catch (EIMServerResponseError e) {
			assertEquals(EIMResource.getMessage("JA",
				"EIM.ERROR.LOGIC.ATTRTREE.MANY_OR_LESS.VALUES"), e.getMessage());
		}

	}
}
