package app.document.attrTree;

import java.util.Arrays;
import java.util.Iterator;

import junit.extensions.TestSetup;
import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.commons.lang.ClassUtils;

import common.bo.AttributeTree;
import common.bo.AttributeTreeItem;
import common.util.AttributeTreeUtil;

import eim.bo.EIMResource;
import eim.net.EIMSession;
import eim.util.AttributeUtils;
import eimtest.app.util.JSPTestCase;
import eimtest.app.util.TestAppDBUtil;
import eimtest.app.util.TestAppMisc;
import eimtest.app.util.net.EIMServerResponseError;
import eimtest.util.Misc;
import eimtest.util.TestSessionUtil;

/** */
public class DspAttrTreeTest extends JSPTestCase {
	/**
	 * 
	 * @return o
	 * @throws Exception
	 */
	public static Test suite() throws Exception {
		TestSuite suite = new TestSuite(DspAttrTreeTest.class);
		TestSetup w = new TestSetup(suite) {

			/** */
			public void setUp() throws Exception {
				TestAppDBUtil.loadPrimitiveData();
				EIMSession sess = TestSessionUtil.createEIMSession();
				TestAttrTreeUtil.createFolderTree(sess);
				sess.commit();
			}
		};
		return w;
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void test0NoAttrTree() throws Exception {
		ds.switchUser("tu1");

		// TestCase:属性ツリー定義無しでの検証
		String assertText = Misc.loadFile(this.getClass().getResource(
			ClassUtils.getShortClassName(this.getClass()) + "0_01.txt").getPath());
		// <nodes></nodes>を検証
		assertEquals(assertText, xu.toStr(xu.toDOM(ds.attrTree_dspAttrTree(-1, null, null))));
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void test1AttrTree() throws Exception {
		// TestCase:属性ツリー定義が1つでの検証
		AttributeTree attrt1 = TestAttrTreeUtil.createAttrTree1(sess);

		// 検証用雛形XMLをテキストファイルから読み込み
		String assertBaseText = Misc.loadFile(this.getClass().getResource(
			ClassUtils.getShortClassName(this.getClass()) + "1_01.txt").getPath());

		// 検証用XMLを生成
		String assertText = TestAttrTreeUtil.replaceAssertText(sess, assertBaseText, attrt1, null,
			null);

		// JSP返却結果XMLを検証
		assertEquals(assertText, xu.toStr(xu.toDOM(ds.attrTree_dspAttrTree(-1, null, null))));

		// TestCase:英語名を検証
		ds.switchUser("tu1", null, "EN", null);
		// 検証用XMLを生成
		assertText = assertBaseText//
		.replaceAll("\\[id\\_1\\]", String.valueOf(attrt1.getId()))//
		.replaceAll("\\[settings\\_1\\]", AttributeTreeUtil.getAttrTreeSettingsStr(sess, attrt1))//
		.replaceAll("\\[name\\_1\\]",
			AttributeTreeUtil.getOtherAttributeTreeName(sess, attrt1.getId(), "EN"))//
		;
		// JSP返却結果XMLを検証
		assertEquals(assertText, xu.toStr(xu.toDOM(ds.attrTree_dspAttrTree(attrt1.getId(), null,
			null))));
		ds.switchUser("tu1", null, "JA", null);

		// TestCase:2つ目の属性ツリー定義で検証
		AttributeTree attrt2 = TestAttrTreeUtil.createAttrTree2(sess);
		// 検証用雛形XMLをテキストファイルから読み込み
		assertBaseText = Misc.loadFile(this.getClass().getResource(
			ClassUtils.getShortClassName(this.getClass()) + "1_02.txt").getPath());
		// 検証用XMLを生成
		assertText = TestAttrTreeUtil.replaceAssertText(sess, assertBaseText, attrt1, attrt2, null);
		// JSP返却結果XMLを検証
		assertEquals(assertText, xu.toStr(xu.toDOM(ds.attrTree_dspAttrTree(-1, null, null))));

		// TestCase:展開する属性ツリーを指定
		// 検証用雛形XMLをテキストファイルから読み込み
		assertBaseText = Misc.loadFile(this.getClass().getResource(
			ClassUtils.getShortClassName(this.getClass()) + "1_03.txt").getPath());
		// 検証用XMLを生成
		assertText = TestAttrTreeUtil.replaceAssertText(sess, assertBaseText, attrt1, attrt2, null);
		// JSP返却結果XMLを検証
		assertEquals(assertText, xu.toStr(xu.toDOM(ds.attrTree_dspAttrTree(attrt2.getId(), null,
			null))));
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testNoItemAttrTree() throws Exception {
		AttributeTree attrt1 = null;
		AttributeTree attrt2 = null;
		for (Iterator i = AttributeTreeUtil.getAttributeTreeList(sess).iterator(); i.hasNext();) {
			AttributeTree attrt = (AttributeTree) i.next();
			if (attrt.getName().equals("t数値-t日付"))
				attrt1 = attrt;
			else if (attrt.getName().equals("t文字-tテキスト"))
				attrt2 = attrt;
		}

		AttributeTree attrt3 = AttributeTreeUtil.createAttributeTree(sess, "x-attrTree3", 0);
		sess.commit();

		// TestCase:属性ツリー項目を持たないattrt3は返却リストに含まれない
		// 検証用雛形XMLをテキストファイルから読み込み、検証用XMLを生成
		String assertText = TestAttrTreeUtil.replaceAssertText(sess, TestAppMisc.loadAppTextFile(
			this, "1_02.txt"), attrt1, attrt2, null);
		// JSP返却結果XMLを検証
		assertEquals(assertText, xu.toStr(xu.toDOM(ds.attrTree_dspAttrTree(-1, null, null))));

		// TestCase:属性ツリー項目を持たないattrt3を直接指定したらエラー
		try {
			assertEquals(assertText, xu.toStr(xu.toDOM(ds.attrTree_dspAttrTree(attrt3.getId(),
				null, null))));
			fail();
		} catch (EIMServerResponseError e) {
			assertEquals(EIMResource.getMessage("JA", "EIM.ERROR.LOGIC.NOATTRTREE"), e.getMessage());
		}

		// TestCase:属性ツリー項目を持つようになればattrt3は返却リストに含まれる
		AttributeTreeUtil.setTreeItems(sess, attrt3,
			Arrays.asList(new Object[] { new AttributeTreeItem(-1,
					AttributeUtils.getAttributeTypeByName(sess, "パス"), false) }));
		sess.commit();
		attrt3 = AttributeTreeUtil.getAttributeTreeById(sess, attrt3.getId());
		// 検証用雛形XMLをテキストファイルから読み込み、検証用XMLを生成
		assertText = TestAttrTreeUtil.replaceAssertText(sess,
			TestAppMisc.loadAppTextFile(this, "1_04.txt"), attrt1, attrt2, null)//
		.replaceAll("\\[id\\_3\\]", String.valueOf(attrt3.getId()))//
		.replaceAll("\\[settings\\_3\\]", AttributeTreeUtil.getAttrTreeSettingsStr(sess, attrt3))//
		.replaceAll("\\[name\\_3\\]", attrt3.getName());
		// JSP返却結果XMLを検証
		assertEquals(assertText, xu.toStr(xu.toDOM(ds.attrTree_dspAttrTree(-1, null, null))));
	}

}
