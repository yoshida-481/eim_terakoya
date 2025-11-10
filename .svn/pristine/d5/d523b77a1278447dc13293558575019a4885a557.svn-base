package app.document.object;

import java.io.File;
import java.util.Arrays;

import junit.extensions.TestSetup;
import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.commons.lang.ArrayUtils;

import common.util.AppObjectConditionHelper;

import eim.bo.EIMFormat;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMResource;
import eim.bo.EIMUser;
import eim.net.EIMSession;
import eim.util.FileUtils;
import eim.util.ObjectAttributeUtils;
import eim.util.ObjectUtils;
import eim.util.UserUtils;
import eimtest.app.util.CreateFolderTreeData;
import eimtest.app.util.JSPTestCase;
import eimtest.app.util.TestAppDBUtil;
import eimtest.app.util.TestAppMisc;
import eimtest.app.util.TestAppObjectUtil;
import eimtest.app.util.net.EIMServerResponseError;
import eimtest.util.TestSessionUtil;

/** */
public class ActCreateDocumentTest extends JSPTestCase {
	/**
	 * 
	 * @return o
	 * @throws Exception
	 */
	public static Test suite() throws Exception {
		TestSuite suite = new TestSuite(ActCreateDocumentTest.class);
		TestSetup w = new TestSetup(suite) {

			/** */
			public void setUp() throws Exception {
				if (true) {
					TestAppDBUtil.loadPrimitiveData();
					EIMSession sess = TestSessionUtil.createEIMSession();
					new CreateFolderTreeData(sess, null).process();
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
	public void testDocAttr() throws Exception {
		ds.switchUser("tu1");
		// TestCase:ワークフローなしフォルダの下にワークフローなしドキュメントを作成
		EIMObject of = TestAppObjectUtil.findObj("F普通のフォルダ", objs);
		EIMObjectType t = ObjectUtils.getObjectTypeByName(sess, "ツリーテスト用ドキュメント");
		File f = new File(TestAppMisc.getAppFilePath(this.getClass(), ".class"));
		ds.object_actCreateDocument(of.getId(), t.getId(), "", tu1.getId(), "", "1_" + f.getName(),
			f);
		EIMObject o = TestAppObjectUtil.getObj(sess, "1_" + f.getName());
		assertNull(o.getStatus());
		AppObjectConditionHelper h = new AppObjectConditionHelper(sess);
		assertNull(o.getAttribute(h.getAttrNameDocumentHigherWFFolder()));
		assertNull(o.getAttribute(h.getAttrNameOfFromHighAttr()));

		// TestCase:ワークフローなしフォルダの下にワークフロー有りドキュメントを作成
		t = ObjectUtils.getObjectTypeByName(sess, "ツリーテスト用ドキュメントWF");
		ds.object_actCreateDocument(of.getId(), t.getId(), "", tu1.getId(), "", "2_" + f.getName(),
			f);
		o = TestAppObjectUtil.getObj(sess, "2_" + f.getName());
		assertNotNull(o.getStatus());
		assertNull(o.getAttribute(h.getAttrNameDocumentHigherWFFolder()));
		assertNull(o.getAttribute(h.getAttrNameOfFromHighAttr()));

		// TestCase:ワークフロー有りフォルダの下に同じ属性セットを持つワークフローなしドキュメントを作成
		of = TestAppObjectUtil.findObj("F編集中", objs);
		t = ObjectUtils.getObjectTypeByName(sess, "ツリーテスト用ドキュメント");
		ds.object_actCreateDocument(of.getId(), t.getId(), "", tu1.getId(), "", "3_" + f.getName(),
			f);
		o = TestAppObjectUtil.getObj(sess, "3_" + f.getName());
		assertEquals(of.getStatus().getId(), o.getStatus().getId());
		assertEquals(of.getId(), o.getAttribute(h.getAttrNameDocumentHigherWFFolder()).getInt());
		TestAppObjectUtil.assertEqualsAttr(of, o, h.getAttrNameOfToLowAttr(),
			h.getAttrNameOfFromHighAttr());
		TestAppObjectUtil.assertEqualsAttr(of, o, new String[] { //
			"tステータス"//
					, "t開発管理"//
					, "t数値N"//
					, "t日付N"//
					, "t文字N"//
					, "tテキストN" });

		// TestCase:ワークフロー有りフォルダの下に1つしか引き継げない属性セットを持つワークフローなしドキュメントを作成
		EIMObjectType newt = ObjectUtils.createObjectType(sess, "1つだけ引き継ぐドキュメント",
			ObjectUtils.getObjectTypeByName(sess, "フォルダ"));
		ObjectAttributeUtils.applyAttributeType(sess, newt, of.getAttribute("t開発管理").getType());
		EIMFormat fmt = FileUtils.getFormatByName(sess, "一時フォーマット");
		FileUtils.applyFormat(sess, newt, fmt);
		FileUtils.setDefault(sess, newt, fmt);
		sess.commit();

		ds.object_actCreateDocument(of.getId(), newt.getId(), "", tu1.getId(), "", "4_"
				+ f.getName(), f);
		o = TestAppObjectUtil.getObj(sess, "4_" + f.getName());
		assertEquals(of.getStatus().getId(), o.getStatus().getId());
		assertEquals(of.getId(), o.getAttribute(h.getAttrNameDocumentHigherWFFolder()).getInt());
		assertEquals(
			"[" + of.getAttribute("t開発管理").getType().getId() + "]"//
			,
			Arrays.asList(
				ArrayUtils.toObject(o.getAttribute(h.getAttrNameOfFromHighAttr()).getInts())).toString());
		TestAppObjectUtil.assertEqualsAttr(of, o, new String[] { //
			"t開発管理"//
			});
		assertNull(o.getAttribute("t数値N"));//
		assertNull(o.getAttribute("t日付N"));//
		assertNull(o.getAttribute("t文字N"));//
		assertNull(o.getAttribute("tテキストN"));

		// TestCase:ワークフロー有りフォルダの下のフォルダに同じ属性セットを持つワークフローなしドキュメントを作成
		EIMObject off = TestAppObjectUtil.findObj("F編集中_F", objs);
		t = ObjectUtils.getObjectTypeByName(sess, "ツリーテスト用ドキュメント");
		ds.object_actCreateDocument(off.getId(), t.getId(), "", tu1.getId(), "",
			"5_" + f.getName(), f);
		o = TestAppObjectUtil.getObj(sess, "5_" + f.getName());
		assertEquals(of.getStatus().getId(), o.getStatus().getId());// ステータス主管「F編集中」のステータスと同じ
		assertEquals(of.getId(), o.getAttribute(h.getAttrNameDocumentHigherWFFolder()).getInt());// ステータス主管「F編集中」のオブジェクトID
		TestAppObjectUtil.assertEqualsAttr(off, o, h.getAttrNameOfToLowAttr(),
			h.getAttrNameOfFromHighAttr());
		TestAppObjectUtil.assertEqualsAttr(off, o, new String[] { //
			"tステータス"//
					, "t開発管理"//
					, "t数値N"//
					, "t日付N"//
					, "t文字N"//
					, "tテキストN"//
					, "t工程" });

	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testAccessRole() throws Exception {
		// TestCase:ワークフローなしドキュメント
		Object[][] testPattern = {//
		{ new Integer(1), "tu4", "F編集中" }// 公開読み取り権限
				, { new Integer(1), "tu4", "F編集中_F" }//
				, { new Integer(1), "tu4", "F承認依頼中" }//
				, { new Integer(1), "tu4", "F承認依頼中_F" }//
				, { new Integer(1), "tu4", "F公開処理中" }//
				, { new Integer(1), "tu4", "F公開処理中_F" }//
				, { new Integer(1), "tu4", "F公開中" }//
				, { new Integer(1), "tu4", "F公開中_F" }//
				, { new Integer(1), "tu4", "F普通のフォルダ" }//
				, { new Integer(1), "tu4", "F普通のフォルダ_F" }//
				, { new Integer(1), "tu3", "F編集中" }// 常時読取権限
				, { new Integer(1), "tu3", "F編集中_F" }//
				, { new Integer(1), "tu3", "F承認依頼中" }//
				, { new Integer(1), "tu3", "F承認依頼中_F" }//
				, { new Integer(1), "tu3", "F公開処理中" }//
				, { new Integer(1), "tu3", "F公開処理中_F" }//
				, { new Integer(1), "tu3", "F公開中" }//
				, { new Integer(1), "tu3", "F公開中_F" }//
				, { new Integer(1), "tu3", "F普通のフォルダ" }//
				, { new Integer(1), "tu3", "F普通のフォルダ_F" }//
				, { new Integer(1), "tu2", "F編集中" }// ステータス変更権限
				, { new Integer(1), "tu2", "F編集中_F" }//
				, { new Integer(1), "tu2", "F承認依頼中" }//
				, { new Integer(1), "tu2", "F承認依頼中_F" }//
				, { new Integer(1), "tu2", "F公開処理中" }//
				, { new Integer(1), "tu2", "F公開処理中_F" }//
				, { new Integer(1), "tu2", "F公開中" }//
				, { new Integer(1), "tu2", "F公開中_F" }//
				, { new Integer(1), "tu2", "F普通のフォルダ" }//
				, { new Integer(1), "tu2", "F普通のフォルダ_F" }//
				, { new Integer(0), "tu1", "F編集中" }// 書込権限
				, { new Integer(0), "tu1", "F編集中_F" }//
				, { new Integer(1), "tu1", "F承認依頼中" }//
				, { new Integer(1), "tu1", "F承認依頼中_F" }//
				, { new Integer(1), "tu1", "F公開処理中" }//
				, { new Integer(1), "tu1", "F公開処理中_F" }//
				, { new Integer(1), "tu1", "F公開中" }//
				, { new Integer(1), "tu1", "F公開中_F" }//
				, { new Integer(0), "tu1", "F普通のフォルダ" }//
				, { new Integer(0), "tu1", "F普通のフォルダ_F" } //
		};
		doDspCreateOneDocument(testPattern, false);

		// TestCase:ワークフロー付きドキュメント
		for (int i = 0; i < testPattern.length; i++) {
			if (testPattern[i][1].equals("tu1") && ((String) testPattern[i][2]).startsWith("F編集中"))
				testPattern[i][0] = new Integer(2);
		}
		doDspCreateOneDocument(testPattern, true);

		// TestCase:ワークフローなしフォルダの下に一般ドキュメント(ワークフローあり)
		ds.switchUser("tu1");
		EIMObject f = TestAppObjectUtil.getObj(sess, "F普通のフォルダ");
		ds.object_dspCreateOneDocument(f.getId(), -1);
		// 例外発生せず

		// TestCase:ワークフロー有りフォルダの下に一般ドキュメント(ワークフローあり)
		f = TestAppObjectUtil.getObj(sess, "F編集中");
		try {
			ds.object_dspCreateOneDocument(f.getId(), -1);
			fail();
		} catch (EIMServerResponseError e) {
			assertEquals(
				EIMResource.getMessage("JA", "EIM.ERROR.LOGIC.CANTSET.UNDERWF.WFDOCUMENT"),
				e.getMessage());
		}
	}

	/**
	 * 
	 * @param testPattern
	 * @param hasWfDoc
	 * @throws Exception
	 */
	void doDspCreateOneDocument(Object[][] testPattern, boolean hasWfDoc) throws Exception {
		EIMObjectType objType = ObjectUtils.getObjectTypeByName(sess, "ツリーテスト用ドキュメント"
				+ (hasWfDoc ? "WF" : ""));
		File f = new File(TestAppMisc.getAppFilePath(this.getClass(), ".class"));
		for (int i = 0; i < testPattern.length; i++) {
			ds.switchUser((String) testPattern[i][1]);
			EIMUser u = UserUtils.getUserByCode(sess, (String) testPattern[i][1]);
			EIMObject o = TestAppObjectUtil.findObj((String) testPattern[i][2], objs);
			if (o == null)
				throw new RuntimeException(testPattern[i][2] + " is not found");
			try {
				ds.object_actCreateDocument(o.getId(), objType.getId(), "p", u.getId(), "",
					(hasWfDoc ? "WF-" : "") + f.getName(), f);
				if (((Integer) testPattern[i][0]).intValue() != 0)
					fail(Arrays.asList(testPattern[i]).toString() + "," + hasWfDoc);
			} catch (EIMServerResponseError e) {
				switch (((Integer) testPattern[i][0]).intValue()) {
				case 0:
					fail(e.getMessage() + ":" + Arrays.asList(testPattern[i]).toString() + ","
							+ hasWfDoc);
				case 1:
					assertEquals("Http:1002", e.getMessage());
					break;
				case 2:
					assertEquals("Http:1015", e.getMessage());
					break;
				}
			}
		}
	}
}
