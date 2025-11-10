package app.document.folder;

import java.util.Arrays;
import java.util.List;

import junit.extensions.TestSetup;
import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.commons.lang.ArrayUtils;
import org.w3c.dom.Document;

import app.document.search.ActSearchTestUtil;
import eim.bo.EIMObject;
import eim.net.EIMSession;
import eim.util.AttributeUtils;
import eim.util.ObjectUtils;
import eimtest.app.util.CreateFolderTreeData;
import eimtest.app.util.JSPTestCase;
import eimtest.app.util.TestAppDBUtil;
import eimtest.app.util.TestAppMisc;
import eimtest.app.util.TestAppObjectUtil;
import eimtest.util.TestSessionUtil;

/** */
public class DspChildObject2_returnValueTest extends JSPTestCase {
	/**
	 * 
	 * @return o
	 * @throws Exception
	 */
	public static Test suite() throws Exception {
		TestSuite suite = new TestSuite(DspChildObject2_returnValueTest.class);
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
	public void testAttrTreeParamReturn() throws Exception {
		// TestCase:プロジェクトXフォルダをリスト
		Document xdoc = xu.toDOM(ds.folder_dspChildObject("プロジェクトX", 1234//
			, new String[] { "2", "1025449200000", "a" }, "abc", "d/e/f"));
		assertObjlistValues(xdoc, new String[][] {//
			{ "1234", "attrTreeId" }//
					, { "2", "attrTreeValue1" }//
					, { "1025449200000", "attrTreeValue2" }//
					, { "a", "attrTreeValue3" }//
					, { "d/e/f", "attrTreePath" }//
					, { "フォルダ", "objTypeName" } //
			});
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testWorkflowFolder() throws Exception {
		// TestCase:プロジェクトXフォルダをリスト
		Document xdoc = xu.toDOM(ds.folder_dspChildObject("プロジェクトX", 1234//
			, new String[] { "2", "1025449200000", "a" }, "abc", "d/e/f"));

		EIMObject f = TestAppObjectUtil.getObj(sess, "F編集中");
		EIMObject fNoWf = TestAppObjectUtil.getObj(sess, "F普通のフォルダ");
		EIMObject dunderWf = TestAppObjectUtil.getObj(sess, "F編集中1.txt");
		// ワークフロー付きフォルダかどうかのフラグ,上位WFフォルダ
		assertObjValues(xdoc, f.getId(), new String[][] { { "true", "isWorkflowFolder" },
				{ "", "higherWFFolder" } });
		assertObjValues(xdoc, fNoWf.getId(), new String[][] { { "false", "isWorkflowFolder" } });

		xdoc = xu.toDOM(ds.folder_dspChildObject("F編集中", 1234//
			, new String[] { "2", "1025449200000", "a" }, "abc", "d/e/f"));
		assertObjValues(xdoc, dunderWf.getId(), new String[][] { { "false", "isWorkflowFolder" },
				{ String.valueOf(f.getId()), "higherWFFolder" } });
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testMultivalue() throws Exception {
		// TestCase:プロジェクトXフォルダをリスト
		Document xdoc = xu.toDOM(ds.folder_dspChildObject("プロジェクトX", 1234//
			, new String[] { "2", "1025449200000", "a" }, "abc", "d/e/f"));

		EIMObject f = TestAppObjectUtil.getObj(sess, "F編集中");
		// 数値
		assertEquals("[1, 2]", TestAppMisc.getNodeValues(
			xdoc,
			"//objList/object[@objId='" + f.getId() + "']/attType_"
					+ AttributeUtils.getAttributeTypeByName(sess, "t数値N").getId()
					+ "_multivalue/attValue/@value").toString());

		// 日付
		assertEquals("[2000-01-01, 2001-02-02]", TestAppMisc.getNodeValues(
			xdoc,
			"//objList/object[@objId='" + f.getId() + "']/attType_"
					+ AttributeUtils.getAttributeTypeByName(sess, "t日付N").getId()
					+ "_multivalue/attValue/@value").toString());
		// 日付のlong値
		assertEquals("[946652400, 981039600]", TestAppMisc.getNodeValues(
			xdoc,
			"//objList/object[@objId='" + f.getId() + "']/attType_"
					+ AttributeUtils.getAttributeTypeByName(sess, "t日付N").getId()
					+ "_multivalue/attValue/@value_datetime").toString());
		// 文字
		assertEquals("[a, b]", TestAppMisc.getNodeValues(
			xdoc,
			"//objList/object[@objId='" + f.getId() + "']/attType_"
					+ AttributeUtils.getAttributeTypeByName(sess, "t文字N").getId()
					+ "_multivalue/attValue/@value").toString());
		// テキスト(改行が再現されないのはXmlUtilの制限。無視)
		assertEquals("[c C, d D]", TestAppMisc.getNodeValues(
			xdoc,
			"//objList/object[@objId='" + f.getId() + "']/attType_"
					+ AttributeUtils.getAttributeTypeByName(sess, "tテキストN").getId()
					+ "_multivalue/attValue/@value").toString());
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testAccessRole() throws Exception {
		// TestCase:書込権限
		ds.switchUser("tu1", null);
		long[] assertObjIds = { //
		TestAppObjectUtil.getObj(sess, "F公開中").getId(),
				TestAppObjectUtil.getObj(sess, "F公開処理中").getId(),
				TestAppObjectUtil.getObj(sess, "F公開処理中で失敗").getId(),
				TestAppObjectUtil.getObj(sess, "F承認依頼中").getId(),
				TestAppObjectUtil.getObj(sess, "F普通のフォルダ").getId(),
				TestAppObjectUtil.getObj(sess, "F編集中").getId(),
				TestAppObjectUtil.getObj(sess, "ステータスなし1.txt").getId(),
				TestAppObjectUtil.getObj(sess, "ステータスなし2.txt").getId(),
				TestAppObjectUtil.getObj(sess, "公開中1.txt").getId(),
				TestAppObjectUtil.getObj(sess, "公開中2.txt").getId(),
				TestAppObjectUtil.getObj(sess, "公開処理中1.txt").getId(),
				TestAppObjectUtil.getObj(sess, "公開処理中2.txt").getId(),
				TestAppObjectUtil.getObj(sess, "公開処理中で失敗1.txt").getId(),
				TestAppObjectUtil.getObj(sess, "公開処理中で失敗2.txt").getId(),
				TestAppObjectUtil.getObj(sess, "承認依頼中1.txt").getId(),
				TestAppObjectUtil.getObj(sess, "承認依頼中2.txt").getId(),
				TestAppObjectUtil.getObj(sess, "編集中1.txt").getId(),
				TestAppObjectUtil.getObj(sess, "編集中2.txt").getId() };
		Document xdoc = xu.toDOM(ds.folder_dspChildObject("プロジェクトX", 1234//
			, new String[] { "2", "1025449200000", "a" }, "abc", "d/e/f"));
		List objIds = ActSearchTestUtil.getObjIdsFromResult(xdoc);
		assertEquals(Arrays.asList(ArrayUtils.toObject(assertObjIds)).toString(), objIds.toString());

		// TestCase:ステータス変更権限
		ds.switchUser("tu2", null);
		xdoc = xu.toDOM(ds.folder_dspChildObject("プロジェクトX", 1234//
			, new String[] { "2", "1025449200000", "a" }, "abc", "d/e/f"));
		objIds = ActSearchTestUtil.getObjIdsFromResult(xdoc);
		assertEquals(Arrays.asList(ArrayUtils.toObject(assertObjIds)).toString(), objIds.toString());

		// TestCase:常時読取権限
		ds.switchUser("tu3", null);
		xdoc = xu.toDOM(ds.folder_dspChildObject("プロジェクトX", 1234//
			, new String[] { "2", "1025449200000", "a" }, "abc", "d/e/f"));
		objIds = ActSearchTestUtil.getObjIdsFromResult(xdoc);
		assertEquals(Arrays.asList(ArrayUtils.toObject(assertObjIds)).toString(), objIds.toString());

		// TestCase:公開読取権限
		ds.switchUser("tu4", null);
		assertObjIds = new long[] { //
		TestAppObjectUtil.getObj(sess, "F公開中").getId(),
				TestAppObjectUtil.getObj(sess, "F普通のフォルダ").getId(),
				TestAppObjectUtil.getObj(sess, "ステータスなし1.txt").getId(),
				TestAppObjectUtil.getObj(sess, "ステータスなし2.txt").getId(),
				TestAppObjectUtil.getObj(sess, "公開中1.txt").getId(),
				TestAppObjectUtil.getObj(sess, "公開中2.txt").getId() };
		xdoc = xu.toDOM(ds.folder_dspChildObject("プロジェクトX", 1234//
			, new String[] { "2", "1025449200000", "a" }, "abc", "d/e/f"));
		objIds = ActSearchTestUtil.getObjIdsFromResult(xdoc);
		assertEquals(Arrays.asList(ArrayUtils.toObject(assertObjIds)).toString(), objIds.toString());
	}

	/**
	 * 
	 * @param xdoc
	 * @param objId
	 * @param values
	 * @throws Exception
	 */
	void assertObjValues(Document xdoc, int objId, String[][] values) throws Exception {
		for (int i = 0; i < values.length; i++)
			assertEquals("[" + values[i][0] + "]", TestAppMisc.getNodeValues(xdoc,
				"//objList/object[@objId='" + objId + "']/@" + values[i][1]).toString());
	}

	/**
	 * 
	 * @param xdoc
	 * @param objId
	 * @param values
	 * @throws Exception
	 */
	void assertObjlistValues(Document xdoc, String[][] values) throws Exception {
		for (int i = 0; i < values.length; i++)
			assertEquals("[" + values[i][0] + "]", TestAppMisc.getNodeValues(xdoc,
				"//objList/@" + values[i][1]).toString());
	}
}
