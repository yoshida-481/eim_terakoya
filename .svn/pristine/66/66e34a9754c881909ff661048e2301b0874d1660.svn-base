package app.document.object;

import java.util.Arrays;

import junit.extensions.TestSetup;
import junit.framework.Test;
import junit.framework.TestSuite;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMResource;
import eim.net.EIMSession;
import eim.util.ObjectUtils;
import eimtest.app.util.CreateFolderTreeData;
import eimtest.app.util.JSPTestCase;
import eimtest.app.util.TestAppDBUtil;
import eimtest.app.util.TestAppObjectUtil;
import eimtest.app.util.net.EIMServerResponseError;
import eimtest.util.TestSessionUtil;

/** */
public class DspCreateOneDocumentTest extends JSPTestCase {
	/**
	 * 
	 * @return o
	 * @throws Exception
	 */
	public static Test suite() throws Exception {
		TestSuite suite = new TestSuite(DspCreateOneDocumentTest.class);
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
			assertEquals(EIMResource.getMessage("JA",
			"EIM.ERROR.LOGIC.CANTSET.UNDERWF.WFDOCUMENT"), e.getMessage());
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
		for (int i = 0; i < testPattern.length; i++) {
			ds.switchUser((String) testPattern[i][1]);
			EIMObject o = TestAppObjectUtil.findObj((String) testPattern[i][2], objs);
			if (o == null)
				throw new RuntimeException(testPattern[i][2] + " is not found");
			try {
				ds.object_dspCreateOneDocument(o.getId(), objType.getId());
				if (((Integer) testPattern[i][0]).intValue() != 0)
					fail(Arrays.asList(testPattern[i]).toString());
			} catch (EIMServerResponseError e) {
				switch (((Integer) testPattern[i][0]).intValue()) {
				case 0:
					fail(e.getMessage() + ":" + Arrays.asList(testPattern[i]).toString());
				case 1:
					assertEquals(EIMResource.getMessage("JA", "EIM.ERROR.LOGIC.NOCREATEROLE"),
						e.getMessage());
					break;
				case 2:
					assertEquals(EIMResource.getMessage("JA",
						"EIM.ERROR.LOGIC.CANTSET.UNDERWF.WFDOCUMENT"), e.getMessage());
				}
			}
		}
	}
}
