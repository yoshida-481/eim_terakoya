package app.document.object;

import java.io.File;
import java.util.Arrays;

import junit.extensions.TestSetup;
import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.xpath.XPathAPI;

import app.document.DocumentService;
import eim.bo.EIMObject;
import eim.net.EIMSession;
import eim.util.ObjectUtils;
import eimtest.app.tool.CreateMissingFile;
import eimtest.app.util.CreateFolderTreeData;
import eimtest.app.util.JSPTestCase;
import eimtest.app.util.TestAppDBUtil;
import eimtest.app.util.TestAppMisc;
import eimtest.app.util.TestAppObjectUtil;
import eimtest.app.util.XMLUtil;
import eimtest.app.util.net.EIMServerResponseError;
import eimtest.util.TestSessionUtil;

/** */
public class ActCheckinTest extends JSPTestCase {
	/**
	 * 
	 * @return o
	 * @throws Exception
	 */
	public static Test suite() throws Exception {
		TestSuite suite = new TestSuite(ActCheckinTest.class);
		TestSetup w = new TestSetup(suite) {

			/** */
			public void setUp() throws Exception {
				if (true) {
					TestAppDBUtil.loadPrimitiveData();
					EIMSession sess = TestSessionUtil.createEIMSession();
					new CreateFolderTreeData(sess, null).process();
					sess.commit();
					new CreateMissingFile().start(null);
					DocumentService ds = new DocumentService("tu1", null);
					XMLUtil xu = new XMLUtil();
					//公開中2.txtをチェックアウト状態に
					ObjectUtils.rename(sess, ObjectUtils.getObjectById(sess,
						Integer.parseInt(XPathAPI.selectSingleNode(
							xu.toDOM(ds.object_actCheckout(TestAppObjectUtil.getObj(sess,
								"公開中2.txt"))), "/objectList/object/@objId").getNodeValue())),
						"公開中2r0.txt");
					//ステータスなし2.txtをチェックアウト状態に
					ObjectUtils.rename(sess, ObjectUtils.getObjectById(sess,
						Integer.parseInt(XPathAPI.selectSingleNode(
							xu.toDOM(ds.object_actCheckout(TestAppObjectUtil.getObj(sess,
								"ステータスなし2.txt"))), "/objectList/object/@objId").getNodeValue())),
						"ステータスなし2r0.txt");
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
		Object[][] testPattern = {//
		{ new Integer(1), "tu4", "編集中1.txt" }// 公開読み取り権限
				, { new Integer(1), "tu4", "承認依頼中1.txt" }//
				, { new Integer(1), "tu4", "公開処理中1.txt" }//
				, { new Integer(1), "tu4", "公開中1.txt" }//
				, { new Integer(1), "tu4", "ステータスなし1.txt" }//
				, { new Integer(1), "tu3", "編集中1.txt" }// 常時読み取り権限
				, { new Integer(1), "tu3", "承認依頼中1.txt" }//
				, { new Integer(1), "tu3", "公開処理中1.txt" }//
				, { new Integer(1), "tu3", "公開中1.txt" }//
				, { new Integer(1), "tu3", "ステータスなし1.txt" }//
				, { new Integer(1), "tu2", "編集中1.txt" }// ステータス変更権限
				, { new Integer(1), "tu2", "承認依頼中1.txt" }//
				, { new Integer(1), "tu2", "公開処理中1.txt" }//
				, { new Integer(1), "tu2", "公開中1.txt" }//
				, { new Integer(1), "tu2", "ステータスなし1.txt" }//
				, { new Integer(0), "tu1", "編集中1.txt" }// 書き込み権限
				, { new Integer(2), "tu1", "承認依頼中1.txt" }//
				, { new Integer(2), "tu1", "公開処理中1.txt" }//
				, { new Integer(2), "tu1", "公開中1.txt" }//
				, { new Integer(0), "tu1", "ステータスなし1.txt" } //
				, { new Integer(0), "tu1", "公開中2.txt" } //
				, { new Integer(2), "tu1", "公開中2r0.txt" } //
				, { new Integer(0), "tu1", "ステータスなし2.txt" } //
				/* 標準品バグ */, { new Integer(0), "tu1", "ステータスなし2r0.txt" } //
				// ワークフロー付きフォルダの下
				, { new Integer(1), "tu4", "F編集中1.txt" }// 公開読み取り権限
				, { new Integer(1), "tu4", "F承認依頼中1.txt" }//
				, { new Integer(1), "tu4", "F公開処理中1.txt" }//
				, { new Integer(1), "tu4", "F公開中1.txt" }//
				, { new Integer(1), "tu3", "F編集中1.txt" }// 常時読み取り権限
				, { new Integer(1), "tu3", "F承認依頼中1.txt" }//
				, { new Integer(1), "tu3", "F公開処理中1.txt" }//
				, { new Integer(1), "tu3", "F公開中1.txt" }//
				, { new Integer(1), "tu2", "F編集中1.txt" }// ステータス変更権限
				, { new Integer(1), "tu2", "F承認依頼中1.txt" }//
				, { new Integer(1), "tu2", "F公開処理中1.txt" }//
				, { new Integer(1), "tu2", "F公開中1.txt" }//
				, { new Integer(0), "tu1", "F編集中1.txt" }// 書き込み権限
				, { new Integer(2), "tu1", "F承認依頼中1.txt" }//
				, { new Integer(2), "tu1", "F公開処理中1.txt" }//
				, { new Integer(2), "tu1", "F公開中1.txt" } //
		};
		doCheckin(testPattern);
	}

	/**
	 * 
	 * @param testPattern
	 * @throws Exception
	 */
	void doCheckin(Object[][] testPattern) throws Exception {
		File f = new File(TestAppMisc.getAppFilePath(this.getClass(), ".class"));
		for (int i = 0; i < testPattern.length; i++) {
			ds.switchUser((String) testPattern[i][1]);
			EIMObject o = TestAppObjectUtil.findObj((String) testPattern[i][2], objs);
			if (o == null)
				throw new RuntimeException(testPattern[i][2] + " is not found");
			try {
				System.out.println("ckin..[" + i + "]" + Arrays.asList(testPattern[i]));
				ds.object_actCheckin(o.getId(), "cki_" + o.getName(), f, "");
				if (((Integer) testPattern[i][0]).intValue() != 0)
					fail(Arrays.asList(testPattern[i]).toString());
			} catch (EIMServerResponseError e) {
				switch (((Integer) testPattern[i][0]).intValue()) {
				case 0:
					fail(e.getMessage() + ":" + Arrays.asList(testPattern[i]).toString());
				case 1:
					assertEquals(Arrays.asList(testPattern[i]).toString(), "Http:1002",
						e.getMessage());
					break;
				case 2:
					assertEquals(Arrays.asList(testPattern[i]).toString(), "Http:1004",
						e.getMessage());
					break;
				}
			} catch (Exception e) {
				throw new RuntimeException(Arrays.asList(testPattern[i]).toString(), e);
			}
		}
	}
}
