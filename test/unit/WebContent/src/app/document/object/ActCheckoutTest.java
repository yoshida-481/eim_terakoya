package app.document.object;

import java.io.File;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.Arrays;

import junit.extensions.TestSetup;
import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.commons.lang.ClassUtils;
import org.apache.xpath.XPathAPI;

import eim.bo.EIMObject;
import eim.bo.EIMResource;
import eim.net.EIMSession;
import eimtest.app.tool.CreateMissingFile;
import eimtest.app.util.CreateFolderTreeData;
import eimtest.app.util.JSPTestCase;
import eimtest.app.util.TestAppDBUtil;
import eimtest.app.util.TestAppObjectUtil;
import eimtest.app.util.net.EIMServerResponseError;
import eimtest.util.TestSessionUtil;

/** */
public class ActCheckoutTest extends JSPTestCase {
	/**
	 * 
	 * @return o
	 * @throws Exception
	 */
	public static Test suite() throws Exception {
		TestSuite suite = new TestSuite(ActCheckoutTest.class);
		TestSetup w = new TestSetup(suite) {

			/** */
			public void setUp() throws Exception {
				if (true) {
					TestAppDBUtil.loadPrimitiveData();
					EIMSession sess = TestSessionUtil.createEIMSession();
					new CreateFolderTreeData(sess, null).process();
					sess.commit();
					new CreateMissingFile().start(null);
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
		{ Boolean.FALSE, "tu4", "編集中1.txt" }// 公開読み取り権限
				, { Boolean.FALSE, "tu4", "承認依頼中1.txt" }//
				, { Boolean.FALSE, "tu4", "公開処理中1.txt" }//
				, { Boolean.FALSE, "tu4", "公開中1.txt" }//
				, { Boolean.FALSE, "tu4", "ステータスなし1.txt" }//
				, { Boolean.FALSE, "tu3", "編集中1.txt" }// 常時読み取り権限
				, { Boolean.FALSE, "tu3", "承認依頼中1.txt" }//
				, { Boolean.FALSE, "tu3", "公開処理中1.txt" }//
				, { Boolean.FALSE, "tu3", "公開中1.txt" }//
				, { Boolean.FALSE, "tu3", "ステータスなし1.txt" }//
				, { Boolean.FALSE, "tu2", "編集中1.txt" }// ステータス変更権限
				, { Boolean.FALSE, "tu2", "承認依頼中1.txt" }//
				, { Boolean.FALSE, "tu2", "公開処理中1.txt" }//
				, { Boolean.FALSE, "tu2", "公開中1.txt" }//
				, { Boolean.FALSE, "tu2", "ステータスなし1.txt" }//
				, { Boolean.FALSE, "tu1", "編集中1.txt" }// 書き込み権限
				, { Boolean.FALSE, "tu1", "承認依頼中1.txt" }//
				, { Boolean.FALSE, "tu1", "公開処理中1.txt" }//
				, { Boolean.TRUE, "tu1", "公開中1.txt" }//
				, { Boolean.TRUE, "tu1", "ステータスなし1.txt" } //
				// ワークフロー付きフォルダの下
				, { Boolean.FALSE, "tu4", "F編集中1.txt" }// 公開読み取り権限
				, { Boolean.FALSE, "tu4", "F承認依頼中1.txt" }//
				, { Boolean.FALSE, "tu4", "F公開処理中1.txt" }//
				, { Boolean.FALSE, "tu4", "F公開中1.txt" }//
				, { Boolean.FALSE, "tu3", "F編集中1.txt" }// 常時読み取り権限
				, { Boolean.FALSE, "tu3", "F承認依頼中1.txt" }//
				, { Boolean.FALSE, "tu3", "F公開処理中1.txt" }//
				, { Boolean.FALSE, "tu3", "F公開中1.txt" }//
				, { Boolean.FALSE, "tu2", "F編集中1.txt" }// ステータス変更権限
				, { Boolean.FALSE, "tu2", "F承認依頼中1.txt" }//
				, { Boolean.FALSE, "tu2", "F公開処理中1.txt" }//
				, { Boolean.FALSE, "tu2", "F公開中1.txt" }//
				, { Boolean.FALSE, "tu1", "F編集中1.txt" }// 書き込み権限
				, { Boolean.FALSE, "tu1", "F承認依頼中1.txt" }//
				, { Boolean.FALSE, "tu1", "F公開処理中1.txt" }//
				, { Boolean.FALSE, "tu1", "F公開中1.txt" } //
		};
		doCheckout(testPattern);
	}

	/**
	 * 
	 * @param testPattern
	 * @throws Exception
	 */
	void doCheckout(Object[][] testPattern) throws Exception {
		for (int i = 0; i < testPattern.length; i++) {
			ds.switchUser((String) testPattern[i][1]);
			EIMObject o = TestAppObjectUtil.findObj((String) testPattern[i][2], objs);
			try {
				ds.object_actCheckout(o);
				if (!((Boolean) testPattern[i][0]).booleanValue())
					fail(Arrays.asList(testPattern[i]).toString());
			} catch (EIMServerResponseError e) {
				if (((Boolean) testPattern[i][0]).booleanValue())
					fail(e.getMessage() + ":" + Arrays.asList(testPattern[i]).toString());
				assertTrue(e.getMessage() + ":" + Arrays.asList(testPattern[i]).toString(),
					EIMResource.getMessage("JA", "EIM.ERROR.LOGIC.NOCHECKOUTROLE").equals(
						e.getMessage()) //
							|| EIMResource.getMessage("JA", "EIM.ERROR.LOGIC.NOTPUBLIC",
								new Object[] { o.getName() }).equals(e.getMessage()));
			}
		}
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testResultXML() throws Exception {
		ds.switchUser("tu1");
		EIMObject o = TestAppObjectUtil.findObj("公開中2.txt", objs);
		assertEquals(String.valueOf(o.getId()), XPathAPI.selectSingleNode(
			xu.toDOM(ds.object_actCheckout(o)), "/objectList/object/@objId").getNodeValue());

	}

}
