package servlet.dl;

import java.util.List;

import junit.extensions.TestSetup;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import app.document.DocumentService;

import common.util.AppObjectUtil;

import eim.bo.EIMObject;
import eim.net.EIMSession;
import eim.util.ObjectUtils;
import eimtest.app.tool.CreateMissingFile;
import eimtest.app.util.CreateFolderTreeData;
import eimtest.app.util.TestAppDBUtil;
import eimtest.app.util.TestAppObjectUtil;
import eimtest.app.util.XMLUtil;
import eimtest.util.TestSessionUtil;

/** */
public class DownloadPublicDocumentTest extends TestCase {
	/**
	 * 
	 * @return o
	 * @throws Exception
	 */
	public static Test suite() throws Exception {
		TestSuite suite = new TestSuite(DownloadPublicDocumentTest.class);
		TestSetup w = new TestSetup(suite) {

			/** */
			public void setUp() throws Exception {
				if (false) {
					TestAppDBUtil.loadPrimitiveData();
					EIMSession sess = TestSessionUtil.createEIMSession();
					EIMObject obj = TestAppObjectUtil.getObj(sess, "F1");
					ObjectUtils.rename(sess, obj, "WS1F1");
					new CreateFolderTreeData(sess, null).process();
					obj = TestAppObjectUtil.getObj(sess, "%-WFステータステスト");
					ObjectUtils.rename(sess, obj, "A-WFステータステスト");
					sess.commit();
					new CreateMissingFile().start(null);// create physical file
				}
			}
		};
		return w;
	}

	/** */
	DocumentService ds;

	/** */
	XMLUtil xu;

	/** */
	EIMSession sess;

	/** */
	List objs;

	/** */
	public void setUp() throws Exception {
		sess = TestSessionUtil.createEIMSession();// データ直接操作用のEIMセッション
		ds = new DocumentService();
		xu = new XMLUtil();
		objs = TestAppObjectUtil.getFolderAndDocObjs(sess);
	}

	protected void tearDown() throws Exception {
		sess.close();
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testMe() throws Exception {
		EIMObject obj = TestAppObjectUtil.getObj(sess, "F公開中3.txt");
		String assertStr = obj.getId() + "," + AppObjectUtil.getStrAttr(sess, obj, "パス")
				+ obj.getName().replaceAll("\\..*", "") + ".pdf";
		// 書き込み権
		ds.switchUser("tu1");
		assertEquals(assertStr, ds.servlet_dl_DownloadPublicDocument(obj.getId()));

		// ステータス変更権
		ds.switchUser("tu2");
		assertEquals(assertStr, ds.servlet_dl_DownloadPublicDocument(obj.getId()));

		// 常時読み取り権
		ds.switchUser("tu3");
		assertEquals(assertStr, ds.servlet_dl_DownloadPublicDocument(obj.getId()));

		// 公開読み取り権
		ds.switchUser("tu4");
		assertEquals(assertStr, ds.servlet_dl_DownloadPublicDocument(obj.getId()));
	}
}
