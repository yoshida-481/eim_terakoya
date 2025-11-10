package app.document.object;

import java.io.File;

import org.apache.commons.lang.ClassUtils;
import org.apache.xpath.XPathAPI;
import org.w3c.dom.Document;

import eim.bo.EIMObject;
import eimtest.app.util.JSPTestCase;
import eimtest.app.util.TestAppDBUtil;
import eimtest.app.util.TestAppObjectUtil;

/** */
public class DspPropertyTest extends JSPTestCase {
	public void setUp() throws Exception {
		TestAppDBUtil.loadPrimitiveData();
		super.setUp();
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testCreateDocumentAndMore() throws Exception {
		// create document
		// jsp login is 'system'
		File file = new File(this.getClass().getResource(
			ClassUtils.getShortClassName(this.getClass()) + ".class").getPath());
		EIMObject docObj = TestAppObjectUtil.createObject(u1, file);

		// @Test:日本語でログインし、セキュリティ名が日本語
		Document xdoc = xu.toDOM(ds.object_dspProperty(docObj.getId()));
		assertEquals("sec", XPathAPI.selectSingleNode(xdoc, "/object/@securityName").getNodeValue());

		// @Test:英語語でログインし、セキュリティ名が英語
		ds.switchUser("u1", null, "EN", null);
		xdoc = xu.toDOM(ds.object_dspProperty(docObj.getId()));
		assertEquals("en:sec",
			XPathAPI.selectSingleNode(xdoc, "/object/@securityName").getNodeValue());
	}
}
