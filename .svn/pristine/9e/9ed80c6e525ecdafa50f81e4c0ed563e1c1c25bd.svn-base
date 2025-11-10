package app.document.senario;

import java.io.File;

import junit.framework.TestCase;

import org.apache.commons.lang.ClassUtils;
import org.apache.xpath.XPathAPI;
import org.w3c.dom.Document;

import app.document.DocumentService;
import eim.bo.EIMAttributeType;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMSearchSelectEIMObject;
import eim.bo.EIMUser;
import eim.bo.EIMValueType;
import eim.bo.EIMSearchSelectEIMObject.PsedoAttributeTypeEnum;
import eim.bo.EIMSearchSelectEIMObject.SearchConditionBuildHelper;
import eim.net.EIMSession;
import eim.util.AttributeUtils;
import eim.util.ObjectAttributeUtils;
import eim.util.ObjectUtils;
import eim.util.SearchUtils;
import eim.util.UserUtils;
import eimtest.app.util.TestAppDBUtil;
import eimtest.app.util.XMLUtil;
import eimtest.util.TestSessionUtil;

/** */
public class CreeatorNameTest extends TestCase {
	/** */
	DocumentService ds;

	/** */
	XMLUtil xu;

	/** */
	EIMSession sess;

	public void setUp() throws Exception
	{
		ds = new DocumentService();
		xu = new XMLUtil();
		sess = TestSessionUtil.createEIMSession();
		TestAppDBUtil.loadPrimitiveData();
	}

	protected void tearDown() throws Exception
	{
		sess.close();
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testCreateDocumentAndMore() throws Exception
	{
		// @Test:create document result
		// login is 'system'
		File file = new File(this.getClass().getResource(
				ClassUtils.getShortClassName(this.getClass()) + ".class").getPath());
		EIMUser u1 = UserUtils.getUserByCode(sess, "u1");
		EIMObjectType docType = ObjectUtils.getObjectTypeByName(sess, "ドキュメント");
		ds.object_actCreateDocument("F1", docType, "pp"//
				, u1.getId()//
				, "", file);
		EIMSearchSelectEIMObject s = new EIMSearchSelectEIMObject();
		SearchConditionBuildHelper h = new SearchConditionBuildHelper();
		s.setCondition(h.group(h.opAnd())//
				.addCondition(
						h.eq(h.opAnd(), AttributeUtils.getAttributeTypeByName(sess, "パス"),
								"/WS1/F1/"))//
				.addCondition(h.eq(h.opAnd(), PsedoAttributeTypeEnum.NAME, file.getName())//
				));
		EIMObject docObj = (EIMObject) SearchUtils.searchObjects(sess, s, null).get(0);
		assertEquals("system", docObj.getCreateUser().getCode());
		assertEquals(u1.getId(), docObj.getAttribute("作成者").getInt());
		assertNull(docObj.getAttribute("作成者名"));

		// @Test:dsp property result.creator name is 'u1'
		Document xdoc = xu.toDOM(ds.object_dspProperty(docObj.getId()));
		assertEquals("ja:u1", XPathAPI.selectNodeIterator(xdoc, "/object/@createUserName")
				.nextNode().getNodeValue());

		// @Test:dspAttribute result is normal end without creator-name
		// attribute
		xdoc = xu.toDOM(ds.object_dspAttribute(docObj.getId()));

		// @Test:approve/dspProperty result.creator name is 'u1'
		xdoc = xu.toDOM(ds.approve_dspProperty(docObj.getId()));
		assertEquals("ja:u1", XPathAPI.selectNodeIterator(xdoc, "/object/@createUserName")
				.nextNode().getNodeValue());

		// create '作成者名' attribute
		EIMAttributeType attrTypeCreatorName = AttributeUtils.createAttributeType(sess, "作成者名",
				EIMValueType.getTypeById(sess, EIMValueType.STRING));
		ObjectAttributeUtils.applyAttributeType(sess, docType, attrTypeCreatorName);

		// @Test:dspAttribute result
		ObjectAttributeUtils.setAttribute(sess, docObj, attrTypeCreatorName, "hogehoge");
		sess.commit();
		xdoc = xu.toDOM(ds.object_dspAttribute(docObj.getId()));
		assertEquals("hogehoge", XPathAPI.selectNodeIterator(xdoc,
				"/attList/attribute[@attTypeId='" + attrTypeCreatorName.getId() + "']/@attValue")
				.nextNode().getNodeValue());

		// @Test:approve/dspProperty result
		xdoc = xu.toDOM(ds.approve_dspProperty(docObj.getId()));
		assertEquals("ja:u1", XPathAPI.selectNodeIterator(xdoc, "/object/@createUserName")
				.nextNode().getNodeValue());

	}
}
