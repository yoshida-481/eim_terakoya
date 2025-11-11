package admin.object;

import junit.framework.TestCase;

import org.apache.xpath.XPathAPI;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

import admin.AdminService;
import eim.bo.EIMAttributeType;
import eim.bo.EIMObjectType;
import eim.bo.EIMValueType;
import eim.net.EIMSession;
import eim.util.AttributeUtils;
import eim.util.ObjectAttributeUtils;
import eim.util.ObjectUtils;
import eimtest.app.util.TestAppDBUtil;
import eimtest.app.util.XMLUtil;
import eimtest.util.TestSessionUtil;

/** */
public class DspAttributeTypeTest extends TestCase {
	/** */
	AdminService os;

	/** */
	XMLUtil xu;

	/** */
	EIMSession sess;

	/** */
	EIMObjectType docType;

	public void setUp() throws Exception
	{
		TestAppDBUtil.loadPrimitiveData();
		os = new AdminService(false);
		xu = new XMLUtil();
		sess = TestSessionUtil.createEIMSession();
		docType = ObjectUtils.getObjectTypeByName(sess, "ドキュメント");
	}

	protected void tearDown() throws Exception
	{
		sess.close();
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testDocumentAdmin() throws Exception
	{
		// ESSENTIAL_ATTRIBUTE_DEFNAMEから「作成者名」削除をテスト
		// @Test: jsp call without creator name attribute
		os.object_dspAttributeType(docType.getId());
		// no error

		EIMAttributeType attrTypeCreatorName = createAttrTypeCreatorName();

		Document xdoc = xu.toDOM(os.object_dspAttributeType(docType.getId()));
		Node xnode = XPathAPI.selectSingleNode(xdoc, "/attTypes/attType[@attTypeId='"
				+ attrTypeCreatorName.getId() + "']");
		assertEquals("作成者名", XPathAPI.selectSingleNode(xnode, "@attTypeName").getNodeValue());
		assertEquals("false", XPathAPI.selectSingleNode(xnode, "@attTypeEssential").getNodeValue());

	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testGeneralAdmin() throws Exception
	{
		// ESSENTIAL_ATTRIBUTE_DEFNAMEから「作成者名」削除をテスト
		os = new AdminService(true);// General mode
		// @Test: jsp call without creator name attribute
		os.object_dspAttributeType(docType.getId());
		// no error

		EIMAttributeType attrTypeCreatorName = createAttrTypeCreatorName();

		Document xdoc = xu.toDOM(os.object_dspAttributeType(docType.getId()));
		Node xnode = XPathAPI.selectSingleNode(xdoc, "/attTypes/attType[@attTypeId='"
				+ attrTypeCreatorName.getId() + "']");
		assertEquals("作成者名", XPathAPI.selectSingleNode(xnode, "@attTypeName").getNodeValue());
		assertEquals("false", XPathAPI.selectSingleNode(xnode, "@attTypeEssential").getNodeValue());
	}

	/**
	 * 
	 * @return o
	 * @throws Exception
	 */
	EIMAttributeType createAttrTypeCreatorName() throws Exception
	{
		EIMAttributeType attrTypeCreatorName = AttributeUtils.createAttributeType(sess, "作成者名",
				EIMValueType.getTypeById(sess, EIMValueType.STRING));
		AttributeUtils.addOtherAttributeTypeName(sess, attrTypeCreatorName.getId(), "JA", "作成者名");
		AttributeUtils.addOtherAttributeTypeName(sess, attrTypeCreatorName.getId(), "EN",
				"Creator name");
		ObjectAttributeUtils.applyAttributeType(sess, docType, attrTypeCreatorName);
		sess.commit();
		return attrTypeCreatorName;
	}
}
