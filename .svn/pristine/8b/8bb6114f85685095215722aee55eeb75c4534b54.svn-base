package admin.attribute;

import junit.framework.TestCase;

import org.apache.xpath.XPathAPI;
import org.w3c.dom.Document;

import admin.AdminService;
import eim.bo.EIMAttributeType;
import eim.bo.EIMValueType;
import eim.net.EIMSession;
import eim.util.AttributeUtils;
import eimtest.app.util.TestAppDBUtil;
import eimtest.app.util.XMLUtil;
import eimtest.util.TestSessionUtil;

/** */
public class DspAttributeTypeListTest extends TestCase {
	/** */
	AdminService as;

	/** */
	XMLUtil xu;

	/** */
	EIMSession sess;

	public void setUp() throws Exception
	{
		as = new AdminService(false);
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
	public void testDocumentAdmin() throws Exception
	{
		// ESSENTIAL_ATTRIBUTE_DEFNAMEから「作成者名」削除をテスト
		// @Test: jsp call without creator name attribute
		as.attribute_dspAttributeTypeList();
		// no error

		EIMAttributeType attrTypeCreatorName = createAttrTypeCreatorName();

		Document xdoc = xu.toDOM(as.attribute_dspAttributeTypeList());
		assertEquals("作成者名", XPathAPI.selectSingleNode(xdoc,
				"/attTypes/attType[@attTypeId='" + attrTypeCreatorName.getId() + "']/@attTypeName")
				.getNodeValue());
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testGeneralAdmin() throws Exception
	{
		// ESSENTIAL_ATTRIBUTE_DEFNAMEから「作成者名」削除をテスト
		as = new AdminService(true);// General mode
		// @Test: jsp call without creator name attribute
		as.attribute_dspAttributeTypeList();
		// no error

		EIMAttributeType attrTypeCreatorName = createAttrTypeCreatorName();

		Document xdoc = xu.toDOM(as.attribute_dspAttributeTypeList());
		assertEquals("作成者名", XPathAPI.selectSingleNode(xdoc,
				"/attTypes/attType[@attTypeId='" + attrTypeCreatorName.getId() + "']/@attTypeName")
				.getNodeValue());
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
		sess.commit();
		return attrTypeCreatorName;
	}
}
