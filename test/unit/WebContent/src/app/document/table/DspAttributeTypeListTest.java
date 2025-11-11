package app.document.table;

import junit.framework.TestCase;
import app.document.DocumentService;
import eim.bo.EIMObjectType;
import eim.net.EIMSession;
import eim.util.AttributeUtils;
import eim.util.ObjectAttributeUtils;
import eim.util.ObjectUtils;
import eimtest.app.util.TestAppDBUtil;
import eimtest.app.util.XMLUtil;
import eimtest.util.TestSessionUtil;

/** */
public class DspAttributeTypeListTest extends TestCase {
	/** */
	DocumentService ds;

	/** */
	XMLUtil xu;

	/** */
	EIMSession sess;

	/** */
	EIMObjectType docSuper;

	public void setUp() throws Exception
	{
		TestAppDBUtil.loadPrimitiveData();
		ds = new DocumentService();
		xu = new XMLUtil();
		sess = TestSessionUtil.createEIMSession();
		docSuper = ObjectUtils.getObjectTypeByName(sess, "ドキュメント");
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testMe() throws Exception
	{
		xu.assertNodeValues(//
				new String[] { "63", "43", "44", "66", "65", "62", "46", "67", "64", "45", "61", "60", "47" }//
				, xu.toDOM(ds.table_dspAttributeTypeList(""))//
				, "/attTypes/attType/@attTypeId");

		// @Test: include child document type's attribute
		EIMObjectType doc_1 = ObjectUtils.createObjectType(sess, "DOC1", docSuper);
		ObjectAttributeUtils.applyAttributeType(sess, doc_1, AttributeUtils.getAttributeTypeById(
				sess, 36));// 「再承認依頼先種別：ID」
		sess.commit();
		xu.assertNodeValues(//
				new String[] { "63", "43", "44", "36", "66", "65", "62", "46", "67", "64", "45", "61", "60", "47" }//
				, xu.toDOM(ds.table_dspAttributeTypeList(""))//
				, "/attTypes/attType/@attTypeId");

		// @Test: include child-child document type's attribute
		EIMObjectType doc_1_1 = ObjectUtils.createObjectType(sess, "DOC1_1", doc_1);
		ObjectAttributeUtils.applyAttributeType(sess, doc_1_1, AttributeUtils.getAttributeTypeById(
				sess, 27));// 「コメント」
		sess.commit();
		xu.assertNodeValues(//
				new String[] { "63", "43", "44", "36", "66", "65", "62", "46", "67", "64", "27", "45", "61", "60", "47" }//
				, xu.toDOM(ds.table_dspAttributeTypeList(""))//
				, "/attTypes/attType/@attTypeId");

		// @Test: include another child tree document type's attribute
		EIMObjectType doc_2 = ObjectUtils.createObjectType(sess, "DOC2", docSuper);
		ObjectAttributeUtils.applyAttributeType(sess, doc_2, AttributeUtils.getAttributeTypeById(
				sess, 26));// 「通知タイミング」
		sess.commit();
		xu.assertNodeValues(//
				new String[] { "63", "43", "44", "36", "66", "65", "62", "46", "67", "64", "26", "27", "45", "61", "60", "47" }//
				, xu.toDOM(ds.table_dspAttributeTypeList(""))//
				, "/attTypes/attType/@attTypeId");

		// @Test: attribute search condition
		xu.assertNodeValues(//
				new String[] { "45" }//
				, xu.toDOM(ds.table_dspAttributeTypeList("*訂*"))//
				, "/attTypes/attType/@attTypeId");
	}
}
