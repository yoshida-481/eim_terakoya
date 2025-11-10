package app.document.search;

import java.util.Arrays;
import java.util.List;
import java.util.TimeZone;

import junit.framework.TestCase;

import org.apache.commons.lang.ArrayUtils;
import org.apache.xpath.XPathAPI;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.traversal.NodeIterator;

import eim.bo.EIMAttributeType;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMSecurity;
import eim.bo.EIMWorkFlow;
import eim.net.EIMSession;
import eim.util.AttributeUtils;
import eim.util.EIMConfig;
import eim.util.ObjectAttributeUtils;
import eim.util.ObjectUtils;
import eim.util.SecurityUtils;
import eim.util.UserUtils;
import eim.util.WorkFlowUtils;
import eim.util.internal.search.ValueTypeEnum;
import eimtest.app.util.XMLUtil;
import eimtest.app.util.net.EIMServerResponseError;
import eimtest.app.util.net.HttpClientEIM;
import eimtest.util.TestDateTimeUtil;
import eimtest.util.TestSessionUtil;

/** */
public class ActSearchTest extends TestCase {
	/** */
	HttpClientEIM br;

	/** */
	XMLUtil xu;

	/** */
	public void setUp() throws Exception
	{
		br = new HttpClientEIM("app/document");
		xu = new XMLUtil();
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testUserAttribute() throws Exception
	{
		ActSearchTestUtil.loadInitData(false);
		EIMSession sess = TestSessionUtil.createEIMSession();
		EIMObjectType objType = ObjectUtils.getObjectTypeByName(sess, EIMConfig
				.get("OBJECT_TYPE_NAME_DOCUMENT"));
		EIMAttributeType attrTypeInt = AttributeUtils.createAttributeType(sess, "attrInt",
				ValueTypeEnum.INTEGER);
		EIMAttributeType attrTypeDate = AttributeUtils.createAttributeType(sess, "attrDate",
				ValueTypeEnum.DATE);
		EIMAttributeType attrTypeStr = AttributeUtils.createAttributeType(sess, "attrStr",
				ValueTypeEnum.STRING);
		EIMAttributeType attrTypeText = AttributeUtils.createAttributeType(sess, "attrText",
				ValueTypeEnum.TEXT);
		ObjectAttributeUtils.applyAttributeType(sess, objType, attrTypeInt);
		ObjectAttributeUtils.applyAttributeType(sess, objType, attrTypeDate);
		ObjectAttributeUtils.applyAttributeType(sess, objType, attrTypeStr);
		ObjectAttributeUtils.applyAttributeType(sess, objType, attrTypeText);
		EIMObject obj = ObjectUtils.createObject(sess, objType, "withUserAttr");
		ObjectAttributeUtils.setAttribute(sess, obj, attrTypeInt, 12345);
		ObjectAttributeUtils.setAttribute(sess, obj, attrTypeDate, TestDateTimeUtil.DATE_FORMATTER
				.parse("2000-01-01"));
		ObjectAttributeUtils.setAttribute(sess, obj, attrTypeStr, "hogeStr");
		ObjectAttributeUtils.setAttribute(sess, obj, attrTypeText, "fooText");
		EIMSecurity sec = SecurityUtils.getSecurityByName(sess, "system");
		SecurityUtils.setSecurity(sess, obj, sec);
		sess.commit();

		br.eimLogin("system", "manager", "JA", null);
		Document doc = xu.toDOM(ActSearchTestUtil.callActSearch(br, "", "", "", "", "", "", "", "",
				"", "", "", "", ""//
				, new String[][] { { "attType_" + attrTypeInt.getId(), "12345" }//
						, { "attType_" + attrTypeDate.getId(), "2000-01-01" }//
						, { "attType_" + attrTypeStr.getId(), "hogeStr" }//
						, { "attType_" + attrTypeText.getId(), "fooText" } }));
		List objIds = ActSearchTestUtil.getObjIdsFromResult(doc);
		assertEquals(Arrays.asList(ArrayUtils.toObject(new long[] { obj.getId() })).toString(),
				objIds.toString());

		doc = xu.toDOM(ActSearchTestUtil.callActSearch(br, "", "", "", "hogeStr fooText", "", "",
				"", "", "", "", "", "", ""//
				, null));
		objIds = ActSearchTestUtil.getObjIdsFromResult(doc);
		assertEquals(Arrays.asList(ArrayUtils.toObject(new long[] { obj.getId() })).toString(),
				objIds.toString());

		doc = xu.toDOM(ActSearchTestUtil.callActSearch(br, "", "", "", "12345", "", "", "", "", "",
				"", "", "", ""//
				, null));
		objIds = ActSearchTestUtil.getObjIdsFromResult(doc);
		assertEquals(0, objIds.size());
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testXmlEscape() throws Exception
	{
		ActSearchTestUtil.loadInitData(false);
		EIMSession sess = TestSessionUtil.createEIMSession();
		EIMObjectType pobjType = ObjectUtils.getObjectTypeByName(sess, EIMConfig
				.get("OBJECT_TYPE_NAME_DOCUMENT"));
		EIMObjectType objType = ObjectUtils.createObjectType(sess, "objType<>\"&", pobjType);
		EIMWorkFlow wf = WorkFlowUtils.getWorkFlowById(sess, 478);
		WorkFlowUtils.applyWorkFlow(sess, objType, wf);
		EIMObject object = ObjectUtils.createObject(sess, objType, "act<>\"&search");
		ObjectUtils.lock(sess, object);
		WorkFlowUtils.updateOtherStatusTypeName(sess, object.getStatus().getType().getId(), "JA",
				"編集中<>\"&");
		EIMAttributeType attType;
		attType = AttributeUtils.getAttributeTypeByName(sess, "パス");
		ObjectAttributeUtils.setAttribute(sess, object, attType, "path<>\"&");
		attType = AttributeUtils.getAttributeTypeByName(sess, "プロパティ");
		ObjectAttributeUtils.setAttribute(sess, object, attType, "prop<>\"&");
		UserUtils.updateOtherUserName(sess, 1, "JA", "sysad<>\"&");
		EIMSecurity sec = SecurityUtils.getSecurityByName(sess, "system");
		SecurityUtils.setSecurity(sess, object, sec);
		sess.commit();
		sess.close();

		br.eimLogin("system", "manager", null, null);
		String resStr = ActSearchTestUtil.callActSearch(br, "", "", "", "act<>\"&search", "", "",
				""//
				, "", "", "", "", "", "", null);
		assertTrue(resStr.indexOf(" objTypeName=\"objType&lt;&gt;&quot;&amp;\"") > 0);
		assertTrue(resStr.indexOf(" objName=\"act&lt;&gt;&quot;&amp;search\"") > 0);
		assertTrue(resStr.indexOf(" modifyUserName=\"sysad&lt;&gt;&quot;&amp;\"") > 0);
		assertTrue(resStr.indexOf(" statusTypeName=\"編集中&lt;&gt;&quot;&amp;\"") > 0);
		assertTrue(resStr.indexOf(" lockUserName=\"sysad&lt;&gt;&quot;&amp;\"") > 0);
		assertTrue(resStr.indexOf(" property=\"prop&lt;&gt;&quot;&amp;\"") > 0);
		assertTrue(resStr.indexOf(" path=\"path&lt;&gt;&quot;&amp;\"") > 0);
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testLang() throws Exception
	{
		br.eimLogin("t1", "t1", null, null);
		Document doc = xu.toDOM(ActSearchTestUtil.callActSearch(br, "", "", "", "申請書1.log", "", "",
				""//
				, "", "", "", "", "", "", null));
		NodeIterator nl = XPathAPI.selectNodeIterator(doc, "/objList/object/@modifyDate");
		assertEquals("2007-07-09", nl.nextNode().getNodeValue());

		br.eimLogin("t1", "t1", "EN", null);
		doc = xu.toDOM(ActSearchTestUtil.callActSearch(br, "", "", "", "申請書1.log", "", "", ""//
				, "", "", "", "", "", "", null));
		nl = XPathAPI.selectNodeIterator(doc, "/objList/object/@modifyDate");
		assertEquals("07-09-2007", nl.nextNode().getNodeValue());

		// 検索件数上限オーバー時のメッセージLANG切り替えはtest1000limit()で行う
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testTimeZone() throws Exception
	{
		// リクエスト日付のタイムゾーン確認:日本時間
		br.eimLogin("t1", "t1", null, null);
		Document doc = xu.toDOM(ActSearchTestUtil.callActSearch(br, "", "", "", "", "", "", ""//
				, "2007-07-08", "2007-07-08", "", "", "", "", null));
		List objIds = ActSearchTestUtil.getObjIdsFromResult(doc);
		assertEquals(Arrays.asList(ArrayUtils.toObject(new long[] { 552, 509, 514 })).toString(),
				objIds.toString());

		// リクエスト日付のタイムゾーン確認:日本時間+1
		br.eimLogin("t1", "t1", null, TimeZone.getTimeZone("GMT+10:00"));
		doc = xu.toDOM(ActSearchTestUtil.callActSearch(br, "", "", "", "", "", "", ""//
				, "2007-07-08", "2007-07-08", "", "", "", "", null));
		objIds = ActSearchTestUtil.getObjIdsFromResult(doc);
		assertEquals(Arrays.asList(ArrayUtils.toObject(new long[] { 501, 509 })).toString(), objIds
				.toString());

		// 戻り日付のタイムゾーン確認:日本時間
		br.eimLogin("t1", "t1", null, null);
		doc = xu.toDOM(ActSearchTestUtil.callActSearch(br, "", "", "", "概算見積書2.mpp", "", "", ""//
				, "", "", "", "", "", "", null));
		NodeIterator nl = XPathAPI.selectNodeIterator(doc, "/objList/object/@modifyDate");
		assertEquals("2007-07-10", nl.nextNode().getNodeValue());
		nl = XPathAPI.selectNodeIterator(doc, "/objList/object/@lockDate");
		assertEquals("2007-08-06", nl.nextNode().getNodeValue());

		// 戻り日付のタイムゾーン確認:日本時間+0:01
		br.eimLogin("a", "a", null, TimeZone.getTimeZone("GMT+9:01"));
		doc = xu.toDOM(ActSearchTestUtil.callActSearch(br, "", "", "", "概算見積書2.mpp", "", "", ""//
				, "", "", "", "", "", "", null));
		nl = XPathAPI.selectNodeIterator(doc, "/objList/object/@objId");
		assertEquals("569", nl.nextNode().getNodeValue());
		nl = XPathAPI.selectNodeIterator(doc, "/objList/object/@modifyDate");
		assertEquals("2007-07-11", nl.nextNode().getNodeValue());
		nl = XPathAPI.selectNodeIterator(doc, "/objList/object/@lockDate");
		assertEquals("2007-08-06", nl.nextNode().getNodeValue());

		// 戻り日付のタイムゾーン確認:日本時間-15:11
		br.eimLogin("a", "a", null, TimeZone.getTimeZone("GMT-6:11"));
		doc = xu.toDOM(ActSearchTestUtil.callActSearch(br, "", "", "", "概算見積書2.mpp", "", "", ""//
				, "", "", "", "", "", "", null));
		nl = XPathAPI.selectNodeIterator(doc, "/objList/object/@modifyDate");
		assertEquals("2007-07-10", nl.nextNode().getNodeValue());
		nl = XPathAPI.selectNodeIterator(doc, "/objList/object/@lockDate");
		assertEquals("2007-08-05", nl.nextNode().getNodeValue());
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testReadOnly() throws Exception
	{
		br.eimLogin("t1", "t1", null, null);
		Document doc = xu.toDOM(ActSearchTestUtil.callActSearch(br, "", "", "", "概算見積書2.mpp", "",
				"", ""//
				, "", "", "", "", "", "", null));
		List objIds = ActSearchTestUtil.getObjIdsFromResult(doc);
		assertEquals(Arrays.asList(ArrayUtils.toObject(new long[] { 569, 586 })).toString(), objIds
				.toString());
		NodeIterator nl = XPathAPI.selectNodeIterator(doc, "/objList/object/@readOnly");
		for (Node n; (n = nl.nextNode()) != null;)
		{
			assertEquals("false", n.getNodeValue());
		}

		br.eimLogin("a", "a", null, null);
		doc = xu.toDOM(ActSearchTestUtil.callActSearch(br, "", "", "", "概算見積書2.mpp", "", "", ""//
				, "", "", "", "", "", "", null));
		objIds = ActSearchTestUtil.getObjIdsFromResult(doc);
		assertEquals(Arrays.asList(ArrayUtils.toObject(new long[] { 569 })).toString(), objIds
				.toString());
		nl = XPathAPI.selectNodeIterator(doc, "/objList/object/@readOnly");
		assertEquals("true", nl.nextNode().getNodeValue());
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void test1000limit() throws Exception
	{
		EIMSession sess = TestSessionUtil.createEIMSession();
		EIMObjectType objType = ObjectUtils.getObjectTypeByName(sess, EIMConfig
				.get("OBJECT_TYPE_NAME_DOCUMENT"));
		System.out.print("create 1000 datas ");
		EIMSecurity sec = SecurityUtils.getSecurityByName(sess, "system");
		for (int i = 0; i < 1000; i++)
		{
			if (((i + 1) % 100) == 0)
				System.out.print(".");
			SecurityUtils.setSecurity(sess, ObjectUtils.createObject(sess, objType,
					"actsearchTest_" + i), sec);
		}
		System.out.println();
		sess.commit();

		br.eimLogin("system", "manager", null, null);
		Document doc = xu.toDOM(ActSearchTestUtil.callActSearch(br, "", "", "", "actsearchTest_",
				"", "", "", "", "", "", "", "", "", null));
		List objIds = ActSearchTestUtil.getObjIdsFromResult(doc);
		assertEquals(1000, objIds.size());

		SecurityUtils.setSecurity(sess, ObjectUtils.createObject(sess, objType,
				"actsearchTest_1000"), sec);
		sess.commit();
		try
		{
			ActSearchTestUtil.callActSearch(br, "", "", "", "actsearchTest_", "", "", "", "", "",
					"", "", "", "", null);
			fail();
		} catch (EIMServerResponseError e)
		{
			assertEquals("検索結果が1000件を超えました（1001 件）\n検索条件を絞り込んで下さい。".replace('\n', ' '), e
					.getMessage());
		}

		br.eimLogin("system", "manager", "EN", null);
		try
		{
			ActSearchTestUtil.callActSearch(br, "", "", "", "actsearchTest_", "", "", "", "", "",
					"", "", "", "", null);
			fail();
		} catch (EIMServerResponseError e)
		{
			assertEquals(
					"Hit records are over 1000 (total is 1001).\nPlease review or add search conditions."
							.replace('\n', ' '), e.getMessage());
		}
	}

}
