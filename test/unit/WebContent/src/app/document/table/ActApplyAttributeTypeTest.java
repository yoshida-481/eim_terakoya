package app.document.table;

import junit.framework.TestCase;

import org.apache.commons.lang.ClassUtils;
import org.w3c.dom.Document;

import eimtest.app.util.XMLUtil;
import eimtest.app.util.net.EIMServerResponseError;
import eimtest.app.util.net.HttpClientEIM;
import eimtest.util.Misc;
import eimtest.util.TestDBUtil;

/** */
public class ActApplyAttributeTypeTest extends TestCase {
	/** */
	HttpClientEIM br;

	/** */
	XMLUtil xu;

	/** */
	public void setUp() throws Exception
	{
		TestDBUtil.loadDBData(this.getClass(), ClassUtils.getShortClassName(this.getClass())
				+ ".EIMTables.xls");
		br = new HttpClientEIM("app/document");
		xu = new XMLUtil();
		br.eimLogin("system", "manager", null, null);
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testUserAttribute() throws Exception
	{
		// @Test:属性20,22,21追加による、XML応答とDB内容を確認
		Document doc = callServer("417", "20,22,21");
		assertEquals(Misc.loadFile(this.getClass().getResource(
				ClassUtils.getShortClassName(this.getClass()) + ".applyed.txt").getPath()), xu
				.toStr(doc));
		TestDBUtil.assertTableData("EIMTBLITEM", this.getClass(), ClassUtils.getShortClassName(this
				.getClass())
				+ ".EIMTables.expected.xls");

		// 登録済み属性がエラーになる確認
		try
		{
			callServer("417", "20");
			fail();
		} catch (EIMServerResponseError e)
		{
			assertEquals("指定した属性は既に適用されています。", e.getMessage());
		}

		// 存在しない属性がエラーになり、ロールバックされる確認
		try
		{
			callServer("417", "23,12345");
			fail();
		} catch (EIMServerResponseError e)
		{
			assertEquals("指定した属性タイプが存在しません。", e.getMessage());
			// 23はinsertがrollbackされる
			TestDBUtil.assertTableData("EIMTBLITEM", this.getClass(), ClassUtils
					.getShortClassName(this.getClass())
					+ ".EIMTables.expected.xls");
		}

	}

	/**
	 * 
	 * @param tableId
	 * @param attrTypeIds
	 * @return o
	 * @throws Exception
	 */
	Document callServer(String tableId, String attrTypeIds) throws Exception
	{
		return xu.toDOM(br.get("table/actApplyAttributeType.jsp"//
				, new String[][] {//
				{ "tableId", tableId } //
						, { "attTypeIds", attrTypeIds } }));
	}
}
