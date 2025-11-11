package app.document.table;

import junit.framework.TestCase;

import org.apache.commons.lang.ClassUtils;
import org.w3c.dom.Document;

import eimtest.app.util.XMLUtil;
import eimtest.app.util.net.HttpClientEIM;
import eimtest.util.Misc;
import eimtest.util.TestDBUtil;

/** */
public class DspTableAttributeTypeListTest extends TestCase {
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
		// @Test:テーブル「TB」を取得する
		Document doc = callServer("417");
		assertEquals(Misc.loadFile(this.getClass().getResource(
				ClassUtils.getShortClassName(this.getClass()) + ".TB417.txt").getPath()), xu
				.toStr(doc));
	}

	/**
	 * 
	 * @param tableId
	 * @return o
	 * @throws Exception
	 */
	Document callServer(String tableId) throws Exception
	{
		return xu.toDOM(br.get("table/dspTableAttributeTypeList.jsp"//
				, new String[][] { { "tableId", tableId } //
				}));
	}
}
