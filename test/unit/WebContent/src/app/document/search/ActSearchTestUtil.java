package app.document.search;

import java.util.ArrayList;
import java.util.List;

import org.apache.xpath.XPathAPI;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.traversal.NodeIterator;

import eimtest.app.util.XMLUtil;
import eimtest.app.util.net.HttpClientEIM;
import eimtest.util.TestDBUtil;

/** */
public class ActSearchTestUtil {
	/** */
	static XMLUtil xu = new XMLUtil();

	/** */
	static String jspPath = "search/actSearch.jsp";

	/**
	 * 
	 * @param doRebuildDocIdx
	 * @throws Exception
	 */
	static void loadInitData(boolean doRebuildDocIdx) throws Exception
	{
		TestDBUtil.loadDBData(ActSearchTestUtil.class, "ActSearchTest.EIMTables.xls");
		if (doRebuildDocIdx)
			TestDBUtil.rebuildEIMDocIndex();

	}

	/**
	 * 
	 * @param br
	 * @param pathCondition
	 * @param searchPath
	 * @param status
	 * @param keyword
	 * @param contents
	 * @param property
	 * @param createUserName
	 * @param createDateFrom
	 * @param createDateTo
	 * @param modifyUserName
	 * @param modifyDateFrom
	 * @param modifyDateTo
	 * @param updDesc
	 * @param userExts
	 * @return o
	 * @throws Exception
	 */
	static String callActSearch(HttpClientEIM br, String pathCondition, String searchPath,
			String status, String keyword, String contents, String property, String createUserName,
			String createDateFrom, String createDateTo, String modifyUserName,
			String modifyDateFrom, String modifyDateTo, String updDesc, String[][] userExts)
			throws Exception
	{
		String[][] params = {//
		{ "pathCondition", pathCondition }//
				, { "searchPath", searchPath }//
				, { "status", status }//
				, { "keyword", keyword }//
				, { "contents", contents }//
				, { "property", property }//
				, { "createUserName", createUserName }//
				, { "createDateFrom", createDateFrom }//
				, { "createDateTo", createDateTo }//
				, { "modifyUserName", modifyUserName }//
				, { "modifyDateFrom", modifyDateFrom }//
				, { "modifyDateTo", modifyDateTo }//
				, { "attType_16", updDesc } //
		};
		if (userExts != null)
		{
			String[][] paramsU = new String[params.length + userExts.length][];
			System.arraycopy(params, 0, paramsU, 0, params.length);
			System.arraycopy(userExts, 0, paramsU, params.length, userExts.length);
			params = paramsU;
		}
		return br.get(jspPath, params);
	}

	/**
	 * 
	 * @param doc
	 * @return o
	 * @throws Exception
	 */
	public static List getObjIdsFromResult(Document doc) throws Exception
	{
		List objIds = new ArrayList();
		NodeIterator nl = XPathAPI.selectNodeIterator(doc, "/objList/object/@objId");
		for (Node n; (n = nl.nextNode()) != null;)
		{
			objIds.add(n.getNodeValue());
		}
		return objIds;
	}
}
