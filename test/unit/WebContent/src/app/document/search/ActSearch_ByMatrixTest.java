package app.document.search;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

import junit.extensions.TestSetup;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.apache.commons.lang.ClassUtils;
import org.apache.commons.lang.StringUtils;
import org.w3c.dom.Document;

import eimtest.app.util.XMLUtil;
import eimtest.app.util.XlsSheetAccess;
import eimtest.app.util.net.HttpClientEIM;

/** */
public class ActSearch_ByMatrixTest extends TestCase {

	/**
	 * 
	 * @return o
	 * @throws Exception
	 */
	public static Test suite() throws Exception
	{
		TestSuite suite = new TestSuite();

		AppTestCase[] atc = readTestCaseData();
		for (int i = 0; i < atc.length; i++)
			suite.addTest(new ActSearch_ByMatrixTest(atc[i]));

		TestSetup w = new TestSetup(suite) {
			public void setUp() throws Exception
			{
				if (true)
				{
					ActSearchTestUtil.loadInitData(true);
				}
			}
		};
		return w;
	}

	/** */
	HttpClientEIM br;

	/** */
	XMLUtil xu;

	/** */
	AppTestCase atc;

	/**
	 * 
	 * @param atc
	 */
	public ActSearch_ByMatrixTest(AppTestCase atc) {
		super("test " + atc.caseNo);
		this.atc = atc;
	}

	/**
	 * 
	 */
	public void setUp() throws Exception
	{
		br = new HttpClientEIM("app/document");
		xu = new XMLUtil();
	}

	/**
	 * 
	 */
	protected void runTest() throws Throwable
	{
		br.eimLogin(atc.userId, atc.userId, null, null);
		Document doc = xu.toDOM(ActSearchTestUtil.callActSearch(br, atc.pathCondition() // pathCondition
				, atc.searchPath//
				, atc.status()//
				, atc.keyword()//
				, atc.contents()//
				, atc.dtlProperty//
				, atc.dtlCuserName//
				, atc.dtlCdateFrom//
				, atc.dtlCdateTo//
				, atc.dtlMuserName//
				, atc.dtlMdateFrom//
				, atc.dtlMdateTo//
				, atc.dtlUpdDesc, null));

		List objIds = ActSearchTestUtil.getObjIdsFromResult(doc);

		assertEquals("case " + atc.caseNo + " " + Arrays.asList(atc.expectDocIDs), "case "
				+ atc.caseNo + " " + objIds);
	}

	/**
	 * 
	 * @return o
	 * @throws Exception
	 */
	static AppTestCase[] readTestCaseData() throws Exception
	{
		int caseNoToColOffset = 2;
		int startRowIn = 1;
		int startRowOut = 15;

		XlsSheetAccess xsa = new XlsSheetAccess(ActSearch_ByMatrixTest.class.getResource(
				ClassUtils.getShortClassName(ActSearch_ByMatrixTest.class) + ".xls").getPath());

		String[] ids;
		{
			ArrayList idsList = new ArrayList();
			xsa.seek(startRowOut, caseNoToColOffset);
			while (!StringUtils.isBlank(xsa.get()))
			{
				idsList.add(xsa.get());
				xsa.nextRow();
			}
			ids = (String[]) idsList.toArray(new String[idsList.size()]);
		}

		ArrayList ret = new ArrayList();

		for (int caseNo = 1; !StringUtils.isBlank(xsa.get(startRowIn, caseNo + caseNoToColOffset)); caseNo++)
		{
			AppTestCase atc = new AppTestCase();
			atc.caseNo = caseNo;
			atc.userId = xsa.nextRow().get();
			atc.searchPath = xsa.nextRow().get();
			atc.keyword = xsa.nextRow().get();
			atc.doDocSearch = xsa.nextRow().get();
			atc.status = xsa.nextRow().get();
			atc.dtlProperty = xsa.nextRow().get();
			atc.dtlCuserName = xsa.nextRow().get();
			atc.dtlCdateFrom = xsa.nextRow().get();
			atc.dtlCdateTo = xsa.nextRow().get();
			atc.dtlMuserName = xsa.nextRow().get();
			atc.dtlMdateFrom = xsa.nextRow().get();
			atc.dtlMdateTo = xsa.nextRow().get();
			atc.dtlUpdDesc = xsa.nextRow().get();

			ArrayList expectIds = new ArrayList();
			xsa.seek(startRowOut, -1);
			for (int i = 0; i < ids.length; i++)
			{
				if (!StringUtils.isBlank(xsa.get()))
					expectIds.add(ids[i]);
				xsa.nextRow();
			}
			atc.expectDocIDs = (String[]) expectIds.toArray(new String[expectIds.size()]);
			Arrays.sort(atc.expectDocIDs, new Comparator() {
				public int compare(Object o1, Object o2)
				{
					String s1 = (String) o1;
					String s2 = (String) o2;
					return Integer.parseInt(s1.split(",")[1]) - Integer.parseInt(s2.split(",")[1]);
				}
			});
			for (int i = 0; i < atc.expectDocIDs.length; i++)
			{
				atc.expectDocIDs[i] = atc.expectDocIDs[i].split(",")[0];
			}
			ret.add(atc);
		}
		return (AppTestCase[]) ret.toArray(new AppTestCase[ret.size()]);
	}

	/** */
	static class AppTestCase {
		/** */
		int caseNo;

		/** */
		String userId;

		/** */
		String searchPath;

		/** */
		String keyword;

		/** */
		String doDocSearch;

		/** */
		String status;

		/** */
		String dtlProperty;

		/** */
		String dtlCuserName;

		/** */
		String dtlCdateFrom;

		/** */
		String dtlCdateTo;

		/** */
		String dtlMuserName;

		/** */
		String dtlMdateFrom;

		/** */
		String dtlMdateTo;

		/** */
		String dtlUpdDesc;

		/** */
		String[] expectDocIDs;

		/**
		 * 
		 * @return o
		 */
		String pathCondition()
		{
			return searchPath.equals("*") ? "" : "true";
		}

		/**
		 * 
		 * @return o
		 */
		String status()
		{
			if (status.equals("未公開"))
				return "edit";
			else if (status.equals("公開済"))
				return "public";
			else if (status.equals("改訂中"))
				return "checkout";

			return "";
		}

		/**
		 * 
		 * @return o
		 */
		String keyword()
		{
			return keyword.replace('\n', ' ').replace('\r', ' ');
		}

		/**
		 * 
		 * @return o
		 */
		String contents()
		{
			return doDocSearch.equals("Y") ? "true" : "";
		}
	}
}
