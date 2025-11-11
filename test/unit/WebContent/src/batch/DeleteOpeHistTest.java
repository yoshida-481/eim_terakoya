package batch;

import junit.framework.TestCase;

import org.apache.commons.lang.ClassUtils;

import eimtest.util.TestDBUtil;

/** */
public class DeleteOpeHistTest extends TestCase {
	/** */
	public void setUp() throws Exception
	{
		TestDBUtil.loadDBData(this.getClass(), ClassUtils.getShortClassName(this.getClass())
				+ ".EIMTables.xls");
	}

	/** */
	public void testMe()
	{
	}
}
