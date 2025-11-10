package eimtest.app.tool;

import eimtest.app.util.TestAppDBUtil;

/** */
public class InitDBSampleData {
	/**
	 * 
	 * @param args
	 * @throws Exception
	 */
	public static void main(String[] args) throws Exception
	{
		TestAppDBUtil.loadSampleData();
	}
}
