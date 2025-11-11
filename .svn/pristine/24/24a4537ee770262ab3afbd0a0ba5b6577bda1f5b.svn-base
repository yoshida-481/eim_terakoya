package eimtest.app.util;

import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.ResultSet;

import eim.db.DBUtils;
import eimtest.util.TestDBUtil;

/** */
public class TestAppDBUtil {
	/**
	 * 
	 * @throws Exception
	 */
	public static void loadPrimitiveData() throws Exception {
		checkJunitAvailable();
		TestDBUtil.loadDBData(TestAppDBUtil.class, "/PrimitiveEIMTables.xls");
	}

	/**
	 * 
	 * @throws Exception
	 */
	public static void loadSampleData() throws Exception {
		checkJunitAvailable();
		TestDBUtil.loadDBData(TestAppDBUtil.class, "/SampleEIMTables.xls");
	}

	/**
	 * 
	 * @throws Exception
	 */
	public static void checkJunitAvailable() throws Exception {
		Connection jdbcCon = DBUtils.getConsoleConnection();
		DatabaseMetaData dbm = jdbcCon.getMetaData();
		ResultSet rs = dbm.getTables("", dbm.getUserName(), "EIM_JUNIT_AVAILABLE", null);
		if (!rs.next()) {
			throw new IllegalStateException("target DB schema is not support EIM JUnit test!\n ["
					+ TestDBUtil.getConData(jdbcCon) + "]");
		}
	}
}
