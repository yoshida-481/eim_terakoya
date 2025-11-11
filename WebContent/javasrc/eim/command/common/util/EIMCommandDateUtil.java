package eim.command.common.util;

import java.sql.ResultSet;
import java.sql.Statement;
import java.util.List;

import eim.net.EIMSession;
import jp.co.ctc_g.eim.framework2.integration.dao.plugin.DatabasePlugInLoader;

/**
 * EIMCommandのデータ関連クラス
 * @author kanno
 *
 */
public class EIMCommandDateUtil {
	
	/**
	 * DBサーバより、オフセット・タイムゾーンを取得する。
	 * 取得に失敗した場合はnullを返す。
	 * @param sess
	 * @return
	 * @throws Exception
	 */
	public static String getDBServerOffset(EIMSession sess) throws Exception
	{
		String offset = null;
		
		// DBサーバより、オフセット・タイムゾーンを取得
		Statement stmt = sess.getDBConnection().createStatement();
		try
		{
			List<String> sqlList = DatabasePlugInLoader.getPlugIn().getQueryStringTimezone();

			if (sqlList.size() == 1) {
				// oracleのタイムゾーンオフセット取得
				ResultSet rset = stmt.executeQuery(sqlList.get(0));
				if (rset.next()) {
					offset = rset.getString(1).trim();
				}
			} else {
				// postgresのタイムゾーンオフセット取得
				ResultSet rset = stmt.executeQuery(sqlList.get(0));
				if (rset.next()) {
					offset = rset.getString(1).trim();
					ResultSet rset2 = stmt.executeQuery(String.format(sqlList.get(1), offset));
					if (rset2.next()) {
						offset = rset2.getString(1).trim();
					}
				}
				if (!offset.substring(0,1).equals("-")) {
					// postgresから取得したオフセットには先頭に「+」がつかないため付与する
					offset = "+" + offset;
				}
			}
		}
		catch(Exception e)
		{
			e.printStackTrace();
		}
		finally
		{
			if(stmt != null)
			{
				stmt.close();
			}
		}
		return offset;
	}
	
	/**
	 * EIMSessionより、オフセット・タイムゾーンを取得する。
	 * 取得に失敗した場合はnullを返す。
	 * @param sess
	 * @return
	 */
	public static String getDBServerOffsetFromSession(EIMSession sess)
	{
		String result = null;
		if(sess != null)
		{
			result =  String.valueOf(sess.getAttribute(EIMCommandConstant.SESSION_ATTRIBUTE_DBTZ_OFFSET_FOR_EIMCOMMAND));
		}
		return result;
	}

}
