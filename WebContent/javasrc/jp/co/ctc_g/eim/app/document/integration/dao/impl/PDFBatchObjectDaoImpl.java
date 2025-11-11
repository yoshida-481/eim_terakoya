
package jp.co.ctc_g.eim.app.document.integration.dao.impl;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import jp.co.ctc_g.eim.app.document.integration.dao.PDFBatchObjectDao;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * 排他ロックに関するObjectDaoの追加機能実装クラスです。
 */
public class PDFBatchObjectDaoImpl implements PDFBatchObjectDao {

	/** ロガー */
	private static Log log = LogFactory.getLog(PDFBatchObjectDaoImpl.class);

	/**
	 * 指定したオブジェクトIDのDBレコードの行に排他ロックをかけます。
	 *
	 * @param id 排他ロック対象のオブジェクトID
	 *
	 * @return 処理結果
	 *  1:指定オブジェクトIDのDBレコードが存在し、排他ロックを掛けた。
	 *  0:指定オブジェクトIDのDBレコードが存在せず、排他ロックを掛けない。
	 * @throws Exception 例外<br>
	 *  ロック利用不可(リソースビジー)の場合はSQLExceptionがthrowされる。
	 *    Oracle : エラーコード=54
	 *    PostgreSQL : SQLステータス=55P03
	 */
	public int lockObjectById(long objectId) throws Exception {

		if(objectId < 1){
			// オブジェクトIDが1以下の為、指定オブジェクトIDのDBレコードは存在しない
			return 0;
		}

		// Connection
		Connection conn = EIMThreadContext.getEIMSession().getDBConnection();

		// SQL
		String sql =
					"select " +
						"EO.ID " +
					"from " +
						"EIMOBJ EO " +
					"where " +
						"EO.ID = ? " +
					"for update nowait";

		log.debug(sql);

		PreparedStatement pstmt = null;
		ResultSet rs = null;
		List<Long> arrayList = new ArrayList<>();

		try {
			// PreparedStatement
			pstmt = conn.prepareStatement(sql);

			pstmt.setLong(1, objectId);

			// Execute
			rs = pstmt.executeQuery();

			// Result

			while(rs.next()) {
				arrayList.add(rs.getLong("ID"));
			}

		} finally {
			if (rs != null)
				rs.close();
			if (pstmt != null)
				pstmt.close();
		}

		// 対象が存在しない場合は「0」結果を返却する
		if (arrayList.isEmpty()) {
			return 0;
		}

		// 対象が存在する場合は「1」結果を返却する
		return 1;
	}

}
