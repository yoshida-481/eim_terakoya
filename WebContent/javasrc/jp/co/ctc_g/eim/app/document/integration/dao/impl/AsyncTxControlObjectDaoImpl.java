package jp.co.ctc_g.eim.app.document.integration.dao.impl;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import jp.co.ctc_g.eim.app.document.business.dao.AsyncTxControlObjectDao;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.common.context.TransactionContext;
import jp.co.ctc_g.eim.framework2.common.exception.EIMException;
import jp.co.ctc_g.eim.framework2.integration.dao.plugin.DatabasePlugIn.SQLExceptionTypeEnum;
import jp.co.ctc_g.eim.framework2.integration.dao.plugin.DatabasePlugInLoader;

/**
 * オブジェクト登録時のバックグラウンド処理で非同期処理を実行するとき、
 * フォアグラウンド処理のトランザクションを待機する機能を提供するDao実装クラスです。
 */
public class AsyncTxControlObjectDaoImpl implements AsyncTxControlObjectDao {

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.dao.AsyncTxControlObjectDao#waitTransactionEnd(jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain)
	 */
	@Override
	public void waitTransactionEnd(ObjectDomain object) throws Exception {

		// 対象オブジェクトのIDを指定したselect for update文を生成
		String selectSql =
				"select id from EIMOBJ " +
						"where id = ? " +
						"for update";

		// 対象オブジェクトのIDを指定したinsert文を生成
		String insertSql = String.format(
				"insert into EIMOBJ " +
						"values(?, ?, 'DUMMY', 0, 1, 1, %1$s, 1, %1$s, null, null, 0, 0, 0, 0)",
				DatabasePlugInLoader.getPlugIn().getQueryStringTimestamp());

		TransactionContext txCtx = EIMThreadContext.getTransactionContext();
		Connection conn = txCtx.getDBConnection();

		// select for update文を実行(ファイル更新処理で発行されたupdate文がコミットされるまで待機する)
		try (PreparedStatement selectPstmt = conn.prepareStatement(selectSql)) {
			selectPstmt.setLong(1, object.getId());

			try (ResultSet rset = selectPstmt.executeQuery()) {
				// オブジェクトが存在する場合は正常終了
				if (rset.next()) {
					return;
				}
			}

			// insert文を実行(ファイル登録処理で発行されたinsert文がコミットされるまで待機する)
			try (PreparedStatement insertPstmt = conn.prepareStatement(insertSql)) {
				insertPstmt.setLong(1, object.getId());
				insertPstmt.setLong(2, object.getType().getId());

				insertPstmt.execute();
	
				// 一意制約違反にならない場合は存在しない
				// 例外をthrowしてロールバックする
				// オブジェクト[{0}]が取得できません。
				throw new EIMException("EIM.ERROR.OBJECT.NOTFOUND.DETAIL", object.getName());
			} catch (SQLException e) {
				if (DatabasePlugInLoader.getPlugIn().matchesSQLException(e, SQLExceptionTypeEnum.UNIQUE_CONSTRAINT_VIOLATION)) {
					// オブジェクトが存在する場合(一意制約違反が発生)は正常終了
					return;
				}
				throw e;
			}
		}
	}

}
