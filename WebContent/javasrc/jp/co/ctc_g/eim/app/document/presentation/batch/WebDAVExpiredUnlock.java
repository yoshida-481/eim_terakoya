package jp.co.ctc_g.eim.app.document.presentation.batch;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import eim.util.EIMConfig;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.common.context.TransactionContext;
import jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum;
import jp.co.ctc_g.eim.framework2.common.exception.EIMException;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;
import jp.co.ctc_g.eim.framework2.integration.dao.cache.DataCacheDao;
import jp.co.ctc_g.eim.framework2.integration.dao.plugin.DatabasePlugInLoader;

/**
 * WebDAV期限切れアンロック実行クラス
 */
public class WebDAVExpiredUnlock
{
	
	/** logger */
	private static Log log = LogFactory.getLog(WebDAVExpiredUnlock.class);
	
	/** 常駐フラグ */
	public static final boolean IS_LOOP = false;
	/** WebDAVタイムアウト時間(分) */
	public static int webDAVTimeoutMinute = 180;
	
	/**
	 * WebDAV期限切れアンロック実行のメイン処理。<br>
	 * <br>
	 * @param args 実行時引数<br>
	 * <br>
	 */
	public static void main(String[] args) throws Exception {
		
		// 終了ステータスを保持する
		int status = 0;

		// 異常終了ステータスコードを取得する
		String abnormalStatusConfig = EIMConfig.get("ABNORMAL_EXIT_STATUS_CODE");
		int abnormalStatus = 1;
		if (abnormalStatusConfig != null && abnormalStatusConfig.length() > 0 && Integer.valueOf(abnormalStatusConfig) != abnormalStatus) {
			abnormalStatus = Integer.valueOf(abnormalStatusConfig);
		}

		log.info("WebDAV期限切れアンロック実行バッチ 開始");
		try {
			
			// コンテキストファイル読み込み
			loadApplicationContext();
			
			// WebDAVロックタイムアウト
			String value = ConfigUtils.getByKey("WEBDAV_LOCK_TIMEOUT");
			if (value != null) {
				webDAVTimeoutMinute = Integer.parseInt(ConfigUtils.getByKey("WEBDAV_LOCK_TIMEOUT"));
			}
			
			while(true) {
				
				// トランザクション生成
				createTransaction();
				
				// WebDAVの期限切れアンロック処理
				doWebDAVUnlock();
				
				// コミット
				EIMThreadContext.getTransactionContext().getDBConnection().commit();
				EIMThreadContext.getTransactionContext().getDBConnection().close();
				
				if (IS_LOOP) {
					// 設定ファイルに指定した時間分待機
					Thread.sleep(Integer.parseInt(ConfigUtils.getByKey("MAIL_RESIDENT_WAIT")));
				} else {
					break;
				}
			}
			
		} catch(Exception e) {
			status = abnormalStatus;
			log.error(e.getMessage(), e);
			// ロールバック
			if (EIMThreadContext.getTransactionContext() != null) {
				try {
					EIMThreadContext.getTransactionContext().getDBConnection().rollback();
				} catch (Exception ee) {
					log.error(ee.getMessage(), ee);
				}
			}
		}  catch(Throwable t) {
			status = abnormalStatus;
			log.error(t.getMessage(), t);
			// ロールバック
			if (EIMThreadContext.getTransactionContext() != null) {
				try {
					EIMThreadContext.getTransactionContext().getDBConnection().rollback();
				} catch (Exception ee) {
					log.error(ee.getMessage(), ee);
				}
			}
		} finally {
			// DBクローズ
			if (EIMThreadContext.getTransactionContext() != null) {
				try {
					EIMThreadContext.getTransactionContext().getDBConnection().close();
				} catch (Exception e) {
					log.error(e.getMessage(), e);
				}
			}
		}
		log.info("WebDAV期限切れアンロック実行バッチ 終了");
		
		// 終了ステータスを返却する
		System.exit(status);
	}
	
	/**
	 * 期限切れのWebDAVロックフラグを解除する
	 * @throws Exception
	 */
	private static void doWebDAVUnlock() throws Exception {
		
		// Transaction
		TransactionContext tx = EIMThreadContext.getTransactionContext();
		// Connection
		Connection conn = tx.getDBConnection();
		
		String sqlSelect =
				"select " +
				"id " +
				"from " +
				"EIMOBJ " +
				"where " +
				"id in (select id from EIMOBJINT where type = (select id from EIMATTR where name = '" + ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_WEBDAV_LOCK_FLAG") + "')) " +
				"and luser is not null " +
				"and ldate is not null " +
				"and ldate <= (" +
					DatabasePlugInLoader.getPlugIn().getQueryStringTimestampTrancated() + 
					" - " + 
					DatabasePlugInLoader.getPlugIn().getQueryStringIntervalDays(webDAVTimeoutMinute, ChronoUnit.MINUTES) +
				") for update ";
		
		PreparedStatement stmtSelect = null;
		PreparedStatement stmtUpdate = null;
		PreparedStatement stmtDelete = null;
		PreparedStatement stmtLockDelete = null;
		ResultSet rsetSelect = null;
		
		log.info("WebDAV期限切れアンロック実行バッチ WebDAVタイムアウト時間(分) = " + webDAVTimeoutMinute);
		try {
			
			
			// 対象オブジェクトの取得とロック
			stmtSelect = conn.prepareStatement(sqlSelect);
			log.info("WebDAV期限切れアンロック対象取得 開始");
			log.info(sqlSelect);
			rsetSelect = stmtSelect.executeQuery();
			log.info("WebDAV期限切れアンロック対象取得 終了");
			
			// 返却値リスト
			List<Long> rtList = new ArrayList<Long>();
			while(rsetSelect.next()) {
				rtList.add(rsetSelect.getLong("id"));
			}
			
			if (rtList.size() > 0) {
				
				String inSql = inStr(rtList, "id");
				String sqlUpdate = "update EIMOBJ set luser = null, ldate = null where (" + inSql + ")";
				String sqlDelete = "delete from EIMOBJINT where type = (select id from EIMATTR where name = '" + ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_WEBDAV_LOCK_FLAG") + "') and key = 0 and (" + inSql +  ")";
				
				// ロックユーザ、ロック日時
				stmtUpdate = conn.prepareStatement(sqlUpdate);
				log.info("WebDAV期限切れアンロック実行バッチ ロックユーザ、ロック日時の解除 開始");
				log.info(sqlUpdate);
				stmtUpdate.executeUpdate();
				log.info("WebDAV期限切れアンロック実行バッチ ロックユーザ、ロック日時の解除 終了");
				
				// WebDAVロックフラグ
				stmtDelete = conn.prepareStatement(sqlDelete);
				log.info("WebDAV期限切れアンロック実行バッチ WebDAVロックフラグの解除 開始");
				log.info(sqlDelete);
				stmtDelete.executeUpdate();
				log.info("WebDAV期限切れアンロック実行バッチ WebDAVロックフラグの解除 終了");
				
				// ロックフラグ
				// TODO WebDavサーブレットに対する関電工向け対応が製品に未反映 (ロックフラグ(HEADメソッド通過フラグ)を文書の属性で保持)
				//      暫定対応としてコンフィグに当該属性名称の定義があった場合に限り、その値を削除する。
				String sqlLockDelete = null;
				try {
					sqlLockDelete = "delete from EIMOBJINT where type = (select id from EIMATTR where name = '" + ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_LOCK_FLAG") + "') and key = 0 and (" + inSql +  ")";
				} catch (EIMException e) {
					log.info(e.getMessage());
				}
				if (sqlLockDelete != null) {
					stmtLockDelete = conn.prepareStatement(sqlLockDelete);
					log.info("WebDAV期限切れアンロック実行バッチ ロックフラグの解除 開始");
					log.info(sqlLockDelete);
					stmtLockDelete.executeUpdate();
					log.info("WebDAV期限切れアンロック実行バッチ ロックフラグの解除 終了");
				}
				
				// 更新ログ登録
				DataCacheDao objectCacheDao = (DataCacheDao) ApplicationContextLoader.getApplicationContext().getBean("objectCacheDao");
				List<ObjectDomain> objectList = rtList.stream().map(ObjectDomain::new).collect(Collectors.toList());
				log.info("WebDAV期限切れアンロック実行バッチ 更新ログの作成 開始");
				objectCacheDao.putUpdateLogAll(objectList);
				log.info("WebDAV期限切れアンロック実行バッチ 更新ログの作成 終了");
			} else {
				log.info("WebDAV期限切れアンロック対象なし");
			}
			
		} finally {
			if(rsetSelect != null) {
				rsetSelect.close();
			}
			if(stmtSelect != null) {
				stmtSelect.close();
			}
			if(stmtUpdate != null) {
				stmtUpdate.close();
			}
			if(stmtDelete != null) {
				stmtDelete.close();
			}
			if(stmtLockDelete != null) {
				stmtLockDelete.close();
			}
		}
		
	}
	
	/**
	 * コンテキストファイルロードの処理。<br>
	 * <br>
	 */
	private static void loadApplicationContext() {
		try {
			// コンテキストファイル読み込み
			String[] contextFiles = {"applicationContext.xml"};
			ApplicationContextLoader.init(contextFiles);
		} catch(Exception e) {
			log.error(e.getMessage(), e);
		}
	}
	
	/**
	 * トランザクション生成の処理。<br>
	 * <br>
	 * コンソールモードでトランザクションを生成します。<br>
	 * <br>
	 * @throws Exception 処理中にエラーが発生した場合、例外を通知します。<br>
	 * <br>
	 */
	private static void createTransaction() throws Exception {
		
		// DB接続再試行フラグ
		TransactionContext context = null;
		boolean isRetry = true;
		
		//トランザクション生成
		while (isRetry) {
			try {
				// トランザクション
				context = new TransactionContext(ConnectionModeEnum.CONSOLE);
				EIMThreadContext.putTransactionContext(context);
				// DB接続成功時、ループから抜ける
				isRetry = false;
			} catch (Exception e) {
				log.error(e.getMessage(), e);
				isRetry = true;
				// DB接続失敗時、一定時間待機
				Thread.sleep(Integer.parseInt(ConfigUtils.getByKey("WAIT")));
			}
		}
	}
	
	private static String inStr(List<Long> longList, String column) {
		
		StringBuffer sb = new StringBuffer();
		
		List<StringBuffer> sbCondList = longListToStrList(longList);
		//sb.append("( ");
		for(int i = 0; i < sbCondList.size(); i++) {
			if(i != 0) sb.append("or ");
			sb.append(column + " in (" + sbCondList.get(i) + ") ");
		}
		//sb.append(") ");
		
		return sb.toString();
	}
	
	private static List<StringBuffer> longListToStrList(List<Long> list) {

		List<StringBuffer> sbList = new ArrayList<StringBuffer>();

		StringBuffer sb = new StringBuffer();
		int cnt = 0;
		for(Long element: list) {

			if(cnt == 1000) {
				sbList.add(sb);
				sb = new StringBuffer();
				cnt = 0;
			}

			if(sb.length() > 0) sb.append(",");
			sb.append(element);
			cnt++;
		}

		sbList.add(sb);

		return sbList;
	}
	
}