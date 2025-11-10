package batch.maintenance;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.List;

import javax.sql.DataSource;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.context.ApplicationContext;

import common.util.AppMessageUtils;
import eim.bo.EIMDirectory;
import eim.bo.EIMException;
import eim.bo.EIMFormat;
import eim.bo.EIMUser;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.FileUtils;
import jp.co.ctc_g.eim.framework.common.util.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework2.common.context.TransactionContext;
import jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum;


/**
 * 検証環境用
 * ダミーファイルの作成
 *
 * 第1引数：フォーマット名(原本ドキュメント,公開ドキュメント)
 * 第2引数：コピー元ダミーファイル
 * @author
 */
public class CreateDummyFile
{

	// Error Logging
	private static Log logger = LogFactory.getLog(CreateDummyFile.class);

	/**
	 * Main Function
	 * @param args
	 * @throws Exception
	 */
	public static void main(String[] args)
	throws Exception
	{

		logger.info("######################################start##########################################");

		EIMSession sess = null;
		try
		{
			//user取得
			EIMUser user = new EIMUser(1, null, null, null, null, null, 255, 0, null);
			//lang取得
			String lang = "";
			String EIM_CONFIG_LANG = "MESSAGELANG";
			String DEFAULT_LANG	= "JA";
			if(EIMConfig.get(EIM_CONFIG_LANG) != null){
				lang = EIMConfig.get(EIM_CONFIG_LANG);
			}else{
				lang = DEFAULT_LANG;
			}
			//Session
			ApplicationContext context = ApplicationContextLoader.getContext();
			DataSource ds = (DataSource)context.getBean("dataSource");
			sess = new EIMSession(user,lang);
			sess.setConnection(ds.getConnection());
			sess.setConsoleMode(true);
			sess.getDBConnection().setAutoCommit(false);
			EIMThreadContext.putEIMSession(sess);

			// Transaction context
			TransactionContext tran = new TransactionContext(ConnectionModeEnum.CONSOLE, new UserDomain(user.getId()), lang);
			tran.setDBConnection(sess.getDBConnection());
			EIMThreadContext.putTransactionContext(tran);

			// 第1引数チェック
			String formatParam = null;
			if(args.length > 1 && args[0] != null){
				formatParam = args[0];
			}else{
				logger.error("フォーマット名を入力してください。");
				return;
			}

			// 第2引数チェック
			Path dummyFilePath = null;
			try {
				if(args.length > 1 && args[1] != null){
					dummyFilePath = Paths.get(args[1]);
				}else{
					logger.error("ダミーファイルのパスをFullPathで指定してください。");
					return;
				}
			} catch (Exception e) {
				logger.error("ダミーファイルが確認できません。バッチの引数を確認してください。" + dummyFilePath , e);
			}

			// ファイル一覧取得
			List<String> originalFileList = getFilePathList(sess, formatParam);
			logger.info("ファイル件数：" + originalFileList.size());
			long succsess = 0;
			long warn = 0;
			long err = 0;
			// ファイル生成
			for (String destPath : originalFileList) {
				try {
					File destFile = new File(destPath);

					if( destFile.exists() ) {
						warn++;
						continue;
					}
					Path dest = Paths.get(destPath);
					Files.copy(dummyFilePath, dest);
					succsess ++;
				} catch (IOException e) {
					err++;
					logger.error("ダミーファイルのコピーに失敗しました。" + destPath , e);
				}
			}

			logger.info("■成功："+ succsess + "件");
			logger.info("■既に存在する："+ warn + "件");
			logger.info("■失敗："+ err + "件");

			logger.info("######################################end##########################################");
		}
		catch(EIMException eime)
		{
			logger.error(AppMessageUtils.makeLogMessage(eime.getMessage()), eime);
			try{
				EIMThreadContext.removeEIMSession();
				if(sess != null){
					sess.rollback();
				}
			}
			catch (Exception se) {
				logger.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
			}
		}
		catch(Exception e)
		{
			logger.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
			try{
				EIMThreadContext.removeEIMSession();
				if(sess != null){
					sess.rollback();
				}
			}
			catch (Exception se) {
				logger.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
			}
		}
		finally
		{
			try{
				EIMThreadContext.removeEIMSession();
				if(sess != null){
					sess.close();
				}
			}
			catch (Exception se) {
				logger.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
			}
		}
	}

	/**
	 * ファイルパスのリストを取得する
	 * @param sess
	 * @param formatName
	 * @return
	 * @throws Exception
	 */
	private static List<String> getFilePathList(EIMSession sess, String formatName) throws Exception{

		// ファイル取得
		EIMFormat format = FileUtils.getFormatByName(sess, formatName);
		List<EIMDirectory> dirList = FileUtils.getDirectoryList(sess, format);

		List<String> pathList = new ArrayList<String>();
		Connection conn = sess.getDBConnection();
		CallableStatement cstmt = null;
		ResultSet rs = null;
		String sql = "select id, ext, seq from EIMFILE where dir = ?";
		for (EIMDirectory dir :  dirList) {
			cstmt = conn.prepareCall(sql);
			cstmt.setLong(1, dir.getId());
			rs = cstmt.executeQuery();
			while (rs.next()) {
				long seq = rs.getLong("seq");
				if (seq == 0) {

					pathList.add(dir.getPath() + rs.getLong("id") + rs.getString("ext"));
					logger.debug(dir.getPath() + rs.getLong("id") + rs.getString("ext"));

				} else {

					pathList.add(dir.getPath() + rs.getLong("id") + "_" + seq + rs.getString("ext"));
					logger.debug(dir.getPath() + rs.getLong("id") + "_" + seq + rs.getString("ext"));

				}
			}
			if (rs != null) {
				rs.close();
			}
			if (cstmt != null) {
				cstmt.close();
			}
		}
		return pathList;
	}
}