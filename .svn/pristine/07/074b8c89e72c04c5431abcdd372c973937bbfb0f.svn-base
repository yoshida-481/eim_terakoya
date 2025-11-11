package batch;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import javax.sql.DataSource;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.context.ApplicationContext;

import common.util.AppMessageUtils;
import eim.bo.EIMException;
import eim.bo.EIMResource;
import eim.bo.EIMUser;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.StringUtils;
import jp.co.ctc_g.eim.admin.business.service.AdminUserService;
import jp.co.ctc_g.eim.framework.common.util.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework2.business.service.UserService;
import jp.co.ctc_g.eim.framework2.common.context.TransactionContext;
import jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum;

/**
*
* ユーザインポートバッチ
* システム管理のユーザインポート機能をバッチ化
* IMPORT_USER_FILEにシステム管理からエクスポートし、編集したExcelファイルを配置する
* その他の仕様はシステム管理のユーザインポート機能に準拠する
*/
public class UserImportBatch {

	/** ログ */
	private static Log log = LogFactory.getLog(UserImportBatch.class);

	/** アプリケーションコンテキスト */
	private static ApplicationContext context;

	/** セッション */
	private static EIMSession sess;

	/**
	 * ユーザインポートバッチメイン処理
	 * @param args なし
	 */
	public static void main(String[] args) {
		try {
			// 開始ログ
			log.info(" " + EIMResource.getMessage("EIM.INFO.USER.IMPORT.START"));

			// 初期処理
			init();

			// インポート対象のファイルパスを取得
			String importFilePath = EIMConfig.get("IMPORT_USER_FILE");
			Path orgFile = Paths.get(importFilePath);

			if (!Files.exists(orgFile)){
				throw new Exception(EIMResource.getMessage("EIM.ERROR.LOGIC.NOT.EXSIST.IMPORT.FILE", new Object[]{importFilePath}));
			}

			String tempImportFileDir = EIMConfig.get("TEMP_UPLOAD_DIR");
			String cpFileName = tempImportFileDir + orgFile.getFileName() + System.currentTimeMillis() + ".tmp";
			Path copyFile = Paths.get(cpFileName);

			// 一時ファイルを生成(AdminUserService#importUser内でパラメータのファイルを削除するため)
			try{
				Files.copy(orgFile, copyFile);
			}catch(IOException e){
				throw new Exception(EIMResource.getMessage("EIM.ERROR.LOGIC.FAILED.FILE.COPY"));
			}

			AdminUserService adminUserService = (AdminUserService)context.getBean("adminUserService");

			// インポート処理を呼び出し
			String errorMessage = adminUserService.importUser(cpFileName);

			if(!StringUtils.isBlank(errorMessage)) {
				String[] spErrorMessage = errorMessage.split("\r");
				for(int i = 0; i < spErrorMessage.length; i++) {
					log.error(spErrorMessage[i]);
				}
			} else {
				log.info(" " + EIMResource.getMessage("EIM.INFO.USER.IMPORT.COMPLETED"));
			}

			sess.commit();

		} catch(EIMException eime) {
			log.error(AppMessageUtils.makeLogMessage(1,eime.getMessage()), eime);
			try{
				// ロールバック
				if(sess != null){
					sess.rollback();
				}

			} catch (Exception se) {
				log.error(AppMessageUtils.makeLogMessage(1,se.getMessage()), se);

			}

		} catch (Exception e) {
			log.error(AppMessageUtils.makeLogMessage(1,e.getMessage()), e);
			try{
				// ロールバック
				if(sess != null){
					sess.rollback();
				}

			} catch (Exception se) {
				log.error(AppMessageUtils.makeLogMessage(1,se.getMessage()), se);

			}

		} finally {
			try{
				log.info(" " + EIMResource.getMessage("EIM.INFO.USER.IMPORT.END"));
				// セッションクローズ
				EIMThreadContext.removeEIMSession();
				if(sess != null){
					sess.close();
				}

			} catch (Exception se) {
				log.error(AppMessageUtils.makeLogMessage(1,se.getMessage()), se);

			}
		}

	}

	/**
	 * 初期処理
	 * @throws Exception 例外
	 */
	private static void init() throws Exception{

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
		context = ApplicationContextLoader.getContext();
		DataSource ds = (DataSource)context.getBean("dataSource");

		sess = new EIMSession(user,lang);
		sess.setConnection(ds.getConnection());
		sess.setConsoleMode(true);
		sess.getDBConnection().setAutoCommit(false);
		EIMThreadContext.putEIMSession(sess);

		// トランザクション取得
		TransactionContext tc = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
		EIMThreadContext.putTransactionContext(tc);
		tc.setLangId(lang);
		tc.setDBConnection(sess.getDBConnection());

		UserService userService = (UserService)context.getBean("userService2");

		UserDomain sessUser = userService.getById(1);
		tc.setUser(sessUser);
	}
}
