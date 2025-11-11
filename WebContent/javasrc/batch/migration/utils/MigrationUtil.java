package batch.migration.utils;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.HashMap;

import javax.sql.DataSource;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.poi.hpsf.PropertySetFactory;
import org.apache.poi.hpsf.SummaryInformation;
import org.apache.poi.ooxml.POIXMLProperties;
import org.apache.poi.openxml4j.opc.OPCPackage;
import org.apache.poi.poifs.eventfilesystem.POIFSReader;
import org.apache.poi.poifs.eventfilesystem.POIFSReaderEvent;
import org.apache.poi.poifs.eventfilesystem.POIFSReaderListener;

import eim.bo.EIMException;
import eim.bo.EIMUser;
import eim.net.EIMSession;
import jp.co.ctc_g.eim.app.document.common.util.AppDocumentUtil;
import jp.co.ctc_g.eim.app.document.common.util.StringUtils;
import jp.co.ctc_g.eim.framework.common.util.EIMMessageUtils;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;

public class MigrationUtil {

	private static Log log = LogFactory.getLog(MigrationUtil.class);

	private static SummaryInformation summaryInfo = null;

	/** 移行可否 判定結果返却値 */
	public static enum migrationJudgeResult{
		migratable("移行可能"),
		exclusionExtensionFile("移行対象の拡張子ではありません"),
		validateNameFailed("Windows禁止文字が使われています"),
		overfileSize("ファイルサイズが上限を超えています"),
		noFileSize("ファイルサイズが0です"),
		hiddenFile("隠しファイルです"),
		cannotReadFile("読み取り権限の無いファイルです"),
		exclusionFolder("移行対象外フォルダです"),
		configFileError("設定ファイルの読み取りエラーです"),
		noFileInFolder("フォルダ内にファイルがありません"),
		overFileNumberInFolder("フォルダ内のファイル数が上限を超えているフォルダです"),
		cannotReadFolder("読み取り権限の無いフォルダです"),
		hiddenFolder("隠しフォルダです"),
		failed("失敗"),
		noExtensionFile("拡張子の無いファイルです");

		private String message;

		migrationJudgeResult(String message) { this.message = message; }

        public String getMessage() { return message; }

        public void setMessage(String message){ this.message = message; }

	}

	/** ファイルが除外対象かどうか判定
	 * @throws EIMException
	 */
	public static migrationJudgeResult isMigrationTargetFile(File file) {
		//除外条件チェック
		try {
			//読み取り権限判定
			if (!file.canRead()) {
				return migrationJudgeResult.cannotReadFile;
			}
			//拡張子判定
			//移行対象拡張子リスト取得
			String migrationTargetExtensionList = ConfigUtils.getByKey("MIGRATION_TARGET_EXTENSION");
			if (migrationTargetExtensionList != null && migrationTargetExtensionList.length() != 0) {
				String[] migrationTargetExtensionSplitedList = migrationTargetExtensionList.split(",");
				//ファイルの拡張子取得
				String extension = StringUtils.getFileExt(file.getName());

				if (extension == null || extension == "") {
					if ("0".equals(ConfigUtils.getByKey("MIGRATION_NO_EXTENSION_PERMIT_FLAG"))){
						return migrationJudgeResult.noExtensionFile;
					}
				} else {
					int checkExtensionCount = 0;
					extension = extension.replace(".","");
					for (String migrationTargetExtension : migrationTargetExtensionSplitedList) {
						if (extension.equals(migrationTargetExtension)
							|| extension.equals(migrationTargetExtension.toUpperCase())
						) {
							break;
						}
						checkExtensionCount++;
					}

						//移行対象拡張子リストと一致しなかった場合、移行しない
					if (checkExtensionCount == migrationTargetExtensionSplitedList.length) {
						return migrationJudgeResult.exclusionExtensionFile;
					}
				}
			}
			// Windows禁止文字チェック
			try {
				AppDocumentUtil.checkValidateFName(file.getName());
			} catch (Exception e) {
				return migrationJudgeResult.validateNameFailed;
			}

			//ファイルサイズ判定(0Byte)
			if (file.length() == 0) {
				return migrationJudgeResult.noFileSize;
			}
			//ファイルサイズ判定(最大ファイルサイズ)
			long maxFileSize = Long.parseLong(ConfigUtils.getByKey("UPLOAD_FILE_SIZE_MAX"));
			if (maxFileSize != 0 && file.length() > maxFileSize) {
				return migrationJudgeResult.overfileSize;
			}
			//隠しファイル判定
			if (file.isHidden()) {
				return migrationJudgeResult.hiddenFile;
			}

		} catch(EIMException e) {
			log.error(EIMMessageUtils.makeLogMessage(1,e.getMessage()), e);
			return migrationJudgeResult.configFileError;

		} catch (Exception e1) {
			e1.printStackTrace();
			return migrationJudgeResult.configFileError;
		}

		// 除外条件を満たさないファイルの場合、移行する
		return migrationJudgeResult.migratable;
	}

	/**
	 * フォルダが除外対象かどうか判定
	 * @throws EIMException
	 */
	public static migrationJudgeResult isMigrationTargetFolder(File folder) throws Exception {
		try {
			// 除外条件チェック

			// 読み取り権限判定
			if (!folder.canRead()) {
				return migrationJudgeResult.cannotReadFolder;
			}

			// 隠しフォルダ判定
			if (folder.isHidden()) {
				return migrationJudgeResult.hiddenFolder;
			}

			// 設定ファイルから、移行対象外フォルダリスト生成
			String exclusionFolderList = ConfigUtils.getByKey("MIGRATION_EXCLUSION_FOLDER_PATH");
			if (exclusionFolderList != null && exclusionFolderList.length() != 0) {
				String[] exclusionFolderSplitedList = exclusionFolderList.split(",");

				// フォルダのパスが移行対象外フォルダリストと一致している場合、移行しない
				String rootFolderPath = ConfigUtils.getByKey("MIGRATION_TARGET_PATH");
				for (String exclusionFolder : exclusionFolderSplitedList) {
					String exclusionFolderAbsolutePath = rootFolderPath + exclusionFolder;
					if (folder.getAbsolutePath().replace("\\", "/").equals(exclusionFolderAbsolutePath)) {
						return migrationJudgeResult.exclusionFolder;
					}
				}
			}
			// フォルダ内のドキュメント数判定
			File[] files = folder.listFiles();
				//配下にフォルダ・ファイルが無い場合、移行しない
			if (files == null) {
				return migrationJudgeResult.noFileInFolder;
			}
			// フォルダ内のドキュメント数+ファイル数が設定値以上の場合、移行しない
			int fileNumberInFolder = 0;
			for (File file : files) {
				if (file.isFile()) {
					fileNumberInFolder++;
				}
			}
			String maxFileNumber = ConfigUtils.getByKey("MIGRATION_TARGET_MAX_FILE_NUMBER");
			int maxFileNumberInFolder = Integer.parseInt(maxFileNumber);
			if (fileNumberInFolder > maxFileNumberInFolder) {
				return migrationJudgeResult.overFileNumberInFolder;
			}
		} catch(EIMException e) {
			return migrationJudgeResult.configFileError;
		}

		// 除外条件を満たさないフォルダの場合、移行する
		return migrationJudgeResult.migratable;
	}

	/**
	* Microsoft系ファイルの作成者情報を取得する
	* 作成者情報が取得できない場合、空文字を返却する
	*
	* @param file ファイル
	* @return creater 作成者
	*/
	public static String getMicrosoftFileCreater(File file) {
		String creater = "";
		if (file == null) {
			return creater;
		}
		String fileName = file.getName();
		String filePath = file.getPath();
		FileInputStream fs = null;

		try{
			fs = new FileInputStream(filePath);
			String ext = StringUtils.getFileExt(fileName).replace(".", "");
			HashMap<String,String> before2007extMap = getBefore2007ExtMap();
			if (before2007extMap.containsKey(ext) || before2007extMap.containsKey(ext.toUpperCase())) {
				// Office 2007以前の拡張子の場合(大文字もチェックする)
				POIFSReader poiReader = new POIFSReader();
				poiReader.registerListener(new IPOIFSReaderListener(),"\005SummaryInformation");
				poiReader.read(fs);
				// 作成者取得
				creater = summaryInfo.getAuthor();
			} else {
				// その他(Office2007以降含む)
				OPCPackage pkg = OPCPackage.open(fs);
				POIXMLProperties props = new POIXMLProperties(pkg);
				// 作成者取得
				creater = props.getCoreProperties().getCreator();
			}
		} catch (Exception e) {
			// ログを出力し、処理を続行する
			log.info(file.getPath() + "に作成者情報が存在しません。");

		} finally {
			try {
				fs.close();
			} catch (IOException e1) {
				e1.printStackTrace();
			}
		}

		return creater;
	}

	/**
	 * セッション取得
	 *
	 * @return セッション
	 * @throws Exception 例外
	 */
	public static EIMSession getSession() throws Exception {

		// user取得
		EIMUser user = new EIMUser(1, null, null, null, null, null, 255, 0, null);

		// lang取得
		String lang = "";
		String EIM_CONFIG_LANG = "MESSAGELANG";
		String DEFAULT_LANG	= "JA";
		if (ConfigUtils.getByKey(EIM_CONFIG_LANG) != null) {
			lang = ConfigUtils.getByKey(EIM_CONFIG_LANG);
		}else{
			lang = DEFAULT_LANG;
		}

		// データソース取得
		org.springframework.context.ApplicationContext context = jp.co.ctc_g.eim.framework.common.util.ApplicationContextLoader.getContext();
		DataSource ds = (DataSource)context.getBean("dataSource");

		// セッション取得
		EIMSession sess = new EIMSession(user,lang);
		sess.setConnection(ds.getConnection());
		sess.setConsoleMode(true);
		sess.getDBConnection().setAutoCommit(false);
		EIMThreadContext.putEIMSession(sess);

		return sess;
	}

	/**
	 * 2007以前の拡張子を設定ファイルから取得する
	 */
	private static HashMap<String, String> getBefore2007ExtMap() throws Exception {
		HashMap<String, String> before2007extMap = new HashMap<String, String>();
		String extStr = ConfigUtils.getByKey("MIGRATION_BEFORE_OFFICE2007_EXT_LIST");
		String[] exts = extStr.split(",");

		for (String ext : exts) {
			before2007extMap.put(ext, ext);
		}
		return before2007extMap;
	}

	/**
	 * IPOIFSReaderListenerクラス
	 */
	private static class IPOIFSReaderListener implements POIFSReaderListener {

		public void processPOIFSReaderEvent(POIFSReaderEvent event) {
			try {
				summaryInfo = (SummaryInformation)PropertySetFactory.create(event.getStream());
			} catch (Exception e) {
				throw new RuntimeException
				("Property set stream \"" + event.getPath() + event.getName() + "\": " + e);
			}
		}
	}
}
