package jp.co.ctc_g.eim.app.document.presentation.batch;

import java.io.File;
import java.lang.reflect.Method;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.file.FileSystemException;
import java.nio.file.StandardOpenOption;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import eim.bo.EIMResource;
import eim.util.EIMConfig;

/**
 * フォルダを監視し、ファイルの追加に従い別のクラスを呼び出します。<br>
 */
public class FileCreateWatcher {
	
	/** ログ */
	private static Log log = LogFactory.getLog(FileCreateWatcher.class);
	
	/** ファイルコピー完了チェック間隔（ミリ秒） */
	private static long waitMS = 500;

	/** 監視停止ファイル名 */
	private static final String STOP_FILENAME = "stop.txt";
	
	/**
	 * 設定ファイルで定義されたディレクトリを監視します。<br>
	 * 配置されたファイルを検索し、引数で指定されたクラスを呼び出します。<br>
	 * <br>
	 * @param args 呼び出されるクラス名称
	 * @throws Exception 例外発生 
	 * @since Ver 6.6 
	 */
	public static void main(String[] args) throws Exception {

		// 開始ログ
		log.info(EIMResource.getMessage("EIM.INFO.LOGIC.FILE.CREATE.WATCHER.START"));
		
		String className = args[0];

		// 設定ファイルの監視対象ディレクトリのファイルオブジェクトを取得
		String path = EIMConfig.getValue("FILE_CREATE_WATCHER_DIR");
		File dirObj = new File(path);

		// ディレクトリが空の場合、後続の処理を行わない
		if (dirObj == null || dirObj.listFiles() == null || dirObj.listFiles().length == 0) {
			// 終了ログ
			log.info(EIMResource.getMessage("EIM.INFO.LOGIC.FILE.CREATE.WATCHER.END"));
			System.exit(0);
		}
		
		File[] fileList = dirObj.listFiles();

		File lockFile = new File(path + "/" + STOP_FILENAME);
		// ロックファイルがある場合、終了
		if (lockFile.exists()) {
			// 終了ログ
			log.info(EIMResource.getMessage("EIM.INFO.LOGIC.FILE.CREATE.WATCHER.END"));
			System.exit(0);

		// ロックファイルがない場合、作成
		} else {
			lockFile.createNewFile();
		}
		
		try {
			// オブジェクト数ループ
			for (File file : fileList) {
				// ファイルに対してのみ処理を行う
				if (file.isFile()) {
					
					try {
						String fileFullPath = file.getAbsolutePath();
						// コピー完了を待つ
						if (waitCopyComplete(fileFullPath)) {
							Class<?> clazz = Class.forName(className);
							Method method = clazz.getMethod("exec",  new Class[] {String[].class});
							String[] arguments = {fileFullPath};
							// ファイル名ログ出力 
							log.info(EIMResource.getMessage("EIM.INFO.LOGIC.FILE.CREATE.WATCHER.FILENAME", arguments));
							// メイン処理実行
							method.invoke(null, (Object)arguments);
						}
					} catch (Exception e) {
						// 何もしない
					}
				}
			}
		} finally {
			// ロックファイル削除
			lockFile.delete();

			// 終了ログ
			log.info(EIMResource.getMessage("EIM.INFO.LOGIC.FILE.CREATE.WATCHER.END"));
		}
		System.exit(0);
	}

	/**
	 * コピー完了を待ちます。
	 * ファイルをロックできるまでコピー中とみなします。
	 * 
	 * @param pathname ファイルパス
	 * @return コピー完了フラグ
	 */
	private static boolean waitCopyComplete(String pathname) {
		boolean isCopyComplete = false;
		File file = new File(pathname);
		while (true) {
			// ファイルの存在チェック(改名、削除時の無限ループ回避)
			if (!(new File(pathname)).exists()) {
				// ファイルがなくなっている場合、ループを抜ける
				break;
			}
			try (FileChannel fc = FileChannel.open(file.toPath(), StandardOpenOption.WRITE);
					FileLock lock = fc.tryLock()) {
				if (lock == null) {
					// コピー中
					Thread.sleep(waitMS);
				} else {
					// コピー完了
					isCopyComplete = true;
					break;
				}
			} catch (FileSystemException e) {
				// 何もしない
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
		return isCopyComplete;
	}
}
