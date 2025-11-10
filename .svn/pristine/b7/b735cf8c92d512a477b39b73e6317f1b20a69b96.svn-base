package jp.co.ctc_g.eim.app.document.common.util;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import org.springframework.context.ApplicationContext;

import jp.co.ctc_g.eim.framework2.business.dao.FileDao;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.FileCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.entity.DirectoryDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.FileDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.FormatDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework2.common.exception.EIMException;
import jp.co.ctc_g.eim.framework2.integration.util.internal.FileUtil;

/**
 * 【ドキュメントAPI】
 * ファイル関連クラス
 */
public class FileUtils {

	/** エラー：ディレクトリ作成時の空き容量不足 */
	public final static int ERR_DIR_SPACE_OVER = 551;

	/**
	 * 指定したファイル内容を指定ファイルにコピーします。
	 *
	 * @param orgFile コピー元ファイル
	 * @param dstFile コピー先ファイル
	 * @throws Exception 例外
	 */
	public static void copyFile(File orgFile, File dstFile) throws Exception {
		FileInputStream from = null;
		FileOutputStream to = null;

		try {
			from = new FileInputStream(orgFile);
			to = new FileOutputStream(dstFile);
			byte[] buffer = new byte[4096];
			int bytes_read;
			while ((bytes_read = from.read(buffer)) != -1) {
				to.write(buffer, 0, bytes_read);
			}
		} finally {
			if (from != null) {
				try {
					from.close();
				} catch (IOException e) {
				}
			}
			if (to != null) {
				try {
					to.close();
				} catch (IOException e) {
				}
			}
		}
	}

	/**
	 * 指定したオブジェクトのチェックインを行います。<br>
	 * またチェックイン前に拡張子が変わる場合は変更前のファイルを削除します。
	 *
	 * @param object
	 * @param format
	 * @param fileName
	 * @param size
	 * @throws Exception
	 */
	public static void checkin(ObjectDomain object, FormatDomain format, String fileName, long size) throws Exception {

		ApplicationContext context = ApplicationContextLoader.getApplicationContext();
		FileDao fileDao = (FileDao) context.getBean("fileDao2");

		// Variables
		String fileExt = StringUtils.getFileExt(fileName);

		// 拡張子が変わる場合は変更前のファイルを削除
		FileDomain file = fileDao.getByObjectAndFormat(object, format);
		if (file != null && !StringUtils.nullToBlank(file.getExt()).equals(StringUtils.nullToBlank(fileExt))) {
			File target = new File(file.getDirectory().getPath() + FileUtil.getFileName(object, file));
			target.delete();
		}

		try {
			long dirId = 0;
			for (DirectoryDomain directory : format.getDirectoryList()) {
				if (directory.isOnline()) {
					dirId = directory.getId();
					break;
				}
			}

			// Domainを生成
			DirectoryDomain directoryDomain = new DirectoryDomain(dirId);
			FileDomain fileDomain = new FileDomain();
			fileDomain.setObject(object);
			fileDomain.setFormat(format);
			fileDomain.setDirectory(directoryDomain);
			fileDomain.setName(fileName);
			fileDomain.setSize(size);

			// 処理委譲する
			fileDao.create(object, fileDomain);

		} catch (jp.co.ctc_g.eim.framework2.common.exception.EIMException e) {
			// Daoの例外をUtilの例外に変換
			EIMException ee = new EIMException(e.getMessageKey());
			ee.initCause(e);
			throw ee;
		}
	}

	/**
	 * 指定されたパスにファイルをアップロードします。
	 *
	 * @param filePath ファイルのパス
	 * @param is
	 * @throws Exception
	 */
	public static long upload(String filePath, InputStream is) throws Exception {
		byte[] buff = new byte[4096];
		int len = 0;
		File file = new File(filePath);

		try {
			int total = 0;

			FileOutputStream fos = new FileOutputStream(file);
			while ((len = is.read(buff)) > 0) {
				fos.write(buff, 0, len);

				total += len;
			}
			is.close();
			fos.close();

		} catch (Exception e) {
			throw e;
		}
		return file.length();
	}

	/**
	 * ファイル継承。物理ファイルのコピーします。
	 *
	 * @param orgObj 継承元オブジェクト
	 * @param dstObj 継承先オブジェクト
	 * @throws Exception
	 */
	public static void inheritFile(ObjectDomain orgObj, ObjectDomain dstObj) throws Exception {
		ApplicationContext context = ApplicationContextLoader.getApplicationContext();
		FileDao fileDao = (FileDao) context.getBean("fileDao2");

		FileCriteria fileCriteria = new FileCriteria();
		fileCriteria.setObjectId(orgObj.getId());
		List<FileDomain> fileList = fileDao.getList(fileCriteria);

		if (fileList != null) {
			for (FileDomain file : fileList) {
				// Database
				checkin(dstObj, file.getFormat(), file.getName(), file.getSize());

				// Real
				File substance = new File(file.getDirectory().getPath() + FileUtil.getFileName(orgObj, file));
				File newSubstance = new File(file.getDirectory().getPath() + dstObj.getId() + file.getExt());
				copyFile(substance, newSubstance);
			}
		}
	}

	/**
	 *
	 * 指定したファイルのシンボリックリンクを作成します。
	 *
	 * @param orgFile リンク元ファイル
	 * @param dstFile リンク先ファイル
	 *
	 * @throws Exception 例外が発生した場合
	 *
	 */
	public static void createSymbolicLink(File orgFile, File dstFile) throws Exception {
		FileSystem fs = FileSystems.getDefault();

		boolean fileExist = Files.exists(fs.getPath(dstFile.getPath()));
		boolean isSymbolicLink = Files.isSymbolicLink(fs.getPath(dstFile.getPath()));

		String osName = System.getProperty("os.name").toLowerCase();

		int osAdminAuth = 0;
		if(osName.startsWith("windows")){
			//OSがWindowsの場合、OSの管理者権限を判定
			osAdminAuth = osAdminAuthCheck(osName);
		}

		if(fileExist && !isSymbolicLink){
			//実ファイルを削除
			dstFile.delete();
		}

		if(osAdminAuth == 1){
			//管理者権限を持たない場合
			copyFile(orgFile, dstFile);
		}else if(!fileExist || (fileExist && !isSymbolicLink)){
			//シンボリックリンクが存在しない、実ファイルが存在する場合、
			try{
				if(osName.startsWith("windows")){
					//Windowsの場合
					//リンク元ドキュメントの絶対パスを取得
					Path absoluteOrgPath = fs.getPath(orgFile.getAbsolutePath());
					Files.createSymbolicLink(fs.getPath(dstFile.getPath()), absoluteOrgPath);
				}else{
					//Linuxの場合
					Files.createSymbolicLink(fs.getPath(dstFile.getPath()), fs.getPath(orgFile.getPath()));
				}
			}catch(Exception e){
					throw e;
			}
		}
	}

	/**
	 *
	 * コマンド実行ユーザがOSの管理者権限であるかどうかを判定します。
	 * windows環境でopenfilesコマンドは、管理者権限でないと実行できない。
	 *
	 * @param osName OS名称
	 * @return openfilesコマンド実行結果(0：成功、1：失敗)
	 *                                   0: 管理者権限あり
	 *                                   1: 管理者権限なし
	 * @throws Exception 例外が発生した場合
	 *
	 */
	private static int osAdminAuthCheck(String osName) throws Exception{

		try {
			Runtime runtime = Runtime.getRuntime();
			Process process = runtime.exec("openfiles");
			int rcode = process.waitFor();
			return rcode;
		} catch (Exception e) {
			throw e;
		}
	}

}
