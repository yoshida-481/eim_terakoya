package common.util;

import java.io.File;

import eim.util.FTPUtils;

/**
 * OCR用処理用FTP/SFTPユーティリティクラス.
 */
public class OcrFtpUtil {
	
	/** OCR用FTP/SFTPユーティリティクラスインスタンス */
	private static OcrFtpUtil instance = null;
	
	/**
	 * OCR用FTP/SFTPユーティリティクラスインスタンス取得.<br/>
	 * @return 同期化FTP/SFTPユーティリティクラスインスタンス
	 */
	public static OcrFtpUtil getInstance(){
		synchronized(OcrFtpUtil.class) {
			if(instance == null){
				instance = new OcrFtpUtil();
			}
		}
		return instance;
	}
	
	/**
	 * SFTPまたはFTPでリモートファイルをダウンロードする.<br/>
	 * SFTPを利用する場合は、config.propertiesのキー「SFTP_USE」に「true」または「True」を指定してください.<br/>
	 * 上記以外の指定は全てFTPを利用することになります.<br/>
	 * FTPの転送モードは「バイナリモード」固定となります.<br/>
	 * ポートは内部で下記表の通りに指定されます.<br/>
	 * SFTP利用の場合(SFTP_USE=True)、FTPUtils.getFile()はスレッドセーフでないため、排他制御をする。
	 * <table border="1">
	 *   <tr><th>File Systems</th><th>ポート</th></tr>
	 *   <tr><th>FTP</th><th>21</th></tr>
	 *   <tr><th>SFTP</th><th>22</th></tr>
	 * </table>
	 * 
	 * @param host ホスト名
	 * @param user リモートログインユーザー
	 * @param pass リモートログインパスワード
	 * @param serverFile サーバファイル
	 * @param localFile ローカルファイル
	 * @throws Exception 例外
	 */
	 public void getFile(String host, 
			                   String user, 
                               String pass, 
                               File serverFile, 
                               File localFile) throws Exception {
		
		 // SFTP利用の場合(SFTP_USE=True)、FTPUtils.getFile()はスレッドセーフでないため、排他制御をする。
		synchronized(OcrFtpUtil.class){
			FTPUtils.getFile(host, user, pass, serverFile, localFile);
		}
	}
	
	/**
	 * SFTPまたはFTPでファイルをアップロードする.<br/>
	 * SFTPを利用する場合は、config.propertiesのキー「SFTP_USE」に「true」または「True」を指定してください.<br/>
	 * 上記以外の指定は全てFTPを利用することになります.<br/>
	 * FTPの転送モードは「バイナリモード」固定となります.<br/>
	 * ポートは内部で下記表の通りに指定されます.<br/>
	 * SFTP利用の場合(SFTP_USE=True)、FTPUtils.putFile()はスレッドセーフでないため、排他制御をする。
	 * <table border="1">
	 *   <tr><th>File Systems</th><th>ポート</th></tr>
	 *   <tr><th>FTP</th><th>21</th></tr>
	 *   <tr><th>SFTP</th><th>22</th></tr>
	 * </table>
	 * 
	 * @param host ホスト名
	 * @param user リモートログインユーザー
	 * @param pass リモートログインパスワード
	 * @param localFile ローカルファイル
	 * @param serverFile サーバファイル
	 * @throws Exception 例外
	 */
	public void putFile(String host, 
			                   String user, 
			                   String pass, 
			                   File localFile, 
			                   File serverFile) throws Exception {
		
		 // SFTP利用の場合(SFTP_USE=True)、FTPUtils.putFile()はスレッドセーフでないため、排他制御をする。
		synchronized(OcrFtpUtil.class){
			FTPUtils.putFile(host, user, pass, localFile, serverFile);
		}
	}
	
	/**
	 * SFTPまたはFTPでファイルを削除する.<br>
	 * 削除対象のファイルが存在しない場合でも、エラーは発生しません.<br>
	 * SFTPを利用する場合は、config.propertiesのキー「SFTP_USE」に「true」または「True」を指定してください.<br>
	 * 上記以外の指定は全てFTPを利用することになります.<br>
	 * ポートは内部で下記表の通りに指定されます.<br>
	 * <table border="1">
	 *   <caption></caption>
	 *   <tr><th>File Systems</th><th>ポート</th></tr>
	 *   <tr><th>FTP</th><th>21</th></tr>
	 *   <tr><th>SFTP</th><th>22</th></tr>
	 * </table>
	 *
	 * @param host ホスト名
	 * @param user リモートログインユーザー
	 * @param pass リモートログインパスワード
	 * @param serverFile サーバファイル
	 * @throws Exception
	 */
	
	public void deleteFile(String host,
				String user,
				String pass,
				File serverFile) throws Exception {

		// SFTP利用の場合(SFTP_USE=True)、FTPUtils.putFile()はスレッドセーフでないため、排他制御をする。
		synchronized(OcrFtpUtil.class){
			FTPUtils.deleteFile(host, user, pass, serverFile);
		}
	}

}
