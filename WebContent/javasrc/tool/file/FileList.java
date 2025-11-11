package tool.file;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;

/**
 * ファイルリスト情報クラス
 */
public class FileList {

	/**
	 * 個別のファイル情報を格納するクラス
	 */
	private class FileInfo{

		/**
		 * コンストラクタ
		 * 
		 * @param pFile ファイル情報
		 * @param pPath パス名
		 * @param pFileName ファイル名
		 * @param pFileSize ファイルサイズ
		 * @param pLastModified 最終更新日
		 * @param pMD5Sum MD5チェックサム値
		 */
		public FileInfo(File pFile, String pPath, String pFileName, long pFileSize, long pLastModified , String pMD5Sum){
			file = pFile;
			path = pPath;
			fileName = pFileName;
			fileSize = pFileSize;
			lastModified = pLastModified;
			MD5sum = pMD5Sum;
		}
		
		/**
		 * ファイル情報
		 */
		public File file;

		/**
		 * パス名
		 */
		public String path;

		/**
		 * ファイル名
		 */
		public String fileName;

		/**
		 * ファイルサイズ
		 */
		public long fileSize;

		/**
		 * 最終更新日時
		 */
		public long lastModified;

		/**
		 * MD5チェックサム値
		 */
		public String MD5sum;
	}

	// ファイル一覧(FileInfoの配列)
	private List fileList = new ArrayList();
	
	/**
	 * コンストラクタ
	 * 
	 * @param rootDir 最上位ディレクトリのファイル情報
	 */
	public FileList(File rootDir){
		
		// ルートディレクトリ直下のファイル一覧の生成
		//   ファイル読込不可、ディレクトリ読込不可の例外返却
		RecurrentGetFileInfo(rootDir);
		
		// MD5チェックサム値の計算
		//   ファイル読込不可の例外返却
		this.md5sum();
		
		// ソートの実行
		this.sort();
	}
	
	/**
	 * ディレクトリ以下のファイル情報を再帰的に取得します。
	 * 
	 * @param dir
	 */
	private void RecurrentGetFileInfo(File dir){

		// ディレクトリ直下のファイル一覧の取得
		File[] files = dir.listFiles();

		// ファイル数分ループ
		for (int i = 0; i < files.length; i++) {

			File file = files[i];
			
			// 読み込めないファイルがある場合
			if(!file.canRead()) {
				String errorMessage = "";
				if(file.isDirectory()){
					errorMessage = Constants.ERR_CANNOT_READ_DIR_EXIST;
				}
				else{
					errorMessage = Constants.ERR_CANNOT_READ_FILE_EXIST;
				}
				throw new RuntimeException(errorMessage);
			}
			
			// ディレクトリの場合、再帰的に配下のファイル情報を取得
			if(file.isDirectory()){
				RecurrentGetFileInfo(file);
			}

			// ファイル情報の保存
			else {
				// リストにファイル情報を保存
				FileInfo fileInfo = new FileInfo(
						file,
						file.getParent(),
						file.getName(),
						file.length(),
						file.lastModified(),
						null);
				fileList.add(fileInfo);
			}
		}
	}
	
	/**
	 * MD5チェックサム値を求めます
	 */
	private void md5sum(){
		
		for(int i = 0 ; i < fileList.size(); i++){
			
			// MD5チェックサム値の取得
			FileInfo fileInfo = (FileInfo)fileList.get(i);
			try {
				fileInfo.MD5sum = MD5Sum.getMD5Sum(fileInfo.file);
			} catch(Exception e) {
				// 「読み込めないファイルが存在します。」
				throw new RuntimeException(Constants.ERR_CANNOT_READ_FILE_EXIST);
			}
		}
		
	}
	
	/**
	 * ファイル一覧をパス名、ファイル名の順でソートします。
	 */ 
	private void sort(){

		// ソートする
		Collections.sort(fileList, new Comparator(){

			/**
			 * ソート用の比較メソッド
			 * このメソッドはCollections.sortの内部で呼ばれることを前提としています。
			 * 
			 * @param o1 比較対象のオブジェクト1
			 * @param o2 比較対象のオブジェクト2
			 * @return o1が大きい場合は1,o2が大きい場合は-1,その他は0
			 */
			public int compare(Object o1, Object o2) {
				
				FileInfo fi1 = (FileInfo)o1;
				FileInfo fi2 = (FileInfo)o2;
				
				// パス名で比較
				if(fi1.path.compareTo(fi2.path) == 0){
					
					// パス名が同一の場合はファイル名で比較
					return fi1.fileName.compareTo(fi2.fileName);
				}	
				
				return fi1.path.compareTo(fi2.path);
			}			
		});
		
	}
	
	/**
	 * ファイル一覧をファイル出力します。
	 * 
	 * @param outputFile 出力ファイル情報
	 * @param encoding 出力ファイルのエンコード
	 */
	public void output(File outputFile, String encoding) {

		// ライターの生成
		BufferedWriter writer = getWriter(outputFile, encoding);
		
		// フォーマッタの生成
		SimpleDateFormat formatter = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");

		for(int i = 0; i < fileList.size() ; i++){
			// ファイル情報の取得
			FileInfo fileInfo = (FileInfo)fileList.get(i);

			// 最終更新日を文字列に変換
			String lastModifiedStr = formatter.format(new Date(fileInfo.lastModified));
			
			try {
				// ファイル出力
				writer.write(fileInfo.path);
				writer.write("\t");
				writer.write(fileInfo.fileName);
				writer.write("\t");
				writer.write(String.valueOf(fileInfo.fileSize));
				writer.write("\t");
				writer.write(lastModifiedStr);
				writer.write("\t");
				writer.write(fileInfo.MD5sum);
				writer.newLine();
			} catch (Exception e) {
				
				// 出力ファイルを削除しておく
				if(outputFile.exists()){
					try{
						outputFile.delete();
					} catch (Exception e2) {
						// 無視する
					}
				}
				
				// 「出力ファイルに書き込めません。」
				throw new RuntimeException(Constants.ERR_CANNOT_WRITE_FILE);
			}
		}
		
		// ライターの削除
		try {
			writer.flush();
			writer.close();
		} catch (Exception e) {
			// 「出力ファイルに書き込めません。」
			throw new RuntimeException(Constants.ERR_CANNOT_WRITE_FILE);
		}
	}

	/**
	 * ファイル出力用のライター取得メソッド
	 * 
	 * @param outputFile 出力ファイル情報
	 * @param encoding 出力ファイルのエンコード
	 * @return ファイル出力用のライター
	 */
	private static BufferedWriter getWriter(File outputFile, String encoding){

		FileOutputStream fostrm = null;
		OutputStreamWriter ostrWriter = null;
		BufferedWriter writer = null;
		
		try {
			fostrm = new FileOutputStream(outputFile);
		} catch (Exception e) {
			// 「出力ファイルに書き込めません。」
			throw new RuntimeException(Constants.ERR_CANNOT_WRITE_FILE);
		}
		
		try {
			ostrWriter = new OutputStreamWriter(fostrm,encoding);
		} catch (Exception e) {
			// 「出力ファイルに書き込めません。」
			throw new RuntimeException(Constants.ERR_CANNOT_WRITE_FILE);
		}
		
		try {
			writer = new BufferedWriter(ostrWriter);
		} catch (Exception e) {
			// 「出力ファイルに書き込めません。」
			throw new RuntimeException(Constants.ERR_CANNOT_WRITE_FILE);
		}
		
		return writer;
	}
}
