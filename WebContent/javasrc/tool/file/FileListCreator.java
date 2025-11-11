/**
 * 
 */
package tool.file;

/**
 * ファイル一覧生成クラス。
 * 本クラスは、コンソールから直接起動されることを想定しています。<br/>
 * 起動引数は以下の通りです。
 * <li>-d <i>リストするディレクトリパス(フルパス)</i>
 * <li>-f <i>出力するリストファイルのファイル名</i>
 */
public class FileListCreator {

	/**
	 * 出力ファイルのエンコーディング
	 */
	private static final String OUTPUT_FILE_ENCODING = "UTF-8";
	
	/**
	 * ファイル一覧生成のメインメソッド
	 * 
	 * @param args 起動引数
	 */
	public static void main(String[] args) {

		// このアプリケーションの起動情報
		FileListCreatorInfo info = null;
		
		// ファイル一覧情報
		FileList fileList = null;

		try{
			// 起動引数からパラメータの生成
			info = new FileListCreatorInfo(args);
			
			// ファイルリストの生成
			fileList = new FileList(info.getDirectory());
			
			// ファイルリストの出力
			fileList.output(info.getOutputFile(),OUTPUT_FILE_ENCODING);

		} catch (Exception e) {
			// エラーメッセージを出力して強制終了
			System.out.println(e.getMessage());
			System.exit(1);
		}

		return;
	}
}
