package tool.file;

import java.io.File;

/**
 * パラメータ値用クラス
 */	
public class FileListCreatorInfo {
	
	// 読込元のディレクトリ情報
	private File _dir = null;

	// 出力ファイル情報
	private File _outputFile = null;
	
	/**
	 * コンストラクタ
	 * 起動引数から、リストするディレクトリ情報と出力ファイル情報を取得します。
	 * 
	 * @param args 起動引数
	 */	
	public FileListCreatorInfo(String[] args) {
		
		for(int i = 0 ; i < args.length ; i++){
			// リストするディレクトリの取得
			if(args[i].equals("-d")){

				// 複数指定された場合はエラー
				if(_dir != null){
					// 「引数が正しく指定されていません。」
					throw new RuntimeException(Constants.ERR_INVALID_ARGS);
				}
				
				// パス名の取得
				i++;
				if(i >= args.length){
					// 「引数が正しく指定されていません。」
					throw new RuntimeException(Constants.ERR_INVALID_ARGS);
				}
				String path = args[i];
				
				// ディレクトリファイルの生成
				_dir = new File(path);
				
				// 存在チェック
				if(!_dir.exists()){
					// 「パスが存在ません。」
					throw new RuntimeException(Constants.ERR_PATH_NOT_FOUND);
				}
				// ディレクトリかチェック
				if(!_dir.isDirectory()){
					// 「パスが存在ません。」
					throw new RuntimeException(Constants.ERR_PATH_NOT_FOUND);
				}
				// 読込チェック
				if(!_dir.canRead()){
					// 「パスが読み込めません。」
					throw new RuntimeException(Constants.ERR_PATH_CANNOT_READ);
				}
			}
			// 出力ファイル名の取得
			else if(args[i].equals("-f")){

				// 複数指定された場合はエラー
				if(_outputFile != null){
					// 「引数が正しく指定されていません。」
					throw new RuntimeException(Constants.ERR_INVALID_ARGS);
				}
				
				// ファイル名の取得
				i++;
				if(i >= args.length){
					// 「引数が正しく指定されていません。」
					throw new RuntimeException(Constants.ERR_INVALID_ARGS);
				}
				String fileName = args[i]; 
				
				// 出力ファイル情報の生成
				_outputFile = new File(fileName);
				
				// 存在チェック
				if(_outputFile.exists()){
					// 「出力ファイルが存在します。」
					throw new RuntimeException(Constants.ERR_OUT_PUT_FILE_EXIST);
				}
			}
			// 識別子が異なる場合はエラー
			else{
				// 「引数が正しく指定されていません。」
				throw new RuntimeException(Constants.ERR_INVALID_ARGS);
			}
		}
		
		// 引数が足りていない場合はエラー
		if(_dir == null || _outputFile == null){
			// 「引数が正しく指定されていません。」
			throw new RuntimeException(Constants.ERR_INVALID_ARGS);
		}
	}
	
	/**
	 * パラメータに設定されたディレクトリのファイル情報を返却します。
	 * 
	 * @return ディレクトリを示すFileクラスのインスタンス
	 */	
	public File getDirectory(){
		return _dir;
	}

	/**
	 * パラメータに設定された出力ファイルのファイル情報を返却します。
	 * 
	 * @return ディレクトリを示すFileクラスのインスタンス
	 */	
	public File getOutputFile(){
		return _outputFile;
	}
}
