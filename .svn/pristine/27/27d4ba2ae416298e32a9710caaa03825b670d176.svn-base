/**
 * 
 */
package jp.co.ctc_g.eim.app.document.business.service;

import java.io.File;

/**
 * 
 * PDF変換サービスインタフェース
 *
 */
public interface PdfConverterPlugin {
	
	/**
	 * 本プラグインが実行可能であるかを設定ファイルに対しチェックする
	 * 
	 * @return true 実行可能,false 実行不能
	 * @throws プラグイン使用可能情報取得時例外
	 */
	public boolean canUse() throws Exception;
	
	/**
	 * 本プラグインが変換可能なファイルかチェックする
	 * 
	 * @param file チェックファイル
	 * @return チェック結果
	 * @throws プラグイン対応ファイルチェック時例外
	 */
	public boolean canSupported(File file) throws Exception;
	
	/**
	 * 実行する外部コマンドラインを取得
	 * 
	 * @return コマンドライン
	 * @throw 実行予定のコマンドライン取得時例外
	 */
	public String getExecCommandLine() throws Exception;
	
	/**
	 * 出力するオブジェクトIDを設定
	 * @param objectId オブジェクトID
	 * 
	 * @return 
	 * @throws Exception
	 */
	public void setObjectId(long objectId);
	
	/**
	 * 入力ファイルの設定
	 * 
	 * @param inputFile 入力ファイルの参照
	 * @throws 入力ファイル設定時例外
	 */
	public void setInputFile(File inputFile);
	
	/**
	 * 出力ファイルの設定
	 * 
	 * @param outputFile 出力ファイルの参照
	 * @throws 出力ファイル設定時例外
	 */
	public void setOutputFile(File outputFile);
	
	/**
	 * 外部コマンド実行
	 * 
	 * @return 実行結果成否(詳細は別メソッドで取得)
	 * @throws コマンド実行以外の、コマンド実行処理例外
	 */
	public boolean exec() throws Exception;
	
	/**
	 * 外部コマンド結果メッセージを取得
	 * 
	 * @return 実行結果メッセージ、成功時はnullか空文字
	 * @throw 外部コマンド結果取得時例外
	 */
	public String getMessage() throws Exception;

	/**
	 * 外部コマンド結果メッセージキーを取得(EIMResource用)
	 * 
	 * @return 実行結果メッセージキー、成功時はnullか空文字
	 * @throw 外部コマンド結果取得時例外
	 */
	public String getMessageKey() throws Exception;
}
