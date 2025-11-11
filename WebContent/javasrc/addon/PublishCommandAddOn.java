package addon;

import jakarta.servlet.http.HttpServletRequest;
import java.util.Map;

import jp.co.ctc_g.eim.framework.business.domain.GuardConditionExecDomain;
import eim.bo.EIMObject;
import eim.net.EIMSession;

/**
 * 公開処理コマンドマクロ用アドオンインタフェース
 */
public interface PublishCommandAddOn{

	/**
	 * 設定済みの公開処理情報ラベルの取得メソッド
	 * 
	 * @param sess セッション情報
	 * @param wfpubObj ワークフロー公開処理オブジェクト
	 * 
	 * @return XML形式で設定したラベル情報<br>
	 * 			形式は<br>
	 * 			<setting label="ラベル情報"/>
	 * 
	 * @throws Exception DBアクセス時にエラーが発生した場合
	 */
	public String getPublishStatusLabel(EIMSession sess, EIMObject wfpubObj) throws Exception;
	
	/**
	 * 設定済みの公開処理情報の取得メソッド
	 * 
	 * @param sess セッション情報
	 * @param wfpubObj ワークフロー公開処理オブジェクト
	 * 
	 * @return XML形式で設定した公開処理情報<br>
	 * 			形式は<br>
	 * 			<macroSetting<br>
	 * 			 id="コマンドマクロのID"<br>
	 * 			 swf="SWFファイル名"<br>
	 * 			 コマンドごとのパラメータ="DB上に設定されているパラメータ値"<br>
	 * 							:<br>
	 * 							:<br>
	 * 			/>
	 * 
	 * @throws Exception DBアクセス時にエラーが発生した場合
	 */
	public String getPublishCommandSetting(EIMSession sess, EIMObject wfpubObj) throws Exception;

	/**
	 * 設定済みの公開処理情報の取得メソッド
	 * 
	 * @param sess セッション情報
	 * @param wfpubObj ワークフロー公開処理オブジェクト
	 * 
	 * @return Map形式で設定した公開処理情報<br>
	 * 			形式は<br>
	 * 				[<id>=[<swf>=xxx, <param1>=xxx, <param2>=xxx, ...]
	 * 
	 * 			id="コマンドマクロのID"<br>
	 * 			swf="SWFファイル名"<br>
	 * 			param1～=コマンドごとのパラメータ="DB上に設定されているパラメータ値"<br>
	 * 							:<br>
	 * 							:<br>
	 * 			/>
	 * 
	 * @throws Exception DBアクセス時にエラーが発生した場合
	 */
	public Map<String, Map<String, String>> getPublishCommandSettingList(EIMSession sess, EIMObject wfpubObj) throws Exception;

	/**
	 * 公開処理情報の設定メソッド
	 * 
	 * @param sess セッション情報
	 * @param request	更新リクエスト情報<br>
	 * 					SWFモジュールで設定したパラメータが設定されている
	 * @param wfpubObj ワークフロー公開処理オブジェクト
	 * 
	 * @throws Exception DBアクセス時にエラーが発生した場合
	 */
	public void updatePublishCommandSetting(EIMSession sess, HttpServletRequest request,  EIMObject wfpubObj) throws Exception;

	/**
	 * オンライン用公開処理実行メソッド
	 * 
	 * @param sess セッション情報
	 * @param object 処理対象オブジェクト
	 * 
	 * @throws Exception DBアクセス時にエラーが発生した場合
	 */
	public void doPublishCommandOnline(EIMSession sess, EIMObject object) throws Exception;
	
	/**
	 * バッチ用公開処理実行メソッド
	 * 
	 * @param sess セッション情報
	 * @param object 操作対象オブジェクト
	 * @return 正常終了:true、異常終了:false
	 * @throws Exception DBアクセス時にエラーが発生した場合
	 */
	public boolean doPublishCommandBatch(EIMSession sess, EIMObject object) throws Exception;
	
	/**
	 * 非同期終了かどうかを返却するメソッド
	 * 
	 * @param sess セッション情報
	 * @param object 処理対象オブジェクト
	 * @return 非同期終了ならtrueを返却
	 * @throws Exception DBアクセス時にエラーが発生した場合
	 */
	public boolean isNeedAsyncProcess(EIMSession sess, EIMObject object) throws Exception;

	/**
	 * 非同期終了かどうかを返却するメソッド(ガード条件ドメインを含む)
	 * 
	 * @param sess セッション情報
	 * @param object 処理対象オブジェクト
	 * @param guardConditionExecDomain ガード条件ドメイン
	 * @return 非同期終了ならtrueを返却
	 * @throws Exception DBアクセス時にエラーが発生した場合
	 */
	public boolean isNeedAsyncProcess(EIMSession sess, EIMObject object, GuardConditionExecDomain guardConditionExecDomain) throws Exception;
	
	/**
	 * 非同期処理実行メソッド
	 * 
	 * @param sess セッション情報
	 * @param object 処理対象オブジェクト
	 * @throws Exception DBアクセス時にエラーが発生した場合
	 */
	public void doAsyncProcess(EIMSession sess, EIMObject object) throws Exception;
	
}
