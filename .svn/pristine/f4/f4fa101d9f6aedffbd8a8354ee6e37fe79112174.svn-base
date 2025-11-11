package addon;

import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import jakarta.servlet.http.HttpServletRequest;

import common.util.AppObjectUtil;
import eim.bo.EIMAttribute;
import eim.bo.EIMAttributeType;
import eim.bo.EIMException;
import eim.bo.EIMObject;
import eim.bo.EIMResource;
import eim.net.EIMSession;
import eim.util.AttributeUtils;
import eim.util.EIMConfig;
import eim.util.EIMUtils;
import eim.util.ObjectAttributeUtils;
import eim.util.ObjectUtils;
import eim.util.StringUtils;
import jp.co.ctc_g.eim.framework.business.domain.GuardConditionExecDomain;

/**
 * 公開処理・有効期限設定用アドオン
 */
public class PublishCommandAddOnSetExpirationDate implements PublishCommandAddOn {

	private final String ID = "ctc_setExpirationDate";

	private final String SWF_MODULE_NAME = "CTCAddOnSetExpirationDateSetting";

	private final int doConvertOn = 1;

	private final int doConvertOff = 0;

	/**
	 * 有効期限設定処理の設定内容を取得してラベルに設定するメソッド
	 *
	 * @param sess セッション情報
	 * @param wfpubObj ワークフロー公開処理オブジェクト
	 *
	 * @return PDF変換処理の実施可否を示すラベルのXML
	 */
	public String getPublishStatusLabel(EIMSession sess, EIMObject wfpubObj){
		String outString = "";
		String label = "";
		EIMAttribute attNum = null;
		EIMAttribute attUnit = null;

		if(wfpubObj != null){
			// 属性「有効期限設定期間数字」の取得
			attNum = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_TERM_NUM"));

			// 属性「有効期限設定期間単位」の取得
			attUnit = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_TERM_UNIT"));
		}

		// 「有効期限設定期間数字」「有効期限設定期間単位」の両方設定済みの場合のみON
		if(attNum != null && attUnit != null){
			label = EIMResource.getMessage(sess, "EIM.CTC.SET.EXPIRATION.DATE.SETTING");
			label += String.valueOf(attNum.getInts()[0]);

			String termUnit = attUnit.getStrings()[0];

			if(termUnit.equals("years")){
				label += EIMResource.getMessage(sess, "EIM.CTC.SET.EXPIRATION.DATE.TERM.YEARS");
			}
			else if(termUnit.equals("months")){
				label += EIMResource.getMessage(sess, "EIM.CTC.SET.EXPIRATION.DATE.TERM.MONTHS");
			}
			else{
				label += EIMResource.getMessage(sess, "EIM.CTC.SET.EXPIRATION.DATE.TERM.DAYS");
			}
		}
		else{
			label = EIMResource.getMessage(sess, "EIM.CTC.SET.EXPIRATION.DATE.NOSETTING");
		}

		// 出力文字列の生成
		outString = "<setting";
		outString += " label=\"" + StringUtils.xmlEncode(label) + "\"";
		outString += " />";

		return outString;
	}

	/**
	 * 有効期限設定処理の設定情報を取得するメソッド
	 *
	 * @param sess セッション情報
	 * @param wfpubObj ワークフロー公開処理オブジェクト
	 *
	 * @return PDF変換処理の設定情報を示すXML
	 */
	public String getPublishCommandSetting(EIMSession sess, EIMObject wfpubObj){
		String outString = "";
		String enable = "false";
		String term = "";
		String termUnit = "";
		EIMAttribute attNum = null;
		EIMAttribute attUnit = null;

		if(wfpubObj != null){
			// 属性「有効期限設定期間数字」の取得
			attNum = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_TERM_NUM"));

			// 属性「有効期限設定期間単位」の取得
			attUnit = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_TERM_UNIT"));
		}

		// 「有効期限設定期間数字」「有効期限設定期間単位」の両方設定済みの場合のみON
		if(attNum != null && attUnit != null){
			enable = "true";
			term = String.valueOf(attNum.getInts()[0]);
			termUnit = attUnit.getStrings()[0];
		}

		outString = "<macroSetting";
		outString += " id=\"" + StringUtils.xmlEncode(this.ID) + "\"";
		outString += " swf=\"" + StringUtils.xmlEncode(this.SWF_MODULE_NAME) + "\"";
		outString += " enable=\"" + StringUtils.xmlEncode(enable) + "\"";
		outString += " term=\"" + StringUtils.xmlEncode(term) + "\"";
		outString += " termUnit=\"" + StringUtils.xmlEncode(termUnit) + "\"";
		outString += " />";

		return outString;
	}

	/**
	 * PDF署名処理の設定情報を取得するメソッド
	 * 
	 * @param sess セッション情報
	 * @param wfpubObj ワークフロー公開処理オブジェクト
	 * 
	 * @return Map形式 の公開処理情報<br>
	 */
	public Map<String, Map<String, String>> getPublishCommandSettingList(EIMSession sess, EIMObject wfpubObj) throws Exception {
		Map<String, java.util.Map<String, String>> map = new HashMap<String, java.util.Map<String, String>>();

		String enable = "false";
		String term = "";
		String termUnit = "";
		EIMAttribute attNum = null;
		EIMAttribute attUnit = null;

		if(wfpubObj != null){
			// 属性「有効期限設定期間数字」の取得
			attNum = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_TERM_NUM"));

			// 属性「有効期限設定期間単位」の取得
			attUnit = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_TERM_UNIT"));
		}

		// 「有効期限設定期間数字」「有効期限設定期間単位」の両方設定済みの場合のみON
		if(attNum != null && attUnit != null){
			enable = "true";
			term = String.valueOf(attNum.getInts()[0]);
			termUnit = attUnit.getStrings()[0];
		}

		// Mapに設定
		Map<String, String> paramMap = new HashMap<String, String>();
		
		paramMap.put("swf", StringUtils.xmlEncode(this.SWF_MODULE_NAME));
		paramMap.put("enable", StringUtils.xmlEncode(enable));
		paramMap.put("term", StringUtils.xmlEncode(term));
		paramMap.put("termUnit", StringUtils.xmlEncode(termUnit));
		
		map.put(StringUtils.xmlEncode(this.ID), paramMap);
		
		return map;
	} 

	/**
	 * 有効期限設定処理の設定更新メソッド
	 *
	 * @param sess セッション情報
	 * @param request 更新リクエスト情報
	 * @param wfpubObj ワークフロー公開処理オブジェクト
	 *
	 * @throws Exception	フレームワークで例外が発生した場合<br>
	 * 						予期せぬ例外が発生した場合
	 */
	public void updatePublishCommandSetting(EIMSession sess,
			HttpServletRequest request, EIMObject wfpubObj) throws Exception {

		//Parameter
		String prmEnable = EIMUtils.getParameter(request, "ctc_setExpirationDate_enabled");

		// パラメータチェック
		if(StringUtils.isBlank(prmEnable)){
			throw new EIMException(sess, "EIM.ERROR.SYSTEMERROR");
		}

		// 有効期限設定を行う場合
		if(prmEnable.equals("true")){
			// パラメータの取得
			String prmTermNum = EIMUtils.getParameter(request, "ctc_setExpirationDate_term");
			String prmTermUnit = EIMUtils.getParameter(request, "ctc_setExpirationDate_termUnit");

			// パラメータチェック
			if(StringUtils.isBlank(prmTermNum) || StringUtils.isBlank(prmTermUnit)){
				throw new EIMException(sess, "EIM.ERROR.SYSTEMERROR");
			}

			// 配列型に変換
			long[] termNumParam = {Integer.parseInt(prmTermNum)};
			String[] termUnitParam = {prmTermUnit};

			// 属性「有効期限設定期間数字」の設定
			AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_TERM_NUM"), termNumParam);
			// 属性「有効期限設定期間単位」の設定
			AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_TERM_UNIT"), termUnitParam);
		}
		// 有効期限設定を行わない場合
		else{
			// 属性「有効期限設定期間数字」の削除
			AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_TERM_NUM"), (long[])null);
			// 属性「有効期限設定期間単位」の削除
			AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_TERM_UNIT"), (String[])null);
		}

		return;
	}

	/**
	 * 有効期限設定処理のオンライン用公開処理実行メソッド
	 *
	 * @param sess セッション情報
	 * @param object 処理対象オブジェクト
	 *
	 * @throws Exception DBアクセス時にエラーが発生した場合
	 */
	public void doPublishCommandOnline(EIMSession sess, EIMObject object) throws Exception
	{

		int term = 0;
		String termUnit = "";
		EIMAttribute attNum = null;
		EIMAttribute attUnit = null;
		EIMAttribute attPDFFlag = null;

		// ワークフロー公開処理オブジェクトの取得
		EIMObject wfpubObj = AppObjectUtil.getWorkFlowProcessing(sess, object);
		if(wfpubObj == null){
			return;
		}

		// 属性「有効期限設定期間数字」の取得
		attNum = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_TERM_NUM"));

		// 属性「有効期限設定期間単位」の取得
		attUnit = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_TERM_UNIT"));


		// 属性「PDF変換実施フラグ」の取得
		attPDFFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_FLG"));


		// 「有効期限設定期間数字」「有効期限設定期間単位」のどちらかが未設定の場合処理対象外
		if(attNum == null || attUnit == null){
			return;
		}
		// 「PDF変換」チェック
		/* [08/01/19 modified by ik.]
		 * 以下の処理をコメントアウト。
		 * → PDF変換＝ONの場合でも有効期限設定を行うように修正
		 */
		/*
		if(attPDFFlag != null){
			int flagVal = attPDFFlag.getInts()[0];
			// 属性が存在し、かつ値がOFFの場合処理対象外
			if(flagVal != doConvertOff){
				return;
			}
		}
		*/

		// Calendarのadd使用のためintにキャスト
		term = (int) attNum.getInts()[0];
		termUnit = attUnit.getStrings()[0];

		Calendar cal = Calendar.getInstance();

		if (term == 0 || termUnit == null) {
			return;
		}

		if(termUnit.equals("years")){
			cal.add(Calendar.YEAR, term);
		}
		else if(termUnit.equals("months")){
			cal.add(Calendar.MONTH, term);
		}
		else{
			cal.add(Calendar.DATE, term);
		}

		//時間以下を0に初期化
		cal.set(cal.get(Calendar.YEAR), cal.get(Calendar.MONTH), cal.get(Calendar.DATE), 0, 0, 0);

		//DB登録用Date作成
		Date dateGMT = editExpirationDate(sess, cal.getTime());

		EIMObject parentObj = object;
		List allObj = AppObjectUtil.getChildEIMObjectRecurrently(sess, parentObj, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));
		allObj.add(parentObj);

		EIMAttributeType attType = AttributeUtils.getAttributeTypeByName(sess, "有効期限");

		for (int i = 0; i < allObj.size(); i++) {
			ObjectAttributeUtils.setAttribute(sess, (EIMObject)allObj.get(i), attType, dateGMT);
		}
	}

	/**
	 * 有効期限設定処理のバッチ用公開処理実行メソッド
	 *
	 * @param sess セッション情報
	 * @param object 操作対象オブジェクト
	 * @return 正常終了:true、異常終了:false
	 * @throws Exception DBアクセス時にエラーが発生した場合
	 */
	public boolean doPublishCommandBatch(EIMSession sess, EIMObject object) throws Exception
	{
		int term = 0;
		String termUnit = "";
		EIMAttribute attNum = null;
		EIMAttribute attUnit = null;

		// ワークフロー公開処理オブジェクトの取得
		EIMObject wfpubObj = AppObjectUtil.getObject(sess,
				EIMConfig.get("OBJECT_TYPE_NAME_WFPUB"), String.valueOf(object.getStatus().getType().getId()));

		if(wfpubObj == null){
			return false;
		}

		// 属性「有効期限設定期間数字」の取得
		attNum = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_TERM_NUM"));

		// 属性「有効期限設定期間単位」の取得
		attUnit = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_TERM_UNIT"));

		// 「有効期限設定期間数字」「有効期限設定期間単位」のどちらかが未設定の場合処理対象外
		if(attNum == null || attUnit == null){
			return true;
		}

		// Calendarのadd使用のためintにキャスト
		term = (int) attNum.getInts()[0];
		termUnit = attUnit.getStrings()[0];

		Calendar cal = Calendar.getInstance();

		if (term == 0 || termUnit == null) {
			return true;
		}

		if(termUnit.equals("years")){
			cal.add(Calendar.YEAR, term);
		}
		else if(termUnit.equals("months")){
			cal.add(Calendar.MONTH, term);
		}
		else{
			cal.add(Calendar.DATE, term);
		}

		//時間以下を0に初期化
		cal.set(cal.get(Calendar.YEAR), cal.get(Calendar.MONTH), cal.get(Calendar.DATE), 0, 0, 0);

		//DB登録用Date作成
		Date dateGMT = editExpirationDate(sess, cal.getTime());

		EIMObject parentObj = ObjectUtils.getObjectById(sess, object.getId());
		List allObj = AppObjectUtil.getChildEIMObjectRecurrently(sess,
				parentObj, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));
		allObj.add(parentObj);

		EIMAttributeType attType = AttributeUtils.getAttributeTypeByName(sess, "有効期限");

		for (int i = 0; i < allObj.size(); i++) {
			ObjectAttributeUtils.setAttribute(sess, (EIMObject)allObj.get(i), attType, dateGMT);
		}

		return true;
	}

	/**
	 * 有効期限設定処理が非同期終了かどうかを返却するメソッド<br>
	 * 必ず同期のためfalseを返却します
	 *
	 * @param sess セッション情報
	 * @param object 処理対象オブジェクト
	 * @return 非同期終了ならtrueを返却
	 *
	 * @throws Exception DBアクセス時にエラーが発生した場合
	 */
	public boolean isNeedAsyncProcess(EIMSession sess, EIMObject object) throws Exception
	{
		return isNeedAsyncProcess(sess, object, null);
	}

	/**
	 * 有効期限設定処理が非同期終了かどうかを返却するメソッド(承認不要WF)<br>
	 * 必ず同期のためfalseを返却します
	 *
	 * @param sess セッション情報
	 * @param object 処理対象オブジェクト
	 * @param guardConditionExecDomain ガード条件ドメイン
	 * @return 非同期終了ならtrueを返却
	 *
	 * @throws Exception DBアクセス時にエラーが発生した場合
	 */
	public boolean isNeedAsyncProcess(EIMSession sess, EIMObject object,
				GuardConditionExecDomain guardConditionExecDomain) throws Exception
	{
		// 現状の有効期限設定処理ではguardConditionExecDomainは使用しないので、
		// 処理分岐はありません。使用の際に変更をお願いします。
		
		return false;
	}
	
	/**
	 * 有効期限設定処理の非同期処理実行メソッド<br>
	 * 処理しません
	 *
	 * @param sess セッション情報
	 * @param object 処理対象オブジェクト
	 * @throws Exception DBアクセス時にエラーが発生した場合
	 */
	public void doAsyncProcess(EIMSession sess, EIMObject object) throws Exception
	{
	}

	/**
	 * DateUtilsからの移植（HttpServletRequestを使用しているため）<BR>
	 * 日時編集処理<BR>
	 * [備考]<BR>
	 * 日時をGMT標準時に編集する。
	 * <li>海外の日時・日本の日時ともに、セッションを元にDB登録用の日時に変換して返します。
	 *
	 * @param sess
	 *            セッション情報
	 * @param expirationDate
	 *            有効期限(APサーバで設定した有効期限であること)
	 * @return dbServerDate 編集した有効期限
	 * @throws Exception
	 */
	private Date editExpirationDate(EIMSession sess, Date expirationDate)
			throws Exception {

		// APサーバのオフセットを取得
		TimeZone tz = TimeZone.getDefault();
		long apTzOffset = tz.getRawOffset();

		// DBサーバのオフセットを取得
		long dbTzOffset = eim.util.DateUtils.selectDBTzOffset(sess);

		/*
		 * DBサーバ日時の算出
		 */
		// 有効期限の取得(APサーバ日時)
		long expirationMSec = expirationDate.getTime();

		// クライアント端末の時差を引き、GMT標準値にする
		// GMT標準値 ＝ APサーバ日時 - APサーバの時差
		long gmtTime = expirationMSec - apTzOffset;

		// DBサーバ日時に変換
		// DBサーバ日時 ＝ GMT標準値 + DBサーバの時差
		long dbTime = gmtTime + dbTzOffset;

		return new Date(dbTime);
	}
}
