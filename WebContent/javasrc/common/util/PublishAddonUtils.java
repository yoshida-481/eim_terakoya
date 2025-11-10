package common.util;


import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.commons.configuration2.HierarchicalConfiguration;

import addon.PublishCommandAddOn;
import eim.bo.EIMAccessRole;
import eim.bo.EIMAttribute;
import eim.bo.EIMException;
import eim.bo.EIMFile;
import eim.bo.EIMFormat;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMStatus;
import eim.bo.EIMStatusType;
import eim.bo.EIMWorkFlow;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.FileUtils;
import eim.util.ObjectUtils;
import eim.util.SecurityUtils;
import eim.util.StringUtils;
import eim.util.WorkFlowUtils;
import jp.co.ctc_g.eim.app.document.business.domain.PDFSettingDomain;

/**
 * 公開処理アドオン関連・ユーティリティメソッド群
 */
public class PublishAddonUtils {

	/**
	 *
	 * 公開処理アドオンのコンフィグファイルからクラス名を取得し、
	 * 対象のクラスインスタンスを生成するメソッド
	 *
	 * @param sess EIMSessionインスタンス
	 *
	 * @return 公開処理アドオンのクラスインスタンスリスト(PublishCommandAddOnのリスト)
	 *
	 * @throws Exception
	 */
	public static List<PublishCommandAddOn> getAddonClasses(EIMSession sess) throws Exception{

		List<PublishCommandAddOn> instanceList = new ArrayList<>();

		/*
		 * コンフィグファイルの取得処理
		 */
		HierarchicalConfiguration<?> hcEim = null;
		try {
			// PublishAddonXMLConfigの取得
			PublishAddonXMLConfig conf = PublishAddonXMLConfig.getInstance();
			// コンフィグファイルの取得
			hcEim = conf.getPublishAddonConfig();
		} catch (Exception e) {
			// コンフィグファイルが取得できない場合
			throw new EIMException(sess, "EIM.ERROR.MACRO.NOTEXIST.CONF.FILE");
		}

		/*
		 * CommandMacroタグの属性classの取得処理
		 */
		List<String> classNameList = null;
		try {
			classNameList = hcEim.getList("CommandMacro.[@class]").stream().map(value -> (String) value).collect(Collectors.toList());
		} catch (Exception e) {
			// コンフィグファイルの内容がおかしい場合
			throw new EIMException(sess, "EIM.ERROR.MACRO.INVALID.CONF.FILE");
		}

		/*
		 * クラス名からインスタンスの生成
		 */
		for (String className : classNameList) {
			PublishCommandAddOn addonObj = null;
			try {
				// クラス名からクラスを生成
				Class<?> addonClass = Class.forName(className);
				// インスタンスの生成
				addonObj = (PublishCommandAddOn) addonClass.getDeclaredConstructor().newInstance();
			} catch (Exception e) {
				// インスタンスが生成できない場合
				throw new EIMException(sess, "EIM.ERROR.MACRO.NOTEXIST.CLASS");
			}

			// インスタンスリストに追加
			instanceList.add(addonObj);
		}

		// リストを返却
		return instanceList;
	}

	/**
	 * PDF結合処理において, 権限チェックを行うメソッド
	 * cf.共通設計: アクション権限一覧
	 * 1) ユーザがすべての結合元PDFファイルの公開読み取り権限を持つ
	 * 2-1) 結合先フォルダが「WF付きフォルダ」である場合は, 編集中である
	 * 2-2) 結合先フォルダが「WF付きフォルダ」でない
	 * @param sess         EIMSession
	 * @param joinedDocIds 結合対象オブジェクトID
	 * @param folderId     親オブジェクトID
	 * @return true = 権限あり false = 権限なし
	 * @throws Exception
	 */
	public static boolean checkPDFJoinAuth( EIMSession sess, String[] joinedDocIds, long folderId ) throws Exception{

		long[] joinedDocIdsInt = new long[joinedDocIds.length];

		for( int i = 0; i < joinedDocIds.length; i++ ) {
			 joinedDocIdsInt[i] = Integer.parseInt(joinedDocIds[i]);
		}
		return checkPDFJoinAuth(sess, joinedDocIdsInt, folderId);

	}


	/**
	 * PDF結合処理において, 権限チェックを行うメソッド
	 * cf.共通設計: アクション権限一覧
	 * 1) ユーザがすべての結合元PDFファイルの公開読み取り権限を持つ
	 * 2-1) 結合先フォルダが「WF付きフォルダ」である場合は, 編集中である
	 * 2-2) 結合先フォルダが「WF付きフォルダ」でない
	 * @param sess         EIMSession
	 * @param joinedDocIds 結合対象オブジェクトID
	 * @param folderId     親オブジェクトID
	 * @return true = 権限あり false = 権限なし
	 * @throws Exception
	 */
	public static boolean checkPDFJoinAuth( EIMSession sess, long[] joinedDocIds, long folderId ) throws Exception{
		/** 結合対象オブジェクトIDについて, 公開読み取り権限をチェック */
		for( int i = 0; i < joinedDocIds.length; i++ ) {
			EIMObject obj = ObjectUtils.getObjectById( sess, joinedDocIds[i]);

			// [09/03/10 added by ik.]
			// 選択されたオブジェクトが存在しない場合のエラー処理を実装
			if(obj == null) {
				throw new EIMException(sess, "EIM.ERROR.LOGIC.NODOCUMENT");
			}

			if( !SecurityUtils.authorized( sess, obj, sess.getUser(), EIMAccessRole.READ ) ) {
				throw new EIMException(sess, "EIM.ERROR.LOGIC.NOPDFJOINROLE");
			}
		}
		/** 結合先フォルダチェック */
		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = new AppObjectConditionHelper( sess );
		EIMObject folderObj = ObjectUtils.getObjectById( sess, folderId );
		// ユーザが書き込み権限を持っていなければ false
		if(folderObj == null || !SecurityUtils.authorized( sess, folderObj,sess.getUser(), EIMAccessRole.CREATE ) ) {
			return false;
		}
		// 「上位WFフォルダ」取得
		long parentObjId = helper.getUpperFolderWithWorkflowObjId( folderObj );
		EIMObject parentObj = null;
		if( parentObjId != -1 ) {
			parentObj = ObjectUtils.getObjectById(sess, parentObjId);
		}
		// 上位WFフォルダが存在する場合
		if (parentObj != null && SecurityUtils.authorized(sess, parentObj, sess.getUser(), EIMAccessRole.READ)
				&& ((helper.isTypeOfFolderWithWorkflow(parentObj)
						|| helper.isTypeOfFolderUnderFolderWithWorkflow(parentObj)))) {
			// 上位WFのステータスが「編集中」「なし」以外の場合は権限なし
			long parentStsKind = parentObj.getStatus() != null ? parentObj.getStatus().getType().getKind() : AppConstant.STATUS_TYPE_KIND_ID_NONE;
			if( parentStsKind != AppConstant.STATUS_TYPE_KIND_ID_EDITTING && parentStsKind != AppConstant.STATUS_TYPE_KIND_ID_NONE ) {
				throw new EIMException(sess, "EIM.ERROR.LOGIC.NOPDFJOINROLE");
			}
		// 上位WFフォルダが存在しない(または存在してもWF付きではなかった)、かつ、対象がWFを持つ場合
		} else if( helper.isTypeOfFolderWithWorkflow( folderObj ) ){
			long stsKind = folderObj.getStatus() != null ? folderObj.getStatus().getType().getKind() : AppConstant.STATUS_TYPE_KIND_ID_NONE;
			// 上位WFのステータスが「編集中」「なし」以外の場合は権限なし
			if (stsKind != AppConstant.STATUS_TYPE_KIND_ID_EDITTING && stsKind != AppConstant.STATUS_TYPE_KIND_ID_NONE) {
				throw new EIMException(sess, "EIM.ERROR.LOGIC.NOPDFJOINROLE");
			}
		}
		return true;
	}
	/**
	 * PDF結合処理において, 権限チェックを行うメソッド(結合先フォルダ)
	 * cf.共通設計: アクション権限一覧
	 * 1-1) 結合先フォルダが「WF付きフォルダ」である場合は, 編集中である
	 * 1-2) 結合先フォルダが「WF付きフォルダ」でない
	 * @param  sess         EIMSession
	 * @param  folderId
	 * @return true = OK, false = 権限なし
	 * @throws Exception
	 */
	public static boolean checkPDFJoinFolderAuth( EIMSession sess, long folderId ) throws Exception{
		/** 結合先フォルダチェック */
		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = new AppObjectConditionHelper( sess );
		EIMObject folderObj = ObjectUtils.getObjectById( sess, folderId );
		// ユーザが書き込み権限を持っていなければ false
		if(folderObj == null || !SecurityUtils.authorized( sess, folderObj,sess.getUser(), EIMAccessRole.CREATE ) ) {
			return false;
		}
		// 「上位WFフォルダ」取得
		long parentObjId = helper.getUpperFolderWithWorkflowObjId( folderObj );
		EIMObject parentObj = null;
		if( parentObjId != -1 ) {
			parentObj = ObjectUtils.getObjectById(sess, parentObjId);
		}
		// 上位WFフォルダが存在する場合
		if (parentObj != null && SecurityUtils.authorized(sess, parentObj, sess.getUser(), EIMAccessRole.READ)
				&& ((helper.isTypeOfFolderWithWorkflow(parentObj)
						|| helper.isTypeOfFolderUnderFolderWithWorkflow(parentObj)))) {
			// 上位WFのステータスが「編集中」「なし」以外の場合は権限なし
			long parentStsKind = parentObj.getStatus() != null ? parentObj.getStatus().getType().getKind() : AppConstant.STATUS_TYPE_KIND_ID_NONE;
			if( parentStsKind != AppConstant.STATUS_TYPE_KIND_ID_EDITTING && parentStsKind != AppConstant.STATUS_TYPE_KIND_ID_NONE ) {
				return false;
			}
		// 上位WFフォルダが存在しない(または存在してもWF付きではなかった)、かつ、対象がWFを持つ場合
		} else if( helper.isTypeOfFolderWithWorkflow( folderObj ) ){
			long stsKind = folderObj.getStatus() != null ? folderObj.getStatus().getType().getKind() : AppConstant.STATUS_TYPE_KIND_ID_NONE;
			// 上位WFのステータスが「編集中」「なし」以外の場合は権限なし
			if (stsKind != AppConstant.STATUS_TYPE_KIND_ID_EDITTING && stsKind != AppConstant.STATUS_TYPE_KIND_ID_NONE) {
				return false;
			}
		}
		return true;
	}
	/**
	 * PDF署名処理の権限チェック
	 * @param sess    EIMSession
	 * @param objId   対象オブジェクト
	 * @return true = 権限あり false = 権限なし
	 * @throws Exception
	 */
	public static boolean checkSetPDFSecurityAuth( EIMSession sess, long objId ) throws Exception{
		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = new AppObjectConditionHelper( sess );
		EIMObject obj = ObjectUtils.getObjectById( sess, objId );
		// ユーザが更新権限を持っていなければ false
		if(obj == null || !SecurityUtils.authorized( sess, obj,sess.getUser(), EIMAccessRole.UPDATE ) ) {
			return false;
		}
		// 「上位WFフォルダ」取得
		long parentObjId = helper.getUpperFolderWithWorkflowObjId( obj );
		EIMObject parentObj = null;
		if( parentObjId != -1 ) {
			parentObj = ObjectUtils.getObjectById(sess, parentObjId);
		}
		// 上位WFフォルダが存在する場合
		if (parentObj != null && SecurityUtils.authorized(sess, parentObj, sess.getUser(), EIMAccessRole.READ)
				&& ((helper.isTypeOfFolderWithWorkflow(parentObj)
						|| helper.isTypeOfFolderUnderFolderWithWorkflow(parentObj)))) {
			// 上位WFのステータスが「公開中」以外の場合は権限なし
			long parentStsKind = parentObj.getStatus() != null ? parentObj.getStatus().getType().getKind() : AppConstant.STATUS_TYPE_KIND_ID_NONE;
			if( parentStsKind != AppConstant.STATUS_TYPE_KIND_ID_PUBLIC ) {
				return false;
			}
		} else {
			long stsKind = obj.getStatus() != null ? obj.getStatus().getType().getKind() : AppConstant.STATUS_TYPE_KIND_ID_NONE;
			// WFのステータスが「公開中」以外の場合は権限なし
			if (stsKind != AppConstant.STATUS_TYPE_KIND_ID_PUBLIC) {
				return false;
			}
		}
		return true;
	}
	/**
	 * ワークフロー公開処理・PDF署名オブジェクトの属性を XML 文字列で返す
	 * (署名・セキュリティ設定両方)
	 * @param sess     EIMSession
	 * @param wfpubObj ワークフロー公開処理オブジェクト または PDF署名オブジェクト
	 * @return
	 */
	public static String getSignAndSetSecurityConfig( EIMSession sess, EIMObject wfpubObj ){
		String outString = "";
		int On = 1;
		EIMAttribute attFlag = null;
		// PDF署名実施フラグ
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_FLG"));
		}
		if(attFlag != null){
			long flagVal = attFlag.getInt();
			if(flagVal == On){
				outString += " doSignPDFAndSetSecurity=\"true\"";
			} else {
				outString += " doSignPDFAndSetSecurity=\"false\"";
			}
		} else {
			if(wfpubObj != null && wfpubObj.getType().getDefName().equals(EIMConfig.get("OBJECT_TYPE_NAME_SIGNPDF")) ) {
				// PDF署名オブジェクトなら true とする
				outString += " doSignPDFAndSetSecurity=\"true\"";
			} else {
				outString += " doSignPDFAndSetSecurity=\"false\"";
			}
		}
		// 署名有無
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_ONOFF"));
		}
		if(attFlag != null){
			long flagVal = attFlag.getInt();
			if(flagVal == On){
				outString += " doSignPDF=\"true\"";
			} else {
				outString += " doSignPDF=\"false\"";
			}
		} else {
			outString += " doSignPDF=\"false\"";
		}
		// 承認日付挿入
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_APPDATE"));
		}
		if(attFlag != null){
			long flagVal = attFlag.getInt();
			if(flagVal == On){
				outString += " insertApproveDate=\"true\"";
			} else {
				outString += " insertApproveDate=\"false\"";
			}
		} else {
			outString += " insertApproveDate=\"false\"";
		}
		// 承認者名挿入
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_APPNAME"));
		}
		if(attFlag != null){
			long flagVal = attFlag.getInt();
			outString += " insertApproveUser=\"" + Long.toString(flagVal) + "\"";
		} else {
			outString += " insertApproveUser=\"0\"";
		}
		// 挿入ページ
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_INSPAGE"));
		}
		if(attFlag != null){
			long flagVal = attFlag.getInt();
			outString += " insertPage=\"" + Long.toString(flagVal) + "\"";
		} else {
			outString += " insertPage=\"1\"";
		}
		// 挿入位置基準点
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_BASELINE"));
		}
		if(attFlag != null){
			long flagVal = attFlag.getInt();
			outString += " insertPlace=\"" + Long.toString(flagVal) + "\"";
		} else {
			outString += " insertPlace=\"0\"";
		}
		// 挿入位置座標X
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_BASELINE_X"));
		}
		if(attFlag != null){
			long flagVal = attFlag.getInt();
			outString += " insertPlaceX=\"" + Long.toString(flagVal) + "\"";
		} else {
			outString += " insertPlaceX=\"0\"";
		}
		// 挿入位置座標Y
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_BASELINE_Y"));
		}
		if(attFlag != null){
			long flagVal = attFlag.getInt();
			outString += " insertPlaceY=\"" + Long.toString(flagVal) + "\"";
		} else {
			outString += " insertPlaceY=\"0\"";
		}
		outString += PublishAddonUtils.getSetSecurityConfig(sess, wfpubObj);
		return outString;
	}
	/**
	 * ワークフロー公開処理・PDF署名オブジェクトの属性を XML 文字列で返す
	 * (署名・セキュリティ設定両方)
	 * @param sess     EIMSession
	 * @param wfpubObj ワークフロー公開処理オブジェクト または PDF署名オブジェクト
	 * @return
	 */
	public static Map<String, String> getSignAndSetSecurityConfigList( EIMSession sess, EIMObject wfpubObj ){

		Map<String, String> map = new HashMap<String, String>();
		
		int On = 1;
		EIMAttribute attFlag = null;
		// PDF署名実施フラグ
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_FLG"));
		}
		if(attFlag != null){
			long flagVal = attFlag.getInt();
			if(flagVal == On){
				map.put("doSignPDFAndSetSecurity", "true");
			} else {
				map.put("doSignPDFAndSetSecurity", "false");
			}
		} else {
			if(wfpubObj != null && wfpubObj.getType().getName().equals(EIMConfig.get("OBJECT_TYPE_NAME_SIGNPDF")) ) {
				// PDF署名オブジェクトなら true とする
				map.put("doSignPDFAndSetSecurity", "true");
			} else {
				map.put("doSignPDFAndSetSecurity", "false");
			}
		}
		// 署名有無
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_ONOFF"));
		}
		if(attFlag != null){
			long flagVal = attFlag.getInt();
			if(flagVal == On){
				map.put("doSignPDF", "true");
			} else {
				map.put("doSignPDF", "false");
			}
		} else {
			map.put("doSignPDF", "false");
		}
		// 承認日付挿入
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_APPDATE"));
		}
		if(attFlag != null){
			long flagVal = attFlag.getInt();
			if(flagVal == On){
				map.put("insertApproveDate", "true");
			} else {
				map.put("insertApproveDate", "false");
			}
		} else {
			map.put("insertApproveDate", "false");
		}
		// 承認者名挿入
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_APPNAME"));
		}
		if(attFlag != null){
			long flagVal = attFlag.getInt();
			map.put("insertApproveUser", Long.toString(flagVal));
		} else {
			map.put("insertApproveUser", "0");
		}
		// 挿入ページ
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_INSPAGE"));
		}
		if(attFlag != null){
			long flagVal = attFlag.getInt();
			map.put("insertPage", Long.toString(flagVal));
		} else {
			map.put("insertPage", "1");
		}
		// 挿入位置基準点
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_BASELINE"));
		}
		if(attFlag != null){
			long flagVal = attFlag.getInt();
			map.put("insertPlace", Long.toString(flagVal));
		} else {
			map.put("insertPlace", "0");
		}
		// 挿入位置座標X
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_BASELINE_X"));
		}
		if(attFlag != null){
			long flagVal = attFlag.getInt();
			map.put("insertPlaceX", Long.toString(flagVal));
		} else {
			map.put("insertPlaceX", "0");
		}
		// 挿入位置座標Y
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_BASELINE_Y"));
		}
		if(attFlag != null){
			long flagVal = attFlag.getInt();
			map.put("insertPlaceY", Long.toString(flagVal));
		} else {
			map.put("insertPlaceY", "0");
		}
		// 電子署名用言語
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_APPROVE_NAME_LANG"));
		}
		if(attFlag != null){
			String flagVal = attFlag.getString();
			map.put("approveNamelang", flagVal);
		} else {
			map.put("approveNamelang", "");
		}
		// 署名用ジョブ名
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_JOB_NAME"));
		}
		if(attFlag != null){
			String flagVal = attFlag.getString();
			map.put("signJobName", flagVal);
		} else {
			map.put("signJobName", "");
		}

		// ワークフロー公開処理・PDF署名・PDFセキュリティ設定オブジェクトの属性情報を設定
		map.putAll(PublishAddonUtils.getSetSecurityConfigList(sess, wfpubObj));

		return map;
	}
	/**
	 * ワークフロー公開処理・PDF署名・PDFセキュリティ設定オブジェクトの属性を XML 文字列で返す
	 * (セキュリティ設定)
	 * @param sess     EIMSession
	 * @param wfpubObj ワークフロー公開処理オブジェクト または PDF署名オブジェクト または PDF セキュリティ設定オブジェクト
	 * @return
	 */
	public static Map<String, String> getSetSecurityConfigList( EIMSession sess, EIMObject wfpubObj ){
		Map<String, String> map = new HashMap<String, String>();

		int On = 1;
		EIMAttribute attFlag = null;
		// セキュリティ設定有無
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_ONOFF"));
		}
		if(attFlag != null){
			long flagVal = attFlag.getInt();
			if(flagVal == On){
				map.put("doSetSecurity", "true");
			} else {
				map.put("doSetSecurity", "false");
			}
		} else {
			map.put("doSetSecurity", "false");
		}
		// セキュリティパスワード設定有無
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECPASS_ONOFF"));
		}
		if(attFlag != null){
			long flagVal = attFlag.getInt();
			if(flagVal == On){
				map.put("doSetSecurityPassword", "true");
			} else {
				map.put("doSetSecurityPassword", "false");
			}
		} else {
			map.put("doSetSecurityPassword", "false");
		}
		// セキュリティパスワード
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECPASS"));
		}
		if(attFlag != null){
			String flagVal = attFlag.getString();
			map.put("securityPassword", flagVal);
		} else {
			map.put("securityPassword", "");
		}
		// 参照用パスワード設定有無
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_REFPASS_ONOFF"));
		}
		if(attFlag != null){
			long flagVal = attFlag.getInt();
			if(flagVal == On){
				map.put("doSetReferencePassword", "true");
			} else {
				map.put("doSetReferencePassword", "false");
			}
		} else {
			map.put("doSetReferencePassword", "false");
		}
		// 参照用パスワード
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_REFPASS"));
		}
		if(attFlag != null){
			String flagVal = attFlag.getString();
			map.put("referencePassword", flagVal);
		} else {
			map.put("referencePassword", "");
		}
		// 印刷許可
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_FORBPRINT"));
		}
		if(attFlag != null){
			long flagVal = attFlag.getInt();
			if(flagVal == On){
				map.put("forbidPrint", "true");
			} else {
				map.put("forbidPrint", "false");
			}
		} else {
			map.put("forbidPrint", "false");
		}
		// 編集許可
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_FORBEDIT"));
		}
		if(attFlag != null){
			long flagVal = attFlag.getInt();
			if(flagVal == On){
				map.put("forbidEdit", "true");
			} else {
				map.put("forbidEdit", "false");
			}
		} else {
			map.put("forbidEdit", "false");
		}
		// 注釈追加許可
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_FORBADDREM"));
		}
		if(attFlag != null){
			long flagVal = attFlag.getInt();
			if(flagVal == On){
				map.put("forbidAnnotate", "true");
			} else {
				map.put("forbidAnnotate", "false");
			}
		} else {
			map.put("forbidAnnotate", "false");
		}
		// 転載追加許可
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_FORBREPR"));
		}
		if(attFlag != null){
			long flagVal = attFlag.getInt();
			if(flagVal == On){
				map.put("forbidReproduce", "true");
			} else {
				map.put("forbidReproduce", "false");
			}
		} else {
			map.put("forbidReproduce", "false");
		}
		return map;
	}

	/**
	 * ワークフロー公開処理・PDF署名・PDFセキュリティ設定オブジェクトの属性を Mapで返す
	 * (セキュリティ設定)
	 * @param sess     EIMSession
	 * @param wfpubObj ワークフロー公開処理オブジェクト または PDF署名オブジェクト または PDF セキュリティ設定オブジェクト
	 * @return
	 */
	public static String getSetSecurityConfig( EIMSession sess, EIMObject wfpubObj ){
		String outString = "";
		int On = 1;
		EIMAttribute attFlag = null;
		// セキュリティ設定有無
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_ONOFF"));
		}
		if(attFlag != null){
			long flagVal = attFlag.getInt();
			if(flagVal == On){
				outString += " doSetSecurity=\"true\"";
			} else {
				outString += " doSetSecurity=\"false\"";
			}
		} else {
			outString += " doSetSecurity=\"false\"";
		}
		// セキュリティパスワード設定有無
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECPASS_ONOFF"));
		}
		if(attFlag != null){
			long flagVal = attFlag.getInt();
			if(flagVal == On){
				outString += " doSetSecurityPassword=\"true\"";
			} else {
				outString += " doSetSecurityPassword=\"false\"";
			}
		} else {
			outString += " doSetSecurityPassword=\"false\"";
		}
		// セキュリティパスワード
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECPASS"));
		}
		if(attFlag != null){
			String flagVal = attFlag.getString();
			outString += " securityPassword=\"" + flagVal + "\"";
		} else {
			outString += " securityPassword=\"\"";
		}
		// 参照用パスワード設定有無
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_REFPASS_ONOFF"));
		}
		if(attFlag != null){
			long flagVal = attFlag.getInt();
			if(flagVal == On){
				outString += " doSetReferencePassword=\"true\"";
			} else {
				outString += " doSetReferencePassword=\"false\"";
			}
		} else {
			outString += " doSetReferencePassword=\"false\"";
		}
		// 参照用パスワード
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_REFPASS"));
		}
		if(attFlag != null){
			String flagVal = attFlag.getString();
			outString += " referencePassword=\"" + flagVal + "\"";
		} else {
			outString += " referencePassword=\"\"";
		}
		// 印刷許可
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_FORBPRINT"));
		}
		if(attFlag != null){
			long flagVal = attFlag.getInt();
			if(flagVal == On){
				outString += " forbidPrint=\"true\"";
			} else {
				outString += " forbidPrint=\"false\"";
			}
		} else {
			outString += " forbidPrint=\"false\"";
		}
		// 編集許可
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_FORBEDIT"));
		}
		if(attFlag != null){
			long flagVal = attFlag.getInt();
			if(flagVal == On){
				outString += " forbidEdit=\"true\"";
			} else {
				outString += " forbidEdit=\"false\"";
			}
		} else {
			outString += " forbidEdit=\"false\"";
		}
		// 注釈追加許可
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_FORBADDREM"));
		}
		if(attFlag != null){
			long flagVal = attFlag.getInt();
			if(flagVal == On){
				outString += " forbidAnnotate=\"true\"";
			} else {
				outString += " forbidAnnotate=\"false\"";
			}
		} else {
			outString += " forbidAnnotate=\"false\"";
		}
		// 転載追加許可
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_FORBREPR"));
		}
		if(attFlag != null){
			long flagVal = attFlag.getInt();
			if(flagVal == On){
				outString += " forbidReproduce=\"true\"";
			} else {
				outString += " forbidReproduce=\"false\"";
			}
		} else {
			outString += " forbidReproduce=\"false\"";
		}
		return outString;
	}
	
	/**
	 * 渡されたワークフロー公開処理オブジェクトの「ワークフロー公開処理PDF署名実施フラグ」を返す
	 * @param  sess     EIMSession
	 * @param  wfpubObj ワークフロー公開処理オブジェクト
	 * @return 0 = 実施しない or 取得失敗, 属性なし etc, 1 = 実施する
	 */
	public static long getDoSignAndSetSecurityConfig( EIMSession sess, EIMObject wfpubObj ){
		long ret = 0;
		EIMAttribute attFlag = null;
		// PDF署名実施フラグ
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_FLG"));
		}
		if(attFlag != null){
			ret = attFlag.getInt();
		}
		return ret;
	}
	/**
	 * 渡されたワークフロー公開処理オブジェクトの「ワークフロー公開処理PDF変換実施フラグ」を返す
	 * @param  sess     EIMSession
	 * @param  wfpubObj ワークフロー公開処理オブジェクト
	 * @return 0 = 実施しない or 取得失敗, 属性なし etc, 1 = 実施する
	 */
	public static long getDoConvertPDFConfig( EIMSession sess, EIMObject wfpubObj ){
		long ret = 0;
		EIMAttribute attFlag = null;
		// PDF署名実施フラグ
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_FLG"));
		}
		if(attFlag != null){
			ret = attFlag.getInt();
		}
		return ret;
	}

	/**
	 * ワークフロー公開処理オブジェクトと, PDF署名オブジェクトで共通の属性をコピーする
	 * (署名・セキュリティ設定両方)
	 * @param sess      EIMSession
	 * @param wfpubObj  コピー元のワークフロー公開処理またはPDF署名オブジェクト
	 * @param pdfSignObjコピー先のワークフロー公開処理またはPDF署名オブジェクト
	 */
	public static void copyPDFSignAndSecAttrs2Obj(EIMSession sess, EIMObject wfpubObj, EIMObject pdfSignObj) throws Exception {
		EIMAttribute attFlag = null;
		// 署名有無
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_ONOFF"));
		}
		if(attFlag != null){
			long flagVal = attFlag.getInt();
			AppObjectUtil.setAttr(sess, pdfSignObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_ONOFF"), flagVal);
		}
		// 承認日付挿入
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_APPDATE"));
		}
		if(attFlag != null){
			long flagVal = attFlag.getInt();
			AppObjectUtil.setAttr(sess, pdfSignObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_APPDATE"), flagVal);
		}
		// 承認者名挿入
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_APPNAME"));
		}
		if(attFlag != null){
			long flagVal = attFlag.getInt();
			AppObjectUtil.setAttr(sess, pdfSignObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_APPNAME"), flagVal);
		}
		// 挿入ページ
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_INSPAGE"));
		}
		if(attFlag != null){
			long flagVal = attFlag.getInt();
			AppObjectUtil.setAttr(sess, pdfSignObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_INSPAGE"), flagVal);
		}
		// 挿入位置基準点
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_BASELINE"));
		}
		if(attFlag != null){
			long flagVal = attFlag.getInt();
			AppObjectUtil.setAttr(sess, pdfSignObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_BASELINE"), flagVal);
		}
		// 挿入位置座標X
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_BASELINE_X"));
		}
		if(attFlag != null){
			long flagVal = attFlag.getInt();
			AppObjectUtil.setAttr(sess, pdfSignObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_BASELINE_X"), flagVal);
		}
		// 挿入位置座標Y
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_BASELINE_Y"));
		}
		if(attFlag != null){
			long flagVal = attFlag.getInt();
			AppObjectUtil.setAttr(sess, pdfSignObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_BASELINE_Y"), flagVal);
		}
		PublishAddonUtils.copyPDFSecAttrs2Obj(sess, wfpubObj, pdfSignObj);
	}
	/**
	 * ワークフロー公開処理/PDF署名/PDFセキュリティ設定オブジェクトで共通の属性をコピーする
	 * (セキュリティ設定)
	 * @param sess      EIMSession
	 * @param wfpubObj  コピー元のワークフロー公開処理/PDF署名/PDFセキュリティ設定
	 * @param pdfSignObjコピー先のワークフロー公開処理/PDF署名/PDFセキュリティ設定
	 */
	public static void copyPDFSecAttrs2Obj(EIMSession sess, EIMObject wfpubObj, EIMObject pdfSignObj) throws Exception {
		EIMAttribute attFlag = null;
		// セキュリティ設定有無
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_ONOFF"));
		}
		if(attFlag != null){
			long flagVal = attFlag.getInt();
			AppObjectUtil.setAttr(sess, pdfSignObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_ONOFF"), flagVal);
		}
		// セキュリティパスワード設定有無
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECPASS_ONOFF"));
		}
		if(attFlag != null){
			long flagVal = attFlag.getInt();
			AppObjectUtil.setAttr(sess, pdfSignObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECPASS_ONOFF"), flagVal);
		}
		// セキュリティパスワード
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECPASS"));
		}
		if(attFlag != null){
			String flagVal = attFlag.getString();
			AppObjectUtil.setAttr(sess, pdfSignObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECPASS"), flagVal);
		}
		// 参照用パスワード設定有無
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_REFPASS_ONOFF"));
		}
		if(attFlag != null){
			long flagVal = attFlag.getInt();
			AppObjectUtil.setAttr(sess, pdfSignObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_REFPASS_ONOFF"), flagVal);
		}
		// 参照用パスワード
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_REFPASS"));
		}
		if(attFlag != null){
			String flagVal = attFlag.getString();
			AppObjectUtil.setAttr(sess, pdfSignObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_REFPASS"), flagVal);
		}
		// 印刷許可
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_FORBPRINT"));
		}
		if(attFlag != null){
			long flagVal = attFlag.getInt();
			AppObjectUtil.setAttr(sess, pdfSignObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_FORBPRINT"), flagVal);
		}
		// 編集許可
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_FORBEDIT"));
		}
		if(attFlag != null){
			long flagVal = attFlag.getInt();
			AppObjectUtil.setAttr(sess, pdfSignObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_FORBEDIT"), flagVal);
		}
		// 注釈追加許可
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_FORBADDREM"));
		}
		if(attFlag != null){
			long flagVal = attFlag.getInt();
			AppObjectUtil.setAttr(sess, pdfSignObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_FORBADDREM"), flagVal);
		}
		// 転載追加許可
		if(wfpubObj != null){
			attFlag = wfpubObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_FORBREPR"));
		}
		if(attFlag != null){
			long flagVal = attFlag.getInt();
			AppObjectUtil.setAttr(sess, pdfSignObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_FORBREPR"), flagVal);
		}
	}

	/**
	 * ワークフロー公開処理またはPDF署名オブジェクトに属性を設定する
	 * (署名・セキュリティ設定両方)
	 * @param sess                     EIMSession
	 * @param wfpubObj                 ワークフロー公開処理またはPDF署名オブジェクト
	 * @param pdfSettingDomain         PDF出力設定ドメイン
	 * @throws Exception
	 */
	public static void setSignAndSetSecurityConfig(
			EIMSession sess,
			EIMObject wfpubObj,
			PDFSettingDomain pdfSettingDomain
	) throws Exception {
		
		int On = 1;
		int Off = 0;
		/* Write Attributes */
		
		// 署名有無
		int doSignPDF = Off;
		if(pdfSettingDomain.isDoSignPDF()) {
			doSignPDF = On;
		}
		AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_ONOFF"), doSignPDF);
		
		// 承認日付挿入
		AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_APPDATE"), pdfSettingDomain.getInsertApproveDate());

		// 承認者名挿入
		AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_APPNAME"), pdfSettingDomain.getInsertApproveUser());

		// 挿入ページ
		AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_INSPAGE"), pdfSettingDomain.getInsertPage());

		// 挿入位置基準点
		AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_BASELINE"), pdfSettingDomain.getInsertPlace());

		// 挿入位置座標X
		AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_BASELINE_X"), pdfSettingDomain.getInsertPlaceX());

		// 挿入位置座標Y
		AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_BASELINE_Y"), pdfSettingDomain.getInsertPlaceY());

		PublishAddonUtils.setSetSecurityConfig(sess, wfpubObj, pdfSettingDomain);
		return;
	}
	
	/**
	 * ワークフロー公開処理オブジェクトと, PDF署名オブジェクトで共通の属性を設定する
	 * (セキュリティ設定)
	 * @param sess                     EIMSession
	 * @param wfpubObj                 ワークフロー公開処理またはPDF署名オブジェクト
	 * @param pdfSettingDomain         PDF出力設定ドメイン
	 * @throws Exception
	 */
	public static void setSetSecurityConfig(
			EIMSession sess,
			EIMObject wfpubObj,
			PDFSettingDomain pdfSettingDomain
	) throws Exception {
		
		int On = 1;
		int Off = 0;
		/* Write Attributes */
		
		// セキュリティ設定有無
		int doSetSecurity = Off;
		if(pdfSettingDomain.isDoSetSecurity()){
			doSetSecurity = On;
		}
		AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_ONOFF"), doSetSecurity);

		// セキュリティパスワード設定有無
		int doSetSecurityPassword = Off;
		if(pdfSettingDomain.isDoSetSecurityPassword()){
			doSetSecurityPassword = On;
		}
		AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECPASS_ONOFF"), doSetSecurityPassword);
		
		// セキュリティパスワード
		if (!StringUtils.isBlank(pdfSettingDomain.getSecurityPassword())) {
			AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECPASS"), pdfSettingDomain.getSecurityPassword());
		}
		else {
			AppObjectUtil.deleteAttribute(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECPASS"));
		}

		// 参照用パスワード設定有無
		int doSetReferencePassword = Off;
		if(pdfSettingDomain.isDoSetReferencePassword()){
			doSetReferencePassword = On;
		}
		AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_REFPASS_ONOFF"), doSetReferencePassword);
		
		// 参照用パスワード
		if(!StringUtils.isBlank(pdfSettingDomain.getReferencePassword())){
			AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_REFPASS"), pdfSettingDomain.getReferencePassword());
		} else {
			AppObjectUtil.deleteAttribute(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_REFPASS"));
		}
		
		// 印刷許可設定
		int forbidPrint = Off;
		if(pdfSettingDomain.isForbidPrint()){
			forbidPrint = On;
		}
		AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_FORBPRINT"), forbidPrint);
		
		// 編集許可設定
		int forbidEdit = Off;
		if(pdfSettingDomain.isForbidEdit()){
			forbidEdit = On;
		}
		AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_FORBEDIT"), forbidEdit);

		// 注釈追加許可設定
		int forbidAnnotate = Off;
		if(pdfSettingDomain.isForbidAnnotate()){
			forbidAnnotate = On;
		}
		AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_FORBADDREM"), forbidAnnotate);

		// 転載許可設定
		int forbidReproduce = Off;
		if(pdfSettingDomain.isForbidReproduce()){
			forbidReproduce = On;
		}
		AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_FORBREPR"), forbidReproduce);
		
		return;
	}
	
	/**
	 * ワークフロー公開処理オブジェクトと, PDF署名オブジェクトで共通の属性を設定する
	 * (署名・セキュリティ設定両方)
	 * @param sess                     EIMSession
	 * @param wfpubObj                 ワークフロー公開処理またはPDF署名オブジェクト
	 * @param prmDoSignPDF
	 * @param prmInsertApproveDate
	 * @param prmInsertApproveUser
	 * @param prmInsertPage
	 * @param prmInsertPlace
	 * @param prmInsertPlaceX
	 * @param prmInsertPlaceY
	 * @param prmApproveNamelang
	 * @param prmSignJobName
	 * @param prmDoSetSecurity
	 * @param prmDoSetSecurityPassword
	 * @param prmSecurityPassword
	 * @param prmDoSetReferencePassword
	 * @param prmReferencePassword
	 * @param prmForbidPrint
	 * @param prmForbidEdit
	 * @param prmForbidAnnotate
	 * @param prmForbidReproduce
	 * @throws Exception
	 */
	public static void setSignAndSetSecurityConfig(
			EIMSession sess,
			EIMObject wfpubObj,
			String prmDoSignPDF,
			String prmInsertApproveDate,
			String prmInsertApproveUser,
			String prmInsertPage,
			String prmInsertPlace,
			String prmInsertPlaceX,
			String prmInsertPlaceY,
			String prmApproveNamelang,
			String prmSignJobName,
			String prmDoSetSecurity,
			String prmDoSetSecurityPassword,
			String prmSecurityPassword,
			String prmDoSetReferencePassword,
			String prmReferencePassword,
			String prmForbidPrint,
			String prmForbidEdit,
			String prmForbidAnnotate,
			String prmForbidReproduce
	) throws Exception {
		int On = 1;
		int Off = 0;
		/* Write Attributes */
		int doSignPDF = Off;
		if(prmDoSignPDF.equalsIgnoreCase("true")){
			doSignPDF = On;
		}
		AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_ONOFF"), doSignPDF);

		if(!StringUtils.isBlank(prmInsertApproveDate)){
			int insertApproveDate = Off;
			if(prmInsertApproveDate.equalsIgnoreCase("true")){
				insertApproveDate = On;
			}
			AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_APPDATE"), insertApproveDate);
		} else {
			AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_APPDATE"), Off);
		}

		if(!StringUtils.isBlank(prmInsertApproveUser)){
			int insertApproveUser = Integer.parseInt(prmInsertApproveUser);
			AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_APPNAME"), insertApproveUser);
		} else {
			AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_APPNAME"), 0);
		}

		if(!StringUtils.isBlank(prmInsertPage)){
			int insertPage = Integer.parseInt(prmInsertPage);
			AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_INSPAGE"), insertPage);
		} else {
			AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_INSPAGE"), 0);
		}

		if(!StringUtils.isBlank(prmInsertPlace)){
			int insertPlace = Integer.parseInt(prmInsertPlace);
			AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_BASELINE"), insertPlace);
		} else {
			AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_BASELINE"), 0);
		}

		if(!StringUtils.isBlank(prmInsertPlaceX)){
			int insertPlaceX = Integer.parseInt(prmInsertPlaceX);
			AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_BASELINE_X"), insertPlaceX);
		} else {
			AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_BASELINE_X"), 0);
		}

		if(!StringUtils.isBlank(prmInsertPlaceY)){
			int insertPlaceY = Integer.parseInt(prmInsertPlaceY);
			AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_BASELINE_Y"), insertPlaceY);
		} else {
			AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_BASELINE_Y"), 0);
		}

		if(!StringUtils.isBlank(prmApproveNamelang)){
			AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_APPROVE_NAME_LANG"), prmApproveNamelang);
		}

		if(!StringUtils.isBlank(prmSignJobName)){
			AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_JOB_NAME"), prmSignJobName);
		}
		PublishAddonUtils.setSetSecurityConfig(sess, wfpubObj, prmDoSetSecurity, prmDoSetSecurityPassword, prmSecurityPassword, prmDoSetReferencePassword, prmReferencePassword, prmForbidPrint, prmForbidEdit, prmForbidAnnotate, prmForbidReproduce);
		return;
	}
	/**
	 * ワークフロー公開処理オブジェクトと, PDF署名オブジェクトで共通の属性を設定する
	 * (セキュリティ設定)
	 * @param sess                     EIMSession
	 * @param wfpubObj                 ワークフロー公開処理またはPDF署名オブジェクト
	 * @param prmDoSetSecurity
	 * @param prmDoSetSecurityPassword
	 * @param prmSecurityPassword
	 * @param prmDoSetReferencePassword
	 * @param prmReferencePassword
	 * @param prmForbidPrint
	 * @param prmForbidEdit
	 * @param prmForbidAnnotate
	 * @param prmForbidReproduce
	 * @throws Exception
	 */
	public static void setSetSecurityConfig(
			EIMSession sess,
			EIMObject wfpubObj,
			String prmDoSetSecurity,
			String prmDoSetSecurityPassword,
			String prmSecurityPassword,
			String prmDoSetReferencePassword,
			String prmReferencePassword,
			String prmForbidPrint,
			String prmForbidEdit,
			String prmForbidAnnotate,
			String prmForbidReproduce
	) throws Exception {
		int On = 1;
		int Off = 0;
		/* Write Attributes */
		int doSetSecurity = Off;
		if(prmDoSetSecurity.equalsIgnoreCase("true")){
			doSetSecurity = On;
		}
		AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_ONOFF"), doSetSecurity);

		if(!StringUtils.isBlank(prmDoSetSecurityPassword)){
			int doSetSecurityPassword = Off;
			if(prmDoSetSecurityPassword.equalsIgnoreCase("true")){
				doSetSecurityPassword = On;
			}
			AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECPASS_ONOFF"), doSetSecurityPassword);
		} else {
			AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECPASS_ONOFF"), 0);
		}

		if(!StringUtils.isBlank(prmSecurityPassword)){
			String securityPassword = prmSecurityPassword;
			AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECPASS"), securityPassword);
		} else {
			AppObjectUtil.deleteAttribute(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECPASS"));
		}

		if(!StringUtils.isBlank(prmDoSetReferencePassword)){
			int doSetReferencePassword = Off;
			if(prmDoSetReferencePassword.equalsIgnoreCase("true")){
				doSetReferencePassword = On;
			}
			AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_REFPASS_ONOFF"), doSetReferencePassword);
		} else {
			AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_REFPASS_ONOFF"), 0);
		}

		if(!StringUtils.isBlank(prmReferencePassword)){
			String referencePassword = prmReferencePassword;
			AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_REFPASS"), referencePassword);
		} else {
			AppObjectUtil.deleteAttribute(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_REFPASS"));
		}

		if(!StringUtils.isBlank(prmForbidPrint)){
			int forbidPrint = Off;
			if(prmForbidPrint.equalsIgnoreCase("true")){
				forbidPrint = On;
			}
			AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_FORBPRINT"), forbidPrint);
		} else {
			AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_FORBPRINT"), 0);
		}

		if(!StringUtils.isBlank(prmForbidEdit)){
			int forbidEdit = Off;
			if(prmForbidEdit.equalsIgnoreCase("true")){
				forbidEdit = On;
			}
			AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_FORBEDIT"), forbidEdit);
		} else {
			AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_FORBEDIT"), 0);
		}

		if(!StringUtils.isBlank(prmForbidAnnotate)){
			int forbidAnnotate = Off;
			if(prmForbidAnnotate.equalsIgnoreCase("true")){
				forbidAnnotate = On;
			}
			AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_FORBADDREM"), forbidAnnotate);
		} else {
			AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_FORBADDREM"), 0);
		}

		if(!StringUtils.isBlank(prmForbidReproduce)){
			int forbidReproduce = Off;
			if(prmForbidReproduce.equalsIgnoreCase("true")){
				forbidReproduce = On;
			}
			AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_FORBREPR"), forbidReproduce);
		} else {
			AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_FORBREPR"), 0);
		}
		return;
	}
	/**
	 * 渡されたワークフロー公開処理オブジェクトの
	 * 「ワークフロー公開処理PDF署名実施フラグ」を設定する
	 * @param  sess                    EIMSession
	 * @param  wfpubObj                ワークフロー公開処理オブジェクト
	 * @param  prmDoSignAndSetSecurity true/false
	 */
	public static void setDoSignAndSetSecurityConfig(
			EIMSession sess,
			EIMObject wfpubObj,
			String prmDoSignAndSetSecurity ) throws Exception {
		int On = 1, Off = 0;
		int doSignAndSetSecurity = Off;
		if(prmDoSignAndSetSecurity.equalsIgnoreCase("true")){
			doSignAndSetSecurity = On;
		}
		AppObjectUtil.setAttr(sess, wfpubObj, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_FLG"),
				doSignAndSetSecurity);
	}
	/**
	 * オブジェクトから,それに適用されるWFの「ワークフロー公開処理」オブジェクトを返す
	 * @param sess
	 * @param obj
	 * @return
	 */
	public static EIMObject getWfPubObjFromDocObj(EIMSession sess, EIMObject obj)
	throws Exception {
		// 戻り値の EIMObject (ワークフロー公開処理オブジェクト)
		EIMObject ret = null;
		// EIMObject → status → statusType
		EIMObjectType objTypeWorkflowPub = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_WFPUB"));
		EIMStatus stObj = obj.getStatus();
		EIMStatusType stTypeObj = stObj.getType();
		// ワークフロー取得
		EIMWorkFlow wkObj = WorkFlowUtils.getWorkFlowByStatusType(sess, stTypeObj);
		// ステータスタイプ｢公開処理中｣をキーに公開処理オブジェクトを取得する
		List stList = wkObj.getStatusTypeList();
		for( Iterator i = stList.iterator(); i.hasNext(); ) {
			stTypeObj = (EIMStatusType)i.next();
			if(stTypeObj.getKind() == AppConstant.STATUS_TYPE_KIND_ID_PROCESSING_PUBLIC){
				ret = ObjectUtils.getObjectByTypeAndName(sess, objTypeWorkflowPub, String.valueOf(stTypeObj.getId()));
				break;
			}
		}
		return ret;
	}
	/**
	 * 渡されたオブジェクトが PDF (または PDF に変換される) かを返す
	 * ・その紐づくワークフローのPDF 変換実施フラグが ON の場合
	 *   → PDF 変換対象拡張子を持つ or PDF : true
	 * ・その紐づくワークフローのPDF 変換実施フラグが OFF の場合
	 *   → このドキュメントの拡張子が PDF : true
	 * 上記以外は false を返す
	 * @param sess   セッション
	 * @param object ドキュメントオブジェクト
	 * @return true -- PDF, false -- PDF でない
	 * @throws Exception
	 */
	public static boolean isPDFDocument(EIMSession sess, EIMObject object)
	throws Exception {
		// ワークフロー公開処理オブジェクト
		EIMObject wfPubObj = PublishAddonUtils.getWfPubObjFromDocObj(sess, object);
		// PDF 変換実施フラグ (これが OFF=0 なら, 拡張子は PDF でないとエラー)
		long isPDFConvert = PublishAddonUtils.getDoConvertPDFConfig(sess, wfPubObj);

		EIMFormat format = FileUtils.getDefaultFormat(sess, object.getType());
		EIMFile file = null;

		boolean isFolder = false;
		boolean isPDFConvertExt = false;
		boolean isPDFFile = false;

		//設定ファイルからPDF可能ファイル拡張子を取得
		String convert_file_type = EIMConfig.get("PDF_CONVERT_FILE_TYPE");
		String[] convFileTypeArray = convert_file_type.split(",");

		if (format == null) {
			//フォルダ
			isFolder = true;
		} else {
			// 拡張子のチェック
			file = FileUtils.getFile(sess, object, format);
			if (file == null) {
				//フォルダ
				isFolder = true;
			} else {
				//PDF変換対象か判定
				for (int i = 0; i < convFileTypeArray.length; i++) {
					if(file.getExt().equalsIgnoreCase("."+convFileTypeArray[i])) {
						isPDFConvertExt = true;
						break;
					}
				}
				if(file.getExt().equalsIgnoreCase(EIMConfig.get("PDF_EXT"))) {
					isPDFFile = true;
				}
			}
		}
		if( isFolder == false && isPDFConvertExt == false && isPDFFile == false ) {
			// 選択対象がフォルダでなく, PDF または 変換対象拡張子でもない
			return false;
		}
		if( isFolder == false && isPDFConvert == 0 && isPDFFile == false ) {
			// 選択対象がフォルダでなく, PDF 変換をしない設定で, かつ PDF でない
			return false;
		}
		if( isFolder == true ) {
			List chldObjectList = AppObjectUtil.getChildEIMObjectRecurrently(sess,
				object, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));
			boolean isPDFConvertExt2 = false; // 変換対象拡張子かどうか
			boolean isPDFFile2 = false;       // PDF ファイルかどうか
			boolean isPassed = false;         // 条件に合致するファイルが一つでもあるかどうか
			AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
			// (PDF変換なしの場合: PDF ファイルがある  PDF変換ありの場合: 変換対象拡張子またはPDFファイルがある)
			for (Iterator i = chldObjectList.iterator(); i.hasNext();) {
				isPDFConvertExt2 = false;
				isPDFFile2 = false;
				EIMObject childObj = (EIMObject) i.next();
				// フォルダの場合は読み飛ばす
				if(helper.isTypeOfFolder(childObj.getType())){
					continue;
				}
				EIMFormat childFormat = FileUtils.getDefaultFormat(sess, childObj.getType());
				EIMFile childFile = FileUtils.getFile(sess, childObj, childFormat);
				for (int j = 0; j < convFileTypeArray.length; j++) {
					if(childFile.getExt().equalsIgnoreCase("."+convFileTypeArray[j])) {
						isPDFConvertExt2 = true;
						break;
					}
				}
				if(childFile.getExt().equalsIgnoreCase(EIMConfig.get("PDF_EXT"))) {
					isPDFFile2 = true;
				}
				if( isPDFConvert == 1 ) {
					// PDF 変換する設定の場合, 拡張子が 変換対象 or PDF で TRUE
					if( isPDFConvertExt2 == true || isPDFFile2 == true ) {
						isPassed = true;
						break;
					}
				} else {
					// PDF 変換しない設定の場合, 拡張子が PDF のときのみ TRUE
					if( isPDFFile2 == true ) {
						isPassed = true;
						break;
					}
				}
			}
			if( isPassed == false ) {
				return false;
			}
		}
		return true;
	}
	
}
