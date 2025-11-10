package common.util;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import org.springframework.context.ApplicationContext;

import common.bo.MessageItem;
//import org.apache.commons.lang.ObjectUtils;
import eim.bo.EIMAccessRole;
import eim.bo.EIMAttribute;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMResource;
import eim.bo.EIMStatus;
import eim.bo.EIMStatusType;
import eim.bo.EIMUser;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.MailUtils;
import eim.util.ObjectUtils;
import eim.util.SecurityUtils;
import eim.util.StringUtils;
import eim.util.UserUtils;
import eim.util.WorkFlowUtils;
import jp.co.ctc_g.eim.framework.business.domain.EventHistoryDomain;
import jp.co.ctc_g.eim.framework.business.domain.EventLogDomain;
import jp.co.ctc_g.eim.framework.business.domain.MailDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.AttributeDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework.business.service.EventHistoryService;
import jp.co.ctc_g.eim.framework.business.service.MailService;
import jp.co.ctc_g.eim.framework.common.util.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * 
 * メール関連クラス
 *
 */
public class MailUtil implements AppConstant{
	
	/** 改行コード */
	private static String BR_ = "\n";

	/**
	 * 承認依頼メールを送信します。
	 * 
	 * <li>日本語の内容と英語の内容を併記します。
	 * <li>承認依頼メールにはURL欄に「?objId=xxx」と出力しません。理由は、ログイン直後に承認確認ダイアログが表示されるためです。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param toList 送信先ユーザリスト
	 * @param obj 承認対象ドキュメント
	 * @throws Exception
	 */
	public 	static void sendApproveRequestMail(EIMSession sess, List toList, EIMObject obj) throws Exception
	{
		if (toList == null || toList.size() == 0 || obj == null) return;
		
		String key = "APPROVAL.REQ";
		EIMUser user = getRequester(sess, obj);
		String title = getTitle(key);
		
		// メール送信
		for (Iterator iter = toList.iterator(); iter.hasNext();) {
			EIMUser toUser = (EIMUser)iter.next();
			List languageList = getLanguageList(toUser);

			String body = "";
			for (int i = 0 ; i < languageList.size(); i++)
			{
				String language = (String)languageList.get(i);
				List commentList = new ArrayList();
				List commentaterList = AppObjectUtil.getRequestList(sess, obj.getStatus(), commentList);
				
				body += getMessage(key, language) + BR_;
				body += getPath(sess, key, language, obj);
				body += getName(sess, key, language, obj);
				body += getUser(sess, key, language, user);
				body += getProperty(sess, key, language, obj);
				body += getComment(sess, key, language, commentaterList, commentList);
				body += getURL(key, language) + BR_ + BR_;
			}
			MailUtils.sendEMail(title, body, toUser);	
		}
	}

	/**
	 * 承認依頼メールを「ユーザ毎に複数ドキュメントを1メールにまとめて」送信します。
	 * 
	 * <li>日本語の内容と英語の内容を併記します。
	 * <li>承認依頼メールにはURL欄に「?objId=xxx」と出力しません。理由は、ログイン直後に承認確認ダイアログが表示されるためです。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param deliverList ScheduledMailDeliverオブジェクトのリスト
	 * @throws Exception
	 */
	public 	static void sendApproveRequestMailBySummary(EIMSession sess, List deliverList) throws Exception
	{
		if (deliverList == null || deliverList.size() == 0 ) return;
		
		String key = "APPROVAL.REQ";
		
		HashMap RequestUserMap = new HashMap();	// ユーザ冗長取得回避用のマップ [key]オブジェクトID [value]EIMUser
		
		//ユーザ単位のループ
		for (Iterator iter = deliverList.iterator(); iter.hasNext();) {
			ScheduledMailDeliver deliver = (ScheduledMailDeliver) iter.next();
			String body = "";

			List languageList = getLanguageList( deliver.getUser() );
			//言語単位のループ
			for (Iterator langItr = languageList.iterator(); langItr.hasNext();) {
				String language = (String) langItr.next();
				body += getMessage(key, language) + BR_;
				
				//ドキュメント単位のループ
				for (Iterator objItr = deliver.getObjectList().iterator(); objItr.hasNext();) {
					
					EIMObject obj = (EIMObject) objItr.next();
					
					//起票者の取得
					EIMUser requestUser = null;
					if (RequestUserMap.containsKey(new Long(obj.getId()))) {
						requestUser = (EIMUser)RequestUserMap.get(new Long(obj.getId()));
					} else {
						requestUser = getRequester(sess, obj);
						RequestUserMap.put(new Long(obj.getId()), requestUser);
					}
	
					List commentList = new ArrayList();
					List commentaterList = AppObjectUtil.getRequestList(sess, obj.getStatus(), commentList);
					
					body += getPath(sess, key, language, obj);
					body += getName(sess, key, language, obj);
					body += getUser(sess, key, language, requestUser);
					body += getProperty(sess, key, language, obj);
					body += getComment(sess, key, language, commentaterList, commentList);
					body += getURL(key, language) + BR_ + BR_;
				}
			}
			
			//メール送信
			MailUtils.sendEMail(getTitle(key), body, deliver.getUser());
		}
	}
	
	/**
	 * 承認通知メールを送信します。
	 * 
	 * <li>日本語の内容と英語の内容を併記します。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param toList 送信先ユーザリスト
	 * @param obj 承認対象ドキュメント
	 * @throws Exception
	 */
	public 	static void sendApprovedMail(EIMSession sess, List toList, EIMObject obj) throws Exception
	{
		if (toList == null || toList.size() == 0 || obj == null) return;
		
		String key = "APPROVAL";
		EIMUser user = getCreater(sess, obj);
		user = (user == null ? obj.getCreateUser() : user);
		String title = getTitle(key);
		
		// メール送信
		for (Iterator iter = toList.iterator(); iter.hasNext();) {
			EIMUser toUser = (EIMUser)iter.next();
			List languageList = getLanguageList(toUser);
			
			String body = "";
			for (int i = 0 ; i < languageList.size(); i++)
			{
				String language = (String)languageList.get(i);
				List dateList = new ArrayList();
				List commentList = new ArrayList();
				List commentaterList = AppObjectUtil.getApprovedList(sess, obj.getStatus(), dateList, commentList);
				
				body += getMessage(key, language, new Object[]{getUserName(sess, sess.getUser(), language)}) + BR_;
				body += getPath(sess, key, language, obj);
				body += getName(sess, key, language, obj);
				body += getUser(sess, key, language, user);
				body += getProperty(sess, key, language, obj);
				body += getComment(sess, key, language, commentaterList, commentList);
				body += getURL(key, language, obj) + BR_ + BR_;
			}
			MailUtils.sendEMail(title, body, toUser);	
		}
	}

	/**
	 * 公開通知メールを送信します。
	 * 
	 * <li>日本語の内容と英語の内容を併記します。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param toList 送信先ユーザリスト
	 * @param obj 承認対象ドキュメント
	 * @throws Exception
	 */
	public 	static void sendPublishedMail(EIMSession sess, List toList, EIMObject obj) throws Exception
	{
		if (toList == null || toList.size() == 0 || obj == null) return;
		
		String key = "PUBLIC";
		EIMUser user = getCreater(sess, obj);
		String title = getTitle(key);
		
		// メール送信
		for (Iterator iter = toList.iterator(); iter.hasNext();) {
			EIMUser toUser = (EIMUser)iter.next();
			
			//ユーザーがドキュメントに対して公開読取権を持つかどうかチェックする
			//公開読取権を持たない場合はメールの送信を行わない
			if( !SecurityUtils.authorized(sess, obj, toUser, EIMAccessRole.READ) ) {
				continue;
			}

			List languageList = getLanguageList(toUser);

			String body = "";
			for (int i = 0 ; i < languageList.size(); i++)
			{
				String language = (String)languageList.get(i);
				
				body += getMessage(key, language) + BR_;
				body += getPath(sess, key, language, obj);
				body += getName(sess, key, language, obj);
				body += getUser(sess, key, language, user);
				body += getProperty(sess, key, language, obj);
				body += getPublicComment(sess, key, language, obj);
				body += getURL(key, language, obj) + BR_ + BR_;
			}			
			MailUtils.sendEMail(title, body, toUser);
		}
	}
	
	/**
	 * 公開通知メールを「ユーザ毎に複数ドキュメントを1メールにまとめて」送信します。
	 * 
	 * <li>日本語の内容と英語の内容を併記します。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param deliverList ScheduledMailDeliverオブジェクトのリスト
	 * @throws Exception
	 */
	public 	static void sendPublishedMailBySummary(EIMSession sess, List deliverList) throws Exception
	{
		if (deliverList == null || deliverList.size() == 0) return;
		
		String key = "PUBLIC";
		HashMap createUserMap = new HashMap();	// ユーザ冗長取得回避用のマップ [key]オブジェクトID [value]EIMUser
		
		//ユーザ単位のループ
		for (Iterator iter = deliverList.iterator(); iter.hasNext();) {
			ScheduledMailDeliver deliver = (ScheduledMailDeliver) iter.next();
			String body = "";
			int docCount = 0;

			List languageList = getLanguageList( deliver.getUser() );
			//言語単位のループ
			for (Iterator langItr = languageList.iterator(); langItr.hasNext();) {
				String language = (String) langItr.next();
				body += getMessage(key, language) + BR_;
				
				//ドキュメント単位のループ
				for (Iterator objItr = deliver.getObjectList().iterator(); objItr.hasNext();) {
					
					EIMObject obj = (EIMObject) objItr.next();

					//ユーザーがドキュメントに対して公開読取権を持つかどうかチェックする
					//公開読取権を持たない場合はメールの送信を行わない
					if( !SecurityUtils.authorized(sess, obj, deliver.getUser(), EIMAccessRole.READ) ) {
						continue;
					}
					
					//作成者の取得
					EIMUser createUser = null;
					if (createUserMap.containsKey(new Long(obj.getId()))) {
						createUser = (EIMUser)createUserMap.get(new Long(obj.getId()));
					} else {
						createUser = getCreater(sess, obj);
						createUserMap.put(new Long(obj.getId()), createUser);
					}
					
					body += getPath(sess, key, language, obj);
					body += getName(sess, key, language, obj);
					body += getUser(sess, key, language, createUser);
					body += getProperty(sess, key, language, obj);
					body += getURL(key, language, obj) + BR_ + BR_;
					
					docCount++;
				}
			}
			//ユーザーについて公開通知メールを送信すべきドキュメントが1つも存在しない場合
			if( docCount == 0 )
				continue;
			//メール送信
			MailUtils.sendEMail(getTitle(key), body, deliver.getUser());
		}
	}

	/**
	 * 差戻し通知メールを送信します。
	 * 
	 * <li>日本語の内容と英語の内容を併記します。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param toList 送信先ユーザリスト
	 * @param obj 承認対象ドキュメント
	 * @throws Exception
	 */
	public 	static void sendReturnedMail(EIMSession sess, List toList, EIMObject obj) throws Exception
	{
		if (toList == null || toList.size() == 0 || obj == null) return;
		
		String key = "SEND.BACK";
		EIMUser user = getCreater(sess, obj);
		String title = getTitle(key);
		
		// メール送信
		for (Iterator iter = toList.iterator(); iter.hasNext();) {
			EIMUser toUser = (EIMUser)iter.next();
			List languageList = getLanguageList(toUser);
	
			String body = "";
			for (int i = 0 ; i < languageList.size(); i++)
			{
				String language = (String)languageList.get(i);
				List commentList = new ArrayList();
				List commentaterList = AppObjectUtil.getReturnList(sess, obj.getStatus(), commentList);
				
				body += getMessage(key, language, new Object[]{getUserName(sess, sess.getUser(), language)}) + BR_;
				body += getPath(sess, key, language, obj);
				body += getName(sess, key, language, obj);
				body += getUser(sess, key, language, user);
				body += getProperty(sess, key, language, obj);
				body += getComment(sess, key, language, commentaterList, commentList);
				body += getURL(key, language, obj) + BR_ + BR_;
			}			
			MailUtils.sendEMail(title, body, toUser);
		}
	}	
	
	/**
	 * 受信確認メールを送信します。
	 * 
	 * <li>日本語の内容と英語の内容を併記します。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param toUser 送信先ユーザ
	 * @param obj 承認対象ドキュメント
	 * @throws Exception
	 */
	public 	static void sendReceptConfirmMail(EIMSession sess, EIMUser toUser, EIMObject obj) throws Exception
	{
		if (toUser == null || obj == null) return;
		
		String key = "RECEPTION";
		String body = "";
		EIMUser user = getCreater(sess, obj);
		
		List languageList = getLanguageList(toUser);
		for (int i = 0 ; i < languageList.size(); i++)
		{
			String language = (String)languageList.get(i);
			
			body += getMessage(key, language, new Object[]{getUserName(sess, sess.getUser(), language)}) + BR_;
			body += getPath(sess, key, language, obj);
			body += getName(sess, key, language, obj);
			body += getUser(sess, key, language, user);
			body += getProperty(sess, key, language, obj);
			body += getRefDate(key, language) + BR_ + BR_;
		}

		// メール送信
		MailUtils.sendEMail(getTitle(key), body, toUser);
	}	

	/**
	 * 承認依頼取り消し通知メールを送信します
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param toList 送信先ユーザリスト
	 * @param obj 承認対象ドキュメント
	 * @throws Exception
	 */
	public 	static void sendRequestCancelMail(EIMSession sess, List toList, EIMObject obj) throws Exception
	{
		if (toList == null || toList.size() == 0 || obj == null) return;
		
		String key = "CANCEL";
		EIMUser user = getCreater(sess, obj);
		if (user == null)
		{
			user = obj.getCreateUser();
		}
		String title = getTitle(key);
		
		// メール送信
		for (Iterator iter = toList.iterator(); iter.hasNext();) {
			EIMUser toUser = (EIMUser)iter.next();
			List languageList = getLanguageList(toUser);
			
			String body = "";
			for (int i = 0 ; i < languageList.size(); i++)
			{
				String language = (String)languageList.get(i);
				
				body += getMessage(key, language, new Object[]{getUserName(sess, sess.getUser(), language)}) + BR_;
				body += getPath(sess, key, language, obj);
				body += getName(sess, key, language, obj);
				body += getUser(sess, key, language, user);
				body += getProperty(sess, key, language, obj);
				body += getURL(key, language, obj) + BR_ + BR_;
			}			
			MailUtils.sendEMail(title, body, toUser);
		}
	}
	
	/**
	 * 公開ファイル結合処理失敗メールを送信します。
	 * 
	 * <li>日本語の内容と英語の内容を併記します。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param toUser 送信先ユーザ
	 * @param obj 通知対象ドキュメント
	 * @param joinObj PDF結合オブジェクト
	 * @param orgFiles 結合元ファイル
	 * @param errMsg エラー内容
	 * @throws Exception
	 */
	public 	static void sendPDFJoinFailedMail(EIMSession sess, EIMUser toUser, EIMObject obj, EIMObject joinObj, String orgFiles, String errMsg) throws Exception
	{
		if (toUser == null) return;
		
		String key = "JOIN.FAILED";
		String body = "";
		
		/* 親オブジェクトの属性 */
		EIMAttribute parentId = joinObj.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_PARENT_ID"));
		/* 親オブジェクト */
		EIMObject parentObj = ObjectUtils.getObjectById(sess, parentId.getInt());
		
		List languageList = getLanguageList(toUser);
		for (int i = 0 ; i < languageList.size(); i++)
		{
			String language = (String)languageList.get(i);
			
			body += getMessage(key, language) + BR_;
			body += getURL(key, language, obj) + BR_ + BR_;
			body += ((EIMResource.getMessage(language, "EIM.MAIL." + key + ".NAME") 
					+ StringUtils.getFileBody(joinObj.getName())));
			if(obj != null)
			{
				body += ("(" + String.valueOf(obj.getId()) + ")" + BR_);
			}
			else
			{
				body += "(-)" + BR_;
			}
			body += (EIMResource.getMessage(language, "EIM.MAIL." + key + ".ORIGINAL") + orgFiles + BR_);
			//親オブジェクト名を「出力先」として取得
			body += EIMResource.getMessage(language, "EIM.MAIL." + key + ".PATH");
			if(parentObj != null && SecurityUtils.authorized(sess, parentObj, sess.getUser(), EIMAccessRole.READ))
			{
				body += (parentObj.getName() + "(" + String.valueOf(parentId.getInt()) + ")" + BR_);
			}
			else
			{
				body += " -(" + String.valueOf(parentId.getInt()) + ")" +  BR_;
			}
			body += (EIMResource.getMessage(language, "EIM.MAIL." + key + ".ERROR")
					+ errMsg + BR_ + BR_);
		}

		// メール送信
		MailUtils.sendEMail(getTitle(key), body, toUser);
	}	

	/**
	 * PDF署名処理失敗通知を送信します。
	 * 
	 * @param sess EIMセッション
	 * @param toUser 通知対象ユーザオブジェクト
	 * @param obj 通知対象ドキュメント
	 * @param errMsg エラー内容
	 * @throws Exception
	 */
	public static void sendPDFSignFailedMail(EIMSession sess, EIMUser toUser, EIMObject obj, String errMsg) throws Exception
	{
		if (toUser == null || obj == null) return;
		
		String key = "SIGNATURE.FAILED";
		String body = "";
		
		List languageList = getLanguageList(toUser);
		for (int i = 0 ; i < languageList.size(); i++)
		{
			String language = (String)languageList.get(i);
			
			body += getMessage(key, language) + BR_;
			body += getURL(key, language, obj) + BR_ + BR_;
			body += EIMResource.getMessage(language, "EIM.MAIL." + key + ".NAME");
			body += (StringUtils.getFileBody(obj.getName()) + "(" + String.valueOf(obj.getId()) + ")" + BR_);
			body += EIMResource.getMessage(language, "EIM.MAIL." + key + ".ERROR") + errMsg + BR_ + BR_;
		}
		// メール送信
		MailUtils.sendEMail(getTitle(key), body, toUser);
	}
	
	/**
	 * 署名・暗号化バッチ失敗通知を送信します。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param toUser 通知対象ユーザ
	 * @param msgItemList MessageItemのリスト
	 * @throws Exception
	 */
	public static void sendSignEncryptionFailedMail(EIMSession sess, EIMUser toUser, List msgItemList) throws Exception {
		
		String key = "SIGNANDENCR.FAILED";
		String body = "";
		
		List languageList = getLanguageList(toUser);
		
		for (int i = 0 ; i < languageList.size() ; i++) {
			String language = (String)languageList.get(i);
			// 本文
			body += getMessage(key, language) + BR_;
		}
		
		// 該当ドキュメント毎ループ
		for (int i = 0 ; i < msgItemList.size() ; i++) {
			MessageItem msgItem = (MessageItem)msgItemList.get(i);
			EIMObject obj = msgItem.getEimObject();
			
			// 言語毎ループ : メール受信言語が存在する場合はその言語のみ、存在しない場合は全ての言語で出力
			for (int j = 0 ; j < languageList.size() ; j++) {
				String language = (String)languageList.get(j);
				
				// URL
				body += getURL(key, language, obj);
				// パス
				body += getPath(sess, key, language, obj);
				// 名称
				body += getNameId(sess, key, language, obj);
				// 失敗内容
				body += EIMResource.getMessage(language, "EIM.MAIL." + key + ".ERROR") + msgItem.getMessage(language) + BR_;
				if (j < languageList.size() - 1) {
					// 見易さを考慮して言語の境目は改行
					body += BR_;
				}
			}
			if (i < msgItemList.size() - 1) {
				// ドキュメントの境目の区切り線
				body += "----------" + BR_;
			}
		}
		
		// メールの件名を出す言語を設定
		String language = toUser.getLang();
		if (language == null) {
			language = LANG_VALUE_EN;
		}
		
		// メール送信
		MailUtils.sendEMail(getTitle(key, language), body, toUser);
	}
	
	/**
	 * 公開ファイル比較処理完了通知を送信します。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param toUser 通知対象ユーザ
	 * @param pdfCompObj 公開ファイル比較オブジェクト
	 * @param orgDocObj 比較元ドキュメントオブジェクト
	 * @param targetDocObj 比較対象ドキュメントオブジェクト
	 * @throws Exception
	 */
	public static void sendPDFCompareCompletedMail(EIMSession sess, EIMUser toUser, EIMObject pdfCompObj,
			EIMObject orgDocObj, EIMObject targetDocObj) throws Exception 
	{
		if (toUser == null || pdfCompObj == null) return;
		
		String key = "COMPARE";
		String body = "";
		
		List languageList = getLanguageList(toUser);
		
		for (int i = 0 ; i < languageList.size() ; i++) {
			String language = (String)languageList.get(i);
			// 本文
			body += getMessage(key + ".SUCCESS", language) + BR_ + BR_;
			body += EIMResource.getMessage(language, "EIM.MAIL." + key + ".RESULTFILENAME");
			body += pdfCompObj.getName() + BR_;
			body += "---------------------------------------------------------" + BR_;
			body += EIMResource.getMessage(language, "EIM.MAIL." + key + ".ORGFILENAME");
			body += orgDocObj.getName() + BR_;
			body += EIMResource.getMessage(language, "EIM.MAIL." + key + ".TARGETFILENAME");
			body += targetDocObj.getName() + BR_;
		}
		// メール送信
		MailUtils.sendEMail(getTitle(key + ".SUCCESS"), body, toUser);
	}
	/**
	 * 公開ファイル比較処理失敗通知を送信します。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param toUser 通知対象ユーザ
	 * @param message エラーメッセージ
	 * @throws Exception
	 */
	public static void sendPDFCompareFailedMail(EIMSession sess, EIMUser toUser, EIMObject pdfCompObj,
			EIMObject orgDocObj, EIMObject targetDocObj, String message) throws Exception 
	{
		if (toUser == null || pdfCompObj == null) return;
		
		String key = "COMPARE";
		String body = "";
		
		List languageList = getLanguageList(toUser);
		
		for (int i = 0 ; i < languageList.size() ; i++) {
			String language = (String)languageList.get(i);
			// 本文
			body += getMessage(key + ".FAILURE", language, new Object[]{pdfCompObj.getName()}) + BR_ + BR_;
			body += EIMResource.getMessage(language, "EIM.MAIL." + key + ".ORGFILENAME");
			body += orgDocObj.getName() + BR_;
			body += EIMResource.getMessage(language, "EIM.MAIL." + key + ".TARGETFILENAME");
			body += targetDocObj.getName() + BR_;
			body += EIMResource.getMessage(language, "EIM.MAIL." + key + ".FAILURE.REASON") + message + BR_;
		}
		// メール送信
		MailUtils.sendEMail(getTitle(key + ".FAILURE"), body, toUser);
	}
	
	private static List getLanguageList(EIMUser user) throws Exception
	{
		List result = new ArrayList();

		String mailLang = user.getLang();
		
		if( mailLang != null )
		{
			result.add( mailLang );
		}
		else
		{
//			result = EIMXmlConfigLanguage.getLangIdList();
			result.add(EIMConfig.get("DISPLAYLANG"));
		}

		return(result);
	}

	private static String getTitle(String key) throws Exception
	{
		return(EIMResource.getMessage(LANG_VALUE_EN, "EIM.MAIL." + key + ".TITLE"));
	}

	private static String getTitle(String key, String language) throws Exception
	{
		return(EIMResource.getMessage(language, "EIM.MAIL." + key + ".TITLE"));
	}

	private static String getUserName(EIMSession sess, EIMUser user, String language) throws Exception
	{
		return(StringUtils.nullToBlank(UserUtils.getOtherUserName(sess, user.getId(), language)));
	}

	private static String getMessage(String key, String language) throws Exception
	{
		return(EIMResource.getMessage(language, "EIM.MAIL." + key + ".MESSAGE") + BR_);
	}

	private static String getMessage(String key, String language, Object[] params) throws Exception
	{
		return(EIMResource.getMessage(language, "EIM.MAIL." + key + ".MESSAGE", params) + BR_);
	}

	private static String getPath(EIMSession sess, String key, String language, EIMObject obj) throws Exception
	{
		return(EIMResource.getMessage(language, "EIM.MAIL." + key + ".PATH")
			+ (obj != null ? StringUtils.nullToBlank(AppObjectUtil.getPath(obj)) : "") + BR_);
	}

	private static String getName(EIMSession sess, String key, String language, EIMObject obj) throws Exception
	{
		return(EIMResource.getMessage(language, "EIM.MAIL." + key + ".NAME") + obj.getName() + BR_);
	}

	private static String getNameId(EIMSession sess, String key, String language, EIMObject obj) throws Exception
	{
		return(EIMResource.getMessage(language, "EIM.MAIL." + key + ".NAME")
			+ (obj != null ? obj.getName() : "")
			+ "(" + (obj != null ? String.valueOf(obj.getId()) : "-") + ")" + BR_);
	}

	/**
	 * 指定keyのEIM.MAIL.<b><i>key</i></b>.USER設定文字列に、指定userの指定言語上の名前を埋め込んで返す
	 * @param sess セッション
	 * @param key プロパティキーの一部
	 * @param language 言語ID
	 * @param user ユーザー
	 * @return メール本文用USER文字列
	 * @throws Exception 予期せぬ例外
	 */
	private static String getUser(EIMSession sess, String key, String language, EIMUser user) throws Exception
	{
		return (EIMResource.getMessage(language, "EIM.MAIL." + key + ".USER")
				+ getUserName(sess, user, language) + BR_);
	}

	/**
	 * 指定ドキュメントの作成者ユーザーを返す
	 * @param sess セッション
	 * @param obj ドキュメントオブジェクト
	 * @return 作成者属性から導出した作成者ユーザー
	 * @throws Exception 予期せぬ例外
	 */
	public static EIMUser getCreater(EIMSession sess, EIMObject obj) throws Exception
	{
		EIMUser docCreater = null;
		EIMAttribute attrOfDocCreatorUser = obj.getAttribute(EIMConfig
				.get("ATTR_NAME_DOCUMENT_CREATE"));
		if (attrOfDocCreatorUser != null) {
			docCreater = UserUtils.getUserById(sess, attrOfDocCreatorUser.getInt());
		}
		else {
			docCreater = obj.getCreateUser();
		}
		return docCreater;
	}

	/**
	 * 指定ドキュメントの更新者ユーザーを返す
	 * @param sess セッション
	 * @param obj ドキュメントオブジェクト
	 * @return 更新者から導出した更新者ユーザー
	 * @throws Exception 予期せぬ例外
	 */
	public static EIMUser getModifyUser(EIMSession sess, EIMObject obj) throws Exception
	{
		EIMUser docModifyUser = null;
		EIMAttribute attrOfDocModifyUser = obj.getAttribute(EIMConfig
				.get("ATTR_NAME_DOCUMENT_MODIFY_USER"));
		if (attrOfDocModifyUser != null) {
			docModifyUser = UserUtils.getUserById(sess, attrOfDocModifyUser.getInt());
		}
		else {
			docModifyUser = obj.getModifyUser();
		}
		return docModifyUser;
	}

	/**
	 * 指定ドキュメントの起票者(ワークフロー開始者)ユーザーを返す
	 * @param sess セッション
	 * @param obj ドキュメントオブジェクト
	 * @return ワークフロー情報から導出した起票者ユーザー
	 * @throws Exception 予期せぬ例外
	 */
	private static EIMUser getRequester(EIMSession sess, EIMObject obj)
			throws Exception
	{
		// 'editing'ステータスの次のステータスの承認依頼者オブジェクトから依頼者を取得し、'依頼者'とする
		// 取得アルゴリズムはapp/document/object/dspWorkFlow.jspに順ずる
		EIMUser docRequester = null;
		do
		{
			List stTypeList = WorkFlowUtils
					.getWorkFlowByStatusType(sess, obj.getStatus().getType()).getStatusTypeList();

			// find 'editing' next status type
			EIMStatusType stTypeOfEditNext = null;
			for (Iterator i = stTypeList.iterator(); i.hasNext();)
			{
				EIMStatusType stType = (EIMStatusType) i.next();
				if (stType.getKind() == AppConstant.STATUS_TYPE_KIND_ID_EDITTING)
				{
					stTypeOfEditNext = (EIMStatusType) i.next();
					break;
				}
			}
			if (stTypeOfEditNext == null//
					|| stTypeOfEditNext.getStep() > obj.getStatus().getType().getStep())
				break;

			// find status instance
			EIMStatus stOfEditNext = WorkFlowUtils.getStatus(sess, obj, stTypeOfEditNext);

			// find 承認依頼者 object
			List reqFromList = AppObjectUtil.getObjectList(sess, EIMConfig
					.get("OBJECT_TYPE_NAME_REQUEST_FROM"), String.valueOf(stOfEditNext.getId()));
			if (reqFromList.size() == 0)
				break;
			EIMObject reqFromObj = (EIMObject) reqFromList.get(0);

			// find 承認依頼者 object's 承認依頼者 attribute
			long fromUserId = AppObjectUtil.getIntAttr(sess, reqFromObj//
					, EIMConfig.get("ATTR_NAME_REQUEST_FROM_USER")//
					, Integer.MIN_VALUE);
			if (fromUserId == Integer.MIN_VALUE)
				break;

			// find eimuser
			docRequester = UserUtils.getUserById(sess, fromUserId);
		} while (false);
		return docRequester;
	}

	/**			
	 * 指定ドキュメントから、直近の「編集中」ステータスにおける承認依頼ユーザを返す。			
	 * @param sess セッション			
	 * @param obj ドキュメントオブジェクト			
	 * @return ワークフロー情報から導出した承認依頼ユーザー			
	 * @throws Exception 予期せぬ例外			
	 */			
	public static UserDomain getLatestRequester(EIMSession sess, EIMObject obj) throws Exception
	{
		UserDomain Requester = null;
		EventHistoryService eventHistoryService = (EventHistoryService)ApplicationContextLoader.getContext().getBean("eventHistoryServiceWithFromStatus");
		EventHistoryDomain eventHistoryDomain = eventHistoryService.getByObjId(obj.getId());
		List<EventLogDomain> eventList = eventHistoryDomain.getEventLogList();
		//イベントの実行順に取れるので後ろから取得する
		for(int i = eventList.size()-1; i>-1 ; i--){
			EventLogDomain eventLog = eventList.get(i);
			if(eventLog.getEvent().getFromStatus().getStatusType().getStatusTypeKind().getId() == AppConstant.STATUS_TYPE_KIND_ID_EDITTING){
				Requester = eventLog.getEvent().getCUser();
				break;
			}
		}
		return Requester;
	}			

	private static String getProperty(EIMSession sess, String key, String language, EIMObject obj) throws Exception
	{
		return(EIMResource.getMessage(language, "EIM.MAIL." + key + ".PROPERTY")
			+ StringUtils.nullToBlank(AppObjectUtil.getStrAttr(sess, obj, EIMConfig.get("ATTR_NAME_DOCUMENT_PROP"))) + BR_);
	}

	private static String getURL(String key, String language) throws Exception
	{
		return(getURL(key, language, null));
	}

	private static String getURL(String key, String language, EIMObject obj) throws Exception
	{
		String objStr = (obj == null ? "" : "objId=" + obj.getId());
		return(EIMResource.getMessage(language, "EIM.MAIL." + key + ".URL") + EIMConfig.get("DOCUMENT_URL") + EIMConfig.get("QUERY_STRING")  + objStr + BR_);
	}

	private static String getRefDate(String key, String language) throws Exception
	{
		return(EIMResource.getMessage(language, "EIM.MAIL." + key + ".DAY")
			+ StringUtils.getDateStringByFormat(new Date(), EIMResource.getMessage(language, "EIM.FORMAT.DATETIME")) + BR_);
	}

	private static String getComment(EIMSession sess, String key, String language, List commentaterList, List commentList) throws Exception
	{
		String ret = "";
		for (int i = 0 ; i < commentaterList.size(); i++)
		{
			if (commentList.get(i) != null)
			{
				String name = getUserName(sess, (EIMUser)commentaterList.get(i), language);
				ret += EIMResource.getMessage(language, "EIM.MAIL." + key + ".COMMENT", new Object[]{name}) + (String)commentList.get(i) + BR_;
			}
		}
		return(ret);
	}
	
	private static String getPublicComment(EIMSession sess, String key, String language, EIMObject obj) throws Exception
	{
		// メール通知オブジェクトの公開通知コメント属性を取得
		EIMObjectType objTypeMailNotify = ObjectUtils.getObjectTypeByName(sess, EIMConfig.getValue("OBJECT_TYPE_NAME_MAIL_NOTIFY"));
		EIMObject mailNotifyObj = ObjectUtils.getObjectByTypeAndName(sess, objTypeMailNotify,Long.toString( obj.getId()));
				
		String	publicComment = AppObjectUtil.getTextAttr(sess,mailNotifyObj, EIMConfig.get("ATTR_NAME_PUBLIC_COMMENT"));
		
		return(EIMResource.getMessage(language, "EIM.MAIL." + key + ".PUBLIC.COMMENT")
			+ StringUtils.nullToBlank(publicComment) + BR_);
	}

	/**
	 * メール送信処理
	 * 
	 * [特記事項]
	 * 　JSPからの呼出を前提としたメソッドです。
	 * 　v3.0化対応でメール送信方法をServeceクラスを使用するので
	 * 　v3.0以前のメール送信をv3.0用にマッピングしています。
	 * 
	 * @param sess			EIMSession
	 * @param mailTypeKey 	メール種別設定
	 */
	public static void execute(EIMSession sess,String mailTypeKey) throws Exception{
		execute(sess, null, mailTypeKey);
	}

	/**
	 * メール送信処理
	 * 
	 * [特記事項]
	 * 　JSPからの呼出を前提としたメソッドです。
	 * 　v3.0化対応でメール送信方法をServeceクラスを使用するので
	 * 　v3.0以前のメール送信をv3.0用にマッピングしています。
	 * 
	 * @param sess			EIMSession
	 * @param object		EIMObject
	 * @param mailTypeKey 	メール種別設定
	 */
	public static void execute(EIMSession sess, EIMObject object, String mailTypeKey) throws Exception{
		try {
			EIMThreadContext.putEIMSession(sess);
			// メールドメインの設定
			MailDomain mailDomain = new MailDomain();
			ObjectDomain objDomain = new ObjectDomain();
			if(object == null){
				objDomain = new ObjectDomain();
			}
			else{
				objDomain = new ObjectDomain(object);
			}
			//objDomain.setId(object.getId());
			mailDomain.setObjectDomain(objDomain);
			mailDomain.setMailTypeKey(mailTypeKey);
			// メールサービス呼出
			ApplicationContext context = ApplicationContextLoader.getContext();
//			MailService mailServise = (MailService)context.getBean("mailService");
			//modified by lin.chen at 2010/04/06 for ticket#454
			MailService mailServise = (MailService)context.getBean("SERVICE.BUNDLE");
			mailServise.send(mailDomain);
		} catch (Exception e) {
			throw e;
		}
		finally
		{
			EIMThreadContext.removeAll();
		}
	}
	/**
	 * 対象の最終イベントを実行したユーザとコメントを取得する。
	 * @param eventName ベースイベント名
	 * @param mailDomain 対象のイベントドメイン
	 * @param langId 出力言語
	 * @return
	 * @throws Exception
	 */
	public static String[] getMailElementBodyItemUserNameAndComment(MailDomain mailDomain, String langId) throws Exception{
		String[] returnArray = new String[2];
		long objId = mailDomain.getObjectDomain().getId();
		ApplicationContext context = ApplicationContextLoader.getContext();
		EventHistoryService eventHistoryService = (EventHistoryService)context.getBean("eventHistoryServiceWithComment");
		EventHistoryDomain evHist = eventHistoryService.getByObjId(objId);
		List<EventLogDomain> evLogList = evHist.getEventLogList();
		//イベントの実行順に取れるので後ろから取得する
		//一度だけループ
		for(int i = evLogList.size() ; i >= evLogList.size() ; i--){
			EventLogDomain evLog = evLogList.get(i-1);
			EIMUser user = UserUtils.getUserById(EIMThreadContext.getEIMSession(), evLog.getEvent().getCUser().getId());
			
			Object tmp = EIMThreadContext.get("username");
			if(tmp == null) {
				returnArray[0] = UserUtils.getOtherUserName(EIMThreadContext.getEIMSession(), user.getId(), langId);
				EIMThreadContext.put("username", returnArray[0]);
			}
			else {
				returnArray[0] = tmp.toString();
			}
			
			tmp = EIMThreadContext.get("comment_"+String.valueOf(objId));
			if(tmp == null) {
				AttributeDomain attr = evLog.getEvent().getAttribute(EIMConfig.get("ATTR_NAME_REQUEST_FROM_COMMENT"));
				String comment = "";
				if(attr != null){
					AttributeDomain attrComment = evLog.getEvent().getAttribute(EIMConfig.get("ATTR_NAME_REQUEST_FROM_COMMENT"));
					if(attrComment != null){
						String[] comments = (String[])evLog.getEvent().getAttribute(EIMConfig.get("ATTR_NAME_REQUEST_FROM_COMMENT")).getValues();
						if(comments.length == 1)
							comment += comments[0];
					}					
				}
				returnArray[1] = comment;
				EIMThreadContext.put("comment_"+String.valueOf(objId).toString(), returnArray[1]);
			}
			else {
				returnArray[1] = tmp.toString();
			}
			
			break;
		}
		return returnArray;
	}

}