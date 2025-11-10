package batch;

import java.util.ArrayList;
import java.util.List;

import javax.sql.DataSource;

import jakarta.mail.MessagingException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.context.ApplicationContext;

import common.util.AppConstant;
import common.util.AppMessageUtils;
import common.util.AppObjectUtil;
import eim.bo.EIMException;
import eim.bo.EIMResource;
import eim.bo.EIMUser;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.MailUtils;
import eim.util.UserUtils;
import jp.co.ctc_g.eim.framework.business.dao.MailAccumulateDao;
import jp.co.ctc_g.eim.framework.business.domain.MailAccumulateDomain;
import jp.co.ctc_g.eim.framework.common.util.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.common.context.TransactionContext;
import jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum;
import jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils;

/**
 * 定時メール送信クラス。
 * 
 * 以下のメールを送信します。
 * <li>公開通知
 * <li>承認依頼通知
 *
 */
public class ScheduledMail
{
	/** ログ */
	private static Log log = LogFactory.getLog(ScheduledMail.class);

	private static MailAccumulateDao _mailAccumulateDao = null;
	
	/** 定数：件名接続用記号(パイプ) */
	private final static String CONNECTION_PIPE = "｜";
	/** 定数：省略記号(三点リーダ) */
	private final static String ELLIPSIS ="...";
	/** 定数：メール件数表示用 */
	private final static String MAIL_COUNT_LEFT ="（";
	/** 定数：メール件数表示用 */
	private final static String MAIL_COUNT_RIGHT ="件）";

	/**
	 * Main Function
	 * @param args
	 * @throws Exception
	 */
	public static void main(String[] args)
	throws Exception
	{
		//Error Logging
		log.info(" " + EIMResource.getMessage("EIM.INFO.SCHEMAILSTART"));
		
		EIMSession sess = null;
		boolean isRetry = true;	// 再試行フラグ (DB接続)

		try
		{
			
			////////////////////////////////////////////////////////////////////////////////////////////
			//
			// Session
			//
			////////////////////////////////////////////////////////////////////////////////////////////
			
			while (isRetry) {
				try {
					//user取得
					EIMUser sessUser = new EIMUser(1, null, null, null, null, null, 255, 0, null);
					//lang取得
					String lang = "";
					String EIM_CONFIG_LANG = "MESSAGELANG";
					String DEFAULT_LANG	= "JA";
					if(EIMConfig.get(EIM_CONFIG_LANG) != null){
						lang = EIMConfig.get(EIM_CONFIG_LANG);
					}else{
						lang = DEFAULT_LANG;
					}
					//Session
					ApplicationContext context = ApplicationContextLoader.getContext();
					DataSource ds = (DataSource)context.getBean("dataSource");
					sess = new EIMSession(sessUser,lang);
					sess.setConnection(ds.getConnection());
					sess.setConsoleMode(true);
					sess.getDBConnection().setAutoCommit(false);
					EIMThreadContext.putEIMSession(sess);
					
					TransactionContext tx = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
					EIMThreadContext.putTransactionContext(tx);
					tx.setLangId(sess.getLangId());
					tx.setDBConnection(sess.getDBConnection());
					tx.setUser(ConvertUtils.toUserDomain(sessUser));

					isRetry = false;	// DB接続に成功したらループから脱出
				} catch (Exception e) {
					// DB接続でエラーが発生しました。処理中断後、リトライします。
					log.info(EIMResource.getMessage("EIM.ERROR.LOGIC.CANT.GET.DBCON"));
					isRetry = true;
					// DB接続に失敗した場合は一定時間待機する
					Thread.sleep(Integer.parseInt(EIMConfig.get("WAIT")));
				}
			}

			////////////////////////////////////////////////////////////////////////////////////////////
			//
			// 初期化
			//
			////////////////////////////////////////////////////////////////////////////////////////////
			ApplicationContext context = ApplicationContextLoader.getContext();
			_mailAccumulateDao = (MailAccumulateDao)context.getBean("mailAccumulateDao");

			////////////////////////////////////////////////////////////////////////////////////////////
			//
			// 公開通知メール送信
			//
			////////////////////////////////////////////////////////////////////////////////////////////
			log.info(EIMResource.getMessageValue("EIM.INFO.MAIL.TYPE.PUBLIC"));
			MailAccumulateDomain mailAccumulate_req = new MailAccumulateDomain();
			mailAccumulate_req.setMailType("MailPublic");
			
			sendAccumulateMail(mailAccumulate_req);

			////////////////////////////////////////////////////////////////////////////////////////////
			//
			// 承認依頼通知メール送信
			//
			////////////////////////////////////////////////////////////////////////////////////////////
			log.info(EIMResource.getMessageValue("EIM.INFO.MAIL.TYPE.REQUEST.FOR.APPROVAL"));
			mailAccumulate_req = new MailAccumulateDomain();
			mailAccumulate_req.setMailType("MailRequestForApproval");
			
			sendAccumulateMail(mailAccumulate_req);

		}
		catch(EIMException eime)
		{
			log.warn(AppMessageUtils.makeLogMessage(eime.getMessage()), eime);
			try{
				if(sess != null){
					sess.rollback();
				}
			}
			catch (Exception se) {
				log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
			}
		}
		catch(Exception e)
		{
			log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
			try{
				if(sess != null){
					sess.rollback();
				}
			}
			catch (Exception se) {
				log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
			}
		}
		finally
		{
			try{
				if(sess != null){
					sess.close();
				}
			}
			catch (Exception se) {
				log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
			}
		}
		log.info(" " + EIMResource.getMessage("EIM.INFO.SCHEMAILEND"));
		System.exit(0);
	}

	/**
	 * 送信先アドレス単位に指定したMailTypeについてのメールを送信します
	 * @param mailAccumulateDomain
	 * @throws Exception
	 */
	private static void sendAccumulateMail(MailAccumulateDomain mailAccumulateDomain) throws Exception{

		List<MailAccumulateDomain> mailAccumulateList = _mailAccumulateDao.getMailAcmList(mailAccumulateDomain);
		
		if(mailAccumulateList.size() == 0){
			//蓄積されたメールはありません
			log.info(EIMResource.getMessageValue("EIM.MAIL.INFO.SCHEMAIL.NOTEXIST"));
			return;
		}
		
		// アドレス単位に分類
		List<List<MailAccumulateDomain>> sendMailList = separatePerson(mailAccumulateList);
		
		/*
		 * ユーザ単位でメールを送信する。以下の情報を取得しておく
		 * ・送信済みメール
		 * ・送信失敗メールの送付先
		 */
		List<MailAccumulateDomain> sendedList = new ArrayList<MailAccumulateDomain>();
		List<String> failedList = new ArrayList<String>();
		EIMSession sess = EIMThreadContext.getEIMSession();
		String newLine = EIMConfig.getValue("MAIL_NEWLINE");
		
		try {
			
			
			for(List<MailAccumulateDomain> personList : sendMailList) {

				/*--- ユーザ情報の取得 ---*/
				MailAccumulateDomain headDomain;
				if (personList.size() > 0) {
					headDomain = personList.get(0);
				} else {
					continue;
				}
				
				String userAddr = headDomain.getUserAd();
				EIMUser user = UserUtils.getUserByMail(sess, userAddr, false);
				if (user == null || user.getDisable() == AppConstant.INVALID_FLAG_ON) {

					// メールは全て削除対象
					sendedList.addAll(personList);

					if (user == null) {
						// 送信先アドレスが設定されたユーザが存在しない場合はログ出力対象に追加
						failedList.add(userAddr);
					}
					continue;
				}
				
				/*--- メールの作成 ---*/
				String title = headDomain.getTitle();
				StringBuffer buff = new StringBuffer();
				String separator = EIMResource.getMessageValue(user.getLang(), "APP.MAIL.COMMON.SEPARATE") + newLine;
				
				for (int i = 0; i < personList.size(); i++) 
				{
					MailAccumulateDomain mailAccumulate = personList.get(i);
					// 本文と本文の間にセパレータを入れる
					buff.append(mailAccumulate.getMessage());
					
					// 本文と本文の間にセパレータを入れる(先頭と最後には入れない)
					if (i < personList.size() - 1) {
						buff.append(separator);
					}
					
					// 削除対象に追加
					sendedList.add(mailAccumulate);
				}
				
				/*--- メール送信 ---*/
				try{
					MailUtils.sendEMail(title, buff.toString(), userAddr);	

				} catch(MessagingException me) {
					
					// 送信失敗時はログ出力対象
					failedList.add(headDomain.getUserAd());
				} catch(Exception e) {
					throw e;
				}
			}
		} catch (Exception e) {
			
			// 予期せぬ例外時、そのままスルー
			throw e;
			
		} finally {
			
			/*
			 * 送信済みメールのログ出力、削除と
			 * 送信失敗メールのログ出力は必ず行う
			 */
			removeAccumulateMail(sendedList);
			alertFaildMail(failedList);
		}
		
	}
	

	/**
	 * 送信先アドレス単位に分類します。
	 * 
	 * 送信メール一覧から重複している送信先アドレスを削除する。
	 * 
	 * @param mailAccumulateList 送信メール一覧
	 * @return 送信者アドレス別送信メール一覧
	 * @throws Exception
	 */
	@SuppressWarnings("unchecked")
	private static List<List<MailAccumulateDomain>> separatePerson(List<MailAccumulateDomain> mailAccumulateList) throws Exception{
		
		
		List<List<MailAccumulateDomain>> sendMailList = new ArrayList<List<MailAccumulateDomain>>();

		//送信先アドレスでソートする
		mailAccumulateList = AppObjectUtil.getStrSortedList(mailAccumulateList, "getUserAd", true);

		String prevAddr = "";
		List<MailAccumulateDomain> individualList = null;
		for(MailAccumulateDomain mailAccumulate : mailAccumulateList){
			if(!prevAddr.equals(mailAccumulate.getUserAd())){
				prevAddr = mailAccumulate.getUserAd();
				individualList = new ArrayList<MailAccumulateDomain>();
				individualList.add(mailAccumulate);
				sendMailList.add(individualList);
			}else{
				individualList.add(mailAccumulate);
			}
		}
		
		return sendMailList;
		
	}

	/**
	 * 処理済みのメールを、蓄積用DBから削除します。
	 * 
	 * @param mailAccumulateList 処理済みメール一覧
	 * @throws Exception
	 */
	private static void removeAccumulateMail(List<MailAccumulateDomain> mailAccumulateList) throws Exception{

		EIMSession sess = EIMThreadContext.getEIMSession();
		if(mailAccumulateList.size() > 0){

			for(MailAccumulateDomain mailAccumulate : mailAccumulateList){
	
				_mailAccumulateDao.deleteById(mailAccumulate.getId());
				sess.commit();
				
			}
		}

		return;
	}

	/**
	 * メール送信に失敗した送信先アドレスを出力します。
	 * 
	 * @param failedList 送信失敗メール一覧
	 * @throws Exception
	 */
	private static void alertFaildMail(List<String> failedList) throws Exception{

		String[] args = null;
		if(failedList.size() > 0){

			log.error(EIMResource.getMessageValue("EIM.ERROR.LOGIC.FAILEDSENDMAIL"));

			for(String userAddr : failedList){
	
				args = new String[]{ userAddr };
				log.error(EIMResource.getMessageValue("EIM.ERROR.LOGIC.INVALID.ADDRESS", args));
				
			}
		}

		return;
	}
}