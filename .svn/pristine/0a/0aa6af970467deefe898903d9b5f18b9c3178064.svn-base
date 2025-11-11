package batch;

import java.sql.SQLException;
import java.util.Date;
import java.util.List;

import javax.sql.DataSource;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.context.ApplicationContext;

import addon.PublishCommandAddOnPDFInsertURL;
import common.util.AppConstant;
import common.util.AppMessageUtils;
import common.util.AppObjectUtil;
import eim.bo.EIMAttributeType;
import eim.bo.EIMException;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMResource;
import eim.bo.EIMUser;
import eim.net.EIMSession;
import eim.util.AttributeUtils;
import eim.util.EIMConfig;
import eim.util.ObjectAttributeUtils;
import eim.util.ObjectUtils;
import eim.util.SearchUtils;
import eim.util.StringUtils;
import jp.co.ctc_g.eim.app.document.integration.dao.PDFBatchObjectDao;
import jp.co.ctc_g.eim.framework.business.domain.EventExecDomain;
import jp.co.ctc_g.eim.framework.business.domain.ForcastStatusTypeDomain;
import jp.co.ctc_g.eim.framework.business.domain.MailMethod;
import jp.co.ctc_g.eim.framework.business.domain.NoticeMailDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.StatusDomain;
import jp.co.ctc_g.eim.framework.business.service.EventExecService;
import jp.co.ctc_g.eim.framework.common.util.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.common.context.TransactionContext;
import jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum;
import jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils;
import jp.co.ctc_g.eim.framework2.integration.dao.plugin.DatabasePlugInLoader;
import jp.co.ctc_g.eim.framework2.integration.dao.plugin.DatabasePlugIn.SQLExceptionTypeEnum;

/**
 * URL挿入
 * @author
 */

public class PDFInsertURLWatcher {

	/**
	* Main Function
	* @param args
	* @throws Exception
	*/
	public static void main(String[] args)
	throws Exception
	{
		// Error Logging
		Log log = LogFactory.getLog(PDFInsertURLWatcher.class);
		log.info(" " + EIMResource.getMessage("EIM.INFO.PDFINSERTURLSTART"));

		EIMSession sess = null;
		boolean sessPutFlg = false;
		try
		{
			while(true)
			{
				//Console Session
				//user取得
				EIMUser user = new EIMUser(1, null, null, null, null, null, 255, 0, null);
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
				sess = new EIMSession(user,lang);
				sess.setConnection(ds.getConnection());
				sess.setConsoleMode(true);
				//autoCommitをfalseに設定
				sess.getDBConnection().setAutoCommit(false);
				
				// トランザクション取得
				TransactionContext context1 = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
				EIMThreadContext.putTransactionContext(context1);
				context1.setLangId(sess.getLangId());
				context1.setDBConnection(sess.getDBConnection());
				context1.setUser(ConvertUtils.toUserDomain(sess.getUser()));
				
				if(!sessPutFlg){
					EIMThreadContext.putEIMSession(sess);
					sessPutFlg = true;
				}
				
				//排他ロックに関するObjectDao
				PDFBatchObjectDao batchObjectDao = (PDFBatchObjectDao) context.getBean("objectDaoForPDFBatch");
				
				//Object Type
				EIMObjectType objTypeInsertURL = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_INSERTURL"));
				EIMObjectType objTypePDFDiv = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_DIVIDEPDF"));
				EIMObjectType objTypePDFSig = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_SIGNPDF"));
				EIMObjectType objTypePDFConv = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_CONVPDF"));
				EIMObjectType ocrProcessObjectType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_OCR_PROCESS"));
				
				//Search PDF Insert Object
				List result = SearchUtils.searchObject(	sess,
														objTypeInsertURL,
														null,
														false,
														false,
														-1,//リビジョンは無視
														null,
														null,
														null,
														null,
														null,
														null,
														null,
														null,
														null,
														null);

				//Loop
				for(int i = 0; i < result.size(); i++)
				{
					EIMObject object = null;
					EIMObject pdfInsertURLObj = null;
					int rockResult = 0;

					//PDF Signature Object
					pdfInsertURLObj = (EIMObject)result.get(i);

					//該当ドキュメントにPDF変換と分割オブジェクトが存在する場合
					if(ObjectUtils.getObjectByTypeAndName(sess, objTypePDFConv, pdfInsertURLObj.getName()) != null){
						continue;
					}
					if(ObjectUtils.getObjectByTypeAndName(sess, objTypePDFDiv, pdfInsertURLObj.getName()) != null){
						continue;
					}

					try{
						// 対象文書オブジェクト取得
						object = ObjectUtils.getObjectById(sess, Long.parseLong(pdfInsertURLObj.getName()));
						// 対象文書オブジェクトが存在しない場合
						if(object == null) {
							log.error("対象の文書オブジェクトが存在しません。URL挿入処理を中断します。" + pdfInsertURLObj.getName());

							// PDF関連オブジェクトをすべて削除します。
							deletePDFObj(sess, pdfInsertURLObj);
							sess.commit();
							continue;
						}

						// URL挿入オブジェクトのDBレコードをロック
						rockResult = batchObjectDao.lockObjectById(pdfInsertURLObj.getId());
					}
					catch(Exception e)
					{
						// ロック利用不可(リソースビジー)の場合
						//   Oracle : エラーコード=54
						//   PostgreSQL : SQLステータス=55P03
						if (e instanceof SQLException && DatabasePlugInLoader.getPlugIn().matchesSQLException((SQLException) e, SQLExceptionTypeEnum.LOCK_NOT_AVAILABLE)) {
							log.debug("URL挿入をスキップしました。対象URL挿入オブジェクトが既に排他ロックが掛かっています。ID:" + pdfInsertURLObj.getId() + "  名称: " + pdfInsertURLObj.getName());
							sess.rollback();
						} else {
							log.error(AppMessageUtils.makeLogMessage(0, EIMResource.getMessage("EIM.ERROR.LOGIC.BATCH.OBJECT.LOCK.FAIL"),
									new Object[]{pdfInsertURLObj.getName()}));
							log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
							// PDF関連オブジェクトを全て削除する
							deletePDFObj( sess, pdfInsertURLObj );
							// 「公開処理失敗」フラグ設定
							setFailPubProcAttr(sess, object );
							sess.commit();
						}
						continue;
					}

					// オブジェクトIDのDBレコードが存在しない場合
					if (rockResult == 0) {
						log.info("URL挿入をスキップしました。対象URL挿入オブジェクトが存在しません。ID:" + pdfInsertURLObj.getId() + "  名称: " + pdfInsertURLObj.getName());
						continue;
					}
					// オブジェクトIDのDBレコードが存在し、排他ロックを掛けた場合処理続行
					else if( rockResult == 1 ) {
						// 後続処理を実行
					}

					boolean isSuccess = false;
					try {
						//URL挿入アドオン
						PublishCommandAddOnPDFInsertURL addon = new PublishCommandAddOnPDFInsertURL();
						isSuccess = addon.doPublishCommandBatch(sess, pdfInsertURLObj);
					} catch (Exception e){
						log.error("URL挿入に失敗しました。" + pdfInsertURLObj.getName()) ;
						isSuccess = false;
					}


					//「URL挿入」オブジェクトを削除
					ObjectUtils.deleteObject(sess, pdfInsertURLObj);

					if(!isSuccess)
					{
						// URL挿入に失敗した場合
						// PDF関連オブジェクトをすべて削除
						deletePDFObj(sess, pdfInsertURLObj);
						// 「公開処理失敗」フラグ設定
						setFailPubProcAttr(sess, object);
						sess.commit();
						continue;
					}
					else
					{
							//成功ログ出力
							String sucMsg = (EIMResource.getMessage("EIM.LOG.PDF.URL.COMPLETE")
									+ EIMResource.getMessage("EIM.LOG.PDF.URL.ORIGINALFILE")
									+ StringUtils.getFileBody(object.getName()) + "(" + object.getId() +")");
							log.info(sucMsg);
							
							// 公開処理判定
							// 対象オブジェクトとドキュメント名が同じPDF署名、OCR挿入オブジェクトを検索する
							// Object Type PDFSig
							EIMObject pdfSigObj  = ObjectUtils.getObjectByTypeAndName(sess, objTypePDFSig, String.valueOf(object.getId()));
							// Object Type OCR
							EIMObject ocrObj  = ObjectUtils.getObjectByTypeAndName(sess, ocrProcessObjectType, String.valueOf(object.getId()));
							
							if(pdfSigObj != null || ocrObj != null){
								
								//Commit
								sess.commit();
								continue;
							}
							
							//公開処理
							EventExecService eventExecService = (EventExecService)ApplicationContextLoader.getContext().getBean("eventExecService");
							//予測ドメイン取得
							ForcastStatusTypeDomain forcastStatusType = new ForcastStatusTypeDomain();
							StatusDomain statusDomain = new StatusDomain(object.getStatus());
							forcastStatusType.setObject(new ObjectDomain(object));
							forcastStatusType.getObject().setStatus(statusDomain);
							forcastStatusType.getBaseEventType().setId(AppConstant.BASE_EVENT_TYPE_ID_PUBLIC);
							//各種必要な情報をドメインに代入
							EventExecDomain eventExecDomain = new EventExecDomain();
							//オブジェクト
							ObjectDomain objectDomain = new ObjectDomain(object);
							eventExecDomain.setObject(objectDomain);
							//ベースイベントタイプID
							long baseEventTypeId = AppConstant.BASE_EVENT_TYPE_ID_PUBLIC;
							eventExecDomain.getBaseEventType().setId(baseEventTypeId);

							//ステータスの最終更新日時
							Long mDateLong = objectDomain.getStatus().getMDate().getTime();
							eventExecDomain.getObject().getStatus().setMDate(new Date(mDateLong));


							//メール通知オブジェクト
							EIMObjectType mailNotifyObjType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_MAIL_NOTIFY"));
							EIMObject mailNotifyObj = ObjectUtils.getObjectByTypeAndName(sess, mailNotifyObjType, String.valueOf(object.getId()));
							if(AppObjectUtil.getStrAttrs(sess, mailNotifyObj, EIMConfig.get("ATTR_NAME_MAIL_NOTIFY_PUBLIC_NOTIFY_TO")) != null){
								List mailList = java.util.Arrays.asList(AppObjectUtil.getStrAttrs(sess, mailNotifyObj, EIMConfig.get("ATTR_NAME_MAIL_NOTIFY_PUBLIC_NOTIFY_TO")));
								//即時送信
								if(mailNotifyObj.getAttribute(EIMConfig.get("ATTR_NAME_MAIL_NOTIFY_PUBLIC_TIMING")).getInt() == 0)
								{
									for(int ii = 0; ii < mailList.size(); ii++){
										NoticeMailDomain noticeMailDomain = new NoticeMailDomain();
										noticeMailDomain.getMailType().setId(AppConstant.MAIL_TYPE_ID_PUBLIC);	//	公開通知
										noticeMailDomain.setMailMethod(MailMethod.IMMEDIATE);
										eventExecDomain.getImmediateMailList().add(noticeMailDomain);
									}
								}
								//定時送信
								if(mailNotifyObj.getAttribute(EIMConfig.get("ATTR_NAME_MAIL_NOTIFY_PUBLIC_TIMING")).getInt() == 1)
								{
									for(int ii = 0; ii < mailList.size(); ii++){
										NoticeMailDomain noticeMailDomain = new NoticeMailDomain();
										noticeMailDomain.getMailType().setId(AppConstant.MAIL_TYPE_ID_PUBLIC);	//	公開通知
										noticeMailDomain.setMailMethod(MailMethod.ACCUMULATE);
										eventExecDomain.getAccumlateMailList().add(noticeMailDomain);
									}
								}
								//送信無し
								if(mailNotifyObj.getAttribute(EIMConfig.get("ATTR_NAME_MAIL_NOTIFY_PUBLIC_TIMING")).getInt() == 3){
									for( int ii = 0 ;ii < mailList.size(); ii++){
										NoticeMailDomain noticeMailDomain = new NoticeMailDomain();
										noticeMailDomain.getMailType().setId(AppConstant.MAIL_TYPE_ID_PUBLIC);	//	公開通知
										//メール送信無しの場合はメール種別は無くても良い？(修正次第で変更可能性有り:2010/3/9)
										eventExecDomain.getNothingMailList().add(noticeMailDomain);
									}
								}

							}
							//イベント実行
							eventExecService.doEvent(eventExecDomain);
					}
					//Commit
					sess.commit();
				}
				//Session
				if(sessPutFlg == true){
					EIMThreadContext.removeEIMSession();
					sessPutFlg = false;
				}
				sess.close();

				//Wait
				Thread.sleep(Integer.parseInt(EIMConfig.get("WAIT")));
			}
		}
		catch(EIMException eime)
		{
			log.warn(EIMResource.getMessage("EIM.ERROR.SYSTEMERROR"));
			log.warn(AppMessageUtils.makeLogMessage(eime.getMessage()), eime);
			try{
				if(sess != null){
					sess.rollback();
				}
			}
			catch (Exception se) {
				log.warn(AppMessageUtils.makeLogMessage(se.getMessage()), se);
			}
		}
		catch(Exception e)
		{
			log.warn(EIMResource.getMessage("EIM.ERROR.SYSTEMERROR"));
			log.warn(AppMessageUtils.makeLogMessage(e.getMessage()), e);
			try{
				if(sess != null){
					sess.rollback();
				}
			}
			catch (Exception se) {
				log.warn(AppMessageUtils.makeLogMessage(se.getMessage()), se);
			}
		}
		finally
		{
			try{
				if(sessPutFlg == true){
					EIMThreadContext.removeEIMSession();
					sessPutFlg = false;
				}
				if(sess != null){
					sess.close();
				}
			}
			catch (Exception se) {
				log.warn(AppMessageUtils.makeLogMessage(se.getMessage()), se);
			}
		}
		log.info(" " + EIMResource.getMessage("EIM.INFO.PDFINSERTURLEND"));
	}

		// PDF関連オブジェクト削除
		private static void deletePDFObj(EIMSession sess, EIMObject urlIObj) throws Exception{
			Log log = LogFactory.getLog(PDFConvertWatcher.class);
			try {

				// 署名オブジェクトを削除
				EIMObjectType objTypePDFSig = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_SIGNPDF"));
				EIMObject pdfSigObj  = ObjectUtils.getObjectByTypeAndName(sess, objTypePDFSig, urlIObj.getName());
				if (pdfSigObj != null) {
					ObjectUtils.deleteObject(sess, pdfSigObj);
				}

				// URL挿入オブジェクトを削除
				urlIObj = ObjectUtils.getObjectById(sess, urlIObj.getId());
				if (urlIObj != null) {
					ObjectUtils.deleteObject(sess, urlIObj);
				}

			} catch(Exception e) {
				log.error(" 失敗後の後処理(PDFオブジェクト削除)に失敗しました。  URL挿入オブジェクトID:" + urlIObj.getId() , e);
				throw e;
			}
		}

		// 「公開処理失敗」フラグ設定
		private static void setFailPubProcAttr(EIMSession sess, EIMObject object) throws Exception{
			if (object != null) {
				EIMAttributeType attType = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_PUB_PROC_FAIL"));
				ObjectAttributeUtils.setAttribute(sess, object, attType, 1);
			}
		}
}
