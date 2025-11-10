package batch;

import java.sql.SQLException;
import java.util.Date;
import java.util.List;

import javax.sql.DataSource;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.context.ApplicationContext;

import addon.PublishCommandAddOnPDFConvert;
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
import eim.util.RelationUtils;
import eim.util.SearchUtils;
import eim.util.StringUtils;
import jp.co.ctc_g.eim.app.document.business.service.PdfConverterPlugin;
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
import jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework2.common.context.TransactionContext;
import jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum;
import jp.co.ctc_g.eim.framework2.integration.dao.plugin.DatabasePlugIn.SQLExceptionTypeEnum;
import jp.co.ctc_g.eim.framework2.integration.dao.plugin.DatabasePlugInLoader;


/**
 * PDF変換
 * @author
 */
public class PDFConvertWatcher
{
	/**
	 * デバッグモードです。
	 * コマンドライン引数で指定します。
	 * <ul>
	 * <li>-debug=success:PDF変換処理をスキップします。変換成功時の挙動をシミュレートします。
	 * <li>-debug=failure:PDF変換処理をスキップします。変換失敗時の挙動をシミュレートします。
	 * </ul>
	 */
	public static DebugMode debugMode = DebugMode.OFF;
	
	/**
	 * デバッグモードを表す列挙型です。
	 */
	public enum DebugMode {
		/** 通常実行モード */
		OFF,
		/** デバッグ実行モード(変換成功) */
		SUCCESS,
		/** デバッグ実行モード(変換失敗) */
		FAILURE
	}

	/**
	 * 実行結果コードを表す列挙型です。
	 */
	public enum ResultCode {
		/** 成功 */
		SUCCESS,
		/** 失敗 */
		FAILURE,
		/** スキップ */
		SKIP
	}
	
	/**
	 * Main Function
	 * @param args
	 * @throws Exception
	 */
	public static void main(String[] args)
	throws Exception
	{
		// Error Logging
		Log log = LogFactory.getLog(PDFConvertWatcher.class);
		log.info(" " + EIMResource.getMessage("EIM.INFO.PDFCONVSTART"));

		// コマンドライン引数からデバッグモードを設定する
		for (String arg : args) {
			String[] splitedArg = arg.split("=");
			if (splitedArg.length == 2 && splitedArg[0].equals("-debug") &&  splitedArg[1].equals("success")) {
				// 変換成功モード
				debugMode = DebugMode.SUCCESS;
			} else if (splitedArg.length == 2 && splitedArg[0].equals("-debug") && splitedArg[1].equals("failure")) {
				// 変換失敗モード
				debugMode = DebugMode.FAILURE;
			}
			else {
				log.warn("Usage: java batch.PDFConvertWatcher [-debug=success|failure]");
				System.exit(1);
			}
		}
		
		if (debugMode != DebugMode.OFF) {
			log.info("Debug execution start.");
		}
		
		EIMSession sess = null;
		try
		{
			while(true)
			{

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
				sess.getDBConnection().setAutoCommit(false);
				EIMThreadContext.putEIMSession(sess);

				// Transaction context
				TransactionContext tran = new TransactionContext(ConnectionModeEnum.CONSOLE, new UserDomain(user.getId()), lang);
				tran.setDBConnection(sess.getDBConnection());
				EIMThreadContext.putTransactionContext(tran);

				//排他ロックに関するObjectDao
				PDFBatchObjectDao batchObjectDao = (PDFBatchObjectDao) context.getBean("objectDaoForPDFBatch");

				//Object Type PDFConv
				EIMObjectType objTypePDFConv = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_CONVPDF"));

				//Relation Type Document
				RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));

				//Search PDF Convert Object
				@SuppressWarnings("unchecked")
				List<EIMObject> pdfConvObjList = SearchUtils.searchObject(	sess,
						objTypePDFConv,
						null,
						false,
						false,
						-1,
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
				for(int i = 0; i < pdfConvObjList.size(); i++)
				{
					EIMObject object = null;
					EIMObject pdfConvObj = null;
					int rockResult = 0;
					try{
						//PDF Convert Object
						pdfConvObj = pdfConvObjList.get(i);

						// 対象文書オブジェクト取得
						object = ObjectUtils.getObjectById(sess, Long.parseLong(pdfConvObj.getName()));

						if (object == null) {
							log.error("対象の文書オブジェクトが存在しません。PDF変換処理を中断します。:" + pdfConvObj.getName());
							// PDF関連オブジェクトを全て削除する
							deletePDFObj( sess, pdfConvObj );
							sess.commit();
							continue;
						}

						// PDF変換オブジェクトのDBレコードをロック
						rockResult = batchObjectDao.lockObjectById(pdfConvObj.getId());
					}
					catch(Exception e)
					{
						// ロック利用不可(リソースビジー)の場合
						//   Oracle : エラーコード=54
						//   PostgreSQL : SQLステータス=55P03
						if (e instanceof SQLException && DatabasePlugInLoader.getPlugIn().matchesSQLException((SQLException) e, SQLExceptionTypeEnum.LOCK_NOT_AVAILABLE)) {
							log.debug("PDF変換をスキップしました。対象PDF変換オブジェクトが既に排他ロックが掛かっています。ID:" + pdfConvObj.getId() + "  名称: " + pdfConvObj.getName());
							sess.rollback();
						} else {
							log.warn(AppMessageUtils.makeLogMessage(0, EIMResource.getMessage("EIM.ERROR.LOGIC.BATCH.OBJECT.LOCK.FAIL"),
									new Object[]{pdfConvObj.getName()}));
							log.warn(AppMessageUtils.makeLogMessage(e.getMessage()), e);
							// PDF関連オブジェクトを全て削除する
							deletePDFObj( sess, pdfConvObj );
							// 「公開処理失敗」フラグ設定
							setFailPubProcAttr(sess, object );
							sess.commit();
						}
						continue;
					}

					// オブジェクトIDのDBレコードが存在しない場合
					if (rockResult == 0) {
						log.info("PDF変換をスキップしました。対象PDF変換オブジェクトが存在しません。 ID:" + pdfConvObj.getId() + "  名称: " + pdfConvObj.getName());
						continue;
					}
					// オブジェクトIDのDBレコードが存在し、排他ロックを掛けた場合
					else if( rockResult == 1 ) {
						// 後処理を続行
					}

					// 実行結果コード
					ResultCode resultCode = null;

					// PDF変換実施済みか判定する
					Date previousExecDate = AppObjectUtil.getDateAttr(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_PDF_CONV_EXEC_DATE"));
					Date modifyDate = object.getModifyDate();
					long faildFlag = AppObjectUtil.getIntAttr(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_PUB_PROC_FAIL"), AppConstant.FLAG_OFF);

					// PDF変換失敗状態ではなく、且つ、PDF変換処理実行日時が原本の更新日時より大きい場合、変換対象としない
					if (faildFlag == AppConstant.FLAG_OFF && (previousExecDate != null && previousExecDate.getTime() > modifyDate.getTime())) {
						// スキップ
						resultCode = ResultCode.SKIP;
						String sucMsg = (EIMResource.getMessage("EIM.LOG.PDF.CONVERT.SKIP")
								+ EIMResource.getMessage("EIM.LOG.PDF.CONVERT.ORIGINALFILE")
								+ StringUtils.getFileBody(object.getName()) + "(" + object.getId() +")");
						log.info(sucMsg);
					}

					// 公開PDFが事前登録されているか判定する
					Date pdfPreRegistDate = AppObjectUtil.getDateAttr(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_PDF_PRE_REGIST_DATE"));

					// 公開PDFが事前登録されている場合はPDF変換を実行しない
					if (pdfPreRegistDate != null) {
						// スキップ
						resultCode = ResultCode.SKIP;
						String sucMsg = (EIMResource.getMessage("EIM.LOG.PDF.CONVERT.SKIP")
								+ EIMResource.getMessage("EIM.LOG.PDF.CONVERT.ORIGINALFILE")
								+ StringUtils.getFileBody(object.getName()) + "(" + object.getId() +")");
						log.info(sucMsg);
					}

					// PDF変換処理を実行する
					if (resultCode != ResultCode.SKIP) {
						// PDF変換プラグイン設定
						PublishCommandAddOnPDFConvert addon = new PublishCommandAddOnPDFConvert();
						if (context.containsBean("officePdfConverterPlugin")) {
							PdfConverterPlugin officePdfConverterPlugin = (PdfConverterPlugin)context.getBean("officePdfConverterPlugin");
							addon.setOfficePdfConverterPlugin(officePdfConverterPlugin);
						}
						if (context.containsBean("hgPdfConverterPlugin")) {
							PdfConverterPlugin hgPdfConverterPlugin = (PdfConverterPlugin)context.getBean("hgPdfConverterPlugin");
							addon.setHGPdfConverterPlugin(hgPdfConverterPlugin);
						}

						// PDF変換実行
						if (addon.doPublishCommandBatch(sess, object)) {
							// 成功
							resultCode = ResultCode.SUCCESS;
							String sucMsg = (EIMResource.getMessage("EIM.LOG.PDF.CONVERT.COMPLETE")
									+ EIMResource.getMessage("EIM.LOG.PDF.CONVERT.ORIGINALFILE")
									+ StringUtils.getFileBody(object.getName()) + "(" + object.getId() +")");
							log.info(sucMsg);
						} else {
							// 失敗
							resultCode = ResultCode.FAILURE;
							String sucMsg = (EIMResource.getMessage("EIM.LOG.PDF.CONVERT.FAILED")
									+ EIMResource.getMessage("EIM.LOG.PDF.CONVERT.ORIGINALFILE")
									+ StringUtils.getFileBody(object.getName()) + "(" + object.getId() +")");
							log.info(sucMsg);
						}
					}

					// キュー削除
					try
					{
						//Delete PDF Convert Object
						ObjectUtils.deleteObject(sess, pdfConvObj);

						// PDF変換失敗
						if (resultCode == ResultCode.FAILURE) {
							// PDF関連オブジェクトを全て削除する
							deletePDFObj( sess, pdfConvObj );
							// 「公開処理失敗」フラグ設定
							setFailPubProcAttr(sess, object );
							sess.commit();
							continue;
						}
					}
					catch(Exception dpdfoe)
					{
						log.error(" 予期せぬエラーのため、PDF変換処理に失敗しました。 " + object.getId() + " : " + object.getName(), dpdfoe);

						// PDF関連オブジェクトを全て削除する
						deletePDFObj( sess, pdfConvObj );
						// 「公開処理失敗」フラグ設定
						setFailPubProcAttr(sess, object );
						sess.commit();
						continue;
					}

					// 公開処理判定
					// 「PDF変換」対象オブジェクトとドキュメント名が同じPDF分割処理、PDF署名、URL挿入オブジェクトを検索する
					
					// Object Type PDFDiv
					EIMObjectType objTypePDFDiv = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_DIVIDEPDF"));
					EIMObject pdfDivObj = ObjectUtils.getObjectByTypeAndName(sess, objTypePDFDiv, String.valueOf(object.getId()));
					// Object Type PDFSig
					EIMObjectType objTypePDFSig = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_SIGNPDF"));
					EIMObject pdfSigObj  = ObjectUtils.getObjectByTypeAndName(sess, objTypePDFSig, String.valueOf(object.getId()));
					// Object Type insertURL
					EIMObjectType objTypeInsertURL = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_INSERTURL"));
					EIMObject insertURLObj  = ObjectUtils.getObjectByTypeAndName(sess, objTypeInsertURL, String.valueOf(object.getId()));
					// 他のPDF処理が存在しない場合ステータス遷移しない
					if((pdfDivObj != null) || (pdfSigObj != null) || (insertURLObj != null)) {
						sess.commit();
						continue;
					} 
					// ステータスが「公開処理中」の場合はステータス遷移しない
					if (object.getStatus().getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_PROCESSING_PUBLIC) {
						sess.commit();
						continue;
					}
					
					// ステータスを「公開中」へ遷移させる
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
						List<String> mailList = java.util.Arrays.asList(AppObjectUtil.getStrAttrs(sess, mailNotifyObj, EIMConfig.get("ATTR_NAME_MAIL_NOTIFY_PUBLIC_NOTIFY_TO")));

						//即時送信
						if(mailNotifyObj.getAttribute(EIMConfig.get("ATTR_NAME_MAIL_NOTIFY_PUBLIC_TIMING")).getInt() == 0)
						{
							for(int ii = 0; ii < mailList.size(); ii++){
								NoticeMailDomain noticeMailDomain = new NoticeMailDomain();
								noticeMailDomain.getMailType().setId(AppConstant.MAIL_TYPE_ID_PUBLIC);		//	公開通知
								noticeMailDomain.setMailMethod(MailMethod.IMMEDIATE);
								eventExecDomain.getImmediateMailList().add(noticeMailDomain);
							}
						}
						//定時送信
						if(mailNotifyObj.getAttribute(EIMConfig.get("ATTR_NAME_MAIL_NOTIFY_PUBLIC_TIMING")).getInt() == 1)
						{
							for(int ii = 0; ii < mailList.size(); ii++){
								NoticeMailDomain noticeMailDomain = new NoticeMailDomain();
								noticeMailDomain.getMailType().setId(AppConstant.MAIL_TYPE_ID_PUBLIC);		// 公開通知
								noticeMailDomain.setMailMethod(MailMethod.ACCUMULATE);
								eventExecDomain.getAccumlateMailList().add(noticeMailDomain);
							}
						}
						//送信無し
						if(mailNotifyObj.getAttribute(EIMConfig.get("ATTR_NAME_MAIL_NOTIFY_PUBLIC_TIMING")).getInt() == 3){
							for( int ii = 0 ;ii < mailList.size(); ii++){
								NoticeMailDomain noticeMailDomain = new NoticeMailDomain();
								noticeMailDomain.getMailType().setId(AppConstant.MAIL_TYPE_ID_PUBLIC);		// 公開通知
								eventExecDomain.getNothingMailList().add(noticeMailDomain);
							}
						}
					}
					//イベント実行
					eventExecService.doEvent(eventExecDomain);

					//Commit
					sess.commit();
				}

				//Session
				EIMThreadContext.removeAll();
				sess.close();

				// デバッグモード
				if (debugMode != DebugMode.OFF) {
					// ループせずに終了する
					log.info("Debug execution end.");
					break;
				}
				
				//Wait
				Thread.sleep(Integer.parseInt(EIMConfig.get("WAIT")));
			}
		}
		catch(EIMException eime)
		{
			log.warn(AppMessageUtils.makeLogMessage(eime.getMessage()), eime);
			try{
				EIMThreadContext.removeAll();
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
				EIMThreadContext.removeAll();
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
				EIMThreadContext.removeAll();
				if(sess != null){
					sess.close();
				}
			}
			catch (Exception se) {
				log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
			}
		}
		log.info(" " + EIMResource.getMessage("EIM.INFO.PDFCONVEND"));
	}

	// PDF関連オブジェクト削除
	private static void deletePDFObj(EIMSession sess, EIMObject pdfConvObj) throws Exception{
		Log log = LogFactory.getLog(PDFConvertWatcher.class);
		try {

			// 署名オブジェクトを削除
			EIMObjectType objTypePDFSig = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_SIGNPDF"));
			EIMObject pdfSigObj  = ObjectUtils.getObjectByTypeAndName(sess, objTypePDFSig, pdfConvObj.getName());
			if (pdfSigObj != null) {
				ObjectUtils.deleteObject(sess, pdfSigObj);
			}

			// URL挿入オブジェクトを削除
			EIMObjectType objTypeInsertURL = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_INSERTURL"));
			EIMObject insertURLObj  = ObjectUtils.getObjectByTypeAndName(sess, objTypeInsertURL, pdfConvObj.getName());
			if (insertURLObj != null) {
				ObjectUtils.deleteObject(sess, insertURLObj);
			}

			// PDF分割を削除
			EIMObjectType objTypePDFDivi = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_DIVIDEPDF"));
			EIMObject pdfDiviObj = ObjectUtils.getObjectByTypeAndName(sess, objTypePDFDivi, pdfConvObj.getName());
			if (pdfDiviObj != null) {
				ObjectUtils.deleteObject(sess, pdfDiviObj);
			}

			// PDF変換オブジェクトを削除
			pdfConvObj = ObjectUtils.getObjectById(sess, pdfConvObj.getId());
			if (pdfConvObj != null) {
				// PDF変換対象のドキュメントが物理削除されている時、PDF変換オブジェクトも削除されている
				// その状態でdeleteObject()を呼ぶとシステムエラーとなる
				ObjectUtils.deleteObject(sess, pdfConvObj);
			}

		} catch(Exception e) {
			log.error(" PDF変換失敗後の後処理(PDFオブジェクト削除)に失敗しました。 :" + pdfConvObj.getName() + e);
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