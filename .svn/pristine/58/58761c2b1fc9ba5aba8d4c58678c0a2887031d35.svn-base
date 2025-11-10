package batch;

import java.io.File;
import java.sql.SQLException;
import java.util.Date;
import java.util.List;

import javax.sql.DataSource;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.context.ApplicationContext;

import addon.PublishCommandAddOnPDFSignature;
import batch.util.PDFAttributeUtil;
import common.util.AppConstant;
import common.util.AppMessageUtils;
import common.util.AppObjectUtil;
import eim.bo.EIMAttribute;
import eim.bo.EIMAttributeType;
import eim.bo.EIMException;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMResource;
import eim.bo.EIMSearchConditionCompare;
import eim.bo.EIMSearchConditionGroup;
import eim.bo.EIMSearchConditionIn;
import eim.bo.EIMSearchOperatorEnum;
import eim.bo.EIMSearchResultList;
import eim.bo.EIMSearchSelectEIMObject;
import eim.bo.EIMUser;
import eim.net.EIMSession;
import eim.util.AttributeUtils;
import eim.util.EIMConfig;
import eim.util.ObjectAttributeUtils;
import eim.util.ObjectUtils;
import eim.util.SearchUtils;
import eim.util.StringUtils;
import eim.util.TypeConvertUtils;
import jp.co.ctc_g.eim.app.document.business.service.PublishCommandPDFSignatureHGPScanService;
import jp.co.ctc_g.eim.app.document.business.service.PublishCommandPDFSignatureIoWebDocService;
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
 * PDF署名�
 * @author
 */

public class PDFSignatureWatcher {

	/**
	* Main Function
	* @param args
	* @throws Exception
	*/
	public static void main(String[] args)
	throws Exception
	{
		// Error Logging
		Log log = LogFactory.getLog(PDFSignatureWatcher.class);
		log.info(" " + EIMResource.getMessage("EIM.INFO.PDFSIGNSTART"));

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
				TransactionContext tcontext = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
				jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext.putTransactionContext(tcontext);
				tcontext.setLangId(sess.getLangId());
				tcontext.setDBConnection(sess.getDBConnection());
				tcontext.setUser(ConvertUtils.toUserDomain(sess.getUser()));

				if(!sessPutFlg){
					EIMThreadContext.putEIMSession(sess);
					sessPutFlg = true;
				}

				//排他ロックに関するObjectDao
				PDFBatchObjectDao batchObjectDao = (PDFBatchObjectDao) context.getBean("objectDaoForPDFBatch");

				//Object Type
				EIMObjectType objTypePDFSig = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_SIGNPDF"));
				EIMObjectType objTypePDFDiv = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_DIVIDEPDF"));
				EIMObjectType objTypePDFConv = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_CONVPDF"));
				EIMObjectType objTypeInsertURL = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_INSERTURL"));

				//Search PDF Signature Object
				//検索条件項目インスタンスを生成�
				EIMSearchSelectEIMObject selectTarget = new EIMSearchSelectEIMObject();
				//検索条件グループ作成��
				EIMSearchConditionGroup conds = new EIMSearchConditionGroup(EIMSearchOperatorEnum.AND);
				//条件①：オブジェクトタイプが「PDF署名」��
				EIMAttributeType fieldofObjtype = EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.TYPE;
				EIMSearchConditionCompare cond1 = new EIMSearchConditionCompare(
													EIMSearchOperatorEnum.AND,
													fieldofObjtype,
													EIMSearchOperatorEnum.EQ,
													objTypePDFSig.getId());
				conds.addCondition(cond1);
				//条件②:「PDF署名ステータス」が1または4
				EIMAttributeType pdfSingstattype = AttributeUtils.getAttributeTypeByName
													(sess, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_STATUS"));
				long[] sigstats = {1,4};
				EIMSearchConditionIn cond2 = new EIMSearchConditionIn(
													EIMSearchOperatorEnum.AND,
													pdfSingstattype,
													EIMSearchOperatorEnum.IN,
													TypeConvertUtils.convertToBuildTypeArray(sigstats));
				conds.addCondition(cond2);
				selectTarget.setCondition(conds);

				EIMSearchResultList result = SearchUtils.searchObjects(	sess, selectTarget, null);

				//Loop
				for(int i = 0; i < result.size(); i++)
				{
					EIMObject object = null;
					EIMObject pdfSignObj = null;
					int rockResult = 0;

					//PDF Signature Object
					pdfSignObj = (EIMObject)result.get(i);
					EIMAttribute attrSigstat = pdfSignObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_STATUS"));
					if(attrSigstat == null)
					{
						continue;
					}
					long sigstat = attrSigstat.getInt();

					//該当ドキュメントに「PDF変換」または「PDF分割」オブジェクトが存在する場合
					EIMObject pdfDivObj = ObjectUtils.getObjectByTypeAndName(sess, objTypePDFDiv, pdfSignObj.getName());
					EIMObject pdfConvObj = ObjectUtils.getObjectByTypeAndName(sess, objTypePDFConv, pdfSignObj.getName());
					EIMObject insertURLObj  = ObjectUtils.getObjectByTypeAndName(sess, objTypeInsertURL, pdfSignObj.getName());
					if(pdfDivObj != null || pdfConvObj != null || insertURLObj != null)
					{
						continue;
					}

					// WORKフォルダをクリア
					// 一次フォルダ配下のファイルを削除
					File originalTempFolder = new File(EIMConfig.get("PDF_SIGNATURE_IN_WORK"));
					File publicFileTempFolder = new File(EIMConfig.get("PDF_SIGNATURE_OUT_WORK"));
					File[] originalFiles = originalTempFolder.listFiles();
					for (int ii = 0; ii < originalFiles.length; ii++) {
					  if (originalFiles[ii].exists() && originalFiles[ii].isFile()) {
						  try {
							  originalFiles[ii].delete();
						  } catch ( Exception e) {
							  log.error("inディレクトリのクリアに失敗しました。" +  originalFiles[ii].getPath());
						  }
					  }
					}

					File[] publicFiles = publicFileTempFolder.listFiles();
					for (int ii = 0; ii < publicFiles.length; ii++) {
					  if (publicFiles[ii].exists() && publicFiles[ii].isFile()) {
						  try {
							  publicFiles[ii].delete();
						  } catch ( Exception e) {
							  log.error("outディレクトリのクリアに失敗しました。" +  originalFiles[ii].getPath());
						  }
					  }
					}

					try{
						// 対象文書オブジェクト取得
						object = ObjectUtils.getObjectById(sess, Long.parseLong(pdfSignObj.getName()));
						// 対象文書オブジェクトが存在しない場合
						if(object == null) {
							log.error("対象の文書オブジェクトが存在しません。PDF署名処理を中断します。" + pdfSignObj.getName());

							// PDF関連オブジェクトをすべて削除します。
							deletePDFObj(sess, pdfSignObj);
							sess.commit();
							continue;
						}

						// PDF署名オブジェクトのDBレコードをロック
						rockResult = batchObjectDao.lockObjectById(pdfSignObj.getId());
					}
					catch(Exception e)
					{
						// ロック利用不可(リソースビジー)の場合
						//   Oracle : エラーコード=54
						//   PostgreSQL : SQLステータス=55P03
						if (e instanceof SQLException && DatabasePlugInLoader.getPlugIn().matchesSQLException((SQLException) e, SQLExceptionTypeEnum.LOCK_NOT_AVAILABLE)) {
							log.debug("PDF署名をスキップしました。対象PDF署名オブジェクトが既に排他ロックが掛かっています。ID:" + pdfSignObj.getId() + "  名称: " + pdfSignObj.getName());
							sess.rollback();
						} else {
							log.error(AppMessageUtils.makeLogMessage(0, EIMResource.getMessage("EIM.ERROR.LOGIC.BATCH.OBJECT.LOCK.FAIL"),
									new Object[]{pdfSignObj.getName()}));
							log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
							// PDF関連オブジェクトを全て削除する
							deletePDFObj( sess, pdfSignObj );
							// 「公開処理失敗」フラグ設定
							setFailPubProcAttr(sess, object );
							sess.commit();
						}
						continue;
					}

					// オブジェクトIDのDBレコードが存在しない場合
					if (rockResult == 0) {
						log.info("PDF署名をスキップしました。対象PDF署名オブジェクトが存在しません。ID:" + pdfSignObj.getId() + "  名称: " + pdfSignObj.getName());
						continue;
					}
					// オブジェクトIDのDBレコードが存在し、排他ロックを掛けた場合
					else if( rockResult == 1 ) {
						// 後続処理を続行
					}
					//PDF署名アドオン
					PublishCommandAddOnPDFSignature addon = new PublishCommandAddOnPDFSignature();
					PublishCommandPDFSignatureIoWebDocService ioWebDocService =
							(PublishCommandPDFSignatureIoWebDocService)jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader.getApplicationContext().getBean("publishCommandPDFSignatureIoWebDocService");
					PublishCommandPDFSignatureHGPScanService hgpscanService =
							(PublishCommandPDFSignatureHGPScanService)jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader.getApplicationContext().getBean("publishCommandPDFSignatureHGPScanService");

					boolean isSuccess = false;

					// 電子署名仕様ツールの取得
					String useSignTool = EIMConfig.get("SIGN_USE_TOOL");

					// 電子捺印にYSSのIOWebDocを利用
					if(useSignTool.equals("ioweb")) {
						// IOWebDocを利用した署名処理
						isSuccess = ioWebDocService.doPublishCommandBatch(sess, pdfSignObj);
					}
					// 電子捺印にYSSメイクアップを利用
					else if(useSignTool.equals("normal")) {
						// 既存の署名処理
						isSuccess = addon.doPublishCommandBatch(sess, pdfSignObj);
					}
					// 電子捺印にハイパーギアのHGPScanを利用
					else if(useSignTool.equals("hgpscan")) {
						// HSPCanを利用した署名処理
						isSuccess = hgpscanService.doPublishCommandBatch(sess, pdfSignObj);
					}

					if(!isSuccess)
					{
						// 失敗した場合
						// PDF関連オブジェクトを全て削除する
						deletePDFObj( sess, pdfSignObj );
						// 「公開処理失敗」フラグ設定
						setFailPubProcAttr(sess, object );
						sess.commit();
						continue;
					}


					//ステータスを「2:処理済み」に更新
					EIMAttributeType sigStatType = AttributeUtils.getAttributeTypeByName
									(sess, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SIGN_STATUS"));
					ObjectAttributeUtils.setAttribute(sess, pdfSignObj, sigStatType, 2);

					//初期設定の場合�
					if(sigstat == 1)
					{
						// PDF署名オブジェクト削除
						ObjectUtils.deleteObject(sess, pdfSignObj);

						//成功ログ出力
						String sucMsg = (EIMResource.getMessage("EIM.LOG.PDF.SIGN.INITCOMPLETE")
								+ EIMResource.getMessage("EIM.LOG.PDF.SIGN.FILENAME")
								+ StringUtils.getFileBody(object.getName()) + "(" + object.getId() +")");
						log.info(sucMsg);

						// OCR処理オブジェクトが存在する場合公開処理は行わない
						EIMObjectType ocrProcessObjectType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_OCR_PROCESS"));
						EIMObject ocrObj  = ObjectUtils.getObjectByTypeAndName(sess, ocrProcessObjectType, String.valueOf(object.getId()));
						if(ocrObj != null){
							//Commit
							sess.commit();
							continue;
						}

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
									eventExecDomain.getNothingMailList().add(noticeMailDomain);
								}
							}

						}
						//イベント実行
						eventExecService.doEvent(eventExecDomain);


					}
					//変更の場合�
					else if(sigstat == 4)
					{
						//「PDFセキュリティ設定」オブジェクトの属性値を「PDF署名」オブジェクトにコピー
						//PDFセキュリティ設定オブジェクトを取得
						EIMObjectType objTypePdfSec = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_SECPDF"));
						EIMObject pdfSecObj = ObjectUtils.getObjectByTypeAndName(sess, objTypePdfSec, pdfSignObj.getName());

						PDFAttributeUtil.copyPDFSecToSigObj(sess, pdfSecObj, pdfSignObj);
						//「PDFセキュリティ設定」オブジェクトを削除
						ObjectUtils.deleteObject(sess, pdfSecObj);

						//成功ログ出力�
						String sucMsg = (EIMResource.getMessage("EIM.LOG.PDF.SIGN.CHANGECOMPLETE")
								+ EIMResource.getMessage("EIM.LOG.PDF.SIGN.FILENAME")
								+ StringUtils.getFileBody(object.getName()) + "(" + object.getId() +")");
						log.info(sucMsg);
					}

					//Commit
					sess.commit();
				}
				//Session
				sess.close();
				if(sessPutFlg){
					EIMThreadContext.removeEIMSession();
					sessPutFlg = false;
				}
				//Wait
				Thread.sleep(Integer.parseInt(EIMConfig.get("WAIT")));
			}
		}
		catch(EIMException eime)
		{
			log.warn(EIMResource.getMessage("EIM.ERROR.SYSTEMERROR"));
			log.warn(AppMessageUtils.makeLogMessage(eime.getMessage()), eime);
			try{
				if(sessPutFlg){
					EIMThreadContext.removeEIMSession();
					sessPutFlg = false;
				}
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
				if(sessPutFlg){
					EIMThreadContext.removeEIMSession();
					sessPutFlg = false;
				}
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
				if(sessPutFlg){
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
		log.info(" " + EIMResource.getMessage("EIM.INFO.PDFSIGNEND"));
	}

	// PDF関連オブジェクト削除
	private static void deletePDFObj(EIMSession sess, EIMObject pdfSigObj) throws Exception{
		Log log = LogFactory.getLog(PDFConvertWatcher.class);
		try {

			// 署名オブジェクトを削除
			pdfSigObj = ObjectUtils.getObjectById(sess, pdfSigObj.getId());
			if (pdfSigObj != null) {
				ObjectUtils.deleteObject(sess, pdfSigObj);
			}

		} catch(Exception e) {
			log.error(" PDF変換失敗後の後処理(PDFオブジェクト削除)に失敗しました。 :" + pdfSigObj.getName() + e);
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
