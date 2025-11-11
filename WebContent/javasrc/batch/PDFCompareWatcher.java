package batch;

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import javax.sql.DataSource;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.context.ApplicationContext;

import common.util.AppMessageUtils;
import common.util.ChangeSessionHelper;
import common.util.MailUtil;
import eim.bo.EIMAttribute;
import eim.bo.EIMAttributeType;
import eim.bo.EIMException;
import eim.bo.EIMFile;
import eim.bo.EIMFormat;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMRelationType;
import eim.bo.EIMResource;
import eim.bo.EIMUser;
import eim.net.EIMSession;
import eim.util.AccessUtils;
import eim.util.AttributeUtils;
import eim.util.EIMConfig;
import eim.util.FTPUtils;
import eim.util.FileUtils;
import eim.util.ObjectAttributeUtils;
import eim.util.ObjectUtils;
import eim.util.RelationUtils;
import eim.util.TypeConvertUtils;
import jp.co.ctc_g.eim.framework.common.util.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework2.common.context.TransactionContext;
import jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum;


/**
 * PDF公開ファイル比較
 * @author
 */
public class PDFCompareWatcher {

	/** 比較結果出力ファイル */
	private static File resultFile = null;

	/** ダウンロードしたファイル */
	private static List fileList = new ArrayList();

	/** ログ */
	private static Log log = null;

	/** 「ドキュメント」のデフォルトフォーマット */
	private static EIMFormat defaultFormat = null;

	/** 公開フォーマット */
	private static EIMFormat publicFormat = null;

	/**
	* Main Function
	* @param args
	* @throws Exception
	*/
	public static void main(String[] args)
	throws Exception
	{
		// Error Logging
		log = LogFactory.getLog(PDFCompareWatcher.class);
		log.info(" " + EIMResource.getMessage("EIM.INFO.PDFCOMPARE.START"));

		EIMSession sess = null;
		boolean isRetry = true;	// 再試行フラグ (DB接続)

		try
		{

			while(true)
			{
				publicFormat = null;
				defaultFormat = null;

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

				// Transaction context
				TransactionContext tran = new TransactionContext(ConnectionModeEnum.CONSOLE, new UserDomain(user.getId()), lang);
				tran.setDBConnection(sess.getDBConnection());
				EIMThreadContext.putTransactionContext(tran);

				//get Public Format
				publicFormat  = FileUtils.getFormatByName(sess, EIMConfig.get("FORMAT_NAME_PUBLIC"));
				if(publicFormat == null)
				{
					doErrorActForBreak(sess, EIMResource.getMessage("EIM.ERROR.LOGIC.PUBLICFORMAT.NOTFOUND"));
					break;
				}

				//Object Type 「PDF公開文書比較」
				EIMObjectType objTypePDFCompare = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_DOC_COMPARE"));

				//Object Type 「ドキュメント」
				EIMObjectType objTypedocType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT"));
				if(objTypedocType == null)
				{
					doErrorActForBreak(sess, EIMResource.getMessage("EIM.ERROR.LOGIC.DOCTYPE.NOTFOUND"));
					break;
				}

				// 「ドキュメント」のデフォルトフォーマットを取得
				defaultFormat = FileUtils.getDefaultFormat(sess, objTypedocType);
				if(defaultFormat == null)
				{
					doErrorActForBreak(sess, EIMResource.getMessage("EIM.ERROR.LOGIC.NODEFAULTFORM.WITHTYPENAME", new Object[]{objTypedocType.getName()}));
					break;
				}

				//Relation Type 「ドキュメント」
				EIMRelationType RelTypeDoc = RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));

				//Object Type 「マイドキュメント」
				EIMObjectType objTypeMyDoc = ObjectUtils.getObjectTypeByName(sess,  EIMConfig.get("OBJECT_TYPE_NAME_MYDOCUMENT"));

				//Attribute Type 「作成者」
				EIMAttributeType attTypeCreator = AttributeUtils.getAttributeTypeByName(sess,EIMConfig.get("ATTR_NAME_DOCUMENT_CREATE"));
				//Attribute Type 「比較元ドキュメントオブジェクトID」
				EIMAttributeType attTypeOrgId = AttributeUtils.getAttributeTypeByName(sess,EIMConfig.get("ATTR_NAME_COMP_FILE_SRC_ID"));
				//Attribute Type 「比較対象ドキュメントオブジェクトID」
				EIMAttributeType attTypeDstId = AttributeUtils.getAttributeTypeByName(sess,EIMConfig.get("ATTR_NAME_COMP_FILE_DEST_ID"));

				// 所属ワークスペース対応
				// Attribute Type 「所属ワークスペース」
				EIMAttributeType attrTypeBWS = AttributeUtils.getAttributeTypeByName(sess,EIMConfig.get("ATTR_NAME_DOCUMENT_BELONGING_WS"));
				// Attribute Type 「リンク所属ワークスペース」
				EIMAttributeType attrTypeLBWS = AttributeUtils.getAttributeTypeByName(sess,EIMConfig.get("ATTR_NAME_DOCUMENT_LINK_BELONGING_WS"));

				//Change Session Helper
				ChangeSessionHelper sehelper = new ChangeSessionHelper(sess);

				//「PDF公開文書比較」を取得
				List result = ObjectUtils.getObjectListByType(sess, objTypePDFCompare);

				//Loop
				for(Iterator iter = result.iterator(); iter.hasNext();)
				{
					EIMObject pdfCompObj = null;

					fileList.clear();
					resultFile = null;

					//PDF Compare Object
					pdfCompObj = (EIMObject)iter.next();

					//セッションユーザを「システムユーザ」から「作成者」に変更
					sehelper.setUserSessionConsole(sess, pdfCompObj.getCreateUser().getId());

					//比較結果ドキュメントオブジェクト作成
					EIMObject compResultDocObj = ObjectUtils.createObject(sess, objTypedocType, pdfCompObj.getName());
					//属性「作成者」を設定
					ObjectAttributeUtils.setAttribute(sess, compResultDocObj, attTypeCreator,
																	pdfCompObj.getCreateUser().getId());
					//セッションユーザを元に戻す
					sehelper.setSystemUserSessionConsole(sess);

					//比較元ドキュメントオブジェクト、比較対象ドキュメントオブジェクトを取得
					EIMObject orgDocObj = null;
					EIMObject targetDocObj = null;

					orgDocObj = ObjectUtils.getObjectById(sess, pdfCompObj.getAttribute(attTypeOrgId.getName()).getInt());
					targetDocObj = ObjectUtils.getObjectById(sess, pdfCompObj.getAttribute(attTypeDstId.getName()).getInt());

					if( orgDocObj == null || targetDocObj == null )
					{
						String docObj = "";
						if( orgDocObj == null)
						{
							docObj += String.valueOf(pdfCompObj.getAttribute(attTypeOrgId.getName()).getInt());
						}

						if(targetDocObj == null)
						{
							if( orgDocObj == null)
							{
								docObj += ", ";
							}
							docObj += String.valueOf(pdfCompObj.getAttribute(attTypeDstId.getName()).getInt());
						}

						doErrorAct(sess,  EIMResource.getMessage("EIM.ERROR.LOGIC.NOTEXIST.DOC" , new Object[]{docObj}),
									pdfCompObj, orgDocObj, targetDocObj, compResultDocObj);
						continue;
					}

					// 比較元ドキュメント、比較対象ドキュメントの公開ドキュメントが存在するかチェック
					File orgFile = getFiles(sess, orgDocObj, pdfCompObj, orgDocObj, targetDocObj, compResultDocObj);
					if( orgFile == null )
					{
						continue;
					}
					File targetFile = getFiles(sess, targetDocObj, pdfCompObj, orgDocObj, targetDocObj, compResultDocObj);
					if(targetFile == null )
					{
						continue;
					}

					fileList.add(orgFile);
					fileList.add(targetFile);

					//比較ツール呼び出し用のコマンドを作成
					String orgFilePath = "\"" + orgFile.getPath() + "\"" + " ";
					String targetFilePath = "\"" + targetFile.getPath() + "\"" + " ";
					resultFile = new File(EIMConfig.get("WORK") + compResultDocObj.getId() + EIMConfig.get("PDF_EXT"));

					//レイアウト解析を行うか否かで、参照するiniファイルのパスを切り替える
					String iniFilePath;
					EIMAttributeType attrTypeDoAnalyzeLayout = AttributeUtils.getAttributeTypeByName(sess,EIMConfig.get("ATTR_NAME_ANALYZE_LAYOUT_FLAG"));
					//レイアウト解析を行う場合
					if( pdfCompObj != null &&  pdfCompObj.getAttribute(attrTypeDoAnalyzeLayout.getName()).getInt() == 1)
					{
						iniFilePath = EIMConfig.get("PDF_COMP_INIFILE_DO_ANALYZELAYOUT");
					}
					//レイアウト解析を行わない場合
					else
					{
						iniFilePath = EIMConfig.get("PDF_COMP_INIFILE_DO_NOT_ANALYZELAYOUT");
					}

					//比較ツールログファイル出力用のファイル名を作成
					String datetime = "";
					datetime = formatDate(new Date());

					String cmd = "\"" + EIMConfig.get("PDF_COMP_EXE") + "\""
					+ " " + EIMConfig.get("PDF_COMPARE_ORGFILE_CMD") + " " + orgFilePath
					+ " " + EIMConfig.get("PDF_COMPARE_TARGETFILE_CMD") + " " + targetFilePath
					+ " " + EIMConfig.get("PDF_OUTPUTFILE_CMD") + " " + "\"" + resultFile.getPath() + "\""
					+ " " + EIMConfig.get("PDF_COMPARE_ANALYZELAYOUT_CMD") + " " + "\"" + iniFilePath  + "\""
					+ " " + EIMConfig.get("PDF_COMPARE_LOGOUTPUT_CMD") + " " + "\"" + EIMConfig.get("PDF_COMPARE_LOGFILE_PATH") + datetime
					+ EIMConfig.get("PDF_COMPARE_LOGFILE_EXT") + "\"";

					sess.commit();

					// DB接続を一旦解除
					try{
						if(sess != null){
							sess.close();
							sess = null;
						}
					} catch (Exception se) {
						log.warn(AppMessageUtils.makeLogMessage(se.getMessage()), se);
					}

					//ファイル比較コマンド実行
					Process process = null;
					InputStream is = null;
					BufferedReader br = null;

					try
					{
						process = Runtime.getRuntime().exec(cmd);
						is = process.getInputStream();
						br = new BufferedReader(new InputStreamReader(is));
					}
					catch (Exception pdfe)
					{
						doErrorAct(sess, EIMResource.getMessage("EIM.ERROR.LOGIC.PDFCOMPARE.EXECCOMPHELPER"),
								pdfCompObj, orgDocObj, targetDocObj, compResultDocObj);
						//コマンド内容の出力
						log.warn(cmd);
						continue;
					}

					// 読み取り行の最後まで移動
					while(br.readLine() != null)
					{
					}
					int rcode = process.exitValue();

					// ツールの処理が完了したらDB再接続
					if (sess == null) {
						// DB接続
						isRetry = true;
						while (isRetry) {
							try {
								sess = new EIMSession(user,lang);
								sess.setConnection(ds.getConnection());
								sess.setConsoleMode(true);
								sess.getDBConnection().setAutoCommit(false);

								isRetry = false;	// DB接続に成功したらループから脱出
							} catch (Exception e) {
								// DB接続でエラーが発生しました。処理中断後、リトライします。
								log.info(EIMResource.getMessage("EIM.ERROR.LOGIC.CANT.GET.DBCON"));
								isRetry = true;
								// DB接続に失敗した場合は一定時間待機する
								Thread.sleep(Integer.parseInt(EIMConfig.get("WAIT")));
							}
						}
					}

					//*********
					// Succeed
					//*********
					if(rcode == 0 || rcode == 1)
					{
						//後で削除するためにファイルリストに追加
						fileList.add(resultFile);

						//比較結果ファイルアップロード(default format)
						//Put File Path
						String putFilePath = defaultFormat.getDirectory().getPath()
											+ compResultDocObj.getId() + EIMConfig.get("PDF_EXT");
						File uploadFile = new File(putFilePath);
						//FTP
						try
						{
							FTPUtils.putFile(EIMConfig.get("FTP_HOST"),
											EIMConfig.get("FTP_USER"),
											EIMConfig.get("FTP_PASS"),
											resultFile,
											uploadFile);
						}
						catch(Exception e)
						{
							doErrorAct(sess,
									EIMResource.getMessage("EIM.ERROR.LOGIC.FTPUPLOAD"),
									pdfCompObj, orgDocObj, targetDocObj, compResultDocObj);
							log.warn(cmd);
							continue;
						}

						//チェックイン
						FileUtils.checkin(sess, compResultDocObj, defaultFormat, pdfCompObj.getName(), resultFile.length());

						//マイドキュメントへの登録
						EIMObject myDocObj = null;
						myDocObj = ObjectUtils.getObjectByTypeAndName(sess, objTypeMyDoc,
																pdfCompObj.getCreateUser().getCode());
						if(myDocObj == null)
						{
							myDocObj = ObjectUtils.createObject(sess, objTypeMyDoc, pdfCompObj.getCreateUser().getCode());
						}

						//Create Relation
						RelationUtils.createRelation(sess, RelTypeDoc, myDocObj, compResultDocObj);

						// 所属ワークスペース-マイドキュメント対応
						// 比較元、比較対象の所属ワークスペースの値を取得する
						long orgBWS = getBelongingWorkspace(orgDocObj);
						long targetBWS = getBelongingWorkspace(targetDocObj);

						// 所属ワークスペースを設定する
						if (orgBWS != -1 || targetBWS != -1) {

							if (orgBWS == -1 && targetBWS != -1) {
								orgBWS = targetBWS;
							} else if (orgBWS != -1 && targetBWS == -1) {
								targetBWS = orgBWS;
							}

							ObjectAttributeUtils.setAttribute(sess, compResultDocObj, attrTypeBWS, orgBWS);
							if (orgBWS != targetBWS) {
								long[] values = {orgBWS, targetBWS};
								ObjectAttributeUtils.setAttribute(sess, compResultDocObj, attrTypeLBWS, TypeConvertUtils.convertToBuildTypeArray(values));
							} else {
								long[] values = {orgBWS};
								ObjectAttributeUtils.setAttribute(sess, compResultDocObj, attrTypeLBWS, TypeConvertUtils.convertToBuildTypeArray(values));
							}
						}

						//一時領域からファイルを削除
						if( fileList != null && fileList.size() > 0 )
						{
							for(Iterator iterf = fileList.iterator(); iterf.hasNext();)
							{
								File delFile = (File)iterf.next();
								if((delFile != null) && (delFile.exists() == true))
								{
									delFile.delete();
								}
							}
						}

						//成功後処理を実行
						doSuccessAct(sess, pdfCompObj, orgDocObj, targetDocObj, compResultDocObj);
					}
					else
					{
						doErrorAct(sess, EIMResource.getMessage("EIM.ERROR.LOGIC.PDFCOMPARE.EXECCOMPHELPER"),
								pdfCompObj, orgDocObj, targetDocObj, compResultDocObj);
						//コマンド内容の出力
						log.warn(cmd);
						continue;
					}
					//Commit
					sess.commit();
				}
				//Session
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
				if(sess != null){
					sess.close();
				}
			}
			catch (Exception se) {
				log.warn(AppMessageUtils.makeLogMessage(se.getMessage()), se);
			}
		}
		log.info(" " + EIMResource.getMessage("EIM.INFO.PDFCOMPARE.END"));
	}

	/**
	 * EIMオブジェクトから所属ワークスペースの値を取得する。
	 * @param target 対象オブジェクト
	 * @return int 所属ワークスペースの値。存在しない場合は-1を返す。
	 * @throws Exception
	 */
	private static long getBelongingWorkspace(EIMObject target) throws Exception {

		// 属性タイプ「所属ワークスペース」
		String strBWS = EIMConfig.get("ATTR_NAME_DOCUMENT_BELONGING_WS");

		long result = -1;
		try {
			// EIMAttributeのリストを取得し、「所属ワークスペース」の値のみ返す
			List<EIMAttribute> attrList = target.getAttributeList();
			for (EIMAttribute tmpAttr : attrList) {
				EIMAttributeType tmpType = tmpAttr.getType();
				if (tmpType.getName().equals(strBWS)) {
					result = tmpAttr.getInt();
					break;
				}
			}
		} catch (Exception e) {
			result = -1;
		}

		return result;
	}

	/**
	 * 比較処理に必要なドキュメントを取得します
	 * @param sess セッション情報インスタンス
	 * @param object ドキュメントオブジェクト
	 * @param compResultDocObj 比較結果ドキュメントオブジェクト
	 * @return tmpFile 取得ファイル
	 */
	private static File getFiles(EIMSession sess, EIMObject object, EIMObject pdfCompObj,
			EIMObject orgDocObj, EIMObject targetDocObj, EIMObject compResultDocObj)throws Exception
	{
		File tmpFile = null;

		EIMObjectType objType = ObjectUtils.getObjectTypeById(sess, object.getType().getId());
		EIMFormat dfFormat = null;
		dfFormat = FileUtils.getDefaultFormat(sess, objType);
		if(dfFormat == null)
		{
			doErrorAct(sess, EIMResource.getMessage("EIM.ERROR.LOGIC.NODEFAULTFORM.WITHTYPENAME", new Object[]{objType.getName()}),
					pdfCompObj, orgDocObj, targetDocObj, compResultDocObj);
			return null;
		}


		EIMFile file = FileUtils.getFile(sess, object, dfFormat);
		if(file != null)
		{
			//公開ドキュメントが存在するかチェック
			EIMFile pubFile = FileUtils.getFile(sess, object, publicFormat);
			if (pubFile == null)
			{
				doErrorAct(sess, EIMResource.getMessage("EIM.ERROR.LOGIC.NOPUBLICFILE.WITHDOCNAME",
							new Object[]{file.getName()}), pdfCompObj, orgDocObj, targetDocObj, compResultDocObj);
				return null;
			}

			//Get File Path
			String getFilePath = pubFile.getDirectory().getPath()
					+ FileUtils.getFileName(object, pubFile);

			//Original File
			tmpFile = new File(EIMConfig.get("WORK") + object.getId());

			//FTP転送で該当ファイルをダウンロード
			try
			{
				FTPUtils.getFile(EIMConfig.get("FTP_HOST"),
								EIMConfig.get("FTP_USER"),
								EIMConfig.get("FTP_PASS"),
								new File(getFilePath),
								tmpFile);
			}
			catch(Exception e)
			{
				doErrorAct(sess,
						EIMResource.getMessage("EIM.ERROR.LOGIC.FTPDOWNLOAD"),
						pdfCompObj, orgDocObj, targetDocObj, compResultDocObj);
				return null;
			}
		}
		else
		{
			// EIMFileが取得できない場合も「ドキュメントが存在しない」エラーとする
			doErrorAct(sess, EIMResource.getMessage("EIM.ERROR.LOGIC.NOTEXIST.DOC", new Object[]{String.valueOf(object.getId())}),
					pdfCompObj, orgDocObj, targetDocObj, compResultDocObj);
			return null;
		}
		return tmpFile;
	}

	/**
	 * 成功後処理
	 * @param sess セッションインスタンス
	 * @param pdfCompObj 公開ファイル比較オブジェクト
	 * @param orgDocObj 比較元ドキュメントオブジェクト
	 * @param targetDocObj 比較対象ドキュメントオブジェクト
	 * @param compResultDocObj 比較結果ドキュメントオブジェクト
	 */
	private static void doSuccessAct(EIMSession sess, EIMObject pdfCompObj, EIMObject orgDocObj,
			EIMObject targetDocObj, EIMObject compResultDocObj)throws Exception
	{
		//メール受信設定ONかつ公開ファイル比較オブジェクト作成ユーザにメールアドレスが設定されている場合は完了通知を送信
		EIMAttributeType attrTypeIsMail = AttributeUtils.getAttributeTypeByName
											(sess,EIMConfig.get("ATTR_NAME_NOTIF_MAIL_FLAG"));
		if( pdfCompObj != null &&  pdfCompObj.getAttribute(attrTypeIsMail.getName()).getInt() == 1
				&&  pdfCompObj.getCreateUser().getMail() != null )
		{
			EIMThreadContext.put("PDF.COMP.OBJ", pdfCompObj);
			EIMThreadContext.put("ORG.DOC.OBJ", orgDocObj);
			EIMThreadContext.put("TARGET.DOC.OBJ", targetDocObj);
			MailUtil.execute(sess, pdfCompObj, "IMPL.TYPE.COMPARE.SUCCESS");
//			MailUtil.sendPDFCompareCompletedMail(sess, pdfCompObj.getCreateUser(),
//													pdfCompObj, orgDocObj, targetDocObj);
		}

		//アクセス履歴を作成
		AccessUtils.createAccess(sess, compResultDocObj, "EIM.ACCESS.TYPE.PDFCOMP.CREATE");

		//成功ログ出力
		if(log != null)
		{
			String logmessage = createLogStrings(false, pdfCompObj, orgDocObj, targetDocObj, null);
			log.info(logmessage);
		}

		//公開ファイル比較オブジェクトを削除
		if( pdfCompObj != null )
		{
			ObjectUtils.deleteObject(sess, pdfCompObj);
		}
	}

	/**
	 * 処理中断時エラーログ作成&ロールバック処理
	 * @param sess セッションインスタンス
	 * @param message エラーメッセージ
	 */
	private static void doErrorActForBreak(EIMSession sess, String message) throws Exception
	{
		if(log != null)
		{
			if(message == null)
			{
				message = "";
			}
			log.warn(EIMResource.getMessage("EIM.LOG.PDF.COMP.WARNING") +
					 EIMResource.getMessage("EIM.LOG.PDF.COMP.ORGFILE")+ "-( )" +
					 EIMResource.getMessage("EIM.LOG.PDF.COMP.DSTFILE") + "-( )" +
					 EIMResource.getMessage("EIM.LOG.PDF.COMP.OUTPUTFILE") + "-" +
					 EIMResource.getMessage("EIM.LOG.PDF.COMP.WARNMESSAGE") + message );
		}
		if(sess != null)
		{
			sess.rollback();
		}
	}

	/**
	 * 失敗後処理
	 * @param sess セッションインスタンス
	 * @param message エラーメッセージ
	 * @param pdfCompObj 公開ファイル比較オブジェクト
	 * @param orgDocObj 比較元ドキュメントオブジェクト
	 * @param targetDocObj 比較対象ドキュメントオブジェクト
	 * @param compResultDocObj 比較結果ドキュメントオブジェクト
	 */
	private static void doErrorAct(EIMSession sess, String message, EIMObject pdfCompObj, EIMObject orgDocObj,
			EIMObject targetDocObj, EIMObject compResultDocObj)
	throws Exception
	{
		// DBが切断されている場合は再接続
		if (sess == null) {
			// DB接続
			boolean isRetry = true;
			while (isRetry) {
				try {
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
					isRetry = false;	// DB接続に成功したらループから脱出
				} catch (Exception e) {
					// DB接続でエラーが発生しました。処理中断後、リトライします。
					log.info(EIMResource.getMessage("EIM.ERROR.LOGIC.CANT.GET.DBCON"));
					isRetry = true;
					// DB接続に失敗した場合は一定時間待機する
					Thread.sleep(Integer.parseInt(EIMConfig.get("WAIT")));
				}
			}
		}

		//公開ファイル比較オブジェクト作成ユーザにメールアドレスが設定されている場合は失敗通知を送信
		if( pdfCompObj != null && pdfCompObj.getCreateUser().getMail() != null )
		{
			EIMThreadContext.put("PDF.COMP.OBJ",pdfCompObj);
			EIMThreadContext.put("ORG.DOC.OBJ", orgDocObj);
			EIMThreadContext.put("TARGET.DOC.OBJ", targetDocObj);
			EIMThreadContext.put("EIM.ERROR.LOGIC",message);
			MailUtil.execute(sess, pdfCompObj, "IMPL.TYPE.COMPARE.FAILED");
//			MailUtil.sendPDFCompareFailedMail(sess, pdfCompObj.getCreateUser(),
//					pdfCompObj, orgDocObj, targetDocObj, message);
		}

		//比較結果ドキュメントオブジェクトを削除
		if( compResultDocObj != null )
		{
			ObjectUtils.deleteObject( sess, compResultDocObj );
		}

		//ダウンロードしたファイルが存在する場合は全て削除
		if( fileList != null && fileList.size() > 0 )
		{
			for(Iterator iter = fileList.iterator(); iter.hasNext();)
			{
				File delFile = (File)iter.next();
				if((delFile != null) && (delFile.exists() == true))
				{
					delFile.delete();
				}
			}
		}

		//比較結果ファイルが存在する場合は削除
		if( resultFile != null && (resultFile.exists() == true) )
		{
			resultFile.delete();
		}

		//失敗ログ出力
		if(log != null)
		{
			String logmessage = createLogStrings(true, pdfCompObj, orgDocObj, targetDocObj, message);
			log.warn(logmessage);
		}

		//公開ファイル比較オブジェクトを削除
		if( pdfCompObj != null )
		{
			ObjectUtils.deleteObject(sess, pdfCompObj);
		}

		sess.commit();
	}

	/**
	 * 成功・失敗時のログを作成します。
	 * @param isFail 失敗用ログ判定フラグ(true:失敗用、false:成功用)
	 * @param pdfCompObj 公開ファイル比較オブジェクト
	 * @param orgDocObj 比較元ドキュメントオブジェクト
	 * @param targetDocObj 比較対象ドキュメントオブジェクト
	 * @param message エラーメッセージ
	 * @return contents ログ出力文字列
	 * @throws Exception 予期しないエラー
	 */
	private static String createLogStrings(boolean isFail, EIMObject pdfCompObj, EIMObject orgDocObj,
			EIMObject targetDocObj, String message)throws Exception
	{
		String contents = "";

		if(isFail)
		{
			contents += EIMResource.getMessage("EIM.LOG.PDF.COMP.FAILED");
		}
		else
		{
			contents += EIMResource.getMessage("EIM.LOG.PDF.COMP.COMPLETION");
		}
		contents += EIMResource.getMessage("EIM.LOG.PDF.COMP.ORGFILE");

		if( orgDocObj != null )
		{
			contents += orgDocObj.getName() + "(" + orgDocObj.getId() + ")";
		}
		else
		{
			contents += "-( )";
		}
		contents += "、" + EIMResource.getMessage("EIM.LOG.PDF.COMP.DSTFILE");

		if( targetDocObj != null )
		{
			contents += targetDocObj.getName() + "(" + targetDocObj.getId() + ")";
		}
		else
		{
			contents += "-( )";
		}
		contents += "、" + EIMResource.getMessage("EIM.LOG.PDF.COMP.OUTPUTFILE");

		if( pdfCompObj != null )
		{
			contents += pdfCompObj.getName();
		}
		else
		{
			contents += "-";
		}

		if(isFail)
		{
			if(message == null)
			{
				message = "-";
			}
			contents += " " + EIMResource.getMessage("EIM.LOG.PDF.COMP.FAILUREMESSAGE") + message;
		}

		return contents;
	}

	/**
     * 日付オブジェクトに設定されている時間を"yyyyMMddHHmmssSSS"形式の文字列に
     * 変換します。
     * @param date 日付オブジェクトを指定します。
     * @return "yyyyMMddHHmmssSSS"形式の文字列を返します。
     */
    private static String formatDate(Date date) {
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyyMMddHHmmssSSS");
        return dateFormat.format(date);
    }
}