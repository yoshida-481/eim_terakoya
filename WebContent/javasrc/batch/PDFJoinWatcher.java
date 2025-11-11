package batch;

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import javax.sql.DataSource;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.context.ApplicationContext;

import common.util.AppMessageUtils;
import common.util.AppObjectConditionHelper;
import common.util.AppObjectUtil;
import common.util.AppUpdateNoticeUtils;
import common.util.ChangeSessionHelper;
import common.util.DisplayColorUtil;
import common.util.MailUtil;
import common.util.PublishAddonUtils;
import eim.bo.EIMAttribute;
import eim.bo.EIMAttributeType;
import eim.bo.EIMException;
import eim.bo.EIMFile;
import eim.bo.EIMFormat;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMRelationType;
import eim.bo.EIMResource;
import eim.bo.EIMSecurity;
import eim.bo.EIMUser;
import eim.net.EIMSession;
import eim.util.AccessUtils;
import eim.util.AttributeUtils;
import eim.util.EIMConfig;
import eim.util.EIMConstant;
import eim.util.FTPUtils;
import eim.util.FileUtils;
import eim.util.ObjectAttributeUtils;
import eim.util.ObjectUtils;
import eim.util.RelationUtils;
import eim.util.SearchUtils;
import eim.util.SecurityUtils;
import eim.util.StringUtils;
import eim.util.TypeConvertUtils;
import eim.util.UserUtils;
import eim.util.WorkFlowUtils;
import jp.co.ctc_g.eim.framework.common.util.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework2.common.context.TransactionContext;
import jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum;


/**
 * PDF結合
 * @author
 */
public class PDFJoinWatcher {

	/** 改訂内容登録可能最大文字長 */
	private static final int _maxLength = 170;

	/** List */
	private static List orgFiles = new ArrayList();
	private static List orgSrvFiles = new ArrayList();
	private static List orgObjects = new ArrayList();

	/**
	* Main Function
	* @param args
	* @throws Exception
	*/
	public static void main(String[] args)
	throws Exception
	{
		boolean isError = false;

		// Error Logging
		Log log = LogFactory.getLog(PDFJoinWatcher.class);
		log.info(" " + EIMResource.getMessage("EIM.INFO.PDFJOINSTART"));

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

				//get Public Format
				EIMFormat publicFormat  = FileUtils.getFormatByName(sess, EIMConfig.get("FORMAT_NAME_PUBLIC"));
				if(publicFormat == null)
				{
					if(sess != null)
					{
						log.warn(EIMResource.getMessage("EIM.LOG.PDF.JOIN.FAILED") +
								 EIMResource.getMessage("EIM.LOG.PDF.JOIN.NEWFILE")+ "-( )" +
								 EIMResource.getMessage("EIM.LOG.PDF.JOIN.OUTPUT") + "-( )" +
								 EIMResource.getMessage("EIM.LOG.PDF.JOIN.ORIGINALFILE") + "-" +
								 EIMResource.getMessage("EIM.LOG.PDF.PREFIX.ERROR") +
							     EIMResource.getMessage("EIM.ERROR.LOGIC.PUBLICFORMAT.NOTFOUND"));
						sess.rollback();
					}
					break;
				}

				//Object Type PDFJoin
				EIMObjectType objTypePDFJoin = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_JOINPDF"));

				//Relation Type Document
				EIMRelationType RelTypePDFJoin = RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));

				//Attribute Type
				EIMAttributeType creator = AttributeUtils.getAttributeTypeByName(sess,EIMConfig.get("ATTR_NAME_DOCUMENT_CREATE"));

				//Condition Helper
				AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

				//Change Session Helper
				ChangeSessionHelper sehelper = new ChangeSessionHelper(sess);

				//Search PDF Join Object
				List result = SearchUtils.searchObject(	sess,
														objTypePDFJoin,
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
		MainLoop:for(int i = 0; i < result.size(); i++)
				{
					EIMObject parentObj = null;
					EIMObject pdfJoinObj = null;
					EIMAttribute parentId = null;
					isError = false;

					//Init List
					orgFiles.clear();
					orgSrvFiles.clear();
					orgObjects.clear();

					//PDF Join Object
					pdfJoinObj = (EIMObject)result.get(i);
					//Get Parent Object
					parentId = pdfJoinObj.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_PARENT_ID"));
					parentObj = ObjectUtils.getObjectById(sess, parentId.getInt());

					//Failed to get parent Object
					if(parentObj == null)
					{
						doErrorActNoObj(sess, null, pdfJoinObj, log, EIMResource.getMessage("EIM.ERROR.LOGIC.PDFJOIN.NOFOLWS"));
						continue;
					}
					//Check parent Object site
					if(AppObjectUtil.isObjectInRecycle(sess, parentObj))
					{
						doErrorActNoObj(sess, null, pdfJoinObj, log, EIMResource.getMessage("EIM.ERROR.INPUT.CREATEFILEINRECYCLE"));
						continue;
					}

					//Get DocType of PDFJoinObject
					EIMAttribute docType = pdfJoinObj.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_TYPE_ID"));
					EIMObjectType objTypedocType = ObjectUtils.getObjectTypeById(sess, docType.getInt());
					if(objTypedocType == null)
					{
						isError = true;
						//オブジェクトタイプ「ドキュメント」を設定
						objTypedocType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT"));
					}

					//セッションを「システムユーザ」から「登録者」に変更
					EIMAttribute regUserId = pdfJoinObj.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_PDFJOIN_USER"));
					sehelper.setUserSessionConsole(sess, regUserId.getInt());

					EIMObject pdfJoinDocObj = null;
					try
					{
						//権限チェック
						//結合対象オブジェクトを取得し、各ファイルごとにチェック
						EIMAttribute orgDocAttr = pdfJoinObj.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_JOINEDPDF_ID"));
						PublishAddonUtils.checkPDFJoinAuth(sess, TypeConvertUtils.convertToLongArray(orgDocAttr.getInts()), (long)parentObj.getId());

						//PDF結合ドキュメントオブジェクト作成
						pdfJoinDocObj = ObjectUtils.createObject(sess, objTypedocType,
								pdfJoinObj.getName() + EIMConfig.get("PDF_EXT"));
					}
					catch(EIMException eime)
					{
						doErrorActNoObj(sess, null, pdfJoinObj, log, eime.getMessage());
					}
					finally
					{
						//セッションを元に戻す
						sehelper.setSystemUserSessionConsole(sess);
						if(pdfJoinDocObj == null)
						{
							continue;
						}
					}

					//親オブジェクトへのリレーションを設定
					while(true)
					{
						try
						{
							RelationUtils.createRelation( sess, RelTypePDFJoin, parentObj, pdfJoinDocObj, EIMConstant.DEPU_CHECK_NAME);
						}
						catch(EIMException rele)
						{
							String errMessageKey = rele.getMessageKey();
							if(errMessageKey.equals("EIM.ERROR.LOGIC.OBJECT.NAME.DUPLICATE"))
							{
								String langId = sess.getLangId();
								String newName = "";
								newName = EIMConfig.get("PDF_JOIN_FILENAME_PREFIX_" + langId)
										+ " - " + pdfJoinDocObj.getName();

								ObjectUtils.rename(sess, pdfJoinDocObj, newName);
							}
							continue;
						}
						break;
					}

					//「作成者」を設定
					ObjectAttributeUtils.setAttribute(sess, pdfJoinDocObj, creator, regUserId.getInt());

					//親フォルダの下位引き継ぎ属性を取得
					long[] parentLowAttrIds = AppObjectUtil.getIntAttrs(sess, parentObj, helper.getAttrNameOfToLowAttr());

					if (parentLowAttrIds != null) {
						//上位からの引継ぎ属性の設定内容に従い、parentObjから属性値をコピー
						//ただし、自身のタイプに該当属性が割り当てられているものに限る
						List parentLowAttrTypes = new ArrayList();
						{
							List parentLowAttrIdsInteger = new ArrayList(Arrays.asList(ArrayUtils.toObject(parentLowAttrIds)));
							List objectTypes = ObjectAttributeUtils.getAttributeTypeList(sess, parentObj.getType());
							//引継ぎ対象を自身のタイプに該当属性が割り当てられているものにフィルタリング
			SearchToLowAttr:for (Iterator it = parentLowAttrIdsInteger.iterator(); it.hasNext();)
							{
								long attrTypeIdOfParentLowAttrId = ((Long)it.next()).longValue();
								for (Iterator j = objectTypes.iterator(); j.hasNext();)
								{
									EIMAttributeType attrTypeObj = (EIMAttributeType)j.next();
									if (attrTypeObj.getId() == attrTypeIdOfParentLowAttrId)
									{
										parentLowAttrTypes.add(attrTypeObj);
										continue SearchToLowAttr;
									}
								}
								it.remove();//自身のタイプに無かったので対象から削除
							}
							parentLowAttrIds = ArrayUtils.toPrimitive((Long[])parentLowAttrIdsInteger.toArray(new Long[parentLowAttrIdsInteger.size()]));
						}
						//「上位からの引継ぎ」属性の値を設定
						ObjectAttributeUtils.setAttribute(sess, pdfJoinDocObj, helper.getAttrTypeOfFromHighAttr(), TypeConvertUtils.convertToBuildTypeArray(parentLowAttrIds));

						//上位引継ぎの属性表示色を更新
						DisplayColorUtil.inheritDisplayColor(sess, pdfJoinDocObj, parentLowAttrTypes, parentObj);

						//各属性値の引継ぎ
						for (Iterator it = parentLowAttrTypes.iterator(); it.hasNext();)
						{
							EIMAttribute attr = parentObj.getAttribute(((EIMAttributeType)it.next()).getDefName());
							if (attr != null)
							{
								AppObjectUtil.setAttr(sess, pdfJoinDocObj, attr);
							}
						}
					}

					//Set Path
					String path = AppObjectUtil.getPath(parentObj);
					if(path == null)
					{
						// ワークスペースの場合、パス属性の値を保持していない
						path = "/";
					}
					path += parentObj.getName() + "/";
					AppObjectUtil.setPath(sess, pdfJoinDocObj, path);

					//親オブジェクトがWF付きフォルダ、またはWF付きフォルダ内のフォルダの場合
					if(parentObj.getStatus() != null)
					{
						EIMAttribute attrOfHigherWFFolder =
							parentObj.getAttribute(helper.getAttrNameDocumentHigherWFFolder());
						//WF付フォルダ直下
						if (attrOfHigherWFFolder == null)
						{
							ObjectAttributeUtils.setAttribute
							(sess, pdfJoinDocObj, helper.getAttrTypeOfHigherWFFolder(), parentObj.getId());
						}
						//「WF付フォルダ下のフォルダ」の下
						else
						{
							AppObjectUtil.setAttr(sess, pdfJoinDocObj, attrOfHigherWFFolder);
						}

						//ステータスを設定
						WorkFlowUtils.updateObjectStatus(sess, pdfJoinDocObj, parentObj.getStatus());
					}

					//ドキュメントタイプのフォーマットを取得
					EIMFormat defaultFormat = null;
					defaultFormat = FileUtils.getDefaultFormat(sess, objTypedocType);
					if(defaultFormat == null)
					{
						doErrorAct(sess, pdfJoinDocObj, pdfJoinObj, log, EIMResource.getMessage("EIM.ERROR.LOGIC.NODEFAULTFORM.WITHTYPENAME", new Object[]{objTypedocType.getName()}));
						continue;
					}


					//ドキュメントタイプ取得に失敗していた場合
					if(isError != false)
					{
						doErrorAct(sess, pdfJoinDocObj, pdfJoinObj, log, EIMResource.getMessage("EIM.ERROR.LOGIC.DOCTYPE.NOTFOUND"));
						continue;
					}

					//結合対象ドキュメントオブジェクトの取得
					EIMAttribute orgIds = pdfJoinObj.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_JOINEDPDF_ID"));
					long[] originals = TypeConvertUtils.convertToLongArray(orgIds.getInts());
					for(int j = 0; j < originals.length; j++)
					{
						EIMObject orgObject = ObjectUtils.getObjectById(sess, originals[j]);
						if(orgObject == null)
						{
							doErrorAct(sess, pdfJoinDocObj, pdfJoinObj, log, EIMResource.getMessage("EIM.ERROR.LOGIC.NOTEXIST.DOC" , new Object[]{""}));
							continue MainLoop;
						}
						orgObjects.add(orgObject);
						// 結合対象ドキュメントオブジェクトのオブジェクトタイプを取得
						EIMObjectType orgObjType = ObjectUtils.getObjectTypeById(sess, orgObject.getType().getId());
						// 結合対象ドキュメントオブジェクトのデフォルトフォーマットを取得
						EIMFormat orgDefaultFormat = null;
						orgDefaultFormat = FileUtils.getDefaultFormat(sess, orgObjType);
						if(orgDefaultFormat == null)
						{
							doErrorAct(sess, pdfJoinDocObj, pdfJoinObj, log, EIMResource.getMessage("EIM.ERROR.LOGIC.NODEFAULTFORM.WITHTYPENAME", new Object[]{orgObjType.getName()}));
							continue;
						}
						// 結合対象ドキュメントオブジェクトのデフォルトフォーマットのEIMFileオブジェクトを取得
						EIMFile file = FileUtils.getFile(sess, orgObject, orgDefaultFormat);
						if(file != null)
						{
							orgSrvFiles.add(file);

							//公開ドキュメントが存在するかチェック
							EIMFile pubFile = FileUtils.getFile(sess, orgObject, publicFormat);
							if (pubFile == null)
							{
								doErrorAct(sess, pdfJoinDocObj, pdfJoinObj, log,
										EIMResource.getMessage("EIM.ERROR.LOGIC.NOPUBLICFILE.WITHDOCNAME" ,new Object[]{orgObject.getName()}));
								continue MainLoop;
							}
							//Get File Path
							String getFilePath = pubFile.getDirectory().getPath()
										+ orgObject.getId() + pubFile.getExt();

							//Original File
							File orgFile = new File(EIMConfig.get("WORK") + orgObject.getId() + pubFile.getExt());

							//FTP転送で該当ファイルをダウンロード
							try
							{
								FTPUtils.getFile(EIMConfig.get("FTP_HOST"),
												EIMConfig.get("FTP_USER"),
												EIMConfig.get("FTP_PASS"),
												new File(getFilePath),
												orgFile);
							}
							catch(Exception e)
							{
								log.warn(EIMResource.getMessage("EIM.LOG.PDF.JOIN.FAILED") +
										 EIMResource.getMessage("EIM.LOG.PDF.JOIN.NEWFILE")+
										 StringUtils.getFileBody(pdfJoinDocObj.getName()) + "(" + pdfJoinDocObj.getId() + ")" +
										 EIMResource.getMessage("EIM.LOG.PDF.JOIN.OUTPUT") + parentObj.getName() + "(" + parentId.getInt()
										 + ")" + EIMResource.getMessage("EIM.LOG.PDF.JOIN.ORIGINALFILE") + getOrgObjNameAndId(sess, pdfJoinObj)
										 + EIMResource.getMessage("EIM.LOG.PDF.PREFIX.ERROR")
										 + EIMResource.getMessage("EIM.ERROR.LOGIC.FTPDOWNLOAD"));
								throw e;
							}
							orgFiles.add(orgFile);
						}
						else
						{
							// EIMFileが取得できない場合も「ドキュメントが存在しない」エラーとする
							// ※エラー処理を実施しないと無限にPDF結合ドキュメントObjectを生成することがあるため
							doErrorAct(sess, pdfJoinDocObj, pdfJoinObj, log, EIMResource.getMessage("EIM.ERROR.LOGIC.NOTEXIST.DOC" , new Object[]{""}));
							continue MainLoop;
						}
					}

					//取得した結合対象公開ファイルを結合
					//結合対象ファイルのパスを取得
					String orgFilePath = "";
					for(int j = 0; j < originals.length; j++)
					{
						File orgFile = (File)orgFiles.get(j);
						orgFilePath += "\"" + orgFile.getPath() + "\"" + " ";
					}
					//PDF File
					File pdfFile = new File(EIMConfig.get("WORK") + pdfJoinDocObj.getId() + EIMConfig.get("PDF_EXT"));

					//結合コマンド作成
					String cmd = EIMConfig.get("PDF_JOIN_EXE")
							+ " " + EIMConfig.get("PDF_OUTPUTFILE_CMD") + " " + "\"" + pdfFile.getPath() + "\""
							+ " " + EIMConfig.get("PDF_INPUTFILE_CMD") + " " + orgFilePath;

					//結合コマンド実行
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
						doErrorAct(sess, pdfJoinDocObj, pdfJoinObj, log, EIMResource.getMessage("EIM.ERROR.LOGIC.PDFJOIN.EXECPDFMAKEUP"));
						log.warn(cmd);
						continue;
					}

					// 読み取り行の最後まで移動
					while(br.readLine() != null)
					{
					}
					int rcode = process.exitValue();

					//*********
					// Succeed
					//*********
					if(rcode == 0)
					{
						//結合ファイルアップロード(default format)
						//Put File Path
						String putFilePath = defaultFormat.getDirectory().getPath()
											+ pdfJoinDocObj.getId() + EIMConfig.get("PDF_EXT");
						File uploadFile = new File(putFilePath);
						//FTP
						try
						{
							FTPUtils.putFile(EIMConfig.get("FTP_HOST"),
											EIMConfig.get("FTP_USER"),
											EIMConfig.get("FTP_PASS"),
											pdfFile,
											uploadFile);
						}
						catch(Exception e)
						{
							log.warn(EIMResource.getMessage("EIM.LOG.PDF.JOIN.FAILED") +
									 EIMResource.getMessage("EIM.LOG.PDF.JOIN.NEWFILE")+
									 StringUtils.getFileBody(pdfJoinDocObj.getName()) + "(" + pdfJoinDocObj.getId() + ")" +
									 EIMResource.getMessage("EIM.LOG.PDF.JOIN.OUTPUT") + parentObj.getName() + "(" + parentId.getInt()
									 + ")" + EIMResource.getMessage("EIM.LOG.PDF.JOIN.ORIGINALFILE") + getOrgObjNameAndId(sess, pdfJoinObj)
									 + EIMResource.getMessage("EIM.LOG.PDF.PREFIX.ERROR") + EIMResource.getMessage("EIM.ERROR.LOGIC.FTPUPLOAD"));
							log.warn(cmd);
							throw e;
						}

						//check in
						FileUtils.checkin(sess, pdfJoinDocObj, defaultFormat,
								pdfJoinObj.getName() + EIMConfig.get("PDF_EXT"), pdfFile.length());

						//公開ドキュメントの登録
						//対象ドキュメントがワークフローを持たない場合
						if(pdfJoinDocObj.getStatus() == null)
						{
							String cpyFilePath = publicFormat.getDirectory().getPath()
												+ pdfJoinDocObj.getId() + EIMConfig.get("PDF_EXT");
							File cpyFile = new File(cpyFilePath);
							// FTP
							// ※バッチなのでFileUtils.copyFile()でファイルコピーはできません
							try
							{
								FTPUtils.putFile(EIMConfig.get("FTP_HOST"),
												EIMConfig.get("FTP_USER"),
												EIMConfig.get("FTP_PASS"),
												pdfFile,
												cpyFile);
							}
							catch(Exception e)
							{
								log.warn(EIMResource.getMessage("EIM.LOG.PDF.JOIN.FAILED") +
										 EIMResource.getMessage("EIM.LOG.PDF.JOIN.NEWFILE")+
										 StringUtils.getFileBody(pdfJoinDocObj.getName()) + "(" + pdfJoinDocObj.getId() + ")" +
										 EIMResource.getMessage("EIM.LOG.PDF.JOIN.OUTPUT") + parentObj.getName() + "(" + parentId.getInt()
										 + ")" + EIMResource.getMessage("EIM.LOG.PDF.JOIN.ORIGINALFILE") + getOrgObjNameAndId(sess, pdfJoinObj)
										 + EIMResource.getMessage("EIM.LOG.PDF.PREFIX.ERROR") + EIMResource.getMessage("EIM.ERROR.LOGIC.FTPUPLOAD"));
								log.warn(cmd);
								throw e;
							}
							//check in
							FileUtils.checkin(sess, pdfJoinDocObj, publicFormat,
									pdfJoinObj.getName() + EIMConfig.get("PDF_EXT"), cpyFile.length());
						}

						//成功後処理
						//「改訂履歴」設定
						setRevAttr(sess, pdfJoinObj, pdfJoinDocObj);
						//セキュリティ設定
						EIMSecurity sec = parentObj.getSecurity();
						if(sec != null)
						{
							SecurityUtils.setSecurity(sess, pdfJoinDocObj, sec);
						}
						//ダウンロードした結合対象ファイルを削除
						for(int j = 0; j < originals.length; j++)
						{
							File orgFile = (File)orgFiles.get(j);
							if((orgFile != null) && (orgFile.exists() == true))
							{
								orgFile.delete();
							}
						}
						//結合PDFファイルを削除
						pdfFile.delete();
						//PDF結合オブジェクトを削除
						ObjectUtils.deleteObject(sess, pdfJoinObj);

						// SearchFramework 検索FW更新通知 対象：結合ドキュメントと登録先(親フォルダor親ワークスペース)成功の場合
						AppUpdateNoticeUtils.updateNoticeInsert(pdfJoinDocObj.getId(), "SEARCHFW_PDFJOIN_DOCUMENT");
						AppUpdateNoticeUtils.updateNoticeInsertParent(sess, parentObj,
								"SEARCHFW_PDFJOIN_PARENT_FOLDER", "SEARCHFW_PDFJOIN_PARENT_WORKSPACE", null);

						// [09/01/19 added by ik.]
						// アクセス履歴の登録処理を追加
						AccessUtils.createAccess(sess, pdfJoinDocObj, "EIM.ACCESS.TYPE.PDFJOIN.CREATE");

						//成功ログを出力
						String orgFileNames = getOrgObjNameAndId(sess, pdfJoinObj);

						String sucMsg = (EIMResource.getMessage("EIM.LOG.PDF.JOIN.COMPLETE")
										+ EIMResource.getMessage("EIM.LOG.PDF.JOIN.NEWFILE")
										+ StringUtils.getFileBody(pdfJoinDocObj.getName()) + "(" + pdfJoinDocObj.getId() +")"
										+ EIMResource.getMessage("EIM.LOG.PDF.JOIN.OUTPUT") + parentObj.getName()
										+ "("+ parentObj.getId() + ")" + EIMResource.getMessage("EIM.LOG.PDF.JOIN.ORIGINALFILE") + orgFileNames);
						log.info(sucMsg);


					}
					else
					{
						//*********
						// Failure
						//*********
						doErrorAct(sess, pdfJoinDocObj, pdfJoinObj, log,
								EIMResource.getMessage("EIM.ERROR.LOGIC.PDFJOIN.EXECPDFMAKEUP"));

						log.warn(cmd);
						continue;
					}
					 //Commit
					sess.commit();
				}
				//Session
				EIMThreadContext.removeEIMSession();
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
				EIMThreadContext.removeEIMSession();
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
				EIMThreadContext.removeEIMSession();
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
				EIMThreadContext.removeEIMSession();
				if(sess != null){
					sess.close();
				}
			}
			catch (Exception se) {
				log.warn(AppMessageUtils.makeLogMessage(se.getMessage()), se);
			}
		}
		log.info(" " + EIMResource.getMessage("EIM.INFO.PDFJOINEND"));
	}

	/**
	 * 失敗後処理(作成EIMObjectなし(共通処理))
	 * @param sess EIMSessionインスタンス
	 * @param object 結合ドキュメントオブジェクト
	 * @param joinObj PDF結合オブジェクト
	 * @param log
	 * @throws Exception
	 */
	private static void doErrorActNoObj (EIMSession sess, EIMObject object, EIMObject joinObj, Log log, String errMsg) throws Exception
	{
        //PDF結合オブジェクトの登録者のユーザにメールアドレスが設定されているかチェック
		EIMAttribute regUserAttr = joinObj.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_PDFJOIN_USER"));
		EIMUser regUser = UserUtils.getUserById(sess, regUserAttr.getInt());
		//結合対象ファイル名、IDを取得
		String orgFiles = getOrgObjNameAndId(sess, joinObj);

		//メールアドレスが設定されている場合
		if(regUser.getMail() != null)
		{
			try
			{
				//登録者の該当ユーザ宛に結合処理失敗メールを送信
				EIMThreadContext.put("JOIN.OBJ", joinObj);
				EIMThreadContext.put("ORG.FILES", orgFiles);
				EIMThreadContext.put("EIM.ERROR.LOGIC", errMsg);
				MailUtil.execute(sess, object, "IMPL.TYPE.JOIN.FAILED");
			}
			catch(Exception e)
			{
				log.warn(" " + EIMResource.getMessage("EIM.ERROR.LOGIC.FAILEDSENDMAIL"));
				log.warn(AppMessageUtils.makeLogMessage(e.getMessage()), e);
			}
		}

		//エラーログを出力
		String errMessage = (EIMResource.getMessage("EIM.LOG.PDF.JOIN.FAILED") +
						 EIMResource.getMessage("EIM.LOG.PDF.JOIN.NEWFILE")+
						 StringUtils.getFileBody(joinObj.getName()) + "(" + joinObj.getId() + ")" +
						 EIMResource.getMessage("EIM.LOG.PDF.JOIN.OUTPUT"));
		EIMObject parentObj = null;
		if((parentObj = ObjectUtils.getObjectById
				(sess, joinObj.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_PARENT_ID")).getInt())) == null)
		{
			errMessage += "- (";
		}
		else
		{
			errMessage += (parentObj.getName() + "(");
		}
		errMessage += (joinObj.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_PARENT_ID")).getInt() + ")" +
				   		EIMResource.getMessage("EIM.LOG.PDF.JOIN.ORIGINALFILE") + orgFiles
				   		+ EIMResource.getMessage("EIM.LOG.PDF.PREFIX.ERROR") + errMsg );
		log.warn(errMessage);

		//PDF結合オブジェクトを削除
		try
		{
			ObjectUtils.deleteObject(sess, joinObj);
		}
		catch(Exception dpdfoe)
		{
			log.warn(" " + EIMResource.getMessage("EIM.ERROR.LOGIC.FAILEDDELOBJ"));
			log.warn(AppMessageUtils.makeLogMessage(dpdfoe.getMessage()), dpdfoe);
			return;
		}

		sess.commit();
	}

	/**
	 * 失敗後処理(作成EIMObjectあり)
	 * @param sess EIMSessionインスタンス
	 * @param newObj 結合ドキュメントオブジェクト
	 * @param joinObj PDF結合オブジェクト
	 * @param log
	 * @throws Exception
	 */
	private static void doErrorAct (EIMSession sess, EIMObject newObj, EIMObject joinObj, Log log, String errMsg) throws Exception
	{
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
		//「改訂履歴」に結合対象オブジェクト名を設定
		setRevAttr(sess, joinObj, newObj);

		//「PDF結合処理失敗」に「失敗」を設定
		ObjectAttributeUtils.setAttribute(sess, newObj,
				helper.getAttrType(EIMConfig.get("ATTR_NAME_DOCUMENT_PDF_JOIN_FAIL")), 1);

		//親オブジェクトの「セキュリティ」を設定
		EIMObject parentObj = ObjectUtils.getObjectById(sess,(joinObj.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_PARENT_ID"))).getInt());
		EIMSecurity sec = parentObj.getSecurity();
		if(sec != null)
		{
			SecurityUtils.setSecurity(sess, newObj, sec);
		}

		// SearchFramework 検索FW更新通知 対象：結合ドキュメントと登録先(親フォルダor親ワークスペース)エラーの場合
		AppUpdateNoticeUtils.updateNoticeInsert(newObj.getId(), "SEARCHFW_PDFJOIN_DOCUMENT");
		AppUpdateNoticeUtils.updateNoticeInsertParent(sess, parentObj,
				"SEARCHFW_PDFJOIN_PARENT_FOLDER", "SEARCHFW_PDFJOIN_PARENT_WORKSPACE", null);

		//ダウンロードした結合対象ファイルがあれば削除
		try
		{
			for(int i = 0;
				(i < orgObjects.size()) && (i < orgSrvFiles.size()) && (i < orgFiles.size()); i++)
			{
				EIMObject orgObj = (EIMObject)orgObjects.get(i);
				if(orgObj != null)
				{
					EIMFile file = (EIMFile)orgSrvFiles.get(i);
					if(file != null)
					{
						//Original File
						File orgFile = (File)orgFiles.get(i);
						if((orgFile != null) && (orgFile.exists() == true))
						{
							orgFile.delete();
						}
					}
				}
			}
		}
		catch(Exception e)
		{
			throw e;
		}

		//結合PDFファイルが存在する場合は削除
		try
		{
			File pdfFile = new File(EIMConfig.get("WORK") + newObj.getId() + EIMConfig.get("PDF_EXT"));
			if((pdfFile != null) && (pdfFile.exists() == true))
			{
				pdfFile.delete();
			}
		}
		catch(Exception e)
		{
			throw e;
		}

		//失敗後処理(共通)
		doErrorActNoObj(sess, newObj, joinObj, log, errMsg);
	}

	/**
	 * 改訂履歴設定処理
	 * @param sess EIMSessionインスタンス
	 * @param JoinObj PDF結合オブジェクト
	 * @param setObj 設定対象オブジェクト
	 */
	private static void setRevAttr(EIMSession sess, EIMObject JoinObj, EIMObject setObj)throws Exception
	{
		String joins = "";
		joins = getOrgObjName(sess, JoinObj);

		EIMAttributeType revAttr = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_REV_CONTENT"));

		//ファイル名の合計がが全角170文字以上の場合は170文字までを格納
		if( _maxLength < joins.length())
		{
			String joinsMax = joins.substring(0, _maxLength);
			joins = "";
			joins = joinsMax;
		}
		ObjectAttributeUtils.setAttribute(sess, setObj, revAttr, joins);
	}

	/**
	 * 結合対象オブジェクト名、ID取得処理
	 * @param sess EIMSessionインスタンス
	 * @param object PDF結合オブジェクト
	 * @return オブジェクト名文字列
	 */
	private static String getOrgObjNameAndId(EIMSession sess, EIMObject object)throws Exception
	{
		long[] originals = TypeConvertUtils.convertToLongArray(object.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_JOINEDPDF_ID")).getInts());
		String names = "";
		for(int i = 0; i < originals.length; i++)
		{
			names += (StringUtils.getFileBody(ObjectUtils.getObjectById(sess, originals[i]).getName())+
							"(" + originals[i] + ")");
			if(i < (originals.length - 1))
			{
				names += ", ";
			}
		}
		return names;
	}

	/**
	 * 結合対象オブジェクト名取得処理
	 * @param sess EIMSessionインスタンス
	 * @param object PDF結合オブジェクト
	 * @return オブジェクト名文字列
	 */
	private static String getOrgObjName(EIMSession sess, EIMObject object)throws Exception
	{
		long[] originals = TypeConvertUtils.convertToLongArray(object.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_JOINEDPDF_ID")).getInts());
		String names = "";
		for(int i = 0; i < originals.length; i++)
		{
			names += (StringUtils.getFileBody(ObjectUtils.getObjectById(sess, originals[i]).getName()));
			if(i < (originals.length - 1))
			{
				names += ", ";
			}
		}
		return names;
	}
}