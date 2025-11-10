package batch;

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import javax.sql.DataSource;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.context.ApplicationContext;

import common.bo.MessageItem;
import common.util.AppConstant;
import common.util.AppMessageUtils;
import common.util.AppObjectUtil;
import common.util.AppUpdateNoticeUtils;
import common.util.ChangeSessionHelper;
import common.util.MailUtil;
import common.util.SignUtil;
import eim.bo.EIMAccessRole;
import eim.bo.EIMAttributeType;
import eim.bo.EIMException;
import eim.bo.EIMFile;
import eim.bo.EIMFormat;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMResource;
import eim.bo.EIMUser;
import eim.net.EIMSession;
import eim.util.AccessUtils;
import eim.util.AttributeUtils;
import eim.util.EIMConfig;
import eim.util.FileUtils;
import eim.util.ObjectAttributeUtils;
import eim.util.ObjectUtils;
import eim.util.SecurityUtils;
import eim.util.StringUtils;
import eim.util.UserUtils;
import jp.co.ctc_g.eim.framework.common.util.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * 署名・暗号化バッチ
 *
 * <li>「署名・暗号化」オブジェクトに指定されたドキュメントに対して、
 * 署名・暗号化ツールを実施して署名・暗号化ファイルを生成、登録します。
 *
 */
public class SignEncryption {

	/**
	 * ログ
	 */
	public static Log log = LogFactory.getLog(SignEncryption.class);

	/**
	 * 半角スペース
	 */
	private static String _blank = " ";

	/**
	 * ユーザ無効フラグ : 無効
	 */
	private static final int _USER_DISABLE = 1;

	/**
	 * メイン処理
	 *
	 * @param args
	 * @throws Exception
	 */
	public static void main(String[] args) throws Exception{

		// ログ出力 : 署名・暗号化バッチ処理開始。
		log.info(EIMResource.getMessage("EIM.INFO.SIGN.AND.ENCR.START"));

		EIMSession sess = null;
		boolean isRetry = true;	// 再試行フラグ (DB接続)

		try {
			// DB接続
			sess = getEIMSession(sess);


			// 署名・暗号化フォーマットの取得
			EIMFormat signFormat = FileUtils.getFormatByName(sess, EIMConfig.get("FORMAT_NAME_SIGNENCR"));

			if (signFormat == null) {
				// 署名・暗号化フォーマットが取得できませんでした。
				throw new EIMException(sess , "EIM.ERROR.LOGIC.SIGN.AND.ENCR.FORMAT.NOTFOUND");
			}

			// 署名・暗号化バージョンの取得コマンド生成
			String versionCmd = EIMConfig.get("SIGNENCR_EXE") + _blank + EIMConfig.get("SIGNENCR_VERSION_CMD");

			//  署名・暗号化バージョンの取得
			String signVersion = null;
			try {
				Process process = Runtime.getRuntime().exec(versionCmd);
				InputStream is = process.getInputStream();
				BufferedReader br = new BufferedReader(new InputStreamReader(is));
				signVersion = br.readLine();
			} catch (Exception e) {
				// 署名・暗号化バージョンが取得できませんでした。
				throw new EIMException(sess , "EIM.ERROR.LOGIC.SIGN.AND.ENCR.ENCRVERSION.NOTFOUND");
			}

			if (StringUtils.isBlank(signVersion)) {
				// 署名・暗号化バージョンが取得できませんでした。
				throw new EIMException(sess , "EIM.ERROR.LOGIC.SIGN.AND.ENCR.ENCRVERSION.NOTFOUND");
			}

			// 冗長な取得を回避するための事前取得
			EIMObjectType signObjType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_SIGN_ENC"));	// 「署名・暗号化」オブジェクトタイプ
			EIMAttributeType signStatusAttrType = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_STATUS"));	// 「署名・暗号化状態」属性タイプ
			EIMAttributeType signVersionAttrType = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_VER"));	// 「署名・暗号化バージョン」属性タイプ

			// ループ(1) : 上部メニュー操作の単位
			while(true) {

				// DBが切断されている場合、DBコネクションの再取得
				if (sess == null) {
					// DB接続 デッドコードの可能性あり
					sess = getEIMSession(sess);
				}
				// 「署名・暗号化」オブジェクトの取得
				List signObjList = ObjectUtils.getObjectListByType(sess, signObjType);

				// 「署名・暗号化」オブジェクトがない場合
				if (signObjList == null || signObjList.size() == 0) {

					// DB切断
					sess.close();
					sess = null;	// このバッチのみ明示的に開放

					// 一定時間待機して次の周に移動する
					Thread.sleep(Integer.parseInt(EIMConfig.get("WAIT")));

				// 「署名・暗号化」オブジェクトがある場合
				} else {

					// 属性「実施者」からユーザ取得
					long userId = AppObjectUtil.getIntAttr(sess, (EIMObject)signObjList.get(0), EIMConfig.get("ATTR_NAME_SIGNENC_OPERATOR"), Integer.MIN_VALUE);
					EIMUser user = UserUtils.getUserById(sess, userId);

					HashMap formatMap = new HashMap();	// 冗長な取得回避用のフォーマット格納Map [key]オブジェクトタイプID、[value]EIMFormat
					List msgItemList = new ArrayList();	// 処理に失敗したエラー内容MessageItemを格納するリスト

					// 取得結果1件目と同一オブジェクト名のEIMObjectを、属性「選択タグ」の値毎にグルーピング
					List GroupList = getGroupList(sess, signObjList);

					// ループ(2) : グループ(選択タグ/ドキュメント)単位
					for (Iterator iter = GroupList.iterator(); iter.hasNext();) {
						List objList = (List) iter.next();

						// タグの「署名・暗号化」状態を更新すべき(グループ単位でドキュメント1件でも成功/1件でも暗号化済み/1件でも対象外/1件でもドキュメントが消滅)ならばtrue
						boolean isTagSignEncryptExce = false;

						// ループ(3) ; ドキュメント単位
						for (Iterator iterator = objList.iterator(); iterator.hasNext();) {
							EIMObject signObj = (EIMObject) iterator.next();

							// 対象ドキュメントの取得
							EIMObject docObj = ObjectUtils.getObjectById(sess, AppObjectUtil.getIntAttr(sess, signObj, EIMConfig.get("ATTR_NAME_SIGNENC_TARGET_DOC"), Integer.MIN_VALUE));

							// ユーザが存在しない場合
							if (user == null) {
								// 署名・暗号化を実施したユーザ[ID : {0}]が取得できませんでした。
								String[] message = {(userId != Integer.MIN_VALUE ? String.valueOf(userId) : "")};
								MessageItem msgItem = new MessageItem("EIM.ERROR.LOGIC.SIGN.AND.ENCR.USER.NOTFOUND", message, docObj);
								doWarnAct(sess, signObj, signStatusAttrType, log, msgItem);
								continue;
							}

							// ユーザが無効な場合
							if (user.getDisable() == _USER_DISABLE) {
								// [{0}]({1}) は無効なユーザです。
								String[] message = {user.getName(), String.valueOf(user.getId())};
								MessageItem msgItem = new MessageItem("EIM.EIM.ERROR.LOGIC.DISABLEUSER.WITHUSERNAME", message, docObj);
								doWarnAct(sess, signObj, signStatusAttrType, log, msgItem);
								continue;
							}

							if (docObj == null) {
								// 署名・暗号化処理の対象に指定したドキュメントを取得できませんでした。
								MessageItem msgItem = new MessageItem("EIM.ERROR.LOGIC.NOTFOUND.DOC.FOR.SIGN.AND.ENCR", null, docObj);
								msgItemList.add(msgItem);
								doErrorAct(sess, signObj, null, signStatusAttrType, log, msgItem);
								isTagSignEncryptExce = true;
								continue;
							}

							// 権限チェック
							if (!SecurityUtils.authorized(sess, docObj, user, EIMAccessRole.UPDATE)) {
								// ドキュメント[{0}]に対する署名・暗号化権限がありません。
								String[] message = {docObj.getName()};
								MessageItem msgItem = new MessageItem("EIM.ERROR.LOGIC.NO.SIGN.AND.ENCR.ROLE.FOR.DOC", message, docObj);
								msgItemList.add(msgItem);
								doErrorAct(sess, signObj, null, signStatusAttrType, log, msgItem);
								continue;
							}

							// 署名・暗号化済みファイルの場合はスキップ
							if (AppObjectUtil.getIntAttr(sess, docObj, EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_STATUS"), Integer.MIN_VALUE) == AppConstant.SIGNENCR_KIND_SIGNENCR) {
								// ドキュメント[{0}]は、署名・暗号化済みのファイルです。
								String[] message = {docObj.getName()};
								MessageItem msgItem = new MessageItem("EIM.ERROR.LOGIC.EXECUTED.SIGN.AND.ENCR.FOR.DOC", message, docObj);
								doWarnAct(sess, signObj, signStatusAttrType, log, msgItem);
								isTagSignEncryptExce = true;
								continue;
							}

							// 署名・暗号化対象外ファイルの場合はスキップ
							if (!SignUtil.isSignEncrTarget(sess, docObj)) {
								// ドキュメント[{0}]は、署名・暗号化対象外のファイルです。
								String[] message = {docObj.getName()};
								MessageItem msgItem = new MessageItem("EIM.ERROR.LOGIC.OUT.OF.SIGN.AND.ENCR", message, docObj);
								doWarnAct(sess, signObj, signStatusAttrType, log, msgItem);
								isTagSignEncryptExce = true;
								continue;
							}

							// デフォルトフォーマットの取得
							EIMFormat format = null;
							if (formatMap.containsKey(new Long(docObj.getType().getId()))) {
								// 既に取得済みの場合はMapから取得
								format = (EIMFormat)formatMap.get(new Long(docObj.getType().getId()));
							} else {
								// 未取得の場合はMapに格納しておく
								format = FileUtils.getDefaultFormat(sess, docObj.getType());
								formatMap.put(new Long(docObj.getType().getId()), format);
							}

							if (format == null) {
								// ドキュメントタイプ[{0}]のフォーマットが取得できませんでした。
								String[] message = {docObj.getType().getName()};
								MessageItem msgItem = new MessageItem("EIM.ERROR.LOGIC.NODEFAULTFORM.WITHTYPENAME", message, docObj);
								msgItemList.add(msgItem);
								doErrorAct(sess, signObj, null, signStatusAttrType, log, msgItem);
								continue;
							}

							// 原本ファイルの取得
							EIMFile baseFile = FileUtils.getFile(sess, docObj, format);

							if (baseFile == null) {
								// ドキュメント[{0}]の原本ファイルが取得できませんでした。
								String[] message = {docObj.getName()};
								MessageItem msgItem = new MessageItem("EIM.ERROR.LOGIC.NODOCFILE.WITHDOCNAME", message, docObj);
								msgItemList.add(msgItem);
								doErrorAct(sess, signObj, null, signStatusAttrType, log, msgItem);
								continue;
							}

							// 原本ファイルの物理ファイルを取得
							File orgFile = new File(baseFile.getDirectory().getPath() + FileUtils.getFileName(docObj, baseFile));

							if (!orgFile.exists()) {
								// ドキュメント[{0}]の原本ファイルが取得できませんでした。
								String[] message = {docObj.getName()};
								MessageItem msgItem = new MessageItem("EIM.ERROR.LOGIC.NODOCFILE.WITHDOCNAME", message, docObj);
								msgItemList.add(msgItem);
								doErrorAct(sess, signObj, null, signStatusAttrType, log, msgItem);
								continue;
							}

							// 原本ファイルのコピー
							File copyFile = new File(EIMConfig.get("SIGNENCR_INPUT_DIR_PATH") + "/" + docObj.getName());
							try {
								FileUtils.copyFile(orgFile, copyFile);
							} catch (Exception e) {
								// 原本ファイルのコピーに失敗しました。
								MessageItem msgItem = new MessageItem("EIM.LOGIC.ERROR.COPY.FAILED.BASEFILE", null, docObj);
								msgItemList.add(msgItem);
								doErrorAct(sess, signObj, null, signStatusAttrType, log, msgItem);
								continue;
							}

							// 署名・暗号化ツール呼び出しコマンドの作成
							String cmd = EIMConfig.get("SIGNENCR_EXE")
											+ _blank + EIMConfig.get("SIGNENCR_INPUT_DIR_CMD") + _blank + EIMConfig.get("SIGNENCR_INPUT_DIR_PATH") + "/" + docObj.getName()
											+ _blank + EIMConfig.get("SIGNENCR_OUTPUT_DIR_CMD") + _blank + EIMConfig.get("SIGNENCR_OUTPUT_DIR_PATH")
											+ _blank + EIMConfig.get("SIGNENCR_USEJAVA_CMD")
											+ _blank + EIMConfig.get("SIGNENCR_ADD_CMD");

							// DB接続を切断する (署名・暗号化ツールに時間がかかるため)
							sess.close();
							sess = null;	// このバッチのみ明示的に開放

							Process process = null;
							InputStream is = null;
							BufferedReader br = null;

							// 署名・暗号化ツール実施
							try {
								process = Runtime.getRuntime().exec(cmd);
								is = process.getInputStream();
								br = new BufferedReader(new InputStreamReader(is));
							} catch (Exception e) {
								// 署名・暗号化ツールの実施に失敗しました。
								MessageItem msgItem = new MessageItem("EIM.ERROR.LOGIC.SIGN.AND.ENCR.EXECFAILED", null, docObj);
								msgItemList.add(msgItem);
								doErrorAct(sess, signObj, copyFile, signStatusAttrType, log, msgItem);
								continue;
							}

							// 出力先の取得
							String outPutPath = null;	// <出力先ワークディレクトリのフルパス> + <署名・暗号化ファイル名>
							String line = null;
							while((line = br.readLine()) != null)
							{
								// 「NLMファイル生成先: 」を含んでいる行の場合、出力先を文字列切り出しで取得する
								if (line.indexOf(EIMConfig.get("SIGNENCR_CONSOLE_OUTPUT_FILENAME_STRING")) != -1) {
									outPutPath = (line.split(EIMConfig.get("SIGNENCR_CONSOLE_OUTPUT_FILENAME_STRING"))[1]).trim();
								}
							}

							// 実施結果の取得
							// ※署名暗号化ツール(サブプロセス)の終了を待つ
							int rcode = process.waitFor();

							// DB接続
							sess = getEIMSession(sess);

							// 成功の場合
							if (rcode == 0) {

								// 署名・暗号化ファイルの取得
								File signFile = new File(outPutPath);

								// 署名・暗号化ファイルのコピー
								File dstFile = new File(signFormat.getDirectory().getPath() + docObj.getId() + StringUtils.nullToBlank(StringUtils.getFileExt(signFile.getName())));
								FileUtils.copyFile(signFile, dstFile);

								// 署名・暗号化フォーマットにチェックイン
								FileUtils.checkin(sess, docObj, signFormat, signFile.getName(), signFile.length());

								// 署名・暗号化状態の更新
								ObjectAttributeUtils.setAttribute(sess, docObj, signStatusAttrType, AppConstant.SIGNENCR_KIND_SIGNENCR);

								// 署名・暗号化バージョンの設定
								ObjectAttributeUtils.setAttribute(sess, docObj, signVersionAttrType, signVersion);

								// 「署名・暗号化」オブジェクトの削除
								ObjectUtils.deleteObject(sess, signObj);

								// セッション変更クラス生成
								ChangeSessionHelper sessHelper = new ChangeSessionHelper(sess);
								try{
									// システムユーザのセッションを一時的に実施者ユーザに変更
									sessHelper.setUserSessionConsole(sess, userId);

									// アクセス履歴の生成
									AccessUtils.createAccess(sess, docObj, "EIM.ACCESS.TYPE.TAG.SIGN.AND.ENCR");	// 署名・暗号化
								} catch (Exception e) {
									throw e;

								}finally{
									// セッションをシステムユーザに戻す
									sessHelper.setSystemUserSessionConsole(sess);
								}

								// コピーした原本ファイルの削除
								copyFile.delete();

								// 生成した署名・暗号化ファイルを含むワークディレクトリの削除
								deleteFileRecurrently(signFile.getParentFile().getPath());

								// SearchFramework 検索FW更新通知 対象：ドキュメント
								updateNoticeSignEncryption(sess, docObj, true);

								// 1ドキュメント毎にコミット
								sess.commit();

								// ログ出力 : 署名・暗号化完了 - [処理対象ファイル] <処理対象EIMObjectのドキュメント名>(<処理対象EIMObjectのオブジェクトID>)
								log.info(EIMResource.getMessage("EIM.LOG.SIGN.AND.ENCR.COMPLETE") + " " + docObj.getName() + "(" + docObj.getId() + ")");

								isTagSignEncryptExce = true;

							// 失敗の場合
							} else {
								// 署名・暗号化ツールがエラーを返却しました。 [{0}]
								String[] message = {String.valueOf(rcode)};
								MessageItem msgItem = new MessageItem("EIM.ERROR.LOGIC.ERROR.RET.SIGN.AND.ENCR.TOOL", message, docObj);
								msgItemList.add(msgItem);
								doErrorAct(sess, signObj, copyFile, signStatusAttrType, log, msgItem);
								continue;
							}

						}// ループ(3) ; ドキュメント単位 END

						// タグの「署名・暗号化状態」を更新
						if (isTagSignEncryptExce) {
							// 当該「署名・暗号化」オブジェクトが属性「選択タグ」を保持する場合
							long selectTagId = AppObjectUtil.getIntAttr(sess, (EIMObject)objList.get(0), EIMConfig.get("ATTR_NAME_SIGNENC_SELECT_TAG"), Integer.MIN_VALUE);
							if (selectTagId != Integer.MIN_VALUE) {

								// 選択タグのオブジェクトを取得
								EIMObject tagObj = ObjectUtils.getObjectById(sess, selectTagId);

								if (tagObj == null) {
									// 署名・暗号化処理の対象に指定したタグ[ID : {0}]を取得できませんでした。
									String[] message = {String.valueOf(selectTagId)};
									log.error(EIMResource.getMessage("EIM.ERROR.LOGIC.NOTFOUND.TAG.FOR.SIGN.AND.ENCR", message));
									continue;
								}
								// 当該タグ、およびその配下タグが「署名・暗号化済み」として良い状態の場合、「署名・暗号化済み」に更新
								EIMThreadContext.put("SEARCHFW.SIGNTAG.KEY", "SEARCHFW_BATCH_SIGNENCRYPT_TAG");
								EIMThreadContext.put("SEARCHFW.SIGNCHILDTAG.KEY", "SEARCHFW_BATCH_SIGNENCRYPT_CHILD_TAG");
								SignUtil.setTagSignFlagOn(sess, tagObj);

								// タグ更新がある場合はグループ単位でコミット
								sess.commit();
							}
						}

					}// ループ(2) : グループ(選択タグ/ドキュメント)単位 END

					// 失敗情報が存在する、かつ、実施ユーザがメールアドレスを保持する場合
					if (msgItemList.size() > 0 && !StringUtils.isBlank(user.getMail())) {
						// 失敗情報をメール通知
						MailUtil.sendSignEncryptionFailedMail(sess, user, msgItemList);
					}
				}

			}// ループ(1) : 上部メニュー操作の単位 END

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
			// ログ出力 : 署名・暗号化バッチ処理終了。
			log.info(EIMResource.getMessage("EIM.INFO.SIGN.AND.ENCR.END"));
		}
	}

	/**
	 * オブジェクト名、属性「選択タグ」の値が同一のEIMObject毎にリストに格納して、それらリストを格納したリストを返します。
	 *
	 * <li>引数オブジェクトリストの1件目のEIMObjectと同一名称のもののみを返却対象とします。
	 * <li>属性「選択タグ」の値がないEIMObjectはそれぞれ別のグループとして扱います。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param objList グルーピング対象となるEIMObjectのリスト
	 * @return 同一オブジェクト名、属性「選択タグ」の値のEIMObjectを格納したリストのリスト
	 * @throws Exception
	 */
	private static List getGroupList(EIMSession sess, List objList) throws Exception {

		List groupList = new ArrayList();		// 同一オブジェクト名、属性「選択タグ」の値毎のリストを格納したリスト
		List noAttrValueList = new ArrayList();	// 「選択タグ」の値がない(ドキュメントを直接指定の場合)EIMObjectのリスト
		HashMap sameAttrMap = new HashMap();	// [key]属性「選択タグ」の値、[value]同一の「選択タグ」のEIMObjectを格納したリスト

		EIMObject firstObj = (EIMObject)objList.get(0);
		for (Iterator iter = objList.iterator(); iter.hasNext();) {
			EIMObject obj = (EIMObject) iter.next();

			// 1件目のEIMObjectと同一名称をグルーピング
			if (firstObj.getName().equals(obj.getName())) {

				String selectTag = AppObjectUtil.getStrAttr(sess, obj, EIMConfig.get("ATTR_NAME_SIGNENC_SELECT_TAG"));	// 選択タグ

				if (sameAttrMap.containsKey(selectTag)) {
					// 「選択タグ」の値が既に発見済みの場合は既存リストに追加
					((List)sameAttrMap.get(selectTag)).add(obj);
				} else if (StringUtils.isBlank(selectTag)) {
					// 「選択タグ」の値がない(ドキュメントを直接指定の)場合
					List tmpList = new ArrayList();
					tmpList.add(obj);
					noAttrValueList.add(tmpList);
				} else {
					// 未発見の「選択タグ」の値の場合はリストを新規作成
					List tmpList = new ArrayList();
					tmpList.add(obj);
					sameAttrMap.put(selectTag, tmpList);
					groupList.add(tmpList);
				}
			}
		}
		// 「選択タグ」なしのEIMObjectリストを結合
		groupList.addAll(noAttrValueList);
		return groupList;
	}

	/**
	 * 警告後処理
	 *
	 * @param sess EIMSessionインスタンス
	 * @param signObj 署名・暗号化オブジェクト
	 * @param signStatusAttrType 「署名・暗号化状態」属性タイプ (冗長な取得回避用)
	 * @param log ログ
	 * @param msgItem 警告内容を格納したmessageItem
	 * @throws Exception
	 */
	private static void doWarnAct (EIMSession sess, EIMObject signObj, EIMAttributeType signStatusAttrType, Log log, MessageItem msgItem) throws Exception {

		// 署名・暗号化状態の更新
		EIMObject docObj = msgItem.getEimObject();
		if (docObj != null) {
			// 「署名・暗号化処理中」の場合、「署名・暗号化未実施」に変更する
			long signStsId = AppObjectUtil.getIntAttr(sess, docObj, EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_STATUS"), Integer.MIN_VALUE);
			if (signStsId == AppConstant.SIGNENCR_KIND_PROCESSING_SIGNENCR) {
				ObjectAttributeUtils.setAttribute(sess, docObj, signStatusAttrType, AppConstant.SIGNENCR_KIND_NOT_SIGNENCR);
			}
		}

		// 署名・暗号化オブジェクトの削除
		ObjectUtils.deleteObject(sess, signObj);

		String objName = docObj != null ? docObj.getName() : "";				// ドキュメント名
		String objId = docObj != null ? String.valueOf(docObj.getId()) : "-";	// ドキュメントのオブジェクトID

		// ログ出力 : 署名・暗号化警告 - [処理対象ファイル] <処理対象EIMObjectのﾄﾞｷｭﾒﾝﾄ名>(<処理対象EIMObjectのｵﾌﾞｼﾞｪｸﾄID>) [警告内容] <警告メッセージ>
		log.warn(EIMResource.getMessage("EIM.LOG.SIGN.AND.ENCR.WARN") + _blank + objName + "(" + objId + ")" + _blank
				+ EIMResource.getMessage("EIM.LOG.SIGN.AND.ENCR.WARN.CONTENT") + _blank + msgItem.getMessage());

		// ドキュメント1件毎にコミット
		sess.commit();
	}

	/**
	 * 失敗後処理
	 *
	 * <li>生成した署名・暗号化ファイル、およびそのワークディレクトリは、ここにくる時にはまだ作成されていない、
	 * または、作成済みでもエラー解析のために保管する必要があるため、ここでは削除しない。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param signObj 署名・暗号化オブジェクト
	 * @param copyFile コピーした原本ファイル
	 * @param signStatusAttrType 「署名・暗号化状態」属性タイプ (冗長な取得回避用)
	 * @param log ログ
	 * @param msgItem エラー内容を格納したmessageItem
	 * @throws Exception
	 */
	private static void doErrorAct (EIMSession sess, EIMObject signObj, File copyFile, EIMAttributeType signStatusAttrType, Log log, MessageItem msgItem) throws Exception {

		// 原本ファイルのコピーの削除
		if (copyFile != null && copyFile.exists()) {
			copyFile.delete();
		}

		// DB接続
		sess = getEIMSession(sess);

		// 署名・暗号化状態の更新
		EIMObject docObj = msgItem.getEimObject();
		if (docObj != null) {
			ObjectAttributeUtils.setAttribute(sess, docObj, signStatusAttrType, AppConstant.SIGNENCR_KIND_FAILED);
		}

		// 署名・暗号化オブジェクトの削除
		ObjectUtils.deleteObject(sess, signObj);

		// 更新通知(ドキュメント)
		updateNoticeSignEncryption(sess, signObj, true);

		String objName = docObj != null ? docObj.getName() : "";				// ドキュメント名
		String objId = docObj != null ? String.valueOf(docObj.getId()) : "-";	// ドキュメントのオブジェクトID

		// ログ出力 : 署名・暗号化失敗 - [処理対象ファイル] <処理対象EIMObjectのドキュメント名>(<処理対象EIMObjectのオブジェクトID>) [失敗内容] <失敗メッセージ>
		log.error(EIMResource.getMessage("EIM.LOG.SIGN.AND.ENCR.FAILED") + _blank + objName + "(" + objId + ")" + _blank
				+ EIMResource.getMessage("EIM.LOG.SIGN.AND.ENCR.FAILED.CONTENT") + _blank + msgItem.getMessage());

		// ドキュメント1件毎にコミット
		sess.commit();
	}

	/**
	 * DBが切断されている場合、再接続します。
	 *
	 * <li>DB接続でエラーが発生した場合、一定時間待機した後、リトライします。
	 *
	 * @param sess EIMSessionインスタンス
	 * @return 取得したEIMSessionインスタンス
	 * @throws Exception
	 */
	private static EIMSession getEIMSession(EIMSession sess) throws Exception {

		// DBが切断されている場合、DBコネクションの再取得
		if (sess == null) {
			// DB接続
			boolean isRetry = true;
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
		return sess;
	}

	/**
	 * (再起処理) 指定のパス配下の物理ファイル、物理ディレクトリを削除します。
	 *
	 * @param path パス
	 */
	private static void deleteFileRecurrently(String path) {

		File file = new File(path);
		// ディレクトリの場合
		if (file.isDirectory()) {
			String[] files = file.list();
			for (int i = 0 ; i < files.length ; i++) {
				// 再起呼び出し
				deleteFileRecurrently(path + "/" + files[i]);
			}
		}
		file.delete();
	}

	/**
	 * ThreadLocal設定して更新通知
	 * (接続と切断が繰り替えし行われるため、本メソッド内でセットしたほうが確実のためここでセットする)
	 *
	 * @param sess	セッション
	 * @param object	対象オブジェクト
	 * @param docFlg	ドキュメントかどうかのフラグ true:ドキュメント、false:タグ
	 */
	private static void updateNoticeSignEncryption(EIMSession sess, EIMObject object, boolean docFlg) throws Exception{

		boolean sessPutFlg = false;

		// 設定が有効な場合のみThreadLocal設定
		if( AppUpdateNoticeUtils.doEntry() &&
				AppUpdateNoticeUtils.isEnabledDataType("SEARCHFW_BATCH_SIGNENCRYPT_DOCUMENT")) {

			if(EIMThreadContext.getEIMSession() == null){
				EIMThreadContext.putEIMSession(sess);
				sessPutFlg = true;
			}

			if(docFlg)
			{
				// SearchFramework 検索FW更新通知 対象：ドキュメント
				AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), "SEARCHFW_BATCH_SIGNENCRYPT_DOCUMENT");
			}

			if(sessPutFlg){
				EIMThreadContext.removeEIMSession();
			}

		}
	}
}