package jp.co.ctc_g.eim.app.document.presentation.batch;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.sql.DataSource;

import jakarta.mail.MessagingException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import common.util.AppConstant;
import common.util.AppMessageUtils;
import common.util.AppObjectTypeUtil;
import common.util.AppObjectUtil;
import eim.bo.EIMAccessRole;
import eim.bo.EIMException;
import eim.bo.EIMObject;
import eim.bo.EIMRelation;
import eim.bo.EIMRelationType;
import eim.bo.EIMResource;
import eim.bo.EIMUser;
import eim.bo.EIMVersion;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.MailUtils;
import eim.util.ObjectUtils;
import eim.util.RelationUtils;
import eim.util.SecurityUtils;
import eim.util.UserUtils;
import eim.util.VersionUtils;
import jp.co.ctc_g.eim.app.document.business.domain.DocumentDomain;
import jp.co.ctc_g.eim.app.document.business.service.DocumentService;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.AccumulationMailCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AccumulationMailDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.MailDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.MailTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchConditionCompare;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchConditionGroup;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchConditionIsNull;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchLimitCountCondition;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchSelectObject;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchSelectObject.SearchConditionBuildHelper;
import jp.co.ctc_g.eim.framework2.business.service.AccumulationMailService;
import jp.co.ctc_g.eim.framework2.business.service.AttributeTypeService;
import jp.co.ctc_g.eim.framework2.business.service.MailService;
import jp.co.ctc_g.eim.framework2.business.service.ObjectService;
import jp.co.ctc_g.eim.framework2.business.service.UserService;
import jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework2.common.context.TransactionContext;
import jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;

/**
 * 有効期限切れドキュメント削除（ごみ箱移動)処理。
 * <br>
 * 有効期限切れドキュメントを論理削除(ごみ箱へ移動)し、更新者または作成者または管理用メールアドレスに削除通知メールを送信します。
 *
 */
public class DisposeExpiredDocs {

	/** ログ */
	private static Log log = LogFactory.getLog(DisposeExpiredDocs.class);

	/** TransactionContext */
	private static TransactionContext context = null;
	/** コンテキストファイル */
	private static final String[] contextFiles = {"applicationContext.xml"};
	/** Session */
	private static EIMSession sess = null;

	/** ユーザサービス */
	private static UserService userService;
	/** オブジェクトサービス */
	private static ObjectService objectService;
	/** 属性タイプサービス */
	private static AttributeTypeService attributeTypeService;
	/** メールサービス */
	private static MailService mailService;
	/** 蓄積メールサービス */
	private static AccumulationMailService accumulationMailService;
	/** ドキュメントサービス */
	private static DocumentService documentService;

	/** 通知フラグ */
	private static boolean notification = false;
	/** 管理用アドレス */
	private static String adminAddress = "";
	/** 管理アドレスフラグ */
	private static boolean toManagementFlag = false;
	/** 削除通知1件に記載するドキュメント上限*/
	private static String maxDocCount = "";

	/** リレーションタイプ*/
	private static EIMRelationType eimRelType = null;

	/** ごみ箱パス */
	private static final String PATH_TRASH_BOX = "/ごみ箱/";

	/**
	 * Main Function
	 * @param args
	 * @throws Exception
	 * @since Ver6.91
	 */
	public static void main(String[] args) throws Exception {

		// 開始ログ
		log.info("有効期限切れドキュメント削除開始");

		try {
			try {
				//コンテキストファイル読み込み
				ApplicationContextLoader.init(contextFiles);
				//トランザクション
				context = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
				EIMThreadContext.putTransactionContext(context);
				//サービス取得
				init();
			} catch(Exception e) {
				throw new Exception("DB接続でエラーが発生しました。", e);
			}

			// システム管理者をTransactionContextに再登録
			context.setUser(userService.getById(1));

			// user取得
			EIMUser sessUser = new EIMUser(1, null, null, null, null, null, 255, 0, null);
			// lang取得
			String lang = "";
			String EIM_CONFIG_LANG = "MESSAGELANG";
			String DEFAULT_LANG = "JA";
			if (EIMConfig.get(EIM_CONFIG_LANG) != null) {
				lang = EIMConfig.get(EIM_CONFIG_LANG);
			} else {
				lang = DEFAULT_LANG;
			}

			// セッション
			DataSource ds = (DataSource) ApplicationContextLoader.getApplicationContext().getBean("dataSource");
			sess = new EIMSession();
			sess.setConsoleMode(true);
			sess.setUser(sessUser);
			sess.setLang(lang);
			sess.setConnection(ds.getConnection());
			sess.getDBConnection().setAutoCommit(false);
			EIMThreadContext.putEIMSession(sess);
			// context,sessともに同じDBConnectionを使用する
			context.setDBConnection(sess.getDBConnection());
			
			// 設定チェック
			checkSettingValue();

			// 現在日時
			SimpleDateFormat sdf = new SimpleDateFormat("yyyy/MM/dd");
			Date date = sdf.parse(sdf.format(new Date()));

			List<ObjectDomain> expiredDocs = new ArrayList<>();
			// ドキュメント、およびドキュメントの子のオブジェクトタイプを取得
			@SuppressWarnings("rawtypes")
			Map objTypeMap = AppObjectTypeUtil.getObjTypeMap(sess, ConfigUtils.getByKey("OBJECT_TYPE_NAME_DOCUMENT"));
			@SuppressWarnings("unchecked")
			Iterator<Long> objTypeIds = objTypeMap.keySet().iterator();
			while(objTypeIds.hasNext()) {
				// タイプごとに有効期限切れドキュメント取得
				long objTypeId = (Long)objTypeIds.next();
				expiredDocs.addAll(getExpiredObjList(date, objTypeId));
			}

			if (expiredDocs.size() < 1) {
				log.info("有効期限切れドキュメントが存在しません。");
				return;
			}

			// 有効期限切れドキュメント削除
			disposeExpiredDocs(expiredDocs);

			// 通知ONの際、メール送信
			if (notification) {
				sendAccumulateMail();
			}

		}catch(EIMException eime) {
			log.error(AppMessageUtils.makeLogMessage(eime.getMessage()), eime);
			try{
				context.getDBConnection().rollback();
			} catch (Exception se) {
				log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
			}
		} catch(Exception e) {
			log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
			try {
				context.getDBConnection().rollback();
			} catch (Exception se) {
				log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
			}
		} finally {
			try{
				if(sess != null){
					sess.close();
				}
			} catch (Exception se) {
				log.warn(se.getMessage(), se);
			}

			// 終了ログ
			log.info("有効期限切れドキュメント削除終了");
		}
		System.exit(0);
	}

	/**
	 * 有効期限切れドキュメント取得処理。
	 *
	 * @param date 処理日時
	 * @param objType オブジェクトタイプ文字列
	 * @return 有効期限切れドキュメントリスト
	 * @throws Exception
	 */
	private static List<ObjectDomain> getExpiredObjList(Date date, long objTypeId) throws Exception {

		List<ObjectDomain> result = new ArrayList<>();

		// 有効期限切れオブジェクトを取得する
		SearchSelectObject selectObject = new SearchSelectObject();
		SearchConditionBuildHelper helper = new SearchConditionBuildHelper();
		SearchConditionGroup conditionGroup = helper.group(helper.opAnd());

		// オブジェクトタイプを条件に設定
		conditionGroup.addCondition(new SearchConditionCompare(helper.opAnd(), SearchSelectObject.PsedoAttributeTypeEnum.TYPE, helper.opEq(), objTypeId));
		// 最新フラグを条件に設定
		conditionGroup.addCondition(new SearchConditionCompare(helper.opAnd(), SearchSelectObject.PsedoAttributeTypeEnum.LATEST, helper.opEq(), 1));
		// 有効期限を条件に設定
		AttributeTypeDomain expiredAttrType = attributeTypeService.getByDefinitionName(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_EFFECT_DATE"));
		conditionGroup.addCondition(new SearchConditionCompare(helper.opAnd(), expiredAttrType, helper.opLt(), date));
		// 削除日時を条件に設定
		AttributeTypeDomain deleteAttrType = attributeTypeService.getByDefinitionName(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_DELETE_DATE"));
		conditionGroup.addCondition(new SearchConditionIsNull(helper.opAnd(), deleteAttrType, helper.opIsNull()));

		// パスを条件に設定（「/ごみ箱/」以外)
		AttributeTypeDomain attrTypePass = attributeTypeService.getByDefinitionName(ConfigUtils.getByKey("ATTR_NAME_FOLDER_PASS"));
		conditionGroup.addCondition(new SearchConditionCompare(helper.opAnd(), attrTypePass, helper.opNe(), PATH_TRASH_BOX));
		selectObject.setCondition(conditionGroup);
		// 上限なし
		SearchLimitCountCondition limitCondition = new SearchLimitCountCondition(-2, true);

		result = objectService.getList(selectObject, limitCondition);
		return result;
	}

	/**
	 * 削除処理。
	 * <br>
	 * 有効期限切れドキュメントを削除します。<br>
	 *
	 * @param experiodDocs 有効期限切れドキュメントリスト
	 * @throws Exception
	 */
	private static void disposeExpiredDocs(List<ObjectDomain> expiredDocs) throws Exception {

		// ごみ箱取得
		EIMObject recycleObj = AppObjectUtil.getObject(sess,EIMConfig.get("OBJECT_TYPE_NAME_RECYCLE"), EIMConfig.get("OBJECT_TYPE_NAME_RECYCLE"));

		if (recycleObj == null) {
			throw new Exception("ごみ箱が存在しません。");
		}

		EIMObject object = null;
		List<EIMObject> allRevObjList = null;
		String path = null;
		for (ObjectDomain expiredDoc :expiredDocs) {
			try {
				toManagementFlag = false;

				// Object再取得
				object = ObjectUtils.getObjectById(sess, expiredDoc.getId());

				// 対象オブジェクトのパス
				path = expiredDoc.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_PASS")).getString();

				// 対象ドキュメントの全履歴オブジェクトを取得
				allRevObjList = getAllDocRev(object);
				// 対象ドキュメントの過去版を含む全ドキュメンリンクを削除する
				deleteDocLink(allRevObjList);

				// 論理削除(ワークスペース固有ごみ箱へ移動)
				List<DocumentDomain> documentDomainList = new ArrayList<>();
				DocumentDomain objDocDomain = new DocumentDomain();
				objDocDomain.setId(expiredDoc.getId());
				objDocDomain.setAttributeList(expiredDoc.getAttributeList());
				documentDomainList.add(objDocDomain);

				try {
					documentService.dispose(documentDomainList);
				} catch (Exception e) {
					log.info("削除不可ドキュメント。 ID:" + object.getId() + " " + path + object.getName());
					log.debug("削除不可ドキュメント。 ID:" + object.getId() + " " + path + object.getName(), e);
					// rollback
					context.getDBConnection().rollback();
					continue;
				}

				// 通知ONの場合
				if (notification) {
					// メール作成
					List<String> toUser = createAccumulateMail(expiredDoc);
					if (!toManagementFlag) {
						log.info("削除。 ID:" + object.getId() + " " + path + object.getName() + " 宛先:" + toUser.get(0));
					} else {
						log.info("削除。 ID:" + object.getId() + " " + path + object.getName() + " 宛先:管理用メールアドレス");
					}
				} else {
					log.info("削除。 ID:" + object.getId() + " " + path + object.getName());
				}

				// commit
				context.getDBConnection().commit();
			} catch (Exception e) {
				log.error("削除失敗。 ID:" + object.getId() + " " + path + object.getName(), e);
				// rollback
				context.getDBConnection().rollback();
			}
		}
	}

	/**
	 * ドキュメント全リビジョン取得
	 * @param obj
	 * @return
	 * @throws Exception
	 */
	@SuppressWarnings("unchecked")
	private static List<EIMObject> getAllDocRev(EIMObject obj) throws Exception {

		List<EIMObject> objectList = new ArrayList<>();
		if (obj.getRevision() != 0) {
			// バージョン
			EIMVersion version = VersionUtils.getVersion(sess, obj);
			// 対象ドキュメントの全履歴オブジェクトを取得
			objectList = version.getList();
		} else {
			objectList.add(obj);
		}


		return objectList;
	}

	/**
	 * リンク削除処理。
	 * <br>
	 * 削除ドキュメントの過去版を含む全リンクを削除します。
	 * @param object 削除ドキュメント
	 * @throws Exception
	 */
	@SuppressWarnings({ "rawtypes" })
	private static void deleteDocLink(List<EIMObject> objectList) throws Exception {

		for (EIMObject obj: objectList) {
			// 各版ごとのリンクを取得
			List docLinkList = RelationUtils.getParentRelationListByRelType(sess, obj, eimRelType);
			if (docLinkList.size() > 0) {
				for (int i = 0; i < docLinkList.size(); i++) {
					EIMRelation docLinkRel = (EIMRelation)docLinkList.get(i);
					if (obj.getId() == docLinkRel.getChild().getId() &&
							docLinkRel.getType().getId() == eimRelType.getId()) {
						// リンク削除
						RelationUtils.deleteRelation(sess, docLinkRel);
					}
				}
			}
		}
	}

	/**
	 * 蓄積メール作成処理。
	 * <br>
	 * 削除通知メールを作成しDBに蓄積します。
	 *
	 * @param obj 削除ドキュメント
	 * @throws Exception
	 */
	private static List<String> createAccumulateMail(ObjectDomain obj) throws Exception {

		List<String> address = new ArrayList<>();

		MailTypeDomain mailTypeDomain = null;
		MailDomain mail = null;
		ObjectDomain mailObj = obj.clone();

		// 宛先取得
		// ※削除後(ごみ箱移動後)は対象オブジェクトがシステムセキュリティとなり、更新者と作成者が参照権限を保持しないケースがあるため、
		//  メールプラグインではなくバッチ処理内で宛先を取得する。
		List<UserDomain> destinationUsers = getDestinationUser(mailObj);

		for (UserDomain destinationUser: destinationUsers) {
			// メール用オブジェクトの更新者に宛先ユーザを設定
			mailObj.setModificationUser(destinationUser);

			mailTypeDomain = new MailTypeDomain();
			mailTypeDomain.setId(AppConstant.MAIL_TYPE_ID_DISPOSE_EXPIRED_DOCS);

			mail = new MailDomain();
			mail.setObject(mailObj);
			mail.setType(mailTypeDomain);

			// 一括送信用にメール蓄積
			mailService.accumulate(mail);

			address.add(destinationUser.getMail());
		}

		if (address.size() == 0) {
			address.add("");
		}

		return address;
	}

	/**
	 * 宛先ユーザ取得
	 *
	 * @param obj 対象オブジェクト
	 * @return 宛先ユーザリスト
	 * @throws Exception
	 */
	private static List<UserDomain> getDestinationUser(ObjectDomain obj) throws Exception {

		List<UserDomain> destinationUser = new ArrayList<>();

		// 更新者
		UserDomain modifyUser = obj.getModificationUser();
		if (checkAuth(modifyUser, obj.getId())) {
			modifyUser = userService.getById(modifyUser.getId());
			destinationUser.add(modifyUser);
			return destinationUser;
		}

		// 作成者
		UserDomain createUser = obj.getCreationUser();
		if (obj.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_CREATE")) != null) {
			createUser = userService.getById(obj.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_CREATE")).getLong());
		}
		if (checkAuth(createUser, obj.getId())) {
			if (createUser.getLang() == null) {
				createUser = userService.getById(createUser.getId());
			}
			destinationUser.add(createUser);
			return destinationUser;
		}

		// 更新者と作成者が無効、メールアドレス設定なし、参照権限なしの場合、設定ファイルの管理用メールアドレスを返却する
		String[] mgAddress = adminAddress.split(",");
		EIMUser user = null;
		UserDomain userDomain = null;
		for (String address: mgAddress) {
			// 管理フラグをtruにする
			toManagementFlag = true;
			userDomain = new UserDomain();
			// メール宛先用にユーザドメイン生成
			user = UserUtils.getUserByMail(EIMThreadContext.getEIMSession(), address);
			if (user != null) {
				userDomain.setId(user.getId());
				userDomain.setLang(user.getLang());
			} else {
				// アドレスから登録済みユーザが取得できない場合、言語を一律日本語に設定
				userDomain.setLang("JA");
			}
			userDomain.setMail(address);
			destinationUser.add(userDomain);
		}
		return destinationUser;

	}

	/**
	 * メール宛先チェック
	 * <br>
	 * 対象ユーザが有効、メールアドレス設定済み、オブジェクトに参照権限を保持している場合にtrueを返却します。
	 *
	 * @param user チェック対象ユーザ
	 * @param objId オブジェクトID
	 * @return チェック結果
	 * @throws Exception
	 */
	private static boolean checkAuth(UserDomain user, long objId) throws Exception {

		// 無効フラグ判定
		if (user.isDisable()) {
			return false;
		}
		// メールアドレス判定
		if (user.getMail() == null || user.getMail().equals("")) {
			return false;
		}

		// 参照権限チェック
		if(!SecurityUtils.authorized(sess,
				new EIMObject(objId, null, null, 0, false, null, null, null, null, null, null, false, false, null),
				new EIMUser(user.getId(), null, null, null, null, null, 0, 0, null),
				EIMAccessRole.READ)){
			return false;
		}
		return true;

	}

	/**
	 * 蓄積メール送信処理。
	 * <br>
	 * DBに蓄積した削除通知メールをユーザごとに一括送信します。
	 *
	 * @throws Exception
	 */
	private static void sendAccumulateMail() throws Exception {

		AccumulationMailCriteria criteria = new AccumulationMailCriteria();
		criteria.setType("DisposeExpiredDocs");
		List<AccumulationMailDomain> mailAccumulateList = accumulationMailService.getList(criteria);

		if(mailAccumulateList.size() == 0){
			return;
		}

		// アドレス単位に分類
		List<List<AccumulationMailDomain>> sendMailList = separatePerson(mailAccumulateList);

		/*
		 * ユーザ単位でメールを送信する。以下の情報を取得しておく
		 * ・送信済みメール
		 * ・送信失敗メールの送付先
		 */
		List<AccumulationMailDomain> sendedList = new ArrayList<AccumulationMailDomain>();
		List<String> failedList = new ArrayList<String>();
		String newLine = EIMConfig.getValue("MAIL_NEWLINE");

		try {
			for(List<AccumulationMailDomain> personList : sendMailList) {

				/*--- ユーザ情報の取得 ---*/
				AccumulationMailDomain headDomain;
				if (personList.size() > 0) {
					headDomain = personList.get(0);
				} else {
					continue;
				}

				String userAddr = headDomain.getAddress();

				// メール作成
				String title = headDomain.getTitle();
				StringBuffer buff = new StringBuffer();
				String separator = EIMResource.getMessageValue("JA", "APP.MAIL.COMMON.SEPARATE") + newLine;

				int count = 0;
				for (int i = 0; i < personList.size(); i++) {
					AccumulationMailDomain mailAccumulate = personList.get(i);
					// 本文と本文の間にセパレータを入れる
					buff.append(mailAccumulate.getMessage());

					// 本文と本文の間にセパレータを入れる(先頭と最後には入れない)
					if (i < personList.size() - 1) {
						buff.append(separator);
					}

					// 削除対象に追加
					sendedList.add(mailAccumulate);
					count++;

					// 1通のドキュメント上限数に達した場合に送信
					if (count % Integer.parseInt(maxDocCount) == 0) {
						try{
							MailUtils.sendEMail(title, buff.toString(), userAddr);
						} catch(MessagingException me) {
							// 送信失敗時はログ出力対象
							failedList.add(headDomain.getAddress());
						} catch(Exception e) {
							throw e;
						}
						// 本文初期化
						buff = new StringBuffer();
					}
				}
				// 送信
				try{
					if (!buff.toString().equals("")) {
						MailUtils.sendEMail(title, buff.toString(), userAddr);
					}
				} catch(MessagingException me) {
					// 送信失敗時はログ出力対象
					failedList.add(headDomain.getAddress());
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
	 * <br>
	 * 送信メール一覧から重複している送信先アドレスを削除する。
	 *
	 * @param mailAccumulateList 送信メール一覧
	 * @return 送信者アドレス別送信メール一覧
	 * @throws Exception
	 */
	@SuppressWarnings("unchecked")
	private static List<List<AccumulationMailDomain>> separatePerson(List<AccumulationMailDomain> mailAccumulateList) throws Exception{

		List<List<AccumulationMailDomain>> sendMailList = new ArrayList<List<AccumulationMailDomain>>();

		//送信先アドレスでソートする
		mailAccumulateList = AppObjectUtil.getStrSortedList(mailAccumulateList, "getAddress", true);

		String prevAddr = "";
		List<AccumulationMailDomain> individualList = null;
		for(AccumulationMailDomain mailAccumulate : mailAccumulateList){
			if(!prevAddr.equals(mailAccumulate.getAddress())){
				prevAddr = mailAccumulate.getAddress();
				individualList = new ArrayList<AccumulationMailDomain>();
				individualList.add(mailAccumulate);
				sendMailList.add(individualList);
			}else{
				individualList.add(mailAccumulate);
			}
		}

		return sendMailList;

	}

	/**
	 * 処理済みのメールを蓄積用DBから削除します。
	 *
	 * @param mailAccumulateList 処理済みメール一覧
	 * @throws Exception
	 */
	private static void removeAccumulateMail(List<AccumulationMailDomain> mailAccumulateList) throws Exception{

		if(mailAccumulateList.size() > 0){
			for(AccumulationMailDomain mailAccumulate : mailAccumulateList){
				accumulationMailService.delete(mailAccumulate);
			}
			context.getDBConnection().commit();
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

	/**
	 * 設定チェック<br>
	 * 本バッチで使用する属性、設定値のチェックを行います。
	 *
	 * @throws Exception
	 */
	private static void checkSettingValue() throws Exception {

		// 属性「削除日時」チェック
		AttributeTypeDomain attrTypeDeleteDate = attributeTypeService.getByDefinitionName(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_DELETE_DATE"));
		if (attrTypeDeleteDate == null) {
			throw new Exception("属性「削除日時」が存在しません。");
		}

		// 設定ファイルチェック
		// 削除通知設定
		String sendDeleteNoticeSetting = ConfigUtils.getByKey("SEND_DISPOSE_NOTICE");
		if (sendDeleteNoticeSetting.equals("")) {
			throw new Exception("設定値「SEND_DISPOSE_NOTICE」が空文字です。");
		}
		if (Boolean.valueOf(sendDeleteNoticeSetting)){
			notification = true;
		}

		// 削除通知用管理アドレス
		String adminAddressSetting = ConfigUtils.getByKey("ADMIN_ADDRESS_FOR_DISPOSE_NOTICE");
		if (adminAddressSetting.equals("")) {
			throw new Exception("設定値「ADMIN_ADDRESS_FOR_DISPOSE_NOTICE」が空文字です。");
		}
		adminAddress = adminAddressSetting;

		// 削除通知メール1件に記載するドキュメント上限
		String deleteNoticeDocMaxSetting = ConfigUtils.getByKey("DISPOSE_NOTICE_DOC_MAX");
		if (deleteNoticeDocMaxSetting.equals("")) {
			throw new Exception("設定値「DISPOSE_NOTICE_DOC_MAX」が空文字です。");
		}
		maxDocCount = deleteNoticeDocMaxSetting;

		// リンク取得
		eimRelType = RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_LINK"));
	}

	/**
	 * 初期データの取得を行います
	 * <br>
	 * @throws Exception
	 */
	private static void init() throws Exception {
		documentService = (DocumentService)ApplicationContextLoader.getApplicationContext().getBean("documentService2");
		objectService =(ObjectService)ApplicationContextLoader.getApplicationContext().getBean("objectService2");
		userService = (UserService)ApplicationContextLoader.getApplicationContext().getBean("userService2");
		attributeTypeService = (AttributeTypeService)ApplicationContextLoader.getApplicationContext().getBean("attributeTypeService2");
		mailService = (MailService)ApplicationContextLoader.getApplicationContext().getBean("mailService2");
		accumulationMailService = (AccumulationMailService)ApplicationContextLoader.getApplicationContext().getBean("accumulationMailService2");

	}

}
