package jp.co.ctc_g.eim.app.document.presentation.batch;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
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
import eim.bo.EIMResource;
import eim.bo.EIMUser;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.MailUtils;
import eim.util.SecurityUtils;
import jp.co.ctc_g.eim.app.document.common.util.AppDocumentLogicUtil;
import jp.co.ctc_g.eim.app.document.common.util.AppDocumentUtil;
import jp.co.ctc_g.eim.app.document.common.util.AttributeUtil;
import jp.co.ctc_g.eim.app.document.common.util.VersionUtils;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.AccumulationMailCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AccessRoleTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AccumulationMailDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.MailDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.MailTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.RelationDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.RelationTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchConditionCompare;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchConditionGroup;
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
 * ドキュメント有効期限事前通知処理。
 *
 */
public class SendExpirationDateNotice {

	/** ログ */
	private static Log log = LogFactory.getLog(SendExpirationDateNotice.class);
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

	/** 事前通知日数*/
	private static String expiryNoticeDays = "";
	/** 事前通知1件に記載するドキュメント上限*/
	private static String maxDocCount = "";

	/** ごみ箱パス */
	private static final String PATH_RECYCLE_BOX = "/ごみ箱/";

	/**
	 *  ドキュメント有効期限事前通知処理メインメソッド。<br>
	 *  オブジェクトが保持する有効期限と設定ファイルの事前通知日数をもとに、ユーザに有効期限の事前通知を行います。
	 * @param args
	 * @throws Exception
	 * @since Ver6.91
	 */
	@SuppressWarnings({ "unchecked", "rawtypes" })
	public static void main(String[] args) throws Exception {

		// 開始ログ
		log.info("有効期限事前通知開始");

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

			// 通知対象日時を計算(現在日時 + 事前通知日数)
			SimpleDateFormat sdf = new SimpleDateFormat("yyyy/MM/dd");
			Calendar calendar = Calendar.getInstance();
	        calendar.add(Calendar.DATE, Integer.parseInt(expiryNoticeDays));
			Date expirationDate =  sdf.parse(sdf.format(calendar.getTime()));

			// ドキュメント、およびドキュメントの子のオブジェクトタイプを取得
			Map objTypeMap = AppObjectTypeUtil.getObjTypeMap(sess, ConfigUtils.getByKey("OBJECT_TYPE_NAME_DOCUMENT"));
			Iterator<Long> objTypeIds = objTypeMap.keySet().iterator();
			List<ObjectDomain> resultDocs = new ArrayList<>();
			List<Long> typeIds = new ArrayList<>();
			while(objTypeIds.hasNext()) {
				// 各タイプのID取得
				long objTypeId = (Long)objTypeIds.next();
				typeIds.add(objTypeId);
			}
			// 通知対象ドキュメント取得
			resultDocs =  getNoticeObjList(expirationDate, typeIds);

			if (resultDocs.size() < 1) {
				log.info("通知対象ドキュメントが存在しません。");
				return;
			}

			// メール作成
			createAccumulateMail(resultDocs);

			// メール送信
			sendAccumulateMail();

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
			log.info("有効期限事前通知終了");
		}
		System.exit(0);
	}

	/**
	 * 事前通知対象ドキュメント取得処理。<br>
	 * 最新かつ有効期限が指定日である、指定オブジェクトタイプのオブジェクトを取得します。
	 *
	 * @param date 削除日時
	 * @param typeId オブジェクトタイプID
	 * @return 取得オブジェクトリスト
	 * @throws Exception
	 */
	private static List<ObjectDomain> getNoticeObjList(Date date, List<Long> typeIds) throws Exception {

		List<ObjectDomain> result = new ArrayList<>();

		SearchSelectObject selectObject = new SearchSelectObject();
		SearchConditionBuildHelper helper = new SearchConditionBuildHelper();
		// オブジェクトタイプを条件に設定
		SearchConditionGroup conditionGroup = helper.group(helper.opAnd())
				.addCondition(helper.inArray(helper.opAnd(), SearchSelectObject.PsedoAttributeTypeEnum.TYPE, helper.opIn(), typeIds.toArray()));
		// 最新フラグを条件に設定
		conditionGroup.addCondition(new SearchConditionCompare(helper.opAnd(), SearchSelectObject.PsedoAttributeTypeEnum.LATEST, helper.opEq(), 1));
		// 有効期限を条件に設定
		AttributeTypeDomain attrType = attributeTypeService.getByDefinitionName(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_EFFECT_DATE"));
		conditionGroup.addCondition(new SearchConditionCompare(helper.opAnd(), attrType, helper.opEq(), date));
		selectObject.setCondition(conditionGroup);
		// 上限なし
		SearchLimitCountCondition limitCondition = new SearchLimitCountCondition(-2, true);

		result = objectService.getList(selectObject, limitCondition);
		return result;

	}

	/**
	 * 事前通知蓄積メール作成処理。<br>
	 * 事前通知メールを作成しDBに蓄積します。
	 *
	 * @param objList 通知対象ドキュメントリスト
	 * @throws Exception
	 */
	private static void createAccumulateMail(List<ObjectDomain> objList) throws Exception {

		MailTypeDomain mailTypeDomain = null;
		MailDomain mail = null;
		UserDomain destinationUser = null;
		String path = "";

		for (ObjectDomain obj: objList) {
			try {
				// 論理削除可能であるかチェック
				if (!checkDeleteEnable(obj)) {
					// 削除不可の場合、メールを作成しない
					continue;
				}

				// 宛先取得
				destinationUser = getDestinationUser(obj);
				if (destinationUser == null) {
					continue;
				}

				// メール用オブジェクトの更新者に宛先ユーザを設定
				obj.setModificationUser(destinationUser);

				mailTypeDomain = new MailTypeDomain();
				mailTypeDomain.setId(AppConstant.MAIL_TYPE_ID_EXPIRATION_DATE);

				mail = new MailDomain();
				mail.setObject(obj);
				mail.setType(mailTypeDomain);

				// 一括送信用にメール蓄積
				mailService.accumulate(mail);

				// パス
				path = obj.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PASS")).getString();

				log.info("事前通知。ID:" + obj.getId() + " " + path + obj.getName() + " 宛先:" + destinationUser.getMail());

			} catch(Exception e) {
				log.error("事前通知失敗。ID:"+ obj.getId() + " " + path + obj.getName() + " 宛先:" + destinationUser.getMail(), e);
			}
		}
	}

	/**
	 * 削除チェック。<br>
	 * オブジェクトがタグを割り当てられている、削除不可ステータス、改訂中、既にごみ箱配下のいずれかの場合falseを返却します。
	 *
	 * @param obj ドキュメント
	 * @return チェック結果
	 * @throws Exception
	 */
	@SuppressWarnings("unchecked")
	private static boolean checkDeleteEnable(ObjectDomain obj) throws Exception {

		// 「ドキュメント」リレーションタイプ
		RelationTypeDomain relationTypeDomain = AppDocumentLogicUtil.getDocumentRelType();

		// タグチェック
		if (AppDocumentLogicUtil.isTagAssignedObject(obj, relationTypeDomain, false)) {
			return false;
		}

		List<RelationDomain> relList = AppDocumentUtil.getParentRelationByRelTypeAndChild(relationTypeDomain, obj,
				new AccessRoleTypeDomain("READ"));
		if (relList == null || relList.size() == 0) {
			return false;
		}
		ObjectDomain parentObj = relList.get(0).getParent();
		try {
			// ステータスチェック
			AttributeUtil.checkStatus(obj, parentObj);
		} catch(Exception e) {
			return false;
		}

		// 改訂中チェック
		Map<Long, ObjectDomain> revObjectMap = VersionUtils.getVersion(obj);
		List<ObjectDomain> allRevDocs = Arrays.asList(revObjectMap.values().toArray(new ObjectDomain[0]));
		for (ObjectDomain revDoc: allRevDocs) {
			if (revDoc.getLockUser() != null) {
				return false;
			}
		}

		// パスチェック
		String path = obj.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PASS")).getString();
		// 既にごみ箱配下のドキュメントはfalse
		if (path.startsWith(PATH_RECYCLE_BOX)) {
			return false;
		}

		return true;

	}

	/**
	 * 宛先ユーザ取得処理。<br>
	 *
	 * @param obj 対象オブジェクト
	 * @return 宛先ユーザリスト
	 * @throws Exception
	 */
	private static UserDomain getDestinationUser(ObjectDomain obj) throws Exception {

		// 更新者
		UserDomain modifyUser = obj.getModificationUser();
		if (checkAuth(modifyUser, obj.getId())) {
			modifyUser = userService.getById(modifyUser.getId());
			return modifyUser;
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
			return createUser;
		}

		return null;

	}

	/**
	 * 宛先チェック処理。 <br>
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
	 * 蓄積メール送信処理。 <br>
	 * DBに蓄積した事前通知メールをユーザごとに一括送信します。
	 *
	 * @throws Exception
	 */
	private static void sendAccumulateMail() throws Exception {

		AccumulationMailCriteria criteria = new AccumulationMailCriteria();
		criteria.setType("ExpirationDateNotice");
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
			 * 送信失敗メールのログ出力
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
	 * 本バッチで使用する設定値のチェックを行います。
	 *
	 * @throws Exception
	 */
	private static void checkSettingValue() throws Exception {

		// 事前通知日数取得
		String advanceNoticeDaysSetting = ConfigUtils.getByKey("EXPIRY_NOTICE_DAYS");
		if (advanceNoticeDaysSetting.equals("")) {
			throw new Exception("設定値「EXPIRY_NOTICE_DAYS」が空文字です。");
		}
		expiryNoticeDays = advanceNoticeDaysSetting;

		// 事前通知メール1件に記載するドキュメント上限
		String noticeDocMaxSetting = ConfigUtils.getByKey("EXPIRY_NOTICE_DOC_MAX");
		if (noticeDocMaxSetting.equals("")) {
			throw new Exception("設定値「EXPIRY_NOTICE_DOC_MAX」が空文字です。");
		}
		maxDocCount = noticeDocMaxSetting;
	}

	/**
	 * 初期データの取得を行います
	 * <br>
	 * @throws Exception
	 */
	private static void init() throws Exception {
		objectService =(ObjectService)ApplicationContextLoader.getApplicationContext().getBean("objectService2");
		userService = (UserService)ApplicationContextLoader.getApplicationContext().getBean("userService2");
		attributeTypeService = (AttributeTypeService)ApplicationContextLoader.getApplicationContext().getBean("attributeTypeService2");
		mailService = (MailService)ApplicationContextLoader.getApplicationContext().getBean("mailService2");
		accumulationMailService = (AccumulationMailService)ApplicationContextLoader.getApplicationContext().getBean("accumulationMailService2");
	}
}
