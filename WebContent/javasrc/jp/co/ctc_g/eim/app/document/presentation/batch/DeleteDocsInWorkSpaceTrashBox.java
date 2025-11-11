package jp.co.ctc_g.eim.app.document.presentation.batch;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.sql.DataSource;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import common.util.AppMessageUtils;
import common.util.AppObjectTypeUtil;
import eim.bo.EIMException;
import eim.bo.EIMUser;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import jp.co.ctc_g.eim.app.document.business.domain.DocumentDomain;
import jp.co.ctc_g.eim.app.document.business.domain.FolderDomain;
import jp.co.ctc_g.eim.app.document.business.service.DocumentService;
import jp.co.ctc_g.eim.app.document.business.service.FolderService;
import jp.co.ctc_g.eim.app.document.common.util.AppDocumentLogicUtil;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchConditionCompare;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchConditionGroup;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchConditionIn;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchLimitCountCondition;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchOperatorEnum;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchSelectObject;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchSelectObject.SearchConditionBuildHelper;
import jp.co.ctc_g.eim.framework2.business.service.AttributeTypeService;
import jp.co.ctc_g.eim.framework2.business.service.ObjectService;
import jp.co.ctc_g.eim.framework2.business.service.ObjectTypeService;
import jp.co.ctc_g.eim.framework2.business.service.UserService;
import jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework2.common.context.TransactionContext;
import jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;

/**
 * ワークスペース固有ごみ箱内オブジェクト削除処理。
 *
 */
public class DeleteDocsInWorkSpaceTrashBox {

	/** ログ */
	private static Log log = LogFactory.getLog(DeleteDocsInWorkSpaceTrashBox.class);
	/** TransactionContext */
	private static TransactionContext context = null;
	/** Session */
	private static EIMSession sess = null;
	/** コンテキストファイル */
	private static final String[] contextFiles = {"applicationContext.xml"};
	/** ユーザサービス */
	private static UserService userService;
	/** オブジェクトサービス */
	private static ObjectService objectService;
	/** オブジェクトタイプサービス */
	private static ObjectTypeService objectTypeService;
	/** 属性タイプサービス */
	private static AttributeTypeService attributeTypeService;
	/** ドキュメントサービス */
	private static DocumentService documentService;
	/** フォルダサービス */
	private static FolderService folderService;

	/** ワークスペース固有ごみ箱保管日数*/
	private static String storageDays = "";

	/** 属性タイプ:削除日時*/
	private static AttributeTypeDomain attrTypeDeleteDate = null;
	/** 属性タイプ:パス*/
	private static AttributeTypeDomain attrTypePath = null;

	/** デバッグモード */
	private static boolean isDebug = false;

	/** ごみ箱パス */
	private static final String PATH = "ごみ箱/";

	/** 削除条件:更新者 */
	private static UserDomain muser = null;

	/**
	 * ワークスペース固有ごみ箱内オブジェクト論理削除メインメソッド。<br>
	 * オブジェクトが保持する削除日時と設定ファイルの保管日数をもとに、ワークスペース固有ごみ箱配下のオブジェクトを論理削除します。
	 * @param args 引数に1が渡された場合はデバッグモードで実行
	 * @throws Exception
	 * @since Ver6.91
	 */
	@SuppressWarnings({ "unchecked", "rawtypes" })
	public static void main(String[] args) throws Exception {

		// 開始ログ
		log.info("ワークスペース固有ごみ箱内オブジェクト削除開始");

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
			checkSettingValue(args);

			// 削除日時が-1の場合は削除日時での判定を行わない
			Date deleteDate =  null;
			if(!storageDays.equals("-1")) {
				// 削除対象日時を計算(現在日時-ごみ箱保管日数)
				SimpleDateFormat sdf = new SimpleDateFormat("yyyy/MM/dd");
				Calendar calendar = Calendar.getInstance();
		        calendar.add(Calendar.DATE, -Integer.parseInt(storageDays));
				deleteDate =  sdf.parse(sdf.format(calendar.getTime()));
			}


			// フォルダ、およびフォルダの子のオブジェクトタイプを取得
			Map objTypeMap = AppObjectTypeUtil.getObjTypeMap(sess, ConfigUtils.getByKey("OBJECT_TYPE_NAME_FOLDER"));
			Iterator<Long> objTypeIds = objTypeMap.keySet().iterator();
			List<ObjectDomain> deleteFolders = new ArrayList<>();
			List<Long> typeIds = new ArrayList<>();
			while(objTypeIds.hasNext()) {
				// 各タイプのID取得
				long objTypeId = (Long)objTypeIds.next();
				typeIds.add(objTypeId);
			}
			// 削除対象フォルダ取得
			deleteFolders =  getDeleteObjList(deleteDate, typeIds);

			// ドキュメント、およびドキュメントの子のオブジェクトタイプを取得
			objTypeMap = AppObjectTypeUtil.getObjTypeMap(sess, ConfigUtils.getByKey("OBJECT_TYPE_NAME_DOCUMENT"));
			objTypeIds = objTypeMap.keySet().iterator();
			List<ObjectDomain> deleteDocs = new ArrayList<>();
			typeIds = new ArrayList<>();
			while(objTypeIds.hasNext()) {
				// 各タイプのID取得
				long objTypeId = (Long)objTypeIds.next();
				typeIds.add(objTypeId);
			}
			// 削除対象ドキュメント取得
			deleteDocs =  getDeleteObjList(deleteDate, typeIds);

			if (deleteFolders.size() < 1 && deleteDocs.size() < 1) {
				log.info("削除対象オブジェクトが存在しません。");
				return;
			}

			// フォルダ削除
			deleteObjectInWSRecycleBox(deleteFolders);

			// ドキュメント削除
			deleteObjectInWSRecycleBox(deleteDocs);

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
			log.info("ワークスペース固有ごみ箱内オブジェクト削除終了");
		}
		System.exit(0);
	}

	/**
	 * 削除対象オブジェクト取得処理。<br>
	 * 最新で、削除日時が指定日より過去日、パスがワークスペース固有ごみ箱直下である指定オブジェクトタイプのオブジェクトを取得します。
	 *
	 * @param date 削除対象日時
	 * @param typeIds オブジェクトタイプIDリスト
	 * @return オブジェクトリスト
	 * @throws Exception
	 */
	private static List<ObjectDomain> getDeleteObjList(Date date, List<Long> typeIds) throws Exception {

		// ワークスペース固有ごみ箱オブジェクトのリストを取得
		List<ObjectDomain> wsRecycleObjList = new ArrayList<>();
		SearchSelectObject wsSelectObject = new SearchSelectObject();
		SearchConditionBuildHelper helper = new SearchConditionBuildHelper();
		SearchConditionGroup wsConditionGroup = helper.group(helper.opAnd());

		// オブジェクトタイプがワークスペース固有ごみ箱
		ObjectTypeDomain wsRecycleObjType = objectTypeService.getByDefinitionName(EIMConfig.get("OBJECT_TYPE_NAME_WORKSPACERECYCLE"));

		// オブジェクトタイプを条件に設定
		wsConditionGroup.addCondition(new SearchConditionCompare(helper.opAnd(), SearchSelectObject.PsedoAttributeTypeEnum.TYPE, helper.opEq(), wsRecycleObjType.getId()));
		wsSelectObject.setCondition(wsConditionGroup);

		// 上限なし
		SearchLimitCountCondition wsLimitCondition = new SearchLimitCountCondition(-2, true);

		wsRecycleObjList = objectService.getList(wsSelectObject, wsLimitCondition);

		List<ObjectDomain> result = new ArrayList<>();

		SearchSelectObject selectObject = new SearchSelectObject();
		// オブジェクトタイプを条件に設定
		SearchConditionGroup conditionGroup = helper.group(helper.opAnd())
				.addCondition(helper.inArray(helper.opAnd(), SearchSelectObject.PsedoAttributeTypeEnum.TYPE, helper.opIn(), typeIds.toArray()));

		// 最新フラグを条件に設定
		conditionGroup.addCondition(new SearchConditionCompare(helper.opAnd(), SearchSelectObject.PsedoAttributeTypeEnum.LATEST, helper.opEq(), 1));
		// 削除日時を条件に設定
		if(date != null) {
			conditionGroup.addCondition(new SearchConditionCompare(helper.opAnd(), attrTypeDeleteDate, helper.opLt(), date));
		}

		// ワークスペース固有ごみ箱オブジェクトのパスを条件に設定
		String[] wsRecycleObjPaths = new String[wsRecycleObjList.size()];
		for (int i = 0; i <wsRecycleObjList.size(); i++ ){
			wsRecycleObjPaths[i] = wsRecycleObjList.get(i).getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PASS")).getString() + PATH;
		}
		conditionGroup.addCondition(new SearchConditionIn(helper.opAnd(), attrTypePath, SearchOperatorEnum.IN, wsRecycleObjPaths));

		// 更新者
		if(muser != null) {
			conditionGroup.addCondition(new SearchConditionCompare(helper.opAnd(), SearchSelectObject.PsedoAttributeTypeEnum.MUSER, helper.opEq(), muser.getId()));
		}
		selectObject.setCondition(conditionGroup);
		// 上限なし
		SearchLimitCountCondition limitCondition = new SearchLimitCountCondition(-2, true);

		result = objectService.getList(selectObject, limitCondition);
		return result;

	}

	/**
	 * ワークスペース固有ごみ箱内オブジェクト削除処理。<br>
	 *
	 * @param objList 削除対象オブジェクトリスト
	 * @throws Exception
	 */
	private static void deleteObjectInWSRecycleBox(List<ObjectDomain> objList) throws Exception {

		List<FolderDomain> folderDomainList = new ArrayList<>();
		List<DocumentDomain> docDomainList = new ArrayList<>();
		for (ObjectDomain obj: objList) {

			try {
				// フォルダ
				if (AppDocumentLogicUtil.isTypeOfFolder(obj.getType())) {
					folderDomainList = new ArrayList<>();
					FolderDomain folderDomain = new FolderDomain();
					folderDomain.setId(obj.getId());
					folderDomain.setAttributeList(obj.getAttributeList());
					folderDomainList.add(folderDomain);
					// システムのごみ箱へ論理削除
					folderService.dispose(folderDomainList);

				// ドキュメント
				} else if (AppDocumentLogicUtil.isTypeOfDocument(obj.getType())) {
					docDomainList = new ArrayList<>();
					DocumentDomain docDomain = new DocumentDomain();
					docDomain.setId(obj.getId());
					docDomain.setAttributeList(obj.getAttributeList());
					docDomainList.add(docDomain);
					// システムのごみ箱へ論理削除
					documentService.dispose(docDomainList);
				}

				log.info("削除。ID:" + obj.getId() + "	" + obj.getName() + "	" + obj.getCreationUser().getName() + "	" + obj.getModificationUser().getName());
				if( isDebug ) {
					// デバッグモード
					context.getDBConnection().rollback();
				} else {
					// 通常モード
					context.getDBConnection().commit();
				}


			}catch(Exception e) {
				log.error("削除失敗。ID:" + obj.getId() + " " + obj.getName(), e);
				context.getDBConnection().rollback();
			}
		}
	}

	/**
	 * 設定チェック<br>
	 * 本バッチで使用する属性、設定値のチェックを行います。
	 *
	 * @throws Exception
	 */
	private static void checkSettingValue(String[] args) throws Exception {

		// デバッグモードチェック
		if(args != null && args.length > 0 && args[0].equals("1")) {
			log.info("--デバッグモード--");
			isDebug = true;
		}

		// 属性「削除日時」チェック
		if (attrTypeDeleteDate == null) {
			throw new Exception("属性「削除日時」が存在しません。");
		}

		// ワークスペース固有ごみ箱保管日数取得
		String storageDaysSetting = ConfigUtils.getByKey("WORKSPACE_TRASH_BOX_STRAGE_DAYS");
		if (storageDaysSetting.equals("")) {
			throw new Exception("設定値「WORKSPACE_TRASH_BOX_STRAGE_DAYS」が空文字です。");
		}
		if(storageDaysSetting.equals("-1")) {
			log.info("設定値「WORKSPACE_TRASH_BOX_STRAGE_DAYS」に-1が設定されています。削除日に関わらず、ワークスペース固有ごみ箱配下のすべてのドキュメントが対象となります。");
		}
		storageDays = storageDaysSetting;

		// 更新者
		String muserCode = ConfigUtils.getByKey("WORKSPACE_TRASH_BOX_MUSER_CODE");
		if (muserCode != null && !muserCode.equals("")) {
			muser = userService.getByCode(muserCode);
			if (muser == null) {
				throw new EIMException("設定値「WORKSPACE_TRASH_BOX_MUSER_CODE」に指定したユーザは存在しません。[" + muserCode + "]");
			}
			log.info("削除条件：更新者 " + muser.getCode() + " " + muser.getName());
		}
	}

	/**
	 * 初期データの取得を行います
	 * <br>
	 * @throws Exception
	 */
	private static void init() throws Exception {
		objectService =(ObjectService)ApplicationContextLoader.getApplicationContext().getBean("objectService2");
		objectTypeService = (ObjectTypeService)ApplicationContextLoader.getApplicationContext().getBean("objectTypeService2");
		userService = (UserService)ApplicationContextLoader.getApplicationContext().getBean("userService2");
		attributeTypeService = (AttributeTypeService)ApplicationContextLoader.getApplicationContext().getBean("attributeTypeService2");
		documentService = (DocumentService)ApplicationContextLoader.getApplicationContext().getBean("documentService2");
		folderService = (FolderService)ApplicationContextLoader.getApplicationContext().getBean("folderService2");

		attrTypeDeleteDate = attributeTypeService.getByDefinitionName(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_DELETE_DATE"));
		attrTypePath = attributeTypeService.getByDefinitionName(ConfigUtils.getByKey("ATTR_NAME_FOLDER_PASS"));
	}
}
