package batch.maintenance;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.sql.DataSource;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.context.ApplicationContext;

import common.util.AppConstant;
import common.util.AppMessageUtils;
import common.util.AppObjectConditionHelper;
import common.util.AppObjectUtil;
import eim.bo.EIMAttribute;
import eim.bo.EIMException;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMStatus;
import eim.bo.EIMStatusType;
import eim.bo.EIMUser;
import eim.bo.EIMWorkFlow;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.ObjectUtils;
import eim.util.UserUtils;
import eim.util.WorkFlowUtils;
import jp.co.ctc_g.eim.app.document.common.util.AppWorkFlowUtil;
import jp.co.ctc_g.eim.framework.business.domain.BaseEventTypeDomain;
import jp.co.ctc_g.eim.framework.business.domain.EventExecDomain;
import jp.co.ctc_g.eim.framework.business.domain.NoticeMailDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.StatusTypeDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.WorkFlowDomain;
import jp.co.ctc_g.eim.framework.business.service.EventExecService;
import jp.co.ctc_g.eim.framework.business.service.WorkFlowDefService;
import jp.co.ctc_g.eim.framework.common.util.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework2.common.context.TransactionContext;
import jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum;

/**
*
* 一括公開バッチ
*
*/
public class PublishDocumentBatch {

	/** ログ */
	private static Log log = LogFactory.getLog(PublishDocumentBatch.class);

	/** アプリケーションコンテキスト */
	private static ApplicationContext context;

	/** セッション */
	private static EIMSession sess;

	/** 承認ユーザ */
	private static EIMUser approveUser;

	/** 更新対象フォルダ */
	private static List<EIMObject> targetFolderList;

	/** 「フォルダ」オブジェクトタイプ*/
	private static EIMObjectType folderType;

	/** 条件判定ヘルパー */
	private static AppObjectConditionHelper helper;

	/** イベント実行フラグ */
	private static boolean doEventFlag = false;
	/**
	 * 属性一括更新バッチメイン処理
	 * @param args なし
	 */
	public static void main(String[] args) {
		try {
			// 開始ログ
			log.info("-------------公開処理バッチ開始-------------");
			long start = System.currentTimeMillis();
			//コマンドライン引数に2つ変数が入っていない場合は設定が間違っている
			if(args.length != 2){
				log.error("承認ユーザもしくは更新対象フォルダの設定が出来ていません。PublishDocumentBatch.sh内の設定を確認してください。");
				return;
			}
			// 初期処理(args[0] = UPDATE_ATTR_FOLDER_ID,args[1] = ALL_APPROVE_USER_CODE)
			init(args[0],args[1]);
			long end = System.currentTimeMillis();
			//log.info("   【初期処理】"+(end - start)+"ms");
			// 承認ユーザチェック
			if(approveUser == null){
				log.error(" 承認ユーザが取得できません。PublishDocumentBatch.sh内のALL_APPROVE_USER_CODEを確認してください。");
				return ;
			}
			for(EIMObject folder : targetFolderList){
				log.info("[更新対象フォルダ]"+folder.getId()+" : "+folder.getName());
			}
			log.info( "[承認ユーザ]"+approveUser.getCode()+" : "+approveUser.getName());

			// 公開処理開始
			for(EIMObject targetFolder : targetFolderList){
				log.info("フォルダ"+targetFolder.getName()+"に対する処理開始。");

				// 指定したフォルダ配下のドキュメント全て取得する
				List<EIMObject> objectList = AppObjectUtil.getChildEIMObjectRecurrently(sess, targetFolder, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));
				long end1 = System.currentTimeMillis();
				//log.info("   【対象データ一覧取得】"+(end1 - end)+"ms");
				int index = 0;
				int doIndex = 0;
				// 指定したフォルダ配下のドキュメントの属性を更新する
				log.info( "[更新対象ドキュメント]");
				for (EIMObject obj :objectList){
					doEventFlag = false;
					if(helper.isTypeOfDocument(obj.getType())){
						log.info(" "+obj.getId()+","+obj.getName());
						// 上位がWF付きフォルダかチェック
						long higherFolderId = AppObjectUtil.getIntAttr(sess, obj, EIMConfig.get("ATTR_NAME_DOCUMENT_HIGHER_WF_FOLDER"), -1);
						if (higherFolderId != -1){
							log.warn("     --対象のドキュメントの上位にワークフロー付きフォルダが存在します。");
						}else{
							// 要承認/承認不要かチェック
							boolean isUnnecessary = AppWorkFlowUtil.isUnnecessaryWF(sess, obj);
							if(isUnnecessary){
								// 承認不要
								doPublishUnNecessary(obj);
							}else{
								// 要承認の場合
								doPublish(obj);
							}
						}
						index++;
						if(doEventFlag){
							doIndex++;
						}
					}
				}
				sess.commit();
				log.info("フォルダ"+targetFolder.getName()+"に対する処理終了。");
				log.info( "[公開処理 "+doIndex + "件 / 処理対象" + index  + "件]");
			}
		} catch(EIMException eime) {
			log.error(AppMessageUtils.makeLogMessage(1,eime.getMessage()), eime);
			try{
				// ロールバック
				if(sess != null){
					sess.rollback();
				}

			} catch (Exception se) {
				log.error(AppMessageUtils.makeLogMessage(1,se.getMessage()), se);

			}

		} catch (Exception e) {
			log.error(AppMessageUtils.makeLogMessage(1,e.getMessage()), e);
			try{
				// ロールバック
				if(sess != null){
					sess.rollback();
				}

			} catch (Exception se) {
				log.error(AppMessageUtils.makeLogMessage(1,se.getMessage()), se);

			}

		} finally {
			try{
				log.info("------------公開処理バッチ終了-------------");
				// セッションクローズ
				EIMThreadContext.removeEIMSession();
				if(sess != null){
					sess.close();
				}

			} catch (Exception se) {
				log.error(AppMessageUtils.makeLogMessage(1,se.getMessage()), se);

			}
		}

	}

	/**
	 * 初期処理
	 * @throws Exception 例外
	 */
	private static void init(String UPDATE_ATTR_FOLDER_ID,String ALL_APPROVE_USER_CODE) throws Exception{

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
		context = ApplicationContextLoader.getContext();
		DataSource ds = (DataSource)context.getBean("dataSource");

		sess = new EIMSession(user,lang);
		sess.setConnection(ds.getConnection());
		sess.setConsoleMode(true);
		sess.getDBConnection().setAutoCommit(false);
		EIMThreadContext.putEIMSession(sess);

		// 条件判定ヘルパー
		helper = new AppObjectConditionHelper(sess);

		// Transaction context
		TransactionContext tran = new TransactionContext(ConnectionModeEnum.CONSOLE, new UserDomain(user.getId()), lang);
		tran.setDBConnection(sess.getDBConnection());
		EIMThreadContext.putTransactionContext(tran);

		// 承認ユーザ取得
		String approveUserCode = ALL_APPROVE_USER_CODE;
		if(approveUserCode != null){
			approveUser = UserUtils.getUserByCode(sess, approveUserCode);
		}

		// 属性更新対象フォルダID設定
		targetFolderList = new ArrayList<EIMObject>();
		if(UPDATE_ATTR_FOLDER_ID != null){
			String targetFolderIds = UPDATE_ATTR_FOLDER_ID;
			if(targetFolderIds != null){
				String[] ids = targetFolderIds.split(",");
				for(String id : ids){
					EIMObject targetFolder = ObjectUtils.getObjectById(sess, Integer.valueOf(id));
					if(targetFolder == null){
						log.error(" 属性更新対象フォルダが取得できません。PublishDocumentBatch.sh内のUPDATE_ATTR_FOLDER_IDを確認してください。"+id);
						throw new Exception();
					}
					if(!helper.isTypeOfFolder(targetFolder.getType()) && !helper.isTypeOfWorkspace(targetFolder.getType())){
						log.error(" 属性更新対象フォルダがフォルダ、ワークスペースタイプではありません。PublishDocumentBatch.sh内のUPDATE_ATTR_FOLDER_IDを確認してください。");
						throw new Exception();
					}
					targetFolderList.add(targetFolder);
				}
			}
		}else{
			log.error(" 属性更新対象フォルダが取得できません。PublishDocumentBatch.sh内のUPDATE_ATTR_FOLDER_IDを確認してください。");
			throw new Exception();
		}

	}

	/**
	 * 公開処理(要承認)
	 * @param document
	 * @param attrMap
	 * @throws Exception
	 */
	private static void doPublish(EIMObject object)throws Exception{

		EIMStatus currentStatus = object.getStatus();
		if(currentStatus == null){
			log.warn("     --対象のドキュメントはワークフロー付きドキュメントではありません。");
			return;
		}
		// カレントステータスが編集中以外の場合何もしない
		if(currentStatus.getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_EDITTING){
			log.warn("     --対象のドキュメントは[編集中]ではありません。");
			return;
		}
		// イベント実行前にセッションユーザを承認者ユーザに変更しておく
		sess.setUser(approveUser);
		EIMThreadContext.putEIMSession(sess);
		doEventFlag = true;
		EIMWorkFlow workflow = WorkFlowUtils.getWorkFlowByStatusType(sess, currentStatus.getType());
		WorkFlowDefService workFlowDefService=(WorkFlowDefService)context.getBean("workFlowDefService");
		WorkFlowDomain wfDomain = workFlowDefService.getDefById(workflow.getId());
		List<StatusTypeDomain> statusTypeList = wfDomain.getStatusTypeList();

		// ステータス遷移する
		for(int i = 0; i < statusTypeList.size()-1; i++){
			long start = System.currentTimeMillis();
			StatusTypeDomain nextStatustype = statusTypeList.get(i+1);
			if(nextStatustype.getStatusTypeKind().getId() == AppConstant.STATUS_TYPE_KIND_ID_PROCESSING_PUBLIC){
				// 公開処理中の場合
				continue;
			}
			// for文内でステータスが遷移している為、再度カレントステータスを取得する
			currentStatus = object.getStatus();
			BaseEventTypeDomain baseEventTypeDomain = new BaseEventTypeDomain();
			if(currentStatus.getType().getKind() == AppConstant.STATUS_TYPE_KIND_ID_EDITTING){
				// 現在のステータスが編集中の場合、ベースイベントタイプは「承認依頼」となる
				baseEventTypeDomain.setId(AppConstant.BASE_EVENT_TYPE_ID_REQ_APPROVE);
			}else{
				// それ以外は承認とする
				baseEventTypeDomain.setId(AppConstant.BASE_EVENT_TYPE_ID_APPROVAL);
			}
			EventExecService eventExecService = (EventExecService)ApplicationContextLoader.getContext().getBean("eventExecService");
			EventExecDomain eventExecDomain = new EventExecDomain();
			eventExecDomain.setObject(new ObjectDomain(object));
			eventExecDomain.setBaseEventType(baseEventTypeDomain);
			Map<String,Object> paramMap = new HashMap<String,Object>();
			paramMap.put("forcastStatusTypeId", String.valueOf(nextStatustype.getId()));
			int mailType = 0;
			// 公開処理中の1つ手前の場合
			if(currentStatus.getType().getStep() == statusTypeList.size() - 2){
				paramMap.put("approverId", "");//承認者なし
				// 承認通知メールタイプは即時の為飛んでしまう。
			}else{
				// approverId = {ステータスタイプID}:{エントリタイプ}:{承認者ユーザID}
				paramMap.put("approverId", String.valueOf(nextStatustype.getId()) + ":1:" + String.valueOf(approveUser.getId()));//承認者
				// メール通知タイプ設定(送信なし)
				NoticeMailDomain noticeMailDomain = new NoticeMailDomain();
				noticeMailDomain.getMailType().setId(AppConstant.MAIL_TYPE_ID_REQUEST_APPROVE);
				eventExecDomain.getNothingMailList().add(noticeMailDomain);
			}
			paramMap.put("reply", "0");//受信確認なし
			paramMap.put("timing", "3");//通知タイミング3:なし
			paramMap.put("sendNotifyMailTiming", "3");//公開通知タイミング3:なし
			paramMap.put("localPDFOutputSet", "false");//PDF署名なし
			paramMap.put("insertPlaceX", "");//PDF署名情報(不要だが空文字を入れないとnullpointerExceptionになる)
			paramMap.put("insertPlaceY", "");//PDF署名情報(不要だが空文字を入れないとnullpointerExceptionになる)
			paramMap.put("insertPage", "");//PDF署名情報(不要だが空文字を入れないとnullpointerExceptionになる)
			paramMap.put("insertPlace", "");//PDF署名情報(不要だが空文字を入れないとnullpointerExceptionになる)
			paramMap.put("insertApproveUser", "");//PDF署名情報(不要だが空文字を入れないとnullpointerExceptionになる)

			//オブジェクトIDからWF情報を取得する
			long objId = object.getId();
			// ワークフロー公開処理オブジェクト取得
			long processingPublic = -1;
			for(int t = 0; t < workflow.getStatusTypeList().size(); t++){
				EIMStatusType sttype = (EIMStatusType)workflow.getStatusTypeList().get(t);
				if(sttype.getKind() == AppConstant.STATUS_TYPE_KIND_ID_PROCESSING_PUBLIC){
					processingPublic = sttype.getId();
					break;
				}
			}
			EIMObject wfPublicObj = AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_WFPUB"), String.valueOf(processingPublic));

			// 属性「URL挿入実施フラグ」の取得
			boolean attFlag = false;
			if(wfPublicObj != null){
				EIMAttribute attFlagAttr = wfPublicObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_INSERT_URL_FLG"));
				if(attFlagAttr != null){
					long attFlagInt = (long)attFlagAttr.getInt();
					if(attFlagInt == 1){
						attFlag = true;
					}
				}
			}
			paramMap.put("doInsertURLPDF",attFlag);

			eventExecDomain.setParamMap(paramMap);

			long end1 = System.currentTimeMillis();
			//log.info("   【イベント遷移準備】"+(end1 - start)+"ms");
			//イベント実行
			eventExecService.doEvent(eventExecDomain);
			long end2 = System.currentTimeMillis();
			//log.info("   【イベント遷移実行】"+(end2 - end1)+"ms");
			// オブジェクトを再取得する
			object = ObjectUtils.getObjectById(sess, object.getId());
			long end3 = System.currentTimeMillis();
			//log.info("   【イベント遷移後処理】"+(end3 - end2)+"ms");
			log.info("  --ステータス変更完了"+nextStatustype.getDefName());
		}
		log.info("  --公開処理完了");
	}

	/**
	 * 公開処理(承認不要)
	 * @param document
	 * @param attrMap
	 * @throws Exception
	 */
	private static void doPublishUnNecessary(EIMObject object)throws Exception{

		EIMStatus currentStatus = object.getStatus();
		// カレントステータスが公開処理中、公開済ステータスの場合なにもしない
		if(currentStatus.getType().getKind() == AppConstant.STATUS_TYPE_KIND_ID_PUBLIC
				|| currentStatus.getType().getKind() == AppConstant.STATUS_TYPE_KIND_ID_PROCESSING_PUBLIC){
			log.warn("     --対象のドキュメントは公開済もしくは公開処理中です。");
			return;
		}
		doEventFlag = true;
		EIMWorkFlow workflow = WorkFlowUtils.getWorkFlowByStatusType(sess, currentStatus.getType());
		WorkFlowDefService workFlowDefService=(WorkFlowDefService)context.getBean("workFlowDefService");
		WorkFlowDomain wfDomain = workFlowDefService.getDefById(workflow.getId());
		List<StatusTypeDomain> statusTypeList = wfDomain.getStatusTypeList();
		// 遷移先は公開済ステータスタイプ
		StatusTypeDomain nextStatustype = statusTypeList.get(statusTypeList.size() -1);

		// イベント実行前にセッションユーザを承認者ユーザに変更しておく
		sess.setUser(approveUser);
		EIMThreadContext.putEIMSession(sess);
		BaseEventTypeDomain baseEventTypeDomain = new BaseEventTypeDomain();
		baseEventTypeDomain.setId(AppConstant.BASE_EVENT_TYPE_ID_APPROVAL);

		EventExecService eventExecService = (EventExecService)ApplicationContextLoader.getContext().getBean("eventExecService");
		EventExecDomain eventExecDomain = new EventExecDomain();
		eventExecDomain.setObject(new ObjectDomain(object));
		eventExecDomain.setBaseEventType(baseEventTypeDomain);
		Map<String,Object> paramMap = new HashMap<String,Object>();
		paramMap.put("forcastStatusTypeId", String.valueOf(nextStatustype.getId()));
		//paramMap.put("approverId", "1:" + String.valueOf(approveUser.getId()));//1:<ユーザID>
		paramMap.put("approverId", "");
		paramMap.put(AppConstant.PARAM_KEY_IMMEDIATE_PUBLIC_NAME,"true");//承認不要
		paramMap.put("reply", "0");//受信確認なし
		paramMap.put("timing", "3");//通知タイミング3:なし
		paramMap.put("sendNotifyMailTiming", "3");//公開通知タイミング3:なし
		paramMap.put("localPDFOutputSet", "false");//PDF署名なし
		paramMap.put("insertPlaceX", "");//PDF署名情報(不要だが空文字を入れないとnullpointerExceptionになる)
		paramMap.put("insertPlaceY", "");//PDF署名情報(不要だが空文字を入れないとnullpointerExceptionになる)
		paramMap.put("insertPage", "");//PDF署名情報(不要だが空文字を入れないとnullpointerExceptionになる)
		paramMap.put("insertPlace", "");//PDF署名情報(不要だが空文字を入れないとnullpointerExceptionになる)
		paramMap.put("insertApproveUser", "");//PDF署名情報(不要だが空文字を入れないとnullpointerExceptionになる)

		//オブジェクトIDからWF情報を取得する
		long objId = object.getId();
		// ワークフロー公開処理オブジェクト取得
		long processingPublic = -1;
		for(int t = 0; t < workflow.getStatusTypeList().size(); t++){
			EIMStatusType sttype = (EIMStatusType)workflow.getStatusTypeList().get(t);
			if(sttype.getKind() == AppConstant.STATUS_TYPE_KIND_ID_PROCESSING_PUBLIC){
				processingPublic = (long)sttype.getId();
				break;
			}
		}
		EIMObject wfPublicObj = AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_WFPUB"), String.valueOf(processingPublic));

		// 属性「URL挿入実施フラグ」の取得
		boolean attFlag = false;
		if(wfPublicObj != null){
			EIMAttribute attFlagAttr = wfPublicObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_INSERT_URL_FLG"));
			if(attFlagAttr != null){
				long attFlagInt = (long)attFlagAttr.getInt();
				if(attFlagInt == 1){
					attFlag = true;
				}
			}
		}
		paramMap.put("doInsertURLPDF",attFlag);

		eventExecDomain.setParamMap(paramMap);
		//イベント実行
		eventExecService.doEvent(eventExecDomain);
		log.info("  --公開処理完了");
	}
}
