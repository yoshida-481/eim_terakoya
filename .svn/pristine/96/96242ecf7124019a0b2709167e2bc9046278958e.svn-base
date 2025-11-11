package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.io.File;
import java.util.List;
import java.util.Map;

import common.util.AppConstant;
import common.util.AppObjectConditionHelper;
import common.util.AppObjectUtil;
import common.util.AppSecurityUtils;
import common.util.AppUpdateNoticeUtils;
import eim.bo.EIMAccessRole;
import eim.bo.EIMAccessRoleType;
import eim.bo.EIMAttribute;
import eim.bo.EIMAttributeType;
import eim.bo.EIMEvent;
import eim.bo.EIMException;
import eim.bo.EIMFile;
import eim.bo.EIMFormat;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMSearchConditionGroup;
import eim.bo.EIMSearchConditionIn;
import eim.bo.EIMSearchConditionLike;
import eim.bo.EIMSearchLimitCountCondition;
import eim.bo.EIMSearchSelectEIMObject;
import eim.bo.EIMUser;
import eim.bo.EIMVersion;
import eim.net.EIMSession;
import eim.util.AccessUtils;
import eim.util.AttributeUtils;
import eim.util.EIMConfig;
import eim.util.EIMConstant;
import eim.util.EventAttributeUtils;
import eim.util.FileUtils;
import eim.util.ObjectUtils;
import eim.util.OperationHistoryUtils;
import eim.util.RelationUtils;
import eim.util.SearchUtils;
import eim.util.SecurityUtils;
import eim.util.StringUtils;
import eim.util.VersionUtils;
import jp.co.ctc_g.eim.framework.business.dao.StatusDao;
import jp.co.ctc_g.eim.framework.business.domain.BaseEventTypeExecDomain;
import jp.co.ctc_g.eim.framework.business.domain.StatusTypeKindDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.StatusDomain;
import jp.co.ctc_g.eim.framework.business.service.impl.BaseEventTypePlugInImpl;
import jp.co.ctc_g.eim.framework.common.exception.EIMAppException;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;

/**
 * ベースイベントタイプ「公開取消」
 *
 */
public class BaseEvtPublicCancelPlugInImpl extends BaseEventTypePlugInImpl {

	protected StatusDao statusDao = null;

	/**
	 * 実行権限を判定します。
	 *
	 * @see jp.co.ctc_g.eim.framework.business.service.impl.BaseEventTypePlugInImpl#enabled(jp.co.ctc_g.eim.framework.business.domain.BaseEventTypeExecDomain)
	 */
	public boolean enabled(BaseEventTypeExecDomain baseEventTypeExecDomain) throws Exception  {

		EIMSession sess = EIMThreadContext.getEIMSession();
		ObjectDomain objDomain = baseEventTypeExecDomain.getObject();

		//パラメータを取得
		Map<String, Object> paramMap = baseEventTypeExecDomain.getParamMap();
		String processType = (String)paramMap.get(AppConstant.PARAM_KEY_REFER_TO_APPROVAL);

		//ST遷移予測で呼ばれた場合は無条件でtrue
		if(processType != null && processType.equals(AppConstant.PARAM_VALUE_REFER_TO_APPROVAL)){
			return true;
		}

		// 実行権限チェック
		EIMObject object = null;
		if(objDomain.getSecurity() != null) {

			//ベースイベントタイプに設定されたアクセス権限を取得
			String aclKey = baseEventTypeExecDomain.getBaseEventType().getAclKey();
			EIMAccessRoleType acrType = SecurityUtils.getAccessRoleTypeByName(sess, aclKey);

			object = objDomain.createEIMObject();
			if(SecurityUtils.authorized(sess, object, sess.getUser(), acrType.getId()) != true) {

				return false;
			}
		}

		// ステータスドメインを取得
		StatusDomain statusDomain = statusDao.getById(baseEventTypeExecDomain.getObject().getStatus().getId());

		//ステータスチェック
		StatusTypeKindDomain stTypeKindDomain = statusDomain.getStatusType().getStatusTypeKind();
		if (stTypeKindDomain.getId() != AppConstant.STATUS_TYPE_KIND_ID_PUBLIC) {

			throw new EIMAppException(sess, "EIM.ERROR.LOGIC.NOTPUBLIC", new Object[]{objDomain.getName()});
		}

		// イベント実行ユーザID
		EIMUser user = sess.getUser();
		String publicCancelFlg = ConfigUtils.getByKey("USE_PUBLIC_CANCEL");
		if (publicCancelFlg.equals("0")) {
			Boolean isSystemSecurityAuth =AppSecurityUtils.authorizedBySystemSecurity(sess, EIMAccessRole.UPDATE);
			if(!isSystemSecurityAuth) {
				// システム管理者権限なし
				throw new EIMAppException(sess, "EIM.ERROR.LOGIC.NOPUBLICCANCEL");
			}
		} else if (publicCancelFlg.equals("1")) {
			Boolean isUpdateAuth = SecurityUtils.authorized(sess, object, user, EIMAccessRole.UPDATE);
			if (!isUpdateAuth) {
				// 編集権限なし
				throw new EIMAppException(sess, "EIM.ERROR.LOGIC.NOPUBLICCANCEL");
			}
		} else {
			// 0,1以外が設定されている場合
			throw new EIMAppException(sess, "EIM.ERROR.LOGIC.NOPUBLICCANCEL");
		}

		return true;
	}

	/**
	 * ベースイベントタイプアクションを実行します。
	 *
	 * @see jp.co.ctc_g.eim.framework.business.service.impl.BaseEventTypePlugInImpl#doAction(jp.co.ctc_g.eim.framework.business.domain.BaseEventTypeExecDomain)
	 */
	@SuppressWarnings("unchecked")
	public void doAction(BaseEventTypeExecDomain baseEventTypeExecDomain) throws Exception {

		EIMSession sess = EIMThreadContext.getEIMSession();
		EIMObject object = baseEventTypeExecDomain.getObject().createEIMObject();

		// 排他
		if (object.getLockDate() != null) {
			// 対象のオブジェクトが更新されているためエラーを返す
			throw new EIMException(sess, "EIM.ERROR.LOGIC.EVENT.ALREADY.UPDATED");
		}
		EIMVersion version = VersionUtils.getVersion(sess, object);
		List<EIMObject> objList = version.getList();
		for (int i = 0; i < objList.size(); i++) {
			if (object.getRev() < objList.get(i).getRev()) {
				// 対象のオブジェクトが更新されているためエラーを返す
				throw new EIMException(sess, "EIM.ERROR.LOGIC.EVENT.ALREADY.UPDATED");
			}
		}

		// 公開取消対象が公開され新しい版が作られている場合
		// 改訂されている場合
		if (object.getRev() > 0 ) {
			// 1つ前の版
			EIMObject preLatestObj = (EIMObject)version.getList().get(object.getRev() - 1);
			// ロックをかける
			preLatestObj = ObjectUtils.lock(sess, preLatestObj);
			// latestフラグをたてる
			VersionUtils.setLatest(sess, preLatestObj);
			VersionUtils.setLatestWithNoCheck(sess, object, true);
			// 過去版のリレーションを再作成
			RelationUtils.revisionUp(sess, object, preLatestObj);

		}

		// 公開済みファイル削除
		EIMFormat newObjPubFormat = FileUtils.getFormatByName(sess, EIMConfig.get("FORMAT_NAME_PUBLIC"));
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
		EIMObjectType type = object.getType();

		if (helper.isTypeOfDocument(type)) {
			// 文書の場合
			deleteFile(sess, object, newObjPubFormat);
			// SearchFramework 検索FW更新通知 対象：更新対象ドキュメント
			AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), "SEARCHFW_BASEEVT_PUBLICCANCEL_DOCUMENT");
			if(object.getRev() > 0){
				// リビジョンが0以上の場合 SearchFramework 検索FW更新通知 対象：更新対象ドキュメント(ひとつ前のリビジョン)
				EIMObject preLatestObj = (EIMObject)version.getList().get(object.getRev() - 1);
				AppUpdateNoticeUtils.updateNoticeInsert(preLatestObj.getId(), "SEARCHFW_BASEEVT_PUBLICCANCEL_OLD_DOCUMENT");
			}
		} else if (helper.isTypeOfFolder(type)) {
			// フォルダの場合
			List<EIMObject> list = getPublicDocList(sess, object);
			for (int i =0 ; i < list.size(); i++) {
				deleteFile(sess, list.get(i), newObjPubFormat);
				// SearchFramework 検索FW更新通知 対象：更新対象ドキュメント
				AppUpdateNoticeUtils.updateNoticeInsert(list.get(i).getId(), "SEARCHFW_BASEEVT_PUBLICCANCEL_DOCUMENT");
			}
			// SearchFramework 検索FW更新通知 対象：更新対象フォルダ
			AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), "SEARCHFW_BASEEVT_PUBLICCANCEL_FOLDER");
		}

		// イベント属性「コメント」を設定
		EIMEvent event = baseEventTypeExecDomain.getEvent().createEIMEvent();
		EIMAttributeType attTypeComment = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.getValue("EVENT_ATTR_NAME_COMMENT"));
		EventAttributeUtils.setAttribute(sess, event, attTypeComment, (String)baseEventTypeExecDomain.getParamMap().get("comment"));

		/*--- 共通処理 ---*/
		//アクセス履歴作成
		AccessUtils.createAccess(sess, object, "EIM.ACCESS.TYPE.PUBLICCANCEL");

		// 操作履歴
		OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, AppConstant.PUBLIC_CANCEL,
				EIMConstant.TARGET_APPROVE, EIMConstant.OBJECT, object,
				null, null, null, AppObjectUtil.getPath(object));
	}

	/**
	 * 指定オブジェクトの配下のドキュメントを取得します。（直下だけではなく再帰的に全て）
	 */
	private List<EIMObject> getPublicDocList(EIMSession sess, EIMObject object) throws Exception {

		EIMSearchSelectEIMObject.SearchConditionBuildHelper h = new EIMSearchSelectEIMObject.SearchConditionBuildHelper();
		EIMSearchConditionGroup conds = h.group(h.opAnd());

		// ドキュメントタイプを指定
		EIMObjectType docObjType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT"));
		if(docObjType == null) {
			//ドキュメントタイプが取得できない場合は予期せぬエラーとして処理する
			throw new EIMAppException(sess, "EIM.ERROR.SYSTEMERROR");
		}
		EIMSearchConditionIn objTypeCond = h.eqObjTypeWithSubClasses(h.opAnd(), docObjType.getId(), sess);
		objTypeCond.setHighPriority();
		conds.addCondition(objTypeCond);

		// パスを指定（前方一致）
		EIMAttribute pathAttr = object.getAttribute(EIMConfig.get("ATTR_NAME_FOLDER_PASS"));
		if(pathAttr == null || pathAttr.getStrings() == null || pathAttr.getStrings().length == 0) {
			//パス属性がない場合は予期せぬエラーとして処理する
			throw new EIMAppException(sess, "EIM.ERROR.SYSTEMERROR");
		}
		String searchPath = pathAttr.getStrings()[0] + object.getName();

		EIMAttributeType pathType = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_PASS"));
		EIMSearchConditionLike pathCond = h.like(h.opAnd(), pathType, h.opLike(), StringUtils.escapeUserWildcardChars(searchPath) + "*");
		pathCond.setHighPriority();
		conds.addCondition(pathCond);
		// 検索する
		EIMSearchSelectEIMObject selectTarget = new EIMSearchSelectEIMObject();
		selectTarget.setCondition(conds);
		return SearchUtils.searchObjects(sess, selectTarget, new EIMSearchLimitCountCondition(100000, false));
	}

	/**
	 * 指定オブジェクトのファイル、メタ情報を削除します。
	 */
	private void deleteFile(EIMSession sess, EIMObject object, EIMFormat format) throws Exception {

		EIMFile newObjPubMetaInfo = FileUtils.getFile(sess, object, format);
		FileUtils.deleteFile(sess, object, format);

		File newObjPubFile = new File(newObjPubMetaInfo.getDirectory().getPath() + FileUtils.getFileName(object, newObjPubMetaInfo));
		if (newObjPubFile != null) {
			newObjPubFile.delete();
		}

		// 「公開処理失敗」属性の削除
		AppObjectUtil.deleteAttribute(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_PUB_PROC_FAIL"));
		// 「PDF変換処理実行日時」属性の削除
		AppObjectUtil.deleteAttribute(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_PDF_CONV_EXEC_DATE"));
		// 「公開PDF事前登録日時」属性の削除
		AppObjectUtil.deleteAttribute(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_PDF_PRE_REGIST_DATE"));
	}

	/**
	 * @return the statusDao
	 */
	public StatusDao getStatusDao() {
		return statusDao;
	}

	/**
	 * @param statusDao the statusDao to set
	 */
	public void setStatusDao(StatusDao statusDao) {
		this.statusDao = statusDao;
	}
}
