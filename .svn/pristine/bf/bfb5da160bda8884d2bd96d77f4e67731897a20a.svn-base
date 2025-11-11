package jp.co.ctc_g.eim.app.document.business.service.impl;

import common.util.AppConstant;
import common.util.AppUpdateNoticeUtils;
import eim.bo.EIMObject;
import eim.bo.EIMVersion;
import eim.net.EIMSession;
import eim.util.VersionUtils;
import jp.co.ctc_g.eim.framework.business.domain.BaseEventTypeExecDomain;
import jp.co.ctc_g.eim.framework.business.domain.StatusTypeKindDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework.business.service.impl.BaseEventTypePlugInImpl;
import jp.co.ctc_g.eim.framework.common.exception.EIMAppException;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * ベースイベントタイプ「公開」
 *
 */
public class BaseEvtPublicPlugInImpl extends BaseEventTypePlugInImpl {
	
	
	/**
	 * 実行権限を判定します。 
	 * 
	 * @see jp.co.ctc_g.eim.framework.business.service.impl.BaseEventTypePlugInImpl#enabled(jp.co.ctc_g.eim.framework.business.domain.BaseEventTypeExecDomain)
	 */
	public boolean enabled(BaseEventTypeExecDomain baseEventTypeExecDomain) throws Exception  {
		
		EIMSession sess = EIMThreadContext.getEIMSession();
		ObjectDomain objDomain = baseEventTypeExecDomain.getObject();
		
		//ステータスチェック
		StatusTypeKindDomain stTypeKindDomain = baseEventTypeExecDomain.getObject().getStatus().getStatusType().getStatusTypeKind();
		if (stTypeKindDomain.getId() != AppConstant.STATUS_TYPE_KIND_ID_PROCESSING_PUBLIC) {
			
			throw new EIMAppException(sess, "EIM.ERROR.LOGIC.NOTPROCESSINGPUBLIC", new Object[]{objDomain.getName()});
		}
		
		//ロック状態チェック
		if (objDomain.getLUser() != null || objDomain.getLDate() != null) {
			throw new EIMAppException(sess, "EIM.ERROR.LOGIC.OBJECT.LOCKED", new Object[]{objDomain.getName()});
		}
		
		return true;
	}
	
	/**
	 * ベースイベントタイプアクションを実行します。 
	 * 
	 * @see jp.co.ctc_g.eim.framework.business.service.impl.BaseEventTypePlugInImpl#doAction(jp.co.ctc_g.eim.framework.business.domain.BaseEventTypeExecDomain)
	 */
	public void doAction(BaseEventTypeExecDomain baseEventTypeExecDomain) throws Exception {
		
		EIMSession sess = EIMThreadContext.getEIMSession();
		EIMObject object = baseEventTypeExecDomain.getObject().createEIMObject();
		
		// チェックアウトされたオブジェクト
		if(object.getRev() > 0)
		{
			// 最新履歴を設定
			object = VersionUtils.setLatest(sess, object);
			
			// 前のバージョンのオブジェクトを取得
			EIMVersion version = VersionUtils.getVersion(sess, object);
			EIMObject latestObj = version.getObjectByRev(object.getRev() - 1);
			
			// ### SEARCH FRAMEWORK 検索FW更新通知 前リビジョンのオブジェクト
			AppUpdateNoticeUtils.updateNoticeInsert(latestObj.getId(), "SEARCHFW_BASEEVT_PUBLIC_OLD_DOCUMENT");
		}
		
		// SearchFramework 検索FW更新通知 対象：WF付きフォルダ、ドキュメントと配下のフォルダ、ドキュメント
		AppUpdateNoticeUtils.updateNoticeInsertWFObject(sess, object,
				"SEARCHFW_BASEEVT_PUBLIC_DOCUMENT", "SEARCHFW_BASEEVT_PUBLIC_FOLDER", 
				"SEARCHFW_BASEEVT_PUBLIC_CHILD_DOCUMENT", "SEARCHFW_BASEEVT_PUBLIC_CHILD_FOLDER");
	}
}
