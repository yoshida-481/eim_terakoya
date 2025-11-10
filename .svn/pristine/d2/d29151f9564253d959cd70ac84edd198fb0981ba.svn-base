package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.util.List;

import common.util.AppConstant;
import common.util.AppObjectUtil;
import common.util.AppSecurityUtils;
import common.util.AppUpdateNoticeUtils;
import eim.bo.EIMAccessRole;
import eim.bo.EIMAccessRoleType;
import eim.bo.EIMAttributeType;
import eim.bo.EIMEvent;
import eim.bo.EIMObject;
import eim.net.EIMSession;
import eim.util.AccessUtils;
import eim.util.AttributeUtils;
import eim.util.EIMConfig;
import eim.util.EIMConstant;
import eim.util.EventAttributeUtils;
import eim.util.OperationHistoryUtils;
import eim.util.SecurityUtils;
import jp.co.ctc_g.eim.app.document.business.domain.ApprovalReqInfoDomain;
import jp.co.ctc_g.eim.framework.business.dao.AssignDao;
import jp.co.ctc_g.eim.framework.business.dao.StatusDao;
import jp.co.ctc_g.eim.framework.business.domain.AssignDomain;
import jp.co.ctc_g.eim.framework.business.domain.BaseEventTypeExecDomain;
import jp.co.ctc_g.eim.framework.business.domain.StatusTypeKindDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.StatusDomain;
import jp.co.ctc_g.eim.framework.business.service.impl.BaseEventTypePlugInImpl;
import jp.co.ctc_g.eim.framework.common.exception.EIMAppException;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

public class BaseEvtSendBackPlugInImpl extends BaseEventTypePlugInImpl {
	
	protected StatusDao statusDao = null;
	protected AssignDao assignDao = null;
	
	/**
	 * 実行権限を判定します。「差戻し」 
	 * 
	 * @see jp.co.ctc_g.eim.framework.business.service.impl.BaseEventTypePlugInImpl#enabled(jp.co.ctc_g.eim.framework.business.domain.BaseEventTypeExecDomain)
	 */
	public boolean enabled(BaseEventTypeExecDomain baseEventTypeExecDomain) throws Exception  {
		
		EIMSession sess = EIMThreadContext.getEIMSession();
		ObjectDomain objDomain = baseEventTypeExecDomain.getObject();
		
		// システム管理権限を保有フラグ
		Boolean isSystemSecurityAuth = false;
		
		//オブジェクト更新権チェック
		if(objDomain.getSecurity() != null) {
			
			//ベースイベントタイプに設定されたアクセス権限を取得
			String aclKey = baseEventTypeExecDomain.getBaseEventType().getAclKey();
			EIMAccessRoleType acrType = SecurityUtils.getAccessRoleTypeByName(sess, aclKey);
			
			// システム管理権限を保有するかを取得
			isSystemSecurityAuth = AppSecurityUtils.authorizedBySystemSecurity(sess, EIMAccessRole.STATUS_UP);
			
			EIMObject object = objDomain.createEIMObject();
			if(!isSystemSecurityAuth && !SecurityUtils.authorized(sess, object, sess.getUser(), acrType.getId())) {
				
				return false;
			}
		}

		//ステータスチェック
		StatusTypeKindDomain stTypeKindDomain = baseEventTypeExecDomain.getObject().getStatus().getStatusType().getStatusTypeKind();
		if (stTypeKindDomain.getId() != AppConstant.STATUS_TYPE_KIND_ID_APPROVE) {
			
			throw new EIMAppException(sess, "EIM.ERROR.LOGIC.NOTUPDATING", new Object[]{objDomain.getName()});
		}
		
		// システム管理権限保有者はアサイン先に含まれているかをチェックしない
		if (isSystemSecurityAuth){
			return true;
		}
		
		//ロック状態チェック
		if (objDomain.getLUser() != null || objDomain.getLDate() != null) {
			throw new EIMAppException(sess, "EIM.ERROR.LOGIC.OBJECT.LOCKED", new Object[]{objDomain.getName()});
		}

		//アサイン先を取得
		StatusDomain statusDomain = objDomain.getStatus();
		AssignDomain prmAssignDomain = new AssignDomain();
		prmAssignDomain.setStatus(statusDomain);
		List<AssignDomain> assignDomainList = assignDao.getList(prmAssignDomain);
		
		// アサイン先ユーザであるかチェック
		long userId = sess.getUser().getId();
		for (AssignDomain assign : assignDomainList) {
			
			if (assign.getOwner().getId() == userId) {
				
				if (assign.getEvent() == null) {
					return true;
				}
				else {
					// タスクが完了している
					throw new EIMAppException(sess, "EIM.ERROR.LOGIC.NOAPPROVEROLE");
				}
			}
		}
		// アサイン先でない場合の例外
		throw new EIMAppException(sess, "EIM.ERROR.LOGIC.NOAPPROVEROLE");
	}
	
	/**
	 * ベースイベントタイプアクションを実行します。 
	 * 
	 * @see jp.co.ctc_g.eim.framework.business.service.impl.BaseEventTypePlugInImpl#doAction(jp.co.ctc_g.eim.framework.business.domain.BaseEventTypeExecDomain)
	 */
	public void doAction(BaseEventTypeExecDomain baseEventTypeExecDomain) throws Exception {
		
		EIMSession sess = EIMThreadContext.getEIMSession();
		EIMObject object = baseEventTypeExecDomain.getObject().createEIMObject();
		
		// パラメータを取得
		ApprovalReqInfoDomain approvalReqInfoDomain = new ApprovalReqInfoDomain(baseEventTypeExecDomain);
		
		// 「受信確認」オブジェクトを削除
		AppObjectUtil.deleteReceiveObject(sess, object);
		
		// SearchFramework 検索FW更新通知 対象：WF付きフォルダ、ドキュメントと配下のフォルダ、ドキュメント
		AppUpdateNoticeUtils.updateNoticeInsertWFObject(sess, object,
				"SEARCHFW_BASEEVT_SENDBACK_DOCUMENT", "SEARCHFW_BASEEVT_SENDBACK_FOLDER", 
				"SEARCHFW_BASEEVT_SENDBACK_CHILD_DOCUMENT", "SEARCHFW_BASEEVT_SENDBACK_CHILD_FOLDER");
		
		/*--- 共通処理 ---*/
		
		// イベント属性「コメント」を設定
		EIMEvent event = baseEventTypeExecDomain.getEvent().createEIMEvent();
		EIMAttributeType attTypeComment = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.getValue("EVENT_ATTR_NAME_COMMENT"));
		EventAttributeUtils.setAttribute(sess, event, attTypeComment, approvalReqInfoDomain.getComment());
		
		//Access
		AccessUtils.createAccess(sess, object, "EIM.ACCESS.TYPE.SENDBACK");

		//Create Operation History
		OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.APPROVE_BACK, 
				EIMConstant.TARGET_APPROVE, EIMConstant.OBJECT, object,
				null, null, null, AppObjectUtil.getPath(object));
		
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
	
	/**
	 * @return the assignDao
	 */
	public AssignDao getAssignDao() {
		return assignDao;
	}

	/**
	 * @param assignDao the assignDao to set
	 */
	public void setAssignDao(AssignDao assignDao) {
		this.assignDao = assignDao;
	}

}
