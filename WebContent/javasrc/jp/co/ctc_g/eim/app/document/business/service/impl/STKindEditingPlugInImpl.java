package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.util.ArrayList;
import java.util.List;

import app.document.object.UpdateObjectLink;
import common.util.AppConstant;
import common.util.AppObjectUtil;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMRelation;
import eim.bo.EIMRelationType;
import eim.bo.EIMVersion;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.ObjectUtils;
import eim.util.RelationUtils;
import eim.util.VersionUtils;
import jp.co.ctc_g.eim.framework.business.domain.EventHistoryDomain;
import jp.co.ctc_g.eim.framework.business.domain.EventLogDomain;
import jp.co.ctc_g.eim.framework.business.domain.NoticeMailDomain;
import jp.co.ctc_g.eim.framework.business.domain.StatusTypeKindExecDomain;
import jp.co.ctc_g.eim.framework.business.service.EventHistoryService;
import jp.co.ctc_g.eim.framework.business.service.impl.StatusTypeKindPlugInImpl;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * ステータスタイプ種別「編集中」
 *
 */
public class STKindEditingPlugInImpl extends StatusTypeKindPlugInImpl {

	/** イベントヒストリサービス */
	private  EventHistoryService eventHistoryService;

	/**
	 * ステータスが遷移した直後の処理。<br>
	 *
	 * @param statusTypeKindExecDomain ステータスタイプ種別ドメイン
	 *
	 * @throws Exception
	 */
	public void entry(StatusTypeKindExecDomain statusTypeKindExecDomain) throws java.lang.Exception
	{
		EIMSession sess = EIMThreadContext.getEIMSession();
		EIMObject object = ObjectUtils.getObjectById(sess, statusTypeKindExecDomain.getObject().getId());
		
		EventHistoryDomain eventHistoryDomain = eventHistoryService.getByObjId(object.getId());
		List<EventLogDomain> eventLogDomainList = eventHistoryDomain.getEventLogList();
		if (eventLogDomainList.size() > 0) {
			// 最新のイベントを取得
			EventLogDomain latestEventLogDomain = eventLogDomainList.get(eventLogDomainList.size() - 1);
			// 公開取消により編集中になった場合(最新のイベントが公開取消)の場合
			if (latestEventLogDomain.getEvent().getEventType().getBaseEventType().getId() == AppConstant.BASE_EVENT_TYPE_ID_PUBLIC_CANCEL) {
				
				// 通知メールがない場合、受信確認オブジェクトを削除する
				List<NoticeMailDomain> noticeMailList = latestEventLogDomain.getEvent().getEventType().getMailList();
				if (noticeMailList == null || noticeMailList.size() == 0) {
					AppObjectUtil.deleteReceiveObject(sess, object);
				}
				
				// バージョンを取得する
				EIMVersion version = VersionUtils.getVersion(sess, object);
				if (version != null) {
					// 初版かのフラグ
					boolean isFirstVer = (version.getList().size() > 1)? false : true; 

					List<EIMObject> workflowDocList = new ArrayList<EIMObject>();
					if (isDocumentTypes(object.getType())) {
						workflowDocList.add(object);

					} else if (isFolderTypes(object.getType())) {
						// フォルダ配下のドキュメント全て取得する
						workflowDocList = AppObjectUtil.getChildEIMObjectRecurrently(sess, object, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));

					} else {
						return;
					}

					EIMRelationType relTypeDocLink = RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_LINK"));
					for (EIMObject workflowDoc : workflowDocList) {
						documentLinkPublicCancel(sess, relTypeDocLink, workflowDoc, isFirstVer);
					}
				}
			}
		}
	}
	/**
	 * 引数のオブジェクトタイプがドキュメントかどうかを判定する。<br>
	 *
	 * @param objType 処理対象オブジェクトタイプ
	 *
	 * @throws Exception
	 */
	public boolean isDocumentTypes(EIMObjectType objType) throws Exception {
		
		EIMObjectType rootObjType = objType;
		while(rootObjType.getParent() != null) {
			rootObjType = rootObjType.getParent();
		}
		
		if (rootObjType.getName().equals(EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT"))) {
			return true;
		} else {
			return false;
		}
	}
	/**
	 * 引数のオブジェクトタイプがフォルダかどうかを判定する。<br>
	 *
	 * @param objType 処理対象オブジェクトタイプ
	 *
	 * @throws Exception
	 */
	public boolean isFolderTypes(EIMObjectType objType) throws Exception {
		
		EIMObjectType rootObjType = objType;
		while(rootObjType.getParent() != null) {
			rootObjType = rootObjType.getParent();
		}
		
		if (rootObjType.getName().equals(EIMConfig.get("OBJECT_TYPE_NAME_WORKSPACE"))) {
			return true;
		} else {
			return false;
		}
	}
	/**
	 * 公開取消時のドキュメントリンク最新化処理。<br>
	 * ドキュメントの版で以下のように、最新化する
	 * ・初版の場合、削除
	 * ・1版以上の場合、最新の公開済の版に更新
	 *
	 * @param sess セッション
	 * @param relTypeDocLink リレーションタイプ：ドキュメント
	 * @param targetDoc 対象ドキュメントオブジェクト
	 * @param isFirstVer 初版かどうかのフラグ
	 *
	 * @throws Exception
	 */
	private void documentLinkPublicCancel(EIMSession sess, EIMRelationType relTypeDocLink, EIMObject targetDoc, boolean isFirstVer) throws java.lang.Exception
	{
		// ドキュメンリンクを取得する
		List docLinkList = RelationUtils.getParentRelationListByRelType(sess, targetDoc, relTypeDocLink);
		
		for (int i = 0; i < docLinkList.size(); i++) {
			
			EIMRelation docLinkRel = (EIMRelation)docLinkList.get(i);
			UpdateObjectLink updateObjLink = new UpdateObjectLink(sess, targetDoc.getId(), docLinkRel.getParent().getId());
			// ドキュメントリンクが存在する場合
			if (updateObjLink.isExistDocumentLink()) {
				// 初版の場合、削除
				if (isFirstVer) {
					updateObjLink.deleteLink();
					
				// 版数を重ねている場合、最新公開済にリレーション更新
				} else {
					updateObjLink.updateLink();
				}
			}
		}
	}

	/**
	 * イベントヒストリサービスを取得します。
	 * 
	 * @return イベントヒストリサービス
	 */
	public EventHistoryService getEventHistoryService() {
		return eventHistoryService;
	}

	/**
	 * イベントヒストリサービスを設定します。
	 * 
	 * @param eventHistoryService イベントヒストリサービス
	 */
	public void setEventHistoryService(EventHistoryService eventHistoryService) {
		this.eventHistoryService = eventHistoryService;
	}
}
