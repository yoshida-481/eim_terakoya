package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.util.List;

import addon.PublishCommandAddOnSetExpirationDate;
import common.util.AppConstant;
import common.util.AppObjectUtil;
import common.util.AppOcrUtil;
import common.util.AppUpdateNoticeUtils;
import common.util.UpdateObjectLinkUtil;
import eim.bo.EIMAccessRole;
import eim.bo.EIMAttributeType;
import eim.bo.EIMObject;
import eim.bo.EIMRelation;
import eim.bo.EIMRelationType;
import eim.bo.EIMVersion;
import eim.net.EIMSession;
import eim.util.AttributeUtils;
import eim.util.EIMConfig;
import eim.util.ObjectUtils;
import eim.util.RelationUtils;
import eim.util.VersionUtils;
import jp.co.ctc_g.eim.framework.business.domain.StatusTypeKindExecDomain;
import jp.co.ctc_g.eim.framework.business.service.impl.StatusTypeKindPlugInImpl;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * ステータスタイプ種別「公開中」
 *
 */
public class STKindPublicPlugInImpl extends StatusTypeKindPlugInImpl {
	
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
		
		// チェックアウトされたオブジェクトの場合、古いバージョンのオブジェクトのリレーションを削除する
		if(object.getRev() > 0)
		{
			// 最新履歴を設定
			object = VersionUtils.setLatest(sess, object);
			
			// 前のバージョンのオブジェクトを取得
			EIMVersion version = VersionUtils.getVersion(sess, object);
			EIMObject latestObj = version.getObjectByRev(object.getRev() - 1);

			// アンロック(これを実行しないとステータスが「改訂中」のまま残る)
			ObjectUtils.unLock(sess, latestObj);

			// 親リレーションを削除
			EIMRelationType relType = RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));
			List parentRelList = RelationUtils.getParentRelationListByRelType(sess, latestObj, relType, EIMAccessRole.READ);
			for(int i = 0; i < parentRelList.size(); i++)
			{
				EIMRelation relation = (EIMRelation)parentRelList.get(i);
				RelationUtils.deleteRelation(sess, relation);
				
				// マルチインスタンス暫定対応
				// 過去リビジョンのドキュメントを検索条件にヒットさせるため、リレーションは
				// 削除するが「所属ワークスペース」属性の値は削除しない（トリガ起動後なので再設定する）
				EIMAttributeType attrTypeBelongWS = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.getValue("ATTR_NAME_DOCUMENT_BELONGING_WS"));
				if(attrTypeBelongWS != null){
					EIMObject parent = relation.getParent();
					long parentBelongWS = Integer.MIN_VALUE;
					// 親がワークスペースである場合
					if(parent.getType().getDefName().equals(EIMConfig.getValue("OBJECT_TYPE_NAME_WORKSPACE"))){
						parentBelongWS = parent.getId();
					}else{
						parentBelongWS = AppObjectUtil.getIntAttr(sess, relation.getParent(), EIMConfig.getValue("ATTR_NAME_DOCUMENT_BELONGING_WS"), Integer.MIN_VALUE);
					}
					
					if(parentBelongWS != Integer.MIN_VALUE){
						AppObjectUtil.setAttr(sess, relation.getChild(), EIMConfig.getValue("ATTR_NAME_DOCUMENT_BELONGING_WS"), parentBelongWS);
					}
				}
			}
			
			// 前リビジョンがロックされている（チェックアウトされているドキュメントにチェックインを行う）場合、
			// リンク自動更新処理を呼び出す。
			// チェックアウト無しでチェックインする場合は、リンク自動更新処理は呼び出さない。
			if(latestObj.getLockUser() != null) {
				// リンクリレーションの子オブジェクト（ドキュメント）を最新版のドキュメントに設定する
				UpdateObjectLinkUtil.actUpdateObjectLinkByDocument(sess, object);
			}
			
			// ### SEARCH FRAMEWORK 検索FW更新通知 前リビジョンのオブジェクト
			// 対象リビジョンや配下ドキュメント・オブジェクトの更新通知はEventExecDocDaoImpl.changeStatus()にて実施
			AppUpdateNoticeUtils.updateNoticeInsert(latestObj.getId(), "SEARCHFW_STKIND_PUBLIC_OLD_DOCUMENT");
		}
		
		// 「有効期限」属性を設定
		PublishCommandAddOnSetExpirationDate addOn = new PublishCommandAddOnSetExpirationDate();
		addOn.doPublishCommandOnline(sess, object);
		
		// OCR
		// OCR処理ステータスが「0:処理待ち」の場合、OCR処理オブジェクトを作成する
		if(AppOcrUtil.getOcrProcessingStatus(sess, object) == AppConstant.OCR_PROC_STATUS_PROCESS_WAIT){
			AppOcrUtil.createOcrProcessingObject(sess, object);
		}
		
		return;
	}
}
