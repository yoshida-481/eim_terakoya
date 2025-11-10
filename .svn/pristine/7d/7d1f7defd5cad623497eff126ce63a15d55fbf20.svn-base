package common.util;

import java.util.List;

import eim.bo.EIMAccessEntry;
import eim.bo.EIMAccessRole;
import eim.bo.EIMAccessRoleType;
import eim.bo.EIMAttribute;
import eim.bo.EIMException;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMSecurity;
import eim.bo.EIMStatusSecurity;
import eim.bo.EIMStatusType;
import eim.bo.EIMUser;
import eim.bo.EIMWorkFlow;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.ObjectUtils;
import eim.util.SecurityUtils;
import eim.util.StatusSecurityUtils;
import eim.util.WorkFlowUtils;
/**
 * 
 * アプリケーション共通セキュリティ関連処理クラス
 *
 */
public class AppSecurityUtils {

	/**
	 * オブジェクトタイプセキュリティオブジェクトを取得する。
	 * @param sess
	 * @param objectType 取得対象のオブジェクトタイプ
	 * @return
	 * @throws Exception
	 */
	public static boolean authorizedByObjectType(EIMSession sess, EIMObjectType objectType, int roleId) throws Exception{
		boolean isEnable;
			EIMObjectType objectTypeSec = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_OBJECTTYPE"));
			EIMObject objectSec =  ObjectUtils.getObjectByTypeAndName(sess, objectTypeSec, String.valueOf(objectType.getId()));
			if(objectSec == null){
				throw new EIMException(sess,"EIM.ERROR.LOGIC.OBJECTTYPE.SECURITY.NOEXISTS");
			}
			else{
				isEnable = SecurityUtils.authorized(sess, objectSec, sess.getUser(), roleId);
			}
		return isEnable;
	}
	/**
	 * systemセキュリティでセキュリティをチェックする。
	 * @param sess
	 * @param roleId アクセスロールID
	 * @return
	 * @throws Exception
	 */
	public static boolean authorizedBySystemSecurity(EIMSession sess, int roleId) throws Exception{
		EIMObjectType systemSecType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_SYSTEM_SECURITY"));
		EIMObject object = ObjectUtils.getObjectByTypeAndName(sess, systemSecType, EIMConfig.get("SYSTEM_SECURITY_NAME"));
		if(object == null){
			throw new EIMException(sess,"EIM.ERROR.LOGIC.OBJECTTYPE.SECURITY.NOEXISTS");
		}
		return SecurityUtils.authorized(sess, object, sess.getUser(), roleId);
	}

	/**
	 * 対象のアクセスエントリーのステータス別セキュリティを設定する
	 * 設定するステータスは、公開済以外のステータス
	 * ・アクセス権限が公開読取のみ許可の場合、上記のステータス別セキュリティを拒否
	 * ・アクセス権限がその他の設定の場合、ステータス別セキュリティを無視に設定
	 * @param sess
	 * @param sec セキュリティ
	 * @param workFlow 更新対象のワークフロー
	 * @param entry アクセスエントリー
	 * @throws Exception
	 */
	public static void updateStatusSecurityForDocBySecWfAce(EIMSession sess, EIMSecurity sec, EIMWorkFlow workFlow, EIMAccessEntry entry)throws Exception{
		//ステータスのworkflowのステータスのカラムが入っていない可能性があるので再取得
		workFlow = WorkFlowUtils.getWorkFlowById(sess, workFlow.getId());

		updateStatusSecurityForDocBySecWfAceInternal(sess, sec, workFlow, entry);
	}

	/**
	 * 対象のアクセスエントリーのステータス別セキュリティを設定する
	 * 設定するステータスは、公開済以外のステータス
	 * ・アクセス権限が公開読取のみ許可の場合、上記のステータス別セキュリティを拒否
	 * ・アクセス権限がその他の設定の場合、ステータス別セキュリティを無視に設定
	 * @param sess
	 * @param sec セキュリティ
	 * @param workFlow 更新対象のワークフロー
	 * @param entry アクセスエントリー
	 * @throws Exception
	 */
	private static void updateStatusSecurityForDocBySecWfAceInternal(EIMSession sess, EIMSecurity sec, EIMWorkFlow workFlow, EIMAccessEntry entry)throws Exception{
		@SuppressWarnings("unchecked")
		List<EIMAccessRole> defaultRoleList = SecurityUtils.getAccessRoleList(sess, entry, null);
		//公開読取のアクセス権限
		boolean isPublicRoleAccess = false;
		//公開読取以外のアクセス権限
		boolean isOtherRoleAccess = false;
		//常時読み取りの権限
		int permitRole500 = 0;
		for(EIMAccessRole dfRole : defaultRoleList){
			if("READ".equals(dfRole.getType().getName())){
				if(dfRole.getPermit().getValue() == 1){
					isPublicRoleAccess = true;
				}
			}
			else{
				if(dfRole.getPermit().getValue() == 1){
					isOtherRoleAccess = true;
				}
				if("ROLE_500".equals(dfRole.getType().getName())){
					permitRole500 = dfRole.getPermit().getValue();
				}
			}
		}
		@SuppressWarnings("unchecked")
		List<EIMStatusType> statusTypeList = workFlow.getStatusTypeList();
		EIMAccessRoleType roleType = SecurityUtils.getAccessRoleTypeByName(sess, "READ");
		for(EIMStatusType statustype : statusTypeList){
			if(statustype.getKind() != AppConstant.STATUS_TYPE_KIND_ID_PUBLIC){
				EIMStatusSecurity statusSec = StatusSecurityUtils.getStatusSecurityBySecurityAndStatusType(sess,sec,statustype);
				if(statusSec == null){
					statusSec = StatusSecurityUtils.createStatusSecurity(sess, sec, statustype);
				}
				EIMAccessRole role;
				//公開読取のみ許可の場合
				if(isPublicRoleAccess && !isOtherRoleAccess){
					// 常時読み取りが拒否の場合と無視の場合でステータス別セキュリティの権限を分ける。
					if(permitRole500==EIMAccessRole.PermitMode.IGNORE.getValue()){
						//無視
						role = new EIMAccessRole(roleType.getId(), roleType, statusSec, EIMAccessRole.PermitMode.getModeByValue(2));
					}else{
						//拒否
						role = new EIMAccessRole(roleType.getId(), roleType, statusSec, EIMAccessRole.PermitMode.getModeByValue(0));
					}
				}
				else{
					role = new EIMAccessRole(roleType.getId(), roleType, statusSec, EIMAccessRole.PermitMode.getModeByValue(3));
				}
				
				SecurityUtils.updateAccessRole(sess, entry, statusSec,  role, role.getPermit());
			}
		}
	}
	/**
	 * 対象のワークフローのステータス別セキュリティを設定する
	 * 設定するステータスは、承認依頼中、公開処理中、公開済
	 * アクセス権限が公開読取の場合にのみ許可の場合上記のステータス別セキュリティを拒否
	 * その他の場合、ステータス別セキュリティを無視に設定
	 * @param sess
	 * @param workFlow 更新対象のワークフロー
	 * @throws Exception
	 */
	public static void updateStatusSecurityForDocByWf(EIMSession sess, EIMWorkFlow workFlow)throws Exception{
		@SuppressWarnings("unchecked")
		List<EIMSecurity> secList = SecurityUtils.getSecurityList(sess);

		// ワークフロー再取得
		EIMWorkFlow wf = WorkFlowUtils.getWorkFlowById(sess, workFlow.getId());

		for(EIMSecurity sec : secList){
			@SuppressWarnings("unchecked")
			List<EIMAccessEntry> entryList = SecurityUtils.getAccessEntryList(sess, sec);
			for(EIMAccessEntry entry : entryList){
				updateStatusSecurityForDocBySecWfAceInternal(sess, sec, wf, entry);
			}
		}
	}
	
	/**
	 * フォルダ構成管理権限を持っているかチェックする
	 * @param sess セッション情報
	 * @param object 判定対象となるフォルダもしくはワークスペース
	 * @param user 判定対象となるユーザ
	 * @param accessRole 判定する権限
	 * @param objectSecurity オブジェクトが元々持っているセキュリティ
	 * @return 
	 * @throws Exception
	 */
	public static boolean authorizedLowFolderSecurity(EIMSession sess, EIMObject object, EIMUser user, int accessRole) throws Exception {
		
		// オブジェクトが元々持っていたセキュリティは保持しておく(最後に元に戻す)
		EIMSecurity objectSec = object.getSecurity();

		return AppSecurityUtils.authorizedLowFolderSecurity(sess, object, user, accessRole, objectSec);
	}
	
	/**
	 * フォルダ構成管理権限を持っているかチェックする
	 * @param sess セッション情報
	 * @param object 判定対象となるフォルダもしくはワークスペース
	 * @param user 判定対象となるユーザ
	 * @param accessRole 判定する権限
	 * @param objectSec オブジェクトが元々持っているセキュリティ
	 * @return 
	 * @throws Exception
	 */
	public static boolean authorizedLowFolderSecurity(EIMSession sess, EIMObject object, EIMUser user, int accessRole, EIMSecurity objectSec) throws Exception {
		
		boolean isAuthorized = false;
		
		// フォルダ構成管理権限セキュリティIDを取得
		EIMAttribute lowFolderAttr = object.getAttribute(EIMConfig.getValue("ATTR_NAME_WORKSPACE_LOW_FOLDER_SEC"));
		
		// フォルダ構成管理権限が設定されていない
		if (lowFolderAttr == null) {
			return true;
		}
		
		// フォルダ構成管理権限セキュリティを判定対象オブジェクトのセキュリティと入れ替える
		EIMSecurity lowFolderSec = SecurityUtils.getSecurityById(sess, lowFolderAttr.getInt());
		SecurityUtils.setSecurity(sess, object, lowFolderSec);

		try {
			// 権限判定
			isAuthorized = SecurityUtils.authorized(sess, object, user, accessRole);
		} catch (Exception e) {
			throw e;
		} finally {
			// 設定していたセキュリティを元に戻す
			SecurityUtils.setSecurity(sess, object, objectSec);
		}

		return isAuthorized;
	}
	
}
