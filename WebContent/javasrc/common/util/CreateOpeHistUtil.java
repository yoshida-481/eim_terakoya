package common.util;

import eim.bo.EIMAccessEntry;
import eim.bo.EIMAccessEntryType;
import eim.bo.EIMComp;
import eim.bo.EIMException;
import eim.bo.EIMGroup;
import eim.bo.EIMRole;
import eim.bo.EIMSecurity;
import eim.bo.EIMStatusSecurity;
import eim.bo.EIMUser;
import eim.net.EIMSession;
import eim.util.EIMConstant;
import eim.util.OperationHistoryUtils;


/**
*
* 操作履歴保存関連クラス
*
*/
public class CreateOpeHistUtil {

	/**
	 * セキュリティのエントリーの権限に関する操作履歴を追加します。
	 * 
	 * <LI>ログに記録する時間はDBサーバのシステム時間です。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param sec セキュリティ情報
	 * @param entry エントリー情報
	 * @param entryType エントリー種別情報
	 * @param appType アプリケーション種別
	 * @throws Exception
	 */
	public static void createUpdateSecurityEntryRoleHistory(EIMSession sess, EIMSecurity sec, EIMAccessEntry entry,
			EIMAccessEntryType entryType, String appType) throws Exception{

		if(sess == null || entryType == null || entry == null)
		{
			//必須項目が渡されませんでした。
			throw new EIMException(sess , "EIM.ERROR.LOGIC.NOTACCEPT.INDISPENSABILITY.ITEM");
		}
		
		try
		{
			//User
			if(entryType.getId() == EIMAccessEntryType.USER)
			{
				EIMUser user = entry.getUser();

				OperationHistoryUtils.create(sess, appType, EIMConstant.UPDATE_USER_ENTRY_AUTH, 
						EIMConstant.TARGET_PARENT_SECURITY, EIMConstant.SECURITY, sec,
						EIMConstant.TARGET_UPDATE, EIMConstant.USER, user, null);
			}
			//Group
			if(entryType.getId() == EIMAccessEntryType.GROUP)
			{
				EIMGroup group = entry.getGroup();

				OperationHistoryUtils.create(sess, appType, EIMConstant.UPDATE_GROUP_ENTRY_AUTH, 
						EIMConstant.TARGET_PARENT_SECURITY, EIMConstant.SECURITY, sec,
						EIMConstant.TARGET_UPDATE, EIMConstant.GROUP, group, null);
			}
			//Role
			if(entryType.getId() == EIMAccessEntryType.ROLE)
			{
				EIMRole role = entry.getRole();

				OperationHistoryUtils.create(sess, appType, EIMConstant.UPDATE_ROLE_ENTRY_AUTH, 
						EIMConstant.TARGET_PARENT_SECURITY, EIMConstant.SECURITY, sec,
						EIMConstant.TARGET_UPDATE, EIMConstant.ROLE, role, null);
			}
			//Comp
			if(entryType.getId() == EIMAccessEntryType.COMP)
			{
				EIMComp comp = entry.getComp();

				OperationHistoryUtils.create(sess, appType, EIMConstant.UPDATE_COMPLEX_GROUP_ENTRY_AUTH, 
						EIMConstant.TARGET_PARENT_SECURITY, EIMConstant.SECURITY, sec,
						EIMConstant.TARGET_UPDATE, EIMConstant.COMP, comp, null);
			}
		}
		catch(Exception e)
		{
			throw e;
		}
	}
	
	/**
	 * ステータス別セキュリティのエントリーの権限に関する
	 * 操作履歴を追加します。
	 * 
	 * <LI>ログに記録する時間はDBサーバのシステム時間です。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param stsec ステータス別セキュリティ情報
	 * @param entry エントリー情報
	 * @param entryType エントリー種別情報
	 * @param appType アプリケーション種別
	 * @throws Exception
	 */
	public static void createUpdateStatusSecurityEntryRoleHistor
		(EIMSession sess, EIMStatusSecurity stsec, EIMAccessEntry entry,
			EIMAccessEntryType entryType, String appType) throws Exception{

		if(sess == null || entryType == null || entry == null)
		{
			//必須項目が渡されませんでした。
			throw new EIMException(sess , "EIM.ERROR.LOGIC.NOTACCEPT.INDISPENSABILITY.ITEM");
		}
		
		try
		{
			//User
			if(entryType.getId() == EIMAccessEntryType.USER)
			{
				EIMUser user = entry.getUser();

				OperationHistoryUtils.create(sess, appType, EIMConstant.UPDATE_USER_ENTRY_AUTH, 
						EIMConstant.TARGET_PARENT_STATUS_SECURITY, EIMConstant.SECURITY, stsec,
						EIMConstant.TARGET_UPDATE, EIMConstant.USER, user, null);
			}
			//Group
			if(entryType.getId() == EIMAccessEntryType.GROUP)
			{
				EIMGroup group = entry.getGroup();

				OperationHistoryUtils.create(sess, appType, EIMConstant.UPDATE_GROUP_ENTRY_AUTH, 
						EIMConstant.TARGET_PARENT_STATUS_SECURITY, EIMConstant.SECURITY, stsec,
						EIMConstant.TARGET_UPDATE, EIMConstant.GROUP, group, null);
			}
			//Role
			if(entryType.getId() == EIMAccessEntryType.ROLE)
			{
				EIMRole role = entry.getRole();

				OperationHistoryUtils.create(sess, appType, EIMConstant.UPDATE_ROLE_ENTRY_AUTH, 
						EIMConstant.TARGET_PARENT_STATUS_SECURITY, EIMConstant.SECURITY, stsec,
						EIMConstant.TARGET_UPDATE, EIMConstant.ROLE, role, null);
			}
			//Comp
			if(entryType.getId() == EIMAccessEntryType.COMP)
			{
				EIMComp comp = entry.getComp();

				OperationHistoryUtils.create(sess, appType, EIMConstant.UPDATE_COMPLEX_GROUP_ENTRY_AUTH, 
						EIMConstant.TARGET_PARENT_STATUS_SECURITY, EIMConstant.SECURITY, stsec,
						EIMConstant.TARGET_UPDATE, EIMConstant.COMP, comp, null);
			}
		}
		catch(Exception e)
		{
			throw e;
		}
	}
}