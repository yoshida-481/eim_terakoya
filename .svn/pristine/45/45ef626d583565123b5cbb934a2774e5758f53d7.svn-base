package jp.co.ctc_g.eim.app.document.business.util;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

import jp.co.ctc_g.eim.framework2.business.domain.entity.AccessEntryDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AccessRoleDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ComplexDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.GroupDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.RoleDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.SecurityDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework2.business.service.GroupService;
import jp.co.ctc_g.eim.framework2.business.service.ObjectService;
import jp.co.ctc_g.eim.framework2.business.service.RoleService;
import jp.co.ctc_g.eim.framework2.business.service.SecurityService;
import jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework2.common.enumeration.AccessRolePermissionModeEnum;
import jp.co.ctc_g.eim.framework2.common.enumeration.EntryTypeEnum;

import common.util.AppObjectUtil;

import eim.bo.EIMAccessRole;
import eim.bo.EIMException;
import eim.bo.EIMUser;


public class AccesableSecurityUtil {

	/**
	 *  対象オブジェクトに対して参照権限を保持しているユーザリストを取得します。
	 *  引数に指定するオブジェクトIDに該当するオブジェクトが存在しない場合、空リストを返却します。
	 * 
	 * @param targetObjId 対象オブジェクトID
	 * @return 対象オブジェクトの参照権限を保持するユーザリスト
	 * @throws Exception
	 */
	public static List<UserDomain> getAccesableUserList(long targetObjId) throws Exception {
		
		// サービス取得
		ObjectService objectService = (ObjectService)ApplicationContextLoader.getApplicationContext().getBean("objectServiceForSelectEntry");
		SecurityService securityService = (SecurityService)ApplicationContextLoader.getApplicationContext().getBean("securityServiceForSelectEntry");
		
		
		// エントリリスト取得
		ObjectDomain targetObj = objectService.getById(targetObjId);
		if(targetObj == null) {
			
			// 対象オブジェクトが既に存在しない場合、空リストを返却
			return null;
		}
		long secId = targetObj.getSecurity().getId();
		List<AccessEntryDomain> entryList = securityService.getById(secId).getAccessEntryList();
		
		
		// 優先度でソート
		entryList = AppObjectUtil.getIntSortedList(entryList, "getPriority", true);
		
		// 対象オブジェクトへ参照可能ユーザリスト
		List<UserDomain> accesableUserList = new ArrayList<UserDomain>();
		// 対象オブジェクトの参照可能ユーザIDリスト
		HashSet<Long> accessAcceptUserIdList = new HashSet<Long>();
		// 対象オブジェクトの参照不可ユーザIDリスト
		HashSet<Long> accessDenyUserIdList = new HashSet<Long>();
		
		
		for(AccessEntryDomain entry: entryList) {
			
			// エントリユーザリスト取得
			List<UserDomain> entryUserList = getEntryUserList(entry);
			
			// 対象オブジェクトへのアクセスロールリスト
			List<AccessRoleDomain> accessRoleList = entry.getAccessRoleList();
			for(AccessRoleDomain accessRole: accessRoleList) {
				
				if(accessRole.getType().getId() == EIMAccessRole.READ) { // AccessRoleについてFrameWork2では定義されていないため、EIMAccessRoleを使用
					
					if(accessRole.getPermission() == AccessRolePermissionModeEnum.ACCEPT) {
						
						// 参照権限が許可の場合
						for(UserDomain entryUser: entryUserList) {
							
							if(!accessDenyUserIdList.contains(entryUser.getId())
								&& !accessAcceptUserIdList.contains(entryUser.getId())) {
								
								// 参照可能ユーザリスト、参照不可ユーザリストに含まれていない場合、参照可能ユーザリストに追加
								accesableUserList.add(entryUser);
								// 参照可能ユーザIDリストに追加
								accessAcceptUserIdList.add(entryUser.getId());
							}
						}
						
					} else if(accessRole.getPermission() == AccessRolePermissionModeEnum.DENY) {
						
						for(UserDomain entryUser: entryUserList) {
							
							// 参照権限が拒否の場合、参照不可ユーザリストに追加
							accessDenyUserIdList.add(entryUser.getId());
						}
					}
				}
			}
		}
		return accesableUserList;
	}
	
	
	/**
	 * エントリに所属するユーザリストを取得します。
	 * 
	 * @param entry 対象エントリ
	 * @return 対象エントリに所属するユーザリスト
	 */
	private static List<UserDomain> getEntryUserList(AccessEntryDomain entry) throws Exception {
		
		// エントリユーザリスト
		List<UserDomain> entryUserList = new ArrayList<UserDomain>();
		
		// ユーザ単位に分解しリストに格納
		if(entry.getEntryType() == EntryTypeEnum.USER)
		{
			// 対象オブジェクトへアクセス可能ユーザリストに追加
			entryUserList.add((UserDomain)entry.getEntryElement());
		}
		else if(entry.getEntryType() == EntryTypeEnum.GROUP)
		{
			GroupService groupService  = (GroupService)ApplicationContextLoader.getApplicationContext().getBean("groupServiceForSelectEntry");
			// グループに所属するユーザリスト取得
			GroupDomain group = groupService.getById(entry.getEntryElement().getId());
			
			// 子グループに所属するユーザも再帰的に取得
			entryUserList = getRecursiveUserList(group, entryUserList);
		}
		else if(entry.getEntryType() == EntryTypeEnum.ROLE)
		{
			RoleService roleService  = (RoleService)ApplicationContextLoader.getApplicationContext().getBean("roleServiceForSelectEntry");
			// ロールに所属するユーザリスト取得
			RoleDomain role = roleService.getById(entry.getEntryElement().getId());
			
			for(UserDomain roleUser: role.getUserList())
			{
				// 対象オブジェクトへアクセス可能ユーザリストに追加
				entryUserList.add(roleUser);
			}
		}
		else if(entry.getEntryType() == EntryTypeEnum.COMPLEX)
		{
			// 複合グループに所属するユーザリスト取得
			ComplexDomain comp = (ComplexDomain)entry.getEntryElement();
			
			for(UserDomain compUser: comp.getUserList())
			{
				// 対象オブジェクトへアクセス可能ユーザリストに追加
				entryUserList.add(compUser);
			}
		}
		return entryUserList;
	}
	
	
	/**
	 * 親グループのユーザ情報を再帰的に取得します。
	 * 
	 * @param group 対象グループ
	 * @param userList ユーザリスト
	 * @return 子グループを含めたユーザリスト
	 */
	private static List<UserDomain> getRecursiveUserList(GroupDomain group, List<UserDomain> userList) {
		for(UserDomain user: group.getUserList()) {
			
			// ユーザ情報追加
			userList.add(user);
		}
		
		if(group.getChildList().size() > 0) {
			
			// 子グループが存在する場合、再帰的にユーザ情報取得/追加
			for(GroupDomain childGroup: group.getChildList()) {
				
				getRecursiveUserList(childGroup, userList);
			}
		}
		return userList;
	}
}
