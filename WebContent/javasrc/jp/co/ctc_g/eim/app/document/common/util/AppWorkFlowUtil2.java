package jp.co.ctc_g.eim.app.document.common.util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import common.util.AppObjectUtil;
import eim.bo.EIMGroup;
import eim.bo.EIMRole;
import eim.bo.EIMUser;
import jp.co.ctc_g.eim.framework.common.util.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework2.business.dao.AssignmentEntryDao;
import jp.co.ctc_g.eim.framework2.business.dao.AssignmentPlanDao;
import jp.co.ctc_g.eim.framework2.business.dao.GroupDao;
import jp.co.ctc_g.eim.framework2.business.dao.RoleDao;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.AssignmentPlanCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.MultipleCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AssignmentEntryDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AssignmentPlanDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ComplexDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.GroupDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.RoleDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.StatusTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.UserDefinitionGroupDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework2.business.plugin.UserDefinitionGroupPlugIn;
import jp.co.ctc_g.eim.framework2.common.enumeration.EntryTypeEnum;
import jp.co.ctc_g.eim.framework2.common.exception.EIMSystemException;
import jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils;

/**
 * JSPとして実装されているワークフロー関連機能に必要な各種処理をJavaで実装します。
 * framework2の機能を利用する時、AppWorkFlowUtilに既に利用されているframework1クラスとの名称重複を回避するため、AppWorkFlowUtil2として実装します。
 */
public class AppWorkFlowUtil2 {

	/**
	 * 指定したステータスタイプ毎のアサインエントリーをユーザ単位で取得します。
	 * グループ・ロール・複合グループ・ユーザ定義グループはユーザに展開されて重複ユーザが削除されます。
	 * @param statusTypeIds 取得対象のスタータスタイプIdのリスト
	 * @param objId 取得対象のオブジェクトID(ユーザ定義グループを取得する場合に使用する)
	 * @return ステータスタイプ毎のアサインエントリーをユーザ(EIMUser)に展開
	 * @throws Exception
	 */
	public static Map<Long, List<EIMUser>> getAssinmentEntryUserListMapByStatusTypeIds(List<Long> statusTypeIds, long objId) throws Exception {

		if (statusTypeIds == null || statusTypeIds.size() == 0) {
			return null;
		}

		//アサインエントリーDAO
		AssignmentEntryDao assignmentEntryDao =
				(AssignmentEntryDao)ApplicationContextLoader.getContext().getBean("assignmentEntryDaoForApproverSelection");
		// グループDao
		GroupDao groupDao = (GroupDao) ApplicationContextLoader.getContext().getBean("groupDaoForWorkflowHistory");

		// ステータスタイプ毎のアサインエントリーを取得する
		MultipleCriteria<Long> criteriaIds = new MultipleCriteria<>(statusTypeIds);
		Map<Long, List<AssignmentEntryDomain>> assignmentEntryListMap = assignmentEntryDao.getListMapStatusTypeIds(criteriaIds);

		// ステータスタイプ毎のアサインエントリーのリストからエントリータイプ毎にユーザを取得してMapに格納する
		Map<Long, List<EIMUser>> statusTypeUsersMap = new HashMap<>();
		for (Map.Entry<Long, List<AssignmentEntryDomain>> assignmentEntryEntry : assignmentEntryListMap.entrySet())
		{
			Long statusTypeId = assignmentEntryEntry.getKey();
			if ( statusTypeUsersMap.containsKey(statusTypeId) ) {
				// 既に取得済みの場合は省略
				continue;
			}
			List<UserDomain> userList = new ArrayList<>();

			// アサインエントリーのリストからエントリータイプ毎にユーザを取得してステータスタイプ毎のユーザリストに格納する
			for (AssignmentEntryDomain assignmentEntry : assignmentEntryEntry.getValue())
			{
				//ユーザ
				if(assignmentEntry.getEntryType() == EntryTypeEnum.USER)
				{
					UserDomain user = (UserDomain) assignmentEntry.getEntryElement();
					userList.add(user);
				}
				//ロール
				else if(assignmentEntry.getEntryType() == EntryTypeEnum.ROLE)
				{
					RoleDomain role = (RoleDomain) assignmentEntry.getEntryElement();
					userList.addAll(role.getUserList());
				}
				//複合グループ
				else if(assignmentEntry.getEntryType() == EntryTypeEnum.COMPLEX)
				{
					ComplexDomain complex = (ComplexDomain) assignmentEntry.getEntryElement();
					userList.addAll(complex.getUserList());
				}
				//グループ
				else if(assignmentEntry.getEntryType() == EntryTypeEnum.GROUP)
				{
					GroupDomain group = groupDao.getById(assignmentEntry.getEntryElement().getId());

					// 子グループのユーザを再帰取得する
					gatherBelonggingUserListRecurrently(group, userList);
				}
				//ユーザ定義グループ
				else if(assignmentEntry.getEntryType() == EntryTypeEnum.USERDEFINITION)
				{
					UserDefinitionGroupDomain userDefinitionGroup = (UserDefinitionGroupDomain) assignmentEntry.getEntryElement();

					// 当該 ID のプラグインを取得し、ユーザを取得する
					UserDefinitionGroupPlugIn userDefinitionGroupPlugIn =
							(UserDefinitionGroupPlugIn) ApplicationContextLoader.getContext().getBean(userDefinitionGroup.getKey());
					userList = userDefinitionGroupPlugIn.getUserListByObject(new ObjectDomain(objId));
				}
			}
			// EIMUserに変換しMapに詰める（重複排除）
			Map<Long, EIMUser> userMap = new HashMap<>();
			for (UserDomain user : userList) {
				EIMUser eimUser = ConvertUtils.toEIMUser(user);
				userMap.put(Long.valueOf(user.getId()), eimUser);
			}

			// ステータスタイプ毎のユーザMapを追加
			List<EIMUser> sortedUserList = AppObjectUtil.getStrSortedList(new ArrayList<EIMUser>(userMap.values()), "getName", true);
			statusTypeUsersMap.put(statusTypeId, sortedUserList);;
		}

		return statusTypeUsersMap;
	}

	/**
	 * グループの所属ユーザを再帰的に収集します。<p>
	 * 引数groupが保持するchildGroupを対象にuserListを再帰的に収集します。
	 * @param group 最上位のグループ
	 * @param result
	 */
	private static void gatherBelonggingUserListRecurrently(GroupDomain group, List<UserDomain> result) {
		List<UserDomain> userList = group.getUserList();
		for (UserDomain user : userList) {
			result.add(user);
		}

		List<GroupDomain> childGroupList = group.getChildList();
		for (GroupDomain childGroup : childGroupList) {
			gatherBelonggingUserListRecurrently(childGroup, result);
		}

		return;
	}

	/**
	 * 指定したオブジェクトが保持するアサイン予定をステータスタイプ毎に取得します。
	 * @param objId 取得対象のオブジェクトのID
	 * @return ステータスタイプ毎のアサイン予定をユーザ(EIMUser)として保持
	 * @throws Exception
	 */
	public static Map<Long, List<EIMUser>> getAssignmentPlanUserListMap(long objId) throws Exception {
		//アサインプランDAO
		AssignmentPlanDao assignmentPlanDao = (AssignmentPlanDao) ApplicationContextLoader.getContext().getBean("assignmentPlanDao2");

		// アサイン予定を取得する
		AssignmentPlanCriteria assignmentPlanCriteria = new AssignmentPlanCriteria();
		assignmentPlanCriteria.setObjectId(objId);
		List<AssignmentPlanDomain> allAssignmentPlanList = assignmentPlanDao.getList(assignmentPlanCriteria);

		// 取得したアサイン予定をEIMUserに変換してステータスタイプ毎に分割する
		Map<Long, List<EIMUser>> assignmentPlanListMap = new HashMap<>();
		for (AssignmentPlanDomain assignmentPlan : allAssignmentPlanList) {
			StatusTypeDomain statusType = assignmentPlan.getStatusType();
			if (statusType == null) {
				// AssignmentPlanDomain#statusTypeが空の場合(取得対象に指定されていない、langIdの指定がない場合など)
				throw new EIMSystemException("EIM.ERROR.LOGIC.STTYPE.NOTFOUND");
			}
			if (!assignmentPlanListMap.containsKey(statusType.getId())) {
				assignmentPlanListMap.put(statusType.getId(), new ArrayList<>());
			}

			UserDomain user = (UserDomain) assignmentPlan.getEntryElement();
			EIMUser eimUser = ConvertUtils.toEIMUser(user);
			assignmentPlanListMap.get(statusType.getId()).add(eimUser);
		}

		return assignmentPlanListMap;
	}

	/**
	 * ユーザIDをキーとしてユーザが所属するグループ(EIMGroup)をリストで保持するMapを取得します。
	 * @param userList 対象ユーザ(EIMUser)のリスト
	 * @param groupListMap ユーザIDをキーとしてユーザが保有するグループのリスト	※重複して取得することを防ぐ為に利用
	 * @return ユーザIDをキーとしてユーザが所属するグループをリストで保持するMap
	 * @throws Exception
	 */
	public static Map<Long, List<EIMGroup>> getGroupListMapUserIds(List<EIMUser> userList, Map<Long, List<EIMGroup>> groupListMap) throws Exception {

		GroupDao groupDao = (GroupDao) ApplicationContextLoader.getContext().getBean("groupDaoForWorkflowHistory");
		MultipleCriteria<Long> userIdsCriteria = new MultipleCriteria<Long>();

		for (EIMUser user : userList) {
			long userId = user.getId();
			// 既に取得済みのユーザはスキップする
			if ( groupListMap.containsKey(userId)) {
				continue;
			}
			userIdsCriteria.add(userId);
		}

		// ユーザID毎の所属グループMapを取得する
		Map<Long, List<GroupDomain>> tmpMap = groupDao.getListMapUserIds(userIdsCriteria);

		for (Long userId : tmpMap.keySet()) {
			// GroupDomain->EIMGroupに変換する
			List<EIMGroup> groupList = new ArrayList<EIMGroup>();
			for (GroupDomain groupDomain : tmpMap.get(userId)) {
				groupList.add(new EIMGroup(groupDomain.getId(), groupDomain.getDefinitionName(), groupDomain.getName(), null));
			}
			groupListMap.put(userId, groupList);
		}

		return groupListMap;
	}

	/**
	 * ユーザIDをキーとしてユーザが保有するロール(EIMRole)をリストで保持するMapを取得します。
	 * @param userList 対象ユーザ(EIMUser)のリスト
	 * @param roleListMap ユーザIDをキーとしてユーザが保有するロールのリスト		※重複して取得することを防ぐ為に利用
	 * @return ユーザIDをキーとしてユーザが保有するロールをリストで保持するMap
	 * @throws Exception
	 */
	public static Map<Long, List<EIMRole>> getRoleListMapUserIds(List<EIMUser> userList, Map<Long, List<EIMRole>> roleListMap) throws Exception {

		RoleDao roleDao = (RoleDao) ApplicationContextLoader.getContext().getBean("roleDaoForWorkflowHistory");

		MultipleCriteria<Long> userIdsCriteria = new MultipleCriteria<Long>();

		for (EIMUser user : userList) {
			long userId = user.getId();
			// 既に取得済みのユーザはスキップする
			if ( roleListMap.containsKey(userId)) {
				continue;
			}
			userIdsCriteria.add(userId);
		}

		// ユーザID毎の所属グループMapを取得する
		Map<Long, List<RoleDomain>> tmpMap = roleDao.getListMapUserIds(userIdsCriteria);

		for (Long userId : tmpMap.keySet()) {
			// RoleDomain->EIMRoleに変換する
			List<EIMRole> roleList = new ArrayList<EIMRole>();
			for (RoleDomain roleDomain : tmpMap.get(userId)) {
				roleList.add(new EIMRole(roleDomain.getId(), roleDomain.getDefinitionName(), roleDomain.getName(), 0));
			}
			roleListMap.put(userId, roleList);
		}

		return roleListMap;
	}
}
