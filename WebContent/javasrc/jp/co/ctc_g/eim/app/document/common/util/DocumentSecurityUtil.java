package jp.co.ctc_g.eim.app.document.common.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.springframework.context.ApplicationContext;

import common.util.AppConstant;
import jp.co.ctc_g.eim.app.document.common.enumeration.DocumentAccessRoleTypeEnum;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AccessEntryDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AccessRoleDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AccessRoleTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.SecurityDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.StatusSecurityDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.StatusTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.WorkflowDomain;
import jp.co.ctc_g.eim.framework2.business.service.AccessEntryService;
import jp.co.ctc_g.eim.framework2.business.service.AccessRoleTypeService;
import jp.co.ctc_g.eim.framework2.business.service.SecurityService;
import jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework2.common.enumeration.AccessRolePermissionModeEnum;
import jp.co.ctc_g.eim.framework2.common.exception.EIMException;

/**
 * 【ドキュメントAPI】
 */
public class DocumentSecurityUtil {

	/** 書込み権限 */
	private static String LBL_WRITE = "WRITE";
	/** ステータス変更 */
	private static String LBL_STATUS_CHANGE = "STATUS_CHANGE";
	/** 常時読取 */
	private static String LBL_ALWAYS_READ = "ALWAYS_READ";
	/** 公開読取 */
	private static String LBL_PUBLIC_READ = "PUBLIC_READ";

	/**
	 * アクセスロールをドキュメント管理用から変換したセキュリティ情報に変換します。
	 *
	 * @param securityDomain
	 * @return
	 * @throws Exception
	 */
	public static SecurityDomain convertDocumentSecurityToOriginalSecurity(SecurityDomain securityDomain) throws Exception {
		if (securityDomain == null)
			return null;

		SecurityDomain security = securityDomain.clone();

		// アクセスエントリー一覧を取得
		List<AccessEntryDomain> accessEntryList = security.getAccessEntryList();

		if (accessEntryList != null) {
			for (AccessEntryDomain accessEntryDomain : accessEntryList) {
				// アクセス権限一覧
				List<AccessRoleDomain> roleList = accessEntryDomain.getAccessRoleList();
				Map<String, AccessRoleDomain> roles = new HashMap<String, AccessRoleDomain>();
				Map<String, AccessRoleDomain> accessRoleMap = new HashMap<String, AccessRoleDomain>();

				if (roleList != null) {
					for (AccessRoleDomain accessRole : roleList) {
						// アクセス権限タイプ
						AccessRoleTypeDomain accessRoleType = accessRole.getType();

						// 書込み権限
						if (LBL_WRITE.equals(accessRoleType.getDefinitionName())) {
							accessRoleMap.put(accessRoleType.getDefinitionName(), accessRole);
						}
						// ステータス変更
						if (LBL_STATUS_CHANGE.equals(accessRoleType.getDefinitionName())) {
							accessRoleMap.put(accessRoleType.getDefinitionName(), accessRole);
						}
						// 常時読取
						if (LBL_ALWAYS_READ.equals(accessRoleType.getDefinitionName())) {
							accessRoleMap.put(accessRoleType.getDefinitionName(), accessRole);
						}
						// 公開読取
						if (LBL_PUBLIC_READ.equals(accessRoleType.getDefinitionName())) {
							accessRoleMap.put(accessRoleType.getDefinitionName(), accessRole);
						}
					}
				}

				// 書き込み権限
				AccessRoleDomain writeAccessRole = accessRoleMap.get(DocumentAccessRoleTypeEnum.WRITE.getValue());
				// ステータス変更権限
				AccessRoleDomain statusChangeAccessRole = accessRoleMap.get(DocumentAccessRoleTypeEnum.STATUS_CHANGE.getValue());
				// 常時読み取り権限
				AccessRoleDomain alwaysReadAccessRole = accessRoleMap.get(DocumentAccessRoleTypeEnum.ALWAYS_READ.getValue());
				// 読み取り権限
				AccessRoleDomain publicReadAccessRole = accessRoleMap.get(DocumentAccessRoleTypeEnum.PUBLIC_READ.getValue());

				// パーミッションの整合性をチェック
				// 書き込み権限が許可の場合
				if (writeAccessRole.getPermission().equals(AccessRolePermissionModeEnum.ACCEPT)) {
					// 下位は許可でなければならない
					if (hasPermission(Arrays.asList(statusChangeAccessRole, alwaysReadAccessRole, publicReadAccessRole), AccessRolePermissionModeEnum.DENY)) {
						throw new EIMException("EIM.ERROR.LOGIC.UNJUST.COMBINATION.ROLES");
					}
					// 下位は許可でなければならない
					if (hasPermission(new ArrayList<AccessRoleDomain>(Arrays.asList(statusChangeAccessRole, alwaysReadAccessRole, publicReadAccessRole)), AccessRolePermissionModeEnum.IGNORE)) {
						throw new EIMException("EIM.ERROR.LOGIC.UNJUST.COMBINATION.ROLES");
					}
				}

				// 書き込み権限が無視の場合
				if (writeAccessRole.getPermission().equals(AccessRolePermissionModeEnum.IGNORE)) {
					// 下位が拒否の場合は無視でなければならない
					if (hasPermission(new ArrayList<AccessRoleDomain>(Arrays.asList(statusChangeAccessRole, alwaysReadAccessRole, publicReadAccessRole)), AccessRolePermissionModeEnum.DENY)) {
						throw new EIMException("EIM.ERROR.LOGIC.UNJUST.COMBINATION.ROLES");
					}
				}

				// ステータス変更権限が許可の場合
				if (statusChangeAccessRole.getPermission().equals(AccessRolePermissionModeEnum.ACCEPT)) {
					// 下位は許可でなければならない
					if (hasPermission(Arrays.asList(alwaysReadAccessRole, publicReadAccessRole), AccessRolePermissionModeEnum.DENY)) {
						throw new EIMException("EIM.ERROR.LOGIC.UNJUST.COMBINATION.ROLES");
					}
					// 下位は許可でなければならない
					if (hasPermission(Arrays.asList(alwaysReadAccessRole, publicReadAccessRole), AccessRolePermissionModeEnum.IGNORE)) {
						throw new EIMException("EIM.ERROR.LOGIC.UNJUST.COMBINATION.ROLES");
					}
				}

				// ステータス変更権限が拒否の場合
				if (statusChangeAccessRole.getPermission().equals(AccessRolePermissionModeEnum.DENY)) {
					// 上位は拒否でなければならない
					if (hasPermission(Arrays.asList(writeAccessRole), AccessRolePermissionModeEnum.ACCEPT)) {
						throw new EIMException("EIM.ERROR.LOGIC.UNJUST.COMBINATION.ROLES");
					}
					// 上位は拒否でなければならない
					if (hasPermission(Arrays.asList(writeAccessRole), AccessRolePermissionModeEnum.IGNORE)) {
						throw new EIMException("EIM.ERROR.LOGIC.UNJUST.COMBINATION.ROLES");
					}
				}

				// ステータス変更権限が無視の場合
				if (statusChangeAccessRole.getPermission().equals(AccessRolePermissionModeEnum.IGNORE)) {
					// 上位が許可の場合は無視でなければならない
					if (hasPermission(Arrays.asList(writeAccessRole), AccessRolePermissionModeEnum.ACCEPT)) {
						throw new EIMException("EIM.ERROR.LOGIC.UNJUST.COMBINATION.ROLES");
					}
					// 下位が拒否の場合は無視でなければならない
					if (hasPermission(new ArrayList<AccessRoleDomain>(Arrays.asList(alwaysReadAccessRole, publicReadAccessRole)), AccessRolePermissionModeEnum.DENY)) {
						throw new EIMException("EIM.ERROR.LOGIC.UNJUST.COMBINATION.ROLES");
					}
				}

				// 常時読み取り権限が許可の場合
				if (alwaysReadAccessRole.getPermission().equals(AccessRolePermissionModeEnum.ACCEPT)) {
					// 下位は許可でなければならない
					if (hasPermission(Arrays.asList(publicReadAccessRole), AccessRolePermissionModeEnum.DENY)) {
						throw new EIMException("EIM.ERROR.LOGIC.UNJUST.COMBINATION.ROLES");
					}
					// 下位は許可でなければならない
					if (hasPermission(Arrays.asList(publicReadAccessRole), AccessRolePermissionModeEnum.IGNORE)) {
						throw new EIMException("EIM.ERROR.LOGIC.UNJUST.COMBINATION.ROLES");
					}
				}

				// 常時読み取り権限が拒否の場合
				if (alwaysReadAccessRole.getPermission().equals(AccessRolePermissionModeEnum.DENY)) {
					// 上位は拒否でなければならない
					if (hasPermission(Arrays.asList(writeAccessRole, statusChangeAccessRole), AccessRolePermissionModeEnum.ACCEPT)) {
						throw new EIMException("EIM.ERROR.LOGIC.UNJUST.COMBINATION.ROLES");
					}
					// 上位は拒否でなければならない
					if (hasPermission(Arrays.asList(writeAccessRole, statusChangeAccessRole), AccessRolePermissionModeEnum.IGNORE)) {
						throw new EIMException("EIM.ERROR.LOGIC.UNJUST.COMBINATION.ROLES");
					}
				}

				// 常時読み取り権限が無視の場合
				if (alwaysReadAccessRole.getPermission().equals(AccessRolePermissionModeEnum.IGNORE)) {
					// 上位が許可の場合は無視でなければならない
					if (hasPermission(Arrays.asList(writeAccessRole, statusChangeAccessRole), AccessRolePermissionModeEnum.ACCEPT)) {
						throw new EIMException("EIM.ERROR.LOGIC.UNJUST.COMBINATION.ROLES");
					}
					// 下位が拒否の場合は無視でなければならない
					if (hasPermission(new ArrayList<AccessRoleDomain>(Arrays.asList(publicReadAccessRole)), AccessRolePermissionModeEnum.DENY)) {
						throw new EIMException("EIM.ERROR.LOGIC.UNJUST.COMBINATION.ROLES");
					}
				}

				// 公開読み取り権限が拒否の場合
				if (publicReadAccessRole.getPermission().equals(AccessRolePermissionModeEnum.DENY)) {
					// 上位は拒否でなければならない
					if (hasPermission(Arrays.asList(writeAccessRole, statusChangeAccessRole, alwaysReadAccessRole), AccessRolePermissionModeEnum.ACCEPT)) {
						throw new EIMException("EIM.ERROR.LOGIC.UNJUST.COMBINATION.ROLES");
					}
					// 上位は拒否でなければならない
					if (hasPermission(Arrays.asList(writeAccessRole, statusChangeAccessRole, alwaysReadAccessRole), AccessRolePermissionModeEnum.IGNORE)) {
						throw new EIMException("EIM.ERROR.LOGIC.UNJUST.COMBINATION.ROLES");
					}
				}

				// 公開読み取り権限が無視の場合
				if (publicReadAccessRole.getPermission().equals(AccessRolePermissionModeEnum.IGNORE)) {
					// 上位が許可の場合は無視でなければならない
					if (hasPermission(Arrays.asList(writeAccessRole, statusChangeAccessRole, alwaysReadAccessRole), AccessRolePermissionModeEnum.ACCEPT)) {
						throw new EIMException("EIM.ERROR.LOGIC.UNJUST.COMBINATION.ROLES");
					}
				}

				// DBに登録するアクセスロール情報を取得
				List<AccessRoleDomain> accessRoleList = createAccessRoleList(accessRoleMap);

				accessEntryDomain.setAccessRoleList(accessRoleList);
			}
		}
		return security;
	}

	/**
	 * 権限の組み合わせが正しいかチェックします。
	 * @param accessRoleList
	 * @param permission
	 * @return
	 */
	private static boolean hasPermission(List<AccessRoleDomain> accessRoleList, AccessRolePermissionModeEnum permission) {

		boolean result = false;

		for (int i=0; i<accessRoleList.size(); i++) {
			AccessRoleDomain accessRoleDomain =accessRoleList.get(i);
			if (accessRoleDomain.getPermission().equals(permission)) {
				result = true;
				break;
			}
		}

		return result;
	}

	/**
	 * ドキュメント用のアクセスロールからDB登録用のアクセスロールリストを生成します。
	 * @param accessRoleMap ドキュメント用のアクセスロール定義名称とアクセスロールのマップ
	 * @return DB登録用のアクセスロールリスト
	 * @throws Exception
	 */
	private static List<AccessRoleDomain> createAccessRoleList(Map<String, AccessRoleDomain> accessRoleMap) throws Exception {

		List<AccessRoleDomain> accessRoleList = new ArrayList<>();

		// 書き込み権限
		AccessRoleDomain writeAccessRole = accessRoleMap.get(DocumentAccessRoleTypeEnum.WRITE.getValue());
		if (!Objects.isNull(writeAccessRole)) {

			AccessRolePermissionModeEnum permission = writeAccessRole.getPermission();

			// UPDATE
			accessRoleList.add(createAccessRole("UPDATE", permission));
			// DELETE
			accessRoleList.add(createAccessRole("DELETE", permission));
			// RENAME
			accessRoleList.add(createAccessRole("RENAME", permission));
			// LOCK
			accessRoleList.add(createAccessRole("LOCK", permission));
			// UNLOCK
			accessRoleList.add(createAccessRole("UNLOCK", permission));
			// CHECKIN
			accessRoleList.add(createAccessRole("CHECKIN", permission));
			// CHECKOUT
			accessRoleList.add(createAccessRole("CHECKOUT", permission));
			// REVISION_UP
			accessRoleList.add(createAccessRole("REVISION_UP", permission));
			// CREATE_RELATION
			accessRoleList.add(createAccessRole("CREATE_RELATION", permission));
			// UPDATE_RELATION
			accessRoleList.add(createAccessRole("UPDATE_RELATION", permission));
			// DELETE_RELATION
			accessRoleList.add(createAccessRole("DELETE_RELATION", permission));
			// CREATE
			accessRoleList.add(createAccessRole("CREATE", permission));

		}

		// ステータス変更権限
		AccessRoleDomain statusChangeAccessRole = accessRoleMap.get(DocumentAccessRoleTypeEnum.STATUS_CHANGE.getValue());
		if (!Objects.isNull(statusChangeAccessRole)) {
			AccessRolePermissionModeEnum permission = statusChangeAccessRole.getPermission();

			// STATUS_UP
			accessRoleList.add(createAccessRole("STATUS_UP", permission));
			// STATUS_DOWN
			accessRoleList.add(createAccessRole("STATUS_DOWN", permission));
			// APPROVE
			accessRoleList.add(createAccessRole("APPROVE", permission));

		}

		// 常時読み取り権限
		AccessRoleDomain alwaysReadAccessRole = accessRoleMap.get(DocumentAccessRoleTypeEnum.ALWAYS_READ.getValue());
		if (!Objects.isNull(alwaysReadAccessRole)) {
			AccessRolePermissionModeEnum permission = alwaysReadAccessRole.getPermission();

			// ROLE_500
			accessRoleList.add(createAccessRole("ROLE_500", permission));
		}

		// 読み取り権限
		AccessRoleDomain publicReadAccessRole = accessRoleMap.get(DocumentAccessRoleTypeEnum.PUBLIC_READ.getValue());
		if (!Objects.isNull(publicReadAccessRole)) {
			AccessRolePermissionModeEnum permission = publicReadAccessRole.getPermission();
			// READ
			accessRoleList.add(createAccessRole("READ", permission));
		}

		return accessRoleList;
	}

	/**
	 * アクセスロールリストを生成します。
	 * @param accessRoleDefinitionNameList 生成するアクセスロールの定義名称リスト
	 * @param permission アクセス権限許可モード列挙型
	 * @return アクセスロールリスト
	 * @throws Exception
	 */
	private static AccessRoleDomain createAccessRole(
			String accessRoleDefinitionName,
			AccessRolePermissionModeEnum permission) throws Exception {

		List<AccessRoleDomain> accessRoleList = new ArrayList<>();

		ApplicationContext context = ApplicationContextLoader.getApplicationContext();
		AccessRoleTypeService accessRoleTypeService = (AccessRoleTypeService) context.getBean("accessRoleTypeService2");

		AccessRoleDomain accessRoleDomain = new AccessRoleDomain();
		AccessRoleTypeDomain accessRoleType = accessRoleTypeService.getByDefinitionName(accessRoleDefinitionName);
		// アクセスロールタイプを設定
		accessRoleDomain.setType(accessRoleType);
		// 許可モード(拒否/許可/無視)を設定
		accessRoleDomain.setPermission(permission);
		accessRoleList.add(accessRoleDomain);

		return accessRoleDomain;
	}

	/**
	 * アクセスロールをドキュメント管理用に変換したセキュリティ情報に変換します。
	 *
	 * @param securityDomain セキュリティ情報
	 * @return
	 * @throws Exception
	 */
	public static SecurityDomain convertOriginalSecurityToDocumentSecurity(SecurityDomain securityDomain) throws Exception {
		if (securityDomain == null)
			return null;

		SecurityDomain security = securityDomain.clone();

		// アクセスエントリー一覧を取得
		List<AccessEntryDomain> accessEntryList = security.getAccessEntryList();

		if (accessEntryList == null) {
			return null;
		}

		for (AccessEntryDomain accessEntryDomain : accessEntryList) {

			// アクセス権限一覧
			List<AccessRoleDomain> roleList = accessEntryDomain.getAccessRoleList();
			if (roleList == null) {
				continue;
			}

			Map<String, AccessRoleDomain> documentAccessRoleMap = new HashMap<>();

			for (AccessRoleDomain accessRole : roleList) {

				// アクセス権限タイプ
				AccessRoleTypeDomain accessRoleType = accessRole.getType();

				if ("CREATE".equals(accessRoleType.getDefinitionName())) {
					// 作成権限;
					AccessRoleDomain _accessRole = new AccessRoleDomain();
					AccessRoleTypeDomain _accessRoleType = new AccessRoleTypeDomain();
					_accessRole.setPermission(accessRole.getPermission());
					_accessRole.setType(_accessRoleType);
					_accessRoleType.setDefinitionName(DocumentAccessRoleTypeEnum.WRITE.toString());

					documentAccessRoleMap.put(DocumentAccessRoleTypeEnum.WRITE.toString(), _accessRole);
				}
				if ("STATUS_UP".equals(accessRoleType.getDefinitionName())) {
					// ステータス変更
					AccessRoleDomain _accessRole = new AccessRoleDomain();
					AccessRoleTypeDomain _accessRoleType = new AccessRoleTypeDomain();
					_accessRole.setPermission(accessRole.getPermission());
					_accessRole.setType(_accessRoleType);
					_accessRoleType.setDefinitionName(DocumentAccessRoleTypeEnum.STATUS_CHANGE.toString());

					documentAccessRoleMap.put(DocumentAccessRoleTypeEnum.STATUS_CHANGE.toString(), _accessRole);
				}
				if ("ROLE_500".equals(accessRoleType.getDefinitionName())) {
					// 常時読取
					AccessRoleDomain _accessRole = new AccessRoleDomain();
					AccessRoleTypeDomain _accessRoleType = new AccessRoleTypeDomain();
					_accessRole.setPermission(accessRole.getPermission());
					_accessRole.setType(_accessRoleType);
					_accessRoleType.setDefinitionName(DocumentAccessRoleTypeEnum.ALWAYS_READ.toString());

					documentAccessRoleMap.put(DocumentAccessRoleTypeEnum.ALWAYS_READ.toString(), _accessRole);
				}
				if ("READ".equals(accessRoleType.getDefinitionName())) {
					// 読み取り
					AccessRoleDomain _accessRole = new AccessRoleDomain();
					AccessRoleTypeDomain _accessRoleType = new AccessRoleTypeDomain();
					_accessRole.setPermission(accessRole.getPermission());
					_accessRole.setType(_accessRoleType);
					_accessRoleType.setDefinitionName(DocumentAccessRoleTypeEnum.PUBLIC_READ.toString());

					documentAccessRoleMap.put(DocumentAccessRoleTypeEnum.PUBLIC_READ.toString(), _accessRole);
				}
			}

			List<AccessRoleDomain> documentAccessRoleList = new ArrayList<>();
			documentAccessRoleList.add(documentAccessRoleMap.get(DocumentAccessRoleTypeEnum.WRITE.toString()));
			documentAccessRoleList.add(documentAccessRoleMap.get(DocumentAccessRoleTypeEnum.STATUS_CHANGE.toString()));
			documentAccessRoleList.add(documentAccessRoleMap.get(DocumentAccessRoleTypeEnum.ALWAYS_READ.toString()));
			documentAccessRoleList.add(documentAccessRoleMap.get(DocumentAccessRoleTypeEnum.PUBLIC_READ.toString()));

			accessEntryDomain.setAccessRoleList(documentAccessRoleList);
		}
		return security;
	}

	/**
	 * 対象のアクセスエントリーのステータス別セキュリティを設定します。
	 * 設定するステータスは、公開済以外のステータス
	 * ・アクセス権限が公開読取のみ許可の場合、上記のステータス別セキュリティを拒否
	 * ・アクセス権限がその他の設定の場合、ステータス別セキュリティを無視に設定
	 *
	 * @param sec セキュリティ
	 * @param workFlow 更新対象のワークフロー
	 * @param entry アクセスエントリー
	 * @throws Exception
	 */
	public static void updateStatusSecurityForDocBySecWfAce(SecurityDomain sec, WorkflowDomain workFlow, AccessEntryDomain entry) throws Exception {

		ApplicationContext context = ApplicationContextLoader.getApplicationContext();
		AccessRoleTypeService accessRoleTypeService = (AccessRoleTypeService) context.getBean("accessRoleTypeService2");
		AccessEntryService accessEntryService = (AccessEntryService) context.getBean("accessEntryService2");
		SecurityService securityService = (SecurityService) context.getBean("securityService2");

		AccessEntryDomain accessEntry = accessEntryService.getById(entry.getId());
		if (accessEntry == null) {
			return;
		}
		List<AccessRoleDomain> defaultRoleList = accessEntry.getAccessRoleList();

		// 公開読取のアクセス権限
		boolean isPublicRoleAccess = false;
		// 公開読取以外のアクセス権限
		boolean isOtherRoleAccess = false;
		for (AccessRoleDomain dfRole : defaultRoleList) {
			if ("READ".equals(dfRole.getType().getDefinitionName())) {
				if (dfRole.getPermission().getValue() == 1) {
					isPublicRoleAccess = true;
				}
			} else {
				if (dfRole.getPermission().getValue() == 1) {
					isOtherRoleAccess = true;
				}
			}
		}
		List<StatusTypeDomain> statusTypeList = workFlow.getStatusTypeList();
		AccessRoleTypeDomain roleType = accessRoleTypeService.getByDefinitionName("READ");
		for (StatusTypeDomain statustype : statusTypeList) {
			if (statustype.getBase() != null && statustype.getBase().getId() != AppConstant.STATUS_TYPE_KIND_ID_PUBLIC) {
				StatusSecurityDomain statusSec = securityService.getStatusSecurityBySecurityAndStatusType(sec, statustype);
				if (statusSec == null) {
					statusSec = new StatusSecurityDomain();
					statusSec.setStatusType(statustype);
					statusSec = securityService.createStatusSecurity(sec, statusSec);
				}
				// 公開読取のみ許可の場合
				if (isPublicRoleAccess && !isOtherRoleAccess) {
					accessEntryService.setAccessRole(statusSec, entry, roleType, AccessRolePermissionModeEnum.DENY);
				} else {
					accessEntryService.setAccessRole(statusSec, entry, roleType, AccessRolePermissionModeEnum.IGNORE);
				}
			}
		}
	}


	/**
	 * アクセス権限変更チェック
	 *
	 * @param accessEntry
	 * @param newAccessEntry
	 * @return
	 */
	public static boolean isRoleChanged(AccessEntryDomain accessEntry, AccessEntryDomain newAccessEntry) {
		List<AccessRoleDomain> roleList = accessEntry.getAccessRoleList();
		List<AccessRoleDomain> newRoleList = newAccessEntry.getAccessRoleList();

		// ソート
		Collections.sort(roleList, comparator);
		Collections.sort(newRoleList, comparator);

		boolean result = false;

		for (int i = 0; i < roleList.size(); i++) {
			AccessRoleDomain accessRoleDomain = roleList.get(i);
			AccessRoleDomain newAccessRoleDomain = newRoleList.get(i);

			// 権限変更の場合
			if (accessRoleDomain.getPermission() != newAccessRoleDomain.getPermission()) {
				result = true;
				break;
			}
		}
		return result;
	}

	/**
	 * アクセス権限ドメイン比較
	 */
	private static Comparator<AccessRoleDomain> comparator = new Comparator<AccessRoleDomain>() {
		public int compare(AccessRoleDomain o1, AccessRoleDomain o2) {
			return (int) (o1.getType().getId() - o2.getType().getId());
		}
	};
}
