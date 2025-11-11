package jp.co.ctc_g.eim.app.document.business.service.search.plugin.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.UnaryOperator;
import java.util.stream.Collectors;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import common.util.AppObjectConditionHelper;
import eim.bo.EIMAccessRole;
import eim.bo.EIMAttribute;
import eim.bo.EIMAttributeType;
import eim.bo.EIMComp;
import eim.bo.EIMGroup;
import eim.bo.EIMObject;
import eim.bo.EIMRole;
import eim.bo.EIMSearchLimitCountCondition;
import eim.bo.EIMSearchResultList;
import eim.bo.EIMSearchSelectEIMObject;
import eim.bo.EIMSearchSelectEIMObject.PsedoAttributeTypeEnum;
import eim.bo.EIMUser;
import eim.net.EIMSession;
import eim.util.CompUtils;
import eim.util.EIMConfig;
import eim.util.SearchUtils;
import eim.util.TypeConvertUtils;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.business.dao.ComplexDao;
import jp.co.ctc_g.eim.framework2.business.dao.GroupDao;
import jp.co.ctc_g.eim.framework2.business.dao.RoleDao;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.ComplexCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.GroupCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.MultipleCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.RoleCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ComplexDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.GroupDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.RoleDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchLimitCountCondition;
import jp.co.ctc_g.eim.framework2.common.enumeration.EntryTypeEnum;
import jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils;
import jp.co.ctc_g.eim.search.app.document.business.service.plugin.impl.EIMDocumentSecurityPlugInImpl;
import jp.co.ctc_g.eim.search.core.common.business.domain.AccessControlDomain;
import jp.co.ctc_g.eim.search.core.indexBase.business.domain.AccessRoleListDomain;
import jp.co.ctc_g.eim.search.solr.indexBase.integration.dao.impl.SolrObjectIndexUpdateDaoImpl;

/**
 * ドキュメント用のセキュリティ取得プラグイン実装クラスです。<p>
 * ドキュメントリンクへの対応が実装されてます。
 */
public class DocumentSupportsLinkSecurityPlugInImpl extends EIMDocumentSecurityPlugInImpl {

	/** Logger */
	private static final Log log = LogFactory.getLog(SolrObjectIndexUpdateDaoImpl.class);

	/** グループDao */
	private GroupDao groupDao = null;

	/** ロールDao */
	private RoleDao roleDao = null;

	/** 複合グループDao */
	private ComplexDao complexDao = null;

	/** リンク先属性の属性名 */
	private final String TO_LINK = EIMConfig.get("ATTR_NAME_DOCUMENT_TARGET_TO_LINK");

	// -----------------
	// Pluglicメソッド
	// -----------------

	/**
	 * EIMオブジェクトに対するアクセス権限一覧ドメインのリストを取得します。<p>
	 * ここではリンクの親フォルダのアクセス権限一覧ドメインを取得するよう拡張実装が行われています。
	 * @see jp.co.ctc_g.eim.search.core.indexBase.eim.business.service.plugin.impl.GeneralEIMSecurityPlugInImpl#getSecurityList(java.util.List)
	 */
	@Override
	public List<AccessRoleListDomain> getSecurityList(List<EIMObject> objectList) throws Exception {

		// ------------------------------------------------
		// ドキュメントのアクセス権限一覧ドメインリスト取得
		// ------------------------------------------------

		List<AccessRoleListDomain> resultAccessRoleListDomainList = super.getSecurityList(objectList);

		// ------------------------------------------------
		// リンクの親フォルダのアクセス権限一覧ドメインリスト取得
		// ------------------------------------------------

		// リンクの親フォルダを取得
		Map<Long, List<Long>> parentIdToLinkIdListMap = new HashMap<>();
		for (EIMObject obj : objectList) {

			// リンク先属性を取得してIDを保管
			EIMAttribute toLinkAttr = obj.getAttribute(TO_LINK);
			if (toLinkAttr == null) {
				continue;
			}

			for (long parentId : toLinkAttr.getInts()) {
				if (!parentIdToLinkIdListMap.containsKey(parentId)){
					parentIdToLinkIdListMap.put(parentId, new ArrayList<>());
				}
				parentIdToLinkIdListMap.get(parentId).add((long) obj.getId());
			}
		}

		// リンクが無い場合はリターン
		if (parentIdToLinkIdListMap.size() == 0) {
			return resultAccessRoleListDomainList;
		}

		// Session
		EIMSession sess = EIMThreadContext.getEIMSession();

		EIMSearchSelectEIMObject selectTarget = new EIMSearchSelectEIMObject();

		// アクセス権限
		selectTarget.setRole(EIMAccessRole.NONE);

		// 取得項目
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
		List<EIMAttributeType> resultAttrs = Arrays.asList(
				PsedoAttributeTypeEnum.SECURITY,
				PsedoAttributeTypeEnum.STATUS,
				helper.getAttrType(EIMConfig.get("ATTR_NAME_DOCUMENT_PASS")));
		selectTarget.setResultAttrs(resultAttrs );

		// オブジェクトID配列取得
		long[] parentIds = parentIdToLinkIdListMap.keySet().stream().mapToLong(id -> id).toArray();

		// 検索条件の設定
		EIMSearchSelectEIMObject.SearchConditionBuildHelper h = new EIMSearchSelectEIMObject.SearchConditionBuildHelper();
		selectTarget.setCondition(h.group(h.opAnd())
				.addCondition(
						h.in(h.opAnd(),
								PsedoAttributeTypeEnum.ID,
								h.opIn(),
								TypeConvertUtils.convertToBuildTypeArray(parentIds))	// ID：検索対象のID(配列)
				)
		);

		// 検索の実行・取得件数は無制限
		EIMSearchResultList resultList = SearchUtils.searchObjects(sess, selectTarget,
				new EIMSearchLimitCountCondition(SearchLimitCountCondition.UNLIMITED, false));

		// リンクの親フォルダのアクセス権限一覧ドメインを取得して結果リストに追加
		@SuppressWarnings("unchecked")
		List<AccessRoleListDomain> parentAccessRoleListDomainList = super.getSecurityList(resultList);

		// ドキュメントとリンクの親フォルダのアクセス権限一覧ドメインを統合する
		AclMerge aclMerge = new AclMerge(resultAccessRoleListDomainList, parentAccessRoleListDomainList, parentIdToLinkIdListMap);
		resultAccessRoleListDomainList.addAll(aclMerge.getAccessRoleListDomainList());

		return resultAccessRoleListDomainList;
	}

	// --------------------
	// Private Innerクラス
	// --------------------

	/**
	 * ドキュメントと親フォルダのアクセス権限一覧ドメインをマージしてリンクのアクセス権限一覧ドメインを生成する。
	 */
	private class AclMerge {

		private Map<Long, GroupDomain> groupMap = null;

		private Map<Long, RoleDomain> roleMap = null;

		private Map<Long, ComplexDomain> complexMap = null;

		private List<AccessRoleListDomain> accessRoleListDomainList = new ArrayList<>();

		private List<AccessControlDomain> aclList = new ArrayList<>();

		private Set<String> acceptEntrySet = null;
		private Set<String> rejectEntrySet = null;

		/**
		 * コンストラクタです。<br>
		 * 前処理として引数で指定されたドキュメントが保持するアクセス権限一覧ドメインのリストと
		 * リンクの親フォルダが保持するアクセス権限一覧ドメインのリストからグループ、ロール、複合グループを一括取得します。
		 * その後、mergeAccessRoleList()を呼び出してアクセス権限一覧ドメインのマージ処理を行います。
		 * @param documentARListDomainList ドキュメントが保持するアクセス権限一覧ドメインのリスト
		 * @param parentARListDomainList リンクの親フォルダが保持するアクセス権限一覧ドメインのリスト
		 * @param parentIdToLinkIdListMap リンクの親フォルダ:List<リンクID>のMap
		 * @throws Exception
		 */
		private AclMerge(List<AccessRoleListDomain> documentARListDomainList,
				List<AccessRoleListDomain> parentARListDomainList, Map<Long, List<Long>> parentIdToLinkIdListMap)
				throws Exception {

			// グループ/ロール/複合グループIDリスト
			List<Long> groupIdList = new ArrayList<>();
			List<Long> roleIdList = new ArrayList<>();
			List<Long> complexIdList = new ArrayList<>();

			// ドキュメントのアクセス権限一覧ドメインからグループ/ロール/複合グループIDリストを取得
			for (AccessRoleListDomain documentAccessRoleListDomain : documentARListDomainList) {
				for (AccessControlDomain documentAcl : documentAccessRoleListDomain.getAccessControlList()) {
					if (documentAcl.getType() == EntryTypeEnum.GROUP.getValue()) {
						groupIdList.add(Long.parseLong(documentAcl.getId()));
					} else if (documentAcl.getType() == EntryTypeEnum.ROLE.getValue()) {
						roleIdList.add(Long.parseLong(documentAcl.getId()));
					} else if (documentAcl.getType() == EntryTypeEnum.COMPLEX.getValue()) {
						complexIdList.add(Long.parseLong(documentAcl.getId()));
					}
				}
			}

			// 親フォルダのアクセス権限一覧ドメインからグループ/ロール/複合グループIDリストを取得
			for (AccessRoleListDomain parentAccessRoleListDomain : parentARListDomainList) {
				for (AccessControlDomain parentAcl : parentAccessRoleListDomain.getAccessControlList()) {
					if (parentAcl.getType() == EntryTypeEnum.GROUP.getValue()) {
						groupIdList.add(Long.parseLong(parentAcl.getId()));
					} else if (parentAcl.getType() == EntryTypeEnum.ROLE.getValue()) {
						roleIdList.add(Long.parseLong(parentAcl.getId()));
					} else if (parentAcl.getType() == EntryTypeEnum.COMPLEX.getValue()) {
						complexIdList.add(Long.parseLong(parentAcl.getId()));
					}
				}
			}

			// グループリストの取得
			GroupCriteria groupCriteria = new GroupCriteria();
			MultipleCriteria<Long> groupIds = new MultipleCriteria<>(groupIdList);
			groupCriteria.setIds(groupIds);
			List<GroupDomain> groupList = groupDao.getList(groupCriteria);
			groupMap = groupList.stream().collect(Collectors.toMap(GroupDomain::getId, UnaryOperator.identity()));

			// ロールリストの取得
			RoleCriteria roleCriteria = new RoleCriteria();
			MultipleCriteria<Long> roleIds = new MultipleCriteria<>(roleIdList);
			roleCriteria.setIds(roleIds );
			List<RoleDomain> roleList = roleDao.getList(roleCriteria);
			roleMap = roleList.stream().collect(Collectors.toMap(RoleDomain::getId, UnaryOperator.identity()));

			// 複合グループリストの取得
			// ※ComplexDaoは子グループのユーザーを取得しないため、そこであ取得せずCompUtilsを使って取得したものを設定する
			ComplexCriteria complexCriteria = new ComplexCriteria();
			MultipleCriteria<Long> complexIds = new MultipleCriteria<>(complexIdList);
			complexCriteria.setIds(complexIds );
			List<ComplexDomain> complexList = complexDao.getList(complexCriteria);
			complexMap = complexList.stream().collect(Collectors.toMap(ComplexDomain::getId, UnaryOperator.identity()));

			// CompUtilsを使って所属ユーザを取得し設定
			// TODO V614では対応しないが、下記ComplexDao#getUserList()はグループ階層毎にクエリーを実行していて効率が悪い
			// ※SPのUserDao#getListByCriteria()にstart with - connect byを付加することで取得できるようになることは検証済み
			EIMSession sess = EIMThreadContext.getEIMSession();
			for (ComplexDomain complex : complexList) {
				EIMGroup group = new EIMGroup(complex.getGroup().getId(), null, null, null);
				EIMRole role = new EIMRole(complex.getRole().getId(), null, null, 0);
				EIMComp comp = new EIMComp(complex.getId(), group, role);
				@SuppressWarnings("unchecked")
				List<EIMUser> userList = CompUtils.getUserList(sess, comp);
				for (EIMUser eimUser : userList) {
					complex.getUserList().add(ConvertUtils.toUserDomain(eimUser));
				}
			}

			// ドキュメントのアクセス権限一覧ドメインをMapに保管
			Map<String, AccessRoleListDomain> documentARListDomainMap = documentARListDomainList.stream()
					.collect(Collectors.toMap(AccessRoleListDomain::getId, UnaryOperator.identity()));

			// ドキュメントのアクセス権限一覧ドメインと親フォルダのアクセス権限一覧ドメインをマージしてドキュメントリンクのアクセス権限一覧ドメインを生成
			for (AccessRoleListDomain parentARListDomain : parentARListDomainList) {
				List<Long> linkIdList = parentIdToLinkIdListMap.get(Long.valueOf(parentARListDomain.getId()));
				for (Long linkId : linkIdList) {
					AccessRoleListDomain mergedAclList = mergeAccessRoleList(documentARListDomainMap.get(String.valueOf(linkId)), parentARListDomain);
					accessRoleListDomainList.add(mergedAclList);
				}
			}
		}

		/**
		 * ドキュメントが保持するアクセス権限一覧ドメインとリンクの親フォルダが保持するアクセス権限一覧ドメインを比較し、マージします。<br>
		 * <ul>
		 * <li>許可エントリはAND条件で採用します。
		 * <li>拒否エントリはOR条件で採用します。
		 * </ul>
		 * リスト格納順に優先順位が設定されます。<br>
		 * 内容の比較はmergeAccessControl()に処理移譲しています。
		 * @param documentARListDomain ドキュメントが保持するアクセス権限一覧ドメイン
		 * @param parentARListDomain リンクの親フォルダが保持するアクセス権限一覧ドメイン
		 * @return マージしたアクセス権限情報ドメイン
		 * @throws Exception
		 */
		private AccessRoleListDomain mergeAccessRoleList(AccessRoleListDomain documentARListDomain, AccessRoleListDomain parentARListDomain) throws Exception {
			// アクセス権限一覧ドメインの生成 (ID:ドキュメントID_親フォルダID)
			AccessRoleListDomain resultDomain = new AccessRoleListDomain(documentARListDomain.getId() + "_" + parentARListDomain.getId());
			aclList = new ArrayList<>();
			acceptEntrySet = new HashSet<>();
			rejectEntrySet = new HashSet<>();
			resultDomain.setAccessControlList(aclList);

			// マージ対象の一時アクセス権限情報リスト
			List<AccessControlDomain> tmpDocumentAclList = new ArrayList<>(documentARListDomain.getAccessControlList());
			List<AccessControlDomain> tmpParentAclList = new ArrayList<>(parentARListDomain.getAccessControlList());

			// 多い方のアクセス権限情報の数分ループ
			int size = documentARListDomain.getAccessControlList().size() > parentARListDomain.getAccessControlList().size() ?
					documentARListDomain.getAccessControlList().size() : parentARListDomain.getAccessControlList().size();
			for (int count = 0; count < size; count ++) {
				// ドキュメントのアクセス権限情報とリンク親フォルダのアクセス権限情報を比較
				if (documentARListDomain.getAccessControlList().size() > count) {
					AccessControlDomain documentAcl = documentARListDomain.getAccessControlList().get(count);

					// アクセス権限情報をマージ
					if (mergeAccessControl(documentAcl, tmpParentAclList)) {
						// マージ済の場合は一時リストから削除
						tmpDocumentAclList.remove(documentAcl);
					}

				}

				// リンク親フォルダのアクセス権限情報とドキュメントのアクセス権限情報を比較
				if (parentARListDomain.getAccessControlList().size() > count) {
					AccessControlDomain parentAcl = parentARListDomain.getAccessControlList().get(count);

					// アクセス権限情報をマージ
					if (mergeAccessControl(parentAcl, tmpDocumentAclList)) {
						// マージ済の場合は一時リストから削除
						tmpParentAclList.remove(parentAcl);
					}
				}
			}

			return resultDomain;
		}

		/**
		 * アクセス権限情報と評価対象のアクセス権限情報リストを比較して、合致していればアクセス権限情報をエントリに追加します。<br>
		 * エントリタイプによって、エントリタイプ毎の評価メソッドに処理移譲します。<br>
		 * 拒否エントリがあった場合は即時にエントリに追加します。
		 * @param acl アクセス権限情報
		 * @param comparisonAclList 評価対象のアクセス権限情報リスト
		 * @return 評価が終了したか
		 * @throws Exception
		 */
		private boolean mergeAccessControl(AccessControlDomain acl, List<AccessControlDomain> comparisonAclList) throws Exception {

			if (acl.isPermit()) {
				// 許可
				if (acl.getType() == EntryTypeEnum.USER.getValue()) {
					// ユーザー
					return mergeUserAcl(acl, comparisonAclList);
				} else if (acl.getType() == EntryTypeEnum.GROUP.getValue()) {
					// グループ
					return mergeGroupAcl(acl, comparisonAclList);
				} else if (acl.getType() == EntryTypeEnum.ROLE.getValue()) {
					// ロール
					return mergeRoleAcl(acl, comparisonAclList);
				} else if (acl.getType() == EntryTypeEnum.COMPLEX.getValue()) {
					// 複合グループ
					return mergeComplexAcl(acl, comparisonAclList);
				} else {
					return true;
				}
			} else {
				// 拒否
				addAcl(acl);

				return true;
			}
		}


		/**
		 * ユーザーのアクセス権限情報と評価対象のアクセス権限情報リストを比較して、合致していればアクセス権限情報をエントリに追加します。<br>
		 * 評価対象のアクセス権限情報リストに拒否エントリがあった場合、評価未完了としてfalseを返却します。
		 * @param userAcl ユーザーのアクセス権限情報
		 * @param comparisonAclList 評価対象のアクセス権限情報リスト
		 * @return 評価が終了したか
		 * @throws Exception
		 */
		private boolean mergeUserAcl(AccessControlDomain userAcl, List<AccessControlDomain> comparisonAclList) throws NumberFormatException, Exception {

			for (AccessControlDomain comparisonAcl : comparisonAclList) {

				// 拒否エントリがあった場合はリターンして再評価
				if (!comparisonAcl.isPermit()) {
					return false;
				}

				// 比較対象のアクセス権限情報に含まれる場合はアクセス権限情報を追加
				if (comparisonAcl.getType() == EntryTypeEnum.USER.getValue()) {
					// 比較対象のエントリがユーザー
					if (userAcl.getId().equals(comparisonAcl.getId())) {
						addAcl(userAcl);
						return true;
					}
				} else if (comparisonAcl.getType() == EntryTypeEnum.GROUP.getValue()) {
					// 比較対象のエントリがグループ
					GroupDomain comparisonGroup = groupMap.get(Long.parseLong(comparisonAcl.getId()));
					// 所属ユーザーの場合は追加
					if (isBelongingGroup(new UserDomain(Long.parseLong(userAcl.getId())), comparisonGroup)) {
						addAcl(userAcl);
						return true;
					}
				} else if (comparisonAcl.getType() == EntryTypeEnum.ROLE.getValue()) {
					// 比較対象のエントリがロール
					RoleDomain comparisonRole = roleMap.get(Long.parseLong(comparisonAcl.getId()));
					// 所属ユーザーの場合は追加
					if (isBelongingRole(new UserDomain(Long.parseLong(userAcl.getId())), comparisonRole)) {
						addAcl(userAcl);
						return true;
					}
				} else if (comparisonAcl.getType() == EntryTypeEnum.COMPLEX.getValue()) {
					// 比較対象のエントリが複合グループ
					ComplexDomain comparisonComplex = complexMap.get(Long.parseLong(comparisonAcl.getId()));
					// 所属ユーザーの場合は追加
					if (isBelongingComplex(new UserDomain(Long.parseLong(userAcl.getId())), comparisonComplex)) {
						addAcl(userAcl);
						return true;
					}
				}
			}

			return true;
		}

		/**
		 * グループのアクセス権限情報と評価対象のアクセス権限情報リストを比較して、合致していればアクセス権限情報をエントリに追加します。<br>
		 * 評価対象のアクセス権限情報リストに拒否エントリがあった場合、評価未完了としてfalseを返却します。
		 * @param groupAcl グループのアクセス権限情報
		 * @param comparisonAclList 評価対象のアクセス権限情報リスト
		 * @return 評価が終了したか
		 * @throws Exception
		 */
		private boolean mergeGroupAcl(AccessControlDomain groupAcl, List<AccessControlDomain> comparisonAclList) throws Exception {

			for (AccessControlDomain comparisonAcl : comparisonAclList) {

				// 拒否エントリがあった場合はリターンして再評価
				if (!comparisonAcl.isPermit()) {
					return false;
				}

				if (comparisonAcl.getType() == EntryTypeEnum.USER.getValue()) {
					// 比較対象のエントリがユーザー
					GroupDomain group = groupMap.get(Long.parseLong(groupAcl.getId()));
					// 所属ユーザーの場合は追加
					if (isBelongingGroup(new UserDomain(Long.parseLong(comparisonAcl.getId())), group)) {
						addAcl(comparisonAcl);
					}
				} else if (comparisonAcl.getType() == EntryTypeEnum.GROUP.getValue()) {
					// 比較対象のエントリがグループ
					if (groupAcl.getId().equals(comparisonAcl.getId())) {
						// IDが一致
						addAcl(groupAcl);
					} else {
						// 所属グループの場合は追加
						GroupDomain comparisonGroup = groupMap.get(Long.parseLong(comparisonAcl.getId()));
						if (isBelongingGroup(new GroupDomain(Long.parseLong(groupAcl.getId())), comparisonGroup)) {
							// エントリが比較対象のエントリの所属グループの場合は子を追加
							addAcl(groupAcl);
						}

						// 所属グループの場合は追加
						GroupDomain group = groupMap.get(Long.parseLong(groupAcl.getId()));
						if (isBelongingGroup(new GroupDomain(Long.parseLong(comparisonAcl.getId())), group)) {
							// エントリが比較対象のエントリの所属グループの場合は子を追加
							addAcl(comparisonAcl);
						}
					}
				}
			}

			return true;
		}

		/**
		 * ロールのアクセス権限情報と評価対象のアクセス権限情報リストを比較して、合致していればアクセス権限情報をエントリに追加します。<br>
		 * 評価対象のアクセス権限情報リストに拒否エントリがあった場合、評価未完了としてfalseを返却します。
		 * @param roleAcl ロールのアクセス権限情報
		 * @param comparisonAclList 評価対象のアクセス権限情報リスト
		 * @return 評価が終了したか
		 * @throws Exception
		 */
		private boolean mergeRoleAcl(AccessControlDomain roleAcl, List<AccessControlDomain> comparisonAclList) throws Exception {

			for (AccessControlDomain comparisonAcl : comparisonAclList) {

				// 拒否エントリがあった場合はリターンして再評価
				if (!comparisonAcl.isPermit()) {
					return false;
				}

				if (comparisonAcl.getType() == EntryTypeEnum.USER.getValue()) {
					// 比較対象のエントリがユーザー
					RoleDomain role = roleMap.get(Long.parseLong(roleAcl.getId()));
					// 所属ユーザーの場合は追加
					if (isBelongingRole(new UserDomain(Long.parseLong(comparisonAcl.getId())), role)) {
						addAcl(comparisonAcl);
					}
				} else if (comparisonAcl.getType() == EntryTypeEnum.ROLE.getValue()) {
					// 比較対象のエントリがロール
					if (roleAcl.getId().equals(comparisonAcl.getId())) {
						// IDが一致
						addAcl(roleAcl);
					}
				} else {

				}
			}

			return true;
		}

		/**
		 * 複合グループのアクセス権限情報と評価対象のアクセス権限情報リストを比較して合致していればアクセス権限情報をエントリに追加します。<br>
		 * 評価対象のアクセス権限情報リストに拒否エントリがあった場合、評価未完了としてfalseを返却します。
		 * @param complexAcl 複合グループのアクセス権限情報
		 * @param comparisonAclList 評価対象のアクセス権限情報リスト
		 * @return 評価が終了したか
		 * @throws Exception
		 */
		private boolean mergeComplexAcl(AccessControlDomain complexAcl, List<AccessControlDomain> comparisonAclList) throws Exception {

			for (AccessControlDomain comparisonAcl : comparisonAclList) {

				// 拒否エントリがあった場合はリターンして再評価
				if (!comparisonAcl.isPermit()) {
					return false;
				}

				if (comparisonAcl.getType() == EntryTypeEnum.USER.getValue()) {
					// 比較対象のエントリがユーザー
					ComplexDomain complex = complexMap.get(Long.parseLong(complexAcl.getId()));
					// 所属ユーザーの場合は追加
					if (isBelongingComplex(new UserDomain(Long.parseLong(comparisonAcl.getId())), complex)) {
						addAcl(comparisonAcl);
					}
				} else if (comparisonAcl.getType() == EntryTypeEnum.COMPLEX.getValue()) {
					// 比較対象のエントリが複合グループ
					if (complexAcl.getId().equals(comparisonAcl.getId())) {
						// IDが一致
						addAcl(complexAcl);
					}
				} else {

				}
			}

			return true;
		}

		/**
		 * ユーザーがグループに所属しているかを調べます。
		 * @param user ユーザー
		 * @param group グループ
		 * @return ユーザーがグループに所属しているか
		 */
		private boolean isBelongingGroup(UserDomain user, GroupDomain parentGroup) {

			for (UserDomain belongingUser : parentGroup.getUserList()) {
				if (belongingUser.getId() == user.getId()) {
					return true;
				}
			}

			for (GroupDomain belongingGroup : parentGroup.getChildList()) {
				if (isBelongingGroup(user, belongingGroup)) {
					return true;
				}
			}

			return false;
		}

		/**
		 * グループが親グループの配下に属しているかを調べます。
		 * @param group グループ
		 * @param group 親グループ
		 * @return グループが親グループの配下に属しているか
		 */
		private boolean isBelongingGroup(GroupDomain group, GroupDomain parentGroup) {
			for (GroupDomain belongingGroup : parentGroup.getChildList()) {
				if (belongingGroup.getId() == group.getId()) {
					return true;
				}

				if (isBelongingGroup(group, belongingGroup)) {
					return true;
				}
			}

			return false;
		}

		/**
		 * ユーザーがロールに所属しているかを調べます。
		 * @param user ユーザー
		 * @param parentRole ロール
		 * @return ユーザーがロールに所属しているか
		 */
		private boolean isBelongingRole(UserDomain user, RoleDomain parentRole) {
			for (UserDomain belongingUser : parentRole.getUserList()) {
				if (belongingUser.getId() == user.getId()) {
					return true;
				}
			}

			return false;
		}

		/**
		 * ユーザーが複合グループに所属しているかを調べます。
		 * @param user ユーザー
		 * @param parentComplex 複合グループ
		 * @return ユーザーが複合グループに所属しているか
		 */
		private boolean isBelongingComplex(UserDomain user, ComplexDomain parentComplex) {
			for (UserDomain belongingUser : parentComplex.getUserList()) {
				if (belongingUser.getId() == user.getId()) {
					return true;
				}
			}

			return false;
		}

		/**
		 * アクセス権限情報をエントリに追加します。
		 * @param acl アクセス権限情報
		 * @throws Exception
		 */
		private void addAcl(AccessControlDomain acl) throws Exception {
			if (acl.isPermit()) {
				// 許可
				if (acceptEntrySet.contains(acl.getId())) {
					// 設定済みの場合スキップ
					return;
				} else {
					acceptEntrySet.add(acl.getId());
				}
			} else if (!acl.isPermit()) {
				// 拒否
				if (rejectEntrySet.contains(acl.getId())) {
					// 設定済みの場合スキップ
					return;
				} else {
					rejectEntrySet.add(acl.getId());
				}
			}

			// アクセス権限情報を生成してリストに追加
			AccessControlDomain newAcl = new AccessControlDomain(acl.getId(), acl.getType(), acl.isPermit(), aclList.size() + 1);

			if (log.isDebugEnabled()) {
				log.debug(String.format("リンクアクセス権限情報追加:[ID:%s][TYPE:%s][PERMIT:%b][ORDER:%d]",
						newAcl.getId(), EntryTypeEnum.getByValue(newAcl.getType()), newAcl.isPermit(), newAcl.getOrder()));
			}

			aclList.add(newAcl);

		}

		/**
		 * 生成したアクセス権限一覧ドメインのリストを返却します。
		 * @return アクセス権限一覧ドメインのリスト
		 */
		private List<AccessRoleListDomain> getAccessRoleListDomainList() {
			return accessRoleListDomainList;
		}

	}

	// --------------
	// getter/setter
	// --------------

	/**
	 * グループDaoを取得します。
	 * @return グループDao
	 */
	public GroupDao getGroupDao() {
		return groupDao;
	}

	/**
	 * グループDaoを設定します。
	 * @param groupDao グループDao
	 */
	public void setGroupDao(GroupDao groupDao) {
		this.groupDao = groupDao;
	}

	/**
	 * ロールDaoを取得します。
	 * @return ロールDao
	 */
	public RoleDao getRoleDao() {
		return roleDao;
	}

	/**
	 * ロールDaoを設定します。
	 * @param roleDao ロールDao
	 */
	public void setRoleDao(RoleDao roleDao) {
		this.roleDao = roleDao;
	}

	/**
	 * 複合グループDaoを取得します。
	 * @return 複合グループDao
	 */
	public ComplexDao getComplexDao() {
		return complexDao;
	}

	/**
	 * 複合グループDaoを設定します。
	 * @param complexDao 複合グループDao
	 */
	public void setComplexDao(ComplexDao complexDao) {
		this.complexDao = complexDao;
	}

}
