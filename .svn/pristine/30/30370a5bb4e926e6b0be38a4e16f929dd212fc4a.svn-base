package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import jp.co.ctc_g.eim.app.document.business.service.DocumentSecurityService;
import jp.co.ctc_g.eim.app.document.common.util.DocumentSecurityUtil;
import jp.co.ctc_g.eim.app.document.common.util.OperationHistoryUtils;
import jp.co.ctc_g.eim.app.document.common.util.StringUtils;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.SecurityCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AccessEntryDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.SecurityDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.WorkflowDomain;
import jp.co.ctc_g.eim.framework2.business.service.WorkflowService;
import jp.co.ctc_g.eim.framework2.business.service.impl.SecurityServiceImpl;
import jp.co.ctc_g.eim.framework2.common.enumeration.EntryTypeEnum;
import jp.co.ctc_g.eim.framework2.common.exception.EIMException;
import jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.CompareUtils;
/**
 * 【ドキュメントAPI】
 * @see DocumentSecurityService
 */
public class DocumentSecurityServiceImpl extends SecurityServiceImpl implements DocumentSecurityService {

	/** ワークフローサービス */
	private WorkflowService workflowService = null;

	/**
	 * セキュリティIDを指定してセキュリティを取得します。
	 *
	 * @param id セキュリティID
	 * @return セキュリティドメイン
	 * @throws Exception
	 */
	public SecurityDomain getById(long id) throws Exception {
		if (id < 1) {
			return null;
		}
		SecurityDomain fwSecurityDomain = super.getById(id);
		return DocumentSecurityUtil.convertOriginalSecurityToDocumentSecurity(fwSecurityDomain);
	}

	/**
	 * セキュリティ名を指定してセキュリティを取得します。
	 *
	 * @param name セキュリティ名
	 * @return セキュリティドメイン
	 * @throws Exception
	 */
	public SecurityDomain getByDefinitionName(String name) throws Exception {
		if (StringUtils.isEmpty(name)) {
			throw new EIMException("EIM.ERROR.LOGIC.NAME.VALUE.ILLEGAL");
		}
		SecurityDomain fwSecurityDomain = super.getByDefinitionName(name);
		return DocumentSecurityUtil.convertOriginalSecurityToDocumentSecurity(fwSecurityDomain);
	}

	/**
	 * セキュリティクライテリア指定してセキュリティを取得します。
	 *
	 * @param criteria セキュリティクライテリア
	 * @return セキュリティドメインリスト
	 * @throws Exception
	 */
	public List<SecurityDomain> getList(SecurityCriteria criteria) throws Exception {
		if (criteria == null) {
			// 引数criteriaとしてnullが入力されました。
			throw new EIMException("EIM.ERROR.LOGIC.CRITERIA.VALUE.ILLEGAL");
		}
		List<SecurityDomain> fwSecurityDomainList = super.getList(criteria);
		List<SecurityDomain> securityDomainList = null;
		if (fwSecurityDomainList != null) {
			securityDomainList = new ArrayList<SecurityDomain>();
			for (SecurityDomain fwSecurityDomain : fwSecurityDomainList) {
				securityDomainList.add(DocumentSecurityUtil.convertOriginalSecurityToDocumentSecurity(fwSecurityDomain));
			}
		}
		return securityDomainList;
	}

	/**
	 * セキュリティを登録します。
	 *
	 * @param domain セキュリティドメイン
	 * @return セキュリティドメイン
	 * @throws Exception
	 */
	public SecurityDomain create(SecurityDomain domain) throws Exception {
		if (domain == null) {
			// 引数domainとしてnullが入力されました。
			throw new EIMException("EIM.ERROR.LOGIC.OBJECT.VALUE.ILLEGAL");
		}
		// 権限の組み合わせをチェック
		SecurityDomain fwDomain = DocumentSecurityUtil.convertDocumentSecurityToOriginalSecurity(domain);
		SecurityDomain secDomain = super.create(fwDomain);

		//操作履歴 セキュリティ作成
		OperationHistoryUtils.createOperationHistory(null, 1, 1041, 1, 9, secDomain.getId(), secDomain.getDefinitionName(),
													-1, -1, -1, null, null);

		List<AccessEntryDomain> accessEntryList = secDomain.getAccessEntryList();
		Iterator<AccessEntryDomain> ite = accessEntryList.iterator();
		while(ite.hasNext()){
			AccessEntryDomain accessEntryDomain = ite.next();
			int opeTypeId = -1;
			if(accessEntryDomain.getEntryType().equals(EntryTypeEnum.USER)){
				opeTypeId = 1037;
			}else if(accessEntryDomain.getEntryType().equals(EntryTypeEnum.GROUP)){
				opeTypeId = 1038;
			}else if(accessEntryDomain.getEntryType().equals(EntryTypeEnum.ROLE)){
				opeTypeId = 1039;
			}else if(accessEntryDomain.getEntryType().equals(EntryTypeEnum.COMPLEX)){
				opeTypeId = 1040;
			}

			//操作履歴 エントリ登録
			OperationHistoryUtils.createOperationHistory(null, 1, opeTypeId, 6, 9, secDomain.getId(), secDomain.getDefinitionName(),
														7, 10, accessEntryDomain.getId(), accessEntryDomain.getEntryElement().getName(), null);
		}

		return DocumentSecurityUtil.convertOriginalSecurityToDocumentSecurity(secDomain);
	}

	/**
	 * セキュリティを更新します。
	 *
	 * @param domain セキュリティドメイン
	 * @throws Exception
	 */
	public void update(SecurityDomain domain) throws Exception {
		if (domain == null) {
			// 引数domainとしてnullが入力されました。
			throw new EIMException("EIM.ERROR.LOGIC.OBJECT.VALUE.ILLEGAL");
		}
		// 権限の組み合わせをチェック
		SecurityDomain fwDomain = DocumentSecurityUtil.convertDocumentSecurityToOriginalSecurity(domain);

		// 古いセキュリティを取得
		SecurityDomain oldSecDomain = super.getById(fwDomain.getId());
		oldSecDomain = DocumentSecurityUtil.convertOriginalSecurityToDocumentSecurity(oldSecDomain);
		// old_list
		List<AccessEntryDomain> oldAccesEntryList = oldSecDomain.getAccessEntryList();

		super.update(fwDomain);

		List<WorkflowDomain> workflowList = workflowService.getList();
		if (workflowList != null) {
			List<AccessEntryDomain> entryList = fwDomain.getAccessEntryList();
			for (AccessEntryDomain entry : entryList) {
				for (WorkflowDomain workFlow : workflowList) {
					// 対象のアクセスエントリーのステータス別セキュリティを設定する
					DocumentSecurityUtil.updateStatusSecurityForDocBySecWfAce(fwDomain, workFlow, entry);
				}
			}
		}

		fwDomain = super.getById(fwDomain.getId());
		domain = DocumentSecurityUtil.convertOriginalSecurityToDocumentSecurity(fwDomain);
		// new_list
		List<AccessEntryDomain> newAccesEntryList = domain.getAccessEntryList();

		int opeTypeId = -1;
		// 操作履歴 セキュリティ更新
		opeTypeId = 1086;
		OperationHistoryUtils
				.createOperationHistory(null, 1, opeTypeId, 3, 9, fwDomain.getId(), fwDomain.getDefinitionName(), -1, -1, -1, null, null);

		// 削除リスト
		List<AccessEntryDomain> delList = (List<AccessEntryDomain>) CompareUtils.getMinuses(oldAccesEntryList, newAccesEntryList);
		for (AccessEntryDomain accessEntry : delList) {
			int operationTargetTypeB = -1;
			if (accessEntry.getEntryType().equals(EntryTypeEnum.USER)) {
				// エントリー削除(ユーザ)：EIMConstant.DELETE_USER_ENTRY
				opeTypeId = 1042;
				// ユーザ：EIMConstant.USER
				operationTargetTypeB = 10;
			} else if (accessEntry.getEntryType().equals(EntryTypeEnum.GROUP)) {
				// エントリー削除(グループ)：EIMConstant.DELETE_GROUP_ENTRY
				opeTypeId = 1043;
				// グループ：EIMConstant.GROUP
				operationTargetTypeB = 5;
			} else if (accessEntry.getEntryType().equals(EntryTypeEnum.ROLE)) {
				// エントリー削除(ロール)：EIMConstant.DELETE_ROLE_ENTRY
				opeTypeId = 1044;
				// ロール：EIMConstant.ROLE
				operationTargetTypeB = 7;
			} else if (accessEntry.getEntryType().equals(EntryTypeEnum.COMPLEX)) {
				// エントリー削除(複合グループ)：EIMConstant.DELETE_COMPLEX_GROUP_ENTRY
				opeTypeId = 1045;
				// 複合グループ：EIMConstant.COMP
				operationTargetTypeB = 2;
			}
			// 操作履歴 エントリー削除
			OperationHistoryUtils.createOperationHistory(null, 1, opeTypeId, 8, 9, fwDomain.getId(), fwDomain.getDefinitionName(), 9,
					operationTargetTypeB, accessEntry.getEntryElement().getId(), accessEntry.getEntryElement().getName(), null);
		}

		// 追加リスト
		List<AccessEntryDomain> addList = (List<AccessEntryDomain>) CompareUtils.getMinuses(newAccesEntryList, oldAccesEntryList);
		for (AccessEntryDomain accessEntry : addList) {
			int operationTargetTypeB = -1;
			if (accessEntry.getEntryType().equals(EntryTypeEnum.USER)) {
				// エントリー登録(ユーザ)：EIMConstant.REGIST_USER_ENTRY
				opeTypeId = 1037;
				// ユーザ：EIMConstant.USER
				operationTargetTypeB = 10;
			} else if (accessEntry.getEntryType().equals(EntryTypeEnum.GROUP)) {
				// エントリー登録(グループ)：EIMConstant.REGIST_GROUP_ENTRY
				opeTypeId = 1038;
				// グループ：EIMConstant.GROUP
				operationTargetTypeB = 5;
			} else if (accessEntry.getEntryType().equals(EntryTypeEnum.ROLE)) {
				// エントリー登録(ロール)：EIMConstant.REGIST_ROLE_ENTRY
				opeTypeId = 1039;
				// ロール：EIMConstant.ROLE
				operationTargetTypeB = 7;
			} else if (accessEntry.getEntryType().equals(EntryTypeEnum.COMPLEX)) {
				// エントリー登録(複合グループ)：EIMConstant.REGIST_COMPLEX_GROUP_ENTRY
				opeTypeId = 1040;
				// 複合グループ：EIMConstant.COMP
				operationTargetTypeB = 2;
			} else if (accessEntry.getEntryType().equals(EntryTypeEnum.USERDEFINITION)) {
				// エントリー登録(ユーザ定義グループ)：EIMConstant.REGIST_USER_DEF_GROUP_ENTRY
				opeTypeId = 1107;
				// ユーザ定義グループ：EIMConstant.USERDEF_GROUP
				operationTargetTypeB = 16;
			}
			// 操作履歴 エントリー登録
			OperationHistoryUtils.createOperationHistory(null, 1, opeTypeId, 6, 9, fwDomain.getId(), fwDomain.getDefinitionName(), 7,
					operationTargetTypeB, accessEntry.getEntryElement().getId(), accessEntry.getEntryElement().getName(), null);
		}

		// 更新リスト
		List<AccessEntryDomain> modList = (List<AccessEntryDomain>)CompareUtils.getIntersects(oldAccesEntryList, newAccesEntryList);
		Map<Long,AccessEntryDomain> entryMap = getEntryMapByList(newAccesEntryList);

		for (int i = 0; i < modList.size(); i++) {

			AccessEntryDomain accessEntry = modList.get(i);
			AccessEntryDomain newAccessEntry = entryMap.get(accessEntry.getId());

			int operationTargetTypeB = -1;
			int opeTypeIdPriority = -1;
			if (accessEntry.getEntryType().equals(EntryTypeEnum.USER)) {
				// エントリー権限更新(ユーザ)：EIMConstant.UPDATE_USER_ENTRY_AUTH
				opeTypeId = 1051;
				// ユーザ：EIMConstant.USER
				operationTargetTypeB = 10;
				// エントリー優先順位更新(ユーザ)：EIMConstant.UPDATE_USER_ENTRY_ORDER
				opeTypeIdPriority = 1047;
			} else if (accessEntry.getEntryType().equals(EntryTypeEnum.GROUP)) {
				// エントリー権限更新(グループ)：EIMConstant.UPDATE_GROUP_ENTRY_AUTH
				opeTypeId = 1052;
				// グループ：EIMConstant.GROUP
				operationTargetTypeB = 5;
				// エントリー優先順位更新(グループ)：EIMConstant.UPDATE_GROUP_ENTRY_ORDER
				opeTypeIdPriority = 1048;
			} else if (accessEntry.getEntryType().equals(EntryTypeEnum.ROLE)) {
				// エントリー権限更新(ロール)：EIMConstant.UPDATE_ROLE_ENTRY_AUTH
				opeTypeId = 1053;
				// ロール：EIMConstant.ROLE
				operationTargetTypeB = 7;
				// エントリー優先順位更新(ロール)：EIMConstant.UPDATE_ROLE_ENTRY_ORDER
				opeTypeIdPriority = 1049;
			} else if (accessEntry.getEntryType().equals(EntryTypeEnum.COMPLEX)) {
				// エントリー権限更新(複合グループ)：EIMConstant.UPDATE_COMPLEX_GROUP_ENTRY_AUTH
				opeTypeId = 1054;
				// 複合グループ：EIMConstant.COMP
				operationTargetTypeB = 2;
				// エントリー優先順位更新(複合グループ)：EIMConstant.UPDATE_COMPLEX_GROUP_ENTRY_ORDER
				opeTypeIdPriority = 1050;
			}
			if (DocumentSecurityUtil.isRoleChanged(accessEntry, newAccessEntry)) {
				// 操作履歴 エントリー権限更新
				OperationHistoryUtils.createOperationHistory(null, 1, opeTypeId, 27, 9, fwDomain.getId(), fwDomain.getDefinitionName(), 3,
						operationTargetTypeB, accessEntry.getEntryElement().getId(), accessEntry.getEntryElement().getName(), null);
			}

			if (accessEntry.getPriority() != newAccessEntry.getPriority()) {
				// 操作履歴 エントリー優先順位更新
				OperationHistoryUtils.createOperationHistory(null, 1, opeTypeIdPriority, 27, 9, fwDomain.getId(), fwDomain.getDefinitionName(), 3,
						operationTargetTypeB, accessEntry.getEntryElement().getId(), accessEntry.getEntryElement().getName(), null);
			}
		}
	}

	/**
	 * リストからアクセスエントリードメインのHashMapを生成します。
	 *
	 * @param newAccesEntryList
	 * @return
	 */
	private Map<Long, AccessEntryDomain> getEntryMapByList(List<AccessEntryDomain> newAccesEntryList) {
		Map<Long, AccessEntryDomain> entryMap = new HashMap<Long, AccessEntryDomain>();
		for (AccessEntryDomain entry : newAccesEntryList) {
			if (entry.getId() != 0) {
				entryMap.put(entry.getId(), entry);
			}
		}
		return entryMap;
	}

	/**
	 * セキュリティを削除します。
	 *
	 * @param domain セキュリティドメイン
	 * @throws Exception
	 */
	public void delete(SecurityDomain domain) throws Exception {
		if (domain == null) {
			// 引数domainとしてnullが入力されました。
			throw new EIMException("EIM.ERROR.LOGIC.OBJECT.VALUE.ILLEGAL");
		}
		super.delete(domain);
	}

	/**
	 * @return workflowService
	 */
	public WorkflowService getWorkflowService() {
		return workflowService;
	}

	/**
	 * @param workflowService セットします workflowService
	 */
	public void setWorkflowService(WorkflowService workflowService) {
		this.workflowService = workflowService;
	}

}
