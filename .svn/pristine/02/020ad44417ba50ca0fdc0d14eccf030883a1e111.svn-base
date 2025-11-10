package jp.co.ctc_g.eim.app.document.business.service.search.plugin.impl;

import java.util.List;
import java.util.Set;

import eim.bo.EIMAttribute;
import eim.bo.EIMObject;
import eim.util.EIMConfig;
import jp.co.ctc_g.eim.app.document.business.service.search.plugin.util.ContentSearchFieldDomainHelper;
import jp.co.ctc_g.eim.search.core.indexBase.business.domain.FieldDomain;

/**
 * フォルダ情報の収集を行うプラグインクラスです。
 */
public class FolderDataGatheringPlugInImpl extends BaseContentDataGatheringPlugInImpl {

	/** 上位WFフォルダ属性の属性名 */
	private final String HIGHER_WF_FOLDER = EIMConfig.get("ATTR_NAME_FOLDER_HIGHER_WF_FOLDER");

	/**
	 * フォルダ固有の属性タイプに対するフィールド情報ドメインのリストを生成して返却します。
	 * @see jp.co.ctc_g.eim.app.document.business.service.search.plugin.impl.BaseContentDataGatheringPlugInImpl#getAdditionalFieldList(eim.bo.EIMObject, java.lang.Object)
	 */
	@Override
	protected List<FieldDomain> getAdditionalFieldList(EIMObject obj, Object additionalInfo) throws Exception {

		ContentSearchFieldDomainHelper fieldHelper = new ContentSearchFieldDomainHelper();

		// 共用情報は一括取得済みの空でないフォルダID一覧
		@SuppressWarnings("unchecked")
		Set<Long> notEmptyFolderIdSet = (Set<Long>) additionalInfo;
		boolean isNotEmpty = notEmptyFolderIdSet.contains(Long.valueOf(obj.getId()));
		fieldHelper.addFieldForEmptyFolder(isNotEmpty?0:1);

		// ワークフロー付きフォルダID
		EIMAttribute attWfAttachedFolder = obj.getAttribute(HIGHER_WF_FOLDER);
		if(attWfAttachedFolder != null){
			fieldHelper.addFieldForWfAttachedFolderId(attWfAttachedFolder.getInt());
		}

		// フィールドリストを取得
		List<FieldDomain> fieldList = fieldHelper.getFieldList();

		// 拡張属性をフィールドリストに追加
		fieldList.addAll(super.getAdditionalFieldList(obj, additionalInfo));

		return fieldList;
	}

}