package jp.co.ctc_g.eim.app.document.business.service.search.plugin.impl;

import java.util.List;

import eim.bo.EIMAttribute;
import eim.bo.EIMObject;
import eim.util.EIMConfig;
import jp.co.ctc_g.eim.app.document.business.service.search.plugin.util.ContentSearchFieldDomainHelper;
import jp.co.ctc_g.eim.search.core.indexBase.business.domain.FieldDomain;

/**
 * タグ用関連データ取得プラグインです。
 */
public class TagDataGatheringPlugInImpl extends BaseContentDataGatheringPlugInImpl {

	/** 署名・暗号化状態属性の属性名 */
	private final String SIGN_ENC_STATUS = EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_STATUS");

	/**
	 * タグ固有の属性タイプに対するフィールド情報ドメインを生成して返却します。
	 * @see jp.co.ctc_g.eim.app.document.business.service.search.plugin.impl.BaseContentDataGatheringPlugInImpl#getAdditionalFieldList(eim.bo.EIMObject, java.lang.Object)
	 */
	@Override
	protected List<FieldDomain> getAdditionalFieldList(EIMObject obj, Object additionalInfo) throws Exception {

		ContentSearchFieldDomainHelper fieldHelper = new ContentSearchFieldDomainHelper();

		// 署名・暗号化状態
		EIMAttribute attSignencr = obj.getAttribute(SIGN_ENC_STATUS);
		if(attSignencr != null){
			fieldHelper.addFieldForSignencr(attSignencr.getInt());
		}

		// フィールドリストを取得
		List<FieldDomain> fieldList = fieldHelper.getFieldList();

		// 拡張属性をフィールドリストに追加
		fieldList.addAll(super.getAdditionalFieldList(obj, additionalInfo));

		return fieldList;
	}

}