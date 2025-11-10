package jp.co.ctc_g.eim.app.document.business.service.search.plugin.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import common.util.AppConstant;
import eim.bo.EIMAttribute;
import eim.bo.EIMObject;
import eim.util.EIMConfig;
import jp.co.ctc_g.eim.app.document.common.enumeration.search.ContentSearchFieldEnum;
import jp.co.ctc_g.eim.search.core.indexBase.business.domain.FieldDomain;
import jp.co.ctc_g.eim.search.solr.indexBase.business.service.plugin.impl.SolrEIMObjectDataPlugInImpl;


/**
 * コンテンツ(ドキュメント/フォルダ/タグ)データの収集を行うプラグイン基底クラスです。
 */
public class BaseContentDataGatheringPlugInImpl extends SolrEIMObjectDataPlugInImpl {

	/** 有効期限属性の属性名 */
	protected final String EFFECT_DATE = EIMConfig.get("ATTR_NAME_DOCUMENT_EFFECT_DATE");

	/** プロパティ属性の属性名 */
	protected final String PROP = EIMConfig.get("ATTR_NAME_DOCUMENT_PROP");

	/** パス属性の属性名 */
	protected final String PATH = EIMConfig.get("ATTR_NAME_DOCUMENT_PASS");

	/**
	 * コンストラクタです。<br>
	 * ignoreAttributeTypeNameListシステム属性タイプを設定します。これはダイナミックフィールドの対象から除外されます。
	 */
	public BaseContentDataGatheringPlugInImpl() {
		super();

		// 拡張属性の設定の前にシステム属性は除外する
		Set<String> ignoreAttributeTypeNameSet = new HashSet<>();
		ignoreAttributeTypeNameSet.addAll(Arrays.asList(AppConstant.ESSENTIAL_ATTRIBUTE_DEFNAME));
		ignoreAttributeTypeNameSet.addAll(Arrays.asList(AppConstant.NONTABLE_ATTRIBUTE_DEFNAME));
		ignoreAttributeTypeNameList = new ArrayList<>(ignoreAttributeTypeNameSet);

	}

	/**
	 * コンテンツ(ドキュメント/フォルダ/タグ)のフィールド情報ドメインリストを生成して返却します。<br>
	 * ドキュメント/フォルダ/タグ共通の属性を追加設定しまう。
	 * @see jp.co.ctc_g.eim.search.solr.indexBase.business.service.plugin.impl.SolrEIMObjectDataPlugInImpl#getAdditionalFieldList(eim.bo.EIMObject, java.lang.Object)
	 */
	@Override
	protected List<FieldDomain> getAdditionalFieldList(EIMObject obj, Object additionalInfo) throws Exception {
		List<FieldDomain> fieldList = new ArrayList<>();

		// 有効期限
		EIMAttribute expirationDate = obj.getAttribute(EFFECT_DATE);
		if(expirationDate != null){
			fieldList.add(new FieldDomain(ContentSearchFieldEnum.EFFECTIVE_TERM.toString(), expirationDate.getDate()));
		}

		// プロパティ
		EIMAttribute attProperty = obj.getAttribute(PROP);
		if(attProperty != null){
			fieldList.add(new FieldDomain(ContentSearchFieldEnum.PROPERTY.toString(), attProperty.getString()));
		}

		// パス
		EIMAttribute attPathForSearch = obj.getAttribute(PATH);
		if(attPathForSearch != null){
			if(attPathForSearch.getStrings().length > 0){
				fieldList.add(new FieldDomain(ContentSearchFieldEnum.PATH.toString(), attPathForSearch.getStrings()[0]));
			}
		}

		// 拡張属性の設定
		fieldList.addAll(super.getAdditionalFieldList(obj, additionalInfo));

		return fieldList;
	}

}
