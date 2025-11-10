package jp.co.ctc_g.eim.app.document.business.domain.criteria;

import java.util.ArrayList;
import java.util.List;

import jp.co.ctc_g.eim.app.document.business.domain.PlaceDomain;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.MultipleCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeDomain;

/**
 * 【ドキュメントAPI】
 * フォルダの検索条件を保持します。
 * @since Ver 1.0
 */
public class FolderCriteria {

	/**
	 * フォルダIDに対する検索条件<br>
	 */
	private MultipleCriteria<Long> ids;

	/**
	 * セッション言語名称に対する曖昧検索条件<br>
	 * 指定文字列内にワイルドカード（"*"、"?"）を含めることで曖昧検索を指示します。<br>
	 * エスケープ文字はありません。<br>
	 */
	private String name = null;

	/**
	 * フォルダタイプIDに対する検索条件<br>
	 */
	private Long objectTypeId;

	/** 最大取得件数 */
	/** 取得件数を制限しない場合は0を指定します */
	private Integer limit = null;

	/** 最大取得件数を超えて取得した場合にEIMExceptionをスローするかどうかのフラグ */
	/** 最大取得件数を超えた取得した場合にEIMExceptionをスローする = true */
	private Boolean limitCondition = null;


	/**属性検索リスト AND条件 */
	private List<AttributeDomain> attributeList = null;

	/** 場所情報(WorkspaceDomainもしくはFolderDomain） */
	private PlaceDomain place = null;

	/**
	 * コンストラクタ。<br>
	 * プロパティ値は全てデフォルト値となります。
	 */
	public FolderCriteria() {
	}

	/**
	 * フォルダIDに対する検索条件を取得します。<br>
	 * @return フォルダIDに対する検索条件
	 * @since Ver 1.0
	 */
	public MultipleCriteria<Long> getIds() {
	    return ids;
	}

	/**
	 *  フォルダIDに対する検索条件を設定します。<br>
	 * @param ids  フォルダIDに対する検索条件
	 * @since Ver 1.0
	 */
	public void setIds(MultipleCriteria<Long> ids) {
		List<Long> pseudoIdList = new ArrayList<Long>();
		for (int i=0;i<ids.size();i++){
			Number numId = ids.get(i);
			pseudoIdList.add(Long.valueOf(numId.toString()));
		}
		ids.clear();
		ids.addAll(pseudoIdList);
		this.ids = ids;
	}

	/**
	 * セッション言語名称に対する曖昧検索条件<br>を取得します。
	 * @return セッション言語名称に対する曖昧検索条件<br>
	 * @since Ver1.0
	 */
	public String getName() {
	    return name;
	}

	/**
	 * セッション言語名称に対する曖昧検索条件<br>を設定します。
	 * @param name セッション言語名称に対する曖昧検索条件<br>
	 * @since Ver1.0
	 */
	public void setName(String name) {
	    this.name = name;
	}

	/**
	 * フォルダタイプIDに対する検索条件を取得します。
	 * @return フォルダタイプIDに対する検索条件<br>
	 * @since Ver1.0
	 */
	public Long getObjectTypeId() {
	    return objectTypeId;
	}

	/**
	 * フォルダタイプIDに対する検索条件を設定します。
	 * @param objectTypeId フォルダタイプID<br>
	 * @since Ver1.0
	 */
	public void setObjectTypeId(Long objectTypeId) {
	    this.objectTypeId = objectTypeId;
	}

	/**
	 * 最大取得件数を取得します。
	 * @return 最大取得件数
	 * @since Ver1.0
	 */
	public Integer getLimit() {
	    return limit;
	}

	/**
	 * 最大取得件数を設定します。
	 * @param limit 最大取得件数
	 * @since Ver1.0
	 */
	public void setLimit(Integer limit) {
	    this.limit = limit;
	}

	/**
	 * 最大取得件数を超えて取得した場合にEIMExceptionをスローするかどうかのフラグを取得します。
	 * @return 最大取得件数を超えて取得した場合にEIMExceptionをスローするかどうかのフラグ
	 * @since Ver1.0
	 */
	public Boolean isLimitCondition() {
	    return limitCondition;
	}


	/**
	 * 最大取得件数を超えて取得した場合にEIMExceptionをスローするかどうかのフラグを設定します。
	 * @param limitCondition 最大取得件数を超えて取得した場合にEIMExceptionをスローするかどうかのフラグ
	 * @since Ver1.0
	 */
	public void setLimitCondition(Boolean limitCondition) {
	    this.limitCondition = limitCondition;
	}

	/**
	 * 検索対象の属性リストを取得します。
	 * @return 属性リスト
	 * @since Ver1.0
	 */
	public List<AttributeDomain> getAttributeList() {
	    return attributeList;
	}

	/**
	 * 検索対象の属性リストを設定します。
	 * @param attributeList 属性リスト
	 * @since Ver1.0
	 */
	public void setAttributeList(List<AttributeDomain> attributeList) {
	    this.attributeList = attributeList;
	}

	/**
	 * 検索する場所を取得する。
	 * @return place
	 * @since Ver1.0
	 */
	public PlaceDomain getPlace() {
		return place;
	}

	/**
	 * 検索する場所を設定する。
	 * @param place 場所情報
	 * @since Ver1.0
	 */
	public void setPlace(PlaceDomain place) {
		this.place = place;
	}

}