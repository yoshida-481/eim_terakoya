package jp.co.ctc_g.eim.app.document.business.domain.criteria;

import java.util.ArrayList;
import java.util.List;

import jp.co.ctc_g.eim.framework2.business.domain.criteria.MultipleCriteria;

/**
 * 【ドキュメントAPI】
 * ワークスペースの検索条件を保持します。
 * @since Ver 1.0
 */
public class WorkspaceCriteria {

	/**
	 * ワークスペースIDに対する検索条件<br>
	 */
	private MultipleCriteria<Long> ids;

	/**
	 * セッション言語名称に対する曖昧検索条件<br>
	 * 指定文字列内にワイルドカード（"*"、"?"）を含めることで曖昧検索を指示します。<br>
	 * エスケープ文字はありません。<br>
	 */
	private String name = null;

	/** 最大取得件数 */
	/** 取得件数を制限しない場合は0を指定します */
	private Integer limit = null;

	/** 最大取得件数を超えて取得した場合にEIMExceptionをスローするかどうかのフラグ */
	/** 最大取得件数を超えた取得した場合にEIMExceptionをスローする = true */
	private Boolean limitCondition = null;

	/**
	 * コンストラクタ。<br>
	 * プロパティ値は全てデフォルト値となります。
	 */
	public WorkspaceCriteria() {
	}

	/**
	 * ワークスペースIDに対する検索条件を取得します。<br>
	 * @return ワークスペースIDに対する検索条件
	 * @since Ver 1.0
	 */
	public MultipleCriteria<Long> getIds() {
	    return ids;
	}

	/**
	 *  ワークスペースIDに対する検索条件を設定します。<br>
	 * @param ids  ワークスペースIDに対する検索条件
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
	 * ワークスペース取得時における設定対象項目
	 */
	public enum WorkspaceItemEnum {

		/** フォルダリスト */
		FOLDER_LIST,
		/** ドキュメントリスト */
		DOCUMENT_LIST,
	}

}