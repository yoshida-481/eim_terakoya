package jp.co.ctc_g.eim.admin.business.domain.criteria;

import jp.co.ctc_g.eim.framework2.business.domain.criteria.UserCriteria;


/**
 * システム管理のユーザの検索条件を指定します。
 */
public class AdminUserCriteria extends UserCriteria {
	/**
	 * 所属グループ名に対する曖昧検索条件<br>
	 * 指定文字列内にワイルドカード（"*"、"?"）を含めることで曖昧検索を指示します。<br>
	 * エスケープ文字はありません。<br>
	 */
	private String groupName = null;
	
	/**
	 * 下位のグループを含めるか否かを指定する検索条件
	 * */
	private Boolean includingChildGroup = null;
	/**
	 * 所属グループ名に対する曖昧検索条件を取得します。
	 * @return groupName 所属グループ名に対する曖昧検索条件
	 */
	public String getGroupName() {
		return groupName;
	}

	/**
	 * 所属グループ名に対する曖昧検索条件を設定します。
	 * @param groupName 所属グループ名に対する曖昧検索条件
	 */
	public void setGroupName(String groupName) {
		this.groupName = groupName;
	}

	/**
	 * 下位のグループを含めるか否かを指定する検索条件を取得します。
	 * @return includingChildGroup 下位のグループを含めるか否かを指定する検索条件
	 */
	public Boolean getIncludingChildGroup() {
		return includingChildGroup;
	}

	/**
	 * 下位のグループを含めるか否かを指定する検索条件を設定します。
	 * @param includingChildGroup 下位のグループを含めるか否かを指定する検索条件
	 */
	public void setIncludingChildGroup(Boolean includingChildGroup) {
		this.includingChildGroup = includingChildGroup;
	}
}
