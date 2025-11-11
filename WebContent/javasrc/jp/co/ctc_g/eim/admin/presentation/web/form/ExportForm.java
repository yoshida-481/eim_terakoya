package jp.co.ctc_g.eim.admin.presentation.web.form;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import jp.co.ctc_g.eim.admin.business.domain.ExportDomain;

/**
 * 管理ユーザフォーム
 * @since Ver3.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class ExportForm {

	/** 検索ユーザID*/
	private String searchUserCode;

	/** 検索ユーザ名*/
	private String searchUserName;

	/** 検索ユーザMail*/
	private String searchUserMail;

	/** 検索グループ名*/
	private String belongingGroupName;

	/** 検索グループ 下位を含むチェックボックス */
	private boolean includingChildGroup;

	/** 無効フラグラジオボタンの状態 */
	private int isNotDisplayInvalidityUser;
	private int isNotDisplayValidityUser;

	/** アプリケーション名*/
	private String appName;

	/** 出力日時*/
	private String date;

	/** ファイル名*/
	private String fileName;

	/** ホスト名*/
	private String hostName;



	/**
	 * 検索ユーザIDを取得します。
	 * @return 検索ユーザID
	 */
	public String getSearchUserCode() {
		return searchUserCode;
	}

	/**
	 * 検索ユーザIDを設定します。
	 * @param 検索ユーザID
	 */
	public void setSearchUserCode(String searchUserCode) {
		this.searchUserCode = searchUserCode;
	}

	/**
	 * 検索ユーザ名を取得します。
	 * @return 検索ユーザ名
	 */
	public String getSearchUserName() {
		return searchUserName;
	}

	/**
	 * 検索ユーザ名を設定します。
	 * @param 検索ユーザ名
	 */
	public void setSearchUserName(String searchUserName) {
		this.searchUserName = searchUserName;
	}

	/**
	 * 検索ユーザMailを取得します。
	 * @return 検索ユーザMail
	 */
	public String getSearchUserMail() {
		return searchUserMail;
	}

	/**
	 * 検索ユーザMailを設定します。
	 * @param 検索ユーザMail
	 */
	public void setSearchUserMail(String searchUserMail) {
		this.searchUserMail = searchUserMail;
	}

	/**
	 * 検索グループ名を取得します。
	 * @return 検索グループ名
	 */
	public String getBelongingGroupName() {
		return belongingGroupName;
	}

	/**
	 * 検索グループ名を設定します。
	 * @param 検索グループ名
	 */
	public void setBelongingGroupName(String belongingGroupName) {
		this.belongingGroupName = belongingGroupName;
	}

	/**
	 * 下位グループ包含フラグを取得します。
	 * @return 下位グループ包含フラグ
	 */
	public boolean getIncludingChildGroup() {
		return includingChildGroup;
	}

	/**
	 * 下位グループ包含フラグを設定します。
	 * @param 下位グループ包含フラグ
	 */
	public void setIncludingChildGroup(boolean includingChildGroup) {
		this.includingChildGroup = includingChildGroup;
	}

	/**
	 * 無効フラグの状態を取得します。
	 * @return 無効フラグの状態
	 */
	public int getIsNotDisplayInvalidityUser() {
		return isNotDisplayInvalidityUser;
	}

	/**
	 * 無効フラグの状態を設定します。
	 * @param 無効フラグの状態
	 */
	public void setIsNotDisplayInvalidityUser(int isNotDisplayInvalidityUser) {
		this.isNotDisplayInvalidityUser = isNotDisplayInvalidityUser;
	}

	/**
	 * 無効フラグの状態を取得します。
	 * @return 無効フラグの状態
	 */
	public int getIsNotDisplayValidityUser() {
		return isNotDisplayValidityUser;
	}

	/**
	 * 無効フラグの状態を設定します。
	 * @param 無効フラグの状態
	 */
	public void setIsNotDisplayValidityUser(int isNotDisplayValidityUser) {
		this.isNotDisplayValidityUser = isNotDisplayValidityUser;
	}

	/**
	 * アプリケーション名を取得します。
	 * @return アプリケーション名
	 */
	public String getAppName() {
		return appName;
	}

	/**
	 * アプリケーション名を設定します。
	 * @param アプリケーション名
	 */
	public void setAppName(String appName) {
		this.appName = appName;
	}

	/**
	 * 出力日時を取得します。
	 * @return 出力日時
	 */
	public String getDate() {
		return date;
	}

	/**
	 * 出力日時を設定します。
	 * @param 出力日時
	 */
	public void setDate(String date) {
		this.date = date;
	}

	/**
	 * ファイル名を取得します。
	 * @return ファイル名
	 */
	public String getFileName() {
		return fileName;
	}

	/**
	 * ファイル名を設定します。
	 * @param ファイル名
	 */
	public void setFileName(String fileName) {
		this.fileName = fileName;
	}

	/**
	 * ホスト名を取得します。
	 * @return ホスト名
	 */
	public String getHostName() {
		return hostName;
	}

	/**
	 * ホスト名を設定します。
	 * @param ホスト名
	 */
	public void setHostName(String hostName) {
		this.hostName = hostName;
	}

	/**
	 * ドメインに変換します。
	 * @param domainObject ドメイン
	 * @since Ver3.0
	 */
	public ExportDomain convert(Object domainObject) {
		ExportDomain domain = (ExportDomain)domainObject;

		domain.setSearchUserCode(this.searchUserCode);
		domain.setSearchUserName(this.searchUserName);
		domain.setSearchUserMail(this.searchUserMail);
		domain.setBelongingGroupName(this.belongingGroupName);
		domain.setIncludingChildGroup(this.includingChildGroup);
		domain.setIsNotDisplayInvalidityUser(this.isNotDisplayInvalidityUser);
		domain.setIsNotDisplayValidityUser(this.isNotDisplayValidityUser);
		domain.setAppName(this.appName);
		domain.setDate(this.date);
		domain.setFileName(this.fileName);
		domain.setHostName(this.hostName);

		return domain;
	}
}
