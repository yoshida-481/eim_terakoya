package jp.co.ctc_g.eim.app.document.business.domain;

/*
 * 公開処理設定情報を格納するドメイン
 */
public class WorkFlowDocPDFSignatureDomain {
	/* 電子署名／セキュリティ設定を行う */
	String doSignAndSetSecurity = "";

	/* 電子署名するか・しないか */
	String doSignPDF = "";

	/* 承認日付挿入 */
	String insertApproveDate = "";

	/* 承認名挿入 */
	String insertApproveUser = "";

	/* 挿入ページ */
	String insertPage = "";

	/* 基準点 */
	String insertPlace	 = "";

	/* 基準点からX */
	String insertPlaceX	 = "";

	/* 基準点からY */
	String insertPlaceY	 = "";

	/* 電子署名用言語 */
	String approveNamelang	 = "";

	/* 署名用ジョブ名 */
	String signJobName	 = "";

	/* セキュリティセットするか・しないか */
	String doSetSecurity = "";

	/* セキュリティパスワード設定するか・しないか */
	String doSetSecurityPassword = "";

	/* セキュリティパスワード値 */
	String securityPassword = "";

	/* 参照パスワード設定するか・しないか */
	String doSetReferencePassword = "";

	/* 参照パスワード値 */
	String referencePassword = "";

	/* 印刷許可しない */
	String forbidPrint = "";

	/* 編集許可しない */
	String forbidEdit = "";

	/* 注釈追加許可しない */
	String forbidAnnotate = "";

	/* 転載許可しない */
	String forbidReproduce = "";

	public WorkFlowDocPDFSignatureDomain() {

	}
	public WorkFlowDocPDFSignatureDomain(
			String doSignAndSetSecurity,
			String doSignPDF,
			String insertApproveDate,
			String insertApproveUser,
			String insertPage,
			String insertPlace,
			String insertPlaceX,
			String insertPlaceY,
			String approveNamelang,
			String signJobName,
			String doSetSecurity,
			String doSetSecurityPassword,
			String securityPassword,
			String doSetReferencePassword,
			String referencePassword,
			String forbidPrint,
			String forbidEdit,
			String forbidAnnotate,
			String forbidReproduce) {

		this.doSignAndSetSecurity = doSignAndSetSecurity;
		this.doSignPDF = doSignPDF;
		this.insertApproveDate = insertApproveDate;
		this.insertApproveUser = insertApproveUser;
		this.insertPage = insertPage;
		this.insertPlace = insertPlace;
		this.insertPlaceX = insertPlaceX;
		this.insertPlaceY = insertPlaceY;
		this.approveNamelang = approveNamelang;
		this.signJobName = signJobName;
		this.doSetSecurity = doSetSecurity;
		this.doSetSecurityPassword = doSetSecurityPassword;
		this.securityPassword = securityPassword;
		this.doSetReferencePassword = doSetReferencePassword;
		this.referencePassword = referencePassword;
		this.forbidPrint = forbidPrint;
		this.forbidEdit = forbidEdit;
		this.forbidAnnotate = forbidAnnotate;
		this.forbidReproduce = forbidReproduce;
	}
	/**
	 * doSignAndSetSecurityを取得します。
	 * @return doSignAndSetSecurity
	 */
	public String getDoSignAndSetSecurity() {
	    return doSignAndSetSecurity;
	}
	/**
	 * doSignAndSetSecurityを設定します。
	 * @param doSignAndSetSecurity doSignAndSetSecurity
	 */
	public void setDoSignAndSetSecurity(String doSignAndSetSecurity) {
	    this.doSignAndSetSecurity = doSignAndSetSecurity;
	}
	/**
	 * doSignPDFを取得します。
	 * @return doSignPDF
	 */
	public String getDoSignPDF() {
	    return doSignPDF;
	}
	/**
	 * doSignPDFを設定します。
	 * @param doSignPDF doSignPDF
	 */
	public void setDoSignPDF(String doSignPDF) {
	    this.doSignPDF = doSignPDF;
	}
	/**
	 * insertApproveDateを取得します。
	 * @return insertApproveDate
	 */
	public String getInsertApproveDate() {
	    return insertApproveDate;
	}
	/**
	 * insertApproveDateを設定します。
	 * @param insertApproveDate insertApproveDate
	 */
	public void setInsertApproveDate(String insertApproveDate) {
	    this.insertApproveDate = insertApproveDate;
	}
	/**
	 * insertApproveUserを取得します。
	 * @return insertApproveUser
	 */
	public String getInsertApproveUser() {
	    return insertApproveUser;
	}
	/**
	 * insertApproveUserを設定します。
	 * @param insertApproveUser insertApproveUser
	 */
	public void setInsertApproveUser(String insertApproveUser) {
	    this.insertApproveUser = insertApproveUser;
	}
	/**
	 * insertPageを取得します。
	 * @return insertPage
	 */
	public String getInsertPage() {
	    return insertPage;
	}
	/**
	 * insertPageを設定します。
	 * @param insertPage insertPage
	 */
	public void setInsertPage(String insertPage) {
	    this.insertPage = insertPage;
	}
	/**
	 * insertPlaceを取得します。
	 * @return insertPlace
	 */
	public String getInsertPlace() {
	    return insertPlace;
	}
	/**
	 * insertPlaceを設定します。
	 * @param insertPlace insertPlace
	 */
	public void setInsertPlace(String insertPlace) {
	    this.insertPlace = insertPlace;
	}
	/**
	 * insertPlaceXを取得します。
	 * @return insertPlaceX
	 */
	public String getInsertPlaceX() {
	    return insertPlaceX;
	}
	/**
	 * insertPlaceXを設定します。
	 * @param insertPlaceX insertPlaceX
	 */
	public void setInsertPlaceX(String insertPlaceX) {
	    this.insertPlaceX = insertPlaceX;
	}
	/**
	 * insertPlaceYを取得します。
	 * @return insertPlaceY
	 */
	public String getInsertPlaceY() {
	    return insertPlaceY;
	}
	/**
	 * insertPlaceYを設定します。
	 * @param insertPlaceY insertPlaceY
	 */
	public void setInsertPlaceY(String insertPlaceY) {
	    this.insertPlaceY = insertPlaceY;
	}
	/**
	 * approveNamelangを取得します。
	 * @return approveNamelang
	 */
	public String getApproveNamelang() {
		return approveNamelang;
	}
	/**
	 * approveNamelangを設定します。
	 * @param approveNamelang approveNamelang
	 */
	public void setApproveNamelang(String approveNamelang) {
		this.approveNamelang = approveNamelang;
	}
	/**
	 * signJobNameを取得します。
	 * @return signJobName
	 */
	public String getSignJobName() {
		return signJobName;
	}
	/**
	 * signJobNameを設定します。
	 * @param signJobName signJobName
	 */
	public void setSignJobName(String signJobName) {
		this.signJobName = signJobName;
	}
	/**
	 * doSetSecurityを取得します。
	 * @return doSetSecurity
	 */
	public String getDoSetSecurity() {
	    return doSetSecurity;
	}
	/**
	 * doSetSecurityを設定します。
	 * @param doSetSecurity doSetSecurity
	 */
	public void setDoSetSecurity(String doSetSecurity) {
	    this.doSetSecurity = doSetSecurity;
	}
	/**
	 * doSetSecurityPasswordを取得します。
	 * @return doSetSecurityPassword
	 */
	public String getDoSetSecurityPassword() {
	    return doSetSecurityPassword;
	}
	/**
	 * doSetSecurityPasswordを設定します。
	 * @param doSetSecurityPassword doSetSecurityPassword
	 */
	public void setDoSetSecurityPassword(String doSetSecurityPassword) {
	    this.doSetSecurityPassword = doSetSecurityPassword;
	}
	/**
	 * securityPasswordを取得します。
	 * @return securityPassword
	 */
	public String getSecurityPassword() {
	    return securityPassword;
	}
	/**
	 * securityPasswordを設定します。
	 * @param securityPassword securityPassword
	 */
	public void setSecurityPassword(String securityPassword) {
	    this.securityPassword = securityPassword;
	}
	/**
	 * doSetReferencePasswordを取得します。
	 * @return doSetReferencePassword
	 */
	public String getDoSetReferencePassword() {
	    return doSetReferencePassword;
	}
	/**
	 * doSetReferencePasswordを設定します。
	 * @param doSetReferencePassword doSetReferencePassword
	 */
	public void setDoSetReferencePassword(String doSetReferencePassword) {
	    this.doSetReferencePassword = doSetReferencePassword;
	}
	/**
	 * referencePasswordを取得します。
	 * @return referencePassword
	 */
	public String getReferencePassword() {
	    return referencePassword;
	}
	/**
	 * referencePasswordを設定します。
	 * @param referencePassword referencePassword
	 */
	public void setReferencePassword(String referencePassword) {
	    this.referencePassword = referencePassword;
	}
	/**
	 * forbidPrintを取得します。
	 * @return forbidPrint
	 */
	public String getForbidPrint() {
	    return forbidPrint;
	}
	/**
	 * forbidPrintを設定します。
	 * @param forbidPrint forbidPrint
	 */
	public void setForbidPrint(String forbidPrint) {
	    this.forbidPrint = forbidPrint;
	}
	/**
	 * forbidEditを取得します。
	 * @return forbidEdit
	 */
	public String getForbidEdit() {
	    return forbidEdit;
	}
	/**
	 * forbidEditを設定します。
	 * @param forbidEdit forbidEdit
	 */
	public void setForbidEdit(String forbidEdit) {
	    this.forbidEdit = forbidEdit;
	}
	/**
	 * forbidAnnotateを取得します。
	 * @return forbidAnnotate
	 */
	public String getForbidAnnotate() {
	    return forbidAnnotate;
	}
	/**
	 * forbidAnnotateを設定します。
	 * @param forbidAnnotate forbidAnnotate
	 */
	public void setForbidAnnotate(String forbidAnnotate) {
	    this.forbidAnnotate = forbidAnnotate;
	}
	/**
	 * forbidReproduceを取得します。
	 * @return forbidReproduce
	 */
	public String getForbidReproduce() {
	    return forbidReproduce;
	}
	/**
	 * forbidReproduceを設定します。
	 * @param forbidReproduce forbidReproduce
	 */
	public void setForbidReproduce(String forbidReproduce) {
	    this.forbidReproduce = forbidReproduce;
	}




}
