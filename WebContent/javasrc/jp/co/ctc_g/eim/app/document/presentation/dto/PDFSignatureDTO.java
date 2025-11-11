package jp.co.ctc_g.eim.app.document.presentation.dto;

import org.springframework.util.StringUtils;

import jp.co.ctc_g.eim.app.document.business.domain.WorkFlowDocDomain;

/**
 * 他言語のIDと名前を管理するDTO
 * <p>
 * Struts->SpringMVCへの移行に際して、クライアントへ送信するXMLデータを再現したデータ転送用オブジェクトです。
 */
public class PDFSignatureDTO {

	/**
	 * クライアントへ送信していたXMLデータにおいて、単数値のプロパティを保持する親要素を再現するためのクラスです。
	 */
	public class Attr {

		/** PDF変換実施フラグ */
		private String doPDFConvert ="";

		/** URL挿入フラグ */
		private String doPDFURL = "";

		/** 参照パスワード設定するか・しないか */
		private String doSetReferencePassword = "";

		/** セキュリティセットするか・しないか */
		private String doSetSecurity = "";

		/** セキュリティパスワード設定するか・しないか */
		private String doSetSecurityPassword = "";

		/** 有効期限設定フラグ */
		private String doSetTerm = "";

		/** 電子署名／セキュリティ設定を行う */
		private String doSignAndSetSecurity = "";

		/** 電子署名するか・しないか */
		private String doSignPDF = "";

		/** 注釈追加許可しない */
		private String forbidAnnotate = "";

		/** 編集許可しない */
		private String forbidEdit = "";

		/** 印刷許可しない */
		private String forbidPrint = "";

		/** 転載許可しない */
		private String forbidReproduce = "";

		/** 承認日付挿入 */
		private String insertApproveDate = "";

		/** 承認名挿入 */
		private String insertApproveUser = "";

		/** 挿入ページ */
		private String insertPage = "";

		/** 基準点 */
		private String insertPlace	 = "";

		/** 基準点からX */
		private String insertPlaceX	 = "";

		/** 基準点からY */
		private String insertPlaceY	 = "";
		
		/** 署名言語 */
		private String approveNamelang	 = "";
		
		/** 署名ジョブ名 */
		private String signJobName	 = "";

		/** 参照パスワード値 */
		private String referencePassword = "";

		/** セキュリティパスワード値 */
		private String securityPassword = "";

		/** 有効期限設定期間数字 */
		private String termNumParam;

		/** 有効期限設定期間単位 */
		private String termUnitParam;
		
		private Attr(WorkFlowDocDomain workFlow) {
					
			doPDFConvert = String.valueOf(workFlow.isDoPDFConvert());
			doPDFURL = String.valueOf(workFlow.isDoPDFURL());
			doSetTerm = String.valueOf(workFlow.isDoSetTerm());	
			termNumParam = String.valueOf(workFlow.getTermNumParam());
			termUnitParam = String.valueOf(workFlow.getTermUnitParam());
			if (!StringUtils.isEmpty(workFlow.getSignatureCommand())) {
				doSetReferencePassword = workFlow.getSignatureCommand().getDoSetReferencePassword();
				doSetSecurity = workFlow.getSignatureCommand().getDoSetSecurity();
				doSetSecurityPassword = workFlow.getSignatureCommand().getDoSetSecurityPassword();
				doSignAndSetSecurity = workFlow.getSignatureCommand().getDoSignAndSetSecurity();
				doSignPDF = workFlow.getSignatureCommand().getDoSignPDF();
				forbidAnnotate = workFlow.getSignatureCommand().getForbidAnnotate();
				forbidEdit = workFlow.getSignatureCommand().getForbidEdit();
				forbidPrint = workFlow.getSignatureCommand().getForbidPrint();
				forbidReproduce = workFlow.getSignatureCommand().getForbidReproduce();
				insertApproveDate = workFlow.getSignatureCommand().getInsertApproveDate();
				insertApproveUser = workFlow.getSignatureCommand().getInsertApproveUser();
				insertPage = workFlow.getSignatureCommand().getInsertPage();
				insertPlace	 = workFlow.getSignatureCommand().getInsertPlace();
				insertPlaceX = workFlow.getSignatureCommand().getInsertPlaceX();
				insertPlaceY = workFlow.getSignatureCommand().getInsertPlaceY();
				approveNamelang = workFlow.getSignatureCommand().getApproveNamelang();
				signJobName = workFlow.getSignatureCommand().getSignJobName();
				referencePassword = "";workFlow.getSignatureCommand().getReferencePassword();
				securityPassword = workFlow.getSignatureCommand().getSecurityPassword();		
			}

		}

		public String getDoSetReferencePassword() {
			return doSetReferencePassword;
		}

		public void setDoSetReferencePassword(String doSetReferencePassword) {
			this.doSetReferencePassword = doSetReferencePassword;
		}

		public String getDoPDFConvert() {
			return doPDFConvert;
		}

		public void setDoPDFConvert(String doPDFConvert) {
			this.doPDFConvert = doPDFConvert;
		}

		public String getDoPDFURL() {
			return doPDFURL;
		}

		public void setDoPDFURL(String doPDFURL) {
			this.doPDFURL = doPDFURL;
		}

		public String getDoSetSecurity() {
			return doSetSecurity;
		}

		public void setDoSetSecurity(String doSetSecurity) {
			this.doSetSecurity = doSetSecurity;
		}

		public String getDoSetSecurityPassword() {
			return doSetSecurityPassword;
		}

		public void setDoSetSecurityPassword(String doSetSecurityPassword) {
			this.doSetSecurityPassword = doSetSecurityPassword;
		}

		public String getDoSetTerm() {
			return doSetTerm;
		}

		public void setDoSetTerm(String doSetTerm) {
			this.doSetTerm = doSetTerm;
		}

		public String getDoSignAndSetSecurity() {
			return doSignAndSetSecurity;
		}

		public void setDoSignAndSetSecurity(String doSignAndSetSecurity) {
			this.doSignAndSetSecurity = doSignAndSetSecurity;
		}

		public String getDoSignPDF() {
			return doSignPDF;
		}

		public void setDoSignPDF(String doSignPDF) {
			this.doSignPDF = doSignPDF;
		}

		public String getForbidAnnotate() {
			return forbidAnnotate;
		}

		public void setForbidAnnotate(String forbidAnnotate) {
			this.forbidAnnotate = forbidAnnotate;
		}

		public String getForbidEdit() {
			return forbidEdit;
		}

		public void setForbidEdit(String forbidEdit) {
			this.forbidEdit = forbidEdit;
		}

		public String getForbidPrint() {
			return forbidPrint;
		}

		public void setForbidPrint(String forbidPrint) {
			this.forbidPrint = forbidPrint;
		}

		public String getForbidReproduce() {
			return forbidReproduce;
		}

		public void setForbidReproduce(String forbidReproduce) {
			this.forbidReproduce = forbidReproduce;
		}

		public String getInsertApproveDate() {
			return insertApproveDate;
		}

		public void setInsertApproveDate(String insertApproveDate) {
			this.insertApproveDate = insertApproveDate;
		}

		public String getInsertApproveUser() {
			return insertApproveUser;
		}

		public void setInsertApproveUser(String insertApproveUser) {
			this.insertApproveUser = insertApproveUser;
		}

		public String getInsertPage() {
			return insertPage;
		}

		public void setInsertPage(String insertPage) {
			this.insertPage = insertPage;
		}

		public String getInsertPlace() {
			return insertPlace;
		}

		public void setInsertPlace(String insertPlace) {
			this.insertPlace = insertPlace;
		}

		public String getInsertPlaceX() {
			return insertPlaceX;
		}

		public void setInsertPlaceX(String insertPlaceX) {
			this.insertPlaceX = insertPlaceX;
		}

		public String getInsertPlaceY() {
			return insertPlaceY;
		}

		public void setInsertPlaceY(String insertPlaceY) {
			this.insertPlaceY = insertPlaceY;
		}
		
		public String getApproveNamelang() {
			return approveNamelang;
		}
		
		public void setApproveNamelang(String approveNamelang) {
			this.approveNamelang = approveNamelang;
		}
		
		public String getSignJobName() {
			return signJobName;
		}
		
		public void setSignJobName(String signJobName) {
			this.signJobName = signJobName;
		}

		public String getReferencePassword() {
			return referencePassword;
		}

		public void setReferencePassword(String referencePassword) {
			this.referencePassword = referencePassword;
		}

		public String getSecurityPassword() {
			return securityPassword;
		}

		public void setSecurityPassword(String securityPassword) {
			this.securityPassword = securityPassword;
		}

		public String getTermNumParam() {
			return termNumParam;
		}

		public void setTermNumParam(String termNumParam) {
			this.termNumParam = termNumParam;
		}

		public String getTermUnitParam() {
			return termUnitParam;
		}

		public void setTermUnitParam(String termUnitParam) {
			this.termUnitParam = termUnitParam;
		}

	}

	/**
	 * Strutsで生成したXMLデータにおいて単数値のプロパティを保持する親要素です。
	 */
	private Attr attr = null;

	/**
	 * コンストラクタ<br>
	 * SysFuncDomainが保持しているプロパティ値を設定します。<br>
	 *
	 * @param WorkFlowDocPDFSignatureDomain
	 */
	public PDFSignatureDTO(WorkFlowDocDomain workFlow) {
		setAttr(new Attr(workFlow));
	}

	/**
	 * @return
	 */
	public Attr getAttr() {
		return attr;
	}

	/**
	 * @param attr
	 */
	public void setAttr(Attr attr) {
		this.attr = attr;
	}

}
