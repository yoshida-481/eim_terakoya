package jp.co.ctc_g.eim.app.document.business.domain;

import java.util.Map;

/**
 * PDF出力設定ドメイン
 *
 */
public class PDFSettingDomain {

	/** セキュリティタブ  編集を許可しない */
	private long forbidEdit = 0;
	
	/** 電子署名タブ  基準点から Y =  */
	private long insertPlaceY = 0;
	
	/** 電子署名タブ  基準点から X =  */
	private long insertPlaceX = 0;
	
	/** セキュリティタブ  参照用パスワード */
	private String referencePassword = null;
	
	/** セキュリティタブ  参照用パスワード設定フラグ */
	private boolean doSetReferencePassword = false;
	
	/** セキュリティタブ  セキュリティパスワード */
	private String securityPassword = null;
	
	/** セキュリティタブ  セキュリティパスワード設定フラグ */
	private boolean doSetSecurityPassword = false;
	
	/** 電子署名  転載を許可しない */
	private long forbidReproduce = 0;
	
	/** 電子署名  挿入ページ */
	private long insertPage = 0;
	
	/** 電子署名  承認日付挿入 */
	private long insertApproveDate = 0;
	
	/** 電子署名  挿入位置 基準点 */
	private long insertPlace = 0;
	
	/** 電子署名タブ  承認者名挿入 */
	private long insertApproveUser = 0;
	
	/** 全体  電子署名／セキュリティ設定を行う */
	private boolean doSignPDFAndSetSecurity = false;
	
	/** セキュリティタブ  注釈追加を許可しない */
	private long forbidAnnotate = 0;
	
	/** セキュリティタブ  印刷を許可しない */
	private long forbidPrint = 0;
	
	/** 電子署名タブ  する／しない */
	private boolean doSignPDF = false;
	
	/** URL挿入 する/しない */
	private boolean doInsertURLPDF = false;
	
	/** セキュリティタブ  する／しない */
	private boolean doSetSecurity = false;
	
	/** 承認依頼する時  画面上からPDF出力の設定をする/しない */
	private boolean localPDFOutputSet = false;


	/**
	 * コンストラクタ
	 */
	PDFSettingDomain(){
	 
	}
	/**
	 * コンストラクタ
	 */
	PDFSettingDomain(Map<String, Object> paramMap){
		//マップから値を取り出しフィールドにセットする
		
		if((String)paramMap.get("forbidEdit") != null ){
			this.forbidEdit = ((String)paramMap.get("forbidEdit")).equals("true") ? 1 : 0;
		}
		if(paramMap.get("insertPlaceY") != null){
			this.insertPlaceY			= !((String)paramMap.get("insertPlaceY")).equals("") ? Long.parseLong((String)paramMap.get("insertPlaceY")) : 0 ;
		}
		if(paramMap.get("insertPlaceX") != null){
			this.insertPlaceX			= !((String)paramMap.get("insertPlaceX")).equals("") ? Long.parseLong((String)paramMap.get("insertPlaceX")) : 0 ;
		}
		
		this.referencePassword		= (String)paramMap.get("referencePassword") != null ? (String)paramMap.get("referencePassword") : null ;
		
		if((String)paramMap.get("doSetReferencePassword") != null ){
			this.doSetReferencePassword = ((String)paramMap.get("doSetReferencePassword")).equals("true") ? true : false;
		}
		
		
		this.securityPassword		= (String)paramMap.get("securityPassword") != null ? (String)paramMap.get("securityPassword") : null ;
		
		if((String)paramMap.get("doSetSecurityPassword") != null ){
			this.doSetSecurityPassword = ((String)paramMap.get("doSetSecurityPassword")).equals("true") ? true : false;
		}
		
		
		if((String)paramMap.get("forbidReproduce") != null ){
			this.forbidReproduce = ((String)paramMap.get("forbidReproduce")).equals("true") ? 1 : 0;
		}
		
		if(paramMap.get("insertPage") != null){
			this.insertPage				= !((String)paramMap.get("insertPage")).equals("") ? Long.parseLong((String)paramMap.get("insertPage")) : 0 ;
		}
		
		if((String)paramMap.get("insertApproveDate") != null ){
			this.insertApproveDate = ((String)paramMap.get("insertApproveDate")).equals("true") ? 1 : 0;
		}
		
		if(paramMap.get("insertPlace") != null){
			this.insertPlace			= !((String)paramMap.get("insertPlace")).equals("") ? Long.parseLong((String)paramMap.get("insertPlace")) : 0 ;
		}
		
		if(paramMap.get("insertApproveUser") != null){
			this.insertApproveUser		= !((String)paramMap.get("insertApproveUser")).equals("") ? Long.parseLong((String)paramMap.get("insertApproveUser")) : 0 ;
		}
		
		if((String)paramMap.get("doSignPDFAndSetSecurity") != null ){
			this.doSignPDFAndSetSecurity = ((String)paramMap.get("doSignPDFAndSetSecurity")).equals("true") ? true : false;
		}
		
		
		if((String)paramMap.get("forbidAnnotate") != null ){
			this.forbidAnnotate = ((String)paramMap.get("forbidAnnotate")).equals("true") ? 1 : 0;
		}
		
		if((String)paramMap.get("forbidPrint") != null ){
			this.forbidPrint = ((String)paramMap.get("forbidPrint")).equals("true") ? 1 : 0;
		}
		
		
		if((String)paramMap.get("doSignPDF") != null ){
			this.doSignPDF = ((String)paramMap.get("doSignPDF")).equals("true") ? true : false;
		}
		
		
		if((String)paramMap.get("doSetSecurity") != null ){
			this.doSetSecurity = ((String)paramMap.get("doSetSecurity")).equals("true") ? true : false;
		}
		
		if((String)paramMap.get("localPDFOutputSet") != null ){
			this.localPDFOutputSet = ((String)paramMap.get("localPDFOutputSet")).equals("true") ? true : false;
		}
	}

	/**
	 * @return 編集許可設定（true:チェックあり／false:チェックなし）
	 */
	public boolean isForbidEdit() {
		return (forbidEdit == 1 ? true : false);
	}

	/**
	 * @param forbidEdit 編集許可設定
	 */
	public void setForbidEdit(long forbidEdit) {
		this.forbidEdit = forbidEdit;
	}

	/**
	 * @return 挿入位置座標Y
	 */
	public long getInsertPlaceY() {
		return insertPlaceY;
	}

	/**
	 * @param insertPlaceY 挿入位置座標Y
	 */
	public void setInsertPlaceY(long insertPlaceY) {
		this.insertPlaceY = insertPlaceY;
	}

	/**
	 * @return 挿入位置座標X
	 */
	public long getInsertPlaceX() {
		return insertPlaceX;
	}

	/**
	 * @param insertPlaceX 挿入位置座標X
	 */
	public void setInsertPlaceX(long insertPlaceX) {
		this.insertPlaceX = insertPlaceX;
	}

	/**
	 * @return 参照用パスワード
	 */
	public String getReferencePassword() {
		return referencePassword;
	}

	/**
	 * @param referencePassword 参照用パスワード
	 */
	public void setReferencePassword(String referencePassword) {
		this.referencePassword = referencePassword;
	}

	/**
	 * @return 参照用パスワード設定有無（true:設定されている／false:設定されていない）
	 */
	public boolean isDoSetReferencePassword() {
		return doSetReferencePassword;
	}

	/**
	 * @param doSetReferencePassword 参照用パスワード設定有無
	 */
	public void setDoSetReferencePassword(boolean doSetReferencePassword) {
		this.doSetReferencePassword = doSetReferencePassword;
	}

	/**
	 * @return セキュリティパスワード
	 */
	public String getSecurityPassword() {
		return securityPassword;
	}

	/**
	 * @param securityPassword セキュリティパスワード
	 */
	public void setSecurityPassword(String securityPassword) {
		this.securityPassword = securityPassword;
	}

	/**
	 * @return セキュリティパスワード設定有無（true:設定されている／false:設定されていない）
	 */
	public boolean isDoSetSecurityPassword() {
		return doSetSecurityPassword;
	}

	/**
	 * @param doSetSecurityPassword セキュリティパスワード設定有無
	 */
	public void setDoSetSecurityPassword(boolean doSetSecurityPassword) {
		this.doSetSecurityPassword = doSetSecurityPassword;
	}

	/**
	 * @return 転載許可設定（true:チェックあり／false:チェックなし）
	 */
	public boolean isForbidReproduce() {
		return (forbidReproduce == 1 ? true : false);
	}

	/**
	 * @param forbidReproduce 転載許可設定
	 */
	public void setForbidReproduce(long forbidReproduce) {
		this.forbidReproduce = forbidReproduce;
	}

	/**
	 * @return 挿入ページ（0:前ページ／1:表紙のみ）
	 */
	public long getInsertPage() {
		return insertPage;
	}

	/**
	 * @param insertPage 挿入ページ
	 */
	public void setInsertPage(long insertPage) {
		this.insertPage = insertPage;
	}

	/**
	 * @return 承認日付挿入（0:承認日付挿入しない／1:承認日付挿入する）
	 */
	public long getInsertApproveDate() {
		return insertApproveDate;
	}

	/**
	 * @param insertApproveDate 承認日付挿入
	 */
	public void setInsertApproveDate(long insertApproveDate) {
		this.insertApproveDate = insertApproveDate;
	}

	/**
	 * @return 挿入位置基準点（0:右上／1:右下／2:左上／3:左下）
	 */
	public long getInsertPlace() {
		return insertPlace;
	}

	/**
	 * @param insertPlace 挿入位置基準点
	 */
	public void setInsertPlace(long insertPlace) {
		this.insertPlace = insertPlace;
	}

	/**
	 * @return 承認者名挿入（0:承認者名挿入しない／1:最終承認者のみ挿入／2:全承認者を挿入）
	 */
	public long getInsertApproveUser() {
		return insertApproveUser;
	}

	/**
	 * @param insertApproveUser 承認者名挿入
	 */
	public void setInsertApproveUser(long insertApproveUser) {
		this.insertApproveUser = insertApproveUser;
	}

	/**
	 * @return PDF署名実施フラグ（0:"off"／1:"on"）
	 */
	public boolean isDoSignPDFAndSetSecurity() {
		return doSignPDFAndSetSecurity;
	}

	/**
	 * @param doSignPDFAndSetSecurity PDF署名実施フラグ
	 */
	public void setDoSignPDFAndSetSecurity(boolean doSignPDFAndSetSecurity) {
		this.doSignPDFAndSetSecurity = doSignPDFAndSetSecurity;
	}

	/**
	 * @return 注釈追加許可設定（true:チェックあり／false:チェックなし）
	 */
	public boolean isForbidAnnotate() {
		return (forbidAnnotate == 1 ? true : false);
	}

	/**
	 * @param forbidAnnotate 注釈追加許可設定
	 */
	public void setForbidAnnotate(long forbidAnnotate) {
		this.forbidAnnotate = forbidAnnotate;
	}

	/**
	 * @return 印刷許可設定（true:チェックあり／false:チェックなし）
	 */
	public boolean isForbidPrint() {
		return (forbidPrint == 1 ? true : false);
	}

	/**
	 * @param forbitPrint 印刷許可設定
	 */
	public void setForbitPrint(long forbitPrint) {
		this.forbidPrint = forbitPrint;
	}

	/**
	 * @return 署名有無（0:電子署名しない／1:電子署名する）
	 */
	public boolean isDoSignPDF() {
		return doSignPDF;
	}
	
	/**
	 * @param doSignPDF 署名有無
	 */
	public void setDoSignPDF(boolean doSignPDF) {
		this.doSignPDF = doSignPDF;
	}

	/**
	 * @return セキュリティ設定有無（0:セキュリティ設定しない／1:セキュリティ設定する）
	 */
	public boolean isDoSetSecurity() {
		return doSetSecurity;
	}

	/**
	 * @param doSetSecurity セキュリティ設定有無
	 */
	public void setDoSetSecurity(boolean doSetSecurity) {
		this.doSetSecurity = doSetSecurity;
	}
	/**
	 * 承認依頼する時  画面上からPDF出力の設定をする/しないを取得します。
	 * @return 承認依頼する時  画面上からPDF出力の設定をする/しない
	 */
	public boolean isLocalPDFOutputSet() {
	    return localPDFOutputSet;
	}
	/**
	 * 承認依頼する時  画面上からPDF出力の設定をする/しないを設定します。
	 * @param localPDFOutputSet 承認依頼する時  画面上からPDF出力の設定をする/しない
	 */
	public void setLocalPDFOutputSet(boolean localPDFOutputSet) {
	    this.localPDFOutputSet = localPDFOutputSet;
	}

	/**
	 * @return 挿入有無（0:URL挿入しない／1:URL挿入する）
	 */
	public boolean isDoInsertURLPDF() {
		return doInsertURLPDF;
	}

	/**
	 * @param doInsertURLPDF 挿入有無
	 */
	public void setDoInsertURLPDF(boolean doInsertURLPDF) {
		this.doInsertURLPDF = doInsertURLPDF;
	}
	
}
