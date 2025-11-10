package jp.co.ctc_g.eim.app.document.presentation.dto.search;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * コンテンツ検索結果返却用コンテンツレコードを表すDTOです。
 */
public class ContentSearchRecordDTO {

	/** オブジェクトID */
	private Long objId;

	/** オブジェクトタイプ名 */
	private String objTypeName;

	/** オブジェクト名 */
	private String objName;

	/** 更新者名 */
	private String modifyUserName;

	/** 更新日時 */
	private Date modifyDateTime;

	/** 作成者名 */
	private String createUserName;

	/** 作成日時 */
	private Date createDateTime;

	/** 履歴 */
	private Integer rev;

	/** 最新履歴フラグ */
	private boolean isLatest = false;

	/** ドキュメントかどうか */
	private boolean isDocument = false;

	/** 公開済みフラグ */
	private boolean isPublished = false;

	/** PDFアイコン表示フラグ */
	private boolean isDspPdfIcon = false;

	/** WFなしドキュメントの公開アイコン表示フラグ */
	private boolean isDspPubIconForNoWF = false;

	/** ページアイコン表示フラグ */
	private boolean isDspPageIcon = false;

	// TODO actSearch.jspではOCR処理ステータス表示フラグ。dspChildObject.jspと用途が異なる？不要と思われるので削除した方がいい。
//	/** WFなしドキュメントの公開フラグ */
//	private boolean isNoWFPublic = false;

	/** OCR処理ステータス (クライアント側の実装に合わせてStringにする) */
	private String ocrProcessStatus;

	/** OCR結果ステータス (クライアント側の実装に合わせてStringにする) */
	private String ocrResultStatus;

	/** ステータスタイプ名 */
	private String statusTypeName;

	/** ステータス種別 */
	private Integer statusTypeKind;

	/** PDF変換ステータス */
	private Integer pdfConversionStatus;

	/** 公開PDFが事前登録されているかどうかのフラグ */
	private boolean isPdfPreRegistered;

	/** 上位階層ワークフロー付きフォルダ */
	private Long higherWFFolder;

	/** 有効期限切れかどうか */
	private boolean expiration = false;

	/** 署名・暗号化状態 */
	private Integer signencr;

	/** プロパティ */
	private String property;

	/** 公開のみ読取り可 */
	private boolean readOnly;

	/** ドキュメントリンクかどうか */
	private boolean isDocumentLink = false;

	/** ドキュメントリンク過去版通知 */
	private boolean isDspLatestLink = false;

	/** リンク更新タイミング (0:手動更新 1:公開時更新) */
	private Integer documentLinkUpdateTiming;

	/** ジャンプ対象リンク親フォルダID */
	private Long linkParentObjId;

	/** パス */
	private String path;

	/** WFフォルダかどうか */
	private boolean isWorkflowFolder = false;

	/** 上位階層ワークフロー付きフォルダステータス種別 */
	private Integer higherWFFolderStatusTypeKind;

	/** ロックユーザ */
	private String lockUserName;

	/** ロック日時 */
	private Date lockDate;

	/** 有効期限 */
	private Date effectiveTerm;

	/** ファイルサイズ */
	private Long fileSize;

	/** 番号(自動採番) */
	private String number;

	/** カスタム属性値を保持するMapです。 */
	private Map<String, Object> attr = null;

	/** 属性値スニペットです。 */
	private List<String> snippetAttributeList = null;

	/** ファイル全文スニペットです。 */
	private List<String> snippetFullTextList = null;

	// --------------------------
	// getterおよびsetter
	//---------------------------

	/**
	 * オブジェクトIDを取得します。
	 * @return オブジェクトID
	 */
	public Long getObjId() {
		return objId;
	}

	/**
	 * オブジェクトIDを設定します。
	 * @param objId オブジェクトID
	 */
	public void setObjId(Long objId) {
		this.objId = objId;
	}

	/**
	 * オブジェクトタイプ名を取得します。
	 * @return オブジェクトタイプ名
	 */
	public String getObjTypeName() {
		return objTypeName;
	}

	/**
	 * オブジェクトタイプ名を設定します。
	 * @param objTypeName オブジェクトタイプ名
	 */
	public void setObjTypeName(String objTypeName) {
		this.objTypeName = objTypeName;
	}

	/**
	 * オブジェクト名を取得します。
	 * @return オブジェクト名
	 */
	public String getObjName() {
		return objName;
	}

	/**
	 * オブジェクト名を設定します。
	 * @param objName オブジェクト名
	 */
	public void setObjName(String objName) {
		this.objName = objName;
	}

	/**
	 * 更新者名を取得します。
	 * @return 更新者名
	 */
	public String getModifyUserName() {
		return modifyUserName;
	}

	/**
	 * 更新者名を設定します。
	 * @param modifyUserName 更新者名
	 */
	public void setModifyUserName(String modifyUserName) {
		this.modifyUserName = modifyUserName;
	}

	/**
	 * 更新日時を取得します。
	 * @return 更新日時
	 */
	public Date getModifyDateTime() {
		return modifyDateTime;
	}

	/**
	 * 更新日時を設定します。
	 * @param modifyDateTime 更新日時
	 */
	public void setModifyDateTime(Date modifyDateTime) {
		this.modifyDateTime = modifyDateTime;
	}

	/**
	 * 作成者名を取得します。
	 * @return 作成者名
	 */
	public String getCreateUserName() {
		return createUserName;
	}

	/**
	 * 作成者名を設定します。
	 * @param createUserName 作成者名
	 */
	public void setCreateUserName(String createUserName) {
		this.createUserName = createUserName;
	}

	/**
	 * 作成日時を取得します。
	 * @return 作成日時
	 */
	public Date getCreateDateTime() {
		return createDateTime;
	}

	/**
	 * 作成日時を設定します。
	 * @param createDateTime 作成日時
	 */
	public void setCreateDateTime(Date createDateTime) {
		this.createDateTime = createDateTime;
	}

	/**
	 * 履歴を取得します。
	 * @return 履歴
	 */
	public Integer getRev() {
		return rev;
	}

	/**
	 * 履歴を設定します。
	 * @param rev 履歴
	 */
	public void setRev(Integer rev) {
		this.rev = rev;
	}

	/**
	 * 最新履歴フラグを取得します。
	 * @return 最新履歴フラグ
	 */
	public boolean getIsLatest() {
		return isLatest;
	}

	/**
	 * 最新履歴フラグを設定します。
	 * @param isLatest 最新履歴フラグ
	 */
	public void setIsLatest(boolean isLatest) {
		this.isLatest = isLatest;
	}

	/**
	 * ドキュメントかどうかを取得します。
	 * @return ドキュメントかどうか
	 */
	public boolean getIsDocument() {
		return isDocument;
	}

	/**
	 * ドキュメントかどうかを設定します。
	 * @param isDocument ドキュメントかどうか
	 */
	public void setIsDocument(boolean isDocument) {
		this.isDocument = isDocument;
	}

	/**
	 * 公開済みフラグを取得します。
	 * @return 公開済みフラグ
	 */
	public boolean getIsPublished() {
		return isPublished;
	}

	/**
	 * 公開済みフラグを設定します。
	 * @param isPublished 公開済みフラグ
	 */
	public void setIsPublished(boolean isPublished) {
		this.isPublished = isPublished;
	}

	/**
	 * PDFアイコン表示フラグを取得します。
	 * @return PDFアイコン表示フラグ
	 */
	public boolean getIsDspPdfIcon() {
		return isDspPdfIcon;
	}

	/**
	 * PDFアイコン表示フラグを設定します。
	 * @param isDspPdfIcon PDFアイコン表示フラグ
	 */
	public void setIsDspPdfIcon(boolean isDspPdfIcon) {
		this.isDspPdfIcon = isDspPdfIcon;
	}

	/**
	 * WFなしドキュメントの公開アイコン表示フラグを取得します。
	 * @return WFなしドキュメントの公開アイコン表示フラグ
	 */
	public boolean getIsDspPubIconForNoWF() {
		return isDspPubIconForNoWF;
	}

	/**
	 * WFなしドキュメントの公開アイコン表示フラグを設定します。
	 * @param isDspPubIconForNoWF WFなしドキュメントの公開アイコン表示フラグ
	 */
	public void setIsDspPubIconForNoWF(boolean isDspPubIconForNoWF) {
		this.isDspPubIconForNoWF = isDspPubIconForNoWF;
	}

	/**
	 * ページアイコン表示フラグを取得します。
	 * @return ページアイコン表示フラグ
	 */
	public boolean getIsDspPageIcon() {
		return isDspPageIcon;
	}

	/**
	 * ページアイコン表示フラグを設定します。
	 * @param isDspPageIcon ページアイコン表示フラグ
	 */
	public void setIsDspPageIcon(boolean isDspPageIcon) {
		this.isDspPageIcon = isDspPageIcon;
	}

	// TODO actSearch.jspではOCR処理ステータス表示フラグ。dspChildObject.jspと用途が異なる？不要と思われるので削除した方がいい。
//	/**
//	 * WFなしドキュメントの公開フラグを取得します。
//	 * @return WFなしドキュメントの公開フラグ
//	 */
//	public boolean getIsNoWFPublic() {
//		return isNoWFPublic;
//	}
//
//	/**
//	 * WFなしドキュメントの公開フラグを設定します。
//	 * @param isNoWFPublic WFなしドキュメントの公開フラグ
//	 */
//	public void setIsNoWFPublic(boolean isNoWFPublic) {
//		this.isNoWFPublic = isNoWFPublic;
//	}

	/**
	 * OCR処理ステータスを取得します。
	 * @return OCR処理ステータス
	 */
	public String getOcrProcessStatus() {
		return ocrProcessStatus;
	}

	/**
	 * OCR処理ステータスを設定します。
	 * @param ocrProcessStatus OCR処理ステータス
	 */
	public void setOcrProcessStatus(String ocrProcessStatus) {
		this.ocrProcessStatus = ocrProcessStatus;
	}

	/**
	 * OCR結果ステータスを取得します。
	 * @return OCR結果ステータス
	 */
	public String getOcrResultStatus() {
		return ocrResultStatus;
	}

	/**
	 * OCR結果ステータスを設定します。
	 * @param ocrResultStatus OCR結果ステータス
	 */
	public void setOcrResultStatus(String ocrResultStatus) {
		this.ocrResultStatus = ocrResultStatus;
	}

	/**
	 * ステータスタイプ名を取得します。
	 * @return ステータスタイプ名
	 */
	public String getStatusTypeName() {
		return statusTypeName;
	}

	/**
	 * ステータスタイプ名を設定します。
	 * @param statusTypeName ステータスタイプ名
	 */
	public void setStatusTypeName(String statusTypeName) {
		this.statusTypeName = statusTypeName;
	}

	/**
	 * ステータス種別を取得する。
	 * @return ステータス種別
	 */
	public Integer getStatusTypeKind() {
		return statusTypeKind;
	}

	/**
	 * ステータス種別を設定する。
	 * @param statusTypeKind ステータス種別
	 */
	public void setStatusTypeKind(Integer statusTypeKind) {
		this.statusTypeKind = statusTypeKind;
	}

	/**
	 * PDF変換ステータスを取得します。
	 * @return PDF変換ステータス
	 */
	public Integer getPdfConversionStatus() {
		return pdfConversionStatus;
	}

	/**
	 * PDF変換ステータスを設定します。
	 * @param pdfConversionStatus PDF変換ステータス
	 */
	public void setPdfConversionStatus(Integer pdfConversionStatus) {
		this.pdfConversionStatus = pdfConversionStatus;
	}

	/**
	 * 公開PDFが事前登録されているかどうかのフラグを取得します。
	 * @return 公開PDFが事前登録されているかどうかのフラグ
	 */
	public boolean getIsPdfPreRegistered() {
		return isPdfPreRegistered;
	}

	/**
	 * 公開PDFが事前登録されているかどうかのフラグを設定します。
	 * @param isPdfPreRegistered 公開PDFが事前登録されているかどうかのフラグ
	 */
	public void setIsPdfPreRegistered(boolean isPdfPreRegistered) {
		this.isPdfPreRegistered = isPdfPreRegistered;
	}

	/**
	 * 上位階層ワークフロー付きフォルダを取得します。
	 * @return 上位階層ワークフロー付きフォルダ
	 */
	public Long getHigherWFFolder() {
		return higherWFFolder;
	}

	/**
	 * 上位階層ワークフロー付きフォルダを設定します。
	 * @param higherWFFolder 上位階層ワークフロー付きフォルダ
	 */
	public void setHigherWFFolder(Long higherWFFolder) {
		this.higherWFFolder = higherWFFolder;
	}

	/**
	 * 有効期限切れかどうかを取得します。
	 * @return 有効期限切れかどうか
	 */
	public boolean isExpiration() {
		return expiration;
	}

	/**
	 * 有効期限切れかどうかを設定します。
	 * @param expiration 有効期限切れかどうか
	 */
	public void setExpiration(boolean expiration) {
		this.expiration = expiration;
	}

	/**
	 * 署名・暗号化状態を取得します。
	 * @return 署名・暗号化状態
	 */
	public Integer getSignencr() {
		return signencr;
	}

	/**
	 * 署名・暗号化状態を設定します。
	 * @param signencr 署名・暗号化状態
	 */
	public void setSignencr(Integer signencr) {
		this.signencr = signencr;
	}

	/**
	 * プロパティを取得します。
	 * @return プロパティ
	 */
	public String getProperty() {
		return property;
	}

	/**
	 * プロパティを設定します。
	 * @param property プロパティ
	 */
	public void setProperty(String property) {
		this.property = property;
	}

	/**
	 * 公開のみ読取り可を取得します。
	 * @return 公開のみ読取り可
	 */
	public boolean isReadOnly() {
		return readOnly;
	}

	/**
	 * 公開のみ読取り可を設定します。
	 * @param readOnly 公開のみ読取り可
	 */
	public void setReadOnly(boolean readOnly) {
		this.readOnly = readOnly;
	}

	/**
	 * ドキュメントリンクかどうかを取得します。
	 * @return isDocumentLink
	 */
	public boolean getIsDocumentLink() {
		return isDocumentLink;
	}

	/**
	 * ドキュメントリンクかどうかを設定します。
	 * @param isDocumentLink ドキュメントリンクかどうか
	 */
	public void setIsDocumentLink(boolean isDocumentLink) {
		this.isDocumentLink = isDocumentLink;
	}

	/**
	 * ドキュメントリンク過去版通知を取得します。
	 * @return ドキュメントリンク過去版通知
	 */
	public boolean getIsDspLatestLink() {
		return isDspLatestLink;
	}

	/**
	 * ドキュメントリンク過去版通知を設定します。
	 * @param isDspLatestLink ドキュメントリンク過去版通知
	 */
	public void setIsDspLatestLink(boolean isDspLatestLink) {
		this.isDspLatestLink = isDspLatestLink;
	}

	/**
	 * リンク更新タイミングを取得します。
	 * @return リンク更新タイミング
	 */
	public Integer getDocumentLinkUpdateTiming() {
		return documentLinkUpdateTiming;
	}

	/**
	 * リンク更新タイミングを設定します。
	 * @param documentLinkUpdateTiming リンク更新タイミング
	 */
	public void setDocumentLinkUpdateTiming(Integer documentLinkUpdateTiming) {
		this.documentLinkUpdateTiming = documentLinkUpdateTiming;
	}

	/**
	 * ジャンプ対象リンク親フォルダIDを取得します。
	 * @return ジャンプ対象リンク親フォルダID
	 */
	public Long getLinkParentObjId() {
		return linkParentObjId;
	}

	/**
	 * ジャンプ対象リンク親フォルダIDを設定します。
	 * @param linkParentObjId ジャンプ対象リンク親フォルダID
	 */
	public void setLinkParentObjId(Long linkParentObjId) {
		this.linkParentObjId = linkParentObjId;
	}

	/**
	 * パスを取得します。
	 * @return パス
	 */
	public String getPath() {
		return path;
	}

	/**
	 * パスを設定します。
	 * @param path パス
	 */
	public void setPath(String path) {
		this.path = path;
	}

	/**
	 * WFフォルダかどうかを取得します。
	 * @return WFフォルダかどうか
	 */
	public boolean getIsWorkflowFolder() {
		return isWorkflowFolder;
	}

	/**
	 * WFフォルダかどうかを設定します。
	 * @param isWorkflowFolder WFフォルダかどうか
	 */
	public void setIsWorkflowFolder(boolean isWorkflowFolder) {
		this.isWorkflowFolder = isWorkflowFolder;
	}

	/**
	 * 上位階層ワークフロー付きフォルダステータス種別を取得します。
	 * @return 上位階層ワークフロー付きフォルダステータス種別
	 */
	public Integer getHigherWFFolderStatusTypeKind() {
		return higherWFFolderStatusTypeKind;
	}

	/**
	 * 上位階層ワークフロー付きフォルダステータス種別を設定します。
	 * @param higherWFFolderStatusTypeKind 上位階層ワークフロー付きフォルダステータス種別
	 */
	public void setHigherWFFolderStatusTypeKind(Integer higherWFFolderStatusTypeKind) {
		this.higherWFFolderStatusTypeKind = higherWFFolderStatusTypeKind;
	}

	/**
	 * ロックユーザ名を取得します。
	 * @return ロックユーザ名
	 */
	public String getLockUserName() {
		return lockUserName;
	}

	/**
	 * ロックユーザ名を設定します。
	 * @param lockUserName ロックユーザ名
	 */
	public void setLockUserName(String lockUserName) {
		this.lockUserName = lockUserName;
	}

	/**
	 * ロック日時を取得します。
	 * @return ロック日時
	 */
	public Date getLockDate() {
		return lockDate;
	}

	/**
	 * ロック日時を設定します。
	 * @param lockDate ロック日時
	 */
	public void setLockDate(Date lockDate) {
		this.lockDate = lockDate;
	}

	/**
	 * 有効期限を取得します。
	 * @return 有効期限
	 */
	public Date getEffectiveTerm() {
		return effectiveTerm;
	}

	/**
	 * 有効期限を設定します。
	 * @param effectiveTerm 有効期限
	 */
	public void setEffectiveTerm(Date effectiveTerm) {
		this.effectiveTerm = effectiveTerm;
	}

	/**
	 * ファイルサイズを取得します。
	 * @return ファイルサイズ
	 */
	public Long getFileSize() {
		return fileSize;
	}

	/**
	 * ファイルサイズを設定します。
	 * @param fileSize ファイルサイズ
	 */
	public void setFileSize(Long fileSize) {
		this.fileSize = fileSize;
	}

	/**
	 * 番号(自動採番)を取得します。
	 * @return 番号(自動採番)
	 */
	public String getNumber() {
		return number;
	}

	/**
	 * 番号(自動採番)を設定します。
	 * @param number 番号(自動採番)
	 */
	public void setNumber(String number) {
		this.number = number;
	}

	/**
	 * カスタム属性値を保持するMapを取得します。
	 * @return attr カスタム属性値を保持するMap
	 */
	public Map<String, Object> getAttr() {
		return attr;
	}

	/**
	 * カスタム属性値を保持するMapを設定します。
	 * @param attr カスタム属性値を保持するMap
	 */
	public void setAttr(Map<String, Object> attr) {
		this.attr = attr;
	}

	/**
	 * 属性値スニペットを取得します。
	 * @return 属性値スニペット
	 */
	public List<String> getSnippetAttributeList() {
		return snippetAttributeList;
	}

	/**
	 * 属性値スニペットを設定します。
	 * @param snippet 属性値スニペット
	 */
	public void setSnippetAttributeList(List<String> snippetAttributeList) {
		this.snippetAttributeList = snippetAttributeList;
	}

	/**
	 * ファイル全文スニペットを取得します。
	 * @return ファイル全文スニペット
	 */
	public List<String> getSnippetFullTextList() {
		return snippetFullTextList;
	}

	/**
	 * ファイル全文スニペットを設定します。
	 * @param snippet ファイル全文スニペット
	 */
	public void setSnippetFullTextList(List<String> snippetFullTextList) {
		this.snippetFullTextList = snippetFullTextList;
	}

	// ----------------------
	// Publicメソッド
	// ----------------------

	/**
	 * カスタム属性値を追加します。
	 * @param key レスポンスデータパラメータ名(フィールド名プレフィックス_属性タイプID_データ型サフィックス)
	 * @param value カスタム属性値
	 */
	public void addAttr(String key, Object value) {
		if (attr == null) {
			attr = new HashMap<>();
		}

		attr.put(key, value);
	}

	/**
	 * 属性値フィールドスニペットを追加します。
	 * @param snippet スニペット
	 */
	public void addSnippetAttribute(String snippet) {
		if (this.snippetAttributeList == null) {
			this.snippetAttributeList = new ArrayList<>();
		}

		snippetAttributeList.add(snippet);
	}

	/**
	 * 全文テキストフィールドスニペットを追加します。
	 * @param snippet スニペット
	 */
	public void addSnippetFullText(String snippet) {
		if (this.snippetFullTextList == null) {
			this.snippetFullTextList = new ArrayList<>();
		}

		snippetFullTextList.add(snippet);
	}

}
