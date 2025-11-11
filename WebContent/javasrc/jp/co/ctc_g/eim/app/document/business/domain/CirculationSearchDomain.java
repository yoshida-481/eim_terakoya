package jp.co.ctc_g.eim.app.document.business.domain;

import java.text.SimpleDateFormat;
import java.util.Date;

import common.util.AppConstant;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;

/**
 * 承認依頼情報ドメイン
 */
public class CirculationSearchDomain {

	/** 定数：OCR処理ステータス未設定 */
	private final static long OCR_PROC_STATUS_NONE = -1;
	
	/** ログイン言語の日付フォーマット */
	private SimpleDateFormat sdf;
	
	/** ファイル名 */
	private String objName;

	/** オブジェクトID */
	private long objId;

	/** オブジェクトタイプID */
	private long objTypeId;

	/** オブジェクトタイプ名称 */
	private String objTypeName;

	/** 自動採番 番号 */
	private String number;

	/** ステータスID */
	private long statusId;

	/** 履歴 */
	private String rev = "-";

	/** ステータスベースタイプID */
	private long statusTypeKind;

	/** ステータスタイプ名称 */
	private String statusTypeName;

	/** ステータスタイプID */
	private long statusTypeId;

	/** WFなしドキュメント公開フラグ */
	private String isNoWFPublic;

	/** 承認依頼者 */
	private String requestUser;

	/** 承認依頼者ID */
	private long requestUserId;

	/** 承認依頼日 */
	private Date requestDate;

	/** ワークフローつきフォルダフラグ */
	private String isWFFolder;

	/** フォルダフラグ */
	private String isFolder;

	/** ドキュメントフラグ */
	private String isDocument;

	/** フルパス表示フラグ */
	private String isFullPath;

	/** 閲覧のみフラグ */
	private String isReadOnly;

	/** 公開済フラグ */
	private String isPublished;

	/** PDFドキュメントフラグ */
	private String isDspPdfIcon;

	/** PDF変換ステータス出力 */
	private int pdfConversionStatus;

	/** 次の承認者 */
	private  String nextApprover;

	/** パス */
	private String path;

	/** PDF結合失敗フラグ */
	private String isPDFJoinFailed;

	/** OCR処理ステータス */
	private long ocrProcessStatus;

	/** OCR結果ステータス */
	private long ocrResultStatus;

	/** 有効期限切れ */
	private String expiration;

	/** Lock User */
	private String lockUserName;

	/** 過去版フラグ */
	private String isOldVer;

	/** 公開PDFが事前登録されているかどうかのフラグ */
	private String isPdfPreRegistered;
	/**
	 * コンストラクタ 
	 * @param object オブジェクト情報
	 * @throws Exception 
	 */
	public CirculationSearchDomain(ObjectDomain object) throws Exception {

		this.objId = object.getId();
		this.objTypeId = object.getType().getId();
		this.objName = object.getName();
		this.statusId = object.getStatus().getId();
		this.statusTypeId = object.getStatus().getType().getId();
		this.statusTypeKind = object.getStatus().getType().getBase().getId();
		this.isPublished  =String.valueOf((this.statusTypeKind == AppConstant.STATUS_TYPE_KIND_ID_PUBLIC));
		this.statusTypeName = object.getStatus().getType().getName();
		if (object.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PASS")) != null) {
			this.path = object.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PASS")).getString();
		}
		if (object.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PDF_JOIN_FAIL")) != null) {
			this.isPDFJoinFailed = "true";
		} else {
			this.isPDFJoinFailed = "false";
		}
		if (object.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_OCR_PROCESS_STATUS")) != null) {
			this.ocrProcessStatus = object.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_OCR_PROCESS_STATUS")).getLong();
		} else {
			this.ocrProcessStatus = OCR_PROC_STATUS_NONE;
		}
		if (object.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_OCR_RESULT_STATUS")) != null) {
			this.ocrResultStatus = object.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_OCR_RESULT_STATUS")).getLong();
		} else {
			this.ocrResultStatus = OCR_PROC_STATUS_NONE;
		}
		if (object.getLockUser() != null) {
			this.lockUserName = object.getLockUser().getName();
		} else {
			this.lockUserName = "";
		}
	}

	/**
	 * ドキュメントフラグを取得します。
	 * @return ドキュメントフラグ
	 */
	public String getIsDocument() {
		return isDocument;
	}

	/**
	 * ドキュメントフラグを設定します。
	 * @param isDocument ドキュメントフラグ
	 */
	public void setIsDocument(String isDocument) {
		this.isDocument = isDocument;
	}

	/**
	 * オブジェクト名称を取得します。
	 * @return オブジェクト名称
	 */
	public String getObjName() {
		return objName;
	}

	/**
	 * オブジェクト名称を設定します。
	 * @param objName オブジェクト名称
	 */
	public void setObjName(String objName) {
		this.objName = objName;
	}

	/**
	 * オブジェクトIDを取得します。
	 * @return オブジェクトID
	 */
	public long getObjId() {
		return objId;
	}

	/**
	 * オブジェクトIDを設定します。
	 * @param objId オブジェクトID
	 */
	public void setObjId(long objId) {
		this.objId = objId;
	}

	/**
	 * オブジェクトタイプIDを取得します。
	 * @return オブジェクトタイプID
	 */
	public long getObjTypeId() {
		return objTypeId;
	}

	/**
	 * オブジェクトタイプIDを設定します。
	 * @param objTypeId オブジェクトタイプID
	 */
	public void setObjTypeId(long objTypeId) {
		this.objTypeId = objTypeId;
	}

	/**
	 * フォルダフラグを取得します。
	 * @return フォルダフラグ
	 */
	public String getIsFolder() {
		return isFolder;
	}

	/**
	 * フォルダフラグを設定します。
	 * @param isFolder フォルダフラグ
	 */
	public void setIsFolder(String isFolder) {
		this.isFolder = isFolder;
	}

	/**
	 * WFつきフォルダフラグを取得します。
	 * @return WFつきフォルダフラグ
	 */
	public String getIsWFFolder() {
		return isWFFolder;
	}

	/**
	 * WFつきフォルダフラグを設定します。
	 * @param isWFFolder WFつきフォルダフラグ
	 */
	public void setIsWFFolder(String isWFFolder) {
		this.isWFFolder = isWFFolder;
	}

	/**
	 * フルパスフラグを取得します。
	 * @return フルパスフラグ
	 */
	public String getIsFullPath() {
		return isFullPath;
	}

	/**
	 * フルパスフラグを設定します。
	 * @param isFullPath フルパスフラグ
	 */
	public void setIsFullPath(String isFullPath) {
		this.isFullPath = isFullPath;
	}

	/**
	 * 承認依頼者を取得します。
	 * @return 承認依頼者
	 */
	public String getRequestUser() {
		return requestUser;
	}

	/**
	 * 承認依頼者を設定します。
	 * @param requestUser 承認依頼者
	 */
	public void setRequestUser(String requestUser) {
		this.requestUser = requestUser;
	}

	/**
	 * 承認依頼者を取得します。
	 * @return 次の承認予定者
	 */
	public String getNextApprover() {
		return nextApprover;
	}

	/**
	 * 次の承認予定者を取得します。
	 * @param nextApprover 次の承認予定者
	 */
	public void setNextApprover(String nextApprover) {
		this.nextApprover = nextApprover;
	}

	/**
	 * 次の承認予定者を設定します。
	 * @return 承認依頼者ID
	 */
	public long getRequestUserId() {
		return requestUserId;
	}

	/**
	 * 承認依頼者IDを取得します。
	 * @param requestUser 承認依頼者ID
	 */
	public void setRequestUserId(long requestUserId) {
		this.requestUserId = requestUserId;
	}

	/**
	 * 承認依頼者IDを設定します。
	 * @return 承認依頼日(文字列)
	 */
	public String getRequestDateStr() {
		return sdf.format(requestDate);
	}
	
	/**
	 * 承認依頼日を取得します。
	 * @return 承認依頼日
	 */
	public Date getRequestDate() {
		return requestDate;
	}

	/**
	 * 承認依頼日を設定します。
	 * @param requesDate 承認依頼日
	 */
	public void setRequestDate(Date requestDate) {
		this.requestDate = requestDate;
	}

	/**
	 * 有効期限切れフラグを取得します。
	 * @return 有効期限切れフラグ
	 */
	public String getExpiration() {
		return expiration;
	}

	/**
	 * 有効期限切れフラグを設定します。
	 * @param expiration 有効期限切れフラグ
	 */
	public void setExpiration(String expiration) {
		this.expiration = expiration;
	}

	/**
	 * オブジェクトタイプ名称を取得します。
	 * @return オブジェクトタイプ名称
	 */
	public String getObjTypeName() {
		return objTypeName;
	}

	/**
	 * オブジェクトタイプ名称を設定します。
	 * @param objTypeName オブジェクトタイプ名称
	 */
	public void setObjTypeName(String objTypeName) {
		this.objTypeName = objTypeName;
	}

	/**
	 * 自動採番 番号を取得します。
	 * @return 自動採番 番号
	 */
	public String getNumber() {
		return number;
	}

	/**
	 * 自動採番 番号を設定します。
	 * @param number 自動採番 番号
	 */
	public void setNumber(String number) {
		this.number = number;
	}

	/**
	 * ステータスIDを取得します。
	 * @return ステータスID
	 */
	public long getStatusId() {
		return statusId;
	}

	/**
	 * ステータスIDを設定します。
	 * @param statusId ステータスID
	 */
	public void setStatusId(long statusId) {
		this.statusId = statusId;
	}

	/**
	 * 履歴を取得します。
	 * @return 履歴
	 */
	public String getRev() {
		return rev;
	}

	/**
	 * 履歴を設定します。
	 * @param rev 履歴
	 */
	public void setRev(String rev) {
		this.rev = rev;
	}

	/**
	 * ステータスベースタイプIDを取得します。
	 * @return ステータスベースタイプID
	 */
	public long getStatusTypeKind() {
		return statusTypeKind;
	}

	/**
	 * ステータスベースタイプIDを設定します。
	 * @param statusTypeKind ステータスベースタイプID
	 */
	public void setStatusTypeKind(long statusTypeKind) {
		this.statusTypeKind = statusTypeKind;
	}

	/**
	 * ステータスタイプ名称を取得します。
	 * @return ステータスタイプ名称
	 */
	public String getStatusTypeName() {
		return statusTypeName;
	}

	/**
	 * ステータスタイプ名称を設定します。
	 * @param statusTypeName ステータスタイプ名称
	 */
	public void setStatusTypeName(String statusTypeName) {
		this.statusTypeName = statusTypeName;
	}
	
	/**
	 * ステータスタイプIDを取得します。
	 * @return statusTypeId ステータスタイプID
	 */
	public long getStatusTypeId() {
		return statusTypeId;
	}

	/**
	 * ステータスタイプIDを設定します。
	 * @param ステータスタイプID
	 */
	public void setStatusTypeId(long statusTypeId) {
		this.statusTypeId = statusTypeId;
	}

	/**
	 * WFなしドキュメント公開フラグを取得します。
	 * @return WFなしドキュメント公開フラグ
	 */
	public String getIsNoWFPublic() {
		return isNoWFPublic;
	}

	/**
	 * WFなしドキュメント公開フラグを設定します。
	 * @param isNoWFPublic WFなしドキュメント公開フラグ
	 */
	public void setIsNoWFPublic(String isNoWFPublic) {
		this.isNoWFPublic = isNoWFPublic;
	}

	/**
	 * 閲覧のみフラグを取得します。
	 * @return 閲覧のみフラグ
	 */
	public String getIsReadOnly() {
		return isReadOnly;
	}

	/**
	 * 閲覧のみフラグを設定します。
	 * @param isReadOnly 閲覧のみフラグ
	 */
	public void setIsReadOnly(String isReadOnly) {
		this.isReadOnly = isReadOnly;
	}

	/**
	 * 公開済フラグを取得します。
	 * @return 公開済フラグ
	 */
	public String getIsPublished() {
		return isPublished;
	}

	/**
	 * 公開済フラグを設定します。
	 * @param isPublished 公開済フラグ
	 */
	public void setIsPublished(String isPublished) {
		this.isPublished = isPublished;
	}

	/**
	 * PDFドキュメントフラグを取得します。
	 * @return PDFドキュメントフラグ
	 */
	public String getIsDspPdfIcon() {
		return isDspPdfIcon;
	}

	/**
	 * PDFドキュメントフラグを設定します。
	 * @param isDspPdfIcon PDFドキュメントフラグ
	 */
	public void setIsDspPdfIcon(String isDspPdfIcon) {
		this.isDspPdfIcon = isDspPdfIcon;
	}

	/**
	 * PDF変換ステータス出力を取得します。
	 * @return pdfConversionStatus PDF変換ステータス出力
	 */
	public int getPdfConversionStatus() {
		return pdfConversionStatus;
	}

	/**
	 * PDF変換ステータス出力を設定します。
	 * @param pdfConversionStatus PDF変換ステータス出力
	 */
	public void setPdfConversionStatus(int pdfConversionStatus) {
		this.pdfConversionStatus = pdfConversionStatus;
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
	 * PDFドキュメントフラグを取得します。
	 * @return PDFドキュメントフラグ
	 */
	public String getIsPDFJoinFailed() {
		return isPDFJoinFailed;
	}

	/**
	 * PDFドキュメントフラグを設定します。
	 * @param isPDFJoinFailed PDFドキュメントフラグ
	 */
	public void setIsPDFJoinFailed(String isPDFJoinFailed) {
		this.isPDFJoinFailed = isPDFJoinFailed;
	}

	/**
	 * OCR処理ステータスを取得します。
	 * @return OCR処理ステータス
	 */
	public long getOcrProcessStatus() {
		return ocrProcessStatus;
	}

	/**
	 * OCR処理ステータスを設定します。
	 * @param ocrProcessStatus OCR処理ステータス
	 */
	public void setOcrProcessStatus(long ocrProcessStatus) {
		this.ocrProcessStatus = ocrProcessStatus;
	}

	/**
	 * OCR結果ステータスを取得します。
	 * @return OCR結果ステータス
	 */
	public long getOcrResultStatus() {
		return ocrResultStatus;
	}

	/**
	 * OCR結果ステータスを設定します。
	 * @param ocrResultStatus OCR結果ステータス
	 */
	public void setOcrResultStatus(long ocrResultStatus) {
		this.ocrResultStatus = ocrResultStatus;
	}

	/**
	 * Lock User(改訂者)を取得します。
	 * @return Lock User(改訂者)
	 */
	public String getLockUserName() {
		return lockUserName;
	}

	/**
	 * Lock User(改訂者)を設定します。
	 * @param lockUserName Lock User(改訂者)
	 */
	public void setLockUserName(String lockUserName) {
		this.lockUserName = lockUserName;
	}
	
	/**
	 * 過去版フラグを取得します。
	 * @return 過去版フラグ
	 */
	public String getIsOldVer() {
		return isOldVer;
	}

	/**
	 * 過去版フラグを設定します。
	 * @param isOldVer 過去版フラグ
	 */
	public void setIsOldVer(String isOldVer) {
		this.isOldVer = isOldVer;
	}

	/**
	 * 日付フォーマットを取得します。
	 * @return 日付フォーマット
	 */
	public SimpleDateFormat getSdf() {
		return sdf;
	}

	/**
	 * 日付フォーマットを設定します。
	 * @param sdf 日付フォーマット
	 */
	public void setSdf(SimpleDateFormat sdf) {
		this.sdf = sdf;
	}

	/**
	 * @return 公開PDFが事前登録されているかどうかのフラグを取得します。
	 */
	public String getIsPdfPreRegistered() {
		return isPdfPreRegistered;
	}

	/**
	 * @param 公開PDFが事前登録されているかどうかのフラグを設定します。
	 */
	public void setIsPdfPreRegistered(String isPdfPreRegistered) {
		this.isPdfPreRegistered = isPdfPreRegistered;
	}
}
