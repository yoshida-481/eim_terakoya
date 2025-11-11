package jp.co.ctc_g.eim.app.document.business.domain;

import java.util.Date;

import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;

/**
 * ドキュメントの情報を保持します.
 * @since Ver 6.6
 */
public class DocumentObjectDomain extends ObjectDomain {

	/** 公開ファイル名 */
	private String publicFileName;
	
	/** 公開読取権限のみ保有フラグ */
	private boolean authOnlyPublicRead = false;
	
	/** PDF変換処理実行日時 */
	private Date pdfConversionExecutedDate;
	
	/** 公開PDF事前登録日時 */
	private Date pdfPreRegistDate;

	/**
	 * コンストラクタ<br>
	 *
	 * @throws Exception
	 * @since Ver6.6
	 */
	public DocumentObjectDomain() throws Exception {
		super();
	}
	
	/**
	 * コンストラクタ<br>
	 * ObjectDomainが保持しているプロパティ値を設定します。<br>
	 *
	 * @param object オブジェクト
	 * @throws Exception
	 * @since Ver6.6
	 */
	public DocumentObjectDomain(ObjectDomain object) throws Exception {
		super();
		super.setAttributeList(object.getAttributeList());
		super.setCreationUser(object.getCreationUser());
		super.setCreationDate(object.getCreationDate());
		super.setId(object.getId());
		super.setLatest(object.isLatest());
		super.setLockUser(object.getLockUser());
		super.setLockDate(object.getLockDate());
		super.setModificationUser(object.getModificationUser());
		super.setModificationDate(object.getModificationDate());
		super.setName(object.getName());
		super.setType(object.getType());
		super.setRevision(object.getRevision());
		super.setSecurity(object.getSecurity());
		super.setStatus(object.getStatus());
		super.setRevisionGroupId(object.getRevisionGroupId());
		super.setStatusList(object.getStatusList());
	}
	
	/**
	 * 公開ファイル名を取得します。
	 * @return 公開ファイル名
	 * @since Ver6.6
	 */
	public String getPublicFileName() {
		return publicFileName;
	}

	/**
	 * 公開ファイル名を設定します。
	 * @param publicFileName 公開ファイル名
	 * @since Ver6.6
	 */
	public void setPublicFileName(String publicFileName) {
		this.publicFileName = publicFileName;
	}

	/**
	 * 複製を生成します。
	 * @return オブジェクトドメインの複製（ディープコピー）
	 * @since Ver6.6
	 */
	public DocumentObjectDomain clone() {
		DocumentObjectDomain documentObject = (DocumentObjectDomain)super.clone();
		
		return documentObject;
	}

	/**
	 * 公開読取権限のみ保有フラグを取得します。
	 * @return 公開読取権限のみ保有フラグ
	 * @since Ver6.6
	 */
	public boolean getAuthOnlyPublicRead() {
		return authOnlyPublicRead;
	}

	/**
	 * 公開読取権限のみ保有フラグを設定します。
	 * @param authOnlyPublicRead 公開読取権限のみ保有フラグ
	 * @since Ver6.6
	 */
	public void setAuthOnlyPublicRead(boolean authOnlyPublicRead) {
		this.authOnlyPublicRead = authOnlyPublicRead;
	}

	/**
	 * PDF変換処理実行日時を取得します。
	 * @return PDF変換処理実行日時
	 * @since Ver6.6
	 */
	public Date getPdfConversionExecutedDate() {
		return pdfConversionExecutedDate;
	}

	/**
	 * PDF変換処理実行日時を設定します。
	 * @param pdfConversionExecutedDate PDF変換処理実行日時
	 * @since Ver6.6
	 */
	public void setPdfConversionExecutedDate(Date pdfConversionExecutedDate) {
		this.pdfConversionExecutedDate = pdfConversionExecutedDate;
	}

	/**
	 * 公開PDF事前登録日時を取得します。
	 * @return 公開PDF事前登録日時
	 * @since Ver6.16
	 */
	public Date getPdfPreRegistDate() {
		return pdfPreRegistDate;
	}

	/**
	 * 公開PDF事前登録日時を設定します。
	 * @param pdfPreRegistDate 公開PDF事前登録日時
	 * @since Ver6.16
	 */
	public void setPdfPreRegistDate(Date pdfPreRegistDate) {
		this.pdfPreRegistDate = pdfPreRegistDate;
	}
}
