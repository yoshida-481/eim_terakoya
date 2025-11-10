package jp.co.ctc_g.eim.app.document.presentation.dto;

import common.util.NamespaceUtil;
import jp.co.ctc_g.eim.app.document.business.domain.WorkFlowDocDomain;
import jp.co.ctc_g.eim.app.document.presentation.dto.StatusTypeDefDocDTO.StatusTypeList;
import jp.co.ctc_g.eim.common.presentation.web.dto.AssignEntryDTO.AsEntryList;
import jp.co.ctc_g.eim.common.presentation.web.dto.EventTypeDefDTO.EventTypeList;
import jp.co.ctc_g.eim.common.presentation.web.dto.OtherNameDTO.NameList;


public class WorkFlowDefDocDTO {

	/**
	 * Strutsで生成したXMLデータをjsonへ変換した際、単数値のプロパティを保持する親要素を再現するためのクラスです。
	 */
	public class Attr {
		/* メール通知方法のデフォルト設定 */
		private String defNotifyMail = "";

		/* 処理依頼先のデフォルト設定 */
		private String defApproveRequest = "";

		/* 公開通知デフォルト設定 */
		private String publishNotifyMail = "";

		/* 処理待ちポップアップ通知 */
		private String processWaitPopup = "";

		/* 差戻し・取戻しメール通知 */
		private String backMail = "";

		/* OCR処理のデフォルト設定 */
		private String defOcr = "";

		/* 公開機能用：上長承認   necessary：要承認  unnecessary：承認不要 */
		private String defBossApproval = "";

		/* ワークフローID */
		private String id = "";

		/* ワークフロー名（デフォルト） */
		private String name = "";

		/* namespace */
		private String namespace = "";
		
		protected Attr(WorkFlowDocDomain workFlowDocDomain) {

			backMail = String.valueOf(workFlowDocDomain.getBackMail());
			defApproveRequest = String.valueOf(workFlowDocDomain.getDefApproveRequest());
			defBossApproval = workFlowDocDomain.getDefBossApproval();
			defNotifyMail = workFlowDocDomain.getDefNotifyMail();
			defOcr = String.valueOf(workFlowDocDomain.getDefOcr());
			id = String.valueOf(workFlowDocDomain.getId());
			name = workFlowDocDomain.getDefName();
			namespace = NamespaceUtil.getNamespaceByDefName(workFlowDocDomain.getDefName());
			processWaitPopup = String.valueOf(workFlowDocDomain.getProcessWaitPopup());
			publishNotifyMail = String.valueOf(workFlowDocDomain.getPublishNotifyMail());
		}

		/**
		 * defNotifyMailを取得します。
		 * @return defNotifyMail
		 */
		public String getDefNotifyMail() {
		    return defNotifyMail;
		}

		/**
		 * defNotifyMailを設定します。
		 * @param defNotifyMail defNotifyMail
		 */
		public void setDefNotifyMail(String defNotifyMail) {
		    this.defNotifyMail = defNotifyMail;
		}

		/**
		 * defApproveRequestを取得します。
		 * @return defApproveRequest
		 */
		public String getDefApproveRequest() {
		    return defApproveRequest;
		}

		/**
		 * defApproveRequestを設定します。
		 * @param defApproveRequest defApproveRequest
		 */
		public void setDefApproveRequest(String defApproveRequest) {
		    this.defApproveRequest = defApproveRequest;
		}

		/**
		 * publishNotifyMailを取得します。
		 * @return publishNotifyMail
		 */
		public String getPublishNotifyMail() {
		    return publishNotifyMail;
		}

		/**
		 * publishNotifyMailを設定します。
		 * @param publishNotifyMail publishNotifyMail
		 */
		public void setPublishNotifyMail(String publishNotifyMail) {
		    this.publishNotifyMail = publishNotifyMail;
		}

		/**
		 * processWaitPopupを取得します。
		 * @return processWaitPopup
		 */
		public String getProcessWaitPopup() {
		    return processWaitPopup;
		}

		/**
		 * processWaitPopupを設定します。
		 * @param processWaitPopup processWaitPopup
		 */
		public void setProcessWaitPopup(String processWaitPopup) {
		    this.processWaitPopup = processWaitPopup;
		}

		/**
		 * backMailを取得します。
		 * @return backMail
		 */
		public String getBackMail() {
		    return backMail;
		}

		/**
		 * backMailを設定します。
		 * @param backMail backMail
		 */
		public void setBackMail(String backMail) {
		    this.backMail = backMail;
		}

		/**
		 * bossApprovalを取得します。
		 * @return defApproveRequest
		 */
		public String getDefBossApproval() {
		    return defBossApproval;
		}

		/**
		 * bossApprovalを設定します。
		 * @param bossApproval bossApproval
		 */
		public void setDefBossApproval(String defBossApproval) {
		    this.defBossApproval = defBossApproval;
		}

		/**
		 * defOcrを取得します。
		 * @return defOcr
		 */
		public String getDefOcr() {
			return defOcr;
		}

		/**
		 * defOcrを設定します。
		 * @param defOcr
		 */
		public void setDefOcr(String defOcr) {
			this.defOcr = defOcr;
		}

		public String getId() {
			return id;
		}

		public void setId(String id) {
			this.id = id;
		}

		public String getName() {
			return name;
		}

		public void setName(String name) {
			this.name = name;
		}

		/**
		 * @return namespace
		 */
		public String getNamespace() {
			return namespace;
		}

		/**
		 * @param namespace セットする namespace
		 */
		public void setNameSpace(String namespace) {
			this.namespace = namespace;
		}
			
	}

	/**
	 *
	 */
	Attr attr = null;

	/** 公開処理設定電子署名 */
	PDFSignatureDTO pdfSignature;

	/** デフォルト公開通知先参照 */
	AsEntryList publishNotifyList = null;

	/** ステータスタイプリスト */
	private StatusTypeList statusTypeList = null;;

	/** イベントタイプタイプリスト */
	private EventTypeList eventTypeList = null;

	/** 名称一覧 */
	private NameList nameList = null;

	/**
	 * コンストラクタ<br>
	 * WorkFlowDomainが保持しているプロパティ値を設定します。<br>
	 * <li>イベントタイプ一覧は空です。
	 * <li>ワークフロー名称一覧には、セッション言語分しか設定されていません。
	 *
	 * @param workFlow ワークフロー
	 * @throws Exception
	 */
	public WorkFlowDefDocDTO(WorkFlowDocDomain workFlow) throws Exception {

		attr = new Attr(workFlow);

		pdfSignature = new PDFSignatureDTO(workFlow);
		eventTypeList = new EventTypeList(workFlow.getEventTypeList(),workFlow.getStatusTypeList().size());
		nameList = new NameList(workFlow.getNameList());
		publishNotifyList = new AsEntryList(workFlow.getPublishNotyfyAssignEntryDomain());
		statusTypeList = new StatusTypeList(workFlow);
	}


	public Attr getAttr() {
		return attr;
	}

	public void setAttr(Attr attr) {
		this.attr = attr;
	}

	/**
	 * signatureCommandを取得します。
	 * @return signatureCommand
	 */
	public PDFSignatureDTO getPDFSignature() {
	    return pdfSignature;
	}

	/**
	 * signatureCommandを設定します。
	 * @param signatureCommand signatureCommand
	 */
	public void setPDFSignature(PDFSignatureDTO signatureCommand) {
	    this.pdfSignature = signatureCommand;
	}

	/**
	 * @return statusTypeListを取得します。
	 */
	public StatusTypeList getStatusTypeList() {
		return statusTypeList;
	}


	/**
	 * @param statusTypeListを設定します。
	 */
	public void setStatusTypeList(StatusTypeList statusTypeList) {
		this.statusTypeList = statusTypeList;
	}


	/**
	 * @return eventTypeListを取得します。
	 */
	public EventTypeList getEventTypeList() {
		return eventTypeList;
	}


	/**
	 * @param eventTypeListを設定します。
	 */
	public void setEventTypeList(EventTypeList eventTypeList) {
		this.eventTypeList = eventTypeList;
	}


	/**
	 * @return nameListを取得します。
	 */
	public NameList getNameList() {
		return nameList;
	}


	/**
	 * @param nameListを設定します。
	 */
	public void setNameList(NameList nameList) {
		this.nameList = nameList;
	}
	

	/**
	 * publishNotifyListを取得します。
	 * @return publishNotifyList
	 */
	public AsEntryList getPublishNotifyList() {
	    return publishNotifyList;
	}

	/**
	 * publishNotifyListを設定します。
	 * @param publishNotifyList publishNotifyList
	 */
	public void setPublishNotifyList(AsEntryList publishNotifyList) {
	    this.publishNotifyList = publishNotifyList;
	}

}
