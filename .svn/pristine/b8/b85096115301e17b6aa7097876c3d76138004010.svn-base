package jp.co.ctc_g.eim.common.presentation.web.dto;

import common.util.NamespaceUtil;
import jp.co.ctc_g.eim.admin.business.domain.WorkflowAdminDomain;
import jp.co.ctc_g.eim.common.presentation.web.dto.EventTypeDefDTO.EventTypeList;
import jp.co.ctc_g.eim.common.presentation.web.dto.OtherNameDTO.NameList;
import jp.co.ctc_g.eim.common.presentation.web.dto.StatusTypeDefDTO.StatusTypeList;
import jp.co.ctc_g.eim.framework.business.domain.entity.WorkFlowDomain;

/**
 * ワークフロー定義DTO
 * <p>
 * Struts->SpringMVCへの移行に際して、クライアントへ送信するXMLデータを再現したデータ転送用オブジェクトです。
 */
public class WorkFlowDefDTO {

	/**
	 * Strutsで生成したXMLデータをjsonへ変換した際、単数値のプロパティを保持する親要素を再現するためのクラスです。
	 */
	public class Attr {

		/** ワークフローID */
		private String id = "";

		/** ワークフロー名（デフォルト） */
		private String name = null;

		/** ネームスペース */
		private String namespace = null;

		/**
		 * コンストラクタ
		 * 
		 * @param workFlow
		 * @param wfDefName 
		 * @param nameSpace2 
		 */
		protected Attr(WorkFlowDomain workFlow, String wfDefName, String nameSpace2) {
			id = String.valueOf(workFlow.getId());
			name = wfDefName;
			namespace = nameSpace2;
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

		public String getNamespace() {
			return namespace;
		}

		public void setNamespace(String nameSpace) {
			this.namespace = nameSpace;
		}
	}

	/**
	 * クライアントへ送信していたXMLデータにおいて、単数値のプロパティを保持する親要素です。
	 */
	private Attr attr = null;

	/** ステータスタイプリスト */
	private StatusTypeList statusTypeList = null;;

	/** イベントタイプタイプリスト */
	private EventTypeList eventTypeList = null;

	/** 名称一覧 */
	private NameList nameList = null;

	/**
	 * コンストラクタ
	 * 
	 * @param workflowAdminDomain 
	 * @param wfDefName
	 * @throws Exception
	 */
	public WorkFlowDefDTO(WorkflowAdminDomain workflowAdminDomain, String wfDefName) throws Exception {
		String namespace = NamespaceUtil.getNamespaceByDefName(workflowAdminDomain.getDefName());
		attr = new Attr(workflowAdminDomain, wfDefName, namespace);

		nameList = new NameList(workflowAdminDomain.getNameList());
		statusTypeList = new StatusTypeList(workflowAdminDomain);
		eventTypeList = new EventTypeList(workflowAdminDomain.getEventTypeList(), workflowAdminDomain.getStatusTypeList().size());
		
	}

	/**
	 * @return attrを取得します。
	 */
	public Attr getAttr() {
		return attr;
	}

	/**
	 * @param attrを設定します。
	 */
	public void setAttr(Attr attr) {
		this.attr = attr;
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
	 * @param eventTypeLisを設定します。
	 */
	public void setEventTypeList(EventTypeList eventTypeList) {
		this.eventTypeList = eventTypeList;
	}

	/**
	 * @return nameList を取得します。
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
}
