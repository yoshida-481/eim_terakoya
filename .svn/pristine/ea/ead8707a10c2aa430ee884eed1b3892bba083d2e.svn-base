package jp.co.ctc_g.eim.common.presentation.web.dto;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import eim.bo.EIMStatusType;
import eim.bo.EIMWorkFlow;
import jp.co.ctc_g.eim.admin.business.domain.WorkflowAdminDomain;
import jp.co.ctc_g.eim.app.document.business.domain.WorkFlowHistoryDomain;
import jp.co.ctc_g.eim.app.form.business.domain.AttributeTypeLayoutDomain;
import jp.co.ctc_g.eim.app.form.business.domain.StatusTypeLayoutDomain;
import jp.co.ctc_g.eim.common.presentation.web.dto.AssignEntryDTO.AsEntryList;
import jp.co.ctc_g.eim.common.presentation.web.dto.AttTypesDTO.AttTypes;
import jp.co.ctc_g.eim.common.presentation.web.dto.EventDTO.EventList;
import jp.co.ctc_g.eim.common.presentation.web.dto.OtherNameDTO.NameList;
import jp.co.ctc_g.eim.common.presentation.web.dto.UserListDTO.UserList;
import jp.co.ctc_g.eim.framework.business.domain.EventLogDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.StatusDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.StatusTypeDomain;

/**
 * ステータスタイプ一覧DTO
 * <p>
 * Struts->SpringMVCへの移行に際して、クライアントへ送信するXMLデータを再現したデータ転送用オブジェクトです。
 */
public class StatusTypeDefDTO {

	/**
	 * Strutsで生成したXMLデータにおける配列要素を再現するためのクラスです。
	 */
	public static class StatusTypeList {
		/** ステータスタイプリスト */
		private List<StatusTypeDefDTO> statusType = new ArrayList<StatusTypeDefDTO>();

		/**
		 * Strutsで生成したXMLデータにおいて単数値のプロパティを保持する親要素です。
		 */
		private Attr attr = null;

		/**
		 * クライアントへ送信していたXMLデータにおいて、単数値のプロパティを保持する親要素を再現するためのクラスです。
		 */
		public class Attr {
			private String objId;
			private String wfFolderName;
			private String workFlowId;
			private String workFlowName;
		
			/**
			 * コンストラクタ.
			 * 
			 * @param workFlow
			 * @param objDomain
			 */
			public Attr(EIMWorkFlow workFlow, ObjectDomain objDomain) {
				setWorkFlowId(String.valueOf(workFlow.getId()));
				setWorkFlowName(workFlow.getName());
				setWfFolderName(objDomain.getName());
				setObjId(String.valueOf(objDomain.getId()));
			}

			public String getObjId() {
				return objId;
			}

			public void setObjId(String objId) {
				this.objId = objId;
			}

			public String getWfFolderName() {
				return wfFolderName;
			}

			public void setWfFolderName(String wfFolderName) {
				this.wfFolderName = wfFolderName;
			}

			public String getWorkFlowId() {
				return workFlowId;
			}

			public void setWorkFlowId(String workFlowId) {
				this.workFlowId = workFlowId;
			}

			public String getWorkFlowName() {
				return workFlowName;
			}

			public void setWorkFlowName(String workFlowName) {
				this.workFlowName = workFlowName;
			}
		}
		
		/**
		 * (WorkflowAdminDomain)コンストラクタ.
		 * 
		 * @param workFlow
		 * @throws Exception 
		 */
		public StatusTypeList(WorkflowAdminDomain workFlow) throws Exception {
			for (StatusTypeDomain srcStatusType : workFlow.getStatusTypeList()) {
				statusType.add(new StatusTypeDefDTO(workFlow, srcStatusType));
			}
		}

		/**
		 * (wfHistoryDomain)コンストラクタ.
		 * 
		 * @param wfHistoryDomain 
		 * @param workFlow 
		 * @param objDomain 
		 * @param statusMap 
		 * @param eventMap
		 * @throws Exception 
		 */
		public StatusTypeList(WorkFlowHistoryDomain wfHistoryDomain, EIMWorkFlow workFlow, ObjectDomain objDomain
				, Map<Long, StatusDomain> statusMap, Map<Long, List<EventLogDomain>> eventMap) throws Exception {

			attr = new Attr(workFlow,objDomain);

			//ステータスタイプ一覧
			List statusTypeList = workFlow.getStatusTypeList();

			for(int i = 0; i < statusTypeList.size(); i++)
			{
				EIMStatusType eimStatusType = (EIMStatusType)statusTypeList.get(i);
				statusType.add(new StatusTypeDefDTO(wfHistoryDomain, eimStatusType,objDomain,statusMap,eventMap));
			}
		}

		public List<StatusTypeDefDTO> getStatusType() {
			return statusType;
		}

		public void setStatusType(List<StatusTypeDefDTO> statusType) {
			this.statusType = statusType;
		}

		public Attr getAttr() {
			return attr;
		}

		public void setAttr(Attr attr) {
			this.attr = attr;
		}
	}
	
	/**
	 * Strutsで生成したXMLデータにおいて単数値のプロパティを保持する親要素です。
	 */
	private Attr attr = null;

	/**
	 * クライアントへ送信していたXMLデータにおいて、単数値のプロパティを保持する親要素を再現するためのクラスです。
	 */
	public class Attr {
		private String id = null;
		private String seq = null;
		private String kind = null;
		private String auto = null;

		private String statusTypeId = null;
		private String statusTypeName = null;
		private String statusKindId = null;
		private String currentStatus = null;
		private String skipFlag = null;
		private String step = null;
		
		/**
		 * コンストラクタ.
		 * 
		 * @param statusType 
		 * @throws Exception 
		 */
		private Attr(StatusTypeDomain statusType) throws Exception {
			setId(String.valueOf(statusType.getId()));
			setSeq(String.valueOf(statusType.getSeq()));
			setKind(String.valueOf(statusType.getStatusTypeKind().getId()));
			setAuto(String.valueOf(statusType.isAuto()));
		}

		/**
		 * コンストラクタ.
		 * 
		 * @param eimStatusType 
		 * @param objDomain
		 */
		public Attr(EIMStatusType eimStatusType, ObjectDomain objDomain) {
			setStatusTypeId(String.valueOf(eimStatusType.getId()));
			setStatusTypeName(eimStatusType.getName());
			setStatusKindId(String.valueOf(eimStatusType.getKind()));
			if (eimStatusType.getId() == objDomain.getStatus().getStatusType().getId()){
				setCurrentStatus("true");
			} else {
				setCurrentStatus("false");
			}
			setSkipFlag("false");
			setStep(String.valueOf(eimStatusType.getStep()));
		}

		public String getId() {
			return id;
		}

		public void setId(String id) {
			this.id = id;
		}

		public String getAuto() {
			return auto;
		}

		public void setAuto(String auto) {
			this.auto = auto;
		}

		public String getSeq() {
			return seq;
		}

		public void setSeq(String seq) {
			this.seq = seq;
		}

		public String getKind() {
			return kind;
		}

		public void setKind(String kind) {
			this.kind = kind;
		}

		public String getStatusTypeId() {
			return statusTypeId;
		}

		public void setStatusTypeId(String statusTypeId) {
			this.statusTypeId = statusTypeId;
		}

		public String getStatusTypeName() {
			return statusTypeName;
		}

		public void setStatusTypeName(String statusTypeName) {
			this.statusTypeName = statusTypeName;
		}

		public String getCurrentStatus() {
			return currentStatus;
		}

		public void setCurrentStatus(String currentStatus) {
			this.currentStatus = currentStatus;
		}

		public String getSkipFlag() {
			return skipFlag;
		}

		public void setSkipFlag(String skipFlag) {
			this.skipFlag = skipFlag;
		}

		public String getStep() {
			return step;
		}

		public void setStep(String step) {
			this.step = step;
		}

		public String getStatusKindId() {
			return statusKindId;
		}

		public void setStatusKindId(String statusKindId) {
			this.statusKindId = statusKindId;
		}
	}
	

	/** アサイン先エントリー */
	private AsEntryList asEntryList = null;

	/** 名称一覧 */
	private NameList nameList = null;

	/** attTypes */
	private AttTypes attTypes = null;

	/** eventList */
	private EventList eventList = null;

	/** userList */
	private UserList userList = null;
	
	/**
	 * (WorkflowAdminDomain)コンストラクタ<br>
	 * EIMStatusTypeが保持しているプロパティ値を設定します。<br>
	 * <ul>
	 * <li>アサイン先エントリーは空です。</li>
	 * <li>ステータス属性タイプ一覧は空です。</li>
	 * <li>ステータスタイプ名称一覧には、セッション言語分しか設定されていません。</li>
	 * </ul>
	 * @param workFlow 
	 * @param statusType ステータスタイプ
	 * @throws Exception
	 *
	 */
	public StatusTypeDefDTO(WorkflowAdminDomain workFlow, StatusTypeDomain statusType) throws Exception {

		attr = new Attr(statusType);
		
		nameList = new NameList(statusType.getNameList());
		asEntryList = new AsEntryList(statusType.getAssignEntry());
		
		// ステータスタイプ(カスタム属性(レイアウト)含む)を取得
		Map<Integer, StatusTypeLayoutDomain> statusTypeLayoutMap = workFlow.getStatusTypeLayoutMap();
		StatusTypeLayoutDomain stTypeLayout = statusTypeLayoutMap.get(statusType.getSeq());
		
		List<AttributeTypeLayoutDomain> attrTypeLayoutList = new ArrayList<AttributeTypeLayoutDomain>();
		
		if (stTypeLayout != null) {
			attrTypeLayoutList = 
				stTypeLayout.getAttributeLayoutList();
		}
		if (attrTypeLayoutList.size() > 0){
			attTypes = new AttTypes(attrTypeLayoutList, statusType);
		}
	}

	/** 
	 * (WorkFlowHistoryDomain)コンストラクタ.
	 *  
	 * @param wfHistoryDomain
	 * @param eimStatusType
	 * @param objDomain 
	 * @param statusMap 
	 * @param eventMap 
	 * @throws Exception 
	 * */
	public StatusTypeDefDTO(WorkFlowHistoryDomain wfHistoryDomain, EIMStatusType eimStatusType, ObjectDomain objDomain, Map<Long, StatusDomain> statusMap,
			Map<Long, List<EventLogDomain>> eventMap) throws Exception {
		
		attr = new Attr(eimStatusType, objDomain);
		
		// 現在のステータスよりも後の段階でない場合、情報を出力する
		// また、公開処理を行わず「公開処理中」ステータスへ遷移しなかった場合はステータスが存在しないのでスキップ
		if (eimStatusType.getStep() <= objDomain.getStatus().getStatusType().getSeq() && statusMap.get((long)eimStatusType.getId()) != null){
			// 該当ステータスタイプのステータス・ステータスのアサイン先・ステータスが遷移元or遷移先のイベントログ
			long statusTypeId = eimStatusType.getId();
			StatusDomain status = statusMap.get(statusTypeId);

			long statusId = status.getId();
			List<EventLogDomain> eventLogList = eventMap.get(statusId);
			
			// イベント実行情報
			eventList = new EventList(eventLogList);
			// アサイン先情報
			userList = new UserList(wfHistoryDomain,status,eimStatusType,eventLogList);
		}
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
	 * @return asEntryListを取得します。
	 */
	public AsEntryList getAsEntryList() {
		return asEntryList;
	}

	/**
	 * @param asEntryListを設定します。
	 */
	public void setAsEntryList(AsEntryList asEntryList) {
		this.asEntryList = asEntryList;
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
	 * @return attTypesを取得します。
	 */
	public AttTypes getAttTypes() {
		return attTypes;
	}

	/**
	 * @param attTypesを設定します。
	 */
	public void setAttTypes(AttTypes attTypes) {
		this.attTypes = attTypes;
	}

	/**
	 * @return eventListを取得します。
	 */
	public EventList getEventList() {
		return eventList;
	}

	/**
	 * @param eventListを設定します。
	 */
	public void setEventList(EventList eventList) {
		this.eventList = eventList;
	}

	/**
	 * @return userListを取得します。
	 */
	public UserList getUserList() {
		return userList;
	}

	/**
	 * @param userListを設定します。
	 */
	public void setUserList(UserList userList) {
		this.userList = userList;
	}
}
