package jp.co.ctc_g.eim.app.document.presentation.dto;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import common.util.AppConstant;
import jp.co.ctc_g.eim.app.document.business.domain.WorkFlowDocDomain;
import jp.co.ctc_g.eim.common.presentation.web.dto.AssignEntryDTO.AsEntryList;
import jp.co.ctc_g.eim.common.presentation.web.dto.OtherNameDTO.NameList;
import jp.co.ctc_g.eim.framework.business.domain.entity.EventTypeDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.StatusTypeDomain;

/**
 * ステータスタイプDTO
 * <p>
 * Struts->SpringMVCへの移行に際して、クライアントへ送信するXMLデータを再現したデータ転送用オブジェクトです。
 */
public class StatusTypeDefDocDTO {

	/**
	 * Strutsで生成したXMLデータにおける配列要素を再現するためのクラスです。
	 */
	public static class StatusTypeList {
		/** ステータスタイプリスト */
		private List<StatusTypeDefDocDTO> statusType = new ArrayList<StatusTypeDefDocDTO>();

		/**
		 * @param eventTypeList
		 * @throws Exception 
		 */
		public StatusTypeList(WorkFlowDocDomain workFlow) throws Exception {
			for (StatusTypeDomain srcStatusType : workFlow.getStatusTypeList()) {
				statusType.add(new StatusTypeDefDocDTO(workFlow, srcStatusType));
			}
		}

		/**
		 * @return
		 */
		public List<StatusTypeDefDocDTO> getStatusType() {
			return statusType;
		}

		/**
		 * @param statusType
		 */
		public void setStatusType(List<StatusTypeDefDocDTO> statusType) {
			this.statusType = statusType;
		}
	}

	/**
	 * クライアントへ送信していたXMLデータにおいて、単数値のプロパティを保持する親要素を再現するためのクラスです。
	 */
	public class Attr {
		/** ステータスタイプID */
		private String id;
		/** ステータスタイプの並び順 */
		private String seq;
		/** ステータスタイプ種別 */
		private String kind;
		/** アサイン登録 */
		private String auto;
		private String defBossOnly;
		private String enableCheckIn;
		private String through;
		
		private Attr(StatusTypeDomain statusType, String through2, String enableCheckIn2, String bossOnly) throws Exception {
			setId(String.valueOf(statusType.getId()));
			setSeq(String.valueOf(statusType.getSeq()));
			setKind(String.valueOf(statusType.getStatusTypeKind().getId()));
			setAuto(String.valueOf(statusType.isAuto()));
			setDefBossOnly(bossOnly);
			setEnableCheckIn(enableCheckIn2);
			setThrough(through2);
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

		public String getDefBossOnly() {
			return defBossOnly;
		}

		public void setDefBossOnly(String defBossOnly) {
			this.defBossOnly = defBossOnly;
		}

		public String getEnableCheckIn() {
			return enableCheckIn;
		}

		public void setEnableCheckIn(String enableCheckIn) {
			this.enableCheckIn = enableCheckIn;
		}

		public String getThrough() {
			return through;
		}

		public void setThrough(String through) {
			this.through = through;
		}

	}
	

	/** アサイン先エントリー */
	private AsEntryList asEntryList = null;

	/** 名称一覧 */
	private NameList nameList = null;

	/**
	 * Strutsで生成したXMLデータにおいて単数値のプロパティを保持する親要素です。
	 */
	private Attr attr = null;

	
	/**
	 * コンストラクタ<br>
	 * EIMStatusTypeが保持しているプロパティ値を設定します。<br>
	 * <ul>
	 * <li>アサイン先エントリーは空です。</li>
	 * <li>ステータス属性タイプ一覧は空です。</li>
	 * <li>ステータスタイプ名称一覧には、セッション言語分しか設定されていません。</li>
	 * </ul>
	 * @param workFlow 
	 *
	 * @param statusType ステータスタイプ
	 * @throws Exception
	 *
	 */
	public StatusTypeDefDocDTO(WorkFlowDocDomain workFlowDocDomain, StatusTypeDomain statusTypeDomain) throws Exception {

		String through = "0";
		if (statusTypeDomain.getStatusTypeKind().getId() == AppConstant.STATUS_TYPE_KIND_ID_EDITTING) {
			// 編集中
			through = String.valueOf(AppConstant.THROUGH_APPROVE_ONE);
		}
		else if (statusTypeDomain.getStatusTypeKind().getId() == AppConstant.STATUS_TYPE_KIND_ID_APPROVE) {

			List<EventTypeDomain> eventTypeDomainList = workFlowDocDomain.getEventTypeList();

			// 通過条件判定(false:一人, true:全員 を判定)
			boolean isAll = false;
			for (EventTypeDomain eventTypeDomain : eventTypeDomainList) {
				if (eventTypeDomain.getFromStatusType().getId() == statusTypeDomain.getId() &&
						eventTypeDomain.getToStatusType().getId() == statusTypeDomain.getId() &&
						eventTypeDomain.getGuardCondition().getId() == AppConstant.GUARD_COND_ID_FINAL_APPROVED_IS_NOT) {
					isAll = true;
					break;
				}
			}

			if(isAll){
				// 承認依頼中(全員)
				through = String.valueOf(AppConstant.THROUGH_APPROVE_ALL);
			}
			else {
				// 承認依頼中(一人)
				through = String.valueOf(AppConstant.THROUGH_APPROVE_ONE);
			}
		}
		else if (statusTypeDomain.getStatusTypeKind().getId() == AppConstant.STATUS_TYPE_KIND_ID_PROCESSING_PUBLIC) {
			// 公開処理中
			through = String.valueOf(AppConstant.THROUGH_APPROVE_NONE);
		}
		else if (statusTypeDomain.getStatusTypeKind().getId() == AppConstant.STATUS_TYPE_KIND_ID_PUBLIC) {
			// 公開済
			through = String.valueOf(AppConstant.THROUGH_APPROVE_NONE);
		}

		// 承認依頼中チェックイン可否を取得
		Map<Long, Long> enableCheckinStatusTypeMap = new HashMap<Long, Long>();
		long[] enableCheckinStatusTypeArr = workFlowDocDomain.getEnableCheckinStatusTypeArr();
		if (enableCheckinStatusTypeArr != null) {
			for (long statusTypeId : enableCheckinStatusTypeArr) {
				enableCheckinStatusTypeMap.put(statusTypeId, statusTypeId);
			}
		}
		
		// 承認依頼中チェックイン可否
		String enableCheckIn = String.valueOf(enableCheckinStatusTypeMap.containsKey((long)statusTypeDomain.getId()));
		
		// 上長のみ表示設定を取得
		Map<Long, Long> bossOnlyStatusTypeMap = new HashMap<Long, Long>();
		long[] bossOnlyStatusTypeArr = workFlowDocDomain.getDefBossOnlyStatusTypeArr();
		if (bossOnlyStatusTypeArr != null) {
			for (long statusTypeId : bossOnlyStatusTypeArr) {
				bossOnlyStatusTypeMap.put(statusTypeId, statusTypeId);
			}
		}

		// 上長のみ表示設定
		String bossOnly = String.valueOf(bossOnlyStatusTypeMap.containsKey((long)statusTypeDomain.getId()));

		
		attr = new Attr(statusTypeDomain,through,enableCheckIn,bossOnly);
		
		nameList = new NameList(statusTypeDomain.getNameList());
		asEntryList = new AsEntryList(statusTypeDomain.getAssignEntry());
	}

	/**
	 * @return nameList ステータスタイプ名称一覧
	 */
	public NameList getNameList() {
		return nameList;
	}

	/**
	 * @param nameList ステータスタイプ名称一覧
	 */
	public void setNameList(NameList nameList) {
		this.nameList = nameList;
	}


	/**
	 * @return asEntryList
	 */
	public AsEntryList getAsEntryList() {
		return asEntryList;
	}

	/**
	 * @param asEntryList セットする asEntryList
	 */
	public void setAsEntryList(AsEntryList asEntryList) {
		this.asEntryList = asEntryList;
	}

	/**
	 * @return attr
	 */
	public Attr getAttr() {
		return attr;
	}

	/**
	 * @param attr セットする attr
	 */
	public void setAttr(Attr attr) {
		this.attr = attr;
	}


}
