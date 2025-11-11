package jp.co.ctc_g.eim.common.presentation.web.dto;

import java.util.ArrayList;
import java.util.List;

import common.util.AppConstant;
import jp.co.ctc_g.eim.common.presentation.web.dto.NoticeMailDTO.MailList;
import jp.co.ctc_g.eim.common.presentation.web.dto.OtherNameDTO.NameList;
import jp.co.ctc_g.eim.framework.business.domain.entity.EventTypeDomain;

/**
 * イベントタイプDTO
 * <p>
 * Struts->SpringMVCへの移行に際して、クライアントへ送信していたXMLデータを再現したデータ転送用オブジェクトです。
 *
 */
public class EventTypeDefDTO {

	/**
	 * Strutsで生成したXMLデータにおける配列要素を再現するためのクラスです。
	 */
	public static class EventTypeList {
		/** イベントタイプリスト */
		private List<EventTypeDefDTO> eventType = new ArrayList<EventTypeDefDTO>();

		/**
		 * コンストラクタ.
		 * 
		 * @param eventTypeList
		 * @param statusListSize 
		 * @throws Exception
		 */
		public EventTypeList(List<EventTypeDomain> eventTypeList, int statusListSize) throws Exception {
			for (EventTypeDomain eventTypeDomain : eventTypeList) {
				String skipFlag = "false";
				
				if(eventTypeDomain.getBaseEventType().getId() == AppConstant.BASE_EVENT_TYPE_ID_APPROVAL 
						|| eventTypeDomain.getBaseEventType().getId() == AppConstant.BASE_EVENT_TYPE_ID_REQ_APPROVE){
					// イベントタイプが承認依頼、もしくは承認の場合
					// 最終承認から出ている承認イベントと公開処理中へのイベントは無視する
					if(eventTypeDomain.getFromStatusType().getSeq() == statusListSize -2 
						|| eventTypeDomain.getToStatusType().getSeq() == statusListSize -1){
						// スキップイベントではない
						skipFlag = "false";
					}else{
						if(eventTypeDomain.getFromStatusType().getSeq() < eventTypeDomain.getToStatusType().getSeq() -1 ){
							skipFlag = "true";
						}
					}
				}else{
					// スキップイベントではない
					skipFlag = "false";
				}
				
				eventType.add(new EventTypeDefDTO(eventTypeDomain, skipFlag));
			}
		}

		public List<EventTypeDefDTO> getEventType() {
			return eventType;
		}

		public void setEventType(List<EventTypeDefDTO> eventType) {
			this.eventType = eventType;
		}
	}

	/** 名称一覧 */
	private NameList nameList = null;

	/** 通知メール一覧 */
	private MailList mailList = null;

	/**
	 * Strutsで生成したXMLデータにおいて単数値のプロパティを保持する親要素です。
	 */
	private Attr attr = null;

	/**
	 * クライアントへ送信していたXMLデータにおいて、単数値のプロパティを保持する親要素を再現するためのクラスです。
	 */
	public class Attr {

		/** イベントタイプID */
		private String id = null;

		/** イベントタイプ名称（デフォルト）*/
		private String defName = null;

		/** 同一Fromステータスタイプ、同一ベースイベント内での優先度 */
		private String seq = null;

		/** Fromステータスタイプ */
		private String fromStatusTypeId = null;
		private String fromStatusTypeSeq = null;

		/** Toステータスタイプ */
		private String toStatusTypeId = null;
		private String toStatusTypeSeq = null;

		/** 継承するベースイベントタイプ */
		private String baseEventTypeId = null;

		/** 設定されているガード条件 */
		private String guardConditionId = null;

		/** スキップフラグ */
		private String skipFlag = "false";

		/**
		 * コンストラクタ.
		 * 
		 * @param eventType
		 * @param skipFlag2 
		 * @throws Exception
		 */
		private Attr(EventTypeDomain eventType, String skipFlag2) throws Exception {
			id = String.valueOf(eventType.getId());
			defName = eventType.getDefName();
			seq = String.valueOf(eventType.getSeq());
			fromStatusTypeId = String.valueOf(eventType.getFromStatusType().getId());
			fromStatusTypeSeq = String.valueOf(eventType.getFromStatusType().getSeq());
			toStatusTypeId = String.valueOf(eventType.getToStatusType().getId());
			toStatusTypeSeq = String.valueOf(eventType.getToStatusType().getSeq());
			baseEventTypeId = String.valueOf(eventType.getBaseEventType().getId());
			guardConditionId = String.valueOf(eventType.getGuardCondition().getId());
			skipFlag = skipFlag2;
		}

		public String getId() {
			return id;
		}

		public void setId(String id) {
			this.id = id;
		}

		public String getDefName() {
			return defName;
		}

		public void setDefName(String defName) {
			this.defName = defName;
		}

		public String getSeq() {
			return seq;
		}

		public void setSeq(String seq) {
			this.seq = seq;
		}

		public String getFromStatusTypeId() {
			return fromStatusTypeId;
		}

		public void setFromStatusTypeId(String fromStatusTypeId) {
			this.fromStatusTypeId = fromStatusTypeId;
		}

		public String getFromStatusTypeSeq() {
			return fromStatusTypeSeq;
		}

		public void setFromStatusTypeSeq(String fromStatusTypeSeq) {
			this.fromStatusTypeSeq = fromStatusTypeSeq;
		}

		public String getToStatusTypeId() {
			return toStatusTypeId;
		}

		public void setToStatusTypeId(String toStatusTypeId) {
			this.toStatusTypeId = toStatusTypeId;
		}

		public String getToStatusTypeSeq() {
			return toStatusTypeSeq;
		}

		public void setToStatusTypeSeq(String toStatusTypeSeq) {
			this.toStatusTypeSeq = toStatusTypeSeq;
		}

		public String getBaseEventTypeId() {
			return baseEventTypeId;
		}

		public void setBaseEventTypeId(String baseEventTypeId) {
			this.baseEventTypeId = baseEventTypeId;
		}

		public String getGuardConditionId() {
			return guardConditionId;
		}

		public void setGuardConditionId(String guardConditionId) {
			this.guardConditionId = guardConditionId;
		}
		public String getSkipFlag() {
			return skipFlag;
		}
		public void setSkipFlag(String skipFlag) {
			this.skipFlag = skipFlag;
		}

	}
	
	/**
	 * コンストラクタ.
	 * 
	 * @param eventType
	 * @param skipFlag
	 * @throws Exception
	 */
	public EventTypeDefDTO(EventTypeDomain eventType, String skipFlag) throws Exception {

		attr = new Attr(eventType,skipFlag);

		nameList = new NameList(eventType.getNameList());
		mailList = new MailList(eventType.getMailList());
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
	 * @return nameListを取得します。
	 */
	public MailList getMailList() {
		return mailList;
	}

	/**
	 * @param mailListを設定します。
	 */
	public void setMailList(MailList mailList) {
		this.mailList = mailList;
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

}
