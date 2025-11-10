package jp.co.ctc_g.eim.common.presentation.web.dto;

import java.util.ArrayList;
import java.util.List;

import org.springframework.context.ApplicationContext;

import common.util.AppConstant;
import eim.bo.EIMResource;
import eim.net.EIMSession;
import eim.util.DateUtils;
import eim.util.EIMConfig;
import eim.util.StringUtils;
import jp.co.ctc_g.eim.framework.business.dao.SysFuncDao;
import jp.co.ctc_g.eim.framework.business.dao.UserDefGroupDao;
import jp.co.ctc_g.eim.framework.business.domain.AssignDomain;
import jp.co.ctc_g.eim.framework.business.domain.BelongDomain;
import jp.co.ctc_g.eim.framework.business.domain.BelongType;
import jp.co.ctc_g.eim.framework.business.domain.EventLogDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.EventDomain;
import jp.co.ctc_g.eim.framework.common.util.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * EventLogDTO
 * <p>
 * Struts->SpringMVCへの移行に際して、クライアントへ送信するXMLデータを再現したデータ転送用オブジェクトです。
 */
public class EventLogDTO {

	private class DataUtil
	{
		UserDefGroupDao userDefGroupDao;
		SysFuncDao sysFuncDao;
		
		// コンストラクタ
		DataUtil(ApplicationContext contxt) {
			
			userDefGroupDao = (UserDefGroupDao)contxt.getBean("userDefGroupDao");
			sysFuncDao =  (SysFuncDao)contxt.getBean("sysFuncDao");
		}
		
		// アサイン先から、所属元の文字列を生成する（画面表示用）
		public String getBelongString(AssignDomain assign) throws Exception
		{
			StringBuffer sb = new StringBuffer();
			for (BelongDomain belong : assign.getBelongList()) {
				
				// ユーザの場合は出力しない
				if (belong.getBelongType().getSymbol().equals(BelongType.USER.getSymbol()) ) {
					continue;
				}
				
				// 複数の所属元を出力する場合はパイプで区切る
				if (sb.length() > 0) {sb.append(" | ");}
				
				// 所属元の名称を取得して追加
				if (belong.getBelongType().getSymbol().equals(BelongType.USERDEFGROUP.getSymbol()) ) {
					
					sb.append(userDefGroupDao.getById(belong.getBelonging().getId()).getName());
				}
				else if (belong.getBelongType().getSymbol().equals(BelongType.SYSFUNC.getSymbol())) {
					
					sb.append(sysFuncDao.getById(belong.getBelonging().getId()).getName());
				}
				else {
					sb.append(belong.getBelonging().getName());
				}
			}
			
			return (sb.length() > 0)? sb.toString():null;
		}
		
		// ベースイベントタイプ名をアプリケーションに適した名称にして返却する
		public String getBaseEventTypeName(EIMSession sess, EventDomain event)
		{
			String ret;
			switch((int)event.getEventType().getBaseEventType().getId()) {
			
			case AppConstant.BASE_EVENT_TYPE_ID_REQ_APPROVE :
				ret = EIMResource.getMessage(sess, "EIM.ACCESS.TYPE.APPROVALREQUEST");
				break;
			case AppConstant.BASE_EVENT_TYPE_ID_APPROVAL :
				ret = EIMResource.getMessage(sess, "EIM.ACCESS.TYPE.APPROVAL");
				break;
			case AppConstant.BASE_EVENT_TYPE_ID_CANCEL_REQ_APPROVE :
				ret = EIMResource.getMessage(sess, "EIM.ACCESS.TYPE.APPROVALREQUESTCANCEL");
				break;
			case AppConstant.BASE_EVENT_TYPE_ID_TAKE_BACK :
				ret = EIMResource.getMessage(sess, "EIM.ACCESS.TYPE.APPROVALTAKEBACK");
				break;
			case AppConstant.BASE_EVENT_TYPE_ID_SEND_BACK :
				ret = EIMResource.getMessage(sess, "EIM.ACCESS.TYPE.SENDBACK");
				break;
			case AppConstant.BASE_EVENT_TYPE_ID_PUBLIC :
				ret = EIMResource.getMessage(sess, "EIM.ACCESS.TYPE.PUBLIC");
				break;
			default :
				ret = event.getEventType().getBaseEventType().getName();
				break;
		}
			return ret;
		}
	}
	
	/**
	 * Strutsで生成したXMLデータにおける配列要素を再現するためのクラスです。
	 */
	public static class EventLogList {

		private List<EventLogDTO> event = new ArrayList<EventLogDTO>();

		/**
		 * コンストラクタ
		 * 
		 * @param eventLogList
		 * @throws Exception 
		 */
		public EventLogList(List<EventLogDomain> eventLogList) throws Exception {
			for (EventLogDomain eventLogDomain : eventLogList) {
				event.add(new EventLogDTO(eventLogDomain));
			}
		}

		public List<EventLogDTO> getEvent() {
			return event;
		}

		public void setEvent(List<EventLogDTO> event) {
			this.event = event;
		}
	}

	/**
	 * クライアントへ送信していたXMLデータにおいて、単数値のプロパティを保持する親要素を再現するためのクラスです。
	 */
	public class Attr {

		private String date = null;
		private String userName = null;
		private String belong = null;
		private String baseEvtType = null;
		private String baseEvtTypeId = null;
		private String toStatus = null;
		private String toStatusId = null;
		private String toStatusStep = null;
		private String fromStatus = null;
		private String fromStatusId = null;
		private String comment = null;
		private String noCommentEvent = null;
		private String publicCommentLog = null;
		private String noPublicCommentEvent = null;

		private Attr(EventLogDomain eventLogDomain) throws Exception {
			EIMSession sess = EIMThreadContext.getEIMSession();
			
			ApplicationContext contxt = ApplicationContextLoader.getContext();
			DataUtil util = new DataUtil(contxt);
			long baseEvtTypeId = eventLogDomain.getEvent().getEventType().getBaseEventType().getId();
			
			setDate(DateUtils.getDBTzToCLTzDate(sess, eventLogDomain.getEvent().getCDate(), "EIM.FORMAT.DATETIME"));
			setUserName(StringUtils.xmlEncode(eventLogDomain.getEvent().getCUser().createEIMUser().getName()));
			if (eventLogDomain.getAssign() != null) {
				String belongStr = util.getBelongString(eventLogDomain.getAssign());
				if (belongStr != null) {
					setBelong(StringUtils.xmlEncode(belongStr));
				}
			}
			
			setBaseEvtType(util.getBaseEventTypeName(sess, eventLogDomain.getEvent()));
			setBaseEvtTypeId(String.valueOf(eventLogDomain.getEvent().getEventType().getBaseEventType().getId()));
			setToStatus(StringUtils.xmlEncode(eventLogDomain.getEvent().getToStatus().getStatusType().createEIMStatusType().getName()));
			setToStatusId(String.valueOf(eventLogDomain.getEvent().getToStatus().getStatusType().createEIMStatusType().getId()));
			setToStatusStep(String.valueOf(eventLogDomain.getEvent().getToStatus().getStatusType().createEIMStatusType().getStep()));
			setFromStatus(StringUtils.xmlEncode(eventLogDomain.getEvent().getFromStatus().getStatusType().createEIMStatusType().getName()));
			setFromStatusId(String.valueOf(eventLogDomain.getEvent().getFromStatus().getStatusType().createEIMStatusType().getId()));
			
			String noCommentEvent = (baseEvtTypeId == AppConstant.BASE_EVENT_TYPE_ID_CANCEL_REQ_APPROVE
					|| baseEvtTypeId == AppConstant.BASE_EVENT_TYPE_ID_PUBLIC)?"true":"false";
			String comment = (eventLogDomain.getEvent().getAttribute(EIMConfig.getValue("EVENT_ATTR_NAME_COMMENT")) != null)?
					(String)eventLogDomain.getEvent().getAttribute(EIMConfig.getValue("EVENT_ATTR_NAME_COMMENT")).getValues()[0]:"";
			
			setComment(StringUtils.xmlEncode(comment));
			setNoCommentEvent(noCommentEvent);
			
			String noPublicCommentEvent = (baseEvtTypeId == AppConstant.BASE_EVENT_TYPE_ID_REQ_APPROVE || 
					baseEvtTypeId == AppConstant.BASE_EVENT_TYPE_ID_APPROVAL)?"false":"true";
			String publicCommentLog = (eventLogDomain.getEvent().getAttribute(EIMConfig.getValue("ATTR_NAME_PUBLIC_COMMENT_LOG")) != null)?
					(String)eventLogDomain.getEvent().getAttribute(EIMConfig.getValue("ATTR_NAME_PUBLIC_COMMENT_LOG")).getValues()[0]:"";
			
			setPublicCommentLog(StringUtils.xmlEncode(publicCommentLog));
			setNoPublicCommentEvent(noPublicCommentEvent);
		}

		public String getDate() {
			return date;
		}

		public void setDate(String date) {
			this.date = date;
		}

		public String getUserName() {
			return userName;
		}

		public void setUserName(String userName) {
			this.userName = userName;
		}

		public String getBelong() {
			return belong;
		}

		public void setBelong(String belong) {
			this.belong = belong;
		}

		public String getBaseEvtType() {
			return baseEvtType;
		}

		public void setBaseEvtType(String baseEvtType) {
			this.baseEvtType = baseEvtType;
		}

		public String getBaseEvtTypeId() {
			return baseEvtTypeId;
		}

		public void setBaseEvtTypeId(String baseEvtTypeId) {
			this.baseEvtTypeId = baseEvtTypeId;
		}

		public String getToStatus() {
			return toStatus;
		}

		public void setToStatus(String toStatus) {
			this.toStatus = toStatus;
		}

		public String getToStatusStep() {
			return toStatusStep;
		}

		public void setToStatusStep(String toStatusStep) {
			this.toStatusStep = toStatusStep;
		}

		public String getFromStatus() {
			return fromStatus;
		}

		public void setFromStatus(String fromStatus) {
			this.fromStatus = fromStatus;
		}

		public String getFromStatusId() {
			return fromStatusId;
		}

		public void setFromStatusId(String fromStatusId) {
			this.fromStatusId = fromStatusId;
		}


		public String getComment() {
			return comment;
		}


		public void setComment(String comment) {
			this.comment = comment;
		}


		public String getNoCommentEvent() {
			return noCommentEvent;
		}


		public void setNoCommentEvent(String noCommentEvent) {
			this.noCommentEvent = noCommentEvent;
		}


		public String getPublicCommentLog() {
			return publicCommentLog;
		}


		public void setPublicCommentLog(String publicCommentLog) {
			this.publicCommentLog = publicCommentLog;
		}


		public String getNoPublicCommentEvent() {
			return noPublicCommentEvent;
		}


		public void setNoPublicCommentEvent(String noPublicCommentEvent) {
			this.noPublicCommentEvent = noPublicCommentEvent;
		}

		public String getToStatusId() {
			return toStatusId;
		}

		public void setToStatusId(String toStatusId) {
			this.toStatusId = toStatusId;
		}
	}

	/**
	 * Strutsで生成したXMLデータにおいて単数値のプロパティを保持する親要素です。
	 */
	private Attr attr = null;

	/**
	 * コンストラクタ
	 *
	 * @param eventLogDomain 
	 * @throws Exception
	 */
	public EventLogDTO(EventLogDomain eventLogDomain) throws Exception {
		setAttr(new Attr(eventLogDomain));
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
