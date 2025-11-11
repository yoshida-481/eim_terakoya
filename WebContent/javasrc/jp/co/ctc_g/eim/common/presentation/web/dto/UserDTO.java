package jp.co.ctc_g.eim.common.presentation.web.dto;

import org.springframework.context.ApplicationContext;

import common.util.AppConstant;
import eim.net.EIMSession;
import eim.util.DateUtils;
import eim.util.EIMConfig;
import eim.util.StringUtils;
import jp.co.ctc_g.eim.framework.business.dao.SysFuncDao;
import jp.co.ctc_g.eim.framework.business.dao.UserDefGroupDao;
import jp.co.ctc_g.eim.framework.business.domain.AssignDomain;
import jp.co.ctc_g.eim.framework.business.domain.BaseEventTypeDomain;
import jp.co.ctc_g.eim.framework.business.domain.BelongDomain;
import jp.co.ctc_g.eim.framework.business.domain.BelongType;
import jp.co.ctc_g.eim.framework.business.domain.EventLogDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.EventDomain;
import jp.co.ctc_g.eim.framework.common.util.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * イベント実行者情報DTO
 * <p>
 * Struts->SpringMVCへの移行に際して、クライアントへ送信するXMLデータを再現したデータ転送用オブジェクトです。
 */
public class UserDTO {

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
	}
	/**
	 * クライアントへ送信していたXMLデータにおいて、単数値のプロパティを保持する親要素を再現するためのクラスです。
	 */
	public class Attr {

		private String date = null;
		private String userName = null;
		private String belong = null;
		private String comment = null;
		private String publicCommentLog = null;
		private String noCommentEvent = null;
		private String noPublicCommentEvent = null;
		
		/**
		 * コンストラクタ
		 *
		 * @param eventLogDomain
		 * @throws Exception
		 */
		private Attr(EventLogDomain eventLogDomain) throws Exception {

			EIMSession sess = EIMThreadContext.getEIMSession();
			
			BaseEventTypeDomain baseEventType = eventLogDomain.getEvent().getEventType().getBaseEventType();
			EventDomain event = eventLogDomain.getEvent();
			
			long baseEvtTypeId = baseEventType.getId();
	
			setDate(DateUtils.getDBTzToCLTzDate(sess, event.getCDate(), "EIM.FORMAT.DATETIME"));
			setUserName(StringUtils.xmlEncode(event.getCUser().createEIMUser().getName()));
			
			ApplicationContext contxt = ApplicationContextLoader.getContext();
			DataUtil util = new DataUtil(contxt);
			
			
			if (eventLogDomain.getAssign() != null) {
				String belongStr = util.getBelongString(eventLogDomain.getAssign());
				if (belongStr != null) {
					setBelong(StringUtils.xmlEncode(belongStr));
				}
			}
			
			String noCommentEvent = (baseEvtTypeId == AppConstant.BASE_EVENT_TYPE_ID_CANCEL_REQ_APPROVE 
					|| baseEvtTypeId == AppConstant.BASE_EVENT_TYPE_ID_PUBLIC)?"true":"false";
			
			String comment = (event.getAttribute(EIMConfig.getValue("EVENT_ATTR_NAME_COMMENT")) != null) ? 
					(String)event.getAttribute(EIMConfig.getValue("EVENT_ATTR_NAME_COMMENT")).getValues()[0]:"";
			setComment(StringUtils.xmlEncode(comment));
			
			setNoCommentEvent(noCommentEvent);
			String publicCommentLog = (event.getAttribute(EIMConfig.getValue("ATTR_NAME_PUBLIC_COMMENT_LOG")) != null) ? 
					(String)event.getAttribute(EIMConfig.getValue("ATTR_NAME_PUBLIC_COMMENT_LOG")).getValues()[0]:"";
			String noPublicCommentEvent = (baseEvtTypeId == AppConstant.BASE_EVENT_TYPE_ID_REQ_APPROVE 
					|| baseEvtTypeId == AppConstant.BASE_EVENT_TYPE_ID_APPROVAL)?"false":"true";
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

		public String getComment() {
			return comment;
		}

		public void setComment(String comment) {
			this.comment = comment;
		}

		public String getPublicCommentLog() {
			return publicCommentLog;
		}

		public void setPublicCommentLog(String publicCommentLog) {
			this.publicCommentLog = publicCommentLog;
		}

		public String getNoCommentEvent() {
			return noCommentEvent;
		}

		public void setNoCommentEvent(String noCommentEvent) {
			this.noCommentEvent = noCommentEvent;
		}

		public String getNoPublicCommentEvent() {
			return noPublicCommentEvent;
		}

		public void setNoPublicCommentEvent(String noPublicCommentEvent) {
			this.noPublicCommentEvent = noPublicCommentEvent;
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
	public UserDTO(EventLogDomain eventLogDomain) throws Exception {
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
