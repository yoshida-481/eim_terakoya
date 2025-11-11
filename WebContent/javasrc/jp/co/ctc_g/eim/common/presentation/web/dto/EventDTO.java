package jp.co.ctc_g.eim.common.presentation.web.dto;

import java.util.ArrayList;
import java.util.List;

import org.springframework.context.ApplicationContext;

import common.util.AppConstant;
import eim.bo.EIMResource;
import eim.net.EIMSession;
import jp.co.ctc_g.eim.framework.business.domain.EventLogDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.EventDomain;
import jp.co.ctc_g.eim.framework.common.util.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * イベント実行情報DTO
 * <p>
 * Struts->SpringMVCへの移行に際して、クライアントへ送信していたXMLデータを再現したデータ転送用オブジェクトです。
 *
 */
public class EventDTO {

	/**
	 * Strutsで生成したXMLデータにおける配列要素を再現するためのクラスです。
	 */
	public static class EventList {

		private List<EventDTO> event = new ArrayList<EventDTO>();

		/**
		 * @param eventTypeList
		 * @param statusListSize 
		 * @throws Exception
		 */
		public EventList(List<EventLogDomain> eventLogList) throws Exception {

			// イベント実行日のDB→CL時間変換にDB接続が必要
			EIMSession sess = EIMThreadContext.getEIMSession();
			
			ApplicationContext contxt = ApplicationContextLoader.getContext();
			DataUtil util = new DataUtil();
			
			// ベースイベントタイプ毎、遷移先ステータス別にまとめて出力する
			for (EventLogDomain eventTypeDomain : eventLogList) {
				String baseEventTypeName = util.getBaseEventTypeName(sess, eventTypeDomain.getEvent());
				event.add(new EventDTO(eventTypeDomain, baseEventTypeName));
			}
		}

		public List<EventDTO> getEvent() {
			return event;
		}

		public void setEvent(List<EventDTO> event) {
			this.event = event;
		}

		private class DataUtil
		{
			
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
	}
	
	/**
	 * Strutsで生成したXMLデータにおいて単数値のプロパティを保持する親要素です。
	 */
	private Attr attr = null;

	/**
	 * イベント実行者情報.
	 */
	private UserDTO user = null;

	
	/**
	 * クライアントへ送信していたXMLデータにおいて、単数値のプロパティを保持する親要素を再現するためのクラスです。
	 */
	public class Attr {

		/** イベントタイプID */
		private String baseEvtType = null;

		/**
		 * コンストラクタ.
		 * 
		 * @param baseEventTypeName
		 * @throws Exception
		 */
		private Attr(String baseEventTypeName) throws Exception {
			setBaseEvtType(baseEventTypeName);
			
		}

		public String getBaseEvtType() {
			return baseEvtType;
		}

		public void setBaseEvtType(String baseEvtType) {
			this.baseEvtType = baseEvtType;
		}

	}

	/**
	 * コンストラクタ.
	 * 
	 * @param eventLogDomain
	 * @param baseEvenTypeName
	 */
	public EventDTO(EventLogDomain eventLogDomain, String baseEventTypeName) throws Exception {
		setAttr(new Attr(baseEventTypeName));
		// イベント実行者情報のセット
		setUser(new UserDTO(eventLogDomain));
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
	 * @return userを取得します。
	 */
	public UserDTO getUser() {
		return user;
	}

	/**
	 * @param userを設定します。
	 */
	public void setUser(UserDTO user) {
		this.user = user;
	}

}
