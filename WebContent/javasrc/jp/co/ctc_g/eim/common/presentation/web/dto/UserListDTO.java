package jp.co.ctc_g.eim.common.presentation.web.dto;

import java.util.ArrayList;
import java.util.List;

import common.util.AppConstant;
import eim.bo.EIMStatusType;
import eim.util.StringUtils;
import jp.co.ctc_g.eim.app.document.business.domain.WorkFlowHistoryDomain;
import jp.co.ctc_g.eim.framework.business.domain.AssignDomain;
import jp.co.ctc_g.eim.framework.business.domain.EventLogDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.EventDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.EventTypeDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.StatusDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.WorkFlowDomain;

/**
 * アサイン先情報DTO
 * <p>
 * Struts->SpringMVCへの移行に際して、クライアントへ送信するXMLデータを再現したデータ転送用オブジェクトです。
 */
public class UserListDTO {

	/**
	 * Strutsで生成したXMLデータにおける配列要素を再現するためのクラスです。
	 */
	public static class UserList {
		/** アサイン先リスト */
		private List<UserListDTO> user = new ArrayList<UserListDTO>();

		/**
		 * コンストラクタ.
		 * 
		 * @param wfHistoryDomain 
		 * @param status 
		 * @param statusType 
		 * @param eventLogList
		 * @throws Exception
		 */
		public UserList(WorkFlowHistoryDomain wfHistoryDomain, StatusDomain status, EIMStatusType statusType, 
				List<EventLogDomain> eventLogList) throws Exception {
			
			List<AssignDomain> assignList = status.getAssignList();
			
			// 「全員承認」ステータスの場合"true"
			String isMust = "false";
			// 「承認」ステータスフラグ
			boolean isApproveStatus = false;
			if(status.getStatusType().getStatusTypeKind().getId() == AppConstant.STATUS_TYPE_KIND_ID_APPROVE){
				isApproveStatus = true;
				WorkFlowDomain wfDomain = wfHistoryDomain.getWorkflow();
				
				// 遷移元ステータスタイプが一致しガード条件が「最後の承認者でない」のものがある場合、全員承認
				for (EventTypeDomain eventType : wfDomain.getEventTypeList()) {	
					if (eventType.getGuardCondition().getId() == AppConstant.GUARD_COND_ID_FINAL_APPROVED_IS_NOT
						&& eventType.getFromStatusType().getId() == statusType.getId()) {
						
						isMust = "true";
						break;
					}
				}
			}
			
			// アサイン先情報
			for (AssignDomain stAssign : assignList) {
				String isApproved = (stAssign.getEvent() != null && isApproveStatus)?"true":"false";
				user.add(new UserListDTO(isApproved,isMust,stAssign.getType().getSymbol(),StringUtils.xmlEncode(stAssign.getOwner().getName())));
			}
			
			// 実行者情報
			for (EventLogDomain log : eventLogList) {
				EventDomain execEvent = log.getEvent();
				// 遷移元ステータスが一致しない場合、出力しない
				if (execEvent.getFromStatus().getId() != status.getId()) {
					continue;
				}
				// イベントが「承認依頼取消」の場合、出力しない
				if (execEvent.getEventType().getBaseEventType().getId() == AppConstant.BASE_EVENT_TYPE_ID_CANCEL_REQ_APPROVE) {
					continue;
				}
				// ステータスのアサイン先に含まれる場合、出力しない
				boolean isAssign = false;
				for (AssignDomain stAssign : assignList) {
					if (stAssign.getOwner().getId() == execEvent.getCUser().getId()) {
						isAssign = true;
						break;
					}
				}
				if (isAssign) {continue;}

				user.add(new UserListDTO("false","false","user",StringUtils.xmlEncode(execEvent.getCUser().createEIMUser().getName())));
			}
		}

		public List<UserListDTO> getUser() {
			return user;
		}

		public void setUser(List<UserListDTO> user) {
			this.user = user;
		}
	}
	
	/**
	 * クライアントへ送信していたXMLデータにおいて、単数値のプロパティを保持する親要素を再現するためのクラスです。
	 */
	public class Attr {

		private String approve = null;
		private String entryType = null;
		private String must = null;
		private String name = null;

		/**
		 * コンストラクタ.
		 * 
		 * @param isApproved 
		 * @param isMust 
		 * @param entryType2 
		 * @param name2
		 */
		public Attr(String isApproved, String isMust, String entryType2, String name2) {
			setApprove(isApproved);
			setEntryType(entryType2);
			setMust(isMust);
			setName(name2);
		}

		public String getApprove() {
			return approve;
		}

		public void setApprove(String approve) {
			this.approve = approve;
		}

		public String getEntryType() {
			return entryType;
		}

		public void setEntryType(String entryType) {
			this.entryType = entryType;
		}

		public String getMust() {
			return must;
		}

		public void setMust(String must) {
			this.must = must;
		}

		public String getName() {
			return name;
		}

		public void setName(String name) {
			this.name = name;
		}
	}

	/**
	 * Strutsで生成したXMLデータにおいて単数値のプロパティを保持する親要素です。
	 */
	private Attr attr = null;

	/**
	 * コンストラクタ.
	 *
	 * @param isApproved 
	 * @param isMust 
	 * @param entryType 
	 * @param name
	 */
	public UserListDTO(String isApproved, String isMust, String entryType, String name) {
		setAttr(new Attr(isApproved,isMust,entryType,name));
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
