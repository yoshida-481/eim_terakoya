package jp.co.ctc_g.eim.app.document.business.domain;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import jp.co.ctc_g.eim.framework.business.domain.AssignPlanDomain;
import jp.co.ctc_g.eim.framework.business.domain.BaseEventTypeDomain;
import jp.co.ctc_g.eim.framework.business.domain.BaseEventTypeExecDomain;
import jp.co.ctc_g.eim.framework.business.domain.BelongType;
import jp.co.ctc_g.eim.framework.business.domain.EntryDomain;
import jp.co.ctc_g.eim.framework.business.domain.GuardConditionExecDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework.business.domain.entity.StatusTypeDomain;

/**
 * 承認依頼情報ドメイン
 *
 */
public class ApprovalReqInfoDomain {

	/** 実行するベースイベントタイプID */
	private long baseEventTypeId = 0;

	/** ステータスの更新日付 */
	private long statusMDateLong = 0;

	/** ST遷移予測結果のステータスタイプID */
	private long forcastStatusTypeId = 0;

	/** 受信確認の有無 */
	private long reply = 0;

	/** 通知タイミング */
	private String timing = null;

	/** コメント */
	private String comment = null;

	/** ドキュメントorWF付きフォルダ */
	private long objId = 0;

	/** ステータス毎の処理依頼先 */
	private Map<Long, List<AssignPlanDomain>> approverPlanListMap = new HashMap<>();

	/** 公開通知先 */
	private List<String> publisherList = null;

	/** PDF出力設定ドメイン */
	private PDFSettingDomain pdfDomain = null;

	/** 承認不要WFの公開処理フラグ（true:承認不要WFの公開処理／false:それ以外） **/
	private boolean immediatePublicFlag = false;

	/** 公開通知タイミング */
	private String sendNotifyMailTiming = null;

	/** 公開通知コメント */
	private String publicComment = null;


	/**
	 * コンストラクタ
	 */
	ApprovalReqInfoDomain(){

	}

	/**
	 * コンストラクタ
	 */
	public ApprovalReqInfoDomain(BaseEventTypeExecDomain baseEventTypeExecDomain){
		this(baseEventTypeExecDomain.getObject(), baseEventTypeExecDomain.getBaseEventType(), baseEventTypeExecDomain.getParamMap());
	}

	/**
	 * コンストラクタ
	 */
	public ApprovalReqInfoDomain(GuardConditionExecDomain guardConditionExecDomain){
		this(guardConditionExecDomain.getObject(), guardConditionExecDomain.getEventType().getBaseEventType(), guardConditionExecDomain.getParamMap());
	}

	private ApprovalReqInfoDomain(ObjectDomain objectDomain, BaseEventTypeDomain baseEventTypeDomain, Map<String,Object> paramMap){
		//マップから値を取り出しフィールドにセットする
		this.baseEventTypeId		= baseEventTypeDomain.getId();
		//this.statusMDateLong		= baseEventTypeExecDomain.getObject().getStatus().getMDate().getTime();
		if (paramMap.get("forcastStatusTypeId") != null){
			this.forcastStatusTypeId	= !((String)paramMap.get("forcastStatusTypeId")).equals("") ? Long.parseLong((String)paramMap.get("forcastStatusTypeId")) : 0 ;
		}	
		if (paramMap.get("reply") != null){
			this.reply = !((String)paramMap.get("reply")).equals("") ? Long.parseLong((String)paramMap.get("reply")) : 0 ;
		}
		this.timing				= (String)paramMap.get("timing") != null ? (String)paramMap.get("timing") : null ;
		this.comment			= (String)paramMap.get("comment") != null ? (String)paramMap.get("comment") : null ;
		this.objId				= objectDomain.getId();
		String immediatePublicStr = (String)paramMap.get("immediatePublicFlag");
		this.immediatePublicFlag = immediatePublicStr != null && immediatePublicStr.equals("true");

		//処理依頼先
		String prmApproverId = (String)paramMap.get("approverId");
		if(prmApproverId != null && !prmApproverId.equals("")){
			//カンマ区切りを分割する
			String[] prmApproverArray = prmApproverId.split(",");
			for(String prmApproverValue : prmApproverArray){
				// {ステータスタイプID}:{エントリタイプ}:{承認者ユーザID}	※承認者無しの場合は-1
				String[] token = prmApproverValue.split(":");

				// 指定されたステータスタイプIDを取得し、保管先を生成する
				Long statusTypeId = Long.parseLong(token[0]);
				if (!approverPlanListMap.containsKey(statusTypeId)) {
					approverPlanListMap.put(statusTypeId, new ArrayList<AssignPlanDomain>());
				}

				// 指定された承認者IDを取得する
				String  approverId = token[2];

				// 承認依頼先の指定がない時(承認者IDが-1の時)は空にする
				if(approverId.equals("-1")){
					continue;
				}

				// アサイン予定を生成する
				AssignPlanDomain assignPlan = new AssignPlanDomain();
				assignPlan.setType(BelongType.USER);

				ObjectDomain object = new ObjectDomain();
				object.setId(this.objId);
				assignPlan.setObject(object);

				StatusTypeDomain statusType = new StatusTypeDomain();
				statusType.setId(statusTypeId);
				assignPlan.setStatusType(statusType);

				EntryDomain entry = new EntryDomain();
				entry.setId(Long.parseLong(approverId));
				assignPlan.setOwner(entry);

				// 各ステータスタイプ毎のアサイン予定を保管する
				approverPlanListMap.get(statusTypeId).add(assignPlan);
			}
		}

		//公開通知先
		String prmPublisherId = (String)paramMap.get("publisherId");
		this.publisherList = new ArrayList<String>();
		if(prmPublisherId != null && !prmPublisherId.equals("")){
			//カンマ区切りを分割する
			 String[] tokensComma = prmPublisherId.split(",");
			 for(String tokenComma : tokensComma){
				 this.publisherList.add(tokenComma);
			 }
		}

		//PDF出力設定ドメイン
		this.pdfDomain = new PDFSettingDomain(paramMap);
		// 公開通知タイミング
		this.sendNotifyMailTiming = (String)paramMap.get("sendNotifyMailTiming");
		// 公開通知コメント
		this.publicComment = (String)paramMap.get("publicComment");

	}


	/**
	 * @return 実行するベースイベントタイプ
	 */
	public long getBaseEventTypeId() {
		return this.baseEventTypeId;
	}

	/**
	 * @param baseEventTypeDomain 実行するベースイベントタイプ
	 */
	public void setBaseEventTypeId(long baseEventTypeId) {
		this.baseEventTypeId= baseEventTypeId;
	}

	/**
	 * @return ステータスの更新日付
	 */
	public long getStatusMDateLong() {
		return this.statusMDateLong;
	}

	/**
	 * @param statusMDateLong ステータスの更新日付
	 */
	public void setStatusMDateLong(long statusMDateLong) {
		this.statusMDateLong= statusMDateLong;
	}
	/**
	 * @return ST遷移予測結果のステータスタイプID
	 */
	public long getForcastStatusTypeId() {
		return this.forcastStatusTypeId;
	}

	/**
	 * @param statusMDateLong ST遷移予測結果のステータスタイプID
	 */
	public void setForcastStatusTypeId(long forcastStatusTypeId) {
		this.forcastStatusTypeId= forcastStatusTypeId;
	}
	/**
	 * @return 受信確認有無（0:チェック無し／1:チェックあり）
	 */
	public long getReply() {
		return reply;
	}

	/**
	 * @param reply 受信確認有無
	 */
	public void setReply(long reply) {
		this.reply = reply;
	}

	/**
	 * @return メール通知タイミング（0:即時／1:定時／3:なし）
	 */
	public String getTiming() {
		return timing;
	}

	/**
	 * @param timing メール通知タイミング
	 */
	public void setTiming(String timing) {
		this.timing = timing;
	}

	/**
	 * @return the コメント
	 */
	public String getComment() {
		return comment;
	}

	/**
	 * @param comment コメント
	 */
	public void setComment(String comment) {
		this.comment = comment;
	}

	/**
	 * @return objId ドキュメント、またはWF付きフォルダのオブジェクトID
	 */
	public long getObject() {
		return this.objId;
	}

	/**
	 * @param objId ドキュメント、またはWF付きフォルダのオブジェクトID
	 */
	public void setObject(long objId) {
		this.objId = objId;
	}

	/**
	 * @return ステータスタイプ毎の処理依頼先
	 */
	public Map<Long, List<AssignPlanDomain>> getApproverPlanListMap() {
		return approverPlanListMap;
	}

	/**
	 * @return 公開通知先
	 */
	public List<String> getPublisherList() {
		return publisherList;
	}

	/**
	 * @param publisherList 公開通知先
	 */
	public void setPublisherList(List<String> publisherList) {
		this.publisherList = publisherList;
	}

	/**
	 * @return PDF出力設定ドメイン
	 */
	public PDFSettingDomain getPdfDomain() {
		return pdfDomain;
	}

	/**
	 * @param pdfDomain PDF出力設定ドメイン
	 */
	public void setPdfDomain(PDFSettingDomain pdfDomain) {
		this.pdfDomain = pdfDomain;
	}

	/**
	 * @return 承認不要WFの公開処理フラグ（true:承認不要WFの公開処理／false:それ以外）
	 */
	public boolean getImmediatePublicFlag() {
		return immediatePublicFlag;
	}

	/**
	 * @param immediatePublicFlag 承認不要WFの公開処理フラグ
	 */
	public void setImmediatePublicFlag(boolean immediatePublicFlag) {
		this.immediatePublicFlag = immediatePublicFlag;
	}

	/**
	 * @return 公開通知タイミング
	 */
	public String getSendNotifyMailTiming() {
		return sendNotifyMailTiming;
	}

	/**
	 * @param sendNotifyMailTiming 公開通知タイミング
	 */
	public void setSendNotifyMailTiming(String sendNotifyMailTiming) {
		this.sendNotifyMailTiming = sendNotifyMailTiming;
	}

	/**
	 * @return 公開通知コメント
	 */
	public String getPublicComment() {
		return publicComment;
	}

	/**
	 * @param publicComment 公開通知コメント
	 */
	public void setPublicComment(String publicComment) {
		this.publicComment = publicComment;
	}
}
