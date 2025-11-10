package jp.co.ctc_g.eim.app.document.business.domain;

import java.util.List;
import java.util.ArrayList;
import jp.co.ctc_g.eim.framework.business.domain.MailMethod;

public class EventTypeDocDomain {
	/* メール種別ID(単数の場合使用) */
	long mailTypeID;
	
	/* メール種別IDリスト(複数の場合使用) */
	List<Long> mailTypeIDList = new ArrayList<Long>();
	
	/* メール通知方法(単数の場合使用) */
	MailMethod mailMethod;

	/* メール通知方法リスト(複数の場合使用) */
	List<MailMethod> mailMethodList = new ArrayList<MailMethod>();

	/* イベントタイプ名称(日本語) */
	String otherEventTypeNameJA;
	
	/* イベントタイプ名称(英語) */
	String otherEventTypeNameEN;

	/* イベント優先度 */
	int eventSeq;

	/* イベント始点ステータスタイプ順序 */
	int fromStatysTypeSeq;
	
	/* イベント終点ステータスタイプ順序 */
	int toStatysTypeSeq;
	
	/* ベースイベントタイプID */
	long baseEventTypeID;

	/* ガード条件ID */
	long guardCondID;
	
	/**
	 * @return mailMethod
	 */
	public MailMethod getMailMethod() {
		return mailMethod;
	}

	/**
	 * @param mailMethod セットする mailMethod
	 */
	public void setMailMethod(MailMethod mailMethod) {
		this.mailMethod = mailMethod;
	}

	/**
	 * @return mailTypeIDList
	 */
	public List<Long> getMailTypeIDList() {
		return mailTypeIDList;
	}

	/**
	 * @param mailTypeIDList セットする mailTypeIDList
	 */
	public void setMailTypeIDList(List<Long> mailTypeIDList) {
		this.mailTypeIDList = mailTypeIDList;
	}

	/**
	 * @return mailTypeID
	 */
	public long getMailTypeID() {
		return mailTypeID;
	}

	/**
	 * @param mailTypeID セットする mailTypeID
	 */
	public void setMailTypeID(long mailTypeID) {
		this.mailTypeID = mailTypeID;
	}

	/**
	 * @return mailMethodList
	 */
	public List<MailMethod> getMailMethodList() {
		return mailMethodList;
	}

	/**
	 * @param mailMethodList セットする mailMethodList
	 */
	public void setMailMethodList(List<MailMethod> mailMethodList) {
		this.mailMethodList = mailMethodList;
	}

	/**
	 * @return otherEventTypeNameJA
	 */
	public String getOtherEventTypeNameJA() {
		return otherEventTypeNameJA;
	}

	/**
	 * @param otherEventTypeNameJA セットする otherEventTypeNameJA
	 */
	public void setOtherEventTypeNameJA(String otherEventTypeNameJA) {
		this.otherEventTypeNameJA = otherEventTypeNameJA;
	}

	/**
	 * @return otherEventTypeNameEN
	 */
	public String getOtherEventTypeNameEN() {
		return otherEventTypeNameEN;
	}

	/**
	 * @param otherEventTypeNameEN セットする otherEventTypeNameEN
	 */
	public void setOtherEventTypeNameEN(String otherEventTypeNameEN) {
		this.otherEventTypeNameEN = otherEventTypeNameEN;
	}

	/**
	 * @return eventSeq
	 */
	public int getEventSeq() {
		return eventSeq;
	}

	/**
	 * @param eventSeq セットする eventSeq
	 */
	public void setEventSeq(int eventSeq) {
		this.eventSeq = eventSeq;
	}

	/**
	 * @return fromStatysTypeSeq
	 */
	public int getFromStatysTypeSeq() {
		return fromStatysTypeSeq;
	}

	/**
	 * @param fromStatysTypeSeq セットする fromStatysTypeSeq
	 */
	public void setFromStatysTypeSeq(int fromStatysTypeSeq) {
		this.fromStatysTypeSeq = fromStatysTypeSeq;
	}

	/**
	 * @return toStatysTypeSeq
	 */
	public int getToStatysTypeSeq() {
		return toStatysTypeSeq;
	}

	/**
	 * @param toStatysTypeSeq セットする toStatysTypeSeq
	 */
	public void setToStatysTypeSeq(int toStatysTypeSeq) {
		this.toStatysTypeSeq = toStatysTypeSeq;
	}

	/**
	 * @return baseEventTypeID
	 */
	public long getBaseEventTypeID() {
		return baseEventTypeID;
	}

	/**
	 * @param baseEventTypeID セットする baseEventTypeID
	 */
	public void setBaseEventTypeID(long baseEventTypeID) {
		this.baseEventTypeID = baseEventTypeID;
	}

	/**
	 * @return guardCondID
	 */
	public long getGuardCondID() {
		return guardCondID;
	}

	/**
	 * @param guardCondID セットする guardCondID
	 */
	public void setGuardCondID(long guardCondID) {
		this.guardCondID = guardCondID;
	}


}
