/**
 *
 */
package jp.co.ctc_g.eim.app.document.common.aop.advice;

import jp.co.ctc_g.eim.framework2.business.domain.entity.CodeDomain;
import jp.co.ctc_g.eim.search.core.updateNotice.business.domain.UpdateNoticeDomain;
import jp.co.ctc_g.eim.search.core.updateNotice.business.service.UpdateNoticeService;

/**
 * コードの更新通知出力を行うアドバイスクラスです。
 */
public class CodeUpdateNoticeAdvice {

	/** 更新通知サービス */
	private UpdateNoticeService updateNoticeService = null;

	/** データタイプ */
	private String dataType = null;

	/** 更新種別 */
	private String updateKind = null;

	/** 通知対象オブジェクトタイプ名 */
	private String targetObjectTypeName = null;

	// ---------------
	// Publicメソッド
	// ---------------

	/**
	 * 更新通知を登録します。
	 * @param object オブジェクトドメイン
	 * @throws Exception
	 */
	public void insert(Object object) throws Exception {

        CodeDomain code = (CodeDomain) object;

		// コードのIDをキーに更新通知を登録
		UpdateNoticeDomain domain = new UpdateNoticeDomain(String.valueOf(code.getId()), dataType);
		domain.setUpdateKind(updateKind);
		updateNoticeService.insert(domain);
	}

	// ---------------
	// Privateメソッド
	// ---------------

	// ---------------
	// getter/setter
	// ---------------

	/**
	 * 更新通知サービスを取得します。
	 * @return 更新通知サービス
	 */
	public UpdateNoticeService getUpdateNoticeService() {
		return updateNoticeService;
	}

	/**
	 * 更新通知サービスを設定します。
	 * @param updateNoticeService 更新通知サービス
	 */
	public void setUpdateNoticeService(UpdateNoticeService updateNoticeService) {
		this.updateNoticeService = updateNoticeService;
	}

	/**
	 * データタイプを取得します。
	 * @return データタイプ
	 */
	public String getDataType() {
		return dataType;
	}

	/**
	 * データタイプを設定します。
	 * @param daatType データタイプ
	 */
	public void setDataType(String dataType) {
		this.dataType = dataType;
	}

	/**
	 * 更新種別を取得します。
	 * @return 更新種別
	 */
	public String getUpdateKind() {
		return updateKind;
	}

	/**
	 * 更新種別を設定します。
	 * @param updateKind 更新種別
	 */
	public void setUpdateKind(String updateKind) {
		this.updateKind = updateKind;
	}

	/**
	 * 通知対象オブジェクトタイプ名を取得します。
	 * @return オブジェクトタイプ名
	 */
	public String getTargetObjectTypeName() {
		return targetObjectTypeName;
	}

	/**
	 * 通知対象オブジェクトタイプ名を設定します。
	 * @param targetObjectTypeName 通知対象オブジェクトタイプ名
	 */
	public void setTargetObjectTypeName(String targetObjectTypeName) {
		this.targetObjectTypeName = targetObjectTypeName;
	}

}
