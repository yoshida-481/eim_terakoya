package jp.co.ctc_g.eim.app.document.business.service;

import jp.co.ctc_g.eim.framework2.business.service.WebDAVService;


/**
 * WebDAV通信に関連する操作を行うための機能を提供します。(ドキュメント管理用)
 * @since Ver6.6
 */
public interface DocumentWebDAVService extends WebDAVService {

	/**
	 * 指定されたIDのドキュメントが直接編集可能なステータスかを判定します。<br>
	 * 編集不可能の場合、Exceptionをthrowします。
	 * 
	 * @param objectId ID<br>
	 * <p style="padding-left:4em">
	 * @throws Exception
	 * <br><br>
	 * @since Ver 6.6
	 * <br>
	 */
	public void checkStatus(long objectId) throws Exception;

	
}
