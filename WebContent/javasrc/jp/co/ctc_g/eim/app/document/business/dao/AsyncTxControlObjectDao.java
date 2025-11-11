package jp.co.ctc_g.eim.app.document.business.dao;

import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;

/**
 * オブジェクト登録・更新時のバックグラウンド処理で非同期処理を実行するとき、
 * フォアグラウンド処理のトランザクションを待機する機能を提供するDaoインターフェイスです。
 */
public interface AsyncTxControlObjectDao {

	/**
	 * オブジェクト登録・更新処理のトランザクションの終了を待機します。<p>
	 * <ol>
	 * <li>引き渡されたオブジェクトIDでEIMOBJテーブルにレコードをINSERT
	 * <li>オブジェクト登録・更新処理の終了を待機
	 * <li>登録がCOMMITされた時、更新がCOMMITまたはROLLBACKされた時はユニークキー例外をcatchして正常終了
	 * </ol>
	 * 登録がROLLBACKされinsertが成功した時、オブジェクトが存在しないことを示す例外をthrowします。
	 * このメソッドにトランザクション境界(REQUIRES_NEW)を設定して下さい。
	 * そうすることで、既存のトランザクションに影響を与えずにinsertしたレコードはTransactionManagerによってロールバックされます。
	 * @param object 対象のオブジェクト
	 * @throws Exception
	 * EIMException("EIM.ERROR.OBJECT.NOTFOUND.DETAIL"): オブジェクト[{オブジェクト名}]が取得できません。
	 */
	void waitTransactionEnd(ObjectDomain object) throws Exception;

}
