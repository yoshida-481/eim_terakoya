package jp.co.ctc_g.eim.app.document.common.util;

import org.springframework.context.ApplicationContext;

import jp.co.ctc_g.eim.framework2.business.dao.AccessHistoryDao;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AccessHistoryDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.common.context.TransactionContext;
import jp.co.ctc_g.eim.framework2.common.exception.EIMException;


/**
* 【ドキュメントAPI】
* アクセスログ関連クラス
*
*/
public class AccessUtils {

	/**
	 * 指定したオブジェクトに対し、セッションから取得されたユーザが<br>
	 * 指定したアクションを行ったことを示すアクセスログを生成します。<br>
	 * ログに記録する時間はDBサーバのシステム時間となります。<br>
	 *
	 * @param object アクセスログを生成する対象のオブジェクト
	 * @param action アクセスログに記録するアクション
	 * @throws Exception
	 */
	public static void createAccess(ObjectDomain object, String action) throws Exception {

		try {
			ApplicationContext context = ApplicationContextLoader.getApplicationContext();
			AccessHistoryDao accessHistoryDao = (AccessHistoryDao) context.getBean("accessHistoryDaoForUtil");

			// User
			TransactionContext tx = EIMThreadContext.getTransactionContext();
			UserDomain user = tx.getUser();

			// Domain生成
			AccessHistoryDomain accessHistoryDomain = new AccessHistoryDomain();
			accessHistoryDomain.setAction(action);
			accessHistoryDomain.setUser(user);

			// 処理移譲
			accessHistoryDao.create(object, accessHistoryDomain);

			return;
		} catch (jp.co.ctc_g.eim.framework2.common.exception.EIMException e) {
			// Daoの例外をUtilの例外に変換
			EIMException ee = new EIMException(e.getMessageKey());
			ee.initCause(e);
			throw ee;
		}
	}
}
