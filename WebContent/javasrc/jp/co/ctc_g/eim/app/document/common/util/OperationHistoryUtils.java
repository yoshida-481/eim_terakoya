package jp.co.ctc_g.eim.app.document.common.util;

import org.springframework.context.ApplicationContext;

import jp.co.ctc_g.eim.framework2.business.dao.OperationHistoryDao;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.OperationHistoryDomain;
import jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework2.common.exception.EIMException;

/**
* 【ドキュメントAPI】
* 操作履歴関連クラス
*
*/
public class OperationHistoryUtils {

	/**
	 * 指定したオブジェクトに対し、セッションから取得されたユーザが<br>
	 * 指定したアクションを行ったことを示すアクセスログを生成します。<br>
	 * ログに記録する時間はDBサーバのシステム時間となります。<br>
	 *
	 * @param object アクセスログを生成する対象のオブジェクト
	 * @param action アクセスログに記録するアクション
	 * @throws Exception
	 */
	public static void createOperationHistory(ObjectDomain object,
												long applicationTypeId,
												long operationTypeId ,
												long operationTargetInfoA ,
												long operationTargetTypeA ,
												long operationTypeIdA ,
												String operationTargetNameA ,
												long operationTargetInfoB ,
												long operationTargetTypeB ,
												long operationTypeIdB ,
												String operationTargetNameB ,
												String detail) throws Exception {

		try {
			OperationHistoryDomain domain = new OperationHistoryDomain();
			//アプリケーション種別
			domain.setApplicationTypeId(applicationTypeId);
			//操作種別
			domain.setOperationTypeId(operationTypeId);
			//操作対象情報 A
			domain.setRecordInfoIdA(operationTargetInfoA);
			//操作対象種別 A
			domain.setRecordTypeIdA(operationTargetTypeA);
			//操作対象ID A
			//操作対象名 A
			ObjectDomain objA = new ObjectDomain();
			objA.setId(operationTypeIdA);
			objA.setName(operationTargetNameA);
			domain.setRecordObjectA(objA);

			//操作対象情報 B
			domain.setRecordInfoIdB(operationTargetInfoB);
			//操作対象種別 B
			domain.setRecordTypeIdB(operationTargetTypeB);
			//操作対象ID B
			//操作対象名 B
			ObjectDomain objB = new ObjectDomain();
			objB.setId(operationTypeIdB);
			objB.setName(operationTargetNameB);
			domain.setRecordObjectB(objB);
			
			//操作詳細
			domain.setDetail(detail);

			// 処理委譲する
			ApplicationContext context = ApplicationContextLoader.getApplicationContext();
			OperationHistoryDao operationHistoryDao = (OperationHistoryDao) context.getBean("operationHistoryDaoForUtil");

			operationHistoryDao.create(domain);
		} catch (jp.co.ctc_g.eim.framework2.common.exception.EIMException e) {
			// Daoの例外をUtilの例外に変換
			EIMException ee = new EIMException(e.getMessageKey());
			ee.initCause(e);
			throw ee;
		}
	}

}
