package jp.co.ctc_g.eim.app.document.common.util;

import org.springframework.context.ApplicationContext;

import jp.co.ctc_g.eim.framework2.business.dao.RelationDao;
import jp.co.ctc_g.eim.framework2.business.domain.entity.RelationDomain;
import jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework2.common.enumeration.DuplicateCheckModeEnum;
import jp.co.ctc_g.eim.framework2.common.exception.EIMException;

/**
 * 【ドキュメントAPI】
 */
public class RelationUtils {

	/**
	 * リレーションを作成します。
	 *
	 * @param relationDomain
	 * @return
	 * @throws Exception
	 */
	public static void createRelation(RelationDomain relationDomain) throws Exception {

		try {
			// PostgreSQL対応にてcheckCode > 0の場合は一様にDuplicateCheckModeEnum.ALLTYPEでチェックするように仕様変更
			//（本メソッドではもともとはcheckCode=2固定だった）
			DuplicateCheckModeEnum checkMode = DuplicateCheckModeEnum.ALLTYPE;

			// リレーションを登録
			ApplicationContext context = ApplicationContextLoader.getApplicationContext();
			RelationDao relationDao = (RelationDao) context.getBean("relationDaoForUtil");

			relationDao.create(relationDomain, checkMode);

			return;
		} catch (jp.co.ctc_g.eim.framework2.common.exception.EIMException e) {
			// Daoの例外をUtilの例外に変換
			EIMException ee = new EIMException(e.getMessageKey());
			ee.initCause(e);
			throw ee;
		}
	}
}
