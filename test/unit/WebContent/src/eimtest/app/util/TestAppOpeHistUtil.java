package eimtest.app.util;

import junit.framework.Assert;
import eim.bo.EIMOperationHistory;
import eim.net.EIMSession;
import eim.util.OperationHistoryUtils;
import eim.util.UserUtils;

/** */
public class TestAppOpeHistUtil {
	/**
	 * 
	 * @param sess
	 * @param userCd
	 * @param applicationType
	 * @param operationType
	 * @param recordInfoA
	 * @param recordTypeA
	 * @param recordIdA
	 * @param recordNameA
	 * @param recordInfoB
	 * @param recordTypeB
	 * @param recordIdB
	 * @param recordNameB
	 * @param detail
	 * @throws Exception
	 */
	public static void assetOpeHist(EIMSession sess, String userCd//
			, String applicationType//
			, String operationType//
			, String recordInfoA//
			, String recordTypeA//
			, int recordIdA//
			, String recordNameA, String recordInfoB//
			, String recordTypeB//
			, int recordIdB//
			, String recordNameB, String detail//
	) throws Exception {
		// 操作履歴が出来ているか
		EIMOperationHistory op = (EIMOperationHistory) OperationHistoryUtils.search(sess,
			UserUtils.getUserByCode(sess, userCd).getId(), null, null, 1).get(0);
		Assert.assertEquals(applicationType, op.getApplicationType());
		Assert.assertEquals(operationType, op.getOperationType());
		Assert.assertEquals(recordInfoA, op.getRecordInfoA());
		Assert.assertEquals(recordTypeA, op.getRecordTypeA());
		Assert.assertEquals(recordIdA, op.getRecordIdA());
		Assert.assertEquals(recordNameA, op.getRecordNameA());
		Assert.assertEquals(recordInfoB, op.getRecordInfoB());
		Assert.assertEquals(recordTypeB, op.getRecordTypeB());
		Assert.assertEquals(recordIdB, op.getRecordIdB());
		Assert.assertEquals(recordNameB, op.getRecordNameB());
		Assert.assertEquals(detail, op.getDetail());
	}
}
