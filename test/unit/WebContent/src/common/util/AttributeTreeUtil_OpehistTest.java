package common.util;

import junit.framework.TestCase;

import common.bo.AttributeTree;

import eim.bo.EIMOperationHistory;
import eim.net.EIMSession;
import eim.util.EIMConstant;
import eim.util.OperationHistoryUtils;
import eimtest.app.util.TestAppDBUtil;
import eimtest.util.TestSessionUtil;

/** */
public class AttributeTreeUtil_OpehistTest extends TestCase {
	/** */
	EIMSession sess;

	public void setUp() throws Exception {
		sess = TestSessionUtil.createEIMSession(null);
		sess.setAttribute(EIMSession.LANG, "JA");
		TestAppDBUtil.loadPrimitiveData();
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testMe() throws Exception {
		AttributeTree attrTree = AttributeTreeUtil.createAttributeTree(sess,
				"hoge", 0);
		// 他言語名登録無しの状態で登録。
		// RecordNameAにはデフォルト名が使用される。
		// RecordIdAはattrTreeのidとなる。
		AttributeTreeUtil.createOperationHistory(sess//
				, AppConstant.SYSTEM//
				, EIMConstant.CREATE_ATTRIBUTE_TREE_VIEW//
				, EIMConstant.TARGET_CREATE//
				, EIMConstant.ATTRIBUTE_TREE, attrTree, null, null, null, null);
		EIMOperationHistory target;
		target = (EIMOperationHistory) OperationHistoryUtils.search(sess, -1,
				null, null, 10000).get(0);
		assertEquals(1, target.getUserId());
		assertEquals("システム管理者", target.getUserName());
		assertEquals("登録対象", target.getRecordInfoA());
		assertEquals("属性ツリー", target.getRecordTypeA());
		assertEquals("hoge", target.getRecordNameA());
		assertEquals(attrTree.getId(), target.getRecordIdA());

		// ENの他言語名のみ登録、JA無しの状態で登録。履歴にはデフォルト名が使用される
		AttributeTreeUtil.addOtherAttributeTreeName(sess, attrTree.getId(),
				"EN", "en:hoge");
		AttributeTreeUtil.createOperationHistory(sess//
				, AppConstant.SYSTEM//
				, EIMConstant.CREATE_ATTRIBUTE_TREE_VIEW//
				, EIMConstant.TARGET_CREATE//
				, EIMConstant.ATTRIBUTE_TREE, attrTree, null, null, null, null);
		target = (EIMOperationHistory) OperationHistoryUtils.search(sess, -1,
				null, null, 10000).get(0);
		assertEquals(1, target.getUserId());
		assertEquals("hoge", target.getRecordNameA());

		// JAの他言語名も登録。履歴にはJA名が使用される
		AttributeTreeUtil.addOtherAttributeTreeName(sess, attrTree.getId(),
				"JA", "ja:hoge");
		AttributeTreeUtil.createOperationHistory(sess//
				, AppConstant.SYSTEM//
				, EIMConstant.CREATE_ATTRIBUTE_TREE_VIEW//
				, EIMConstant.TARGET_CREATE//
				, EIMConstant.ATTRIBUTE_TREE, attrTree, null, null, null, null);
		target = (EIMOperationHistory) OperationHistoryUtils.search(sess, -1,
				null, null, 10000).get(0);
		assertEquals(1, target.getUserId());
		assertEquals("ja:hoge", target.getRecordNameA());
	}
}
