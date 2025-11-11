package admin.attrTree;

import java.util.List;

import junit.framework.TestCase;

import admin.AdminService;

import common.bo.AttributeTree;
import common.util.AttributeTreeUtil;

import eim.bo.EIMResource;
import eim.bo.EIMUser;
import eim.net.EIMSession;
import eim.util.UserUtils;
import eimtest.app.util.TestAppDBUtil;
import eimtest.app.util.XMLUtil;
import eimtest.app.util.net.EIMServerResponseError;
import eimtest.util.TestSessionUtil;

/**
 * 属性ツリービュー削除クラスのテスト
 */
public class ActDeleteAttrTreeTest extends TestCase {
	/** */
	AdminService as;

	/** */
	XMLUtil xu;

	/** */
	EIMSession sess;

	public void setUp() throws Exception {
		if (true)
			TestAppDBUtil.loadPrimitiveData();// DB内容を初期化する
		as = new AdminService(false);// 引数false==システム管理(ドキュメント)を対象とする
		xu = new XMLUtil();
		sess = TestSessionUtil.createEIMSession();// データ直接操作用のEIMセッション
	}

	protected void tearDown() throws Exception {
		sess.close();
	}

	/**
	 * 正常系 属性ツリービューが正常に削除されること。
	 * 
	 * @throws Exception
	 */
	public void testMe() throws Exception {
		// 属性ツリーを1件作成し
		String ja_name = "langName";
		String en_name = "en:langName";
		int classifyTarget = 0;
		String[][] otherLangNames = { { "JA", ja_name }, { "EN", en_name } };

		List attributeTreelist = AttributeTreeUtil.getAttributeTreeList(sess);
		assertEquals(0, attributeTreelist.size());

		as.attrTree_actCreateAttrTree(otherLangNames, classifyTarget);
		attributeTreelist = AttributeTreeUtil.getAttributeTreeList(sess);
		// 1件になっていること
		assertEquals(1, attributeTreelist.size());

		AttributeTree attributeTree = (AttributeTree) attributeTreelist.get(0);

		as.attrTree_actDeleteAttrTree(attributeTree.getId());

		attributeTreelist = AttributeTreeUtil.getAttributeTreeList(sess);
		// 0件になっていること
		assertEquals(0, attributeTreelist.size());
	}

	/**
	 * 異常系 セッションなし
	 * 
	 * @throws Exception
	 */
	public void testAbnormal_1() throws Exception {
		// ログインエラーを検証
		as.br.eimLogout();
		try {
			as.attrTree_actDeleteAttrTree(1);
			fail();
		} catch (EIMServerResponseError e) {
			assertEquals(EIMResource.getMessage("JA", "EIM.ERROR.SESSIONTIMEOUT"), e.getMessage());
		}
	}

	/**
	 * 異常系 ユーザ権限なし
	 * 
	 * @throws Exception
	 */
	public void testAbnormal_2() throws Exception {
		// 該当管理権限無しエラーを検証
		// u1をシステム管理者(操作履歴のみ)にしてログイン
		EIMUser u1 = UserUtils.getUserByCode(sess, "u1");
		UserUtils.updateUser(sess, u1, u1.getCode(), u1.getName(), u1.getKana(), "u1",
			u1.getMail(), 128);
		sess.commit();
		as.switchUser("u1");
		try {
			as.attrTree_actDeleteAttrTree(1);
			fail();
		} catch (EIMServerResponseError e) {
			assertEquals(EIMResource.getMessage("JA", "EIM.ERROR.NOTADMINISTRATOR"), e.getMessage());
		}
	}

	/**
	 * 異常系 属性ツリービュー存在しない
	 * 
	 * @throws Exception
	 */
	public void testAbnormal_3() throws Exception {
		try {
			as.attrTree_actDeleteAttrTree(1);
			fail();
		} catch (EIMServerResponseError e) {
			assertEquals(EIMResource.getMessage("JA", "EIM.ERROR.LOGIC.ATTRIBUTETREE.NOTFOUND"),
				e.getMessage());
		}
	}
}
