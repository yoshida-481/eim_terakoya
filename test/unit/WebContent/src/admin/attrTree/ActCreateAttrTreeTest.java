package admin.attrTree;

import java.util.List;

import org.apache.commons.lang.ClassUtils;

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
import eimtest.util.Misc;
import eimtest.util.TestSessionUtil;

/**
 * 属性ツリービュー新規クラスのテスト
 */
public class ActCreateAttrTreeTest extends TestCase {
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
	 * 正常系 属性ツリービューが正常に作られること。
	 * 
	 * @throws Exception
	 */
	public void testMe() throws Exception {
		String ja_name = "langName";
		String en_name = "en:langName";
		int classifyTarget = 0;
		String[][] otherLangNames = { { "JA", ja_name }, { "EN", en_name } };

		List attributeTreelist = AttributeTreeUtil.getAttributeTreeList(sess);
		assertEquals(0, attributeTreelist.size());

		String returnXML = as.attrTree_actCreateAttrTree(otherLangNames, classifyTarget);

		attributeTreelist = AttributeTreeUtil.getAttributeTreeList(sess);
		// 1件になっていること
		assertEquals(1, attributeTreelist.size());
		// 日本語でログインしているので、日本語語名を検証。分類対象を検証
		AttributeTree attributeTree = (AttributeTree) attributeTreelist.get(0);
		assertEquals(ja_name, attributeTree.getName());
		assertEquals(classifyTarget, attributeTree.getClassifyTarget());

		// 英語名を検証
		sess.setAttribute(EIMSession.LANG, "EN");
		attributeTree = (AttributeTree) AttributeTreeUtil.getAttributeTreeList(sess).get(0);
		assertEquals(en_name, attributeTree.getName());
		assertEquals(classifyTarget, attributeTree.getClassifyTarget());
		
		// 検証用雛形XMLをテキストファイルから読み込み
		String assertBaseText = Misc.loadFile(this.getClass().getResource(
			ClassUtils.getShortClassName(this.getClass()) + ".list_1.txt").getPath());

		// 検証用XMLを生成
		String assertText = assertBaseText
		    .replaceAll("\\[attrTreeId\\]", String.valueOf(attributeTree.getId()));

		// JSP返却結果XMLを検証
		assertEquals(assertText, xu.toStr(xu.toDOM(returnXML)));
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
			String ja_name = "langName";
			String en_name = "en:langName";
			int classifyTarget = 0;
			String[][] otherLangNames = { { "JA", ja_name }, { "EN", en_name } };

			as.attrTree_actCreateAttrTree(otherLangNames, classifyTarget);

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
			String ja_name = "langName";
			String en_name = "en:langName";
			int classifyTarget = 0;
			String[][] otherLangNames = { { "JA", ja_name }, { "EN", en_name } };

			as.attrTree_actCreateAttrTree(otherLangNames, classifyTarget);

			fail();
		} catch (EIMServerResponseError e) {
			assertEquals(EIMResource.getMessage("JA", "EIM.ERROR.NOTADMINISTRATOR"), e.getMessage());
		}
	}
}
