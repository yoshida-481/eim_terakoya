package admin.attrTree;

import junit.framework.TestCase;

import org.apache.commons.lang.ClassUtils;

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
 * 属性ツリービュー取得クラスのテスト
 */
public class DspAttrTreeTest extends TestCase {
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
	 * 正常系 属性ツリービューが正常に取得されること。
	 * 
	 * @throws Exception
	 */
	public void testMe() throws Exception {
		// 属性ツリーを1件作成し、JSP返却を検証
		String ja_name = "langName";
		String en_name = "en:langName";
		AttributeTree attributeTree = AttributeTreeUtil.createAttributeTree(sess, "attrtHoge", 0);
		AttributeTreeUtil.updateOtherAttributeTreeName(sess, attributeTree.getId(), "JA", ja_name);
		AttributeTreeUtil.updateOtherAttributeTreeName(sess, attributeTree.getId(), "EN", en_name);

		sess.commit();// commitしないとAPサーバーからは見えない

		// 検証用雛形XMLをテキストファイルから読み込み
		String assertBaseText = Misc.loadFile(this.getClass().getResource(
			ClassUtils.getShortClassName(this.getClass()) + ".list_1.txt").getPath());

		// 検証用XMLを生成
		String assertText = assertBaseText.replaceAll("\\[attrTreeId\\]",
			String.valueOf(attributeTree.getId())).replaceAll("\\[classifyTarget\\]",
			String.valueOf(attributeTree.getClassifyTarget())).replaceAll("\\[otherCnt\\]", "2").replaceAll(
			"\\[EN_otherName\\]", en_name).replaceAll("\\[JA_otherName\\]", ja_name);
		// JSP返却結果XMLを検証
		assertEquals(assertText, xu.toStr(xu.toDOM(as.attrTree_dspAttrTree(attributeTree.getId()))));

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
			as.attrTree_dspAttrTree(1);
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
			as.attrTree_dspAttrTree(1);
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
			as.attrTree_dspAttrTree(1);
			fail();
		} catch (EIMServerResponseError e) {
			assertEquals(EIMResource.getMessage("JA", "EIM.ERROR.LOGIC.ATTRIBUTETREE.NOTFOUND"),
				e.getMessage());
		}
	}
}
