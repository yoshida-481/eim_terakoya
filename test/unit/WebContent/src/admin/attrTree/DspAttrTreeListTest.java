package admin.attrTree;

import junit.framework.TestCase;

import org.apache.commons.lang.ClassUtils;
import org.w3c.dom.Document;

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
 * 属性取得クラスのテスト
 */
public class DspAttrTreeListTest extends TestCase {
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
	 * 正常系 属性が正常に取得されること。
	 * 
	 * @throws Exception
	 */
	public void testMe() throws Exception {
		// 属性ツリーを2件作成し、JSP返却を検証
		AttributeTree attrt1 = AttributeTreeUtil.createAttributeTree(sess, "attributeTree1", 0);
		AttributeTreeUtil.updateOtherAttributeTreeName(sess, attrt1.getId(), "JA", "attrt1hoge");
		AttributeTreeUtil.updateOtherAttributeTreeName(sess, attrt1.getId(), "EN", "en:attrt1hoge");

		AttributeTree attrt2 = AttributeTreeUtil.createAttributeTree(sess, "attributeTree2", 0);
		AttributeTreeUtil.updateOtherAttributeTreeName(sess, attrt2.getId(), "JA", "attrt2foo");
		AttributeTreeUtil.updateOtherAttributeTreeName(sess, attrt2.getId(), "EN", "en:attrt2foo");

		sess.commit();// commitしないとAPサーバーからは見えない

		// 検証用雛形XMLをテキストファイルから読み込み
		String assertBaseText = Misc.loadFile(this.getClass().getResource(
			ClassUtils.getShortClassName(this.getClass()) + ".list_1.txt").getPath());

		// 検証用XMLを生成
		String assertText = assertBaseText//
		.replaceAll("\\[1\\_id\\]", String.valueOf(attrt1.getId()))//
		.replaceAll("\\[1\\_nm\\]", "attrt1hoge")//
		.replaceAll("\\[2\\_id\\]", String.valueOf(attrt2.getId()))//
		.replaceAll("\\[2\\_nm\\]", "attrt2foo")//
		;
		// JSP返却結果XMLを検証
		assertEquals(assertText, xu.toStr(xu.toDOM(as.attrTree_dspAttrTreeList())));

		// 他言語での名称切り替わりを検証
		// 英語でログイン。
		as.switchUser("system", "manager", "EN", null);
		assertText = assertBaseText//
		.replaceAll("\\[1\\_id\\]", String.valueOf(attrt1.getId()))//
		.replaceAll("\\[1\\_nm\\]", "en:attrt1hoge")//
		.replaceAll("\\[2\\_id\\]", String.valueOf(attrt2.getId()))//
		.replaceAll("\\[2\\_nm\\]", "en:attrt2foo")//
		;
		Document xdoc = xu.toDOM(as.attrTree_dspAttrTreeList());
		assertEquals(assertText, xu.toStr(xdoc));
	}

	/**
	 * 異常系
	 * 
	 * @throws Exception
	 */
	public void testAbnormal() throws Exception {
		// ログインエラーを検証
		as.br.eimLogout();
		try {
			as.attrTree_dspAttrTreeList();
			fail();
		} catch (EIMServerResponseError e) {
			assertEquals(EIMResource.getMessage("JA", "EIM.ERROR.SESSIONTIMEOUT"), e.getMessage());
		}

		// 該当管理権限無しエラーを検証
		// u1をシステム管理者(操作履歴のみ)にしてログイン
		EIMUser u1 = UserUtils.getUserByCode(sess, "u1");
		UserUtils.updateUser(sess, u1, u1.getCode(), u1.getName(), u1.getKana(), "u1",
			u1.getMail(), 128);
		sess.commit();
		as.switchUser("u1");
		try {
			as.attrTree_dspAttrTreeList();
			fail();
		} catch (EIMServerResponseError e) {
			assertEquals(EIMResource.getMessage("JA", "EIM.ERROR.NOTADMINISTRATOR"), e.getMessage());
		}
	}
}
