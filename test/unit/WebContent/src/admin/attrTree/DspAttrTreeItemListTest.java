package admin.attrTree;

import java.util.List;

import junit.framework.TestCase;

import org.apache.commons.lang.ClassUtils;

import admin.AdminService;

import common.bo.AttributeTree;
import common.bo.AttributeTreeItem;
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
 * 属性ツリー項目リスト取得クラスのテスト
 */
public class DspAttrTreeItemListTest extends TestCase {
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
	 * 正常系
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
		assertEquals(ja_name, attributeTree.getName());
		assertEquals(classifyTarget, attributeTree.getClassifyTarget());
		String attrTypeIdList = "26,27,28,29";
		String viewNoValuesFlagList = "true,false,true,false";
		String operationList = ",,,add";
		as.attrTree_actUpdateAttrTreeItemList(attributeTree.getId(), attrTypeIdList,
			viewNoValuesFlagList, operationList);

		String returnXML = as.attrTree_dspAttrTreeItemList(attributeTree.getId());

		attributeTreelist = AttributeTreeUtil.getAttributeTreeList(sess);
		attributeTree = (AttributeTree) attributeTreelist.get(0);
		List treeItemList = attributeTree.getTreeItemList();

		// 検証用雛形XMLをテキストファイルから読み込み
		String assertBaseText = Misc.loadFile(this.getClass().getResource(
			ClassUtils.getShortClassName(this.getClass()) + ".list_1.txt").getPath());

		// 検証用XMLを生成
		String assertText = assertBaseText.replaceAll("\\[attrTreeItemId0\\]",
			String.valueOf(((AttributeTreeItem) treeItemList.get(0)).getId())).replaceAll(
			"\\[attrTreeItemId1\\]",
			String.valueOf(((AttributeTreeItem) treeItemList.get(1)).getId())).replaceAll(
			"\\[attrTreeItemId2\\]",
			String.valueOf(((AttributeTreeItem) treeItemList.get(2)).getId())).replaceAll(
			"\\[attrTreeItemId3\\]",
			String.valueOf(((AttributeTreeItem) treeItemList.get(3)).getId())).replaceAll(
			"\\[attrTypeId0\\]", "26").replaceAll("\\[attrTypeId1\\]", "27").replaceAll(
			"\\[attrTypeId2\\]", "28").replaceAll("\\[attrTypeId3\\]", "29").replaceAll(
			"\\[attrTypeName0\\]",
			String.valueOf(((AttributeTreeItem) treeItemList.get(0)).getType().getName())).replaceAll(
			"\\[attrTypeName1\\]",
			String.valueOf(((AttributeTreeItem) treeItemList.get(1)).getType().getName())).replaceAll(
			"\\[attrTypeName2\\]",
			String.valueOf(((AttributeTreeItem) treeItemList.get(2)).getType().getName())).replaceAll(
			"\\[attrTypeName3\\]",
			String.valueOf(((AttributeTreeItem) treeItemList.get(3)).getType().getName())).replaceAll(
			"\\[viewNoValuesFlag0\\]", "true").replaceAll("\\[viewNoValuesFlag1\\]", "false").replaceAll(
			"\\[viewNoValuesFlag2\\]", "true").replaceAll("\\[viewNoValuesFlag3\\]", "false");
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
			as.attrTree_dspAttrTreeItemList(1);
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
			as.attrTree_dspAttrTreeItemList(1);
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
			as.attrTree_dspAttrTreeItemList(1);
			fail();
		} catch (EIMServerResponseError e) {
			assertEquals(EIMResource.getMessage("JA", "EIM.ERROR.LOGIC.ATTRIBUTETREE.NOTFOUND"),
				e.getMessage());
		}
	}
}
