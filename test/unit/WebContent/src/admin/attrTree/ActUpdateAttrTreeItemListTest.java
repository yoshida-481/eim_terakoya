package admin.attrTree;

import java.util.List;

import junit.framework.TestCase;

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
import eimtest.util.TestSessionUtil;

/**
 * 属性ツリー項目リスト更新クラスのテスト
 */
public class ActUpdateAttrTreeItemListTest extends TestCase {
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
	 * 正常系 属性ツリービューが正常に更新されること。
	 * 作成
	 * @throws Exception
	 */
	public void testMe_1() throws Exception {
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

		// 更新パーラーメータ
		String attrTypeIdList = "26,27,28,29";
		String viewNoValuesFlagList = "true,false,true,false";
		String operationList = ",,,add";

		as.attrTree_actUpdateAttrTreeItemList(attributeTree.getId(), attrTypeIdList,
			viewNoValuesFlagList, operationList);

		attributeTreelist = AttributeTreeUtil.getAttributeTreeList(sess);
		assertEquals(1, attributeTreelist.size());
		attributeTree = (AttributeTree) attributeTreelist.get(0);

		List treeItemList = attributeTree.getTreeItemList();
		assertEquals(26, ((AttributeTreeItem) treeItemList.get(0)).getType().getId());
		assertEquals(27, ((AttributeTreeItem) treeItemList.get(1)).getType().getId());
		assertEquals(28, ((AttributeTreeItem) treeItemList.get(2)).getType().getId());
		assertEquals(29, ((AttributeTreeItem) treeItemList.get(3)).getType().getId());
		assertEquals(true, ((AttributeTreeItem) treeItemList.get(0)).isViewNoValues());
		assertEquals(false, ((AttributeTreeItem) treeItemList.get(1)).isViewNoValues());
		assertEquals(true, ((AttributeTreeItem) treeItemList.get(2)).isViewNoValues());
		assertEquals(false, ((AttributeTreeItem) treeItemList.get(3)).isViewNoValues());

	}
	
	/**
	 * 正常系 属性ツリービューが正常に更新されること。
	 * ドラッグ＆ドロップ
	 * @throws Exception
	 */
	public void testMe_2() throws Exception {
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

		// 更新パーラーメータ
		String attrTypeIdList = "26,27,28,29";
		String viewNoValuesFlagList = "true,false,true,false";
		String operationList = ",,,move";

		as.attrTree_actUpdateAttrTreeItemList(attributeTree.getId(), attrTypeIdList,
			viewNoValuesFlagList, operationList);

		attributeTreelist = AttributeTreeUtil.getAttributeTreeList(sess);
		assertEquals(1, attributeTreelist.size());
		attributeTree = (AttributeTree) attributeTreelist.get(0);

		List treeItemList = attributeTree.getTreeItemList();
		assertEquals(26, ((AttributeTreeItem) treeItemList.get(0)).getType().getId());
		assertEquals(27, ((AttributeTreeItem) treeItemList.get(1)).getType().getId());
		assertEquals(28, ((AttributeTreeItem) treeItemList.get(2)).getType().getId());
		assertEquals(29, ((AttributeTreeItem) treeItemList.get(3)).getType().getId());
		assertEquals(true, ((AttributeTreeItem) treeItemList.get(0)).isViewNoValues());
		assertEquals(false, ((AttributeTreeItem) treeItemList.get(1)).isViewNoValues());
		assertEquals(true, ((AttributeTreeItem) treeItemList.get(2)).isViewNoValues());
		assertEquals(false, ((AttributeTreeItem) treeItemList.get(3)).isViewNoValues());

	}
	
	/**
	 * 正常系 属性ツリービューが正常に更新されること。
	 * 削除
	 * @throws Exception
	 */
	public void testMe_3() throws Exception {
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

		// 更新パーラーメータ
		String attrTypeIdList = "26,27,28,29";
		String viewNoValuesFlagList = "true,false,true,false";
		String operationList = ",,delete,";

		as.attrTree_actUpdateAttrTreeItemList(attributeTree.getId(), attrTypeIdList,
			viewNoValuesFlagList, operationList);

		attributeTreelist = AttributeTreeUtil.getAttributeTreeList(sess);
		assertEquals(1, attributeTreelist.size());
		attributeTree = (AttributeTree) attributeTreelist.get(0);
		
		assertEquals(3, attributeTree.getTreeItemList().size());

		List treeItemList = attributeTree.getTreeItemList();
		assertEquals(26, ((AttributeTreeItem) treeItemList.get(0)).getType().getId());
		assertEquals(27, ((AttributeTreeItem) treeItemList.get(1)).getType().getId());
		assertEquals(29, ((AttributeTreeItem) treeItemList.get(2)).getType().getId());
		assertEquals(true, ((AttributeTreeItem) treeItemList.get(0)).isViewNoValues());
		assertEquals(false, ((AttributeTreeItem) treeItemList.get(1)).isViewNoValues());
		assertEquals(false, ((AttributeTreeItem) treeItemList.get(2)).isViewNoValues());

	}
	
	/**
	 * 正常系 属性ツリービューが正常に更新されること。
	 * 更新
	 * @throws Exception
	 */
	public void testMe_4() throws Exception {
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

		// 更新パーラーメータ
		String attrTypeIdList = "26,27,28,29";
		String viewNoValuesFlagList = "true,false,true,false";
		String operationList = ",,,update";

		as.attrTree_actUpdateAttrTreeItemList(attributeTree.getId(), attrTypeIdList,
			viewNoValuesFlagList, operationList);

		attributeTreelist = AttributeTreeUtil.getAttributeTreeList(sess);
		assertEquals(1, attributeTreelist.size());
		attributeTree = (AttributeTree) attributeTreelist.get(0);

		List treeItemList = attributeTree.getTreeItemList();
		assertEquals(26, ((AttributeTreeItem) treeItemList.get(0)).getType().getId());
		assertEquals(27, ((AttributeTreeItem) treeItemList.get(1)).getType().getId());
		assertEquals(28, ((AttributeTreeItem) treeItemList.get(2)).getType().getId());
		assertEquals(29, ((AttributeTreeItem) treeItemList.get(3)).getType().getId());
		assertEquals(true, ((AttributeTreeItem) treeItemList.get(0)).isViewNoValues());
		assertEquals(false, ((AttributeTreeItem) treeItemList.get(1)).isViewNoValues());
		assertEquals(true, ((AttributeTreeItem) treeItemList.get(2)).isViewNoValues());
		assertEquals(false, ((AttributeTreeItem) treeItemList.get(3)).isViewNoValues());

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
			String attrTypeIdList = "11,12,13,14";
			String viewNoValuesFlagList = "true,false,true,false";
			String operationList = ",,delete,";

			as.attrTree_actUpdateAttrTreeItemList(1, attrTypeIdList, viewNoValuesFlagList,
				operationList);

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
			String attrTypeIdList = "11,12,13,14";
			String viewNoValuesFlagList = "true,false,true,false";
			String operationList = ",,delete,";

			as.attrTree_actUpdateAttrTreeItemList(1, attrTypeIdList, viewNoValuesFlagList,
				operationList);

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
			String attrTypeIdList = "11,12,13,14";
			String viewNoValuesFlagList = "true,false,true,false";
			String operationList = ",,delete,";

			as.attrTree_actUpdateAttrTreeItemList(1, attrTypeIdList, viewNoValuesFlagList,
				operationList);

			fail();
		} catch (EIMServerResponseError e) {
			assertEquals(EIMResource.getMessage("JA", "EIM.ERROR.LOGIC.ATTRIBUTETREE.NOTFOUND"),
				e.getMessage());
		}
	}

	/**
	 * 異常系 属性タイプ存在しない
	 * 
	 * @throws Exception
	 */
	public void testAbnormal_4() throws Exception {
		try {
			// 属性ツリーを1件作成し
			String ja_name = "langName";
			String en_name = "en:langName";
			int classifyTarget = 0;
			String[][] otherLangNames = { { "JA", ja_name }, { "EN", en_name } };

			List attributeTreelist = AttributeTreeUtil.getAttributeTreeList(sess);
			as.attrTree_actCreateAttrTree(otherLangNames, classifyTarget);
			attributeTreelist = AttributeTreeUtil.getAttributeTreeList(sess);
			// 1件になっていること
			AttributeTree attributeTree = (AttributeTree) attributeTreelist.get(0);

			// 更新パーラーメータ
			String attrTypeIdList = "11,12,13,14";
			String viewNoValuesFlagList = "true,false,true,false";
			String operationList = ",,delete,";

			as.attrTree_actUpdateAttrTreeItemList(attributeTree.getId(), attrTypeIdList,
				viewNoValuesFlagList, operationList);

			fail();
		} catch (EIMServerResponseError e) {
			assertEquals(EIMResource.getMessage("JA", "EIM.ERROR.LOGIC.ATTTYPE.NOTFOUND"),
				e.getMessage());
		}
	}
	
	/**
	 * 異常系 属性は既に適用されています
	 * 
	 * @throws Exception
	 */
	public void testAbnormal_5() throws Exception {
		try {
			// 属性ツリーを1件作成し
			String ja_name = "langName";
			String en_name = "en:langName";
			int classifyTarget = 0;
			String[][] otherLangNames = { { "JA", ja_name }, { "EN", en_name } };

			List attributeTreelist = AttributeTreeUtil.getAttributeTreeList(sess);
			as.attrTree_actCreateAttrTree(otherLangNames, classifyTarget);
			attributeTreelist = AttributeTreeUtil.getAttributeTreeList(sess);
			// 1件になっていること
			AttributeTree attributeTree = (AttributeTree) attributeTreelist.get(0);

			// 更新パーラーメータ
			String attrTypeIdList = "11,12,13,11";
			String viewNoValuesFlagList = "true,false,true,true";
			String operationList = ",,,add";

			as.attrTree_actUpdateAttrTreeItemList(attributeTree.getId(), attrTypeIdList,
				viewNoValuesFlagList, operationList);

			fail();
		} catch (EIMServerResponseError e) {
			assertEquals(EIMResource.getMessage("JA", "EIM.ERROR.LOGIC.ATTR.IS.APPLYED"),
				e.getMessage());
		}
	}
}
