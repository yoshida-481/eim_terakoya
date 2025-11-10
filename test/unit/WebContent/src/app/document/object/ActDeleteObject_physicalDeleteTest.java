package app.document.object;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import org.apache.xpath.XPathAPI;
import org.w3c.dom.Document;

import common.util.AppLogicUtil;
import common.util.AppObjectConditionHelper;
import common.util.AppLogicUtil.ProcessFolderTreeWalker;

import eim.bo.EIMObject;
import eim.bo.EIMRelation;
import eim.bo.EIMResource;
import eim.bo.EIMSecurity;
import eim.bo.EIMUser;
import eim.util.ObjectUtils;
import eim.util.RelationUtils;
import eim.util.SearchUtils;
import eim.util.SecurityUtils;
import eim.util.UserUtils;
import eim.util.VersionUtils;
import eim.util.WorkFlowUtils;
import eimtest.app.tool.CreateMissingFile;
import eimtest.app.util.CreateFolderTreeData;
import eimtest.app.util.JSPTestCase;
import eimtest.app.util.TestAppDBUtil;
import eimtest.app.util.TestAppMisc;
import eimtest.app.util.TestAppObjectUtil;
import eimtest.app.util.TestAppOpeHistUtil;
import eimtest.app.util.TestAppSecurityUtil;
import eimtest.app.util.net.EIMServerResponseError;

/** */
public class ActDeleteObject_physicalDeleteTest extends JSPTestCase {

	/** */
	EIMObject recycle;

	/** */
	EIMSecurity systemSec;

	/** */
	List wfObjs;

	/** */
	public void setUp() throws Exception {
		TestAppDBUtil.loadPrimitiveData();
		super.setUp();
		// DB初期化
		ObjectUtils.deleteObject(sess, TestAppObjectUtil.getObj(sess, "F1"));
		ObjectUtils.deleteObject(sess, TestAppObjectUtil.getObj(sess, "WS1"));
		// フォルダツリーを作成
		new CreateFolderTreeData(sess, TestAppMisc.getAppFilePath(
			ActDeleteObject_physicalDeleteTest.class, "_01.xls")).process();
		sess.commit();
		// 物理ファイルを作成
		new CreateMissingFile().start(null);
		super.setUp();// once agein

		recycle = TestAppObjectUtil.getObj(sess, "ごみ箱");
		systemSec = SecurityUtils.getSecurityByName(sess, "system");

		// テスト用のフォルダツリー状態を作成
		// d1.txtの履歴を2つ作る
		EIMObject d1 = TestAppObjectUtil.getObj(sess, "d1.txt");
		// 履歴1つめ
		EIMObject d1up = VersionUtils.revisionUp(sess, d1);
		RelationUtils.deleteRelation(sess, (EIMRelation) RelationUtils.getParentRelationList(sess,
			d1).get(0));
		ObjectUtils.rename(sess, d1up, "d1r1.txt");
		// 公開中まで持っていく(一度差し戻し有り)
		WorkFlowUtils.statusUp(sess, d1up);
		WorkFlowUtils.statusDown(sess, d1up);
		WorkFlowUtils.statusUp(sess, d1up);
		WorkFlowUtils.statusUp(sess, d1up);
		WorkFlowUtils.statusUp(sess, d1up);
		d1 = d1up;
		// 履歴2つめ
		d1up = VersionUtils.revisionUp(sess, d1);
		ObjectUtils.rename(sess, d1up, "d1r2.txt");
		sess.commit();

		// 申請・承認してワークフロー関係オブジェクトを作らせる
		ds.switchUser("tu1");
		ds.approve_actRequestApprove(d1up.getId(), "1:" + tu1.getId(), new String[] { "1:"
				+ tu3.getId() }, "", true, true);
		d1up = ObjectUtils.getObjectById(sess, d1up.getId());
		ds.approve_actApprove(d1up, "approve", tu1.getId(), "", true, false);
		// ワークフロー関係オブジェクト(公開通知,承認依頼通知)が出来ていることを確認
		wfObjs = SearchUtils.searchObject(sess, null, String.valueOf(d1up.getId()), false, false,
			-1, null, null, null, null, null, null, null, null, null);
		assertTrue(wfObjs.size() == 2);
		// チェックアウトして編集中にする
		d1up = ObjectUtils.getObjectById(sess, Integer.parseInt(XPathAPI.selectSingleNode(
			xu.toDOM(ds.object_actCheckout(d1up)), "/objectList/object/@objId").getNodeValue()));
		ObjectUtils.rename(sess, d1up, "d1r3.txt");

		// systemセキュリティにtu1:書き込み、tu3読み取りで権限を与える
		TestAppSecurityUtil.setEntry(sess, "system"//
			, new EIMUser[] { UserUtils.getUserById(sess, 1), tu1, tu3 }//
			, new int[] { 0, 0, 3 });

		sess.commit();
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testAbnormal() throws Exception {
		// d121.txtドキュメント削除
		EIMObject target = moveToRecycle("d121.txt");
		sess.commit();

		// systemセキュリティに書き込み権のないユーザー
		ds.switchUser("tu3");
		try {
			ds.object_actDeleteObject(target.getId(), recycle.getId());
			fail();
		} catch (EIMServerResponseError e) {
			assertEquals(e.getMessage(), EIMResource.getMessage("JA",
				"EIM.ERROR.LOGIC.NODELETEROLE"));
		}
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testMe() throws Exception {
		ds.switchUser("tu1");
		// d121.txtドキュメント削除
		EIMObject target = moveToRecycle("d121.txt");
		sess.commit();
		Document xdoc = xu.toDOM(ds.object_actDeleteObject(target.getId(), recycle.getId()));
		// JSP応答を検証
		assertEquals("[" + target.getId() + "]",
			TestAppMisc.getNodeValues(xdoc, "/object/@objId").toString());
		// d121.txtがなくなっていることを検証
		assertNull(TestAppObjectUtil.getObj(sess, target.getName()));
		// 操作履歴が出来ているか
		TestAppOpeHistUtil.assetOpeHist(sess, "tu1"//
			, "ドキュメント管理"//
			, "ごみ箱からの削除"//
			, "削除対象"//
			, "オブジェクト"//
			, target.getId()//
			, target.getName()//
			, null//
			, null//
			, 0//
			, null//
			, null//
		);

		// F1フォルダ削除
		target = moveToRecycle("F1");
		sess.commit();
		String[] objNames = { "F1"//
				, "d1.txt"//
				, "d1r1.txt"//
				, "d1r2.txt"//
				, "d1r3.txt"//
				, "F11"//
				, "d11.txt"//
				, "F12"//
				, "d12.txt"//
				, "F121"//
				, "F122"//
		};
		// 削除前の存在確認。上記＋ワークフロー関係オブジェクト
		checkExists(objNames, wfObjs, false);
		ds.object_actDeleteObject(TestAppObjectUtil.getObj(sess, "F1").getId(), recycle.getId());
		// 全てなくなっていること
		checkExists(objNames, wfObjs, true);

		// 操作履歴が出来ているか
		TestAppOpeHistUtil.assetOpeHist(sess, "tu1"//
			, "ドキュメント管理"//
			, "ごみ箱からの削除"//
			, "削除対象"//
			, "オブジェクト"//
			, target.getId()//
			, target.getName()//
			, null//
			, null//
			, 0//
			, null//
			, null//
		);

	}

	/**
	 * 
	 * @param objNames
	 * @param objs
	 * @param checkNotExist
	 * @throws Exception
	 */
	void checkExists(String[] objNames, List objs, boolean checkNotExist) throws Exception {
		List ts = new ArrayList(Arrays.asList(objNames));
		ts.addAll(objs);
		for (Iterator i = ts.iterator(); i.hasNext();) {
			Object t = i.next();
			EIMObject o = (t instanceof String) ? TestAppObjectUtil.getObj(sess, (String) t)
					: ObjectUtils.getObjectById(sess, ((EIMObject) t).getId());
			String name = (t instanceof String) ? (String) t : ("obj:" + ((EIMObject) t).getName());
			if (o == null) {
				if (!checkNotExist)
					fail("obj '" + name + "' is not exist!");
			} else {
				if (checkNotExist)
					fail("obj '" + name + "' is exist!");
			}
		}
	}

	/**
	 * 
	 * @param objName
	 * @return o
	 * @throws Exception
	 */
	EIMObject moveToRecycle(String objName) throws Exception {
		EIMObject o = TestAppObjectUtil.getObj(sess, objName);

		// ごみ箱へ
		AppObjectConditionHelper h = new AppObjectConditionHelper(sess);
		RelationUtils.deleteRelation(sess, (EIMRelation) RelationUtils.getParentRelationList(sess,
			o).get(0));
		RelationUtils.createRelation(sess, h.getRelationTypeOfDocument(), recycle, o);
		// systemセキュリティに付け替え
		ProcessFolderTreeWalker treeWalker = new ProcessFolderTreeWalker() {
			public void processDocument(EIMObject docObj, AppObjectConditionHelper helper)
					throws Exception {
				SecurityUtils.setSecurity(sess, docObj, systemSec);
			}

			public void walkIn(EIMObject upperFolderObjFrom, EIMObject lowerFolderObjTo,
					AppObjectConditionHelper helper) throws Exception {
			}

			public void walkOut(EIMObject lowerFolderObjFrom, EIMObject upperFolderObjTo,
					AppObjectConditionHelper helper) throws Exception {
				SecurityUtils.setSecurity(sess, lowerFolderObjFrom, systemSec);
			}
		};

		if (h.isTypeOfDocument(o.getType())) {
			treeWalker.processDocument(o, h);
		} else {
			AppLogicUtil.processFolderTree(sess, o, false, treeWalker, h);
		}
		return o;
	}
}
