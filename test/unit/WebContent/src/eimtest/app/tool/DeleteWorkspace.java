package eimtest.app.tool;

import java.io.BufferedReader;
import java.io.InputStreamReader;

import common.util.AppLogicUtil;
import common.util.AppObjectConditionHelper;
import common.util.AppObjectUtil;
import common.util.AppLogicUtil.ProcessFolderTreeWalker;

import eim.bo.EIMObject;
import eim.net.EIMSession;
import eim.util.ObjectUtils;
import eimtest.util.TestDBUtil;
import eimtest.util.TestSessionUtil;

/** */
public class DeleteWorkspace {
	/**
	 * 
	 * @param args
	 * @throws Exception
	 */
	public static void main(String[] args) throws Exception {
		new DeleteWorkspace().start(args);
		System.out.println("fin");
	}

	/**
	 * 
	 * @param args
	 * @throws Exception
	 */
	void start(String[] args) throws Exception {
		EIMSession sess = TestSessionUtil.createEIMSession();
		System.out.println("Delete workspace in " + TestDBUtil.getConData(sess.getDBConnection()));
		System.out.println("Enter workspace name:");
		String wsName = new BufferedReader(new InputStreamReader(System.in)).readLine();
		if (wsName.length() == 0) {
			return;
		}

		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
		EIMObject wsObj = ObjectUtils.getObjectByTypeAndName(sess, helper.getTypeOfWorkspace(),
			wsName);
		if (wsObj == null) {
			System.out.println("workspace is not found. name=" + wsName);
		}
		deleteObjects(sess, wsObj, helper);
		sess.commit();
	}

	/**
	 * 引数オブジェクトおよび下位オブジェクトを全て物理削除します。
	 * 
	 * @param sess セッション
	 * @param object 基点オブジェクト
	 * @param helper 条件判定ヘルパー
	 * @throws Exception
	 */
	void deleteObjects(EIMSession sess, EIMObject object, AppObjectConditionHelper helper)
			throws Exception {
		// 削除処理プロセッサを作成
		ProcessFolderTreeWalker processer = new ProcessFolderTreeWalker() {
			public void walkIn(EIMObject upperFolderObjFrom, EIMObject lowerFolderObjTo,
					AppObjectConditionHelper helper) throws Exception {
			}
			public void walkOut(EIMObject lowerFolderObjFrom, EIMObject upperFolderObjTo,
					AppObjectConditionHelper helper) throws Exception {
				deleteObject(lowerFolderObjFrom, helper);
			}
			public void processDocument(EIMObject object, AppObjectConditionHelper helper)
					throws Exception {
				deleteObject(object, helper);
			}

			void deleteObject(EIMObject object, AppObjectConditionHelper helper) throws Exception {
				// 補足：上方・下方リレーションの削除はFW層PL/SQLで行われるため不要

				// ステータスを持つオブジェクト(ただしワークフローフォルダ下を除く)なら、ワークフロー関連のオブジェクトを削除
				if (object.getStatus() != null && !helper.isUnderFolderWithWorkflow(object)) {
					AppObjectUtil.deleteWFRelatedObject(helper.getSession(), object);
				}

				// 物理削除
				ObjectUtils.deleteObject(helper.getSession(), object);
			}

		};

		// 再帰して削除実行
		AppLogicUtil.processFolderTree(sess, object, true, processer, helper);
	}
}
