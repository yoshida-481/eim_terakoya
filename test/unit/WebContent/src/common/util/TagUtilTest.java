package common.util;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import junit.framework.TestCase;

import common.bo.TagTreeItem;

import eim.bo.EIMAccessRole;
import eim.bo.EIMException;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMRelation;
import eim.bo.EIMRelationType;
import eim.bo.EIMResource;
import eim.bo.EIMSecurity;
import eim.bo.EIMUser;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.EIMConstant;
import eim.util.ObjectUtils;
import eim.util.RelationUtils;
import eim.util.SecurityUtils;
import eim.util.UserUtils;
import eimtest.util.TestSessionUtil;

/**
 * 
 * TagUtilのテストクラス
 * 
 */
public class TagUtilTest extends TestCase {
	
	// ★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★
	// ★　　以下の項目はDB環境に合わせて事前に書き換えておく必要があります　　★
	// ★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★
	
	/* テスト前設定項目 ここから------------------------------------------ */

	/** テスト結果のコンソール出力有無 */
	private boolean _viewTestData = true;
	
	/** システムユーザ */
	private int _userSystem = 1;
	
	/** テスト用ユーザA (セキュリティAに対して書込権限を保持) */
	private int _userA = 503;
	
	/** システムセキュリティ */
	private int _securitySystem = 109;
	
	/** セキュリティA */
	private int _securityA = 506;

	/** ワークスペースタイプ */
	private int _typeWS = 9;
	
	/** ドキュメントタイプA */
	private int _typeDocumentA = 601;

	/** フォルダタイプA */
	private int _typeFolderA = 602;

	/** タグタイプA */
	private int _typeTagA = 603;
	
	/** ドキュメントリレーション名 */
	private String _relNameDoc = "ドキュメント";

	/* テスト前設定項目 ここまで------------------------------------------ */
	

	/** ワークスペースのオブジェクトタイプ */
	EIMObjectType _objTypeWS = null;
	
	/** フォルダのオブジェクトタイプ */
	EIMObjectType _objTypeFOL = null;
	
	/** ドキュメントのオブジェクトタイプ */
	EIMObjectType _objTypeDOC = null;
	
	/** タグのオブジェクトタイプ */
	EIMObjectType _objTypeTAG = null;

	/** ドキュメントリレーション */
	EIMRelationType _relType =null;
	
	/** systemセキュリティ */
	EIMSecurity _secSystem = null;
	
	/** セキュリティA */
	EIMSecurity _secA = null;

	/** ワークスペースAのオブジェクトID */
	private int _objId_WS_A = 0;
	
	/** ワークスペースBのオブジェクトID */
	private int _objId_WS_B = 0;
	
	/** フォルダ1のオブジェクトID */
	private int _objId_FOL_1 = 0;
	
	/** フォルダ2のオブジェクトID */
	private int _objId_FOL_2 = 0;
	
	/** フォルダ3のオブジェクトID */
	private int _objId_FOL_3 = 0;
	
	/** フォルダ4のオブジェクトID */
	private int _objId_FOL_4 = 0;

	/** フォルダ5のオブジェクトID */
	private int _objId_FOL_5 = 0;
	
	/** フォルダ6のオブジェクトID */
	private int _objId_FOL_6 = 0;

	/** ドキュメント1のオブジェクトID */
	private int _objId_DOC_1 = 0;

	/** ドキュメント2のオブジェクトID */
	private int _objId_DOC_2 = 0;
	
	/** ドキュメント3のオブジェクトID */
	private int _objId_DOC_3 = 0;

	/** ドキュメント4のオブジェクトID */
	private int _objId_DOC_4 = 0;
	
	/** ドキュメント5のオブジェクトID */
	private int _objId_DOC_5 = 0;

	/** ドキュメント6のオブジェクトID */
	private int _objId_DOC_6 = 0;

	/** タグ1のオブジェクトID */
	private int _objId_TAG_1 = 0;
	
	/** タグ2のオブジェクトID */
	private int _objId_TAG_2 = 0;

	/** (ワークスペースBの)フォルダ1のオブジェクトID */
	private int _objId_FOL_B_1 = 0;

	/** (ワークスペースBの)もうひとつのフォルダ1のオブジェクトID */
	private int _objId_FOL_B_1_OTHER = 0;

	/** (ワークスペースBの)フォルダ2のオブジェクトID */
	private int _objId_FOL_B_2 = 0;
	
	/** (ワークスペースBの)フォルダ3のオブジェクトID */
	private int _objId_FOL_B_3 = 0;
	
	/** (ワークスペースBの)フォルダ4のオブジェクトID */
	private int _objId_FOL_B_4 = 0;

	/** (ワークスペースBの)ドキュメント1のオブジェクトID */
	private int _objId_DOC_B_1 = 0;

	/** (ワークスペースBの)もうひとつのドキュメント1のオブジェクトID */
	private int _objId_DOC_B_1_OTHER = 0;
	
	/** (ワークスペースBの)タグ1のオブジェクトID */
	private int _objId_TAG_B_1 = 0;

	/** (ワークスペースBの)タグ2のオブジェクトID */
	private int _objId_TAG_B_2 = 0;

	/** (ワークスペースBの)タグ3のオブジェクトID */
	private int _objId_TAG_B_3 = 0;

	/** DB環境準備OKフラグ */
	private boolean isPreparationOK = false;

	
	/**
	 * テスト環境の事前チェック
	 */
	protected void setUp() throws Exception{

		EIMSession sess = null;
		
		try {
			sess = TestSessionUtil.createEIMSession();
			
			// タイプ取得
			_objTypeWS = ObjectUtils.getObjectTypeById(sess, _typeWS);
			
			if (_objTypeWS == null) {
				System.out.println("ID[" + _typeWS + "]に該当するワークスペースタイプが存在しません。DB環境を整えてからJUnitを実行して下さい。");
				return;
			}
			
			_objTypeFOL = ObjectUtils.getObjectTypeById(sess, _typeFolderA);
			if (_objTypeFOL == null) {
				System.out.println("ID[" + _typeFolderA + "]に該当するフォルダタイプが存在しません。DB環境を整えてからJUnitを実行して下さい。");
				return;
			}
			
			_objTypeDOC = ObjectUtils.getObjectTypeById(sess, _typeDocumentA);
			if (_objTypeDOC == null) {
				System.out.println("ID[" + _typeDocumentA + "]に該当するドキュメントタイプが存在しません。DB環境を整えてからJUnitを実行して下さい。");
				return;
			}

			_objTypeTAG = ObjectUtils.getObjectTypeById(sess, _typeTagA);
			if (_objTypeTAG == null) {
				System.out.println("ID[" + _typeTagA + "]に該当するタグタイプが存在しません。DB環境を整えてからJUnitを実行して下さい。");
				return;
			}

			// リレーション取得
			_relType = RelationUtils.getRelationTypeByName(sess, _relNameDoc);
			if (_relType == null) {
				System.out.println("[" + _relNameDoc + "]に該当するリレーションタイプが存在しません。DB環境を整えてからJUnitを実行して下さい。");
				return;
			}
			
			// セキュリティ取得
			_secSystem = SecurityUtils.getSecurityById(sess, _securitySystem);
			if (_secSystem == null) {
				System.out.println("ID[" + _securitySystem + "]に該当するシステムセキュリティが存在しません。DB環境を整えてからJUnitを実行して下さい。");
				return;
			}
			
			_secA = SecurityUtils.getSecurityById(sess, _securityA);
			if (_secA == null) {
				System.out.println("ID[" + _securityA + "]に該当するセキュリティが存在しません。DB環境を整えてからJUnitを実行して下さい。");
				return;
			}

			EIMUser userA = UserUtils.getUserById(sess, _userA);
			if (userA == null) {
				System.out.println("ID[" + _userA + "]に該当するユーザが存在しません。DB環境を整えてからJUnitを実行して下さい。");
				return;
			}
			
			if(!SecurityUtils.enabled(sess, _secA, userA, EIMAccessRole.CREATE)) {
				System.out.println("ID[" + _userA + "]に該当するユーザは、[" + _secA.getName() + "に対して書込権限を保持しません。DB環境を整えてからJUnitを実行して下さい。");
				return;
			}
			
			isPreparationOK = true;

		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {
			if (sess != null) {
				sess.close();
			}
		}
	}
	
	/**
	 * TagUtil.getTagTree(EIMSession sess, EIMObject obj) のためのテスト・メソッド
	 * 
	 * @throws Exception
	 */
	public void testGetTagTree() throws Exception {

		if (!isPreparationOK) return;
		
		EIMSession sess = null;
		
		/**
		 * 正常(1) ワークスペース直下から末端まで付与、取得
		 * 
		 */
		try {
			sess = TestSessionUtil.createEIMSession();
			
			// テストデータ準備
			createTestData1(sess);
			
			// タグ付与対象
			int[] targetIds = {
					_objId_FOL_1,
					_objId_FOL_6,
					_objId_DOC_5,
					_objId_DOC_6
					};
			
			// 付与するタグ
			int tagObjId = _objId_TAG_2;
			
			// タグ付与
			setTag(sess, tagObjId, targetIds);

			EIMObject wsObjA = ObjectUtils.getObjectById(sess, _objId_WS_A);
			EIMObject tagObj = ObjectUtils.getObjectById(sess, tagObjId);
			
			// 事前確認
			viewFolderTree(sess, wsObjA, "正常(1)");
			
			// メソッド実行
			TagTreeItem tree = TagUtil.getTagTree(sess, tagObj);
			
			// 結果確認
			viewTagTree(tree, "正常(1)");
			
			// 正誤判定 (ツリー上の存在すべき階層にあるか)
			assertTrue(searchTagTreeRecurrently(tree, _objId_FOL_1, 1));
			assertTrue(searchTagTreeRecurrently(tree, _objId_FOL_2, 2));
			assertTrue(searchTagTreeRecurrently(tree, _objId_FOL_3, 3));
			assertTrue(searchTagTreeRecurrently(tree, _objId_FOL_4, 3));
			assertTrue(searchTagTreeRecurrently(tree, _objId_FOL_5, 2));
			assertTrue(searchTagTreeRecurrently(tree, _objId_FOL_6, 1));
			assertTrue(searchTagTreeRecurrently(tree, _objId_DOC_1, 4));
			assertTrue(searchTagTreeRecurrently(tree, _objId_DOC_2, 4));
			assertTrue(searchTagTreeRecurrently(tree, _objId_DOC_3, 3));
			assertTrue(searchTagTreeRecurrently(tree, _objId_DOC_4, 2));
			assertTrue(searchTagTreeRecurrently(tree, _objId_DOC_5, 1));
			assertTrue(searchTagTreeRecurrently(tree, _objId_DOC_6, 1));
			assertTrue(searchTagTreeRecurrently(tree, _objId_TAG_1, 3));
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {
			
			if (sess != null) {
				sess.rollback();
				sess.close();
			}
		}
		
		
		/**
		 * 正常(2) 付与対象間のフォルダがない場合は補完する
		 * 
		 */
		try {
			sess = TestSessionUtil.createEIMSession();
			
			// テストデータ準備
			createTestData1(sess);
			
			// タグ付与対象
			int[] targetIds = {
					_objId_FOL_1
					};
			
			// 付与するタグ
			int tagObjId = _objId_TAG_2;
			
			// タグ付与
			setTag(sess, tagObjId, targetIds);
			
			// タグ解除
			cancelTag(sess, _objId_FOL_2);
			cancelTag(sess, _objId_FOL_3);

			EIMObject wsObjA = ObjectUtils.getObjectById(sess, _objId_WS_A);
			EIMObject tagObj = ObjectUtils.getObjectById(sess, tagObjId);
			
			// 事前確認
			viewFolderTree(sess, wsObjA, "正常(2)");
			
			// メソッド実行
			TagTreeItem tree = TagUtil.getTagTree(sess, tagObj);
			
			// 結果確認
			viewTagTree(tree, "正常(2)");
			
			// 正誤判定 (ツリー上の存在すべき階層にあるか)
			assertTrue(searchTagTreeRecurrently(tree, _objId_FOL_1, 1));
			assertTrue(searchTagTreeRecurrently(tree, _objId_FOL_2, 2));
			assertTrue(searchTagTreeRecurrently(tree, _objId_FOL_3, 3));
			assertTrue(searchTagTreeRecurrently(tree, _objId_DOC_1, 4));
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {
			
			if (sess != null) {
				sess.rollback();
				sess.close();
			}
		}


		/**
		 * 正常(3) ワークスペース直下から途中まで
		 * 
		 */
		try {
			sess = TestSessionUtil.createEIMSession();
			
			// テストデータ準備
			createTestData1(sess);
			
			// タグ付与対象
			int[] targetIds = {
					_objId_FOL_1,
					_objId_FOL_6
					};
			
			// 付与するタグ
			int tagObjId = _objId_TAG_2;
			
			// タグ付与
			setTag(sess, tagObjId, targetIds);
			
			// タグ解除
			cancelTag(sess, _objId_FOL_3);
			cancelTag(sess, _objId_FOL_4);
			cancelTag(sess, _objId_FOL_5);
			cancelTag(sess, _objId_DOC_1);
			cancelTag(sess, _objId_DOC_2);
			cancelTag(sess, _objId_DOC_4);
			cancelTag(sess, _objId_TAG_1);

			EIMObject wsObjA = ObjectUtils.getObjectById(sess, _objId_WS_A);
			EIMObject tagObj = ObjectUtils.getObjectById(sess, tagObjId);
			
			// 事前確認
			viewFolderTree(sess, wsObjA, "正常(3)");
			
			// メソッド実行
			TagTreeItem tree = TagUtil.getTagTree(sess, tagObj);
			
			// 結果確認
			viewTagTree(tree, "正常(3)");
			
			// 正誤判定 (ツリー上の存在すべき階層にあるか)
			assertTrue(searchTagTreeRecurrently(tree, _objId_FOL_1, 1));
			assertTrue(searchTagTreeRecurrently(tree, _objId_FOL_2, 2));
			assertTrue(searchTagTreeRecurrently(tree, _objId_FOL_5, 2));
			assertTrue(searchTagTreeRecurrently(tree, _objId_FOL_6, 1));
			assertTrue(searchTagTreeRecurrently(tree, _objId_DOC_3, 3));
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {

			if (sess != null) {
				sess.rollback();
				sess.close();
			}
		}
		
		
		/**
		 * 正常(4) 途中から末端まで
		 * 
		 */
		try {
			sess = TestSessionUtil.createEIMSession();
			
			// テストデータ準備
			createTestData1(sess);
			
			// タグ付与対象
			int[] targetIds = {
					_objId_FOL_1,
					_objId_FOL_6
					};
			
			// 付与するタグ
			int tagObjId = _objId_TAG_2;
			
			// タグ付与
			setTag(sess, tagObjId, targetIds);
			
			// タグ解除
			cancelTag(sess, _objId_FOL_1);
			cancelTag(sess, _objId_FOL_2);
			cancelTag(sess, _objId_FOL_6);

			EIMObject wsObjA = ObjectUtils.getObjectById(sess, _objId_WS_A);
			EIMObject tagObj = ObjectUtils.getObjectById(sess, tagObjId);
			
			// 事前確認
			viewFolderTree(sess, wsObjA, "正常(4)");
			
			// メソッド実行
			TagTreeItem tree = TagUtil.getTagTree(sess, tagObj);
			
			// 結果確認
			viewTagTree(tree, "正常(4)");
			
			// 正誤判定 (ツリー上の存在すべき階層にあるか)
			assertTrue(searchTagTreeRecurrently(tree, _objId_FOL_3, 1));
			assertTrue(searchTagTreeRecurrently(tree, _objId_FOL_4, 1));
			assertTrue(searchTagTreeRecurrently(tree, _objId_FOL_5, 1));
			assertTrue(searchTagTreeRecurrently(tree, _objId_DOC_4, 1));
			assertTrue(searchTagTreeRecurrently(tree, _objId_DOC_1, 2));
			assertTrue(searchTagTreeRecurrently(tree, _objId_DOC_2, 2));
			assertTrue(searchTagTreeRecurrently(tree, _objId_DOC_3, 2));
			assertTrue(searchTagTreeRecurrently(tree, _objId_TAG_1, 1));
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {
			
			if (sess != null) {
				sess.rollback();
				sess.close();
			}
		}
		
		
		/**
		 * 正常(5) 途中に読取権限のないフォルダが混入
		 * 
		 */
		try {
			sess = new EIMSession();
			sess.setConsole();
			
			// 一般ユーザ
			EIMUser nonAuthUser = UserUtils.getUserById(sess, _userA);
			sess.setUser(nonAuthUser);
			
			// テストデータ準備
			createTestData1(sess);
			
			// タグ付与対象
			int[] targetIds = {
					_objId_FOL_1,
					_objId_FOL_6,
					_objId_DOC_5,
					_objId_DOC_6
					};
			
			// 付与するタグ
			int tagObjId = _objId_TAG_2;
			
			// タグ付与
			setTag(sess, tagObjId, targetIds);
			
			// systemユーザにセッション変更
			EIMUser systemUser = UserUtils.getUserById(sess, _userSystem);
			sess.setUser(systemUser);

			// フォルダのセキュリティを変更
			// セキュリティ変更対象
			int[] chgSecIds = {
					_objId_FOL_2,
					_objId_FOL_4,
					_objId_FOL_6,
					_objId_DOC_6
					};
			setSecurity(sess, _secSystem, chgSecIds);
			
			EIMObject wsObjA = ObjectUtils.getObjectById(sess, _objId_WS_A);
			EIMObject tagObj = ObjectUtils.getObjectById(sess, tagObjId);
			
			// 事前確認
			viewFolderTree(sess, wsObjA, "正常(5)");
			
			// 一般ユーザにセッションを戻す
			sess.setUser(nonAuthUser);
			
			// メソッド実行
			TagTreeItem tree = TagUtil.getTagTree(sess, tagObj);
			
			// 結果確認
			viewTagTree(tree, "正常(5)");
			
			// 正誤判定 (ツリー上の存在すべき階層にあるか)
			assertTrue(searchTagTreeRecurrently(tree, _objId_FOL_1, 1));
			assertTrue(searchTagTreeRecurrently(tree, _objId_FOL_3, 1));
			assertTrue(searchTagTreeRecurrently(tree, _objId_FOL_5, 2));
			assertTrue(searchTagTreeRecurrently(tree, _objId_DOC_3, 3));
			assertTrue(searchTagTreeRecurrently(tree, _objId_DOC_1, 2));
			assertTrue(searchTagTreeRecurrently(tree, _objId_DOC_2, 1));
			assertTrue(searchTagTreeRecurrently(tree, _objId_DOC_4, 1));
			assertTrue(searchTagTreeRecurrently(tree, _objId_DOC_5, 1));
			assertTrue(searchTagTreeRecurrently(tree, _objId_TAG_1, 1));
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {
			
			if (sess != null) {
				sess.rollback();
				sess.close();
			}
		}
		
		
		/**
		 * 正常(6) WSまたがり
		 * 
		 */
		try {
			sess = TestSessionUtil.createEIMSession();
			
			// テストデータ準備
			createTestData1(sess);
			createTestData2(sess);
			
			// タグ付与対象
			int[] targetIds = {
					_objId_FOL_1,
					_objId_FOL_B_1
					};
			
			// 付与するタグ
			int tagObjId = _objId_TAG_2;
			
			// タグ付与
			setTag(sess, tagObjId, targetIds);
			
			EIMObject wsObjA = ObjectUtils.getObjectById(sess, _objId_WS_A);
			EIMObject wsObjB = ObjectUtils.getObjectById(sess, _objId_WS_B);
			EIMObject tagObj = ObjectUtils.getObjectById(sess, tagObjId);
			
			// 事前確認
			viewFolderTree(sess, wsObjA, "正常(6)");
			viewFolderTree(sess, wsObjB, "正常(6)");
			
			// メソッド実行
			TagTreeItem tree = TagUtil.getTagTree(sess, tagObj);
			
			// 結果確認
			viewTagTree(tree, "正常(6)");
			
			// 正誤判定 (ツリー上の存在すべき階層にあるか)
			assertTrue(searchTagTreeRecurrently(tree, _objId_FOL_1, 1));
			assertTrue(searchTagTreeRecurrently(tree, _objId_FOL_B_1, 1));
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {
			
			if (sess != null) {
				sess.rollback();
				sess.close();
			}
		}
	}
	
	
	/**
	 * TagUtil.getTagTreeWithChild(EIMSession sess, EIMObject obj) のためのテスト・メソッド
	 * 
	 * @throws Exception
	 */
	public void testGetTagTreeWithChild() throws Exception {
		
		if (!isPreparationOK) return;

		EIMSession sess = null;
		
		boolean isExceptionExist = false;		

		/**
		 * 正常(1) タグ配下のタグが存在しない
		 * 
		 */
		try {
			sess = TestSessionUtil.createEIMSession();
			
			// テストデータ準備
			createTestData1(sess);
			
			// タグ付与対象
			int[] targetIds = {
					_objId_FOL_3,
					_objId_FOL_4,
					};
			
			// 付与するタグ
			int tagObjId = _objId_TAG_2;
			
			// タグ付与
			setTag(sess, tagObjId, targetIds);

			EIMObject wsObjA = ObjectUtils.getObjectById(sess, _objId_WS_A);
			EIMObject tagObj = ObjectUtils.getObjectById(sess, tagObjId);
			
			// 事前確認
			viewFolderTree(sess, wsObjA, "正常(1)");
			
			// メソッド実行
			TagTreeItem tree = TagUtil.getTagTreeWithChild(sess, tagObj);
			
			// 結果確認
			viewTagTree(tree, "正常(1)");
			
			// 正誤判定 (ツリー上の存在すべき階層にあるか)
			assertTrue(searchTagTreeRecurrently(tree, _objId_FOL_3, 1));
			assertTrue(searchTagTreeRecurrently(tree, _objId_FOL_4, 1));
			assertTrue(searchTagTreeRecurrently(tree, _objId_DOC_1, 2));
			assertTrue(searchTagTreeRecurrently(tree, _objId_DOC_2, 2));
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {

			if (sess != null) {
				sess.rollback();
				sess.close();
			}
		}

		
		/**
		 * 正常(2) タグ配下のタグが存在する
		 * 
		 */
		try {
			sess = TestSessionUtil.createEIMSession();
			
			// テストデータ準備
			createTestData1(sess);
			createTestData2(sess);
			
			//[1回目]
			// タグ付与対象
			int[] targetIds = {
					_objId_FOL_1,
					_objId_FOL_B_4
					};
			
			// 付与するタグ
			int tagObjId = _objId_TAG_2;
			
			// タグ付与
			setTag(sess, tagObjId, targetIds);
			
			//[2回目]
			// タグ付与対象
			int[] targetIds2 = {
					_objId_FOL_B_1,
					_objId_FOL_B_4
					};
			
			// 付与するタグ
			int tagObjId2 = _objId_TAG_1;
			
			// タグ付与
			setTag(sess, tagObjId2, targetIds2);
			
			//[3回目]
			// タグ付与対象
			int[] targetIds3 = {
					_objId_TAG_B_1,
					_objId_TAG_B_2
					};
			
			// 付与するタグ
			int tagObjId3 = _objId_TAG_B_3;
			
			// タグ付与
			setTag(sess, tagObjId3, targetIds3);
			
			//[4回目]
			// タグ付与対象
			int[] targetIds4 = {
					_objId_FOL_6
					};
			
			// 付与するタグ
			int tagObjId4 = _objId_TAG_B_2;
			
			// タグ付与
			setTag(sess, tagObjId4, targetIds4);


			EIMObject wsObjA = ObjectUtils.getObjectById(sess, _objId_WS_A);
			EIMObject wsObjB = ObjectUtils.getObjectById(sess, _objId_WS_B);
			EIMObject tagObj = ObjectUtils.getObjectById(sess, tagObjId);
			
			// 事前確認
			viewFolderTree(sess, wsObjA, "正常(2)");
			viewFolderTree(sess, wsObjB, "正常(2)");
			
			// メソッド実行
			TagTreeItem tree = TagUtil.getTagTreeWithChild(sess, tagObj);
			
			// 結果確認
			viewTagTree(tree, "正常(2)");
			
			// 正誤判定 (ツリー上の存在すべき階層にあるか)
			assertTrue(searchTagTreeRecurrently(tree, _objId_FOL_1, 1));
			assertTrue(searchTagTreeRecurrently(tree, _objId_FOL_2, 2));
			assertTrue(searchTagTreeRecurrently(tree, _objId_TAG_1, 3));
			assertTrue(searchTagTreeRecurrently(tree, _objId_FOL_B_4, 4));
			assertTrue(searchTagTreeRecurrently(tree, _objId_TAG_B_3, 5));
			assertTrue(searchTagTreeRecurrently(tree, _objId_TAG_B_2, 6));
			assertTrue(searchTagTreeRecurrently(tree, _objId_FOL_6, 7));
			assertTrue(searchTagTreeRecurrently(tree, _objId_DOC_4, 8));
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {

			if (sess != null) {
				sess.rollback();
				sess.close();
			}
		}
		
		
		/**
		 * 異常(1) 無限ループ
		 * 
		 */
		try {
			isExceptionExist = false;

			sess = TestSessionUtil.createEIMSession();
			
			// テストデータ準備
			createTestData1(sess);
			createTestData2(sess);
			
			//[1回目]
			// タグ付与対象
			int[] targetIds = {
					_objId_FOL_1
					};
			
			// 付与するタグ
			int tagObjId = _objId_TAG_2;
			
			// タグ付与
			setTag(sess, tagObjId, targetIds);
			
			//[2回目]
			// タグ付与対象
			int[] targetIds2 = {
					_objId_FOL_B_4
					};
			
			// 付与するタグ
			int tagObjId2 = _objId_TAG_1;
			
			// タグ付与
			setTag(sess, tagObjId2, targetIds2);
			
			//[3回目]
			// タグ付与対象
			int[] targetIds3 = {
					_objId_FOL_1,
					_objId_FOL_2,
					_objId_TAG_1
					};
			
			// 付与するタグ
			int tagObjId3 = _objId_TAG_B_3;
			
			// タグ付与
			setTagForce(sess, tagObjId3, targetIds3);
			
			EIMObject wsObjA = ObjectUtils.getObjectById(sess, _objId_WS_A);
			EIMObject wsObjB = ObjectUtils.getObjectById(sess, _objId_WS_B);
			EIMObject tagObj = ObjectUtils.getObjectById(sess, tagObjId);
			
			// 事前確認
			viewFolderTree(sess, wsObjA, "異常(1)");
			viewFolderTree(sess, wsObjB, "異常(1)");
			
			// メソッド実行
			TagTreeItem tree = TagUtil.getTagTreeWithChild(sess, tagObj);
			
			// 結果確認
			viewTagTree(tree, "異常(1)");
			
			
		} catch (EIMException eime) {

			isExceptionExist = true;
			assertEquals(eime.getMessage(), EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.TAGASSIGN.TAG.LOOPED"));
			if (_viewTestData) eime.printStackTrace();
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {

			if (sess != null) {
				sess.rollback();
				sess.close();
			}
			// 例外発生の確認
			assertTrue(isExceptionExist);
		}
	}
	
	
	
	/**
	 * TagUtil.getOnlyTagTree(EIMSession sess, EIMObject obj) のためのテスト・メソッド
	 * 
	 * @throws Exception
	 */
	public void testGetOnlyTagTree() throws Exception {
		
		if (!isPreparationOK) return;
		
		EIMSession sess = null;
		
		boolean isExceptionExist = false;	

		/**
		 * 正常(1) タグ配下のタグが存在しない
		 * 
		 */
		try {
			sess = TestSessionUtil.createEIMSession();
			
			// テストデータ準備
			createTestData1(sess);
			
			// タグ付与対象
			int[] targetIds = {
					_objId_FOL_3,
					_objId_FOL_4,
					};
			
			// 付与するタグ
			int tagObjId = _objId_TAG_2;
			
			// タグ付与
			setTag(sess, tagObjId, targetIds);

			EIMObject wsObjA = ObjectUtils.getObjectById(sess, _objId_WS_A);
			EIMObject tagObj = ObjectUtils.getObjectById(sess, tagObjId);
			
			// 事前確認
			viewFolderTree(sess, wsObjA, "正常(1)");
			
			// メソッド実行
			TagTreeItem tree = TagUtil.getOnlyTagTree(sess, tagObj);
			
			// 結果確認
			viewTagTree(tree, "正常(1)");
			
			// 正誤判定 (ツリー上の存在すべき階層にあるか)
			assertTrue(searchTagTreeRecurrently(tree, _objId_TAG_2, 0));
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {

			if (sess != null) {
				sess.rollback();
				sess.close();
			}
		}

		
		/**
		 * 正常(2) タグ配下のタグが存在する
		 * 
		 */
		try {
			sess = TestSessionUtil.createEIMSession();
			
			// テストデータ準備
			createTestData1(sess);
			createTestData2(sess);
			
			//[1回目]
			// タグ付与対象
			int[] targetIds = {
					_objId_FOL_1,
					_objId_FOL_B_4
					};
			
			// 付与するタグ
			int tagObjId = _objId_TAG_2;
			
			// タグ付与
			setTag(sess, tagObjId, targetIds);
			
			//[2回目]
			// タグ付与対象
			int[] targetIds2 = {
					_objId_FOL_B_1,
					_objId_FOL_B_4
					};
			
			// 付与するタグ
			int tagObjId2 = _objId_TAG_1;
			
			// タグ付与
			setTag(sess, tagObjId2, targetIds2);
			
			//[3回目]
			// タグ付与対象
			int[] targetIds3 = {
					_objId_TAG_B_1,
					_objId_TAG_B_2
					};
			
			// 付与するタグ
			int tagObjId3 = _objId_TAG_B_3;
			
			// タグ付与
			setTag(sess, tagObjId3, targetIds3);
			
			//[4回目]
			// タグ付与対象
			int[] targetIds4 = {
					_objId_FOL_6
					};
			
			// 付与するタグ
			int tagObjId4 = _objId_TAG_B_2;
			
			// タグ付与
			setTag(sess, tagObjId4, targetIds4);


			EIMObject wsObjA = ObjectUtils.getObjectById(sess, _objId_WS_A);
			EIMObject wsObjB = ObjectUtils.getObjectById(sess, _objId_WS_B);
			EIMObject tagObj = ObjectUtils.getObjectById(sess, tagObjId);
			
			// 事前確認
			viewFolderTree(sess, wsObjA, "正常(2)");
			viewFolderTree(sess, wsObjB, "正常(2)");
			
			// メソッド実行
			TagTreeItem tree = TagUtil.getOnlyTagTree(sess, tagObj);
			
			// 結果確認
			viewTagTree(tree, "正常(2)");
			
			// 正誤判定 (ツリー上の存在すべき階層にあるか)
			assertTrue(searchTagTreeRecurrently(tree, _objId_TAG_1, 1));
			assertTrue(searchTagTreeRecurrently(tree, _objId_TAG_B_1, 2));
			assertTrue(searchTagTreeRecurrently(tree, _objId_TAG_B_2, 2));
			assertTrue(searchTagTreeRecurrently(tree, _objId_TAG_B_3, 2));
			assertTrue(searchTagTreeRecurrently(tree, _objId_TAG_B_1, 3));
			assertTrue(searchTagTreeRecurrently(tree, _objId_TAG_B_2, 3));
			assertTrue(searchTagTreeRecurrently(tree, _objId_TAG_B_1, 1));
			assertTrue(searchTagTreeRecurrently(tree, _objId_TAG_B_2, 1));
			assertTrue(searchTagTreeRecurrently(tree, _objId_TAG_B_3, 1));
			assertTrue(searchTagTreeRecurrently(tree, _objId_TAG_B_1, 2));
			assertTrue(searchTagTreeRecurrently(tree, _objId_TAG_B_2, 2));
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {

			if (sess != null) {
				sess.rollback();
				sess.close();
			}
		}
		
		
		/**
		 * 異常(1) 無限ループ
		 * 
		 */
		try {
			isExceptionExist = false;

			sess = TestSessionUtil.createEIMSession();
			
			// テストデータ準備
			createTestData1(sess);
			createTestData2(sess);
			
			//[1回目]
			// タグ付与対象
			int[] targetIds = {
					_objId_FOL_1
					};
			
			// 付与するタグ
			int tagObjId = _objId_TAG_2;
			
			// タグ付与
			setTag(sess, tagObjId, targetIds);
			
			//[2回目]
			// タグ付与対象
			int[] targetIds2 = {
					_objId_FOL_B_4
					};
			
			// 付与するタグ
			int tagObjId2 = _objId_TAG_1;
			
			// タグ付与
			setTag(sess, tagObjId2, targetIds2);
			
			//[3回目]
			// タグ付与対象
			int[] targetIds3 = {
					_objId_FOL_1,
					_objId_FOL_2,
					_objId_TAG_1
					};
			
			// 付与するタグ
			int tagObjId3 = _objId_TAG_B_3;
			
			// タグ付与
			setTagForce(sess, tagObjId3, targetIds3);
			
			EIMObject wsObjA = ObjectUtils.getObjectById(sess, _objId_WS_A);
			EIMObject wsObjB = ObjectUtils.getObjectById(sess, _objId_WS_B);
			EIMObject tagObj = ObjectUtils.getObjectById(sess, tagObjId);
			
			// 事前確認
			viewFolderTree(sess, wsObjA, "異常(1)");
			viewFolderTree(sess, wsObjB, "異常(1)");
			
			// メソッド実行
			TagUtil.getOnlyTagTree(sess, tagObj);
			
		} catch (EIMException eime) {

			isExceptionExist = true;
			assertEquals(eime.getMessage(), EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.TAGASSIGN.TAG.LOOPED"));
			if (_viewTestData) eime.printStackTrace();
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {

			if (sess != null) {
				sess.rollback();
				sess.close();
			}
			// 例外発生の確認
			assertTrue(isExceptionExist);
		}
	}
	
	
	
	/**
	 * TagUtil.getParentTags(EIMSession sess, EIMObject obj) のためのテスト・メソッド
	 * 
	 * @throws Exception
	 */
	public void testGetParentTags() throws Exception {
		
		if (!isPreparationOK) return;
		
		EIMSession sess = null;
		
		boolean isExceptionExist = false;	
		
		/**
		 * 正常(1) タグ配下のタグが存在する
		 * 
		 */
		try {
			sess = TestSessionUtil.createEIMSession();
			
			// テストデータ準備
			createTestData1(sess);
			createTestData2(sess);
			
			//[1回目]
			// タグ付与対象
			int[] targetIds = {
					_objId_FOL_1,
					_objId_FOL_B_4
					};
			
			// 付与するタグ
			int tagObjId = _objId_TAG_2;
			
			// タグ付与
			setTag(sess, tagObjId, targetIds);
			
			//[2回目]
			// タグ付与対象
			int[] targetIds2 = {
					_objId_FOL_B_1,
					_objId_FOL_B_4
					};
			
			// 付与するタグ
			int tagObjId2 = _objId_TAG_1;
			
			// タグ付与
			setTag(sess, tagObjId2, targetIds2);
			
			//[3回目]
			// タグ付与対象
			int[] targetIds3 = {
					_objId_TAG_B_1,
					_objId_TAG_B_2
					};
			
			// 付与するタグ
			int tagObjId3 = _objId_TAG_B_3;
			
			// タグ付与
			setTag(sess, tagObjId3, targetIds3);
			
			//[4回目]
			// タグ付与対象
			int[] targetIds4 = {
					_objId_FOL_6
					};
			
			// 付与するタグ
			int tagObjId4 = _objId_TAG_B_2;
			
			// タグ付与
			setTag(sess, tagObjId4, targetIds4);

			//[5回目]
			// タグ付与対象
			int[] targetIds5 = {
					_objId_TAG_B_2
					};
			
			// 付与するタグ
			int tagObjId5 = _objId_TAG_B_1;
			
			// タグ付与
			setTag(sess, tagObjId5, targetIds5);

			
			EIMObject wsObjA = ObjectUtils.getObjectById(sess, _objId_WS_A);
			EIMObject wsObjB = ObjectUtils.getObjectById(sess, _objId_WS_B);
			EIMObject folObj = ObjectUtils.getObjectById(sess, _objId_FOL_6);
			
			// 事前確認
			viewFolderTree(sess, wsObjA, "正常(1)");
			viewFolderTree(sess, wsObjB, "正常(1)");
			
			// メソッド実行
			List objList = TagUtil.getParentTags(sess, folObj);
			
			// 結果確認
			viewList(objList, "正常(1)");
			
			// 正誤判定 (ツリー上の存在すべき階層にあるか)
			assertTrue(searchObjList(objList, _objId_TAG_1));
			assertTrue(searchObjList(objList, _objId_TAG_2));
			assertTrue(searchObjList(objList, _objId_TAG_B_1));
			assertTrue(searchObjList(objList, _objId_TAG_B_2));
			assertTrue(searchObjList(objList, _objId_TAG_B_3));
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {

			if (sess != null) {
				sess.rollback();
				sess.close();
			}
		}
		
		
		/**
		 * 異常(1) 無限ループ
		 * 
		 */
		try {
			isExceptionExist = false;

			sess = TestSessionUtil.createEIMSession();
			
			// テストデータ準備
			createTestData1(sess);
			createTestData2(sess);
			
			//[1回目]
			// タグ付与対象
			int[] targetIds = {
					_objId_FOL_1,
					_objId_FOL_B_4
					};
			
			// 付与するタグ
			int tagObjId = _objId_TAG_2;
			
			// タグ付与
			setTag(sess, tagObjId, targetIds);
			
			//[2回目]
			// タグ付与対象
			int[] targetIds2 = {
					_objId_FOL_B_1,
					_objId_FOL_B_4
					};
			
			// 付与するタグ
			int tagObjId2 = _objId_TAG_1;
			
			// タグ付与
			setTag(sess, tagObjId2, targetIds2);
			
			//[3回目]
			// タグ付与対象
			int[] targetIds3 = {
					_objId_TAG_B_1,
					_objId_TAG_B_2
					};
			
			// 付与するタグ
			int tagObjId3 = _objId_TAG_B_3;
			
			// タグ付与
			setTag(sess, tagObjId3, targetIds3);
			
			//[4回目]
			// タグ付与対象
			int[] targetIds4 = {
					_objId_FOL_6
					};
			
			// 付与するタグ
			int tagObjId4 = _objId_TAG_B_2;
			
			// タグ付与
			setTag(sess, tagObjId4, targetIds4);

			//[5回目]
			// タグ付与対象
			int[] targetIds5 = {
					_objId_TAG_1
					};
			
			// 付与するタグ
			int tagObjId5 = _objId_TAG_B_1;
			
			// タグ付与
			setTagForce(sess, tagObjId5, targetIds5);
			
			EIMObject wsObjA = ObjectUtils.getObjectById(sess, _objId_WS_A);
			EIMObject wsObjB = ObjectUtils.getObjectById(sess, _objId_WS_B);
			EIMObject folObj = ObjectUtils.getObjectById(sess, _objId_FOL_6);
			
			// 事前確認
			viewFolderTree(sess, wsObjA, "異常(1)");
			viewFolderTree(sess, wsObjB, "異常(1)");
			
			// メソッド実行
			TagUtil.getParentTags(sess, folObj);
			
		} catch (EIMException eime) {

			isExceptionExist = true;
			assertEquals(eime.getMessage(), EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.TAGASSIGN.TAG.LOOPED"));
			if (_viewTestData) eime.printStackTrace();
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {

			if (sess != null) {
				sess.rollback();
				sess.close();
			}
			// 例外発生の確認
			assertTrue(isExceptionExist);
		}
	}

	
	/**
	 * TagUtil.getTagGivenObj(EIMSession, EIMObject) のためのテスト・メソッド
	 * 
	 * @throws Exception
	 */
	public void testGetTagGivenObj() throws Exception {
		
		if (!isPreparationOK) return;

		EIMSession sess = null;
		
		/**
		 * 正常(1) タグ配下のタグが存在する
		 * 
		 */
		try {
			sess = TestSessionUtil.createEIMSession();
			
			// テストデータ準備
			createTestData1(sess);
			createTestData2(sess);
			
			//[1回目]
			// タグ付与対象
			int[] targetIds = {
					_objId_FOL_1,
					_objId_FOL_B_4
					};
			
			// 付与するタグ
			int tagObjId = _objId_TAG_2;
			
			// タグ付与
			setTag(sess, tagObjId, targetIds);
			
			//[2回目]
			// タグ付与対象
			int[] targetIds2 = {
					_objId_FOL_B_1,
					_objId_FOL_B_4
					};
			
			// 付与するタグ
			int tagObjId2 = _objId_TAG_1;
			
			// タグ付与
			setTag(sess, tagObjId2, targetIds2);
			
			//[3回目]
			// タグ付与対象
			int[] targetIds3 = {
					_objId_TAG_B_1,
					_objId_TAG_B_2
					};
			
			// 付与するタグ
			int tagObjId3 = _objId_TAG_B_3;
			
			// タグ付与
			setTag(sess, tagObjId3, targetIds3);
			
			//[4回目]
			// タグ付与対象
			int[] targetIds4 = {
					_objId_FOL_6
					};
			
			// 付与するタグ
			int tagObjId4 = _objId_TAG_B_2;
			
			// タグ付与
			setTag(sess, tagObjId4, targetIds4);

			//[5回目]
			// タグ付与対象
			int[] targetIds5 = {
					_objId_TAG_B_2
					};
			
			// 付与するタグ
			int tagObjId5 = _objId_TAG_B_1;
			
			// タグ付与
			setTag(sess, tagObjId5, targetIds5);

			
			EIMObject wsObjA = ObjectUtils.getObjectById(sess, _objId_WS_A);
			EIMObject wsObjB = ObjectUtils.getObjectById(sess, _objId_WS_B);
			EIMObject tagObj = ObjectUtils.getObjectById(sess, _objId_TAG_1);
			
			// 事前確認
			viewFolderTree(sess, wsObjA, "正常(1)");
			viewFolderTree(sess, wsObjB, "正常(1)");
			
			// メソッド実行
			List objList = TagUtil.getTagGivenObj(sess, tagObj);
			
			// 結果確認
			viewList(objList, "正常(1)");
			
			// 正誤判定 (ツリー上の存在すべき階層にあるか)
			assertTrue(searchObjList(objList, _objId_TAG_B_1));
			assertTrue(searchObjList(objList, _objId_TAG_B_2));
			assertTrue(searchObjList(objList, _objId_TAG_B_3));
			assertTrue(searchObjList(objList, _objId_DOC_B_1));
			assertTrue(searchObjList(objList, _objId_DOC_B_1_OTHER));
			assertTrue(searchObjList(objList, _objId_FOL_B_1));
			assertTrue(searchObjList(objList, _objId_FOL_B_1_OTHER));
			assertTrue(searchObjList(objList, _objId_FOL_B_2));
			assertTrue(searchObjList(objList, _objId_FOL_B_3));
			assertTrue(searchObjList(objList, _objId_FOL_B_4));
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {

			if (sess != null) {
				sess.rollback();
				sess.close();
			}
		}	
	}
	
	
	
	/**
	 * TagUtil.getTagGivenTagObj(EIMSession, EIMObject) のためのテスト・メソッド
	 * 
	 * @throws Exception
	 */
	public void testGetTagGivenTagObj() throws Exception {
		
		if (!isPreparationOK) return;
		
		EIMSession sess = null;
		
		/**
		 * 正常(1) タグ配下のタグが存在する
		 * 
		 */
		try {
			sess = TestSessionUtil.createEIMSession();
			
			// テストデータ準備
			createTestData1(sess);
			createTestData2(sess);
			
			//[1回目]
			// タグ付与対象
			int[] targetIds = {
					_objId_FOL_1,
					_objId_FOL_B_4
					};
			
			// 付与するタグ
			int tagObjId = _objId_TAG_2;
			
			// タグ付与
			setTag(sess, tagObjId, targetIds);
			
			//[2回目]
			// タグ付与対象
			int[] targetIds2 = {
					_objId_FOL_B_1,
					_objId_FOL_B_4
					};
			
			// 付与するタグ
			int tagObjId2 = _objId_TAG_1;
			
			// タグ付与
			setTag(sess, tagObjId2, targetIds2);
			
			//[3回目]
			// タグ付与対象
			int[] targetIds3 = {
					_objId_TAG_B_1,
					_objId_TAG_B_2
					};
			
			// 付与するタグ
			int tagObjId3 = _objId_TAG_B_3;
			
			// タグ付与
			setTag(sess, tagObjId3, targetIds3);
			
			//[4回目]
			// タグ付与対象
			int[] targetIds4 = {
					_objId_FOL_6
					};
			
			// 付与するタグ
			int tagObjId4 = _objId_TAG_B_2;
			
			// タグ付与
			setTag(sess, tagObjId4, targetIds4);

			//[5回目]
			// タグ付与対象
			int[] targetIds5 = {
					_objId_TAG_B_2
					};
			
			// 付与するタグ
			int tagObjId5 = _objId_TAG_B_1;
			
			// タグ付与
			setTag(sess, tagObjId5, targetIds5);

			
			EIMObject wsObjA = ObjectUtils.getObjectById(sess, _objId_WS_A);
			EIMObject wsObjB = ObjectUtils.getObjectById(sess, _objId_WS_B);
			EIMObject tagObj = ObjectUtils.getObjectById(sess, _objId_TAG_1);
			
			// 事前確認
			viewFolderTree(sess, wsObjA, "正常(1)");
			viewFolderTree(sess, wsObjB, "正常(1)");
			
			// メソッド実行
			List objList = TagUtil.getTagGivenTagObj(sess, tagObj);
			
			// 結果確認
			viewList(objList, "正常(1)");
			
			// 正誤判定 (ツリー上の存在すべき階層にあるか)
			assertTrue(searchObjList(objList, _objId_TAG_B_1));
			assertTrue(searchObjList(objList, _objId_TAG_B_2));
			assertTrue(searchObjList(objList, _objId_TAG_B_3));
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {

			if (sess != null) {
				sess.rollback();
				sess.close();
			}
		}	
	}


	
	/**
	 * TagUtil.isDupObjNameUnderTag(EIMSession, EIMObject) のためのテスト・メソッド
	 * 
	 * @throws Exception
	 */
	public void testIsDupObjNameUnderTag() throws Exception {
		
		if (!isPreparationOK) return;

		EIMSession sess = null;
		
		boolean isExceptionExist = false;
	
		/**
		 * 異常(1) 名称重複(タグとタグ)
		 * 
		 */
		try {
			isExceptionExist = false;

			sess = TestSessionUtil.createEIMSession();
			
			// テストデータ準備
			createTestData1(sess);
			createTestData2(sess);
			
			//[1回目]
			// タグ付与対象
			int[] targetIds = {
					_objId_TAG_1,
					_objId_TAG_B_1
					};
			
			// 付与するタグ
			int tagObjId = _objId_TAG_2;
			
			// タグ付与
			setTag(sess, tagObjId, targetIds);
			
			EIMObject wsObjA = ObjectUtils.getObjectById(sess, _objId_WS_A);
			EIMObject wsObjB = ObjectUtils.getObjectById(sess, _objId_WS_B);
			EIMObject tagObj = ObjectUtils.getObjectById(sess, tagObjId);
			
			// 事前確認
			viewFolderTree(sess, wsObjA, "異常(1)");
			viewFolderTree(sess, wsObjB, "異常(1)");
			
			// メソッド実行
			TagUtil.isDupObjNameUnderTag(sess, tagObj);
			
		} catch (EIMException eime) {

			isExceptionExist = true;
			if (_viewTestData) eime.printStackTrace();
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {

			if (sess != null) {
				sess.rollback();
				sess.close();
			}
			// 例外発生の確認
			assertTrue(isExceptionExist);
		}
		
		
		/**
		 * 異常(2) 名称重複(フォルダとフォルダ)
		 * 
		 */
		try {
			isExceptionExist = false;

			sess = TestSessionUtil.createEIMSession();
			
			// テストデータ準備
			createTestData1(sess);
			createTestData2(sess);
			
			//[1回目]
			// タグ付与対象
			int[] targetIds = {
					_objId_FOL_1,
					_objId_FOL_B_1
					};
			
			// 付与するタグ
			int tagObjId = _objId_TAG_2;
			
			// タグ付与
			setTag(sess, tagObjId, targetIds);
			
			EIMObject wsObjA = ObjectUtils.getObjectById(sess, _objId_WS_A);
			EIMObject wsObjB = ObjectUtils.getObjectById(sess, _objId_WS_B);
			EIMObject tagObj = ObjectUtils.getObjectById(sess, tagObjId);
			
			// 事前確認
			viewFolderTree(sess, wsObjA, "異常(2)");
			viewFolderTree(sess, wsObjB, "異常(2)");
			
			// メソッド実行
			TagUtil.isDupObjNameUnderTag(sess, tagObj);
			
		} catch (EIMException eime) {

			isExceptionExist = true;
			if (_viewTestData) eime.printStackTrace();
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {

			if (sess != null) {
				sess.rollback();
				sess.close();
			}
			// 例外発生の確認
			assertTrue(isExceptionExist);
		}
		
		
		/**
		 * 異常(3) 名称重複(ドキュメントとドキュメント)
		 * 
		 */
		try {
			isExceptionExist = false;

			sess = TestSessionUtil.createEIMSession();
			
			// テストデータ準備
			createTestData1(sess);
			createTestData2(sess);
			
			//[1回目]
			// タグ付与対象
			int[] targetIds = {
					_objId_DOC_1,
					_objId_DOC_B_1
					};
			
			// 付与するタグ
			int tagObjId = _objId_TAG_2;
			
			// タグ付与
			setTag(sess, tagObjId, targetIds);
			
			EIMObject wsObjA = ObjectUtils.getObjectById(sess, _objId_WS_A);
			EIMObject wsObjB = ObjectUtils.getObjectById(sess, _objId_WS_B);
			EIMObject tagObj = ObjectUtils.getObjectById(sess, tagObjId);
			
			// 事前確認
			viewFolderTree(sess, wsObjA, "異常(3)");
			viewFolderTree(sess, wsObjB, "異常(3)");
			
			// メソッド実行
			TagUtil.isDupObjNameUnderTag(sess, tagObj);
			
		} catch (EIMException eime) {

			isExceptionExist = true;
			if (_viewTestData) eime.printStackTrace();
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {

			if (sess != null) {
				sess.rollback();
				sess.close();
			}
			// 例外発生の確認
			assertTrue(isExceptionExist);
		}
		
		
		/**
		 * 異常(4) 名称重複(フォルダとタグ)
		 * 
		 */
		try {
			isExceptionExist = false;

			sess = TestSessionUtil.createEIMSession();
			
			// テストデータ準備
			createTestData1(sess);
			createTestData2(sess);
			
			//[1回目]
			// タグ付与対象
			int[] targetIds = {
					_objId_FOL_6,
					_objId_TAG_B_3
					};
			
			// 付与するタグ
			int tagObjId = _objId_TAG_2;
			
			// タグ付与
			setTag(sess, tagObjId, targetIds);
			
			EIMObject wsObjA = ObjectUtils.getObjectById(sess, _objId_WS_A);
			EIMObject wsObjB = ObjectUtils.getObjectById(sess, _objId_WS_B);
			EIMObject tagObj = ObjectUtils.getObjectById(sess, tagObjId);

			EIMObject fol6Obj = ObjectUtils.getObjectById(sess, _objId_FOL_6);
			EIMObject tagB3Obj = ObjectUtils.getObjectById(sess, _objId_TAG_B_3);

			
			// 重複させるために改名
			ObjectUtils.rename(sess, fol6Obj, tagB3Obj.getName());
			
			// 事前確認
			viewFolderTree(sess, wsObjA, "異常(4)");
			viewFolderTree(sess, wsObjB, "異常(4)");
			
			// メソッド実行
			TagUtil.isDupObjNameUnderTag(sess, tagObj);
			
		} catch (EIMException eime) {

			isExceptionExist = true;
			if (_viewTestData) eime.printStackTrace();
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {

			if (sess != null) {
				sess.rollback();
				sess.close();
			}
			// 例外発生の確認
			assertTrue(isExceptionExist);
		}


		/**
		 * 異常(5) 名称重複(フォルダとドキュメント)
		 * 
		 */
		try {
			isExceptionExist = false;

			sess = TestSessionUtil.createEIMSession();
			
			// テストデータ準備
			createTestData1(sess);
			
			//[1回目]
			// タグ付与対象
			int[] targetIds = {
					_objId_FOL_3,
					_objId_DOC_4
					};
			
			// 付与するタグ
			int tagObjId = _objId_TAG_2;
			
			// タグ付与
			setTag(sess, tagObjId, targetIds);
			
			EIMObject wsObjA = ObjectUtils.getObjectById(sess, _objId_WS_A);
			EIMObject tagObj = ObjectUtils.getObjectById(sess, tagObjId);

			EIMObject fol3Obj = ObjectUtils.getObjectById(sess, _objId_FOL_3);
			EIMObject doc4Obj = ObjectUtils.getObjectById(sess, _objId_DOC_4);

			
			// 重複させるために改名
			ObjectUtils.rename(sess, fol3Obj, doc4Obj.getName());
			
			// 事前確認
			viewFolderTree(sess, wsObjA, "異常(5)");
			
			// メソッド実行
			TagUtil.isDupObjNameUnderTag(sess, tagObj);
			
		} catch (EIMException eime) {

			isExceptionExist = true;
			if (_viewTestData) eime.printStackTrace();
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {

			if (sess != null) {
				sess.rollback();
				sess.close();
			}
			// 例外発生の確認
			assertTrue(isExceptionExist);
		}
		
		
		/**
		 * 異常(6) 名称重複(タグとドキュメント)
		 * 
		 */
		try {
			isExceptionExist = false;

			sess = TestSessionUtil.createEIMSession();
			
			// テストデータ準備
			createTestData1(sess);
			
			//[1回目]
			// タグ付与対象
			int[] targetIds = {
					_objId_TAG_1,
					_objId_DOC_4
					};
			
			// 付与するタグ
			int tagObjId = _objId_TAG_2;
			
			// タグ付与
			setTag(sess, tagObjId, targetIds);
			
			EIMObject wsObjA = ObjectUtils.getObjectById(sess, _objId_WS_A);
			EIMObject tagObj = ObjectUtils.getObjectById(sess, tagObjId);

			EIMObject tag1Obj = ObjectUtils.getObjectById(sess, _objId_TAG_1);
			EIMObject doc4Obj = ObjectUtils.getObjectById(sess, _objId_DOC_4);

			
			// 重複させるために改名
			ObjectUtils.rename(sess, tag1Obj, doc4Obj.getName());
			
			// 事前確認
			viewFolderTree(sess, wsObjA, "異常(6)");
			
			// メソッド実行
			TagUtil.isDupObjNameUnderTag(sess, tagObj);
			
		} catch (EIMException eime) {

			isExceptionExist = true;
			if (_viewTestData) eime.printStackTrace();
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {

			if (sess != null) {
				sess.rollback();
				sess.close();
			}
			// 例外発生の確認
			assertTrue(isExceptionExist);
		}
	}
	
	
	
	/**
	 * テストデータ作成(1)
	 * 
	 * @param sess EIMSessionインスタンス
	 * @throws Exception
	 */
	private void createTestData1(EIMSession sess) throws Exception {
		
		/* [構成]
		 * 
		 * ワークスペースA
		 *   ├フォルダ1
		 *   │  ├フォルダ2
		 *   │  │  ├フォルダ3
		 *   │  │  │  └ドキュメント1
		 *   │  │  ├フォルダ4
		 *   │  │  │  └ドキュメント2
		 *   │  │  └タグ1
		 *   │  └フォルダ5
		 *   │      └ドキュメント3
		 *   ├フォルダ6
		 *   │  └ドキュメント4
		 *   ├タグ2
		 *   ├ドキュメント5
		 *   └ドキュメント6
		 */
		
		// ワークスペース作成
		EIMObject wsObjA = ObjectUtils.createObject(sess, _objTypeWS, "ワークスペースA", EIMConstant.DEPU_CHECK_TYPE_NAME);
		_objId_WS_A = wsObjA.getId();
		wsObjA = refresh(sess, wsObjA);
		
		// フォルダ作成
		EIMObject folObj1 = ObjectUtils.createObject(sess, _objTypeFOL, "フォルダ1");
		_objId_FOL_1 = folObj1.getId();
		RelationUtils.createRelation(sess, _relType, wsObjA, folObj1, EIMConstant.DEPU_CHECK_NAME);
		AppObjectUtil.setPath(sess, folObj1, "/" + wsObjA.getName() + "/");
		folObj1 = refresh(sess, folObj1);

		EIMObject folObj2 = ObjectUtils.createObject(sess, _objTypeFOL, "フォルダ2");
		_objId_FOL_2 = folObj2.getId();
		RelationUtils.createRelation(sess, _relType, folObj1, folObj2, EIMConstant.DEPU_CHECK_NAME);
		AppObjectUtil.setPath(sess, folObj2, AppObjectUtil.getPath(folObj1) + folObj1.getName() + "/");
		folObj2 = refresh(sess, folObj2);
		
		EIMObject folObj3 = ObjectUtils.createObject(sess, _objTypeFOL, "フォルダ3");
		_objId_FOL_3 = folObj3.getId();
		RelationUtils.createRelation(sess, _relType, folObj2, folObj3, EIMConstant.DEPU_CHECK_NAME);
		AppObjectUtil.setPath(sess, folObj3, AppObjectUtil.getPath(folObj2) + folObj2.getName() + "/");
		folObj3 = refresh(sess, folObj3);

		EIMObject folObj4 = ObjectUtils.createObject(sess, _objTypeFOL, "フォルダ4");
		_objId_FOL_4 = folObj4.getId();
		RelationUtils.createRelation(sess, _relType, folObj2, folObj4, EIMConstant.DEPU_CHECK_NAME);
		AppObjectUtil.setPath(sess, folObj4, AppObjectUtil.getPath(folObj2) + folObj2.getName() + "/");
		folObj4 = refresh(sess, folObj4);

		EIMObject folObj5 = ObjectUtils.createObject(sess, _objTypeFOL, "フォルダ5");
		_objId_FOL_5 = folObj5.getId();
		RelationUtils.createRelation(sess, _relType, folObj1, folObj5, EIMConstant.DEPU_CHECK_NAME);
		AppObjectUtil.setPath(sess, folObj5, AppObjectUtil.getPath(folObj1) + folObj1.getName() + "/");
		folObj5 = refresh(sess, folObj5);

		EIMObject folObj6 = ObjectUtils.createObject(sess, _objTypeFOL, "フォルダ6");
		_objId_FOL_6 = folObj6.getId();
		RelationUtils.createRelation(sess, _relType, wsObjA, folObj6, EIMConstant.DEPU_CHECK_NAME);
		AppObjectUtil.setPath(sess, folObj6, "/" + wsObjA.getName() + "/");
		folObj6 = refresh(sess, folObj6);

		// ドキュメント作成
		EIMObject docObj1 = ObjectUtils.createObject(sess, _objTypeDOC, "ドキュメント1.doc");
		_objId_DOC_1 = docObj1.getId();
		RelationUtils.createRelation(sess, _relType, folObj3, docObj1, EIMConstant.DEPU_CHECK_NAME);
		AppObjectUtil.setPath(sess, docObj1, AppObjectUtil.getPath(folObj3) + folObj3.getName() + "/");
		
		EIMObject docObj2 = ObjectUtils.createObject(sess, _objTypeDOC, "ドキュメント2.doc");
		_objId_DOC_2 = docObj2.getId();
		RelationUtils.createRelation(sess, _relType, folObj4, docObj2, EIMConstant.DEPU_CHECK_NAME);
		AppObjectUtil.setPath(sess, docObj2, AppObjectUtil.getPath(folObj4) + folObj4.getName() + "/");
		
		EIMObject docObj3 = ObjectUtils.createObject(sess, _objTypeDOC, "ドキュメント3.doc");
		_objId_DOC_3 = docObj3.getId();
		RelationUtils.createRelation(sess, _relType, folObj5, docObj3, EIMConstant.DEPU_CHECK_NAME);
		AppObjectUtil.setPath(sess, docObj3, AppObjectUtil.getPath(folObj5) + folObj5.getName() + "/");
		
		EIMObject docObj4 = ObjectUtils.createObject(sess, _objTypeDOC, "ドキュメント4.doc");
		_objId_DOC_4 = docObj4.getId();
		RelationUtils.createRelation(sess, _relType, folObj6, docObj4, EIMConstant.DEPU_CHECK_NAME);
		AppObjectUtil.setPath(sess, docObj4, AppObjectUtil.getPath(folObj6) + folObj6.getName() + "/");
		
		EIMObject docObj5 = ObjectUtils.createObject(sess, _objTypeFOL, "ドキュメント5.doc");
		_objId_DOC_5 = docObj5.getId();
		RelationUtils.createRelation(sess, _relType, wsObjA, docObj5, EIMConstant.DEPU_CHECK_NAME);
		AppObjectUtil.setPath(sess, docObj5, "/" + wsObjA.getName() + "/");

		EIMObject docObj6 = ObjectUtils.createObject(sess, _objTypeFOL, "ドキュメント6.doc");
		_objId_DOC_6 = docObj6.getId();
		RelationUtils.createRelation(sess, _relType, wsObjA, docObj6, EIMConstant.DEPU_CHECK_NAME);
		AppObjectUtil.setPath(sess, docObj6, "/" + wsObjA.getName() + "/");
		
		// セキュリティ設定
		int[] objIds = {
				_objId_WS_A,
				_objId_FOL_1,
				_objId_FOL_2,
				_objId_FOL_3,
				_objId_FOL_4,
				_objId_FOL_5,
				_objId_FOL_6,
				_objId_DOC_1,
				_objId_DOC_2,
				_objId_DOC_3,
				_objId_DOC_4,
				_objId_DOC_5,
				_objId_DOC_6,
		};
		setSecurity(sess, _secA, objIds);

		// タグ作成
		EIMObject tagObj1 = TagUtil.createTag(sess, _objTypeTAG, folObj2, "タグ1", sess.getUser());
		_objId_TAG_1 = tagObj1.getId();

		EIMObject tagObj2 = TagUtil.createTag(sess, _objTypeTAG, wsObjA, "タグ2", sess.getUser());
		_objId_TAG_2 = tagObj2.getId();
	}
	
	/**
	 * テストデータ作成(2)
	 * 
	 * @param sess EIMSessionインスタンス
	 * @throws Exception
	 */
	private void createTestData2(EIMSession sess) throws Exception {
		
		/* [構成]
		 * 
		 * ワークスペースB
		 *   ├フォルダ1
		 *   │  └フォルダ2
		 *   │      └フォルダ3
		 *   │          ├フォルダ1
		 *   │          │  └ドキュメント1
		 *   │          └ドキュメント1
		 *   └フォルダ4
		 *       ├タグ1
		 *       ├タグ2
		 *       └タグ3
		 */
		
		// ワークスペース作成
		EIMObject wsObjB = ObjectUtils.createObject(sess, _objTypeWS, "ワークスペースB", EIMConstant.DEPU_CHECK_TYPE_NAME);
		_objId_WS_B = wsObjB.getId();
		wsObjB = refresh(sess, wsObjB);
		
		// フォルダ作成
		EIMObject folObj1 = ObjectUtils.createObject(sess, _objTypeFOL, "フォルダ1");
		_objId_FOL_B_1 = folObj1.getId();
		RelationUtils.createRelation(sess, _relType, wsObjB, folObj1, EIMConstant.DEPU_CHECK_NAME);
		AppObjectUtil.setPath(sess, folObj1, "/" + wsObjB.getName() + "/");
		folObj1 = refresh(sess, folObj1);

		EIMObject folObj2 = ObjectUtils.createObject(sess, _objTypeFOL, "フォルダ2");
		_objId_FOL_B_2 = folObj2.getId();
		RelationUtils.createRelation(sess, _relType, folObj1, folObj2, EIMConstant.DEPU_CHECK_NAME);
		AppObjectUtil.setPath(sess, folObj2, AppObjectUtil.getPath(folObj1) + folObj1.getName() + "/");
		folObj2 = refresh(sess, folObj2);
		
		EIMObject folObj3 = ObjectUtils.createObject(sess, _objTypeFOL, "フォルダ3");
		_objId_FOL_B_3 = folObj3.getId();
		RelationUtils.createRelation(sess, _relType, folObj2, folObj3, EIMConstant.DEPU_CHECK_NAME);
		AppObjectUtil.setPath(sess, folObj3, AppObjectUtil.getPath(folObj2) + folObj2.getName() + "/");
		folObj3 = refresh(sess, folObj3);

		EIMObject folObj1_Other = ObjectUtils.createObject(sess, _objTypeFOL, "フォルダ1");
		_objId_FOL_B_1_OTHER = folObj1_Other.getId();
		RelationUtils.createRelation(sess, _relType, folObj3, folObj1_Other, EIMConstant.DEPU_CHECK_NAME);
		AppObjectUtil.setPath(sess, folObj1_Other, AppObjectUtil.getPath(folObj3) + folObj3.getName() + "/");
		folObj1_Other = refresh(sess, folObj1_Other);

		EIMObject folObj4 = ObjectUtils.createObject(sess, _objTypeFOL, "フォルダ4");
		_objId_FOL_B_4 = folObj4.getId();
		RelationUtils.createRelation(sess, _relType, wsObjB, folObj4, EIMConstant.DEPU_CHECK_NAME);
		AppObjectUtil.setPath(sess, folObj4, "/" + wsObjB.getName() + "/");
		folObj4 = refresh(sess, folObj4);

		// ドキュメント作成
		EIMObject docObj1 = ObjectUtils.createObject(sess, _objTypeDOC, "ドキュメント1.doc");
		_objId_DOC_B_1 = docObj1.getId();
		RelationUtils.createRelation(sess, _relType, folObj1_Other, docObj1, EIMConstant.DEPU_CHECK_NAME);
		AppObjectUtil.setPath(sess, docObj1, AppObjectUtil.getPath(folObj1_Other) + folObj1_Other.getName() + "/");
		
		EIMObject docObj1_Other = ObjectUtils.createObject(sess, _objTypeDOC, "ドキュメント1.doc");
		_objId_DOC_B_1_OTHER = docObj1_Other.getId();
		RelationUtils.createRelation(sess, _relType, folObj3, docObj1_Other, EIMConstant.DEPU_CHECK_NAME);
		AppObjectUtil.setPath(sess, docObj1_Other, AppObjectUtil.getPath(folObj3) + folObj3.getName() + "/");
		
		// セキュリティ設定
		int[] objIds = {
				_objId_WS_B,
				_objId_FOL_B_1,
				_objId_FOL_B_2,
				_objId_FOL_B_3,
				_objId_FOL_B_1_OTHER,
				_objId_FOL_B_4,
				_objId_DOC_B_1,
				_objId_DOC_B_1_OTHER,
		};
		setSecurity(sess, _secA, objIds);

		// タグ作成
		EIMObject tagObj1 = TagUtil.createTag(sess, _objTypeTAG, folObj4, "タグ1", sess.getUser());
		_objId_TAG_B_1 = tagObj1.getId();
		
		EIMObject tagObj2 = TagUtil.createTag(sess, _objTypeTAG, folObj4, "タグ2", sess.getUser());
		_objId_TAG_B_2 = tagObj2.getId();

		EIMObject tagObj3 = TagUtil.createTag(sess, _objTypeTAG, folObj4, "タグ3", sess.getUser());
		_objId_TAG_B_3 = tagObj3.getId();
	}

	
	/**
	 * タグ付与
	 * 
	 * @param sess
	 * @param tagObjId
	 * @param targetIds
	 * @throws Exception
	 */
	private void setTag(EIMSession sess, int tagObjId, int[] targetIds) throws Exception {
		
		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

		// 属性タイプの冗長な取得回避用Mapを生成
		HashMap attrTypeMap = TagUtil.getTagAttrTypeMap(sess);

		EIMObject tagObj = ObjectUtils.getObjectById(sess, tagObjId);
		for (int i = 0 ; i < targetIds.length ; i++) {
			EIMObject obj = ObjectUtils.getObjectById(sess, targetIds[i]);
			// タグ付与メソッド呼び出し
			TagUtil.assignTag(sess, obj, tagObj, sess.getUser(), attrTypeMap, helper);
		}
	}

	/**
	 * (異常データを作成するために強制的に)タグ付与
	 * 
	 * <li>フォルダ配下には付与しない、あくまでも引数オブジェクトIDに該当するEIMObjectのみ対象
	 * 
	 * @param sess
	 * @param tagObjId
	 * @param targetIds
	 * @throws Exception
	 */
	private void setTagForce(EIMSession sess, int tagObjId, int[] targetIds) throws Exception {
		
		for (int i = 0 ; i < targetIds.length ; i++) {
			
			EIMObject targetObj = ObjectUtils.getObjectById(sess, targetIds[i]);

			//タグ付与対象のオブジェクトから「タグ」属性を取得
			int[] tagArray = AppObjectUtil.getIntAttrs(sess, targetObj, EIMConfig.get("ATTR_NAME_DOCUMENT_TAG"));

			int[] newTagArray = null;
			if(tagArray != null) {
				newTagArray = new int[tagArray.length + 1];
				System.arraycopy(tagArray, 0, newTagArray, 0, tagArray.length);
				newTagArray[tagArray.length] = tagObjId;
			}
			else {	// 新規登録
				newTagArray = new int[1];
				newTagArray[0] = tagObjId;
			}
			AppObjectUtil.setAttr(sess, targetObj, EIMConfig.get("ATTR_NAME_DOCUMENT_TAG"), newTagArray);
		}
	}
	
	/**
	 * タグ解除
	 * 
	 * @param sess
	 * @param objId
	 * @throws Exception
	 */
	private void cancelTag(EIMSession sess, int objId) throws Exception {
		
		EIMObject obj = ObjectUtils.getObjectById(sess, objId);
		AppObjectUtil.deleteAttribute(sess, obj, EIMConfig.get("ATTR_NAME_DOCUMENT_TAG"));
	}
	
	/**
	 * セキュリティ設定
	 * 
	 * @param sess
	 * @param sec
	 * @param objIds
	 * @throws Exception
	 */
	private void setSecurity(EIMSession sess, EIMSecurity sec, int[] objIds) throws Exception {
		
		for (int i = 0 ; i < objIds.length ; i++) {
			EIMObject obj = ObjectUtils.getObjectById(sess, objIds[i]);
			SecurityUtils.setSecurity(sess, obj, sec);
		}
	}
	
	
	/**
	 * タグツリーの指定の階層に指定のオブジェクトが存在するか否かを判定します。
	 * 
	 * @param tree 判定するタグツリー
	 * @param targetObjId 対象のオブジェクトID
	 * @param searchLvl 対象が存在すべき階層(1から)
	 * @return タグツリーの指定の階層に指定のオブジェクトが存在するか否か
	 */
	private boolean searchTagTreeRecurrently(TagTreeItem tree, int targetObjId, int searchLvl) {
		
		if (searchLvl == 0) {
			if (tree.getEimObject().getId() == targetObjId) {
				return true;
			}
		} else 	if (tree.getTreeItemList() != null) {
			for (Iterator iter = tree.getTreeItemList().iterator(); iter.hasNext();) {
				// 再起呼び出し
				if (searchTagTreeRecurrently((TagTreeItem) iter.next(), targetObjId, searchLvl-1)) {
					return true;
				}
			}
		}
		return false;
	}
	
	/**
	 * EIMObjectのリストに指定のオブジェクトIDのEIMObjectが存在するか否かを判定します。
	 * 
	 * @param objList 判定するEIMObjectのリスト
	 * @param searchId 指定のオブジェクトID
	 * @return 指定のオブジェクトIDのEIMObjectが存在するか否か
	 */
	private boolean searchObjList(List objList, int searchId) {
		
		for (Iterator iter = objList.iterator(); iter.hasNext();) {
			EIMObject obj = (EIMObject) iter.next();
			if (obj.getId() == searchId) {
				return true;
			}
		}
		return false;
	}
	
	private EIMObject refresh(EIMSession sess, EIMObject obj) throws Exception {
		
		return ObjectUtils.getObjectById(sess, obj.getId());
	}
	
	/**
	 * フォルダツリー表示
	 * 
	 * @param sess
	 * @param obj
	 * @throws Exception
	 */
	private void viewFolderTree(EIMSession sess, EIMObject obj, String message) throws Exception {
		
		if (!_viewTestData) return;
		
		System.out.println("[" + message + "]\n--------<<フォルダツリー START>>-------------");
		// 再起呼び出し
		viewFolderTreeRecurrently(sess, obj, 0);
		System.out.println("----------<<フォルダツリー END>>-------------");
	}
	
	/**
	 * (再帰的処理)フォルダツリー表示
	 * 
	 * @param sess
	 * @param obj
	 * @param level
	 * @throws Exception
	 */
	private void viewFolderTreeRecurrently(EIMSession sess, EIMObject obj, int level) throws Exception {
		
		if (obj == null) return;
		
		for (int i = 0 ; i < level ; i++) {
			System.out.print("  ");
		}
		System.out.print("[" + level + "]" + obj.getName() + " (" + obj.getId() + ")");
		
		int[] tags = AppObjectUtil.getIntAttrs(sess, obj, EIMConfig.get("ATTR_NAME_DOCUMENT_TAG"));
		if (tags != null) {
			System.out.print(" <---[TAG]");
			for (int i = 0 ; tags.length > i ; i++) {
				EIMObject tagObj = ObjectUtils.getObjectById(sess, tags[i]);
				System.out.print(tagObj.getName() + "(" + tagObj.getId() + "),");
			}
		}
//debug
//		System.out.print(" [PATH]" + AppObjectUtil.getPath(obj));
		System.out.println("");
		level++;
		
		List relList = RelationUtils.getChildRelationListByRelType(sess, obj, _relType);
		if (relList != null) {
			for (int i = 0; i < relList.size(); i++) {
				EIMRelation rel = (EIMRelation)relList.get(i);
				EIMObject childObj = rel.getChild();
				// 再起呼び出し
				viewFolderTreeRecurrently(sess, childObj, level);
			}
		}
	}

	/**
	 * タグツリー表示
	 * 
	 * @param sess
	 * @param item
	 * @throws Exception
	 */
	private void viewTagTree(TagTreeItem item, String message) {
		
		if (!_viewTestData) return;
		
		System.out.println("[" + message + "]\n--------<<タグツリー START>>-------------");
		// 再起呼び出し
		viewTagTreeRecurrently(item, 0);
		System.out.println("----------<<タグツリー END>>-------------");
	}
	
	/**
	 * (再帰的処理)タグツリー表示
	 * 
	 * @param item
	 * @param level
	 */
	private void viewTagTreeRecurrently(TagTreeItem item, int level) {
		
		for (int i = 0 ; i < level ; i++) {
			System.out.print("  ");
		}
		System.out.println("[" + level + "]" + item.getEimObject().getName() + " (" + item.getEimObject().getId() + ")");
		level++;
		if (item.getTreeItemList() != null) {
			for (int i = 0 ; item.getTreeItemList().size() > i ; i++) {
				// 再起呼び出し
				viewTagTreeRecurrently((TagTreeItem)item.getTreeItemList().get(i), level);
			}
		}
	}
	
	/**
	 * リスト表示
	 * 
	 * @param list
	 * @param message
	 */
	private void viewList(List list, String message) {

		if (!_viewTestData) return;
		
		System.out.println("[" + message + "]\n--------<<リスト START>>-------------");

		for (Iterator iter = list.iterator(); iter.hasNext();) {
			EIMObject obj = (EIMObject) iter.next();
			System.out.println(obj.getName() + "(" + obj.getId() + ")");
		}
		System.out.println("----------<<リスト END>>-------------");
	}
}