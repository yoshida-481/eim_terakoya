package common.util;

import java.util.HashMap;

import junit.framework.TestCase;

import common.bo.TagTreeItem;

import eim.bo.EIMAccessRole;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMRelationType;
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
 * SignUtilのテストクラス
 * 
 */
public class SignUtilTest extends TestCase {
	
	// ★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★
	// ★　　以下の項目はDB環境に合わせて事前に書き換えておく必要があります　　★
	// ★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★
	
	/* テスト前設定項目 ここから------------------------------------------ */

	/** テスト結果のコンソール出力有無 */
	private boolean _viewTestData = true;
	
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
	 * setTagSignFlagOn(EIMSession, EIMObject) のためのテスト・メソッド
	 * 
	 * @throws Exception
	 */
	public void testSetTagSignFlagOn() throws Exception {

		if (!isPreparationOK) return;
		
		EIMSession sess = null;
		
		/**
		 * 正常(1) 全てのドキュメントが署名・暗号化未実施
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
					_objId_FOL_B_3
					};
			
			// 付与するタグ
			int tagObjId3 = _objId_TAG_B_2;
			
			// タグ付与
			setTag(sess, tagObjId3, targetIds3);			
			
			// 署名・暗号化済みにするオブジェクト
			int[] signObjIds = {
			};
			
			// 「署名・暗号化済み」に更新
			setSignOn(sess, signObjIds);

			EIMObject tagObj = ObjectUtils.getObjectById(sess, tagObjId);
			
			// 事前確認
			TagTreeItem tree = TagUtil.getTagTreeWithChild(sess, tagObj);
			viewTagTree(sess, tree, "正常(1):事前確認");
			
			// メソッド実行
			SignUtil.setTagSignFlagOn(sess, tagObj);
			
			// 結果確認
			tree = TagUtil.getTagTreeWithChild(sess, refresh(sess, tagObj));
			viewTagTree(sess, tree, "正常(1):結果確認");
			
			tree = TagUtil.getOnlyTagTree(sess, refresh(sess, tagObj));
			viewTagTree(sess, tree, "正常(1):結果確認");
			
			// 正誤判定 (ツリー上の存在すべき階層にあるか)
			assertFalse(isSignEncrFile(sess, _objId_TAG_2));
			assertFalse(isSignEncrFile(sess, _objId_TAG_1));
			assertTrue(isSignEncrFile(sess, _objId_TAG_B_1));
			assertFalse(isSignEncrFile(sess, _objId_TAG_B_2));
			assertTrue(isSignEncrFile(sess, _objId_TAG_B_3));
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {

			if (sess != null) {
				sess.rollback();
				sess.close();
			}
		}
		
		
		/**
		 * 正常(2) ドキュメントは署名・暗号化未実施と実施済みのどちらもあり
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
					_objId_FOL_B_3
					};
			
			// 付与するタグ
			int tagObjId3 = _objId_TAG_B_2;
			
			// タグ付与
			setTag(sess, tagObjId3, targetIds3);			
			
			// 署名・暗号化済みにするオブジェクト
			int[] signObjIds = {
					_objId_DOC_1,
					_objId_DOC_2,
					_objId_DOC_B_1
			};
			
			// 「署名・暗号化済み」に更新
			setSignOn(sess, signObjIds);

			EIMObject tagObj = ObjectUtils.getObjectById(sess, tagObjId);
			
			// 事前確認
			TagTreeItem tree = TagUtil.getTagTreeWithChild(sess, tagObj);
			viewTagTree(sess, tree, "正常(2):事前確認");
			
			// メソッド実行
			SignUtil.setTagSignFlagOn(sess, tagObj);
			
			// 結果確認
			tree = TagUtil.getTagTreeWithChild(sess, refresh(sess, tagObj));
			viewTagTree(sess, tree, "正常(2):結果確認");
			
			tree = TagUtil.getOnlyTagTree(sess, refresh(sess, tagObj));
			viewTagTree(sess, tree, "正常(2):結果確認");
			
			// 正誤判定 (ツリー上の存在すべき階層にあるか)
			assertFalse(isSignEncrFile(sess, _objId_TAG_2));
			assertTrue(isSignEncrFile(sess, _objId_TAG_1));
			assertTrue(isSignEncrFile(sess, _objId_TAG_B_1));
			assertTrue(isSignEncrFile(sess, _objId_TAG_B_2));
			assertTrue(isSignEncrFile(sess, _objId_TAG_B_3));
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {

			if (sess != null) {
				sess.rollback();
				sess.close();
			}
		}
		
		
		/**
		 * 正常(3) ドキュメントは全て署名・暗号化済み
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
					_objId_FOL_B_3
					};
			
			// 付与するタグ
			int tagObjId3 = _objId_TAG_B_2;
			
			// タグ付与
			setTag(sess, tagObjId3, targetIds3);			
			
			// 署名・暗号化済みにするオブジェクト
			int[] signObjIds = {
					_objId_DOC_1,
					_objId_DOC_2,
					_objId_DOC_3,
					_objId_DOC_B_1
			};
			
			// 「署名・暗号化済み」に更新
			setSignOn(sess, signObjIds);

			EIMObject tagObj = ObjectUtils.getObjectById(sess, tagObjId);
			
			// 事前確認
			TagTreeItem tree = TagUtil.getTagTreeWithChild(sess, tagObj);
			viewTagTree(sess, tree, "正常(3):事前確認");
			
			// メソッド実行
			SignUtil.setTagSignFlagOn(sess, tagObj);
			
			// 結果確認
			tree = TagUtil.getTagTreeWithChild(sess, refresh(sess, tagObj));
			viewTagTree(sess, tree, "正常(3):結果確認");
			
			tree = TagUtil.getOnlyTagTree(sess, refresh(sess, tagObj));
			viewTagTree(sess, tree, "正常(3):結果確認");
			
			// 正誤判定 (ツリー上の存在すべき階層にあるか)
			assertTrue(isSignEncrFile(sess, _objId_TAG_2));
			assertTrue(isSignEncrFile(sess, _objId_TAG_1));
			assertTrue(isSignEncrFile(sess, _objId_TAG_B_1));
			assertTrue(isSignEncrFile(sess, _objId_TAG_B_2));
			assertTrue(isSignEncrFile(sess, _objId_TAG_B_3));
			
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
	 * setParentTagSignFlagOff(EIMSession, EIMObject) のためのテスト・メソッド
	 * 
	 * @throws Exception
	 */
	public void testSetParentTagSignFlagOff() throws Exception {

		if (!isPreparationOK) return;
		
		EIMSession sess = null;
		
		/**
		 * 正常(1) ドキュメントは全て署名・暗号化済み
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
					_objId_FOL_B_3
					};
			
			// 付与するタグ
			int tagObjId3 = _objId_TAG_B_2;
			
			// タグ付与
			setTag(sess, tagObjId3, targetIds3);			
			
			//[4回目]
			// タグ付与対象
			int[] targetIds4 = {
					_objId_DOC_B_1
					};
			
			// 付与するタグ
			int tagObjId4 = _objId_TAG_B_3;
			
			// タグ付与
			setTag(sess, tagObjId4, targetIds4);			
			
			// 署名・暗号化済みにするオブジェクト
			int[] signObjIds = {
					_objId_DOC_1,
					_objId_DOC_2,
					_objId_DOC_3,
					_objId_DOC_B_1
			};
			
			// 「署名・暗号化済み」に更新
			setSignOn(sess, signObjIds);

			EIMObject tagObj = ObjectUtils.getObjectById(sess, tagObjId);
			
			// 署名・暗号化実施
			SignUtil.setTagSignFlagOn(sess, tagObj);
			
			// 事前確認
			TagTreeItem tree = TagUtil.getTagTreeWithChild(sess, tagObj);
			viewTagTree(sess, tree, "正常(1):事前確認");
			
			tree = TagUtil.getOnlyTagTree(sess, refresh(sess, tagObj));
			viewTagTree(sess, tree, "正常(1):事前確認");
			
			// 署名・暗号化オフにするオブジェクト
			int[] signObjIds2 = {
					_objId_DOC_B_1
			};
			
			// 「署名・暗号化していない」に更新
			setSignOff(sess, signObjIds2);
			
			// メソッド実行
			SignUtil.setParentTagSignFlagOff(sess, ObjectUtils.getObjectById(sess, _objId_DOC_B_1));
			
			// 結果確認
			tree = TagUtil.getTagTreeWithChild(sess, refresh(sess, tagObj));
			viewTagTree(sess, tree, "正常(1):結果確認");
			
			tree = TagUtil.getOnlyTagTree(sess, refresh(sess, tagObj));
			viewTagTree(sess, tree, "正常(1):結果確認");
			
			// 正誤判定 (ツリー上の存在すべき階層にあるか)
			assertFalse(isSignEncrFile(sess, _objId_TAG_2));
			assertFalse(isSignEncrFile(sess, _objId_TAG_1));
			assertTrue(isSignEncrFile(sess, _objId_TAG_B_1));
			assertFalse(isSignEncrFile(sess, _objId_TAG_B_2));
			assertFalse(isSignEncrFile(sess, _objId_TAG_B_3));
			
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
		EIMObject docObj1 = ObjectUtils.createObject(sess, _objTypeDOC, "ドキュメント1.zip");
		_objId_DOC_1 = docObj1.getId();
		RelationUtils.createRelation(sess, _relType, folObj3, docObj1, EIMConstant.DEPU_CHECK_NAME);
		AppObjectUtil.setPath(sess, docObj1, AppObjectUtil.getPath(folObj3) + folObj3.getName() + "/");
		
		EIMObject docObj2 = ObjectUtils.createObject(sess, _objTypeDOC, "ドキュメント2.zip");
		_objId_DOC_2 = docObj2.getId();
		RelationUtils.createRelation(sess, _relType, folObj4, docObj2, EIMConstant.DEPU_CHECK_NAME);
		AppObjectUtil.setPath(sess, docObj2, AppObjectUtil.getPath(folObj4) + folObj4.getName() + "/");
		
		EIMObject docObj3 = ObjectUtils.createObject(sess, _objTypeDOC, "ドキュメント3.zip");
		_objId_DOC_3 = docObj3.getId();
		RelationUtils.createRelation(sess, _relType, folObj5, docObj3, EIMConstant.DEPU_CHECK_NAME);
		AppObjectUtil.setPath(sess, docObj3, AppObjectUtil.getPath(folObj5) + folObj5.getName() + "/");
		
		EIMObject docObj4 = ObjectUtils.createObject(sess, _objTypeDOC, "ドキュメント4.zip");
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
		 *   ├フォルダB1
		 *   │  └フォルダB2
		 *   │      └フォルダB3
		 *   │          ├フォルダB1_2
		 *   │          │  └ドキュメントB1
		 *   │          └ドキュメントB1_2
		 *   └フォルダB4
		 *       ├タグB1
		 *       ├タグB2
		 *       └タグB3
		 */
		
		// ワークスペース作成
		EIMObject wsObjB = ObjectUtils.createObject(sess, _objTypeWS, "ワークスペースB", EIMConstant.DEPU_CHECK_TYPE_NAME);
		_objId_WS_B = wsObjB.getId();
		wsObjB = refresh(sess, wsObjB);
		
		// フォルダ作成
		EIMObject folObj1 = ObjectUtils.createObject(sess, _objTypeFOL, "フォルダB1");
		_objId_FOL_B_1 = folObj1.getId();
		RelationUtils.createRelation(sess, _relType, wsObjB, folObj1, EIMConstant.DEPU_CHECK_NAME);
		AppObjectUtil.setPath(sess, folObj1, "/" + wsObjB.getName() + "/");
		folObj1 = refresh(sess, folObj1);

		EIMObject folObj2 = ObjectUtils.createObject(sess, _objTypeFOL, "フォルダB2");
		_objId_FOL_B_2 = folObj2.getId();
		RelationUtils.createRelation(sess, _relType, folObj1, folObj2, EIMConstant.DEPU_CHECK_NAME);
		AppObjectUtil.setPath(sess, folObj2, AppObjectUtil.getPath(folObj1) + folObj1.getName() + "/");
		folObj2 = refresh(sess, folObj2);
		
		EIMObject folObj3 = ObjectUtils.createObject(sess, _objTypeFOL, "フォルダB3");
		_objId_FOL_B_3 = folObj3.getId();
		RelationUtils.createRelation(sess, _relType, folObj2, folObj3, EIMConstant.DEPU_CHECK_NAME);
		AppObjectUtil.setPath(sess, folObj3, AppObjectUtil.getPath(folObj2) + folObj2.getName() + "/");
		folObj3 = refresh(sess, folObj3);

		EIMObject folObj1_Other = ObjectUtils.createObject(sess, _objTypeFOL, "フォルダB1_2");
		_objId_FOL_B_1_OTHER = folObj1_Other.getId();
		RelationUtils.createRelation(sess, _relType, folObj3, folObj1_Other, EIMConstant.DEPU_CHECK_NAME);
		AppObjectUtil.setPath(sess, folObj1_Other, AppObjectUtil.getPath(folObj3) + folObj3.getName() + "/");
		folObj1_Other = refresh(sess, folObj1_Other);

		EIMObject folObj4 = ObjectUtils.createObject(sess, _objTypeFOL, "フォルダB4");
		_objId_FOL_B_4 = folObj4.getId();
		RelationUtils.createRelation(sess, _relType, wsObjB, folObj4, EIMConstant.DEPU_CHECK_NAME);
		AppObjectUtil.setPath(sess, folObj4, "/" + wsObjB.getName() + "/");
		folObj4 = refresh(sess, folObj4);

		// ドキュメント作成
		EIMObject docObj1 = ObjectUtils.createObject(sess, _objTypeDOC, "ドキュメントB1.zip");
		_objId_DOC_B_1 = docObj1.getId();
		RelationUtils.createRelation(sess, _relType, folObj1_Other, docObj1, EIMConstant.DEPU_CHECK_NAME);
		AppObjectUtil.setPath(sess, docObj1, AppObjectUtil.getPath(folObj1_Other) + folObj1_Other.getName() + "/");
		
		EIMObject docObj1_Other = ObjectUtils.createObject(sess, _objTypeDOC, "ドキュメントB1_2.doc");
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
		EIMObject tagObj1 = TagUtil.createTag(sess, _objTypeTAG, folObj4, "タグB1", sess.getUser());
		_objId_TAG_B_1 = tagObj1.getId();
		
		EIMObject tagObj2 = TagUtil.createTag(sess, _objTypeTAG, folObj4, "タグB2", sess.getUser());
		_objId_TAG_B_2 = tagObj2.getId();

		EIMObject tagObj3 = TagUtil.createTag(sess, _objTypeTAG, folObj4, "タグB3", sess.getUser());
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
	
	private EIMObject refresh(EIMSession sess, EIMObject obj) throws Exception {
		
		return ObjectUtils.getObjectById(sess, obj.getId());
	}
	
	/**
	 * 引数オブジェクトの署名・暗号化状態を「署名・暗号化済み」に更新します。
	 * 
	 * @param sess
	 * @param obj
	 * @throws Exception
	 */
	private void setSignOn(EIMSession sess, int[] objIds) throws Exception {
		
		for (int i = 0; i < objIds.length; i++) {
			EIMObject obj =ObjectUtils.getObjectById(sess, objIds[i]);
			AppObjectUtil.setAttr(sess, obj, EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_STATUS"), AppConstant.SIGNENCR_KIND_SIGNENCR);
		}
	}
	
	/**
	 * 引数オブジェクトの署名・暗号化状態を「署名・暗号化していない」に更新します。
	 * 
	 * @param sess
	 * @param obj
	 * @throws Exception
	 */
	private void setSignOff(EIMSession sess, int[] objIds) throws Exception {
		
		for (int i = 0; i < objIds.length; i++) {
			EIMObject obj =ObjectUtils.getObjectById(sess, objIds[i]);
			AppObjectUtil.setAttr(sess, obj, EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_STATUS"), AppConstant.SIGNENCR_KIND_NOT_SIGNENCR);
		}
	}
	
	/**
	 * 署名・暗号化状態の文字列を返却します。
	 * 
	 * @param sess
	 * @param obj
	 * @param helper
	 * @return 署名・暗号化状態の文字列
	 * @throws Exception
	 */
	private String getSignEncrStsStr(EIMSession sess, EIMObject obj, AppObjectConditionHelper helper) throws Exception {
		
		if (!helper.isTypeOfFolder(obj.getType())) {
			
			if (helper.isTypeOfDocument(obj.getType()) && !SignUtil.isSignEncrTarget(sess, obj)) {
				return ("対象外");
			}
		
			// 属性「署名・暗号化状態」を取得
			int signencr = AppObjectUtil.getIntAttr(sess, obj, EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_STATUS"),	 AppConstant.SIGNENCR_KIND_NOT_SIGNENCR);
			
			switch (signencr) {
			
			case AppConstant.SIGNENCR_KIND_NOT_SIGNENCR:
				return "0:未実施";
				
			case AppConstant.SIGNENCR_KIND_SIGNENCR:
				return "1:済み";
				
			case AppConstant.SIGNENCR_KIND_PROCESSING_SIGNENCR:
				return "2:処理中";
				
			case AppConstant.SIGNENCR_KIND_FAILED:
				return "3:失敗";
				
			default:
				return "";
			}
		}
		return "";
	}

	/**
	 * タグツリー表示
	 * @param sess
	 * @param item
	 * @param message
	 * @throws Exception 
	 * 
	 * @throws Exception
	 */
	private void viewTagTree(EIMSession sess, TagTreeItem item, String message) throws Exception {
		
		if (!_viewTestData) return;

		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

		System.out.println("[" + message + "]\n--------<<タグツリー START>>-------------");
		// 再起呼び出し
		viewTagTreeRecurrently(sess, item, 0, helper);
		System.out.println("----------<<タグツリー END>>-------------");
	}
	
	/**
	 * (再帰的処理)タグツリー表示
	 * @param sess
	 * @param item
	 * @param level
	 * @param helper
	 * 
	 * @throws Exception 
	 */
	private void viewTagTreeRecurrently(EIMSession sess, TagTreeItem item, int level, AppObjectConditionHelper helper) throws Exception {
		
		for (int i = 0 ; i < level ; i++) {
			System.out.print("  ");
		}
		System.out.println("[" + level + "]" + item.getEimObject().getName() + " (" + getSignEncrStsStr(sess, item.getEimObject(), helper) + ")");
		level++;
		if (item.getTreeItemList() != null) {
			for (int i = 0 ; item.getTreeItemList().size() > i ; i++) {
				// 再起呼び出し
				viewTagTreeRecurrently(sess, (TagTreeItem)item.getTreeItemList().get(i), level, helper);
			}
		}
	}
	
	/**
	 * 対象オブジェクトが署名・暗号化済みか否かを判定します。
	 * 
	 * @param sess
	 * @param objId
	 * @return 署名・暗号化済みか否か
	 * @throws Exception
	 */
	private boolean isSignEncrFile(EIMSession sess, int objId) throws Exception {
		
		EIMObject obj = ObjectUtils.getObjectById(sess, objId);
		
		// 属性「署名・暗号化状態」を取得
		int signencr = AppObjectUtil.getIntAttr(sess, obj, EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_STATUS"),	 AppConstant.SIGNENCR_KIND_NOT_SIGNENCR);
		
		// 「署名・暗号化済み（処理成功）」の場合にのみtrueを返却
		if (signencr == AppConstant.SIGNENCR_KIND_SIGNENCR) {
			return true;
		} else {
			return false;
		}
	}
}