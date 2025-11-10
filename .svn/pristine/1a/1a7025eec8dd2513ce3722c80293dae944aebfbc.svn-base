package common.util;

import java.lang.reflect.Method;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import common.bo.AttributeTree;
import common.bo.AttributeTreeItem;
import common.util.AppObjectUtil;

import eim.bo.EIMAccessRole;
import eim.bo.EIMAttributeType;
import eim.bo.EIMException;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMOtherName;
import eim.bo.EIMRelationType;
import eim.bo.EIMUser;
import eim.bo.EIMValueType;
import eim.net.EIMSession;
import eim.util.AttributeUtils;
import eim.util.EIMConfig;
import eim.util.EIMConstant;
import eim.util.ObjectAttributeUtils;
import eim.util.ObjectUtils;
import eim.util.RelationUtils;
import eim.util.SecurityUtils;
import eim.util.UserUtils;
import eim.util.WorkFlowUtils;
import junit.framework.TestCase;

/**
 * AttributeTreeUtilのテストクラス
 * @author Jniya.Noguchi
 */
public class AttributeTreeUtilTest extends TestCase {

	/* ※※※ 以下はDB変更の際には変更する必要あり ※※※ */
	// 事前にDB上に以下のようなユーザ、ワークフローなどを作成/設定し、
	// 変数群にそれぞれのIDを設定する必要がある
	/* 画面側データ作成手順
	 * ①読み取り権限ユーザを作成する（ユーザ１）
	 * ②別のユーザを作成（ユーザ２）
	 * ③権限なしユーザ作成（ユーザ３）
	 * ④カスタム属性作成（文字列型）
	 * ⑤セキュリティ１作成（ユーザ１を読み取り権限のみで設定）
	 * ⑥セキュリティ２作成（ユーザ２を読み取り権限のみで設定）
	 * ⑧ステータスが編集中、承認依頼中、公開処理中、公開済の
	 * 　ワークフロー１を作成
	 * ⑨フォルダタイプ１作成
	 * 　（ワークフロー１、カスタム属性、セキュリティ１を割り当てる）
	 * ⑩フォルダタイプ２作成
	 * 　（カスタム属性、セキュリティ１を割り当てる※ワークフローなし）
	 * ⑪フォルダタイプ３作成
	 * 　（ワークフロー１、カスタム属性、セキュリティ２を割り当てる）
	 * ⑫フォルダタイプ１作成
	 * 　（ワークフロー、カスタム属性、セキュリティ１を割り当てる）
	 * ⑬フォルダタイプ２作成
	 * 　（カスタム属性、セキュリティ１を割り当てる※ワークフローなし）
	 * ⑭フォルダタイプ３作成
	 * 　（ワークフロー、カスタム属性、セキュリティ２を割り当てる）
	 * ⑮ドキュメントタイプ１でドキュメント作成後、公開済にする
	 * ⑯ドキュメントタイプ２でドキュメント作成
	 * ⑰ドキュメントタイプ３でドキュメント作成後、公開済にする
	 * ⑱上記カスタム属性を設定していないドキュメントタイプ
	 * 　（セキュリティ１、ワークフロー１を設定した）でドキュメントを
	 * 　作成後、公開済にする
	 * ⑲上記カスタム属性を設定していないドキュメントタイプ
	 * 　（セキュリティ１を設定した）でドキュメントを
	 * 　作成後、公開済にする
	 */
	// 読み取りユーザ１のID
	private final int READ_ONLY_USER_ID1= 0;
	// 権限なしユーザ
	private final int NOT_READ_ONLY_USER_ID= 0;
	
	// 属性タイプID
	private final int TARGET_ATTR_TYPE_ID = 0;
	private final String TARGET_ATTR_TYPE_VALUE = null;

	// フォルダタイプID
	private final int TARGET_FOLDER_TYPE_ID1 = 0;
	private final int TARGET_FOLDER_TYPE_ID2 = 0;
	private final int TARGET_FOLDER_TYPE_ID3 = 0;
	// ドキュメントタイプID
	private final int TARGET_DOCUMENT_TYPE_ID1 = 0;
	private final int TARGET_DOCUMENT_TYPE_ID2 = 0;
	private final int TARGET_DOCUMENT_TYPE_ID3 = 0;
	
	// ドキュメントID
	// 上記⑮のドキュメント
	private final int TARGET_DOCUMENT_OBJ_ID1 = 0;
	// typeが「公開ドキュメント」
	// 上記⑯のドキュメント
	private final int TARGET_DOCUMENT_OBJ_ID2 = 0;
	// 上記⑰のドキュメント
	private final int TARGET_DOCUMENT_OBJ_ID3 = 0;
	/* ※※※※※※※※※※※※※※※※※※※※※※※※※ */ 
	

	// 重複名称防止に使用
	private static Date date = new Date();
	
	/* -------- 属性ツリー名称 ----------- */	
	private static final String ATT_TREE_NAME1 = "製品名１_" + date;
	private static final String ATT_TREE_NAME2 = "製品名２_" + date;
	private static final String ATT_TREE_NAME3 = "製品名３_" + date;	
	private static final String ATT_TREE_NAME4 = "製品名４_" + date;	
	private static final String ATT_TREE_NAME5 = "製品名５_" + date;	
	private static final String ATT_TREE_NAME6 = "製品名６_" + date;	
	private static final String ATT_TREE_NAME7 = "製品名７_" + date;	
	private static final String ATT_TREE_NAME8 = "製品名８_" + date;	
	private static final String ATT_TREE_NAME9 = "製品名９_" + date;	
	private static final String ATT_TREE_NAME10 = "製品名１０_" + date;	
	private static final String ATT_TREE_NAME11 = "製品名１１_" + date;	
	private static final String ATT_TREE_NAME12 = "製品名１２_" + date;	
	private static final String ATT_TREE_NAME13 = "製品名１３_" + date;	
	private static final String ATT_TREE_NAME14 = "製品名１４_" + date;	
	private static final String ATT_TREE_NAME15 = "新製品情報_" + date;	
	private static final String ATT_TREE_NAME16 = "建物情報_" + date;	
	private static final String ATT_TREE_NAME17 = "製品情報A_" + date;	
	private static final String ATT_TREE_NAME18 = "製品情報B_" + date;	
	private static final String ATT_TREE_NAME19 = "製品情報C_" + date;	
	private static final String ATT_TREE_NAME20 = "製品情報D_" + date;	
	// 更新用
	private static final String attTreeReName = "製品名１(再提案)" + date;

	
	/* -------- 属性ツリー他言語ID ----------- */	
	private static final String langId1 = "en1";
	private static final String langId2 = "en2";
	private static final String langId3 = "en3";
	private static final String langId4 = "en4";
	private static final String langId5 = "en5";	
	private static final String langId6 = "en6";	
	private static final String langId7 = "en7";	
	private static final String langId8 = "en8";	
	private static final String langId9 = "en9";	
	// 存在するはずがないものとして使用
	private static final String NolangId = "no";
	
	
	/* -------- 属性ツリー他言語名称 ----------- */	
	private static final String otherName1 = "Product Name1";
	private static final String otherName2 = "Product Name2";
	private static final String otherName3 = "Product Name3";
	private static final String otherName4 = "Product Name4";
	private static final String otherName5 = "Product Name5";
	private static final String otherName6 = "Product Name6";
	private static final String otherName7 = "Product Name7";
	private static final String otherName8 = "Product Name8";
	private static final String otherName9 = "Product Name9";
	private static final String otherName10 = "Product Name10";
	// 更新用
	private static final String otherReName = "New Product Name";
	
		
	/* ------------ 属性タイプ名称 --------------- */
	private static final String attTypeName_String1 = "納品先_" + date;
	private static final String attTypeName_Int1 = "納品数_" + date;
	private static final String attTypeName_Date1 = "納品期日_" + date;
	private static final String attTypeName_Text1 = "注意点_" + date;
	
	private static final String attTypeName_String2 = "担当者_" + date;
	private static final String attTypeName_Int2 = "売り上げ見込み_" + date;
	private static final String attTypeName_Date2 = "販売日時_" + date;
	private static final String attTypeName_Text2 = "メモ_" + date;
	
	private static final String attTypeName_String3 = "営業担当者_" + date;
	
	private static final String attTypeName_String4 = "新製品名_" + date;
	private static final String attTypeName_Int3 = "工数(人月)_" + date;
	private static final String attTypeName_Date3 = "リリース予定日_" + date;
	private static final String attTypeName_Text3 = "備考_" + date;
	
	private static final String attTypeName_String5 = "所属名_" + date;

	private static final String attTypeName_String6 = "建物名_" + date;
	private static final String attTypeName_Int4 = "耐震強度_" + date;
	private static final String attTypeName_Date4 = "完成日_" + date;
	private static final String attTypeName_Text4 = "場所_" + date;

	private static final String attTypeName_String7 = "製品名_" + date;
	private static final String attTypeName_Int5 = "バージョン_" + date;
	private static final String attTypeName_Date5 = "リリース日_" + date;
	private static final String attTypeName_Text5 = "言語_" + date;
	private static final String attTypeName_String8 = "責任者_" + date;
	
	private static final String attTypeName_String9 = "承認者_" + date;
	
	private static final String attTypeName_String10 = "起案者_" + date;

	private static final String attTypeName_String11 = "有権者氏名_" + date;
	private static final String attTypeName_Int6 = "年齢_" + date;
	private static final String attTypeName_Date6 = "生年月日_" + date;
	
	private static final String attTypeName_String12 = "文字列属性A_" + date;
	private static final String attTypeName_String13 = "文字列属性B_" + date;
	private static final String attTypeName_String14 = "文字列属性C_" + date;
	private static final String attTypeName_String15 = "文字列属性D_" + date;
	private static final String attTypeName_String16 = "文字列属性E_" + date;
	private static final String attTypeName_String17 = "文字列属性F_" + date;
	private static final String attTypeName_String18 = "文字列属性G_" + date;
	private static final String attTypeName_String19 = "文字列属性H_" + date;
	private static final String attTypeName_String20 = "文字列属性I_" + date;
	private static final String attTypeName_String21 = "文字列属性J_" + date;
	private static final String attTypeName_String22 = "文字列属性K_" + date;

	private static final String attTypeName_String23 = "文字列属性L_" + date;
	private static final String attTypeName_String24 = "文字列属性M_" + date;
	private static final String attTypeName_String25 = "文字列属性N_" + date;
	private static final String attTypeName_String26 = "文字列属性O_" + date;
	private static final String attTypeName_String27 = "文字列属性P_" + date;
	private static final String attTypeName_String28 = "文字列属性Q_" + date;
	private static final String attTypeName_String29 = "文字列属性R_" + date;
	private static final String attTypeName_String30 = "文字列属性S_" + date;
	private static final String attTypeName_String31 = "文字列属性T_" + date;
	private static final String attTypeName_String32 = "文字列属性U_" + date;
	private static final String attTypeName_String33 = "文字列属性V_" + date;
	
	/* --------------- フォルダ名称 -------------- */
	private static final String FOLDER_NAME1 = "フォルダ１_" + date;
	private static final String FOLDER_NAME2 = "フォルダ１_子１_" + date;	
	private static final String FOLDER_NAME3 = "フォルダ１_子２_" + date;
	private static final String FOLDER_NAME4 = "フォルダ１_子３_" + date;
	private static final String FOLDER_NAME5 = "フォルダ１_子１_孫１_" + date;
	private static final String FOLDER_NAME6 = "フォルダ２_" + date;
	private static final String FOLDER_NAME7 = "フォルダ３_" + date;
	private static final String FOLDER_NAME8 = "フォルダ８_" + date;
	private static final String FOLDER_NAME9 = "フォルダ９_" + date;
	private static final String FOLDER_NAME10 = "フォルダ１０_" + date;
	private static final String FOLDER_NAME11 = "フォルダ１１_" + date;
	private static final String FOLDER_NAME12 = "フォルダ１２_" + date;
	private static final String FOLDER_NAME13 = "フォルダ１３_" + date;

	
	/* ------------ ドキュメント名称 ------------- */
	private static final String DOCUMENT_NAME1 = "ドキュメント１_" + date;
	private static final String DOCUMENT_NAME2 = "ドキュメント２_" + date;
	private static final String DOCUMENT_NAME3 = "ドキュメント３_" + date;
	private static final String DOCUMENT_NAME4 = "ドキュメント４_" + date;
	private static final String DOCUMENT_NAME5 = "ドキュメント５_" + date;
	
	
	/* --------- 属性タイプオブジェクト ---------- */
	private static EIMAttributeType objAttType_str = null;
	private static EIMAttributeType objAttType_int = null;
	private static EIMAttributeType objAttType_date = null;
	private static EIMAttributeType objAttType_text = null;
	private static EIMAttributeType objAttType_str2 = null;
	private static EIMAttributeType objAttType_str3 = null;
	private static EIMAttributeType objAttType_str4 = null;
	private static EIMAttributeType objAttType_str5 = null;
	private static EIMAttributeType objAttType_str6 = null;
	private static EIMAttributeType objAttType_str7 = null;
	private static EIMAttributeType objAttType_str8 = null;
	private static EIMAttributeType objAttType_str9 = null;
	private static EIMAttributeType objAttType_str10 = null;
	private static EIMAttributeType objAttType_str11 = null;
	
	private static final int NO_GET_INPUT_ID = 1000;

	
	/* --------- オブジェクトタイプ名称 ---------- */
	private static final String FOLDER_TYPE_NAME1 = "フォルダタイプ１_";
	private static final String FOLDER_TYPE_NAME2 = "フォルダタイプ２_";
	private static final String FOLDER_TYPE_NAME3 = "フォルダタイプ３_";
	private static final String DOCUMENT_TYPE_NAME1 = "ドキュメントタイプ１_";
	private static final String DOCUMENT_TYPE_NAME2 = "ドキュメントタイプ２_";
	
	private EIMSession sess = null;

	private static AttributeTree attTree1 = null;
	private static AttributeTree attTree2 = null;
	
	/**
	 * AttributeTreeUtil.createAttributeTree(EIMSession, String, int)のためのテスト・メソッド
	 */	
	public void testCreateAttributeTree() {
		
		/**
		 * 正常に登録できるパターン
		 */		
		try
		{
									
			
			EIMSession sess = new EIMSession();
			sess.setConsole();
					
			// テストメソッド実行
			attTree1 = AttributeTreeUtil.createAttributeTree(sess, ATT_TREE_NAME1, AppConstant.CLASSIFY_TARGET_FOLDER);
			
			sess.commit();
			
			// 属性ツリー情報取得
			attTree2 = AttributeTreeUtil.getAttributeTreeById(sess, attTree1.getId());
			
			sess.close();									
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {			
			e.printStackTrace();			
		} 
		
		// id
		assertEquals(attTree1.getId(), attTree2.getId());
		
		// Name
		assertEquals(ATT_TREE_NAME1, attTree1.getName());
		assertEquals(ATT_TREE_NAME1, attTree2.getName());
		
		// DefName
		assertEquals(ATT_TREE_NAME1, attTree1.getDefName());
		assertEquals(ATT_TREE_NAME1, attTree2.getDefName());
		
		// ClassifyTarget
		assertEquals(AppConstant.CLASSIFY_TARGET_FOLDER, attTree1.getClassifyTarget());
		assertEquals(AppConstant.CLASSIFY_TARGET_FOLDER, attTree2.getClassifyTarget());	
		
		// TreeItemList
		//初期登録時は空のリスト
		assertEquals(0, attTree1.getTreeItemList().size());
		assertEquals(0, attTree2.getTreeItemList().size());
		

		/**
		 * 例外でエラーになるパターン
		 * 引数で指定された名称に該当する属性ツリーがすでに存在する場合
		 */			
		try {
			
			EIMSession sess = new EIMSession();
			
			sess.setConsole();
			
            //テストメソッド実行
			// 例外テストのため上記と同じ属性ツリー名称のオブジェクト作成処理を実施
			AttributeTreeUtil.createAttributeTree(sess, ATT_TREE_NAME1, AppConstant.CLASSIFY_TARGET_FOLDER);
			
			sess.close();
			
			fail("createAttributeTree：例外がthrowされませんでした。");
			
		} catch (EIMException eime) {
			assertEquals("createAttributeTree：例外テスト失敗", "EIM.ERROR.COMMON.API.ATTRIBUTE.TREE.NAME.EXISTS", eime.getMessageKey());			
		}catch (Exception e) {			
			e.printStackTrace();			
		} 
		
	}

	/**
	 * AttributeTreeUtil.deleteAttributeTree(EIMSession, AttributeTree)のためのテスト・メソッド
	 * データはtestCreateAttributeTree()実施後のものを使用
	 * （testCreateAttributeTreeの正常終了が前提）
	 */
	public void testDeleteAttributeTree() {

		String otherAttTreeName = null;
		List attTreePos = new ArrayList();
		Method meth = null;
		
		/**
		 * 正常に削除できるパターン
		 */		
		try {
			
			EIMSession sess = new EIMSession();
			
			sess.setConsole();
			
			// 属性ツリー他言語作成
			AttributeTreeUtil.addOtherAttributeTreeName(sess, attTree1.getId(), "ja", otherName10);

			// 属性タイプ作成
			objAttType_str = AttributeUtils.createAttributeType(sess, attTypeName_String11, EIMValueType.getTypeById(sess, EIMValueType.STRING));
			objAttType_int = AttributeUtils.createAttributeType(sess, attTypeName_Int6, EIMValueType.getTypeById(sess, EIMValueType.INTEGER));
			objAttType_date= AttributeUtils.createAttributeType(sess, attTypeName_Date6, EIMValueType.getTypeById(sess, EIMValueType.DATE));
						
			// オブジェクト生成（属性ツリー所属属性）
			EIMObject obj1 = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), Integer.toString(attTree1.getId()) + "_" + 0);
			EIMObject obj2 = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), Integer.toString(attTree1.getId()) + "_" + 1);
			EIMObject obj3 = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), Integer.toString(attTree1.getId()) + "_" + 2);
			
			int objId1 = objAttType_str.getId();
			int objId2 = objAttType_int.getId();
			int objId3 = objAttType_date.getId();
			
			List attTreeItemList = new ArrayList();
			
			// 属性ツリー所属属性情報をリストに追加
			attTreeItemList.add(new AttributeTreeItem(objId1, objAttType_str, true));
			attTreeItemList.add(new AttributeTreeItem(objId2, objAttType_int, true));
			attTreeItemList.add(new AttributeTreeItem(objId3, objAttType_date, false));
			
			
			AttributeTreeUtil.setTreeItems(sess, attTree1, attTreeItemList);
			
			attTree1 = AttributeTreeUtil.getAttributeTreeById(sess, attTree1.getId());
			
			// テストメソッド実行
			AttributeTreeUtil.deleteAttributeTree(sess, attTree1);
			
			otherAttTreeName = AttributeTreeUtil.getOtherAttributeTreeName(sess, attTree1.getId(), "ja");
			
			// privateメソッドにアクセス
			meth = AttributeTreeUtil.class.getDeclaredMethod("getAttrTreePosByAttTreeId",
					new Class[] { EIMSession.class, int.class });
			meth.setAccessible(true);

			attTreePos = (List) meth.invoke(null, new Object[] { sess, new Integer(attTree1.getId())});
			
			sess.commit();
			
			// 属性ツリー情報取得(取得できないためNullが返却されるはず)
			attTree2 = AttributeTreeUtil.getAttributeTreeById(sess, attTree1.getId());
									
			sess.close();
									
		} catch (EIMException eime) {
			eime.getStackTrace();						
		} catch (Exception e) {			
			e.printStackTrace();			
		}
		
		// 削除確認
		assertEquals(null, attTree2);
		assertEquals(null, otherAttTreeName);
		assertEquals(0, attTreePos.size());

		
		/**
		 * 例外でエラーになるパターン
		 * 引数で指定された名称に該当する属性ツリーが存在しない場合
		 */					
		try {
			
			EIMSession sess = new EIMSession();
			
			sess.setConsole();
			
	        // テストメソッド実行
			// 例外テストのため上記と同じ属性ツリー名称のオブジェクト削除処理を実施
			AttributeTreeUtil.deleteAttributeTree(sess, attTree1);
			
			sess.close();
			
			fail("deleteAttributeTree：例外がthrowされませんでした。");
			
		} catch (EIMException eime) {
			//エラー内容確認
			assertEquals("deleteAttributeTree：例外テスト失敗", "EIM.ERROR.COMMON.API.ATTRIBUTE.TREE.NOT.EXIST", eime.getMessageKey());			
		} catch (Exception e) {			
			e.printStackTrace();
			
		}
	}

	/**
	 * AttributeTreeUtil.updateAttributeTree(EIMSession, AttributeTree, String, int)のためのテスト・メソッド
	 */
	public void testUpdateAttributeTree() {

		/**
		 * 正常に更新できるパターン
		 */				
		try {
			
			EIMSession sess = new EIMSession();
			sess.setConsole();
			
			// データ準備
			attTree1 = AttributeTreeUtil.createAttributeTree(sess, ATT_TREE_NAME1, AppConstant.CLASSIFY_TARGET_FOLDER);
			
			// テストメソッド実行
			AttributeTreeUtil.updateAttributeTree(sess, attTree1, attTreeReName, AppConstant.CLASSIFY_TARGET_DOCUMENT);

			sess.commit();
			
			// 更新した属性ツリー情報取得
			attTree2 = AttributeTreeUtil.getAttributeTreeById(sess, attTree1.getId());


			sess.close();
		
		} catch (EIMException eime) {
			eime.getStackTrace();						
		} catch (Exception e) {			
			e.printStackTrace();			
		}
		
		// オブジェクトIDが更新されていないことを確認
		assertEquals(attTree1.getId(), attTree2.getId());
		
		// 属性ツリー名称が更新されたことを確認
		assertEquals(attTreeReName, attTree2.getName());
		
		// 属性ツリー名称(デフォルト名)が更新されたことを確認
		assertEquals(attTreeReName, attTree2.getDefName());
		
		// 分類対象(0:フォルダ/1:ドキュメント)が更新されたことを確認
		assertEquals(AppConstant.CLASSIFY_TARGET_DOCUMENT, attTree2.getClassifyTarget());
		
		// 属性ツリーに所属する属性アイテムのリストが更新されていないことを確認
		//現在は空かどうかで判定(今後変更必要か？)
		assertEquals(0, attTree2.getTreeItemList().size());

		
		/**
		 * 例外でエラーになるパターン
		 * 引数で指定された名称に該当する属性ツリーが存在しない場合
		 */					
		try {
			
			EIMSession sess = new EIMSession();
			
			sess.setConsole();
			
			// 例外発生の為の事前処理（更新対象削除）
			AttributeTreeUtil.deleteAttributeTree(sess, attTree2);			
			
			// テストメソッド実行
			AttributeTreeUtil.updateAttributeTree(sess, attTree2, ATT_TREE_NAME1, AppConstant.CLASSIFY_TARGET_FOLDER);

			sess.close();

			fail("updateAttributeTree：例外がthrowされませんでした。");
			
		} catch (EIMException eime) {
			assertEquals("updateAttributeTree：例外テスト失敗", "EIM.ERROR.COMMON.API.ATTRIBUTE.TREE.NOT.EXIST", eime.getMessageKey());			
		} catch (Exception e) {			
			e.printStackTrace();			
		}
		
	}

	/**
	 * AttributeTreeUtil.addOtherAttributeTreeName(EIMSession, int, String, String)のためのテスト・メソッド
	 */
	public void testAddOtherAttributeTreeName() {
		
		String localOtherName = null;
		String objName = null;		
		List otherNameList = new ArrayList();
		EIMOtherName objOtherName = null;
		EIMObject obj = null;
		
		/**
		 * 正常に登録できるパターン
		 */				
		try {
			
			EIMSession sess = new EIMSession();
			
			sess.setConsole();

			// データ準備
			attTree1 = AttributeTreeUtil.createAttributeTree(sess, ATT_TREE_NAME1, AppConstant.CLASSIFY_TARGET_FOLDER);
			
            // テストメソッド実行
			AttributeTreeUtil.addOtherAttributeTreeName(sess, attTree1.getId(), langId1, otherName1);

            /* ---- 確認データ作成---- */	
			
			//オブジェクト名
			objName = Integer.toString(attTree1.getId()) + "_" + langId1;
			//属性ツリー他言語オブジェクト取得
			obj = AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEOTHER"), objName);
			
			//EIMOterNameのリスト取得
			otherNameList = AttributeTreeUtil.getOtherAttributeTreeNameList(sess, attTree1.getId());
			
			//リストからEIMOterName取得（テストメソッドによって登録された1件しかデータがないはず【後にsize確認】）
			objOtherName = (EIMOtherName)otherNameList.get(0);
			
			// 生成した属性ツリー他言語の「属性ツリー他言語名称」を取得
			localOtherName = AttributeTreeUtil.getOtherAttributeTreeName(sess, attTree1.getId(), langId1);
			/* ----------------------- */
			
			sess.commit();

			sess.close();
						
		} catch (EIMException eime) {
			eime.getStackTrace();
		} catch (Exception e) {			
			e.printStackTrace();			
		}
			
		// オブジェクト名(オブジェクトID)を確認
		assertEquals(objName, obj.getName());

		// 属性ツリー他言語IDを確認
		assertEquals(langId1, objOtherName.getLangId());
		
		// 属性ツリー他言語名称を確認①
		assertEquals(otherName1, objOtherName.getName());
		
		// 属性ツリー他言語名称を確認②--別パターン
		assertEquals(otherName1, localOtherName);
		
		// 属性ツリー他言語リストサイズを確認
		assertEquals(1, otherNameList.size());			

		
		/**
		 * 例外でエラーになるパターン
		 * 引数で指定された属性ツリーIDと他言語IDに該当する属性ツリー他言語が
		 * すでに存在する場合
		 */	
		try {
			EIMSession sess = new EIMSession();
			
			sess.setConsole();
			
			// テストメソッド実行
			// 例外テストのため上記と同じ名称のオブジェクト作成処理を実施
			AttributeTreeUtil.addOtherAttributeTreeName(sess, attTree1.getId(), langId1, otherName1);

			sess.close();
			
			fail("addOtherAttributeTreeName：例外がthrowされませんでした。");
			
		} catch (EIMException eime) {
			
			assertEquals("EIM.ERROR.COMMON.API.ATTRIBUTE.TREE.ALIAS.EXISTS", eime.getMessageKey());
			
		} catch (Exception e) {
			
			e.printStackTrace();
		}
		
	}
	
	
	/**
	 * AttributeTreeUtil.deleteOtherAttributeTreeName(EIMSession, int, String)のためのテスト・メソッド
	 * 属性ツリーはtestAddOtherAttributeTreeName()で作成したものを使用
	 */
	public void testDeleteOtherAttributeTreeName() {
		
		String otherName = null;
		
		/**
		 * 正常に削除できるパターン
		 */					
		try {
			
			EIMSession sess = new EIMSession();
			
			sess.setConsole();

	        // データ準備
			AttributeTreeUtil.addOtherAttributeTreeName(sess, attTree1.getId(), langId2, otherName2);
			
			// テストメソッド実行
			AttributeTreeUtil.deleteOtherAttributeTreeName(sess, attTree1.getId(), langId2);

			// 削除した属性ツリー他言語の「他言語名称」取得(取得できないためNullが返却されるはず)
			otherName = AttributeTreeUtil.getOtherAttributeTreeName(sess, attTree1.getId(), langId2);
			
			sess.commit();
			
			sess.close();
			
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		//削除確認
		assertEquals(null, otherName);
		
		
		/**
		 * 例外でエラーになるパターン
		 * 引数で指定された属性ツリーIDと他言語IDに該当する属性ツリー他言語が
		 * 存在しない場合
		 */					
		try {
			
			EIMSession sess = new EIMSession();
			
			sess.setConsole();

			// テストメソッド実行
			AttributeTreeUtil.deleteOtherAttributeTreeName(sess, attTree1.getId(), langId2);

			sess.close();
			
			fail("deleteOtherAttributeTreeName：例外がthrowされませんでした。");
			
		} catch (EIMException eime) {
			
			assertEquals("EIM.ERROR.COMMON.API.ATTRIBUTE.TREE.ALIAS.NOT.EXIST", eime.getMessageKey());
			
		} catch (Exception e) {
			
			e.printStackTrace();
		}
		
	}

	/**
	 * AttributeTreeUtil.updateOtherAttributeTreeName(EIMSession, int, String, String)のためのテスト・メソッド
	 * 属性ツリーはtestAddOtherAttributeTreeName()で作成したものを使用
	 */
	public void testUpdateOtherAttributeTreeName() {
		
		String otherName = null;
		
		/**
		 * 正常に更新できるパターン
		 */		
		try {
			
			EIMSession sess = new EIMSession();
			
			sess.setConsole();
			
	        // データ準備
			AttributeTreeUtil.addOtherAttributeTreeName(sess, attTree1.getId(), langId3, otherName3);
			
			// テストメソッド実行
			AttributeTreeUtil.updateOtherAttributeTreeName(sess, attTree1.getId(), langId3, otherReName);
			
			// 更新した属性ツリー他言語の「他言語名称」を取得
			otherName = AttributeTreeUtil.getOtherAttributeTreeName(sess, attTree1.getId(), langId3);
			
			sess.commit();
			
			sess.close();
			
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		}
		
		assertEquals(otherReName, otherName);
		
		
		/**
		 * 属性ツリー他言語オブジェクトを新規作成するパターン
		 * 引数で指定された属性ツリーIDと他言語IDに該当する属性ツリー他言語が
		 * 存在しない場合
		 */	
		try {
			
			EIMSession sess = new EIMSession();
			
			sess.setConsole();
			
			// 更新対象の属性ツリー他言語の「他言語名称」を取得
			otherName = AttributeTreeUtil.getOtherAttributeTreeName(sess, attTree1.getId(), langId4);
			//存在しないことを確認
			assertEquals(null, otherName);
			
			// テストメソッド実行
			AttributeTreeUtil.updateOtherAttributeTreeName(sess, attTree1.getId(), langId4, otherName4);
			
			sess.commit();
			
			// 更新対象の属性ツリー他言語の「他言語名称」を取得
			otherName = AttributeTreeUtil.getOtherAttributeTreeName(sess, attTree1.getId(), langId4);
			// 新規作成されたことを確認
			assertEquals(otherName4, otherName);
						
			sess.close();
			
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		}
		
	}

	/**
	 * AttributeTreeUtil.getOtherAttributeTreeNameList(EIMSession, int)のためのテスト・メソッド
	 */
	public void testGetOtherAttributeTreeNameList() {

		List otherNameList = new ArrayList();
		EIMOtherName[] objOtherName = new EIMOtherName[4];
		
		/**
		 * 空のリストが返却されるパターン
		 * 属性ツリーIDに対応する属性ツリー他言語オブジェクトが存在しない場合
		 */					
		try {
			
			EIMSession sess = new EIMSession();
			
			sess.setConsole();
			
			// データ準備
			// 属性ツリーを作成
			attTree1 = AttributeTreeUtil.createAttributeTree(sess, ATT_TREE_NAME2, AppConstant.CLASSIFY_TARGET_FOLDER);
			
			// テストメソッド実行
			otherNameList = AttributeTreeUtil.getOtherAttributeTreeNameList(sess, attTree1.getId());
			
			sess.commit();
			
			sess.close();
						
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		}
		
		assertEquals(0, otherNameList.size());
		
		
		/**
		 * 正常にリストが取得できるパターン
		 */	
		try {
			
			EIMSession sess = new EIMSession();
			
			sess.setConsole();

			/* ------ データ準備 ------ */
			// 属性ツリー他言語を作成
			AttributeTreeUtil.addOtherAttributeTreeName(sess, attTree1.getId(), langId5, otherName5);
			AttributeTreeUtil.addOtherAttributeTreeName(sess, attTree1.getId(), langId6, otherName6);
			AttributeTreeUtil.addOtherAttributeTreeName(sess, attTree1.getId(), langId7, otherName7);
			AttributeTreeUtil.addOtherAttributeTreeName(sess, attTree1.getId(), langId8, otherName8);			
			
			// テストメソッド実行
			otherNameList = AttributeTreeUtil.getOtherAttributeTreeNameList(sess, attTree1.getId());
			
			sess.commit();
			
			sess.close();
			
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		}
		
		// リストサイズを確認
		assertEquals(4, otherNameList.size());
		
		for (int i = 0; i < otherNameList.size(); i++) {
			objOtherName[i] = (EIMOtherName)otherNameList.get(i);
		}
		
		// 「他言語ID」と「他言語名称」を確認
		assertEquals(langId5, objOtherName[0].getLangId());
		assertEquals(otherName5, objOtherName[0].getName());
		
		assertEquals(langId6, objOtherName[1].getLangId());
		assertEquals(otherName6, objOtherName[1].getName());

		assertEquals(langId7, objOtherName[2].getLangId());
		assertEquals(otherName7, objOtherName[2].getName());
		
		assertEquals(langId8, objOtherName[3].getLangId());
		assertEquals(otherName8, objOtherName[3].getName());
		
	}

	/**
	 * AttributeTreeUtil.getOtherAttributeTreeName(EIMSession, int, String)のためのテスト・メソッド
	 * 属性ツリーはtestGetOtherAttributeTreeNameList()で作成したものを使用
	 */
	public void testGetOtherAttributeTreeName() {
		
		String otherName = null;
		
		/**
		 * 正常に「属性ツリー他言語名称」が取得できるパターン
		 */				
		try {
			
			EIMSession sess = new EIMSession();
			
			sess.setConsole();
			
			// テストメソッド実行
			otherName = AttributeTreeUtil.getOtherAttributeTreeName(sess, attTree1.getId(), langId5);
			
			sess.commit();
			
			sess.close();
			
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		}
		
		// 「属性ツリー他言語名称」を確認
		assertEquals(otherName5, otherName);

		
		/**
		 * Nullが返却されるパターン
		 * 属性ツリーIDと他言語IDに対応する属性ツリー他言語オブジェクトが存在しない場合
		 */	
		try {
			
			EIMSession sess = new EIMSession();
			
			sess.setConsole();
			
			// テストメソッド実行
			otherName = AttributeTreeUtil.getOtherAttributeTreeName(sess, attTree1.getId(), NolangId);
			
			sess.commit();
			
			sess.close();	
			
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		}

		//Nullであることを確認
		assertEquals(null, otherName);		
	}

	/**
	 * AttributeTreeUtil.setTreeItems(EIMSession, AttributeTree, List)のためのテスト・メソッド
	 */
	public void testSetTreeItems() {
		
		List attTreeItemList = new ArrayList();
		List objList = new ArrayList();				
		String objName = null;		
		EIMObject obj = null;
		EIMObject obj1 = null;
		EIMObject obj2 = null;
		EIMObject obj3 = null;
		EIMObject obj4 = null;
		
		// 所属属性オブジェクトのID格納用
		int objId1 = 0;
		int objId2 = 0;
		int objId3 = 0;
		
		// 所属属性ID格納用
		int id = 0;
		
		// 所属属性ポジション格納用		
		int position1 = 0;
		int position2 = 0;
		
		// 所属属性値なしフラグ格納用
		int flag1 = 0;
		int flag2 = 0;

		Method meth = null;
		
		/**
		 * 正常パターン
		 * 該当する属性ツリー所属属性が存在しない場合
		 * →引数指定された属性ツリーアイテムをもとに対応する属性ツリー所属属性を
		 *   DBに作成する
		 */	
		try {
			
			EIMSession sess = new EIMSession();
			
			sess.setConsole();
			
			/* ------ データ準備 ------ */
			// 属性ツリーを作成
			attTree1 = AttributeTreeUtil.createAttributeTree(sess, ATT_TREE_NAME3, AppConstant.CLASSIFY_TARGET_DOCUMENT);

			// オブジェクト名称取得
			objName = Integer.toString(attTree1.getId());
			
			// 属性ツリー名称をオブジェクト名とする属性ツリー所属属性情報を全て削除
			//AppObjectUtil.deleteObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName);

			// 属性ツリー名称をオブジェクト名とする属性ツリー所属属性のリストを取得
			//objList = AppObjectUtil.getObjectList(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName);
			
			// 削除されたことを確認
			//assertEquals(0, objList.size());
			
			// 属性タイプ作成
			objAttType_str = AttributeUtils.createAttributeType(sess, attTypeName_String1, EIMValueType.getTypeById(sess, EIMValueType.STRING));
			objAttType_int = AttributeUtils.createAttributeType(sess, attTypeName_Int1, EIMValueType.getTypeById(sess, EIMValueType.INTEGER));
			objAttType_date= AttributeUtils.createAttributeType(sess, attTypeName_Date1, EIMValueType.getTypeById(sess, EIMValueType.DATE));
			objAttType_text = AttributeUtils.createAttributeType(sess, attTypeName_Text1, EIMValueType.getTypeById(sess, EIMValueType.TEXT));
			
			// オブジェクト生成（属性ツリー所属属性）
			obj1 = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName + "_0");
			obj2 = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName + "_1");
			obj3 = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName + "_2");
			obj4 = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName + "_3");

			// 属性ツリー名称をオブジェクト名とする属性ツリー所属属性のリストを取得
			// objList = AppObjectUtil.getObjectList(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName);
			// privateメソッドにアクセス
			meth = AttributeTreeUtil.class.getDeclaredMethod("getAttrTreePosByAttTreeId",
					new Class[] { EIMSession.class, int.class });
			meth.setAccessible(true);

			objList = (List) meth.invoke(null, new Object[] { sess, new Integer(attTree1.getId())});
			
			// 作成されたか確認
			assertEquals(4, objList.size());
			
			objId1 = objAttType_str.getId();
			objId2 = objAttType_int.getId();
			objId3 = objAttType_date.getId();
			
			// 属性ツリー所属属性情報をリストに追加
			attTreeItemList.add(new AttributeTreeItem(objId1, objAttType_str, true));
			attTreeItemList.add(new AttributeTreeItem(objId2, objAttType_int, true));
			attTreeItemList.add(new AttributeTreeItem(objId3, objAttType_date, false));
			 
			attTree1 = AttributeTreeUtil.getAttributeTreeById(sess, attTree1.getId());
			
			// テストメソッド実行
			AttributeTreeUtil.setTreeItems(sess, attTree1, attTreeItemList);
									
			// 属性ツリー名称をオブジェクト名とする属性ツリー所属属性のリストを取得
			//objList = AppObjectUtil.getObjectList(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName);
			objList = (List) meth.invoke(null, new Object[] { sess, new Integer(attTree1.getId())});
			
			// テストメソッド内で削除されて、新規登録されたか確認
			// (size：4→3になったことで削除されたこととする)
			assertEquals(3, objList.size());
			
			for (int i = 0; i < objList.size(); i++) {
				
				obj = (EIMObject)objList.get(i);
				
				//属性ツリー所属属性ID取得
				id = AppObjectUtil.getIntAttr(sess, obj, EIMConfig.get("ATTR_NAME_ATTRTREEPOS_ATTR_ID"), Integer.MIN_VALUE);
				//属性ツリー所属属性ポジション取得
				position1 = AppObjectUtil.getIntAttr(sess, obj, EIMConfig.get("ATTR_NAME_ATTRTREEPOS_ATTR_POS"), Integer.MIN_VALUE);
				//属性ツリー所属属性値なし表示フラグ取得
				flag1 = AppObjectUtil.getIntAttr(sess, obj, EIMConfig.get("ATTR_NAME_ATTRTREEPOS_NO_FLG"), Integer.MIN_VALUE);
				 
				// 比較値（こうあるべき値）作成
				position2 = i;				
				if (id == objId1 || id == objId2) {
					flag2 = 1;
				} else if (id == objId3) {
					flag2 = 0;											
				}
				
				//属性ツリー所属属性ポジションを確認
				assertEquals(position2, position1);
				//属性ツリー所属属性値なし表示フラグを確認
				assertEquals(flag2, flag1);
			}			

			sess.commit();
			
			sess.close();
			
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		}
		
		
		/**
		 * 正常パターン
		 * 引数で指定された属性ツリーアイテムの一覧がNullの場合
		 * →属性ツリー名称をオブジェクト名とする属性ツリー所属属性情報を全て削除後、
		 *   処理を終了する
		 */				
		try {
			
			EIMSession sess = new EIMSession();
			
			sess.setConsole();
			
			/* ------ データ準備 ------ */
			// 属性ツリーを作成
			attTree1 = AttributeTreeUtil.createAttributeTree(sess, ATT_TREE_NAME4, AppConstant.CLASSIFY_TARGET_DOCUMENT);

			// 属性タイプ作成
			objAttType_str = AttributeUtils.createAttributeType(sess, attTypeName_String2, EIMValueType.getTypeById(sess, EIMValueType.STRING));
			objAttType_int = AttributeUtils.createAttributeType(sess, attTypeName_Int2, EIMValueType.getTypeById(sess, EIMValueType.INTEGER));
			
			// オブジェクト生成（属性ツリー所属属性）
			// オブジェクト名称設定
			objName = Integer.toString(attTree1.getId()) + "_0";
			obj1 = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName);
			
			// オブジェクト名称設定
			objName = Integer.toString(attTree1.getId()) + "_1";
			obj2 = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName);
			
			// 属性ツリー名称をオブジェクト名とする属性ツリー所属属性のリストを取得			
			//objList = AppObjectUtil.getObjectList(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName);
			objList = (List) meth.invoke(null, new Object[] { sess, new Integer(attTree1.getId())});

			// 作成されたか確認
			assertEquals(2, objList.size());
			
			objId1 = obj1.getId();
			objId2 = obj2.getId();
			
			attTreeItemList = null;

			attTree1 = AttributeTreeUtil.getAttributeTreeById(sess, attTree1.getId());
			
			// テストメソッド実行
			AttributeTreeUtil.setTreeItems(sess, attTree1, attTreeItemList);

			// 属性ツリー名称をオブジェクト名とする属性ツリー所属属性のリストを取得			
			objList = AppObjectUtil.getObjectList(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName);
			
            sess.commit();
            
            sess.close();
            
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		}

		// 削除のみされて処理が終了したか確認
		assertEquals(0, objList.size());

		
		/**
		 * 正常パターン
		 * 引数で指定された属性ツリーアイテムんの一覧（リスト）が空の場合
		 * →属性ツリー名称をオブジェクト名とする属性ツリー所属属性情報を全て削除後、
		 *   処理を終了する
		 */				
		try {
			
			EIMSession sess = new EIMSession();
			
			sess.setConsole();
			
			/* ------ データ準備 ------ */
			// 属性ツリーを作成
			attTree1 = AttributeTreeUtil.createAttributeTree(sess, ATT_TREE_NAME5, AppConstant.CLASSIFY_TARGET_DOCUMENT);

			// オブジェクト名称取得
			objName = Integer.toString(attTree1.getId());

			// 属性タイプ作成
			objAttType_str = AttributeUtils.createAttributeType(sess, attTypeName_Date2, EIMValueType.getTypeById(sess, EIMValueType.STRING));
			objAttType_int = AttributeUtils.createAttributeType(sess, attTypeName_Text2, EIMValueType.getTypeById(sess, EIMValueType.INTEGER));
			
			// オブジェクト生成（属性ツリー所属属性）
			obj1 = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName + "_0");
			obj2 = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName + "_1");
			
			// 属性ツリー名称をオブジェクト名とする属性ツリー所属属性のリストを取得			
			objList = (List) meth.invoke(null, new Object[] { sess, new Integer(attTree1.getId())});

			// 作成されたか確認
			assertEquals(2, objList.size());
			
			objId1 = obj1.getId();
			objId2 = obj2.getId();
			
			attTree1 = AttributeTreeUtil.getAttributeTreeById(sess, attTree1.getId());
			
			// 空にする
			attTreeItemList = new ArrayList();
						
			// テストメソッド実行
			AttributeTreeUtil.setTreeItems(sess, attTree1, attTreeItemList);

			// 属性ツリー名称をオブジェクト名とする属性ツリー所属属性のリストを取得			
			objList = (List) meth.invoke(null, new Object[] { sess, new Integer(attTree1.getId())});
			
            sess.commit();
            
            sess.close();
            
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		}

		// 削除のみされて処理が終了したか確認
		assertEquals(0, objList.size());
		
	}

	/**
	 * AttributeTreeUtil.updateViewNoValuesFlag(EIMSession, AttributeTreeItem, boolean)のためのテスト
	 */
	public void testUpdateViewNoValuesFlag() {
		
		String objName = null;
		EIMObject obj = null;
		List objList = new ArrayList();
		List attTreeItemList = new ArrayList();
		int objId = 0;
		AttributeTreeItem attTreeItem = null;
		int viewNoFlag = 0;
		Method meth = null;
		
		/**
		 * 正常パターン
		 * 引数で指定された属性ツリーアイテムんの一覧（リスト）が空の場合
		 * →属性ツリー名称をオブジェクト名とする属性ツリー所属属性情報を全て削除後、
		 *   処理を終了する
		 */							
		try {
			
			EIMSession sess = new EIMSession();
			
			sess.setConsole();

			// 属性ツリーを作成
			attTree1 = AttributeTreeUtil.createAttributeTree(sess, ATT_TREE_NAME6, AppConstant.CLASSIFY_TARGET_FOLDER);

			// オブジェクト名称定義
			objName = Integer.toString(attTree1.getId()) + "_0";
	
			// 属性タイプ作成
			objAttType_str = AttributeUtils.createAttributeType(sess, attTypeName_String10, EIMValueType.getTypeById(sess, EIMValueType.STRING));
			
			// オブジェクト生成（属性ツリー所属属性）
			obj = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName);
			
			// 属性ツリーから属性ツリー所属属性のリストを取得			
			meth = AttributeTreeUtil.class.getDeclaredMethod("getAttrTreePosByAttTreeId",
					new Class[] { EIMSession.class, int.class });
			meth.setAccessible(true);
			objList = (List) meth.invoke(null, new Object[] { sess, new Integer(attTree1.getId())});

			// 作成されたか確認
			assertEquals(1, objList.size());
			
			// 属性ツリー所属属性のIDを取得
			objId = obj.getId();
			
			//attTreeItem = new AttributeTreeItem(objId, objAttType_str, false);
			
			attTreeItemList.add(new AttributeTreeItem(objId, objAttType_str, false));
			
			// 属性ツリー所属属性をDBに登録
			AttributeTreeUtil.setTreeItems(sess, attTree1, attTreeItemList);	

			// 属性ツリー名称をオブジェクト名とする属性ツリー所属属性のリストを取得
			objList = (List) meth.invoke(null, new Object[] { sess, new Integer(attTree1.getId())});
				
			obj = (EIMObject)objList.get(0);
			
            // 属性ツリー所属属性のIDを取得
			objId = obj.getId();
				
			//属性ツリー所属属性値なし表示フラグ取得
			viewNoFlag = AppObjectUtil.getIntAttr(sess, obj, EIMConfig.get("ATTR_NAME_ATTRTREEPOS_NO_FLG"), Integer.MIN_VALUE);
				 
			// falseに設定されたことを確認
			assertEquals(0, viewNoFlag);
			
			attTreeItem = new AttributeTreeItem(objId, objAttType_str, false);
			
			attTree1 = AttributeTreeUtil.getAttributeTreeById(sess, attTree1.getId());
			
			// テストメソッド実行
			AttributeTreeUtil.updateViewNoValuesFlag(sess, attTreeItem, true);
			
			obj = ObjectUtils.getObjectById(sess, objId);
			
			viewNoFlag = AppObjectUtil.getIntAttr(sess, obj, EIMConfig.get("ATTR_NAME_ATTRTREEPOS_NO_FLG"), Integer.MIN_VALUE);
			
			
		} catch (Exception e) {
			
			e.printStackTrace();
		}
		
		// trueに更新されたことを確認
		assertEquals(1, viewNoFlag);
	}

	/**
	 * AttributeTreeUtil.getAttributeTreeList(EIMSession)' のためのテスト・メソッド
	 */
	public void testGetAttributeTreeList() {

		List attTreeList = new ArrayList();
		AttributeTree attTree = null;
		List objList = new ArrayList();
		List attTreeItemList = new ArrayList();
		int objId = 0;
		String objName = null;
		Method meth = null;
		
		/**
		 * 正常に属性ツリーオブジェクトのリストが取得できるパターン
		 */				
		try {
			
			EIMSession sess = new EIMSession();
			
			sess.setConsole();
			
			sess.setLang("en");

			// データ準備
			// 属性ツリーを作成
			attTree1 = AttributeTreeUtil.createAttributeTree(sess, ATT_TREE_NAME7, AppConstant.CLASSIFY_TARGET_FOLDER);

			objName = Integer.toString(attTree1.getId());
			
			// 属性タイプ作成
			objAttType_str = AttributeUtils.createAttributeType(sess, attTypeName_String23, EIMValueType.getTypeById(sess, EIMValueType.STRING));
			objAttType_str2 = AttributeUtils.createAttributeType(sess, attTypeName_String24, EIMValueType.getTypeById(sess, EIMValueType.STRING));
			objAttType_str3 = AttributeUtils.createAttributeType(sess, attTypeName_String25, EIMValueType.getTypeById(sess, EIMValueType.STRING));
			objAttType_str4 = AttributeUtils.createAttributeType(sess, attTypeName_String26, EIMValueType.getTypeById(sess, EIMValueType.STRING));
			objAttType_str5 = AttributeUtils.createAttributeType(sess, attTypeName_String27, EIMValueType.getTypeById(sess, EIMValueType.STRING));
			objAttType_str6 = AttributeUtils.createAttributeType(sess, attTypeName_String28, EIMValueType.getTypeById(sess, EIMValueType.STRING));
			objAttType_str7 = AttributeUtils.createAttributeType(sess, attTypeName_String29, EIMValueType.getTypeById(sess, EIMValueType.STRING));
			objAttType_str8 = AttributeUtils.createAttributeType(sess, attTypeName_String30, EIMValueType.getTypeById(sess, EIMValueType.STRING));
			objAttType_str9 = AttributeUtils.createAttributeType(sess, attTypeName_String31, EIMValueType.getTypeById(sess, EIMValueType.STRING));
			objAttType_str10 = AttributeUtils.createAttributeType(sess, attTypeName_String32, EIMValueType.getTypeById(sess, EIMValueType.STRING));
			objAttType_str11 = AttributeUtils.createAttributeType(sess, attTypeName_String33, EIMValueType.getTypeById(sess, EIMValueType.STRING));
			
			// オブジェクト生成（属性ツリー所属属性）
			EIMObject obj1 = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName + "_0");
			EIMObject obj2 = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName + "_1");
			EIMObject obj3 = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName + "_2");
			EIMObject obj4 = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName + "_3");
			EIMObject obj5 = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName + "_4");
			EIMObject obj6 = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName + "_5");
			EIMObject obj7 = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName + "_6");
			EIMObject obj8 = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName + "_7");
			EIMObject obj9 = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName + "_8");
			EIMObject obj10 = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName + "_9");
			EIMObject obj11 = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName + "_10");
			
			// 属性ツリー名称をオブジェクト名とする属性ツリー所属属性のリストを取得			
			// privateメソッドにアクセス
			//objList = AppObjectUtil.getObjectList(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName);
			meth = AttributeTreeUtil.class.getDeclaredMethod("getAttrTreePosByAttTreeId",
					new Class[] { EIMSession.class, int.class });
			meth.setAccessible(true);
			objList = (List) meth.invoke(null, new Object[] { sess, new Integer(attTree1.getId())});

			// 作成されたか確認
			assertEquals(11, objList.size());
									
			attTreeItemList.add(new AttributeTreeItem(obj1.getId(), objAttType_str, false));
			attTreeItemList.add(new AttributeTreeItem(obj2.getId(), objAttType_str2, false));
			attTreeItemList.add(new AttributeTreeItem(obj3.getId(), objAttType_str3, false));
			attTreeItemList.add(new AttributeTreeItem(obj4.getId(), objAttType_str4, false));
			attTreeItemList.add(new AttributeTreeItem(obj5.getId(), objAttType_str5, false));
			attTreeItemList.add(new AttributeTreeItem(obj6.getId(), objAttType_str6, false));
			attTreeItemList.add(new AttributeTreeItem(obj7.getId(), objAttType_str7, false));
			attTreeItemList.add(new AttributeTreeItem(obj8.getId(), objAttType_str8, false));
			attTreeItemList.add(new AttributeTreeItem(obj9.getId(), objAttType_str9, false));
			attTreeItemList.add(new AttributeTreeItem(obj10.getId(), objAttType_str10, false));
			attTreeItemList.add(new AttributeTreeItem(obj11.getId(), objAttType_str11, false));
			
			attTree1 = AttributeTreeUtil.getAttributeTreeById(sess, attTree1.getId());
					
			// 属性ツリー所属属性をDBに登録
			AttributeTreeUtil.setTreeItems(sess, attTree1, attTreeItemList);
			
			attTree1 = AttributeTreeUtil.getAttributeTreeById(sess, attTree1.getId());
			
			AttributeTreeUtil.addOtherAttributeTreeName(sess, attTree1.getId(), sess.getLangId(), ATT_TREE_NAME7 + "（英語）");
			
			// テストメソッド実行
			attTreeList = AttributeTreeUtil.getAttributeTreeList(sess);
			
			// 上記createAttributeTreeメソッド等で作成した属性ツリーが含まれているか
			// また、取得値は正しいか確認
			for (int i = 0; i < attTreeList.size(); i++) {
				
				attTree = (AttributeTree)attTreeList.get(i);
				
				if (attTree.getName().equals(ATT_TREE_NAME7 + "（英語）")) {
					assertEquals(ATT_TREE_NAME7, attTree.getDefName());
					assertEquals(AppConstant.CLASSIFY_TARGET_FOLDER, attTree.getClassifyTarget());
					List resultList = attTree.getTreeItemList();
					assertEquals(11, resultList.size());
					for (int j =0; j < resultList.size(); j++) {
						AttributeTreeItem attTreeItem = (AttributeTreeItem)resultList.get(i);
						assertEquals(i, attTreeItem.getPosition());
					}
				}
			}
			
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		}

		/**
		 * 空のリストが返却されるパターン
		 * 属性ツリーオブジェクトが存在しない場合
		 */		
		Connection con = null;
		Statement stmt = null;
		ResultSet rs = null;
		
		try {
			
			EIMSession sess = new EIMSession();
			
			sess.setConsole();
			
			con = sess.getDBConnection();
			stmt = con.createStatement();

			//属性ツリーオブジェクトを全て削除
			EIMObjectType objtypeAttTree = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREE"));
			StringBuffer sqlBuf = new StringBuffer("DELETE FROM eimobj WHERE type = ");
			sqlBuf.append(objtypeAttTree.getId());
			
			String sql = new String(sqlBuf);
			
			rs = stmt.executeQuery(sql);
			
			// テストメソッド実行
			attTreeList = AttributeTreeUtil.getAttributeTreeList(sess);
			
			sess.commit();
			
			sess.close();
			
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				if (rs != null) {
					rs.close();
				}
				if (stmt != null) {
					stmt.close();
				}
			} catch (SQLException e) {
				e.printStackTrace();
			}			
		}
		
		//返却値が空のリストかを確認
		assertEquals(0, attTreeList.size());
		
	}

	/**
	 * AttributeTreeUtil.getAttributeTreeById(EIMSession, int)のためのテスト・メソッド
	 */
	public void testGetAttributeTreeById() {

		/**
		 * 正常に属性ツリーオブジェクトが取得できるパターン
		 */						
		try {
			
			EIMSession sess = new EIMSession();
			
			sess.setConsole();
			
			sess.setLang("en");
			
			// データ準備
			// 属性ツリーを作成
			attTree1 = AttributeTreeUtil.createAttributeTree(sess, ATT_TREE_NAME8, AppConstant.CLASSIFY_TARGET_FOLDER);
						
			AttributeTreeUtil.addOtherAttributeTreeName(sess, attTree1.getId(), sess.getLangId(), ATT_TREE_NAME8 + "（英語）");
					
			String objName = Integer.toString(attTree1.getId());
			
			// 属性タイプ作成
			objAttType_str = AttributeUtils.createAttributeType(sess, attTypeName_String12, EIMValueType.getTypeById(sess, EIMValueType.STRING));
			objAttType_str2 = AttributeUtils.createAttributeType(sess, attTypeName_String13, EIMValueType.getTypeById(sess, EIMValueType.STRING));
			objAttType_str3 = AttributeUtils.createAttributeType(sess, attTypeName_String14, EIMValueType.getTypeById(sess, EIMValueType.STRING));
			objAttType_str4 = AttributeUtils.createAttributeType(sess, attTypeName_String15, EIMValueType.getTypeById(sess, EIMValueType.STRING));
			objAttType_str5 = AttributeUtils.createAttributeType(sess, attTypeName_String16, EIMValueType.getTypeById(sess, EIMValueType.STRING));
			objAttType_str6 = AttributeUtils.createAttributeType(sess, attTypeName_String17, EIMValueType.getTypeById(sess, EIMValueType.STRING));
			objAttType_str7 = AttributeUtils.createAttributeType(sess, attTypeName_String18, EIMValueType.getTypeById(sess, EIMValueType.STRING));
			objAttType_str8 = AttributeUtils.createAttributeType(sess, attTypeName_String19, EIMValueType.getTypeById(sess, EIMValueType.STRING));
			objAttType_str9 = AttributeUtils.createAttributeType(sess, attTypeName_String20, EIMValueType.getTypeById(sess, EIMValueType.STRING));
			objAttType_str10 = AttributeUtils.createAttributeType(sess, attTypeName_String21, EIMValueType.getTypeById(sess, EIMValueType.STRING));
			objAttType_str11 = AttributeUtils.createAttributeType(sess, attTypeName_String22, EIMValueType.getTypeById(sess, EIMValueType.STRING));
			
			// オブジェクト生成（属性ツリー所属属性）
			EIMObject obj1 = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName + "_0");
			EIMObject obj2 = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName + "_1");
			EIMObject obj3 = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName + "_2");
			EIMObject obj4 = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName + "_3");
			EIMObject obj5 = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName + "_4");
			EIMObject obj6 = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName + "_5");
			EIMObject obj7 = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName + "_6");
			EIMObject obj8 = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName + "_7");
			EIMObject obj9 = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName + "_8");
			EIMObject obj10 = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName + "_9");
			EIMObject obj11 = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName + "_10");
			
			List attTreeItemList = new ArrayList();
			attTreeItemList.add(new AttributeTreeItem(obj1.getId(), objAttType_str, false));
			attTreeItemList.add(new AttributeTreeItem(obj2.getId(), objAttType_str2, false));
			attTreeItemList.add(new AttributeTreeItem(obj3.getId(), objAttType_str3, false));
			attTreeItemList.add(new AttributeTreeItem(obj4.getId(), objAttType_str4, false));
			attTreeItemList.add(new AttributeTreeItem(obj5.getId(), objAttType_str5, false));
			attTreeItemList.add(new AttributeTreeItem(obj6.getId(), objAttType_str6, false));
			attTreeItemList.add(new AttributeTreeItem(obj7.getId(), objAttType_str7, false));
			attTreeItemList.add(new AttributeTreeItem(obj8.getId(), objAttType_str8, false));
			attTreeItemList.add(new AttributeTreeItem(obj9.getId(), objAttType_str9, false));
			attTreeItemList.add(new AttributeTreeItem(obj10.getId(), objAttType_str10, false));
			attTreeItemList.add(new AttributeTreeItem(obj11.getId(), objAttType_str11, false));
			
			// テストメソッド実行
			attTree2 = AttributeTreeUtil.getAttributeTreeById(sess, attTree1.getId());
			
			// 属性ツリー所属属性をDBに登録
			AttributeTreeUtil.setTreeItems(sess, attTree2, attTreeItemList);	

			// テストメソッド実行
			attTree2 = AttributeTreeUtil.getAttributeTreeById(sess, attTree2.getId());			
			
			sess.commit();
			
			sess.close();
			
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		}

		// getName()
		assertEquals(ATT_TREE_NAME8, attTree1.getName());
		assertEquals( ATT_TREE_NAME8 + "（英語）", attTree2.getName());
		
		// getDefName()
		assertEquals(ATT_TREE_NAME8, attTree1.getDefName());
		assertEquals(ATT_TREE_NAME8, attTree2.getDefName());
		
		// getClassifyTarget()
		assertEquals(AppConstant.CLASSIFY_TARGET_FOLDER, attTree1.getClassifyTarget());
		assertEquals(AppConstant.CLASSIFY_TARGET_FOLDER, attTree2.getClassifyTarget());	
		
		List resultList = attTree2.getTreeItemList();
		
		// getTreeItemList()
		// 空のため0と比較（今後データが必要）
		assertEquals(11, resultList.size());
		
		for (int i =0; i < resultList.size(); i++) {
			AttributeTreeItem attTreeItem = (AttributeTreeItem)resultList.get(i);
			assertEquals(i,attTreeItem.getPosition());
		}
		
		/**
		 * Nullが返却されるパターン
		 * 該当する属性ツリーオブジェクトが存在しない場合
		 */						
		try {
			EIMSession sess = new EIMSession();
			
			sess.setConsole();
			
			attTree2 = AttributeTreeUtil.getAttributeTreeById(sess, NO_GET_INPUT_ID);

			sess.commit();
			
			sess.close();
			
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		}

		// Nullか確認
		assertEquals(null, attTree2);	
		
	}
	
	/**
	 * AttributeTreeUtil.getClassifiedObjects(EIMSession, AttributeTree, List)のためのテスト・メソッド
	 */
	public void testGetClassifiedObjects() {

		EIMObject objParentFolder = null;
		EIMObject objChildFolder1 = null;
		EIMObject objChildFolder2 = null;
		EIMObject objChildFolder3 = null;
		EIMObject objGrandChildFolder1 = null;
		AttributeTree attTree1 = null;
		
        // 属性値
		String inputStr = "第３ビル";
		int inputInt = 8;
		Date inputDate = new Date();
		String inputText = "東京都";
		
		List resultObjList = new ArrayList();
		EIMObject resultObj1 = null;
		EIMObject resultObj2 = null;
		
		EIMObjectType childFolderType = null;
		EIMRelationType relType = null;

		/**
		 * 設定状態チェック
		 */
		if(READ_ONLY_USER_ID1 == 0 || NOT_READ_ONLY_USER_ID == 0 || TARGET_ATTR_TYPE_ID == 0
				|| TARGET_ATTR_TYPE_VALUE == null || TARGET_FOLDER_TYPE_ID1 == 0 || TARGET_FOLDER_TYPE_ID2 == 0
				|| TARGET_FOLDER_TYPE_ID3 == 0 || TARGET_DOCUMENT_TYPE_ID1 == 0 || TARGET_DOCUMENT_TYPE_ID2 == 0
				|| TARGET_DOCUMENT_TYPE_ID3 == 0 || TARGET_DOCUMENT_OBJ_ID1 == 0 || TARGET_DOCUMENT_OBJ_ID2 == 0
				|| TARGET_DOCUMENT_OBJ_ID3 == 0)
		{
			System.out.println("****************************************************************************************");
			System.out.println("このテストプログラムを動作させる前に、ソースコードの先頭に変数を設定してください");
			System.out.println("");
			System.out.println("");
			System.out.println("それぞれの変数についての説明は、ソースコードの先頭のコメントを参照してください");
			System.out.println("****************************************************************************************");
			assertFalse(true);
		}
		
		/* ------ フォルダ ------ */
		/**
		 * データ準備
		 */			
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();
	
			// フォルダタイプ取得
			EIMObjectType folderType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_FOLDER"));
			
			// 子フォルダタイプ作成
			childFolderType = ObjectUtils.createObjectType(sess, FOLDER_TYPE_NAME1 + date , folderType);

			// 属性タイプ作成
			// 文字列型
			objAttType_str = AttributeUtils.createAttributeType(sess, attTypeName_String6, EIMValueType.getTypeById(sess, EIMValueType.STRING));
			// 数値型
			objAttType_int = AttributeUtils.createAttributeType(sess, attTypeName_Int4, EIMValueType.getTypeById(sess, EIMValueType.INTEGER));
			// 日付型
			objAttType_date = AttributeUtils.createAttributeType(sess, attTypeName_Date4, EIMValueType.getTypeById(sess, EIMValueType.DATE));			
			// テキスト型
			objAttType_text = AttributeUtils.createAttributeType(sess, attTypeName_Text4, EIMValueType.getTypeById(sess, EIMValueType.TEXT));
			
			// フォルダタイプに属性タイプを設定
			ObjectAttributeUtils.applyAttributeType(sess, childFolderType, objAttType_str);
			ObjectAttributeUtils.applyAttributeType(sess, childFolderType, objAttType_int);
			ObjectAttributeUtils.applyAttributeType(sess, childFolderType, objAttType_date);
			ObjectAttributeUtils.applyAttributeType(sess, childFolderType, objAttType_text);
	
			// Relation Type Document
			relType = RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));			

			/* ------ 親フォルダ1作成 ------ */
			// 親フォルダ1作成
			objParentFolder = ObjectUtils.createObject(sess, childFolderType, FOLDER_NAME1);

			/* ------ 子フォルダ１作成 ------ */			
			// フォルダ作成
			objChildFolder1 = ObjectUtils.createObject(sess, childFolderType, FOLDER_NAME2);
		
			// Create Relation
			RelationUtils.createRelation(sess, relType, objParentFolder, objChildFolder1, EIMConstant.DEPU_CHECK_NAME);
			
			/* ------ 子フォルダ２作成 ------ */			
			// フォルダ作成
			objChildFolder2 = ObjectUtils.createObject(sess, childFolderType, FOLDER_NAME3);
		
			// Create Relation
			RelationUtils.createRelation(sess, relType, objParentFolder, objChildFolder2, EIMConstant.DEPU_CHECK_NAME);

			/* ------ 子フォルダ３作成 ------ */			
			// フォルダ作成
			objChildFolder3 = ObjectUtils.createObject(sess, childFolderType, FOLDER_NAME4);
		
			// Create Relation
			RelationUtils.createRelation(sess, relType, objParentFolder, objChildFolder3, EIMConstant.DEPU_CHECK_NAME);
						
			// 属性ツリ1ー生成
			attTree1 = AttributeTreeUtil.createAttributeTree(sess, ATT_TREE_NAME16, AppConstant.CLASSIFY_TARGET_FOLDER);

			// オブジェクト名称を取得
			String objName = attTree1.getName();			
			
			// オブジェクト生成（属性ツリー所属属性）
			EIMObject obj1 = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName + "_0");
			EIMObject obj2 = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName + "_1");
			EIMObject obj3 = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName + "_2");
			EIMObject obj4 = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName + "_3");
			
			// 属性ツリー所属属性情報をリストに追加(全て「属性値なしを表示する」)
			List attTreeItemList = new ArrayList();
			attTreeItemList.add(new AttributeTreeItem(obj1.getId(), objAttType_str, true));
			attTreeItemList.add(new AttributeTreeItem(obj2.getId(), objAttType_int, true));
			attTreeItemList.add(new AttributeTreeItem(obj3.getId(), objAttType_date, true));
			attTreeItemList.add(new AttributeTreeItem(obj4.getId(), objAttType_text, true));			

		    // 属性ツリー所属属性を属性ツリーに設定
			AttributeTreeUtil.setTreeItems(sess, attTree1, attTreeItemList);
			attTree1.setTreeItemList(attTreeItemList);

			// 親フォルダに属性値の値を設定
			AppObjectUtil.setAttr(sess, objParentFolder, attTypeName_String6, inputStr);

			objParentFolder = AppObjectUtil.getObject(sess, childFolderType.getName(), objParentFolder.getName());
			
			// 子フォルダ１に全ての属性値の値を設定			
			AppObjectUtil.setAttr(sess, objChildFolder1, attTypeName_String6, inputStr);
			AppObjectUtil.setAttr(sess, objChildFolder1, attTypeName_Int4, inputInt);
			AppObjectUtil.setAttr(sess, objChildFolder1, attTypeName_Date4, inputDate);
			AppObjectUtil.setAttr(sess, objChildFolder1, attTypeName_Text4, inputText);

			objChildFolder1 = AppObjectUtil.getObject(sess, childFolderType.getName(), objChildFolder1.getName());
			
			// 子フォルダ２に全ての属性値の値を設定			
			AppObjectUtil.setAttr(sess, objChildFolder2, attTypeName_String6, inputStr);
			AppObjectUtil.setAttr(sess, objChildFolder2, attTypeName_Int4, inputInt);
			AppObjectUtil.setAttr(sess, objChildFolder2, attTypeName_Date4, inputDate);
			AppObjectUtil.setAttr(sess, objChildFolder2, attTypeName_Text4, inputText);

			objChildFolder2 = AppObjectUtil.getObject(sess, childFolderType.getName(), objChildFolder2.getName());
			
			// 子フォルダ３は全ての属性値はnull		
			
			sess.commit();
			
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}
		
		/**
		 * 正常パターン
		 * 条件
		 * ・検索条件の属性タイプに文字列、数値、日付、テキスト型属性を含む
		 * ・検索条件の属性値リストの文字列属性以外nullを設定
		 * →検索条件に該当するフォルダ(EIMObjectのリスト)を取得
		 */
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();
			
			List upperAttributeValList = new ArrayList();
			upperAttributeValList.add(inputStr);
			upperAttributeValList.add(null);
			upperAttributeValList.add(null);
			upperAttributeValList.add(null);

			// 初期化
			resultObjList = new ArrayList(); 
			 
			resultObjList = AttributeTreeUtil.getClassifiedObjects(sess, attTree1, upperAttributeValList);
			
			sess.commit();
			
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}
		
		// 親フォルダが返却するはず
		assertEquals(1, resultObjList.size());
		
		resultObj1 = (EIMObject)resultObjList.get(0);
		
		// ID
		assertEquals(objParentFolder.getId(), resultObj1.getId());
		// Name
		assertEquals(objParentFolder.getName(), resultObj1.getName());

		/**
		 * 正常パターン
		 * 条件
		 * ・検索条件の属性タイプに文字列、数値、日付、テキスト型属性を含む
		 * ・検索条件の属性値リスト(文字列、数値、日付)全てに値を設定
		 * →検索条件に該当するフォルダ(EIMObjectのリスト)を取得
		 */		
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();
			
			List upperAttributeValList = new ArrayList();
			upperAttributeValList.add(inputStr);
			upperAttributeValList.add(new Integer(inputInt));
			upperAttributeValList.add(inputDate);
			upperAttributeValList.add(inputText);
			
			// 初期化
			resultObjList = new ArrayList(); 

			resultObjList = AttributeTreeUtil.getClassifiedObjects(sess, attTree1, upperAttributeValList);
			
			sess.commit();
			
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}
		
		// 子フォルダ１&２が返却するはず
		assertEquals(2, resultObjList.size());
		
		resultObj1 = (EIMObject)resultObjList.get(0);
		resultObj2 = (EIMObject)resultObjList.get(1);

		// ID
		assertEquals(objChildFolder1.getId(), resultObj1.getId());
		// Name
		assertEquals(objChildFolder1.getName(), resultObj1.getName());
		
		// ID
		assertEquals(objChildFolder2.getId(), resultObj2.getId());
		// Name
		assertEquals(objChildFolder2.getName(), resultObj2.getName());
		
		/**
		 * 正常パターン
		 * 条件
		 * ・検索条件の属性タイプに文字列、数値、日付、テキスト型属性を含む
		 * ・検索条件の属性値リストに全てnullを設定
		 * →検索条件に該当するフォルダ(EIMObjectのリスト)を取得
		 */	
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();
			
			List upperAttributeValList = new ArrayList();
			upperAttributeValList.add(null);
			upperAttributeValList.add(null);
			upperAttributeValList.add(null);
			upperAttributeValList.add(null);

			// 初期化
			resultObjList = new ArrayList(); 

			resultObjList = AttributeTreeUtil.getClassifiedObjects(sess, attTree1, upperAttributeValList);
			
			sess.commit();
			
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}
		
		// 属性値が全てnullのフォルダが全てヒットするため該当フォルダが
		// ヒットしたかで正常判定		
		for (int i = 0; i < resultObjList.size(); i++) {
			
			resultObj1 = (EIMObject)resultObjList.get(i);
			
			// 子フォルダ３が返却するはず			
			if (resultObj1.getId() == objChildFolder3.getId()) {
				// Name
				assertEquals(objChildFolder3.getName(), resultObj1.getName());				
			} else {
				// 親フォルダ、子フォルダ１＆２はヒットしていないことを確認
				assertFalse(objParentFolder.getId() == resultObj1.getId());
				assertFalse(objChildFolder1.getId() == resultObj1.getId());
				assertFalse(objChildFolder2.getId() == resultObj1.getId());
			}
		}
		
		/**
		 * 正常パターン
		 * 条件
		 * ・検索条件の属性タイプに文字列、数値、日付、テキスト型属性を含む
		 * ・子フォルダ１に孫フォルダ１が存在する
		 * ・両フォルダとも属性値は同値
		 * ・検索条件の属性値リストに全て値を設定
		 * →検索条件に該当するフォルダ(EIMObjectのリスト)を取得
		 * 　（孫フォルダ１は取得されない）
		 */			
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();
			
			/* ------孫フォルダ１作成 ------ */			
			// フォルダ作成
			objGrandChildFolder1 = ObjectUtils.createObject(sess, childFolderType, FOLDER_NAME5);
		
			// Create Relation
			RelationUtils.createRelation(sess, relType, objChildFolder1, objGrandChildFolder1, EIMConstant.DEPU_CHECK_NAME);
			
			// 孫フォルダ１に全ての属性値の値を設定(子フォルダ１と同値)			
			AppObjectUtil.setAttr(sess, objGrandChildFolder1, attTypeName_String6, inputStr);
			AppObjectUtil.setAttr(sess, objGrandChildFolder1, attTypeName_Int4, inputInt);
			AppObjectUtil.setAttr(sess, objGrandChildFolder1, attTypeName_Date4, inputDate);
			AppObjectUtil.setAttr(sess, objGrandChildFolder1, attTypeName_Text4, inputText);
			
			sess.commit();
			
			List upperAttributeValList = new ArrayList();
			upperAttributeValList.add(inputStr);
			upperAttributeValList.add(new Integer(inputInt));
			upperAttributeValList.add(inputDate);
			upperAttributeValList.add(inputText);
			
			// 初期化
			resultObjList = new ArrayList(); 

			// テストメソッド実行
			resultObjList = AttributeTreeUtil.getClassifiedObjects(sess, attTree1, upperAttributeValList);
			
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}
	
		// 子フォルダ１&２が返却するはず
		assertEquals(2, resultObjList.size());
		
		resultObj1 = (EIMObject)resultObjList.get(0);
		resultObj2 = (EIMObject)resultObjList.get(1);

		// ID
		assertEquals(objChildFolder1.getId(), resultObj1.getId());
		// Name
		assertEquals(objChildFolder1.getName(), resultObj1.getName());
		
		// ID
		assertEquals(objChildFolder2.getId(), resultObj2.getId());
		// Name
		assertEquals(objChildFolder2.getName(), resultObj2.getName());
		
		
		/* ------ ドキュメント ------ */
		EIMObject objDocument1 = null;
		EIMObject objDocument2 = null;
		EIMObject objDocument3 = null;
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();

			// ドキュメントタイプ取得
			EIMObjectType documentType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT"));
			
			// 子ドキュメントタイプ作成
			EIMObjectType childDocumentType = ObjectUtils.createObjectType(sess, DOCUMENT_TYPE_NAME1 + date , documentType);
			
			// ドキュメントタイプに属性タイプを設定
			ObjectAttributeUtils.applyAttributeType(sess, childDocumentType, objAttType_str);
			ObjectAttributeUtils.applyAttributeType(sess, childDocumentType, objAttType_int);
			ObjectAttributeUtils.applyAttributeType(sess, childDocumentType, objAttType_date);
			ObjectAttributeUtils.applyAttributeType(sess, childDocumentType, objAttType_text);
	
			// ドキュメント１作成
			objDocument1 = ObjectUtils.createObject(sess, childDocumentType, DOCUMENT_NAME1);
			
			// ドキュメント２作成
			objDocument2 = ObjectUtils.createObject(sess, childDocumentType, DOCUMENT_NAME2);
			
			// ドキュメント３作成
			objDocument3 = ObjectUtils.createObject(sess, childDocumentType, DOCUMENT_NAME3);
			
			// ドキュメント１に全ての属性値に値を設定
			AppObjectUtil.setAttr(sess, objDocument1, attTypeName_String6, inputStr);
			AppObjectUtil.setAttr(sess, objDocument1, attTypeName_Int4, inputInt);
			AppObjectUtil.setAttr(sess, objDocument1, attTypeName_Date4, inputDate);
			AppObjectUtil.setAttr(sess, objDocument1, attTypeName_Text4, inputText);

			// ドキュメント１に全ての属性値に値を設定(ドキュメント１と同値)
			AppObjectUtil.setAttr(sess, objDocument2, attTypeName_String6, inputStr);
			AppObjectUtil.setAttr(sess, objDocument2, attTypeName_Int4, inputInt);
			AppObjectUtil.setAttr(sess, objDocument2, attTypeName_Date4, inputDate);
			AppObjectUtil.setAttr(sess, objDocument2, attTypeName_Text4, inputText);
			
			// ドキュメント３の属性値は全てnull
			
			// 属性ツリーの分類対象を「ドキュメント」に変更
			AttributeTreeUtil.updateAttributeTree(sess, attTree1, ATT_TREE_NAME16, AppConstant.CLASSIFY_TARGET_DOCUMENT);

			// 更新した属性ツリーを取得
			attTree1 = AttributeTreeUtil.getAttributeTreeById(sess, attTree1.getId());
			
			sess.commit();
			
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}		
	
		/**
		 * 正常パターン
		 * 条件
		 * ・検索条件の属性タイプに文字列、数値、日付、テキスト型属性を含む
		 * ・検索条件の属性値リストに全てnullを設定
		 * →検索条件に該当するドキュメント(EIMObjectのリスト)を取得
		 */
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();

			List upperAttributeValList = new ArrayList();
			upperAttributeValList.add(null);
			upperAttributeValList.add(null);
			upperAttributeValList.add(null);
			upperAttributeValList.add(null);

			// 初期化
			resultObjList = new ArrayList(); 

			// テストメソッド実行
			resultObjList = AttributeTreeUtil.getClassifiedObjects(sess, attTree1, upperAttributeValList);

			sess.commit();
			
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}
		
		// 属性値が全てnullのドキュメントが全てヒットするため該当フォルダが
		// ヒットしたかで正常判定		
		for (int i = 0; i < resultObjList.size(); i++) {
			
			resultObj1 = (EIMObject)resultObjList.get(i);
			
			// ドキュメント３が返却するはず			
			if (resultObj1.getId() == objDocument3.getId()) {
				// Name
				assertEquals(objDocument3.getName(), resultObj1.getName());				
			} else {
				// ドキュメント１＆２はヒットしていないことを確認
				assertFalse(objDocument1.getId() == resultObj1.getId());
				assertFalse(objDocument2.getId() == resultObj1.getId());
			}
		}
		
		/**
		 * 正常パターン
		 * 条件
		 * ・検索条件の属性タイプに文字列、数値、日付、テキスト型属性を含む
		 * ・検索条件の属性値リストの日付型属性のみに値をそれ以外はnullを設定
		 * →検索条件に該当するドキュメント(EIMObjectのリスト)を取得
		 */		
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();
						
			// 検索ヒットさせるためドキュメント３の日付型属性にのみ値を設定
			AppObjectUtil.setAttr(sess, objDocument3, attTypeName_Date4, inputDate);

			sess.commit();
			
			List upperAttributeValList = new ArrayList();
			upperAttributeValList.add(null);
			upperAttributeValList.add(null);
			upperAttributeValList.add(inputDate);
			upperAttributeValList.add(null);
			
			// 初期化
			resultObjList = new ArrayList(); 

			// テストメソッド実行
			resultObjList = AttributeTreeUtil.getClassifiedObjects(sess, attTree1, upperAttributeValList);
						
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}
		
		// ドキュメント３が返却するはず
		assertEquals(1, resultObjList.size());
		
		resultObj1 = (EIMObject)resultObjList.get(0);
		
		// ID
		assertEquals(objDocument3.getId(), resultObj1.getId());
		// Name
		assertEquals(objDocument3.getName(), resultObj1.getName());
		
		/**
		 * 正常パターン
		 * 条件
		 * ・検索条件の属性タイプに文字列、数値、日付、テキスト型属性を含む
		 * ・検索条件の属性値リストに全て値を設定
		 * →検索条件に該当するドキュメント(EIMObjectのリスト)を取得
		 */	
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();
			
			List upperAttributeValList = new ArrayList();
			upperAttributeValList.add(inputStr);
			upperAttributeValList.add(new Integer(inputInt));
			upperAttributeValList.add(inputDate);
			upperAttributeValList.add(inputText);

			// 初期化
			resultObjList = new ArrayList(); 

			// テストメソッド実行
			resultObjList = AttributeTreeUtil.getClassifiedObjects(sess, attTree1, upperAttributeValList);
			
			sess.commit();
			
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}
		
		// ドキュメント１&２が返却するはず
		assertEquals(2, resultObjList.size());
		
		resultObj1 = (EIMObject)resultObjList.get(0);
		resultObj2 = (EIMObject)resultObjList.get(1);
		
		// ID
		assertEquals(objDocument1.getId(), resultObj1.getId());
		// Name
		assertEquals(objDocument1.getName(), resultObj1.getName());
		
		// ID
		assertEquals(objDocument2.getId(), resultObj2.getId());
		// Name
		assertEquals(objDocument2.getName(), resultObj2.getName());
	
		/**
		 * 正常パターン（検索ヒットなし）
		 * 条件
		 * ・検索条件の属性タイプに文字列、数値、日付、テキスト型属性を含む
		 * ・検索条件の属性値リストに全て値を設定（検索ヒットしない条件）
		 * →検索条件に該当するドキュメントがないため長さ0のEIMObjectのリストが
		 * 　取得される
		 */	
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();
			
			// 検索ヒットしない条件設定
			List upperAttributeValList = new ArrayList();
			upperAttributeValList.add("建設情報");
			upperAttributeValList.add(new Integer(3));
			upperAttributeValList.add(new Date());
			upperAttributeValList.add("メモ");
			
			// 初期化
			resultObjList = new ArrayList(); 

			// テストメソッド実行
			resultObjList = AttributeTreeUtil.getClassifiedObjects(sess, attTree1, upperAttributeValList);
			
			sess.commit();
			
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}
		
		// 長さ0のリストの返却を確認
		assertEquals(0, resultObjList.size());

		/**
		 * 例外パターン
		 * 条件
		 * ・検索条件の属性タイプに文字列、数値、日付、テキスト型属性を含む
		 * ・検索条件の属性値リストに属性ツリーに設定されている数以外の数の
		 * 　属性値を設定
		 * →例外発生
		 */	
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();

			// 指定数が1つ足りない
			List upperAttributeValList = new ArrayList();
			upperAttributeValList.add(inputStr);
			upperAttributeValList.add(new Integer(inputInt));
			upperAttributeValList.add(inputDate);

			// 初期化
			resultObjList = new ArrayList(); 

			// テストメソッド実行
			resultObjList = AttributeTreeUtil.getClassifiedObjects(sess, attTree1, upperAttributeValList);

			sess.commit();
			
			fail("getClassifiedObjects：例外がthrowされませんでした。");
			
		} catch (EIMException eime) {
			assertEquals("getClassifiedObjects：例外テスト失敗", "EIM.ERROR.COMMON.API.INVALID.ATTRIBUTE.TREE.INVALID.VALUE.NUM", eime.getMessageKey());			
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}
		
			
		/* ----- 権限テスト（フォルダ） ----- */
		/* ----- データ準備 ----- */
		EIMObject folder1 = null;
		EIMObject folder2 = null;
		EIMObject folder3 = null;

		Connection con = null;
		Statement stmt = null;
		ResultSet rs = null;
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();
			
			// 前回のテストで作成したフォルダを削除
			con = sess.getDBConnection();
			stmt = con.createStatement();
			String sql = "delete from eimobj where name like " + "'フォルダ８_%'";
			rs = stmt.executeQuery(sql);
			sql = "delete from eimobj where name like " + "'フォルダ９_%'";
			rs = stmt.executeQuery(sql);
			sql = "delete from eimobj where name like " + "'フォルダ１０_%'";
			rs = stmt.executeQuery(sql);
			
			/* フォルダタイプ取得 */
			EIMObjectType folderType1 = ObjectUtils.getObjectTypeById(sess, TARGET_FOLDER_TYPE_ID1);
			// （ワークフローなし）
			EIMObjectType folderType2 = ObjectUtils.getObjectTypeById(sess, TARGET_FOLDER_TYPE_ID2);
			EIMObjectType folderType3 = ObjectUtils.getObjectTypeById(sess, TARGET_FOLDER_TYPE_ID3);

			// 属性タイプ取得(文字列型)
			objAttType_str = AttributeUtils.getAttributeTypeById(sess, TARGET_ATTR_TYPE_ID);

			/* フォルダ作成 */
			folder1 = ObjectUtils.createObject(sess, folderType1, FOLDER_NAME8);
			// （ワークフローなし）
			folder2 = ObjectUtils.createObject(sess, folderType2, FOLDER_NAME9);
			folder3 = ObjectUtils.createObject(sess, folderType3, FOLDER_NAME10);
			
			/* セキュリティ設定 */
			SecurityUtils.setSecurity(sess, folder1, SecurityUtils.getSecurityById(sess, folderType1.getSecurityId()));
			SecurityUtils.setSecurity(sess, folder2, SecurityUtils.getSecurityById(sess, folderType2.getSecurityId()));
			SecurityUtils.setSecurity(sess, folder3, SecurityUtils.getSecurityById(sess, folderType3.getSecurityId()));
					
			// 属性ツリー生成
			attTree1 = AttributeTreeUtil.createAttributeTree(sess, ATT_TREE_NAME19, AppConstant.CLASSIFY_TARGET_FOLDER);

			// オブジェクト名称を取得
			String objName = attTree1.getName();			
			
			// オブジェクト生成（属性ツリー所属属性）
			EIMObject obj = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName + "_0");
			
			// 属性ツリー所属属性情報をリストに追加(全て「属性値なしを表示する」)
			List attTreeItemList = new ArrayList();
			attTreeItemList.add(new AttributeTreeItem(obj.getId(), objAttType_str, true));

		    // 属性ツリー所属属性を属性ツリーに設定
			AttributeTreeUtil.setTreeItems(sess, attTree1, attTreeItemList);
			attTree1.setTreeItemList(attTreeItemList);

			inputStr = TARGET_ATTR_TYPE_VALUE;
			// フォルダに属性値の値を設定
			AppObjectUtil.setAttr(sess, folder1, objAttType_str.getName(), inputStr);
			AppObjectUtil.setAttr(sess, folder2, objAttType_str.getName(), inputStr);
			AppObjectUtil.setAttr(sess, folder3, objAttType_str.getName(), inputStr);
			
			folder1 = AppObjectUtil.getObject(sess, folderType1.getName(), folder1.getName());
			folder2 = AppObjectUtil.getObject(sess, folderType2.getName(), folder2.getName());
			folder3 = AppObjectUtil.getObject(sess, folderType3.getName(), folder3.getName());

			// folder3を「編集中」から「公開済」にステータスUPさせる
			while (!folder3.getStatus().getType().getName().equals("公開済")) {				
				folder3 = WorkFlowUtils.statusUp(sess, folder3);				
			}
			
			sess.commit();
			
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				if (rs != null) {
					rs.close();
				}
				if (stmt != null) {
					stmt.close();
				}
				sess.close();	
			} catch (Exception e) {
				e.printStackTrace();
			}			
			
		}
		
		/**
		 * 正常パターン
		 * 条件
		 * ・読み込み権限のみのユーザーで実行
		 * ・検索条件の属性値リストに全て値を設定（属性が１つなので１つのみ）
		 * →読み込み権限が設定されていて、ステータスが「公開済」
		 * 　またはワークフローがないフォルダを取得する。
		 * 　→ワークフローがないフォルダのみを取得する。
		 */		
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();
			
			EIMUser user = UserUtils.getUserById(sess, READ_ONLY_USER_ID1);
			
			// ユーザー切り替え
			sess.setUser(user);
			
			// folder1
			// 書き込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, folder1, user, EIMAccessRole.UPDATE));
			// 読み込み権限があることを確認
			assertTrue(SecurityUtils.enabled(sess, folder1, user, EIMAccessRole.READ));
			// ステータスが「公開済」でないことを確認
			assertFalse(folder1.getStatus().getType().getName().equals("公開済"));
			// ステータスが「編集中」であることを確認
			assertTrue(folder1.getStatus().getType().getName().equals("編集中"));
			
			// folder2
			// 書き込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, folder2, user, EIMAccessRole.UPDATE));
			// 読み込み権限があることを確認
			assertTrue(SecurityUtils.enabled(sess, folder2, user, EIMAccessRole.READ));
			// ステータスがないことを確認
			assertNull(folder2.getStatus());
			
			// folder3
			// 書き込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, folder3, user, EIMAccessRole.UPDATE));
			// 読み込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, folder3, user, EIMAccessRole.READ));
			// ステータスが「公開済」であることを確認
			assertTrue(folder3.getStatus().getType().getName().equals("公開済"));
			
			List upperAttributeValList = new ArrayList();
			upperAttributeValList.add(inputStr);

			// 初期化
			resultObjList = new ArrayList(); 

			// テストメソッド実行
			resultObjList = AttributeTreeUtil.getClassifiedObjects(sess, attTree1, upperAttributeValList);			
			
			sess.commit();
			
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}
		
		// folder2が取得できるはず
		//（folder1はステータスが「編集中」、folder3は権限がないため取得できない）
		assertEquals(1, resultObjList.size());
					
		resultObj1 = (EIMObject)resultObjList.get(0);
			
		// ID
		assertEquals(folder2.getId(), resultObj1.getId());
		// name
		assertEquals(folder2.getName(), resultObj1.getName());

		/**
		 * 正常パターン
		 * 条件
		 * ・読み込み権限のみのユーザーで実行
		 * ・検索条件の属性値リストに全て値を設定（属性が１つなので１つのみ）
		 * →読み込み権限が設定されていて、ステータスが「公開済み」
		 * 　またはワークフローがないフォルダを取得する。
		 * 　→ステータスが「公開済」のフォルダとワークフローがないフォルダを
		 * 　　取得する。
		 */		
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();
									
			// folder1を「編集中」から「公開済」にステータスUPさせる
			while (!folder1.getStatus().getType().getName().equals("公開済")) {				
				folder1 = WorkFlowUtils.statusUp(sess, folder1);				
			}
	
			sess.commit();
			
			EIMUser user = UserUtils.getUserById(sess, READ_ONLY_USER_ID1);

			// ユーザー切り替え
			sess.setUser(user);
			
			// folder1
			// 書き込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, folder1, user, EIMAccessRole.UPDATE));
			// 読み込み権限があることを確認
			assertTrue(SecurityUtils.enabled(sess, folder1, user, EIMAccessRole.READ));
			// ステータスが「公開済」であることを確認
			assertTrue(folder1.getStatus().getType().getName().equals("公開済"));
			
			// folder2
			// 書き込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, folder2, user, EIMAccessRole.UPDATE));
			// 読み込み権限があることを確認
			assertTrue(SecurityUtils.enabled(sess, folder2, user, EIMAccessRole.READ));
			// ステータスがないことを確認
			assertNull(folder2.getStatus());
			
			// folder3
			// 書き込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, folder3, user, EIMAccessRole.UPDATE));
			// 読み込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, folder3, user, EIMAccessRole.READ));
			// ステータスが「公開済」であることを確認
			assertTrue(folder3.getStatus().getType().getName().equals("公開済"));

			List upperAttributeValList = new ArrayList();
			upperAttributeValList.add(inputStr);

			// 初期化
			resultObjList = new ArrayList(); 

			// テストメソッド実行
			resultObjList = AttributeTreeUtil.getClassifiedObjects(sess, attTree1, upperAttributeValList);			
			
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}

		// folder1とfolder2が取得できるはず
		//（folder3は権限がないため取得できない）
		assertEquals(2, resultObjList.size());
		
		for (int i = 0; i < resultObjList.size(); i++) {
			
			resultObj1 = (EIMObject)resultObjList.get(i);
			
			if (resultObj1.getId() == folder1.getId()) {
				// name
				assertEquals(folder1.getName(), resultObj1.getName());				
			} else if (resultObj1.getId() == folder2.getId()) {
				// name
				assertEquals(folder2.getName(), resultObj1.getName());				
			} else {
				fail("getClassifiedObjects：フォルダ読み込み権限テストNG");
			}
		}

		/**
		 * 正常パターン
		 * 条件
		 * ・対象に権限がないユーザーで実行
		 * ・検索条件の属性値リストに全て値を設定（属性が１つなので１つのみ）
		 * →何も取得できないため長さ0のリストが返却される
		 */		
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();

			EIMUser user = UserUtils.getUserById(sess, NOT_READ_ONLY_USER_ID);

			// ユーザー切り替え
			sess.setUser(user);

			// folder1
			// 書き込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, folder1, user, EIMAccessRole.UPDATE));
			// 読み込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, folder1, user, EIMAccessRole.READ));
			// ステータスが「公開済」であることを確認
			assertTrue(folder1.getStatus().getType().getName().equals("公開済"));
			
			// folder2
			// 書き込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, folder2, user, EIMAccessRole.UPDATE));
			// 読み込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, folder2, user, EIMAccessRole.READ));
			// ステータスがないことを確認
			assertNull(folder2.getStatus());
			
			// folder3
			// 書き込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, folder3, user, EIMAccessRole.UPDATE));
			// 読み込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, folder3, user, EIMAccessRole.READ));
			// ステータスが「公開済」であることを確認
			assertTrue(folder3.getStatus().getType().getName().equals("公開済"));

			List upperAttributeValList = new ArrayList();
			upperAttributeValList.add(inputStr);

			// 初期化
			resultObjList = new ArrayList(); 

			// テストメソッド実行
			resultObjList = AttributeTreeUtil.getClassifiedObjects(sess, attTree1, upperAttributeValList);			
						
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}
		
		// 長さ0を確認
		assertEquals(0, resultObjList.size());
		
		
		/* ----- 権限テスト（ドキュメント） ----- */
		/* ----- データ準備 ----- */		
		EIMObject document1 = null;
		EIMObject document2 = null;
		EIMObject document3 = null;
		
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();
			
			// ドキュメントタイプ取得
			EIMObjectType documentType1 = ObjectUtils.getObjectTypeById(sess, TARGET_DOCUMENT_TYPE_ID1);
			EIMObjectType documentType2 = ObjectUtils.getObjectTypeById(sess, TARGET_DOCUMENT_TYPE_ID2);
			EIMObjectType documentType3 = ObjectUtils.getObjectTypeById(sess, TARGET_DOCUMENT_TYPE_ID3);
			
			// ドキュメント取得
			document1 = ObjectUtils.getObjectById(sess, TARGET_DOCUMENT_OBJ_ID1);
			document2 = ObjectUtils.getObjectById(sess, TARGET_DOCUMENT_OBJ_ID2);
			document3 = ObjectUtils.getObjectById(sess, TARGET_DOCUMENT_OBJ_ID3);
			
			
			// ドキュメントに属性値の値を設定
			AppObjectUtil.setAttr(sess, document1, objAttType_str.getName(), inputStr);
			AppObjectUtil.setAttr(sess, document2, objAttType_str.getName(), inputStr);
			AppObjectUtil.setAttr(sess, document3, objAttType_str.getName(), inputStr);
			
			// 属性ツリーの分類対象を「ドキュメント」に設定
			AttributeTreeUtil.updateAttributeTree(sess, attTree1, ATT_TREE_NAME19, AppConstant.CLASSIFY_TARGET_DOCUMENT);
			
			attTree1 = AttributeTreeUtil.getAttributeTreeById(sess, attTree1.getId());
			
			sess.commit();

			document1 = AppObjectUtil.getObject(sess, documentType1.getName(), document1.getName());
			document2 = AppObjectUtil.getObject(sess, documentType2.getName(), document2.getName());
			document3 = AppObjectUtil.getObject(sess, documentType3.getName(), document3.getName());
			
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}

		/**
		 * 正常パターン
		 * 条件
		 * ・読み込み権限のみのユーザーで実行
		 * ・検索条件の属性値リストに全て値を設定（属性が１つなので１つのみ）
		 * →読み込み権限が設定されていて、ステータスが「公開済」
		 * 　またはワークフローがないドキュメントを取得する。
		 * 　→ステータスが「公開済」のドキュメントと
		 * 　　ワークフローがないドキュメントを取得する。
		 */		
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();
			
			EIMUser user = UserUtils.getUserById(sess, READ_ONLY_USER_ID1);
			
			// ユーザー切り替え
			sess.setUser(user);
			
			// document1
			// 書き込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, document1, user, EIMAccessRole.UPDATE));
			// 読み込み権限があることを確認
			assertTrue(SecurityUtils.enabled(sess, document1, user, EIMAccessRole.READ));
			// ステータスが「公開済」であることを確認
			assertTrue(document1.getStatus().getType().getName().equals("公開済"));
			
			// document2
			// 書き込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, document2, user, EIMAccessRole.UPDATE));
			// 読み込み権限があることを確認
			assertTrue(SecurityUtils.enabled(sess, document2, user, EIMAccessRole.READ));
			// ステータスがないことを確認
			assertNull(document2.getStatus());
			
			// document3
			// 書き込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, document3, user, EIMAccessRole.UPDATE));
			// 読み込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, document3, user, EIMAccessRole.READ));
			// ステータスが「公開済」であることを確認
			assertTrue(document3.getStatus().getType().getName().equals("公開済"));
			
			List upperAttributeValList = new ArrayList();
			upperAttributeValList.add(inputStr);

			// 初期化
			resultObjList = new ArrayList(); 

			// テストメソッド実行
			resultObjList = AttributeTreeUtil.getClassifiedObjects(sess, attTree1, upperAttributeValList);			
			
			sess.commit();
			
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}
		
		// document1とdocument2が取得できるはず
		//（document3は権限がないため取得できない）
		assertEquals(2, resultObjList.size());
		
		for (int i = 0; i < resultObjList.size(); i++) {
			
			resultObj1 = (EIMObject)resultObjList.get(i);
			
			if (resultObj1.getId() == document1.getId()) {
				// name
				assertEquals(document1.getName(), resultObj1.getName());				
			} else if (resultObj1.getId() == document2.getId()) {
				// name
				assertEquals(document2.getName(), resultObj1.getName());				
			} else {
				fail("getClassifiedObjects：ドキュメント読み込み権限テストNG");
			}
		}
		
		/**
		 * 正常パターン
		 * 条件
		 * ・対象に権限がないユーザーで実行
		 * ・検索条件の属性値リストに全て値を設定（属性が１つなので１つのみ）
		 * →何も取得できないため長さ0のリストが返却される
		 */	
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();

			EIMUser user = UserUtils.getUserById(sess, NOT_READ_ONLY_USER_ID);

			// ユーザー切り替え
			sess.setUser(user);

			// document1
			// 書き込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, document1, user, EIMAccessRole.UPDATE));
			// 読み込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, document1, user, EIMAccessRole.READ));
			// ステータスが「公開済」であることを確認
			assertTrue(document1.getStatus().getType().getName().equals("公開済"));
			
			// document2
			// 書き込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, document2, user, EIMAccessRole.UPDATE));
			// 読み込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, document2, user, EIMAccessRole.READ));
			// ステータスがないことを確認
			assertNull(document2.getStatus());
			
			// document3
			// 書き込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, document3, user, EIMAccessRole.UPDATE));
			// 読み込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, document3, user, EIMAccessRole.READ));
			// ステータスが「公開済」であることを確認
			assertTrue(document3.getStatus().getType().getName().equals("公開済"));

			List upperAttributeValList = new ArrayList();
			upperAttributeValList.add(inputStr);

			// 初期化
			resultObjList = new ArrayList(); 

			// テストメソッド実行
			resultObjList = AttributeTreeUtil.getClassifiedObjects(sess, attTree1, upperAttributeValList);			
						
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}
		
		// 長さ0を確認
		assertEquals(0, resultObjList.size());
		
	}
	
	
	/**
	 * AttributeTreeUtil.classifyAttributeValues(EIMSession sess, AttributeTree attTree, 
	 * 					List upperAttributeValList)のためのテスト・メソッド
	 */
	public void testClassifyAttributeValues() {
		
		AttributeTree attTree1 = null;
		AttributeTree attTree2 = null;
		
		List resultList = new ArrayList();
		
		EIMObject objParentFolder1 = null;
		EIMObject objParentFolder2 = null;
		EIMObject objChildFolder1 = null;
		
 		String resultStr = null;
 		Integer resultInt = null;
 		Date resultDate = null;
 		String resultText = null;
		
        // 属性値
		String inputStr = "iWDM";
		int inputInt = 2;
		Date inputDate = null;
		String inputText = "ja";

		/**
		 * 設定状態チェック
		 */
		if(READ_ONLY_USER_ID1 == 0 || NOT_READ_ONLY_USER_ID == 0 || TARGET_ATTR_TYPE_ID == 0
				|| TARGET_ATTR_TYPE_VALUE == null || TARGET_FOLDER_TYPE_ID1 == 0 || TARGET_FOLDER_TYPE_ID2 == 0
				|| TARGET_FOLDER_TYPE_ID3 == 0 || TARGET_DOCUMENT_TYPE_ID1 == 0 || TARGET_DOCUMENT_TYPE_ID2 == 0
				|| TARGET_DOCUMENT_TYPE_ID3 == 0 || TARGET_DOCUMENT_OBJ_ID1 == 0 || TARGET_DOCUMENT_OBJ_ID2 == 0
				|| TARGET_DOCUMENT_OBJ_ID3 == 0)
		{
			System.out.println("****************************************************************************************");
			System.out.println("このテストプログラムを動作させる前に、ソースコードの先頭に変数を設定してください");
			System.out.println("");
			System.out.println("");
			System.out.println("それぞれの変数についての説明は、ソースコードの先頭のコメントを参照してください");
			System.out.println("****************************************************************************************");
			assertFalse(true);
		}
		
		/**
		 * データ準備
		 */
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();
			
			// フォルダタイプ取得
			EIMObjectType folderType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_FOLDER"));
			
			// 子フォルダタイプ作成
			EIMObjectType childFolderType = ObjectUtils.createObjectType(sess, FOLDER_TYPE_NAME3 + date , folderType);

			// 属性タイプ作成
			// 文字列型
			objAttType_str = AttributeUtils.createAttributeType(sess, attTypeName_String7, EIMValueType.getTypeById(sess, EIMValueType.STRING));
			// 数値型
			objAttType_int = AttributeUtils.createAttributeType(sess, attTypeName_Int5, EIMValueType.getTypeById(sess, EIMValueType.INTEGER));
			// 日付型
			objAttType_date = AttributeUtils.createAttributeType(sess, attTypeName_Date5, EIMValueType.getTypeById(sess, EIMValueType.DATE));			
			// テキスト型
			objAttType_text = AttributeUtils.createAttributeType(sess, attTypeName_Text5, EIMValueType.getTypeById(sess, EIMValueType.TEXT));

			
			// フォルダタイプに属性タイプを設定
			ObjectAttributeUtils.applyAttributeType(sess, childFolderType, objAttType_str);
			ObjectAttributeUtils.applyAttributeType(sess, childFolderType, objAttType_int);
			ObjectAttributeUtils.applyAttributeType(sess, childFolderType, objAttType_date);
			ObjectAttributeUtils.applyAttributeType(sess, childFolderType, objAttType_text);
				
			/* ------ 親フォルダ1作成 ------ */
			// 親フォルダ1作成
			objParentFolder1 = ObjectUtils.createObject(sess, childFolderType, FOLDER_NAME6);
			
			/* ------ 親フォルダ2作成 ------ */
			// 親フォルダ2作成
			objParentFolder2 = ObjectUtils.createObject(sess, childFolderType, FOLDER_NAME7);
						
			// 属性ツリ1ー生成
			attTree1 = AttributeTreeUtil.createAttributeTree(sess, ATT_TREE_NAME17, AppConstant.CLASSIFY_TARGET_FOLDER);

			// オブジェクト名称を取得
			String objName = attTree1.getName();			
			
			// オブジェクト生成（属性ツリー所属属性）
			EIMObject obj1 = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName + "_0");
			EIMObject obj2 = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName + "_1");
			EIMObject obj3 = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName + "_2");
			EIMObject obj4 = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName + "_3");
			
			// 属性ツリー所属属性情報をリストに追加(全て「属性値なしを表示する」)
			List attTreeItemList = new ArrayList();
			attTreeItemList.add(new AttributeTreeItem(obj1.getId(), objAttType_str, true));
			attTreeItemList.add(new AttributeTreeItem(obj2.getId(), objAttType_int, true));
			attTreeItemList.add(new AttributeTreeItem(obj3.getId(), objAttType_date, true));
			attTreeItemList.add(new AttributeTreeItem(obj4.getId(), objAttType_text, true));			

		    // 属性ツリー所属属性を属性ツリーに設定
			AttributeTreeUtil.setTreeItems(sess, attTree1, attTreeItemList);
			attTree1.setTreeItemList(attTreeItemList);
			
			// 属性ツリ2ー生成
			attTree2 = AttributeTreeUtil.createAttributeTree(sess, ATT_TREE_NAME18, AppConstant.CLASSIFY_TARGET_FOLDER);

			// オブジェクト名称を取得
			objName = attTree1.getName();			
						
			// オブジェクト生成（属性ツリー所属属性）
			obj1 = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName + "_0");
			obj2 = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName + "_1");
			obj3 = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName + "_2");
			obj4 = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName + "_3");
			
			// 属性ツリー所属属性情報をリストに追加(全て「属性値なしを表示する」)
			attTreeItemList = new ArrayList();
			attTreeItemList.add(new AttributeTreeItem(obj1.getId(), objAttType_str, false));
			attTreeItemList.add(new AttributeTreeItem(obj2.getId(), objAttType_int, false));
			attTreeItemList.add(new AttributeTreeItem(obj3.getId(), objAttType_date, false));
			attTreeItemList.add(new AttributeTreeItem(obj4.getId(), objAttType_text, false));			

		    // 属性ツリー所属属性を属性ツリーに設定
			AttributeTreeUtil.setTreeItems(sess, attTree2, attTreeItemList);
			attTree2.setTreeItemList(attTreeItemList);

			DateFormat dfm = new SimpleDateFormat("yyyy/MM/dd");
			String dateStr = dfm.format(new Date());
			inputDate = dfm.parse(dateStr);
			
			// 親フォルダ1に全ての属性値の値を設定(全て「属性値なしを表示しない」)
			AppObjectUtil.setAttr(sess, objParentFolder1, attTypeName_String7, inputStr);
			AppObjectUtil.setAttr(sess, objParentFolder1, attTypeName_Int5, inputInt);
			AppObjectUtil.setAttr(sess, objParentFolder1, attTypeName_Date5, inputDate);
			AppObjectUtil.setAttr(sess, objParentFolder1, attTypeName_Text5, inputText);
			
			objParentFolder1 = AppObjectUtil.getObject(sess, childFolderType.getName(), objParentFolder1.getName());

			sess.commit();
			
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}

		/* ------ フォルダ ------ */
		/**
		 * 正常パターン
		 * 条件
		 * ・検索条件の属性タイプに文字列、数値、日付、テキスト型属性を含む
		 * ・検索条件の属性値リスト(文字列、数値、日付)全てに値を設定
		 * ・取得した属性値（テキスト型）は「属性値なしを表示する」
		 */		
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();
			
			List upperAttributeValList = new ArrayList();
			upperAttributeValList.add(inputStr);
			upperAttributeValList.add(new Integer(inputInt));
			upperAttributeValList.add(inputDate);
			
			// 初期化
			resultList = new ArrayList();

			// テストメソッド実行
			resultList = AttributeTreeUtil.classifyAttributeValues(sess, attTree1, upperAttributeValList);			
			
			assertEquals(1, resultList.size());
			
			resultText = (String)resultList.get(0);
			
			sess.commit();
			
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}
		
		assertEquals(inputText, resultText);
	
		/**
		 * 正常パターン
		 * 条件
		 * ・検索条件の属性タイプに文字列、数値、日付、テキスト型属性を含む
		 * ・検索条件の属性値リスト(文字列、数値)全てに値を設定
		 * ・取得した属性値（日付型）は「属性値なしを表示する」
		 */		
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();
			
			List upperAttributeValList = new ArrayList();
			upperAttributeValList.add(inputStr);
			upperAttributeValList.add(new Integer(inputInt));
			
			// 初期化
			resultList = new ArrayList();

			// テストメソッド実行
			resultList = AttributeTreeUtil.classifyAttributeValues(sess, attTree1, upperAttributeValList);			
			
			assertEquals(1, resultList.size());
			
			resultDate = (Date)resultList.get(0);
			
			sess.commit();
			
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}
		
		assertEquals(inputDate, resultDate);
		
		/**
		 * 正常パターン
		 * 条件
		 * ・検索条件の属性タイプに文字列、数値、日付、テキスト型属性を含む
		 * ・検索条件の属性値リスト(文字列)全てに値を設定
		 * ・取得した属性値（数値型）は「属性値なしを表示する」
		 */		
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();
			
			List upperAttributeValList = new ArrayList();
			upperAttributeValList.add(inputStr);
			
			// 初期化
			resultList = new ArrayList();

			// テストメソッド実行
			resultList = AttributeTreeUtil.classifyAttributeValues(sess, attTree1, upperAttributeValList);			
			
			assertEquals(1, resultList.size());
			
			resultInt = (Integer)resultList.get(0);
			
			sess.commit();
			
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}
		
		assertEquals(inputInt, resultInt.intValue());
		
		/**
		 * 正常パターン
		 * 条件
		 * ・検索条件の属性タイプに文字列、数値、日付、テキスト型属性を含む
		 * ・検索条件の属性値リストに空のリストを指定
		 * ・取得した属性値（文字列型【最上位】）は「属性値なしを表示する」
		 * 　→最上位の「値あり」＆nullの返却される
		 */		
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();
			
			List upperAttributeValList = new ArrayList();
			
			// 初期化
			resultList = new ArrayList();

			// テストメソッド実行
			resultList = AttributeTreeUtil.classifyAttributeValues(sess, attTree1, upperAttributeValList);			
						
			sess.commit();
			
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}
		
		if (resultList != null) {

			assertEquals(2, resultList.size());
			
			resultStr = (String)resultList.get(0);

			assertEquals(inputStr, resultStr);
			
			assertEquals(null, resultList.get(1));			
		}
		
		/**
		 * 正常パターン
		 * 条件
		 * ・検索条件の属性タイプに文字列、数値、日付、テキスト型属性を含む
		 * ・検索条件の属性値リスト(文字列、数値、日付)全てにnullを指定
		 * ・取得した属性値（テキスト型）nullは「属性値なしを表示する」
		 */		
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();

			List upperAttributeValList = new ArrayList();
			upperAttributeValList.add(null);
			upperAttributeValList.add(null);
			upperAttributeValList.add(null);
			
			// 初期化
			resultList = new ArrayList();
			
			// テストメソッド実行
			resultList = AttributeTreeUtil.classifyAttributeValues(sess, attTree1, upperAttributeValList);			
			
			sess.commit();
						
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}
		
		assertEquals(1, resultList.size());
		assertEquals(null, resultList.get(0));

		/**
		 * 正常パターン
		 * 条件
		 * ・検索条件の属性タイプに文字列、数値、日付、テキスト型属性を含む
		 * ・検索条件の属性値リスト(文字列、数値)全てにnullを指定
		 * ・取得した属性値（日付型）nullは「属性値なしを表示する」
		 */		
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();

			List upperAttributeValList = new ArrayList();
			upperAttributeValList.add(null);
			upperAttributeValList.add(null);

			// 初期化
			resultList = new ArrayList();
			
			// テストメソッド実行
			resultList = AttributeTreeUtil.classifyAttributeValues(sess, attTree1, upperAttributeValList);			
			
			sess.commit();
						
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}
		
		assertEquals(1, resultList.size());
		assertEquals(null, resultList.get(0));

		/**
		 * 正常パターン
		 * 条件
		 * ・検索条件の属性タイプに文字列、数値、日付、テキスト型属性を含む
		 * ・検索条件の属性値リスト(文字列)にnullを指定
		 * ・取得した属性値（数値型）nullは「属性値なしを表示する」
		 */		
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();

			List upperAttributeValList = new ArrayList();
			upperAttributeValList.add(null);

			// 初期化
			resultList = new ArrayList();
			
			// テストメソッド実行
			resultList = AttributeTreeUtil.classifyAttributeValues(sess, attTree1, upperAttributeValList);			
			
			sess.commit();
						
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}

		assertEquals(1, resultList.size());
		assertEquals(null, resultList.get(0));

		/**
		 * 正常パターン（検索ヒットなし）
		 * 条件
		 * ・検索条件の属性タイプに文字列、数値、日付、テキスト型属性を含む
		 * ・検索条件の属性値リストに全て値を設定（検索ヒットしない条件）
		 * →検索条件に該当するドキュメントがないため長さ0のリストが取得される
		 */		
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();

			// 検索ヒットしない条件設定
			List upperAttributeValList = new ArrayList();
			upperAttributeValList.add("旧製品名");
			upperAttributeValList.add(new Integer(9));
			upperAttributeValList.add(new Date());

			// 初期化
			resultList = new ArrayList();
			
			// テストメソッド実行
			resultList = AttributeTreeUtil.classifyAttributeValues(sess, attTree1, upperAttributeValList);			
			
			sess.commit();
						
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}
		
		// 長さ0のリストの返却を確認
		assertEquals(0, resultList.size());	
		
		/*----- 「属性値なしを表示しない」 -----*/
		/**
		 * 正常パターン
		 * 条件
		 * ・検索条件の属性タイプに文字列、数値、日付、テキスト型属性を含む
		 * ・検索条件の属性値リスト(文字列、数値、日付)全てに値を設定
		 * ・取得した属性値（テキスト型）は「属性値なしを表示しない」
		 */		
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();
			
			List upperAttributeValList = new ArrayList();
			upperAttributeValList.add(inputStr);
			upperAttributeValList.add(new Integer(inputInt));
			upperAttributeValList.add(inputDate);
			
			// 初期化
			resultList = new ArrayList();

			// テストメソッド実行
			resultList = AttributeTreeUtil.classifyAttributeValues(sess, attTree2, upperAttributeValList);			
			
			assertEquals(1, resultList.size());
			
			resultText = (String)resultList.get(0);
			
			sess.commit();
			
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}
		
		assertEquals(inputText, resultText);
	
		/**
		 * 正常パターン
		 * 条件
		 * ・検索条件の属性タイプに文字列、数値、日付、テキスト型属性を含む
		 * ・検索条件の属性値リスト(文字列、数値)全てに値を設定
		 * ・取得した属性値（日付型）は「属性値なしを表示しない」
		 */		
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();
			
			List upperAttributeValList = new ArrayList();
			upperAttributeValList.add(inputStr);
			upperAttributeValList.add(new Integer(inputInt));
			
			// 初期化
			resultList = new ArrayList();

			// テストメソッド実行
			resultList = AttributeTreeUtil.classifyAttributeValues(sess, attTree2, upperAttributeValList);			
			
			assertEquals(1, resultList.size());
			
			resultDate = (Date)resultList.get(0);
			
			sess.commit();
			
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}
		
		assertEquals(inputDate, resultDate);
		
		/**
		 * 正常パターン
		 * 条件
		 * ・検索条件の属性タイプに文字列、数値、日付、テキスト型属性を含む
		 * ・検索条件の属性値リスト(文字列)全てに値を設定
		 * ・取得した属性値（数値型）は「属性値なしを表示しない」
		 */		
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();
			
			List upperAttributeValList = new ArrayList();
			upperAttributeValList.add(inputStr);
			
			// 初期化
			resultList = new ArrayList();

			// テストメソッド実行
			resultList = AttributeTreeUtil.classifyAttributeValues(sess, attTree2, upperAttributeValList);			
			
			assertEquals(1, resultList.size());
			
			resultInt = (Integer)resultList.get(0);
			
			sess.commit();
			
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}
		
		assertEquals(inputInt, resultInt.intValue());
		
		/**
		 * 正常パターン
		 * 条件
		 * ・検索条件の属性タイプに文字列、数値、日付、テキスト型属性を含む
		 * ・検索条件の属性値リストに空のリストを指定
		 * ・取得した属性値（文字列型【最上位】）は「属性値なしを表示しない」
		 * 　→最上位の「値あり」が返却される
		 */		
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();
			
			List upperAttributeValList = new ArrayList();
			
			// 初期化
			resultList = new ArrayList();

			// テストメソッド実行
			resultList = AttributeTreeUtil.classifyAttributeValues(sess, attTree2, upperAttributeValList);			
						
			sess.commit();
			
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}
		
		if (resultList != null) {

			assertEquals(1, resultList.size());
			
			resultStr = (String)resultList.get(0);

			assertEquals(inputStr, resultStr);				
		}
		
		/**
		 * 正常パターン
		 * 条件
		 * ・検索条件の属性タイプに文字列、数値、日付、テキスト型属性を含む
		 * ・検索条件の属性値リスト(文字列、数値、日付)全てにnullを指定
		 * ・取得した属性値（テキスト型）nullは「属性値なしを表示しない」
		 */		
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();

			List upperAttributeValList = new ArrayList();
			upperAttributeValList.add(null);
			upperAttributeValList.add(null);
			upperAttributeValList.add(null);

			// 初期化
			resultList = new ArrayList();
			
			// テストメソッド実行
			resultList = AttributeTreeUtil.classifyAttributeValues(sess, attTree2, upperAttributeValList);			
			
			sess.commit();
						
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}

		assertEquals(0, resultList.size());

		/**
		 * 正常パターン
		 * 条件
		 * ・検索条件の属性タイプに文字列、数値、日付、テキスト型属性を含む
		 * ・検索条件の属性値リスト(文字列、数値)全てにnullを指定
		 * ・取得した属性値（日付型）nullは「属性値なしを表示しない」
		 */		
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();

			List upperAttributeValList = new ArrayList();
			upperAttributeValList.add(null);
			upperAttributeValList.add(null);

			// 初期化
			resultList = new ArrayList();
			
			// テストメソッド実行
			resultList = AttributeTreeUtil.classifyAttributeValues(sess, attTree2, upperAttributeValList);			
			
			sess.commit();
						
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}

		assertEquals(0, resultList.size());	

		/**
		 * 正常パターン
		 * 条件
		 * ・検索条件の属性タイプに文字列、数値、日付、テキスト型属性を含む
		 * ・検索条件の属性値リスト(文字列)にnullを指定
		 * ・取得した属性値（数値型）nullは「属性値なしを表示しない」
		 */		
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();

			List upperAttributeValList = new ArrayList();
			upperAttributeValList.add(null);

			// 初期化
			resultList = new ArrayList();
			
			// テストメソッド実行
			resultList = AttributeTreeUtil.classifyAttributeValues(sess, attTree2, upperAttributeValList);			
			
			sess.commit();
						
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}

		assertEquals(0, resultList.size());	

		
		/* ------ ドキュメント ------ */
		EIMObject objDocument1 = null;
		EIMObject objDocument2 = null;
		/**
		 * データ準備
		 */
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();
			
			// ドキュメントタイプ取得
			EIMObjectType documentType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT"));
			
			// 子ドキュメントタイプ作成
			EIMObjectType childDocumentType = ObjectUtils.createObjectType(sess, DOCUMENT_TYPE_NAME2 + date , documentType);
			
			// ドキュメントタイプに属性タイプを設定
			ObjectAttributeUtils.applyAttributeType(sess, childDocumentType, objAttType_str);
			ObjectAttributeUtils.applyAttributeType(sess, childDocumentType, objAttType_int);
			ObjectAttributeUtils.applyAttributeType(sess, childDocumentType, objAttType_date);
			ObjectAttributeUtils.applyAttributeType(sess, childDocumentType, objAttType_text);
	
			
			/* ------ ドキュメント1作成 ------ */
			// ドキュメント1作成
			objDocument1 = ObjectUtils.createObject(sess, childDocumentType, DOCUMENT_NAME4);
			
			/* ------ ドキュメント2作成 ------ */
			// ドキュメント2作成
			objDocument2 = ObjectUtils.createObject(sess, childDocumentType, DOCUMENT_NAME5);
			
			// ドキュメント1に全ての属性値に値を設定(全て「属性値なしを表示しない」)
			AppObjectUtil.setAttr(sess, objDocument1, attTypeName_String7, inputStr);
			AppObjectUtil.setAttr(sess, objDocument1, attTypeName_Int5, inputInt);
			AppObjectUtil.setAttr(sess, objDocument1, attTypeName_Date5, inputDate);
			AppObjectUtil.setAttr(sess, objDocument1, attTypeName_Text5, inputText);
			
			// 属性ツリーの分類対象を「ドキュメント」に変更
			AttributeTreeUtil.updateAttributeTree(sess, attTree1, ATT_TREE_NAME17, AppConstant.CLASSIFY_TARGET_DOCUMENT);
			AttributeTreeUtil.updateAttributeTree(sess, attTree2, ATT_TREE_NAME18, AppConstant.CLASSIFY_TARGET_DOCUMENT);

			sess.commit();
			
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}
		
		/**
		 * 正常パターン
		 * 条件
		 * ・検索条件の属性タイプに文字列、数値、日付、テキスト型属性を含む
		 * ・検索条件の属性値リスト(文字列、数値、日付)全てに値を設定
		 * ・取得した属性値（テキスト型）は「属性値なしを表示する」
		 */		
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();
			
			List upperAttributeValList = new ArrayList();
			upperAttributeValList.add(inputStr);
			upperAttributeValList.add(new Integer(inputInt));
			upperAttributeValList.add(inputDate);
			
			// 初期化
			resultList = new ArrayList();

			// テストメソッド実行
			resultList = AttributeTreeUtil.classifyAttributeValues(sess, attTree1, upperAttributeValList);			
			
			assertEquals(1, resultList.size());
			
			resultText = (String)resultList.get(0);
			
			sess.commit();
			
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}
		
		assertEquals(inputText, resultText);
	
		/**
		 * 正常パターン
		 * 条件
		 * ・検索条件の属性タイプに文字列、数値、日付、テキスト型属性を含む
		 * ・検索条件の属性値リスト(文字列、数値)全てに値を設定
		 * ・取得した属性値（日付型）は「属性値なしを表示する」
		 */		
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();
			
			List upperAttributeValList = new ArrayList();
			upperAttributeValList.add(inputStr);
			upperAttributeValList.add(new Integer(inputInt));
			
			// 初期化
			resultList = new ArrayList();

			// テストメソッド実行
			resultList = AttributeTreeUtil.classifyAttributeValues(sess, attTree1, upperAttributeValList);			
			
			assertEquals(1, resultList.size());
			
			resultDate = (Date)resultList.get(0);
			
			sess.commit();
			
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}
		
		assertEquals(inputDate, resultDate);
		
		/**
		 * 正常パターン
		 * 条件
		 * ・検索条件の属性タイプに文字列、数値、日付、テキスト型属性を含む
		 * ・検索条件の属性値リスト(文字列)全てに値を設定
		 * ・取得した属性値（数値型）は「属性値なしを表示する」
		 */		
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();
			
			List upperAttributeValList = new ArrayList();
			upperAttributeValList.add(inputStr);
			
			// 初期化
			resultList = new ArrayList();

			// テストメソッド実行
			resultList = AttributeTreeUtil.classifyAttributeValues(sess, attTree1, upperAttributeValList);			
			
			assertEquals(1, resultList.size());
			
			resultInt = (Integer)resultList.get(0);
			
			sess.commit();
			
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}
		
		assertEquals(inputInt, resultInt.intValue());
		
		/**
		 * 正常パターン
		 * 条件
		 * ・検索条件の属性タイプに文字列、数値、日付、テキスト型属性を含む
		 * ・検索条件の属性値リストに空のリストを指定
		 * ・取得した属性値（文字列型【最上位】）は「属性値なしを表示する」
		 * 　→最上位の「値あり」＆nullの返却される
		 *     ※この場合はnull返却されたかはは「値あり」のみの確認でOKとする。
		 */		
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();
			
			List upperAttributeValList = new ArrayList();
			
			// 初期化
			resultList = new ArrayList();

			// テストメソッド実行
			resultList = AttributeTreeUtil.classifyAttributeValues(sess, attTree1, upperAttributeValList);			
						
			sess.commit();
			
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}
		
		if (resultList != null) {

			assertEquals(2, resultList.size());
			
			resultStr = (String)resultList.get(0);

			assertEquals(inputStr, resultStr);
			
			assertEquals(null, resultList.get(1));			
		}
		
		/**
		 * 正常パターン
		 * 条件
		 * ・検索条件の属性タイプに文字列、数値、日付、テキスト型属性を含む
		 * ・検索条件の属性値リスト(文字列、数値、日付)全てにnullを指定
		 * ・取得した属性値（テキスト型）nullは「属性値なしを表示する」
		 */		
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();

			List upperAttributeValList = new ArrayList();
			upperAttributeValList.add(null);
			upperAttributeValList.add(null);
			upperAttributeValList.add(null);

			// 初期化
			resultList = new ArrayList();
			
			// テストメソッド実行
			resultList = AttributeTreeUtil.classifyAttributeValues(sess, attTree1, upperAttributeValList);			
			
			sess.commit();
						
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}

		assertEquals(1, resultList.size());
		assertEquals(null, resultList.get(0));

		/**
		 * 正常パターン
		 * 条件
		 * ・検索条件の属性タイプに文字列、数値、日付、テキスト型属性を含む
		 * ・検索条件の属性値リスト(文字列、数値)全てにnullを指定
		 * ・取得した属性値（日付型）nullは「属性値なしを表示する」
		 */		
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();

			List upperAttributeValList = new ArrayList();
			upperAttributeValList.add(null);
			upperAttributeValList.add(null);
			
			// 初期化
			resultList = new ArrayList();

			// テストメソッド実行
			resultList = AttributeTreeUtil.classifyAttributeValues(sess, attTree1, upperAttributeValList);			
			
			sess.commit();
						
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}

		assertEquals(1, resultList.size());
		assertEquals(null, resultList.get(0));

		/**
		 * 正常パターン
		 * 条件
		 * ・検索条件の属性タイプに文字列、数値、日付、テキスト型属性を含む
		 * ・検索条件の属性値リスト(文字列)にnullを指定
		 * ・取得した属性値（数値型）nullは「属性値なしを表示する」
		 */		
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();

			List upperAttributeValList = new ArrayList();
			upperAttributeValList.add(null);
			
			// 初期化
			resultList = new ArrayList();

			// テストメソッド実行
			resultList = AttributeTreeUtil.classifyAttributeValues(sess, attTree1, upperAttributeValList);			
			
			sess.commit();
						
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}

		assertEquals(1, resultList.size());
		assertEquals(null, resultList.get(0));

		/*----- 「属性値なしを表示しない」 -----*/
		/**
		 * 正常パターン
		 * 条件
		 * ・検索条件の属性タイプに文字列、数値、日付、テキスト型属性を含む
		 * ・検索条件の属性値リスト(文字列、数値、日付)全てに値を設定
		 * ・取得した属性値（テキスト型）は「属性値なしを表示しない」
		 */		
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();
			
			List upperAttributeValList = new ArrayList();
			upperAttributeValList.add(inputStr);
			upperAttributeValList.add(new Integer(inputInt));
			upperAttributeValList.add(inputDate);
			
			// 初期化
			resultList = new ArrayList();

			// テストメソッド実行
			resultList = AttributeTreeUtil.classifyAttributeValues(sess, attTree2, upperAttributeValList);			
			
			assertEquals(1, resultList.size());
			
			resultText = (String)resultList.get(0);
			
			sess.commit();
			
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}
		
		assertEquals(inputText, resultText);
	
		/**
		 * 正常パターン
		 * 条件
		 * ・検索条件の属性タイプに文字列、数値、日付、テキスト型属性を含む
		 * ・検索条件の属性値リスト(文字列、数値)全てに値を設定
		 * ・取得した属性値（日付型）は「属性値なしを表示しない」
		 */		
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();
			
			List upperAttributeValList = new ArrayList();
			upperAttributeValList.add(inputStr);
			upperAttributeValList.add(new Integer(inputInt));
			
			// 初期化
			resultList = new ArrayList();

			// テストメソッド実行
			resultList = AttributeTreeUtil.classifyAttributeValues(sess, attTree2, upperAttributeValList);			
			
			assertEquals(1, resultList.size());
			
			resultDate = (Date)resultList.get(0);
			
			sess.commit();
			
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}
		
		assertEquals(inputDate, resultDate);
		
		/**
		 * 正常パターン
		 * 条件
		 * ・検索条件の属性タイプに文字列、数値、日付、テキスト型属性を含む
		 * ・検索条件の属性値リスト(文字列)全てに値を設定
		 * ・取得した属性値（数値型）は「属性値なしを表示しない」
		 */		
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();
			
			List upperAttributeValList = new ArrayList();
			upperAttributeValList.add(inputStr);
			
			// 初期化
			resultList = new ArrayList();

			// テストメソッド実行
			resultList = AttributeTreeUtil.classifyAttributeValues(sess, attTree2, upperAttributeValList);			
			
			assertEquals(1, resultList.size());
			
			resultInt = (Integer)resultList.get(0);
			
			sess.commit();
			
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}
		
		assertEquals(inputInt, resultInt.intValue());
		
		/**
		 * 正常パターン
		 * 条件
		 * ・検索条件の属性タイプに文字列、数値、日付、テキスト型属性を含む
		 * ・検索条件の属性値リストに空のリストを指定
		 * ・取得した属性値（文字列型【最上位】）は「属性値なしを表示しない」
		 * 　→最上位の「値ありが返却される
		 */		
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();
			
			List upperAttributeValList = new ArrayList();
			
			// 初期化
			resultList = new ArrayList();

			// テストメソッド実行
			resultList = AttributeTreeUtil.classifyAttributeValues(sess, attTree2, upperAttributeValList);			
						
			sess.commit();
			
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}
		
		if (resultList != null) {

			assertEquals(1, resultList.size());
			
			resultStr = (String)resultList.get(0);

			assertEquals(inputStr, resultStr);			
		}
		
		/**
		 * 正常パターン
		 * 条件
		 * ・検索条件の属性タイプに文字列、数値、日付、テキスト型属性を含む
		 * ・検索条件の属性値リスト(文字列、数値、日付)全てにnullを指定
		 * ・取得した属性値（テキスト型）nullは「属性値なしを表示しない」
		 */		
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();

			List upperAttributeValList = new ArrayList();
			upperAttributeValList.add(null);
			upperAttributeValList.add(null);
			upperAttributeValList.add(null);

			// 初期化
			resultList = new ArrayList();
			
			// テストメソッド実行
			resultList = AttributeTreeUtil.classifyAttributeValues(sess, attTree2, upperAttributeValList);			
			
			sess.commit();
						
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}

		assertEquals(0, resultList.size());

		/**
		 * 正常パターン
		 * 条件
		 * ・検索条件の属性タイプに文字列、数値、日付、テキスト型属性を含む
		 * ・検索条件の属性値リスト(文字列、数値)全てにnullを指定
		 * ・取得した属性値（日付型）nullは「属性値なしを表示しない」
		 */		
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();

			List upperAttributeValList = new ArrayList();
			upperAttributeValList.add(null);
			upperAttributeValList.add(null);

			// 初期化
			resultList = new ArrayList();
			
			// テストメソッド実行
			resultList = AttributeTreeUtil.classifyAttributeValues(sess, attTree2, upperAttributeValList);			
			
			sess.commit();
						
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}

		assertEquals(0, resultList.size());	

		/**
		 * 正常パターン
		 * 条件
		 * ・検索条件の属性タイプに文字列、数値、日付、テキスト型属性を含む
		 * ・検索条件の属性値リスト(文字列)にnullを指定
		 * ・取得した属性値（数値型）nullは「属性値なしを表示しない」
		 */		
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();

			List upperAttributeValList = new ArrayList();
			upperAttributeValList.add(null);

			// 初期化
			resultList = new ArrayList();
			
			// テストメソッド実行
			resultList = AttributeTreeUtil.classifyAttributeValues(sess, attTree2, upperAttributeValList);			
			
			sess.commit();
						
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}

		assertEquals(0, resultList.size());	
		
		/**
		 * 例外パターン
		 * 条件
		 * ・検索条件の属性タイプに文字列、数値、日付、テキスト型属性を含む
		 * ・検索条件の属性値リストに属性ツリーに設定されている数以上の数の
		 * 　属性値を設定
		 * →例外発生
		 */	
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();

			// 指定数を属性ツリーに設定されている属性の数分指定
			List upperAttributeValList = new ArrayList();
			upperAttributeValList.add(inputStr);
			upperAttributeValList.add(new Integer(inputInt));
			upperAttributeValList.add(inputDate);
			upperAttributeValList.add(inputText);

			// 初期化
			resultList = new ArrayList();

			// テストメソッド実行
			resultList = AttributeTreeUtil.classifyAttributeValues(sess, attTree1, upperAttributeValList);			

			sess.commit();
			
			fail("classifyAttributeValues：例外がthrowされませんでした。");
			
		} catch (EIMException eime) {
			assertEquals("classifyAttributeValues：例外テスト失敗", "EIM.ERROR.COMMON.API.INVALID.ATTRIBUTE.TREE.MANY.VALUES", eime.getMessageKey());			
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}
		
		/* ----- 権限テスト（フォルダ） ----- */
		/* ----- データ準備 ----- */
		EIMObject folder1 = null;
		EIMObject folder2 = null;
		EIMObject folder3 = null;

		Connection con = null;
		Statement stmt = null;
		ResultSet rs = null;
		
		String inputStr1 = "Aえー";
		String inputStr2 = "Bびー";
		String inputStr3 = "Cしー";
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();
			
			// 前回のテストで作成したフォルダを削除
			con = sess.getDBConnection();
			stmt = con.createStatement();
			String sql = "delete from eimobj where name like " + "'フォルダ８_%'";
			rs = stmt.executeQuery(sql);
			sql = "delete from eimobj where name like " + "'フォルダ９_%'";
			rs = stmt.executeQuery(sql);
			sql = "delete from eimobj where name like " + "'フォルダ１０_%'";
			rs = stmt.executeQuery(sql);
			sql = "delete from eimobj where name like " + "'フォルダ１１_%'";
			rs = stmt.executeQuery(sql);
			sql = "delete from eimobj where name like " + "'フォルダ１２_%'";
			rs = stmt.executeQuery(sql);
			sql = "delete from eimobj where name like " + "'フォルダ１３_%'";
			rs = stmt.executeQuery(sql);
			
			/* フォルダタイプ取得 */
			EIMObjectType folderType1 = ObjectUtils.getObjectTypeById(sess, TARGET_FOLDER_TYPE_ID1);
			// （ワークフローなし）
			EIMObjectType folderType2 = ObjectUtils.getObjectTypeById(sess, TARGET_FOLDER_TYPE_ID2);
			EIMObjectType folderType3 = ObjectUtils.getObjectTypeById(sess, TARGET_FOLDER_TYPE_ID3);

			// 属性タイプ取得(文字列型)
			objAttType_str = AttributeUtils.getAttributeTypeById(sess, TARGET_ATTR_TYPE_ID);

			/* フォルダ作成 */
			folder1 = ObjectUtils.createObject(sess, folderType1, FOLDER_NAME11);
			// （ワークフローなし）
			folder2 = ObjectUtils.createObject(sess, folderType2, FOLDER_NAME12);
			folder3 = ObjectUtils.createObject(sess, folderType3, FOLDER_NAME13);
			
			/* セキュリティ設定 */
			SecurityUtils.setSecurity(sess, folder1, SecurityUtils.getSecurityById(sess, folderType1.getSecurityId()));
			SecurityUtils.setSecurity(sess, folder2, SecurityUtils.getSecurityById(sess, folderType2.getSecurityId()));
			SecurityUtils.setSecurity(sess, folder3, SecurityUtils.getSecurityById(sess, folderType3.getSecurityId()));
					
			// 属性ツリー生成
			attTree1 = AttributeTreeUtil.createAttributeTree(sess, ATT_TREE_NAME20, AppConstant.CLASSIFY_TARGET_FOLDER);

			// オブジェクト名称を取得
			String objName = attTree1.getName();			
			
			// オブジェクト生成（属性ツリー所属属性）
			EIMObject obj = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName + "_0");
			
			// 属性ツリー所属属性情報をリストに追加(全て「属性値なしを表示する」)
			List attTreeItemList = new ArrayList();
			attTreeItemList.add(new AttributeTreeItem(obj.getId(), objAttType_str, true));

		    // 属性ツリー所属属性を属性ツリーに設定
			AttributeTreeUtil.setTreeItems(sess, attTree1, attTreeItemList);
			attTree1.setTreeItemList(attTreeItemList);

			// フォルダに属性値の値を設定
			AppObjectUtil.setAttr(sess, folder1, objAttType_str.getName(), inputStr1);
			AppObjectUtil.setAttr(sess, folder2, objAttType_str.getName(), inputStr2);
			AppObjectUtil.setAttr(sess, folder3, objAttType_str.getName(), inputStr3);
			
			folder1 = AppObjectUtil.getObject(sess, folderType1.getName(), folder1.getName());
			folder2 = AppObjectUtil.getObject(sess, folderType2.getName(), folder2.getName());
			folder3 = AppObjectUtil.getObject(sess, folderType3.getName(), folder3.getName());

			// folder3を「編集中」から「公開済」にステータスUPさせる
			while (!folder3.getStatus().getType().getName().equals("公開済")) {				
				folder3 = WorkFlowUtils.statusUp(sess, folder3);				
			}
			
			sess.commit();
			
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				if (rs != null) {
					rs.close();
				}
				if (stmt != null) {
					stmt.close();
				}
				sess.close();	
			} catch (Exception e) {
				e.printStackTrace();
			}			
			
		}
		
		/**
		 * 正常パターン
		 * 条件
		 * ・読み込み権限のみのユーザーで実行
		 * ・検索条件の属性値リストに空を設定
		 * →読み込み権限が設定されていて、ステータスが「公開済」
		 * 　またはワークフローがないフォルダの属性値を取得する。
		 * 　→ワークフローがないフォルダの属性値のみを取得する。
		 */		
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();
			
			EIMUser user = UserUtils.getUserById(sess, READ_ONLY_USER_ID1);
			
			// ユーザー切り替え
			sess.setUser(user);
			
			// folder1
			// 書き込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, folder1, user, EIMAccessRole.UPDATE));
			// 読み込み権限があることを確認
			assertTrue(SecurityUtils.enabled(sess, folder1, user, EIMAccessRole.READ));
			// ステータスが「公開済」でないことを確認
		    assertFalse(folder1.getStatus().getType().getName().equals("公開済"));
			// ステータスが「編集中」であることを確認
			assertTrue(folder1.getStatus().getType().getName().equals("編集中"));
			
			// folder2
			// 書き込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, folder2, user, EIMAccessRole.UPDATE));
			// 読み込み権限があることを確認
			assertTrue(SecurityUtils.enabled(sess, folder2, user, EIMAccessRole.READ));
			// ステータスがないことを確認
			assertNull(folder2.getStatus());
			
			// folder3
			// 書き込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, folder3, user, EIMAccessRole.UPDATE));
			// 読み込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, folder3, user, EIMAccessRole.READ));
			// ステータスが「公開済」であることを確認
			assertTrue(folder3.getStatus().getType().getName().equals("公開済"));
			
			// 空のリスト
			List upperAttributeValList = new ArrayList();

			// 初期化
			resultList = new ArrayList(); 

			// テストメソッド実行
			resultList = AttributeTreeUtil.classifyAttributeValues(sess, attTree1, upperAttributeValList);			
			
			sess.commit();
			
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}
		
		// folder2の属性値とnullが取得できるはず
		//（folder1はステータスが「編集中」、folder3は権限がないため取得できない）
		assertEquals(2, resultList.size());
					
		resultStr = (String)resultList.get(0);
		
		// 属性値
		assertEquals(inputStr2, resultStr);

		resultStr = (String)resultList.get(1);
		
		// 属性値
		assertNull(resultStr);
		
		/**
		 * 正常パターン
		 * 条件
		 * ・読み込み権限のみのユーザーで実行
		 * ・検索条件の属性値リストに全て値を設定
		 * →読み込み権限が設定されていて、ステータスが「公開済み」
		 * 　またはワークフローがないフォルダの属性値を取得する。
		 * 　→ステータスが「公開済」のフォルダとワークフローがないフォルダの
		 * 　　属性値を取得する。
		 */		
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();
									
			// folder1を「編集中」から「公開済」にステータスUPさせる
			while (!folder1.getStatus().getType().getName().equals("公開済")) {				
				folder1 = WorkFlowUtils.statusUp(sess, folder1);				
			}
	
			sess.commit();
			
			EIMUser user = UserUtils.getUserById(sess, READ_ONLY_USER_ID1);

			// ユーザー切り替え
			sess.setUser(user);
			
			// folder1
			// 書き込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, folder1, user, EIMAccessRole.UPDATE));
			// 読み込み権限があることを確認
			assertTrue(SecurityUtils.enabled(sess, folder1, user, EIMAccessRole.READ));
			// ステータスが「公開済」であることを確認
			assertTrue(folder1.getStatus().getType().getName().equals("公開済"));
			
			// folder2
			// 書き込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, folder2, user, EIMAccessRole.UPDATE));
			// 読み込み権限があることを確認
			assertTrue(SecurityUtils.enabled(sess, folder2, user, EIMAccessRole.READ));
			// ステータスがないことを確認
			assertNull(folder2.getStatus());
			
			// folder3
			// 書き込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, folder3, user, EIMAccessRole.UPDATE));
			// 読み込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, folder3, user, EIMAccessRole.READ));
			// ステータスが「公開済」であることを確認
			assertTrue(folder3.getStatus().getType().getName().equals("公開済"));

			// 空のリスト
			List upperAttributeValList = new ArrayList();

			// 初期化
			resultList = new ArrayList(); 

			// テストメソッド実行
			resultList = AttributeTreeUtil.classifyAttributeValues(sess, attTree1, upperAttributeValList);			
			
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}

		// folder1とfolder2の属性値とnullが取得できるはず
		//（folder3は権限がないため取得できない）
		assertEquals(3, resultList.size());
		
		for (int i = 0; i < resultList.size(); i++) {
			
			resultStr = (String)resultList.get(i);

			// fail文で確認
			if (resultStr == null) {
				continue;
			} else if (resultStr.equals(inputStr1)) {
				continue;
			} else if (resultStr.equals(inputStr2)) {
				continue;
			} else {
				fail("classifyAttributeValues：フォルダ読み込み権限テストNG");
			}
			
		}

		/**
		 * 正常パターン
		 * 条件
		 * ・対象に権限がないユーザーで実行
		 * ・検索条件の属性値リストに空を設定
		 * →何も取得できないため長さ1のリストが返却される
		 * 　該当のセキュリティを持った属性を持たないフォルダも
		 * 　取得される。
		 */		
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();

			EIMUser user = UserUtils.getUserById(sess, NOT_READ_ONLY_USER_ID);

			// ユーザー切り替え
			sess.setUser(user);

			// folder1
			// 書き込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, folder1, user, EIMAccessRole.UPDATE));
			// 読み込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, folder1, user, EIMAccessRole.READ));
			// ステータスが「公開済」であることを確認
			assertTrue(folder1.getStatus().getType().getName().equals("公開済"));
			
			// folder2
			// 書き込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, folder2, user, EIMAccessRole.UPDATE));
			// 読み込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, folder2, user, EIMAccessRole.READ));
			// ステータスがないことを確認
			assertNull(folder2.getStatus());
			
			// folder3
			// 書き込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, folder3, user, EIMAccessRole.UPDATE));
			// 読み込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, folder3, user, EIMAccessRole.READ));
			// ステータスが「公開済」であることを確認
			assertTrue(folder3.getStatus().getType().getName().equals("公開済"));

			// 空のリスト
			List upperAttributeValList = new ArrayList();

			// 初期化
			resultList = new ArrayList(); 

			// テストメソッド実行
			resultList = AttributeTreeUtil.classifyAttributeValues(sess, attTree1, upperAttributeValList);			
						
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}
		
		// 長さ1を確認(null)
		assertEquals(1, resultList.size());
		assertNull(resultList.get(0));
		
		
		/* ----- 権限テスト（ドキュメント） ----- */
		/* ----- データ準備 ----- */		
		EIMObject document1 = null;
		EIMObject document2 = null;
		EIMObject document3 = null;
		
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();
			
			// ドキュメントタイプ取得
			EIMObjectType documentType1 = ObjectUtils.getObjectTypeById(sess, TARGET_DOCUMENT_TYPE_ID1);
			EIMObjectType documentType2 = ObjectUtils.getObjectTypeById(sess, TARGET_DOCUMENT_TYPE_ID2);
			EIMObjectType documentType3 = ObjectUtils.getObjectTypeById(sess, TARGET_DOCUMENT_TYPE_ID3);
			
			// ドキュメント取得
			document1 = ObjectUtils.getObjectById(sess, TARGET_DOCUMENT_OBJ_ID1);
			document2 = ObjectUtils.getObjectById(sess, TARGET_DOCUMENT_OBJ_ID2);
			document3 = ObjectUtils.getObjectById(sess, TARGET_DOCUMENT_OBJ_ID3);
			
			// ドキュメントに属性値の値を設定
			AppObjectUtil.setAttr(sess, document1, objAttType_str.getName(), inputStr1);
			AppObjectUtil.setAttr(sess, document2, objAttType_str.getName(), inputStr2);
			AppObjectUtil.setAttr(sess, document3, objAttType_str.getName(), inputStr3);
			
			// 属性ツリーの分類対象を「ドキュメント」に設定
			AttributeTreeUtil.updateAttributeTree(sess, attTree1, ATT_TREE_NAME20, AppConstant.CLASSIFY_TARGET_DOCUMENT);

			attTree1 = AttributeTreeUtil.getAttributeTreeById(sess, attTree1.getId());
			
			sess.commit();

			document1 = AppObjectUtil.getObject(sess, document1.getName(), document1.getName());
			document2 = AppObjectUtil.getObject(sess, document2.getName(), document2.getName());
			document3 = AppObjectUtil.getObject(sess, document3.getName(), document3.getName());
			
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}

		/**
		 * 正常パターン
		 * 条件
		 * ・読み込み権限のみのユーザーで実行
		 * ・検索条件の属性値リストに空を設定
		 * →読み込み権限が設定されていて、ステータスが「公開済」
		 * 　またはワークフローがないドキュメントの属性値を取得する。
		 * 　→ステータスが「公開済」のドキュメントと
		 * 　　ワークフローがないドキュメントの属性値を取得する。
		 */		
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();
			
			EIMUser user = UserUtils.getUserById(sess, READ_ONLY_USER_ID1);
			
			// ユーザー切り替え
			sess.setUser(user);
			
			// document1
			// 書き込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, document1, user, EIMAccessRole.UPDATE));
			// 読み込み権限があることを確認
			assertTrue(SecurityUtils.enabled(sess, document1, user, EIMAccessRole.READ));
			// ステータスが「公開済」であることを確認
			assertTrue(document1.getStatus().getType().getName().equals("公開済"));
			
			// document2
			// 書き込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, document2, user, EIMAccessRole.UPDATE));
			// 読み込み権限があることを確認
			assertTrue(SecurityUtils.enabled(sess, document2, user, EIMAccessRole.READ));
			// ステータスがないことを確認
			assertNull(document2.getStatus());
			
			// document3
			// 書き込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, document3, user, EIMAccessRole.UPDATE));
			// 読み込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, document3, user, EIMAccessRole.READ));
			// ステータスが「公開済」であることを確認
			assertTrue(document3.getStatus().getType().getName().equals("公開済"));
			
			// 空のリスト
			List upperAttributeValList = new ArrayList();

			// 初期化
			resultList = new ArrayList(); 

			// テストメソッド実行
			resultList = AttributeTreeUtil.classifyAttributeValues(sess, attTree1, upperAttributeValList);			
			
			sess.commit();
			
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}
		
		// folder1とfolder2の属性値が取得できるはず
		//（folder3は権限がないため取得できない）
		assertEquals(3, resultList.size());
		
		for (int i = 0; i < resultList.size(); i++) {
			
			resultStr = (String)resultList.get(i);
			
			// fail文で確認
			if (resultStr == null) {
				continue;
			} else if (resultStr.equals(inputStr1)) {
				continue;
			} else if (resultStr.equals(inputStr2)) {
				continue;
			} else {
				fail("classifyAttributeValues：ドキュメント読み込み権限テストNG");
			}
			
		}

		
		/**
		 * 正常パターン
		 * 条件
		 * ・対象に権限がないユーザーで実行
		 * ・検索条件の属性値リストに全て値を設定（属性が１つなので１つのみ）
		 * →何も取得できないため長さ1のリストが返却される
		 * 　該当のセキュリティを持った属性を持たないドキュメントも
		 * 　取得される。
		 */
		
		try {
			
			sess = new EIMSession();
			
			sess.setConsole();

			EIMUser user = UserUtils.getUserById(sess, NOT_READ_ONLY_USER_ID);

			// ユーザー切り替え
			sess.setUser(user);

			// document1
			// 書き込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, document1, user, EIMAccessRole.UPDATE));
			// 読み込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, document1, user, EIMAccessRole.READ));
			// ステータスが「公開済」であることを確認
			assertTrue(document1.getStatus().getType().getName().equals("公開済"));
			
			// document2
			// 書き込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, document2, user, EIMAccessRole.UPDATE));
			// 読み込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, document2, user, EIMAccessRole.READ));
			// ステータスがないことを確認
			assertNull(document2.getStatus());
			
			// document3
			// 書き込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, document3, user, EIMAccessRole.UPDATE));
			// 読み込み権限がないことを確認
			assertFalse(SecurityUtils.enabled(sess, document3, user, EIMAccessRole.READ));
			// ステータスが「公開済」であることを確認
			assertTrue(document3.getStatus().getType().getName().equals("公開済"));

			// 空のリスト
			List upperAttributeValList = new ArrayList();

			// 初期化
			resultList = new ArrayList(); 

			// テストメソッド実行
			resultList = AttributeTreeUtil.classifyAttributeValues(sess, attTree1, upperAttributeValList);			
						
		} catch (EIMException eime) {
			eime.printStackTrace();
		} catch (Exception e) {
			e.getStackTrace();
		} finally {
			try {
				sess.close();
			} catch (Exception e) {
				e.printStackTrace();
			}		
		}
		
		// 長さ1を確認
		assertEquals(1, resultList.size());
		assertNull(resultList.get(0));
		
	}		
}
