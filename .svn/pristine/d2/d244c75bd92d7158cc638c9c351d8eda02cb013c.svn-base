package common.util;

import java.util.*;

import common.bo.AttributeUpdater;
import common.bo.AttributeUpdaterItem;

import junit.framework.TestCase;
import eim.bo.*;
import eim.util.*;
import eim.net.EIMSession;
import eimtest.util.TestSessionUtil;

/**
 * 
 * AttributeUtilのテストクラス
 * 
 */
public class AttributeUtilTest extends TestCase {

	// ★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★
	// ★　　以下の項目はDB環境に合わせて事前に書き換えておく必要があります　　★
	// ★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★
	
	/* テスト前設定項目 ここから------------------------------------------ */
	
	/** テスト結果のコンソール出力有無 */
	private boolean _viewTestData = true;
	
	/** (複数値)属性名称(文字列) */
	private static String ATTR_NAME_STR = "文字列";
	
	/** (複数値)属性名称(数値型) */
	private static String ATTR_NAME_INT = "数値";
	
	/** (複数値)属性名称(日付型) */
	private static String ATTR_NAME_DATE = "日付";
	
	/** (複数値)属性名称(テキスト型) */
	private static String ATTR_NAME_TEXT = "テキスト";

	/** (単数値)属性名称(文字列) */
	private static String ATTR_NAME_STR_ONE = "文字列(単)";
	
	/** (単数値)属性名称(数値型) */
	private static String ATTR_NAME_INT_ONE = "数値(単)";
	
	/** (単数値)属性名称(日付型) */
	private static String ATTR_NAME_DATE_ONE = "日付(単)";
	
	/** (単数値)属性名称(テキスト型) */
	private static String ATTR_NAME_TEXT_ONE = "テキスト(単)";
	
	/** フォルダのオブジェクトタイプ専用の属性 */
	private static String ATTR_NAME_BELONG_ONLY_FOLDER = "フォルダ専用属性";
	
	/** WF付きフォルダのタイプ名称1 */
	private static String WF_FOLDER_TYPE_NAME1 = "WF付きフォルダ1";

	/** フォルダのタイプ名称1 */
	private static String FOLDER_TYPE_NAME1 = "フォルダ";

	/** WF付きドキュメントのタイプ名称1 */
	private static String WF_DOCUMENT_TYPE_NAME1 = "検査ドキュメント";
	
	/** ドキュメントのタイプ名称1 */
	private static String DOCUMENT_TYPE_NAME1 = "ドキュメント";
	
	/** セキュリティID 1 */
	private static String SECURITY_NAME1 = "セキュリティ1";
	
	/** セキュリティID 2 */
	private static String SECURITY_NAME2 = "セキュリティ2";
	
	/** セキュリティID 3 */
	private static String SECURITY_NAME3 = "セキュリティ3";

	/** セキュリティ - SYSTEM */
	private static String SECURITY_NAME_SYSTEM = "system";

	/** ユーザID (セキュリティID 1に権限を持たないユーザ) */
	private static String NON_AUTH_SEC1_USER_CODE = "nonauth1";
	
	/** ユーザID (セキュリティID 2に権限を持たないユーザ) */
	private static String NON_AUTH_SEC2_USER_CODE = "nonauth2";

	/* テスト前設定項目 ここまで------------------------------------------ */
	
	
	/** 重複名称防止に使用 */
	private static Date date = null;

	/** フォルダ名称1 */
	private static String FOLDER_NAME1 = null;;
	/** フォルダ名称21 */
	private static String FOLDER_NAME21 = null;;
	/** フォルダ名称22 */
	private static String FOLDER_NAME22 = null;;
	/** フォルダ名称3 */	
	private static String FOLDER_NAME3 = null;;
	/** フォルダ名称A */
	private static String FOLDER_NAME_A = null;;
	/** フォルダ名称B */
	private static String FOLDER_NAME_B = null;;
	/** フォルダ名称C */
	private static String FOLDER_NAME_C = null;;
	/** WF付きフォルダ名称1 */
	private static String WF_FOLDER_NAME_1 = null;;
	
	/** ドキュメント名称1 */
	private static String DOC_NAME1 = null;;
	/** WF付きドキュメント名称1 */
	private static String WF_DOCUMENT_NAME_1 = null;;
	
	/** 属性(文字列)の値1 */
	private static String[] ATTR_VALUE_STR1 = {"文字列1"};
	/** 属性(文字列)の値2 */
	private static String[] ATTR_VALUE_STR2 = {"文字列2"};
	
	/** 属性(文字列)の値1(単数) */
	private static String ATTR_VALUE_STR1_ONE = "(単数)文字列1";
	/** 属性(文字列)の値2(単数) */
	private static String ATTR_VALUE_STR2_ONE = "(単数)文字列2";

	/** 属性(数値)の値1 */
	private static int[] ATTR_VALUE_INT1 = {1};
	/** 属性(数値)の値2 */
	private static int[] ATTR_VALUE_INT2 = {2};
	
	/** 属性(数値)の値1(単数) */
	private static int ATTR_VALUE_INT1_ONE = 1001;
	/** 属性(数値)の値2(単数) */
	private static int ATTR_VALUE_INT2_ONE = 1002;
	
	/** 属性(日付)の値1 */
	private static Date[] ATTR_VALUE_DATE1 = {new Date(100000000)};
	/** 属性(日付)の値2 */
	private static Date[] ATTR_VALUE_DATE2 = {new Date(200000000)};

	/** 属性(日付)の値1(単数) */
	private static Date ATTR_VALUE_DATE1_ONE = new Date(400000000);
	/** 属性(日付)の値2(単数) */
	private static Date ATTR_VALUE_DATE2_ONE = new Date(500000000);
	
	/** 属性(テキスト)の値1 */
	private static String[] ATTR_VALUE_TEXT1 = {"テキスト1"};
	/** 属性(テキスト)の値2 */
	private static String[] ATTR_VALUE_TEXT2 = {"テキスト2"};

	/** 属性(テキスト)の値1(単数) */
	private static String ATTR_VALUE_TEXT1_ONE = "(単)テキスト1";
	/** 属性(テキスト)の値2(単数) */
	private static String ATTR_VALUE_TEXT2_ONE = "(単)テキスト2";
	
	/** 属性(文字列)の値(文字列)1 */
	private static String[] ATTR_VALUE_STR_BY_STRING1 = {"EIMANAGER", "かきくけこ"};
	/** 属性(文字列)の値(文字列)2 */
	private static String[] ATTR_VALUE_STR_BY_STRING2 = {"CTC", "ITOCHU"};
	/** 属性(文字列)の値(文字列)1(単数) */
	private static String[] ATTR_VALUE_STR_BY_STRING1_ONE = {"EIMANAGER V20"};
	/** 属性(文字列)の値(文字列)2(単数) */
	private static String[] ATTR_VALUE_STR_BY_STRING2_ONE = {"eFiles"};

	/** 属性(数値)の値(文字列)1 */
	private static String[] ATTR_VALUE_INT_BY_STRING1 = {"10000", "9999"};
	/** 属性(数値)の値(文字列)1(単数) */
	private static String[] ATTR_VALUE_INT_BY_STRING1_ONE = {"777"};

	/** 属性(日付)の値(文字列)1 */
	private static String[] ATTR_VALUE_DATE_BY_STRING1 = {"2008-01-01", "2007-12-31"};
	/** 属性(日付)の値(文字列)1(単数) */
	private static String[] ATTR_VALUE_DATE_BY_STRING1_ONE = {"1999-01-01"};

	/** 属性(テキスト)の値(文字列)1 */
	private static String[] ATTR_VALUE_TEXT_BY_STRING1 = {"てきすと3", "てきすと4"};
	/** 属性(テキスト)の値(文字列)1(単数) */
	private static String[] ATTR_VALUE_TEXT_BY_STRING1_ONE = {"てきすと5"};

	/** 属性の値がブランク */
	private static String[] ATTR_VALUE_BLANK = {""};
	
	/** DB環境準備OKフラグ */
	private boolean isPreparationOK = false;
	
	private int attrTypeStrId = 0;
	private int attrTypeIntId = 0;
	private int attrTypeDateId = 0;
	private int attrTypeTextId = 0;
	
	private int attrTypeStrOneId = 0;
	private int attrTypeIntOneId = 0;
	private int attrTypeDateOneId = 0;
	private int attrTypeTextOneId = 0;
	
	private int attrTypeOnlyFolderId = 0;
	
	
	/**
	 * テスト環境の事前チェック
	 */
	protected void setUp() throws Exception{

		EIMSession sess = null;
		
		try {
			sess = TestSessionUtil.createEIMSession();
			setEIMSessionForUnitTest(sess);
			
			HashSet folderAttrSet = new HashSet();
			HashSet docAttrSet = new HashSet();
			
			List folderTypeAttrList = ObjectAttributeUtils.getAttributeTypeList(sess, ObjectUtils.getObjectTypeByName(sess, FOLDER_TYPE_NAME1));
			for (Iterator iter = folderTypeAttrList.iterator(); iter.hasNext();) {
				EIMAttributeType type = (EIMAttributeType) iter.next();
				folderAttrSet.add(new Integer(type.getId()));
			}
			List docTypeAttrList = ObjectAttributeUtils.getAttributeTypeList(sess, ObjectUtils.getObjectTypeByName(sess, DOCUMENT_TYPE_NAME1));
			for (Iterator iter = docTypeAttrList.iterator(); iter.hasNext();) {
				EIMAttributeType type = (EIMAttributeType) iter.next();
				docAttrSet.add(new Integer(type.getId()));
			}
			
			EIMAttributeType attrTypeStr = AttributeUtils.getAttributeTypeByName(sess, ATTR_NAME_STR);
			if (attrTypeStr == null) {
				System.out.println("属性[" + ATTR_NAME_STR + "]が存在しませんでした。DBに登録済みの属性名称を ATTR_NAME_STR に設定してからJUnitを実行して下さい。");
				return;
			} else if (!folderAttrSet.contains(new Integer(attrTypeStr.getId())) || !docAttrSet.contains(new Integer(attrTypeStr.getId()))) {
				System.out.println("属性[" + ATTR_NAME_STR + "]をドキュメントタイプ、フォルダタイプの属性として設定してからJUnitを実行して下さい。。");
				return;
			} else {
				attrTypeStrId = attrTypeStr.getId();
			}
			
			EIMAttributeType attrTypeInt = AttributeUtils.getAttributeTypeByName(sess, ATTR_NAME_INT);
			if (attrTypeInt == null) {
				System.out.println("属性[" + ATTR_NAME_INT + "]が存在しませんでした。DBに登録済みの属性名称を ATTR_NAME_INT に設定してからJUnitを実行して下さい。");
				return;
			} else if (!folderAttrSet.contains(new Integer(attrTypeInt.getId())) || !docAttrSet.contains(new Integer(attrTypeInt.getId()))) {
				System.out.println("属性[" + ATTR_NAME_INT + "]をドキュメントタイプ、フォルダタイプの属性として設定してからJUnitを実行して下さい。。");
				return;
			} else {
				attrTypeIntId = attrTypeInt.getId();
			}

			EIMAttributeType attrTypeDate = AttributeUtils.getAttributeTypeByName(sess, ATTR_NAME_DATE);
			if (attrTypeDate == null) {
				System.out.println("属性[" + ATTR_NAME_DATE + "]が存在しませんでした。DBに登録済みの属性名称を ATTR_NAME_DATE に設定してからJUnitを実行して下さい。");
				return;
			} else if (!folderAttrSet.contains(new Integer(attrTypeDate.getId())) || !docAttrSet.contains(new Integer(attrTypeDate.getId()))) {
				System.out.println("属性[" + ATTR_NAME_DATE + "]をドキュメントタイプ、フォルダタイプの属性として設定してからJUnitを実行して下さい。。");
				return;
			} else {
				attrTypeDateId = attrTypeDate.getId();
			}

			EIMAttributeType attrTypeText = AttributeUtils.getAttributeTypeByName(sess, ATTR_NAME_TEXT);
			if (attrTypeText == null) {
				System.out.println("属性[" + ATTR_NAME_TEXT + "]が存在しませんでした。DBに登録済みの属性名称を ATTR_NAME_TEXT に設定してからJUnitを実行して下さい。");
				return;
			} else if (!folderAttrSet.contains(new Integer(attrTypeText.getId())) || !docAttrSet.contains(new Integer(attrTypeText.getId()))) {
				System.out.println("属性[" + ATTR_NAME_TEXT + "]をドキュメントタイプ、フォルダタイプの属性として設定してからJUnitを実行して下さい。。");
				return;
			} else {
				attrTypeTextId = attrTypeText.getId();
			}

			EIMAttributeType attrTypeStrOne = AttributeUtils.getAttributeTypeByName(sess, ATTR_NAME_STR_ONE);
			if (attrTypeStrOne == null) {
				System.out.println("属性[" + ATTR_NAME_STR_ONE + "]が存在しませんでした。DBに登録済みの属性名称を ATTR_NAME_STR_ONE に設定してからJUnitを実行して下さい。");
				return;
			} else if (!folderAttrSet.contains(new Integer(attrTypeStrOne.getId())) || !docAttrSet.contains(new Integer(attrTypeStrOne.getId()))) {
				System.out.println("属性[" + ATTR_NAME_STR_ONE + "]をドキュメントタイプ、フォルダタイプの属性として設定してからJUnitを実行して下さい。。");
				return;
			} else {
				attrTypeStrOneId = attrTypeStrOne.getId();
			}
			
			EIMAttributeType attrTypeIntOne = AttributeUtils.getAttributeTypeByName(sess, ATTR_NAME_INT_ONE);
			if (attrTypeIntOne == null) {
				System.out.println("属性[" + ATTR_NAME_INT_ONE + "]が存在しませんでした。DBに登録済みの属性名称を ATTR_NAME_INT_ONE に設定してからJUnitを実行して下さい。");
				return;
			} else if (!folderAttrSet.contains(new Integer(attrTypeIntOne.getId())) || !docAttrSet.contains(new Integer(attrTypeIntOne.getId()))) {
				System.out.println("属性[" + ATTR_NAME_INT_ONE + "]をドキュメントタイプ、フォルダタイプの属性として設定してからJUnitを実行して下さい。。");
				return;
			} else {
				attrTypeIntOneId = attrTypeIntOne.getId();
			}

			EIMAttributeType attrTypeDateOne = AttributeUtils.getAttributeTypeByName(sess, ATTR_NAME_DATE_ONE);
			if (attrTypeDateOne == null) {
				System.out.println("属性[" + ATTR_NAME_DATE_ONE + "]が存在しませんでした。DBに登録済みの属性名称を ATTR_NAME_DATE_ONE に設定してからJUnitを実行して下さい。");
				return;
			} else if (!folderAttrSet.contains(new Integer(attrTypeDateOne.getId())) || !docAttrSet.contains(new Integer(attrTypeDateOne.getId()))) {
				System.out.println("属性[" + ATTR_NAME_DATE_ONE + "]をドキュメントタイプ、フォルダタイプの属性として設定してからJUnitを実行して下さい。。");
				return;
			} else {
				attrTypeDateOneId = attrTypeDateOne.getId();
			}

			EIMAttributeType attrTypeTextOne = AttributeUtils.getAttributeTypeByName(sess, ATTR_NAME_TEXT_ONE);
			if (attrTypeTextOne == null) {
				System.out.println("属性[" + ATTR_NAME_TEXT_ONE + "]が存在しませんでした。DBに登録済みの属性名称を ATTR_NAME_TEXT_ONE に設定してからJUnitを実行して下さい。");
				return;
			} else if (!folderAttrSet.contains(new Integer(attrTypeTextOne.getId())) || !docAttrSet.contains(new Integer(attrTypeTextOne.getId()))) {
				System.out.println("属性[" + ATTR_NAME_TEXT_ONE + "]をドキュメントタイプ、フォルダタイプの属性として設定してからJUnitを実行して下さい。。");
				return;
			} else {
				attrTypeTextOneId = attrTypeTextOne.getId();
			}

			
			EIMAttributeType attrTypeOnlyFolder = AttributeUtils.getAttributeTypeByName(sess, ATTR_NAME_BELONG_ONLY_FOLDER);
			if (attrTypeOnlyFolder == null) {
				System.out.println("属性[" + ATTR_NAME_BELONG_ONLY_FOLDER + "]が存在しませんでした。DBに登録済みの属性名称を ATTR_NAME_BELONG_ONLY_FOLDER に設定してからJUnitを実行して下さい。");
				return;
			} else if (folderAttrSet.contains(new Integer(attrTypeTextOne.getId())) && !docAttrSet.contains(new Integer(attrTypeTextOne.getId()))) {
				System.out.println("属性[" + ATTR_NAME_BELONG_ONLY_FOLDER + "]をフォルダタイプのみに属する属性として設定してからJUnitを実行して下さい。。");
				return;
			} else {
				attrTypeOnlyFolderId = attrTypeOnlyFolder.getId();
			}
			
			EIMObjectType objTypeWfFolder = ObjectUtils.getObjectTypeByName(sess, WF_FOLDER_TYPE_NAME1);
			if (objTypeWfFolder == null) {
				System.out.println("ワークフロー付きフォルダのオブジェクトタイプ[" + WF_FOLDER_TYPE_NAME1 + "]が存在しませんでした。DBに登録済みのオブジェクトタイプ名称を WF_FOLDER_TYPE_NAME1 に設定してからJUnitを実行して下さい。");
				return;
			}

			EIMObjectType objTypeWfDocument = ObjectUtils.getObjectTypeByName(sess, WF_DOCUMENT_TYPE_NAME1);
			if (objTypeWfDocument == null) {
				System.out.println("ワークフロー付きドキュメントのオブジェクトタイプ[" + WF_DOCUMENT_TYPE_NAME1 + "]が存在しませんでした。DBに登録済みのオブジェクトタイプ名称を WF_DOCUMENT_TYPE_NAME1 に設定してからJUnitを実行して下さい。");
				return;
			}
			
			EIMSecurity sec1 = SecurityUtils.getSecurityByName(sess, SECURITY_NAME1);
			if (sec1 == null) {
				System.out.println("セキュリティ名称[" + SECURITY_NAME1 + "]が存在しませんでした。DBに登録済みのセキュリティ名称を SECURITY_NAME1 に設定してからJUnitを実行して下さい。");
				return;
			} else {
				boolean isNoError = false;
				List secList = SecurityUtils.getAccessEntryList(sess, sec1);
				for (Iterator iter = secList.iterator(); iter.hasNext();) {
					EIMAccessEntry entry = (EIMAccessEntry) iter.next();
					if(entry.getType().getId() == EIMAccessEntryType.USER) {
						if (entry.getUser().getId() == sess.getUser().getId()) {
							isNoError = true;
						}
					}
				}
				if (!isNoError) {
					System.out.println("セキュリティ名称[" + SECURITY_NAME1 + "]にsystemユーザが未設定です。");
					return;
				}
			}

			EIMSecurity sec2 = SecurityUtils.getSecurityByName(sess, SECURITY_NAME2);
			if (sec2 == null) {
				System.out.println("セキュリティ名称[" + SECURITY_NAME2 + "]が存在しませんでした。DBに登録済みのセキュリティ名称を SECURITY_NAME2 に設定してからJUnitを実行して下さい。");
				return;
			} else {
				boolean isNoError = false;
				List secList = SecurityUtils.getAccessEntryList(sess, sec2);
				for (Iterator iter = secList.iterator(); iter.hasNext();) {
					EIMAccessEntry entry = (EIMAccessEntry) iter.next();
					if(entry.getType().getId() == EIMAccessEntryType.USER) {
						if (entry.getUser().getId() == sess.getUser().getId()) {
							isNoError = true;
						}
					}
				}
				if (!isNoError) {
					System.out.println("セキュリティ名称[" + SECURITY_NAME1 + "]にsystemユーザが未設定です。");
					return;
				}
			}

			EIMSecurity sec3 = SecurityUtils.getSecurityByName(sess, SECURITY_NAME3);
			if (sec3 == null) {
				System.out.println("セキュリティ名称[" + SECURITY_NAME3 + "]が存在しませんでした。DBに登録済みのセキュリティ名称を SECURITY_NAME3 に設定してからJUnitを実行して下さい。");
				return;
			}
			
			EIMUser nonAuthUser1 = UserUtils.getUserByCode(sess, NON_AUTH_SEC1_USER_CODE);
			if (nonAuthUser1 == null) {
				System.out.println("ユーザコード[" + NON_AUTH_SEC1_USER_CODE + "]に該当するユーザが存在しませんでした。DBに登録済みのユーザを NON_AUTH_SEC1_USER_CODE に設定してからJUnitを実行して下さい。");
				return;
			}
			
			EIMUser nonAuthUser2 = UserUtils.getUserByCode(sess, NON_AUTH_SEC2_USER_CODE);
			if (nonAuthUser2 == null) {
				System.out.println("ユーザコード[" + NON_AUTH_SEC2_USER_CODE + "]に該当するユーザが存在しませんでした。DBに登録済みのユーザを NON_AUTH_SEC2_USER_CODE に設定してからJUnitを実行して下さい。");
				return;
			}
			
			isPreparationOK = true;
			
		} catch (EIMException eime) {
			eime.printStackTrace();
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {
			if (sess != null) {
				sess.close();
			}
		}
	}

	

	/**
	 * AttributeUtil.updateAttributeForDelete(EIMSession, EIMObject)のためのテスト・メソッド
	 * 
	 * @throws Exception 
	 */	
	public void testUpdateAttributeForDelete() throws Exception {

		
		if (!isPreparationOK) return;
		
		EIMSession sess = null;
		
		String[] tmpStrs = null; 
		int[] tmpInts = null;
		Date[] tmpDates = null;
		String[] tmpTexts = null;
		
		boolean isExceptionExist = false;		

		
		/**
		 * 正常(1)
		 *  ・「親フォルダ-子フォルダ-孫フォルダ」の子フォルダを削除
		 *  ・古い引継ぎ属性が除去される
		 *  ・元々の属性は消えてない
		 *  ・前セキュリティは保持される
		 *  ・セキュリティはsystemになる
		 */
		try {
			sess = TestSessionUtil.createEIMSession();
			setEIMSessionForUnitTest(sess);
			
			// テストデータ準備
			EIMObject obj = createTestData7(sess);	// objA - objB - objC
			int objId = obj.getId();
			EIMObject delObj = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, FOLDER_TYPE_NAME1), FOLDER_NAME_B);
			int delObjId = delObj.getId();
			
			// 属性情報の設定(下位引継ぎ属性)
			List itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeStrId, ATTR_VALUE_STR_BY_STRING1, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeIntId, ATTR_VALUE_INT_BY_STRING1, false, true));
			AttributeUpdater updater = new AttributeUpdater(obj.getId(), obj.getName(), itemList);
			AttributeUtil.updateAttribute(sess, updater);

			// 属性情報の設定(下位引継ぎ属性)
			itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeDateId, ATTR_VALUE_DATE_BY_STRING1, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeTextId, ATTR_VALUE_TEXT_BY_STRING1, false, true));
			updater = new AttributeUpdater(delObj.getId(), delObj.getName(), itemList);
			AttributeUtil.updateAttribute(sess, updater);
			
			sess.commit();
			
			// 再取得
			obj = ObjectUtils.getObjectById(sess, objId);
			delObj = ObjectUtils.getObjectById(sess, delObjId);

			// 事前確認
			viewTestData(sess, obj, "正常(1) - 事前確認");

			// メソッドの実行
			AttributeUtil.updateAttributeForDelete(sess, delObj, null);

			// 削除処理
			delObj = ObjectUtils.getObjectById(sess, delObjId);
			logicDeleteObject(sess, delObj);
			
			sess.commit();
			
			// 再取得
			obj = ObjectUtils.getObjectById(sess, objId);
			delObj = ObjectUtils.getObjectById(sess, delObjId);
			
			// 結果確認
			viewTestData(sess, obj, 	"正常(1) - 結果確認 - 外");
			viewTestData(sess, delObj, 	"正常(1) - 結果確認 - ごみ箱内");
			
			//-----------------------------------------------------------------
			// 【除去される引継ぎ属性】
			
			// 文字列型属性
			tmpStrs = AppObjectUtil.getStrAttrs(sess, delObj, ATTR_NAME_STR);
			assertEquals(tmpStrs == null ,true);
			
			// 数値型属性
			tmpInts = AppObjectUtil.getIntAttrs(sess, delObj, ATTR_NAME_INT);
			assertEquals(tmpInts == null ,true);
			
			//-----------------------------------------------------------------
			// 【上書きされない属性値】

			// 日付型属性
			tmpDates = AppObjectUtil.getDateAttrs(sess, delObj, ATTR_NAME_DATE);
			assertEquals(tmpDates.length > 0 ,true);
			for (int i = 0 ; i < tmpDates.length ; i++) {
				Date tmp = DateUtils.editExpirationDate(sess, 
						StringUtils.getDateFromString(ATTR_VALUE_DATE_BY_STRING1[i], 
						EIMResource.getMessage(sess, "EIM.FORMAT.DATE")));
				assertEquals(tmpDates[i], tmp);
			}
			
			// テキスト型属性
			tmpTexts = AppObjectUtil.getTextAttrs(sess, delObj, ATTR_NAME_TEXT);
			assertEquals(tmpTexts.length > 0 ,true);
			for (int i = 0 ; i < tmpTexts.length ; i++) {
				assertEquals(tmpTexts[i], ATTR_VALUE_TEXT_BY_STRING1[i]);
			}

			//-----------------------------------------------------------------
			// 【前セキュリティは保持されない】
			EIMSecurity beforeSec = SecurityUtils.getSecurityByName(sess, SECURITY_NAME2);
			tmpInts = AppObjectUtil.getIntAttrs(sess, delObj, EIMConfig.get("ATTR_NAME_FOLDER_PREV_SEC"));	// 前セキュリティ
			assertEquals(tmpInts.length > 0 ,true);
			assertEquals(tmpInts[0], beforeSec.getId());
			
			//-----------------------------------------------------------------
			// 【セキュリティの変更】
			assertEquals(delObj.getSecurity().getDefName(), SECURITY_NAME_SYSTEM);
			
			
		} catch (EIMException eime) {
			eime.printStackTrace();
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {
			if (sess != null) {
				sess.close();
			}
		}
		
		
		
		
		/**
		 * 正常(2)
		 *  ・「親フォルダ-子ドキュメント」の子ドキュメントを削除
		 *  ・古い引継ぎ属性が除去される
		 *  ・元々の属性は消えてない
		 *  ・ドキュメントの前セキュリティは保持されない
		 *  ・セキュリティはsystemになる
		 */
		try {
			sess = TestSessionUtil.createEIMSession();
			setEIMSessionForUnitTest(sess);
			
			// テストデータ準備
			EIMObject obj = createTestData9(sess);	// objA - doc1
			int objId = obj.getId();
			EIMObject delObj = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, DOCUMENT_TYPE_NAME1), DOC_NAME1);
			int delObjId = delObj.getId();
			
			// 属性情報の設定(下位引継ぎ属性)
			List itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeStrId, ATTR_VALUE_STR_BY_STRING1, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeIntId, ATTR_VALUE_INT_BY_STRING1, false, true));
			AttributeUpdater updater = new AttributeUpdater(obj.getId(), obj.getName(), itemList);
			AttributeUtil.updateAttribute(sess, updater);

			// 属性情報の設定(下位引継ぎ属性)
			itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeDateId, ATTR_VALUE_DATE_BY_STRING1, false, false));
			itemList.add(new AttributeUpdaterItem(attrTypeTextId, ATTR_VALUE_TEXT_BY_STRING1, false, false));
			updater = new AttributeUpdater(delObj.getId(), delObj.getName(), itemList);
			AttributeUtil.updateAttribute(sess, updater);

			sess.commit();
			
			// 再取得
			obj = ObjectUtils.getObjectById(sess, objId);
			delObj = ObjectUtils.getObjectById(sess, delObjId);

			// 事前確認
			viewTestData(sess, obj, "正常(2) - 事前確認");

			// メソッドの実行
			AttributeUtil.updateAttributeForDelete(sess, delObj, null);

			// 削除処理
			delObj = ObjectUtils.getObjectById(sess, delObjId);
			logicDeleteObject(sess, delObj);
			
			sess.commit();
			
			// 再取得
			obj = ObjectUtils.getObjectById(sess, objId);
			delObj = ObjectUtils.getObjectById(sess, delObjId);
			
			// 結果確認
			viewTestData(sess, obj, 	"正常(2) - 結果確認 - 外");
			viewTestData(sess, delObj, 	"正常(2) - 結果確認 - ごみ箱内");
			
			//-----------------------------------------------------------------
			// 【除去される引継ぎ属性】
			
			// 文字列型属性
			tmpStrs = AppObjectUtil.getStrAttrs(sess, delObj, ATTR_NAME_STR);
			assertEquals(tmpStrs == null ,true);
			
			// 数値型属性
			tmpInts = AppObjectUtil.getIntAttrs(sess, delObj, ATTR_NAME_INT);
			assertEquals(tmpInts == null ,true);
			
			//-----------------------------------------------------------------
			// 【上書きされない属性値】

			// 日付型属性
			tmpDates = AppObjectUtil.getDateAttrs(sess, delObj, ATTR_NAME_DATE);
			assertEquals(tmpDates.length > 0 ,true);
			for (int i = 0 ; i < tmpDates.length ; i++) {
				Date tmp = DateUtils.editExpirationDate(sess, 
						StringUtils.getDateFromString(ATTR_VALUE_DATE_BY_STRING1[i], 
						EIMResource.getMessage(sess, "EIM.FORMAT.DATE")));
				assertEquals(tmpDates[i], tmp);
			}
			
			// テキスト型属性
			tmpTexts = AppObjectUtil.getTextAttrs(sess, delObj, ATTR_NAME_TEXT);
			assertEquals(tmpTexts.length > 0 ,true);
			for (int i = 0 ; i < tmpTexts.length ; i++) {
				assertEquals(tmpTexts[i], ATTR_VALUE_TEXT_BY_STRING1[i]);
			}

			//-----------------------------------------------------------------
			// 【前セキュリティは保持される】
			tmpInts = AppObjectUtil.getIntAttrs(sess, delObj, EIMConfig.get("ATTR_NAME_FOLDER_PREV_SEC"));	// 前セキュリティ
			assertEquals(tmpInts == null ,true);
			
			//-----------------------------------------------------------------
			// 【セキュリティの変更】
			assertEquals(delObj.getSecurity().getDefName(), SECURITY_NAME_SYSTEM);
			
			
		} catch (EIMException eime) {
			eime.printStackTrace();
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {
			if (sess != null) {
				sess.close();
			}
		}


		
		
		/**
		 * 正常(3)
		 *  ・「親フォルダ-子フォルダ-孫WFフォルダ」の孫WFフォルダを削除 (WFのステータスは編集中)
		 *  ・古い引継ぎ属性が除去される
		 *  ・元々の属性は消えてない
		 *  ・前セキュリティは保持される
		 *  ・セキュリティはsystemになる
		 */
		try {
			sess = TestSessionUtil.createEIMSession();
			setEIMSessionForUnitTest(sess);
			
			// テストデータ準備
			EIMObject obj = createTestData8(sess);	// obj1 - obj21 - (WF)obj3
			int objId = obj.getId();
			EIMObject delObj = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, WF_FOLDER_TYPE_NAME1), WF_FOLDER_NAME_1);
			int delObjId = delObj.getId();
			
			// 属性情報の設定(下位引継ぎ属性)
			List itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeStrId, ATTR_VALUE_STR_BY_STRING1, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeIntId, ATTR_VALUE_INT_BY_STRING1, false, true));
			AttributeUpdater updater = new AttributeUpdater(obj.getId(), obj.getName(), itemList);
			AttributeUtil.updateAttribute(sess, updater);

			// 属性情報の設定(下位引継ぎ属性)
			itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeDateId, ATTR_VALUE_DATE_BY_STRING1, false, false));
			itemList.add(new AttributeUpdaterItem(attrTypeTextId, ATTR_VALUE_TEXT_BY_STRING1, false, false));
			updater = new AttributeUpdater(delObj.getId(), delObj.getName(), itemList);
			AttributeUtil.updateAttribute(sess, updater);

			sess.commit();
			
			// 再取得
			obj = ObjectUtils.getObjectById(sess, objId);
			delObj = ObjectUtils.getObjectById(sess, delObjId);

			// 事前確認
			viewTestData(sess, obj, "正常(3) - 事前確認");

			// メソッドの実行
			AttributeUtil.updateAttributeForDelete(sess, delObj, null);

			// 削除処理
			delObj = ObjectUtils.getObjectById(sess, delObjId);
			logicDeleteObject(sess, delObj);
			
			sess.commit();
			
			// 再取得
			obj = ObjectUtils.getObjectById(sess, objId);
			delObj = ObjectUtils.getObjectById(sess, delObjId);
			
			// 結果確認
			viewTestData(sess, obj, 	"正常(3) - 結果確認 - 外");
			viewTestData(sess, delObj, 	"正常(3) - 結果確認 - ごみ箱内");
			
			//-----------------------------------------------------------------
			// 【除去される引継ぎ属性】
			
			// 文字列型属性
			tmpStrs = AppObjectUtil.getStrAttrs(sess, delObj, ATTR_NAME_STR);
			assertEquals(tmpStrs == null ,true);
			
			// 数値型属性
			tmpInts = AppObjectUtil.getIntAttrs(sess, delObj, ATTR_NAME_INT);
			assertEquals(tmpInts == null ,true);
			
			//-----------------------------------------------------------------
			// 【上書きされない属性値】

			// 日付型属性
			tmpDates = AppObjectUtil.getDateAttrs(sess, delObj, ATTR_NAME_DATE);
			assertEquals(tmpDates.length > 0 ,true);
			for (int i = 0 ; i < tmpDates.length ; i++) {
				Date tmp = DateUtils.editExpirationDate(sess, 
						StringUtils.getDateFromString(ATTR_VALUE_DATE_BY_STRING1[i], 
						EIMResource.getMessage(sess, "EIM.FORMAT.DATE")));
				assertEquals(tmpDates[i], tmp);
			}
			
			// テキスト型属性
			tmpTexts = AppObjectUtil.getTextAttrs(sess, delObj, ATTR_NAME_TEXT);
			assertEquals(tmpTexts.length > 0 ,true);
			for (int i = 0 ; i < tmpTexts.length ; i++) {
				assertEquals(tmpTexts[i], ATTR_VALUE_TEXT_BY_STRING1[i]);
			}

			//-----------------------------------------------------------------
			// 【前セキュリティは保持される】
			EIMSecurity beforeSec = SecurityUtils.getSecurityByName(sess, SECURITY_NAME1);
			tmpInts = AppObjectUtil.getIntAttrs(sess, delObj, EIMConfig.get("ATTR_NAME_FOLDER_PREV_SEC"));	// 前セキュリティ
			assertEquals(tmpInts.length > 0 ,true);
			assertEquals(tmpInts[0], beforeSec.getId());
			
			//-----------------------------------------------------------------
			// 【セキュリティの変更】
			assertEquals(delObj.getSecurity().getDefName(), SECURITY_NAME_SYSTEM);
			
			
		} catch (EIMException eime) {
			eime.printStackTrace();
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {
			if (sess != null) {
				sess.close();
			}
		}
		
		
		
		
		/**
		 * 正常(4)
		 *  ・「親フォルダ-子フォルダ-孫WFフォルダ」の子フォルダを削除 (WFのステータスは編集中)
		 *  ・古い引継ぎ属性が除去される
		 *  ・元々の属性は消えてない
		 *  ・前セキュリティは保持される
		 *  ・セキュリティはsystemになる
		 */
		try {
			sess = TestSessionUtil.createEIMSession();
			setEIMSessionForUnitTest(sess);
			
			// テストデータ準備
			EIMObject obj = createTestData8(sess);	// obj1 - obj21 - (WF)obj3
			int objId = obj.getId();
			EIMObject delObj = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, FOLDER_TYPE_NAME1), FOLDER_NAME21);
			int delObjId = delObj.getId();
			
			// 属性情報の設定(下位引継ぎ属性)
			List itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeStrId, ATTR_VALUE_STR_BY_STRING1, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeIntId, ATTR_VALUE_INT_BY_STRING1, false, true));
			AttributeUpdater updater = new AttributeUpdater(obj.getId(), obj.getName(), itemList);
			AttributeUtil.updateAttribute(sess, updater);

			// 属性情報の設定(下位引継ぎ属性)
			itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeDateId, ATTR_VALUE_DATE_BY_STRING1, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeTextId, ATTR_VALUE_TEXT_BY_STRING1, false, true));
			updater = new AttributeUpdater(delObj.getId(), delObj.getName(), itemList);
			AttributeUtil.updateAttribute(sess, updater);

			sess.commit();
			
			// 再取得
			obj = ObjectUtils.getObjectById(sess, objId);
			delObj = ObjectUtils.getObjectById(sess, delObjId);

			// 事前確認
			viewTestData(sess, obj, "正常(3) - 事前確認");

			// メソッドの実行
			AttributeUtil.updateAttributeForDelete(sess, delObj, null);

			// 削除処理
			delObj = ObjectUtils.getObjectById(sess, delObjId);
			logicDeleteObject(sess, delObj);
			
			sess.commit();
			
			// 再取得
			obj = ObjectUtils.getObjectById(sess, objId);
			delObj = ObjectUtils.getObjectById(sess, delObjId);
			EIMObject wfObj = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, WF_FOLDER_TYPE_NAME1), WF_FOLDER_NAME_1);
			
			// 結果確認
			viewTestData(sess, obj, 	"正常(3) - 結果確認 - 外");
			viewTestData(sess, delObj, 	"正常(3) - 結果確認 - ごみ箱内");
			
			//-----------------------------------------------------------------
			// 【除去される引継ぎ属性】
			
			// 文字列型属性
			tmpStrs = AppObjectUtil.getStrAttrs(sess, wfObj, ATTR_NAME_STR);
			assertEquals(tmpStrs == null ,true);
			
			// 数値型属性
			tmpInts = AppObjectUtil.getIntAttrs(sess, wfObj, ATTR_NAME_INT);
			assertEquals(tmpInts == null ,true);
			
			//-----------------------------------------------------------------
			// 【上書きされない属性値】

			// 日付型属性
			tmpDates = AppObjectUtil.getDateAttrs(sess, wfObj, ATTR_NAME_DATE);
			assertEquals(tmpDates.length > 0 ,true);
			for (int i = 0 ; i < tmpDates.length ; i++) {
				Date tmp = DateUtils.editExpirationDate(sess, 
						StringUtils.getDateFromString(ATTR_VALUE_DATE_BY_STRING1[i], 
						EIMResource.getMessage(sess, "EIM.FORMAT.DATE")));
				assertEquals(tmpDates[i], tmp);
			}
			
			// テキスト型属性
			tmpTexts = AppObjectUtil.getTextAttrs(sess, wfObj, ATTR_NAME_TEXT);
			assertEquals(tmpTexts.length > 0 ,true);
			for (int i = 0 ; i < tmpTexts.length ; i++) {
				assertEquals(tmpTexts[i], ATTR_VALUE_TEXT_BY_STRING1[i]);
			}

			//-----------------------------------------------------------------
			// 【前セキュリティは保持される】
			EIMSecurity beforeSec = SecurityUtils.getSecurityByName(sess, SECURITY_NAME1);
			tmpInts = AppObjectUtil.getIntAttrs(sess, wfObj, EIMConfig.get("ATTR_NAME_FOLDER_PREV_SEC"));	// 前セキュリティ
			assertEquals(tmpInts.length > 0 ,true);
			assertEquals(tmpInts[0], beforeSec.getId());
			
			//-----------------------------------------------------------------
			// 【セキュリティの変更】
			assertEquals(wfObj.getSecurity().getDefName(), SECURITY_NAME_SYSTEM);
			
			
		} catch (EIMException eime) {
			eime.printStackTrace();
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {
			if (sess != null) {
				sess.close();
			}
		}
		
		
		
		
		/**
		 * 異常(1)
		 * 
		 * ・削除対象オブジェクトに対する削除権限がセキュリティにない場合
		 */
		try {
			isExceptionExist = false;

			sess = new EIMSession();
			sess.setConsole();
			
			// テストデータ準備
			createTestData8(sess);	// obj1 - obj21 - (WF)obj3
			EIMObject delObj = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, FOLDER_TYPE_NAME1), FOLDER_NAME21);
			
			EIMUser nonAuthUser = UserUtils.getUserByCode(sess, NON_AUTH_SEC2_USER_CODE);
			sess.setUser(nonAuthUser);
			
			// メソッドの実行
			AttributeUtil.updateAttributeForDelete(sess, delObj, null);
		
		} catch (EIMException eime) {

			isExceptionExist = true;
			assertEquals(eime.getMessage(), EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODELETEROLE"));
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
		 * 異常(2)
		 * 
		 * ・親オブジェクトに「下位フォルダ管理セキュリティ」が存在する場合
		 */
		try {
			isExceptionExist = false;

			sess = new EIMSession();
			sess.setConsole();
			
			// テストデータ準備
			createTestData7(sess);
			EIMObject objB = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, FOLDER_TYPE_NAME1), FOLDER_NAME_B);
			int objBId = objB.getId();
			
			// フォルダ構成管理権限を設定
			EIMSecurity sec = SecurityUtils.getSecurityByName(sess, SECURITY_NAME3);
			AppObjectUtil.setAttr(sess, objB, EIMConfig.get("ATTR_NAME_FOLDER_LOW_FOLDER_SEC"), sec.getId());	// 下位フォルダ管理セキュリティ

			// 再取得
			objB = ObjectUtils.getObjectById(sess, objBId);
			EIMObject objC = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, FOLDER_TYPE_NAME1), FOLDER_NAME_C);
			
			// メソッドの実行
			AttributeUtil.updateAttributeForDelete(sess, objC, objB);
		
		} catch (EIMException eime) {

			isExceptionExist = true;
			assertEquals(eime.getMessage(), EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODELETEROLE"));
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
		 * 異常(3)
		 * 
		 * ・上位WFフォルダが存在する、かつ、上位WFのステータスが「承認依頼中」の場合
		 */
		try {
			isExceptionExist = false;

			sess = new EIMSession();
			sess.setConsole();
			
			// テストデータ準備
			EIMObject wfObj = createTestData6(sess);	// (WF)obj1 - obj1
			int wfObjId = wfObj.getId();
			WorkFlowUtils.statusUp(sess, wfObj);		// 承認依頼中
			
			wfObj = ObjectUtils.getObjectById(sess, wfObjId);
			EIMObject obj1 = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, FOLDER_TYPE_NAME1), FOLDER_NAME1);
			
			sess.commit();
			
			// メソッドの実行
			AttributeUtil.updateAttributeForDelete(sess, obj1, wfObj);
		
		} catch (EIMException eime) {

			isExceptionExist = true;
			assertEquals(eime.getMessage(), EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODELETEROLE"));
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
		 * 異常(4)
		 * 
		 * ・上位WFフォルダが存在する、かつ、上位WFのステータスが「公開処理中」の場合
		 */
		try {
			isExceptionExist = false;

			sess = new EIMSession();
			sess.setConsole();
			
			// テストデータ準備
			EIMObject wfObj = createTestData6(sess);	// (WF)obj1 - obj1
			int wfObjId = wfObj.getId();
			WorkFlowUtils.statusUp(sess, wfObj);		// 承認依頼中
			wfObj = ObjectUtils.getObjectById(sess, wfObjId);
			WorkFlowUtils.statusUp(sess, wfObj);		// 公開処理中
			
			EIMObject obj1 = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, FOLDER_TYPE_NAME1), FOLDER_NAME1);
			wfObj = ObjectUtils.getObjectById(sess, wfObjId);
			
			sess.commit();
			
			// メソッドの実行
			AttributeUtil.updateAttributeForDelete(sess, obj1, wfObj);
		
		} catch (EIMException eime) {

			isExceptionExist = true;
			assertEquals(eime.getMessage(), EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODELETEROLE"));
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
		 * 異常(5)
		 * 
		 * ・上位WFフォルダが存在する、かつ、上位WFのステータスが「公開済み」の場合
		 */
		try {
			isExceptionExist = false;

			sess = new EIMSession();
			sess.setConsole();
			
			// テストデータ準備
			EIMObject wfObj = createTestData6(sess);	// (WF)obj1 - obj1
			int wfObjId = wfObj.getId();
			WorkFlowUtils.statusUp(sess, wfObj);		// 承認依頼中
			wfObj = ObjectUtils.getObjectById(sess, wfObjId);
			WorkFlowUtils.statusUp(sess, wfObj);		// 公開処理中
			wfObj = ObjectUtils.getObjectById(sess, wfObjId);
			WorkFlowUtils.statusUp(sess, wfObj);		// 公開済み
			
			EIMObject obj1 = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, FOLDER_TYPE_NAME1), FOLDER_NAME1);
			wfObj = ObjectUtils.getObjectById(sess, wfObjId);
			
			sess.commit();
			
			// メソッドの実行
			AttributeUtil.updateAttributeForDelete(sess, obj1, wfObj);
		
		} catch (EIMException eime) {

			isExceptionExist = true;
			assertEquals(eime.getMessage(), EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODELETEROLE"));
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
		 * 異常(6)
		 * 
		 * ・上位WFフォルダが存在しない、かつ、対象WFフォルダが「承認依頼中」の場合
		 */
		try {
			isExceptionExist = false;

			sess = new EIMSession();
			sess.setConsole();
			
			// テストデータ準備
			createTestData8(sess);	// obj1 - obj21 - (WF)obj3
			EIMObject wfObj = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, WF_FOLDER_TYPE_NAME1), WF_FOLDER_NAME_1);
			int wfObjId = wfObj.getId();
			
			WorkFlowUtils.statusUp(sess, wfObj);		// 承認依頼中
			
			sess.commit();
			
			wfObj = ObjectUtils.getObjectById(sess, wfObjId);
			EIMObject obj21 = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, FOLDER_TYPE_NAME1), FOLDER_NAME21);
			
			// メソッドの実行
			AttributeUtil.updateAttributeForDelete(sess, wfObj, obj21);
		
		} catch (EIMException eime) {

			isExceptionExist = true;
			assertEquals(eime.getMessage(), EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODELETEROLE"));
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
		 * 異常(7)
		 * 
		 * ・上位WFフォルダが存在しない、かつ、対象WFフォルダが「公開処理中」の場合
		 */
		try {
			isExceptionExist = false;

			sess = new EIMSession();
			sess.setConsole();
			
			// テストデータ準備
			createTestData8(sess);	// obj1 - obj21 - (WF)obj3
			EIMObject wfObj = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, WF_FOLDER_TYPE_NAME1), WF_FOLDER_NAME_1);
			int wfObjId = wfObj.getId();
			
			WorkFlowUtils.statusUp(sess, wfObj);		// 承認依頼中
			wfObj = ObjectUtils.getObjectById(sess, wfObjId);
			WorkFlowUtils.statusUp(sess, wfObj);		// 公開処理中
			
			sess.commit();
			
			wfObj = ObjectUtils.getObjectById(sess, wfObjId);
			EIMObject obj21 = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, FOLDER_TYPE_NAME1), FOLDER_NAME21);
			
			// メソッドの実行
			AttributeUtil.updateAttributeForDelete(sess, wfObj, obj21);
		
		} catch (EIMException eime) {

			isExceptionExist = true;
			assertEquals(eime.getMessage(), EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODELETEROLE"));
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
	 * AttributeUtil.updateAttributeForCopy(EIMSession, EIMObject, EIMObject, String)のためのテスト・メソッド
	 * 
	 * @throws Exception 
	 */	
	public void testUpdateAttributeForCopy() throws Exception {

		
		if (!isPreparationOK) return;
		
		EIMSession sess = null;
		
		String[] tmpStrs = null; 
		int[] tmpInts = null;
		Date[] tmpDates = null;
		String[] tmpTexts = null;
		
		boolean isExceptionExist = false;
		
		/**
		 * 正常(1)
		 *  ・ドキュメント1をフォルダ1の下にコピー
		 *  ・古い引継ぎ属性が除去される
		 *  ・元々の属性は消えてない
		 *  ・新しい引継ぎ属性が適応される
		 *  ・新しいセキュリティが適応される
		 */
		try {
			sess = TestSessionUtil.createEIMSession();
			setEIMSessionForUnitTest(sess);
			
			// テストデータ準備
			EIMObject moveParanetObj = createTestData9(sess);	// objA - doc1
			EIMObject destParanetObj = createTestData10(sess);	// obj1 - obj21
			EIMObject moveObj = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, DOCUMENT_TYPE_NAME1), DOC_NAME1);
			int moveParanetObjId = moveParanetObj.getId();
			int destParanetObjId = destParanetObj.getId();
			int moveObjId = moveObj.getId();
			
			// 属性情報の設定(下位引継ぎ属性)
			List itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeStrId, ATTR_VALUE_STR_BY_STRING1, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeIntId, ATTR_VALUE_INT_BY_STRING1, false, true));
			AttributeUpdater updater = new AttributeUpdater(moveParanetObj.getId(), moveParanetObj.getName(), itemList);
			AttributeUtil.updateAttribute(sess, updater);

			// 属性情報の設定(下位引継ぎ属性)
			itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeStrOneId, ATTR_VALUE_STR_BY_STRING1_ONE, false, false));
			updater = new AttributeUpdater(moveObj.getId(), moveObj.getName(), itemList);
			AttributeUtil.updateAttribute(sess, updater);
			
			// 属性情報の設定(下位引継ぎ属性)
			itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeDateId, ATTR_VALUE_DATE_BY_STRING1, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeTextId, ATTR_VALUE_TEXT_BY_STRING1, false, true));
			updater = new AttributeUpdater(destParanetObj.getId(), destParanetObj.getName(), itemList);
			AttributeUtil.updateAttribute(sess, updater);
			
			sess.commit();
			
			moveParanetObj = ObjectUtils.getObjectById(sess, moveParanetObjId);
			destParanetObj = ObjectUtils.getObjectById(sess, destParanetObjId);
			moveObj = ObjectUtils.getObjectById(sess, moveObjId);

			EIMObject destObj = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, FOLDER_TYPE_NAME1), FOLDER_NAME21);
			
			// 事前確認
			viewTestData(sess, moveParanetObj, "正常(1) - 事前確認 - コピー対象");
			viewTestData(sess, destParanetObj, "正常(1) - 事前確認 - コピー先");

			// コピー＆リレーション設定
			EIMObject copyObj = copyObject(sess, moveObj, destObj);
			int copyObjId = copyObj.getId();
			
			// メソッドの実行
			AttributeUtil.updateAttributeForCopy(sess, copyObj, destObj, getPath(sess, destObj));
			sess.commit();
			
			// 再取得
			moveParanetObj = ObjectUtils.getObjectById(sess, moveParanetObjId);
			destParanetObj = ObjectUtils.getObjectById(sess, destParanetObjId);
			copyObj = ObjectUtils.getObjectById(sess, copyObjId);
			
			// 結果確認
			viewTestData(sess, moveParanetObj, "正常(1) - 結果確認 - コピー対象");
			viewTestData(sess, destParanetObj, "正常(1) - 結果確認 - コピー先");
			
			//-----------------------------------------------------------------
			// 【除去される引継ぎ属性】
			
			// 文字列型属性
			tmpStrs = AppObjectUtil.getStrAttrs(sess, copyObj, ATTR_NAME_STR);
			assertEquals(tmpStrs == null ,true);
			
			// 数値型属性
			tmpInts = AppObjectUtil.getIntAttrs(sess, copyObj, ATTR_NAME_INT);
			assertEquals(tmpInts == null ,true);
			
			//-----------------------------------------------------------------
			// 【上書きされない属性値】

			// 文字列型属性 (単数値)
			tmpStrs = AppObjectUtil.getStrAttrs(sess, copyObj, ATTR_NAME_STR_ONE);
			assertEquals(tmpStrs.length > 0 ,true);
			assertEquals(tmpStrs[0], ATTR_VALUE_STR_BY_STRING1_ONE[0]);
			
			//-----------------------------------------------------------------
			// 【上書きされる属性値】
			
			// 日付型属性
			tmpDates = AppObjectUtil.getDateAttrs(sess, copyObj, ATTR_NAME_DATE);
			assertEquals(tmpDates.length > 0 ,true);
			for (int i = 0 ; i < tmpDates.length ; i++) {
				Date tmp = DateUtils.editExpirationDate(sess, 
						StringUtils.getDateFromString(ATTR_VALUE_DATE_BY_STRING1[i], 
						EIMResource.getMessage(sess, "EIM.FORMAT.DATE")));
				assertEquals(tmpDates[i], tmp);
			}
			
			// テキスト型属性
			tmpTexts = AppObjectUtil.getTextAttrs(sess, copyObj, ATTR_NAME_TEXT);
			assertEquals(tmpTexts.length > 0 ,true);
			for (int i = 0 ; i < tmpTexts.length ; i++) {
				assertEquals(tmpTexts[i], ATTR_VALUE_TEXT_BY_STRING1[i]);
			}

			//-----------------------------------------------------------------
			// 【セキュリティの変更】
			assertEquals(copyObj.getSecurity().getDefName(), SECURITY_NAME1);
			
			
		} catch (EIMException eime) {
			eime.printStackTrace();
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {
			if (sess != null) {
				sess.close();
			}
		}
		
		
		
		
		/**
		 * 正常(2)
		 *  ・ドキュメント1をWFフォルダ1の下にコピー
		 *  ・古い引継ぎ属性が除去される
		 *  ・元々の属性は消えてない
		 *  ・新しい引継ぎ属性が適応される
		 *  ・新しいセキュリティが適応される
		 *  ・上位WFフォルダが更新される
		 */
		try {
			sess = TestSessionUtil.createEIMSession();
			setEIMSessionForUnitTest(sess);
			
			// テストデータ準備
			EIMObject moveParanetObj = createTestData9(sess);	// objA - doc1
			EIMObject destRootObj = createTestData8(sess);		// obj1 - obj21 - (WF)obj3
			EIMObject moveObj = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, DOCUMENT_TYPE_NAME1), DOC_NAME1);
			int moveParanetObjId = moveParanetObj.getId();
			int destRootObjId = destRootObj.getId();
			int moveObjId = moveObj.getId();
			
			// 属性情報の設定(下位引継ぎ属性)
			List itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeStrId, ATTR_VALUE_STR_BY_STRING1, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeIntId, ATTR_VALUE_INT_BY_STRING1, false, true));
			AttributeUpdater updater = new AttributeUpdater(moveParanetObj.getId(), moveParanetObj.getName(), itemList);
			AttributeUtil.updateAttribute(sess, updater);
			
			// 属性情報の設定(下位引継ぎ属性)
			itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeStrOneId, ATTR_VALUE_STR_BY_STRING1_ONE, false, false));
			updater = new AttributeUpdater(moveObj.getId(), moveObj.getName(), itemList);
			AttributeUtil.updateAttribute(sess, updater);
			
			// 属性情報の設定(下位引継ぎ属性)
			itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeDateId, ATTR_VALUE_DATE_BY_STRING1, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeTextId, ATTR_VALUE_TEXT_BY_STRING1, false, true));
			updater = new AttributeUpdater(destRootObj.getId(), destRootObj.getName(), itemList);
			AttributeUtil.updateAttribute(sess, updater);
			
			sess.commit();
			
			moveParanetObj = ObjectUtils.getObjectById(sess, moveParanetObjId);
			destRootObj = ObjectUtils.getObjectById(sess, destRootObjId);
			moveObj = ObjectUtils.getObjectById(sess, moveObjId);

			EIMObject destObj = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, WF_FOLDER_TYPE_NAME1), WF_FOLDER_NAME_1);
			int destObjId = destObj.getId();
			
			// 事前確認
			viewTestData(sess, moveParanetObj, 	"正常(2) - 事前確認 - コピー対象");
			viewTestData(sess, destRootObj, 	"正常(2) - 事前確認 - コピー先");

			// コピー＆リレーション設定
			EIMObject copyObj =  copyObject(sess, moveObj, destObj);
			int copyObjId = copyObj.getId();
			
			// メソッドの実行
			AttributeUtil.updateAttributeForCopy(sess, copyObj, destObj, getPath(sess, destObj));
			sess.commit();
			
			// 再取得
			moveParanetObj = ObjectUtils.getObjectById(sess, moveParanetObjId);
			destRootObj = ObjectUtils.getObjectById(sess, destRootObjId);
			copyObj = ObjectUtils.getObjectById(sess, copyObjId);
			
			// 結果確認
			viewTestData(sess, moveParanetObj, 	"正常(2) - 結果確認 - コピー対象");
			viewTestData(sess, destRootObj, 	"正常(2) - 結果確認 - コピー先");
			
			//-----------------------------------------------------------------
			// 【除去される引継ぎ属性】
			
			// 文字列型属性
			tmpStrs = AppObjectUtil.getStrAttrs(sess, copyObj, ATTR_NAME_STR);
			assertEquals(tmpStrs == null ,true);
			
			// 数値型属性
			tmpInts = AppObjectUtil.getIntAttrs(sess, copyObj, ATTR_NAME_INT);
			assertEquals(tmpInts == null ,true);
			
			//-----------------------------------------------------------------
			// 【上書きされない属性値】

			// 文字列型属性 (単数値)
			tmpStrs = AppObjectUtil.getStrAttrs(sess, copyObj, ATTR_NAME_STR_ONE);
			assertEquals(tmpStrs.length > 0 ,true);
			assertEquals(tmpStrs[0], ATTR_VALUE_STR_BY_STRING1_ONE[0]);
			
			//-----------------------------------------------------------------
			// 【上書きされる属性値】
			
			// 日付型属性
			tmpDates = AppObjectUtil.getDateAttrs(sess, copyObj, ATTR_NAME_DATE);
			assertEquals(tmpDates.length > 0 ,true);
			for (int i = 0 ; i < tmpDates.length ; i++) {
				Date tmp = DateUtils.editExpirationDate(sess, 
						StringUtils.getDateFromString(ATTR_VALUE_DATE_BY_STRING1[i], 
						EIMResource.getMessage(sess, "EIM.FORMAT.DATE")));
				assertEquals(tmpDates[i], tmp);
			}
			
			// テキスト型属性
			tmpTexts = AppObjectUtil.getTextAttrs(sess, copyObj, ATTR_NAME_TEXT);
			assertEquals(tmpTexts.length > 0 ,true);
			for (int i = 0 ; i < tmpTexts.length ; i++) {
				assertEquals(tmpTexts[i], ATTR_VALUE_TEXT_BY_STRING1[i]);
			}

			//-----------------------------------------------------------------
			// 【セキュリティの変更】
			assertEquals(copyObj.getSecurity().getDefName(), SECURITY_NAME1);

			//-----------------------------------------------------------------
			// 【上位WFフォルダの変更】
			tmpInts = AppObjectUtil.getIntAttrs(sess, copyObj, EIMConfig.get("ATTR_NAME_FOLDER_HIGHER_WF_FOLDER"));
			assertEquals(tmpInts[0], destObjId);

			
		} catch (EIMException eime) {
			eime.printStackTrace();
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {
			if (sess != null) {
				sess.close();
			}
		}
		
		
		
		
		/**
		 * 異常(1)
		 * 
		 * ・WFフォルダの下にWFドキュメントの場合
		 */
		try {
			isExceptionExist = false;

			sess = TestSessionUtil.createEIMSession();
			setEIMSessionForUnitTest(sess);
			
			// テストデータ準備
			createTestData6(sess);	// (WF)obj1 - obj1
			createTestData11(sess);	// objA - (WF)doc1
			
			EIMObject obj1 = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, FOLDER_TYPE_NAME1), FOLDER_NAME1);
			
			EIMObject wfDoc = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, WF_DOCUMENT_TYPE_NAME1), WF_DOCUMENT_NAME_1);

			// コピー＆リレーション設定
			EIMObject copyObj =  copyObject(sess, wfDoc, obj1);
			
			// メソッドの実行
			AttributeUtil.updateAttributeForCopy(sess, copyObj, obj1, getPath(sess, obj1));
			
		} catch (EIMException eime) {
			isExceptionExist = true;
			assertEquals(eime.getMessage(), EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.CANTSET.UNDERWF.WFDOCFOL"));
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
	 * AttributeUtil.updateAttributeForMove(EIMSession, EIMObject, EIMObject, String)のためのテスト・メソッド
	 * 
	 * @throws Exception 
	 */	
	public void testUpdateAttributeForMove() throws Exception {

		
		if (!isPreparationOK) return;
		
		EIMSession sess = null;
		
		String[] tmpStrs = null; 
		int[] tmpInts = null;
		Date[] tmpDates = null;
		String[] tmpTexts = null;
		
		boolean isExceptionExist = false;


/******** ここから下はごみ箱からの移動 (論理削除の解除) ****************/		
		
		
		/**
		 * 正常(1)
		 *  ・ごみ箱の下の「フォルダA-フォルダB-フォルダC」をフォルダ21の下に移動
		 *  ・元々の引継ぎ属性はそのまま
		 *  ・新しい引継ぎ属性が適応される
		 *  ・前セキュリティが適応される
		 */
		try {
			sess = TestSessionUtil.createEIMSession();
			setEIMSessionForUnitTest(sess);
			
			// テストデータ準備
			EIMObject objA = createTestData7(sess);		// [Sec2] objA - objB -objC　
			EIMObject obj1 = createTestData10(sess);	// [Sec1] obj1 - obj21
			int objAId = objA.getId();
			int obj1Id = obj1.getId();

			EIMObject obj21 = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, FOLDER_TYPE_NAME1), FOLDER_NAME21);
			int obj21Id = obj21.getId();
			EIMObject objC = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, FOLDER_TYPE_NAME1), FOLDER_NAME_C);
			int objCId = objC.getId();
			
			// 属性情報の設定(下位引継ぎ属性)
			List itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeStrId, ATTR_VALUE_STR_BY_STRING1, false, true));
			AttributeUpdater updater = new AttributeUpdater(obj1.getId(), obj1.getName(), itemList);
			AttributeUtil.updateAttribute(sess, updater);

			itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeIntId, ATTR_VALUE_INT_BY_STRING1, false, true));
			updater = new AttributeUpdater(objA.getId(), objA.getName(), itemList);
			AttributeUtil.updateAttribute(sess, updater);

			// 再取得
			objA  = ObjectUtils.getObjectById(sess, objAId);
			
			// 属性情報更新(論理削除モード)
			AttributeUtil.updateAttributeForDelete(sess, objA, null);

			// 削除処理
			objA = ObjectUtils.getObjectById(sess, objAId);
			logicDeleteObject(sess, objA);
			
			// 再取得
			objA  = ObjectUtils.getObjectById(sess, objAId);
			obj1  = ObjectUtils.getObjectById(sess, obj1Id);
			obj21 = ObjectUtils.getObjectById(sess, obj21Id);

			// 事前確認
			viewTestData(sess, objA, "正常(1) - 事前確認 - ごみ箱");
			viewTestData(sess, obj1, "正常(1) - 事前確認 - 貼付け先");

			// リレーション設定
			changeRelation(sess, objA, obj21);

			// 再取得
			objA  = ObjectUtils.getObjectById(sess, objAId);
			obj21 = ObjectUtils.getObjectById(sess, obj21Id);
			
			// メソッドの実行
			AttributeUtil.updateAttributeForMove(sess, objA, obj21, getPath(sess, obj21), true);

			sess.commit();
			
			// 再取得
			obj1 = ObjectUtils.getObjectById(sess, obj1Id);
			objC = ObjectUtils.getObjectById(sess, objCId);
			
			// 結果確認
			viewTestData(sess, obj1, "正常(1) - 結果確認 - 貼付け先");
			
			//-----------------------------------------------------------------
			// 【貼付け先の引継ぎ属性が適応される属性値】
			
			// 文字列型属性
			tmpStrs = AppObjectUtil.getStrAttrs(sess, objC, ATTR_NAME_STR);
			assertEquals(tmpStrs.length > 0 ,true);
			for (int i = 0 ; i < tmpStrs.length ; i++) {
				assertEquals(tmpStrs[i], ATTR_VALUE_STR_BY_STRING1[i]);
			}

			//-----------------------------------------------------------------
			// 【上書きされない属性値】
			
			// 数値型属性
			tmpInts = AppObjectUtil.getIntAttrs(sess, objC, ATTR_NAME_INT);
			assertEquals(tmpInts.length > 0 ,true);
			for (int i = 0 ; i < tmpInts.length ; i++) {
				assertEquals(tmpInts[i], Integer.parseInt(ATTR_VALUE_INT_BY_STRING1[i]));
			}
			
			//-----------------------------------------------------------------
			// 【セキュリティの変更】
			
			assertEquals(objC.getSecurity().getDefName(), SECURITY_NAME2);

			//-----------------------------------------------------------------
			// 【前セキュリティの削除】
			
			tmpInts = AppObjectUtil.getIntAttrs(sess, objC, EIMConfig.get("ATTR_NAME_FOLDER_PREV_SEC"));	// 前セキュリティ
			assertEquals(tmpInts == null ,true);

			
		} catch (EIMException eime) {
			eime.printStackTrace();
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {
			if (sess != null) {
				sess.close();
			}
		}
		

		
		
		/**
		 * 正常(2)
		 *  ・ごみ箱の下の「フォルダ1 - フォルダ21 - (WF)フォルダ3」をフォルダCの下に移動
		 *  ・元々の引継ぎ属性はそのまま
		 *  ・新しい引継ぎ属性が適応される
		 *  ・前セキュリティが適応される
		 */
		try {
			sess = TestSessionUtil.createEIMSession();
			setEIMSessionForUnitTest(sess);
			
			// テストデータ準備
			EIMObject objA = createTestData7(sess);		// [Sec2] objA - objB  - objC　
			EIMObject obj1 = createTestData8(sess);		// [Sec1] obj1 - obj21 - (WF)obj3
			int objAId = objA.getId();
			int obj1Id = obj1.getId();

			EIMObject objC = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, FOLDER_TYPE_NAME1), FOLDER_NAME_C);
			int objCId = objC.getId();
			EIMObject objWF = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, WF_FOLDER_TYPE_NAME1), WF_FOLDER_NAME_1);
			int objWFId = objWF.getId();
			
			// 属性情報の設定(下位引継ぎ属性)
			List itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeStrId, ATTR_VALUE_STR_BY_STRING1, false, true));
			AttributeUpdater updater = new AttributeUpdater(obj1.getId(), obj1.getName(), itemList);
			AttributeUtil.updateAttribute(sess, updater);

			itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeIntId, ATTR_VALUE_INT_BY_STRING1, false, true));
			updater = new AttributeUpdater(objA.getId(), objA.getName(), itemList);
			AttributeUtil.updateAttribute(sess, updater);

			// 再取得
			obj1  = ObjectUtils.getObjectById(sess, obj1Id);
			
			// 属性情報更新(論理削除モード)
			AttributeUtil.updateAttributeForDelete(sess, obj1, null);

			// 削除処理
			obj1 = ObjectUtils.getObjectById(sess, obj1Id);
			logicDeleteObject(sess, obj1);
			
			// 再取得
			obj1  = ObjectUtils.getObjectById(sess, obj1Id);
			objA  = ObjectUtils.getObjectById(sess, objAId);

			// 事前確認
			viewTestData(sess, obj1, "正常(2) - 事前確認 - ごみ箱");
			viewTestData(sess, objA, "正常(2) - 事前確認 - 貼付け先");

			// リレーション設定
			changeRelation(sess, obj1, objC);

			// 再取得
			objC = ObjectUtils.getObjectById(sess, objCId);
			obj1 = ObjectUtils.getObjectById(sess, obj1Id);
			
			// メソッドの実行
			AttributeUtil.updateAttributeForMove(sess, obj1, objC, getPath(sess, objC), true);

			sess.commit();
			
			// 再取得
			objA  = ObjectUtils.getObjectById(sess, objAId);
			objWF = ObjectUtils.getObjectById(sess, objWFId);
			
			// 結果確認
			viewTestData(sess, objA, "正常(2) - 結果確認 - 貼付け先");
			
			//-----------------------------------------------------------------
			// 【貼付け先の引継ぎ属性が適応される属性値】
			
			// 文字列型属性
			tmpStrs = AppObjectUtil.getStrAttrs(sess, objWF, ATTR_NAME_STR);
			assertEquals(tmpStrs.length > 0 ,true);
			for (int i = 0 ; i < tmpStrs.length ; i++) {
				assertEquals(tmpStrs[i], ATTR_VALUE_STR_BY_STRING1[i]);
			}

			//-----------------------------------------------------------------
			// 【上書きされない属性値】
			
			// 数値型属性
			tmpInts = AppObjectUtil.getIntAttrs(sess, objWF, ATTR_NAME_INT);
			assertEquals(tmpInts.length > 0 ,true);
			for (int i = 0 ; i < tmpInts.length ; i++) {
				assertEquals(tmpInts[i], Integer.parseInt(ATTR_VALUE_INT_BY_STRING1[i]));
			}
			
			//-----------------------------------------------------------------
			// 【セキュリティの変更】
			
			assertEquals(objWF.getSecurity().getDefName(), SECURITY_NAME1);

			//-----------------------------------------------------------------
			// 【前セキュリティの削除】
			
			tmpInts = AppObjectUtil.getIntAttrs(sess, objWF, EIMConfig.get("ATTR_NAME_FOLDER_PREV_SEC"));	// 前セキュリティ
			assertEquals(tmpInts == null ,true);

			
		} catch (EIMException eime) {
			eime.printStackTrace();
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {
			if (sess != null) {
				sess.close();
			}
		}
		
		

		
		/**
		 * 正常(3)
		 *  ・ごみ箱の下の「フォルダA - (WF)ドキュメント1」をフォルダ21の下に移動
		 *  ・元々の引継ぎ属性はそのまま
		 *  ・新しい引継ぎ属性が適応される
		 *  ・前セキュリティが適応される
		 */
		try {
			sess = TestSessionUtil.createEIMSession();
			setEIMSessionForUnitTest(sess);
			
			// テストデータ準備
			EIMObject objA = createTestData11(sess);	// [Sec2] objA - (WF)doc1　
			EIMObject obj1 = createTestData8(sess);		// [Sec1] obj1 - obj21 - (WF)obj3
			int objAId = objA.getId();
			int obj1Id = obj1.getId();

			EIMObject obj21 = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, FOLDER_TYPE_NAME1), FOLDER_NAME21);
			int obj21Id = obj21.getId();
			EIMObject objWF = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, WF_DOCUMENT_TYPE_NAME1), WF_DOCUMENT_NAME_1);
			int objWFId = objWF.getId();
			
			// 属性情報の設定(下位引継ぎ属性)
			List itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeStrId, ATTR_VALUE_STR_BY_STRING1, false, true));
			AttributeUpdater updater = new AttributeUpdater(obj1.getId(), obj1.getName(), itemList);
			AttributeUtil.updateAttribute(sess, updater);

			itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeIntId, ATTR_VALUE_INT_BY_STRING1, false, true));
			updater = new AttributeUpdater(objA.getId(), objA.getName(), itemList);
			AttributeUtil.updateAttribute(sess, updater);

			// 再取得
			objA  = ObjectUtils.getObjectById(sess, objAId);
			
			// 属性情報更新(論理削除モード)
			AttributeUtil.updateAttributeForDelete(sess, objA, null);

			// 削除処理
			objA = ObjectUtils.getObjectById(sess, objAId);
			logicDeleteObject(sess, objA);
			
			// 再取得
			obj1  = ObjectUtils.getObjectById(sess, obj1Id);
			obj21 = ObjectUtils.getObjectById(sess, obj21Id);
			objA  = ObjectUtils.getObjectById(sess, objAId);

			// 事前確認
			viewTestData(sess, obj1, "正常(3) - 事前確認 - ごみ箱");
			viewTestData(sess, objA, "正常(3) - 事前確認 - 貼付け先");

			// リレーション設定
			changeRelation(sess, objA, obj21);

			// 再取得
			obj21 = ObjectUtils.getObjectById(sess, obj21Id);
			objA  = ObjectUtils.getObjectById(sess, objAId);
			
			// メソッドの実行
			AttributeUtil.updateAttributeForMove(sess, objA, obj21, getPath(sess, obj21), true);

			sess.commit();
			
			// 再取得
			obj1  = ObjectUtils.getObjectById(sess, obj1Id);
			objWF = ObjectUtils.getObjectById(sess, objWFId);
			
			// 結果確認
			viewTestData(sess, obj1, "正常(3) - 結果確認 - 貼付け先");
			
			//-----------------------------------------------------------------
			// 【貼付け先の引継ぎ属性が適応される属性値】
			
			// 文字列型属性
			tmpStrs = AppObjectUtil.getStrAttrs(sess, objWF, ATTR_NAME_STR);
			assertEquals(tmpStrs.length > 0 ,true);
			for (int i = 0 ; i < tmpStrs.length ; i++) {
				assertEquals(tmpStrs[i], ATTR_VALUE_STR_BY_STRING1[i]);
			}

			//-----------------------------------------------------------------
			// 【上書きされない属性値】
			
			// 数値型属性
			tmpInts = AppObjectUtil.getIntAttrs(sess, objWF, ATTR_NAME_INT);
			assertEquals(tmpInts.length > 0 ,true);
			for (int i = 0 ; i < tmpInts.length ; i++) {
				assertEquals(tmpInts[i], Integer.parseInt(ATTR_VALUE_INT_BY_STRING1[i]));
			}
			
			//-----------------------------------------------------------------
			// 【セキュリティの変更】
			
			assertEquals(objWF.getSecurity().getDefName(), SECURITY_NAME2);

			//-----------------------------------------------------------------
			// 【前セキュリティの削除】
			
			tmpInts = AppObjectUtil.getIntAttrs(sess, objWF, EIMConfig.get("ATTR_NAME_FOLDER_PREV_SEC"));	// 前セキュリティ
			assertEquals(tmpInts == null ,true);

			
		} catch (EIMException eime) {
			eime.printStackTrace();
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {
			if (sess != null) {
				sess.close();
			}
		}

		
		

		/**
		 * 異常(1)
		 * 
		 * ・下位引継ぎ属性と下位引継ぎ属性が重複する場合
		 */
		try {
			isExceptionExist = false;

			sess = TestSessionUtil.createEIMSession();
			setEIMSessionForUnitTest(sess);
			
			// テストデータ準備
			EIMObject objA = createTestData7(sess);		// [Sec2] objA - objB -objC　
			EIMObject obj1 = createTestData10(sess);	// [Sec1] obj1 - obj21
			int objAId = objA.getId();

			EIMObject obj21 = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, FOLDER_TYPE_NAME1), FOLDER_NAME21);
			int obj21Id = obj21.getId();
			
			// 属性情報の設定(下位引継ぎ属性)
			List itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeStrId, ATTR_VALUE_STR_BY_STRING1, false, true));
			AttributeUpdater updater = new AttributeUpdater(obj1.getId(), obj1.getName(), itemList);
			AttributeUtil.updateAttribute(sess, updater);

			itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeStrId, ATTR_VALUE_STR_BY_STRING1, false, true));
			updater = new AttributeUpdater(objA.getId(), objA.getName(), itemList);
			AttributeUtil.updateAttribute(sess, updater);

			// 再取得
			objA  = ObjectUtils.getObjectById(sess, objAId);
			
			// 属性情報更新(論理削除モード)
			AttributeUtil.updateAttributeForDelete(sess, objA, null);

			// 削除処理
			objA = ObjectUtils.getObjectById(sess, objAId);
			logicDeleteObject(sess, objA);
			
			// 再取得
			objA  = ObjectUtils.getObjectById(sess, objAId);
			obj21 = ObjectUtils.getObjectById(sess, obj21Id);

			// リレーション設定
			changeRelation(sess, objA, obj21);

			// 再取得
			objA  = ObjectUtils.getObjectById(sess, objAId);
			obj21 = ObjectUtils.getObjectById(sess, obj21Id);
			
			// メソッドの実行
			AttributeUtil.updateAttributeForMove(sess, objA, obj21, getPath(sess, obj21), true);
		
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
		 * 異常(2)
		 * 
		 * ・下位引継ぎ属性と名称割当て属性が重複する場合
		 */
		try {
			isExceptionExist = false;

			sess = TestSessionUtil.createEIMSession();
			setEIMSessionForUnitTest(sess);
			
			// テストデータ準備
			EIMObject objA = createTestData7(sess);		// [Sec2] objA - objB -objC　
			EIMObject obj1 = createTestData10(sess);	// [Sec1] obj1 - obj21
			int objAId = objA.getId();

			EIMObject obj21 = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, FOLDER_TYPE_NAME1), FOLDER_NAME21);
			int obj21Id = obj21.getId();
			
			// 属性情報の設定(下位引継ぎ属性)
			List itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeStrId, ATTR_VALUE_STR_BY_STRING1, false, true));
			AttributeUpdater updater = new AttributeUpdater(obj1.getId(), obj1.getName(), itemList);
			AttributeUtil.updateAttribute(sess, updater);

			itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeStrId, ATTR_VALUE_STR_BY_STRING1, true, false));
			updater = new AttributeUpdater(objA.getId(), objA.getName(), itemList);
			AttributeUtil.updateAttribute(sess, updater);

			// 再取得
			objA  = ObjectUtils.getObjectById(sess, objAId);
			
			// 属性情報更新(論理削除モード)
			AttributeUtil.updateAttributeForDelete(sess, objA, null);

			// 削除処理
			objA = ObjectUtils.getObjectById(sess, objAId);
			logicDeleteObject(sess, objA);
			
			// 再取得
			objA  = ObjectUtils.getObjectById(sess, objAId);
			obj21 = ObjectUtils.getObjectById(sess, obj21Id);

			// リレーション設定
			changeRelation(sess, objA, obj21);

			// 再取得
			objA  = ObjectUtils.getObjectById(sess, objAId);
			obj21 = ObjectUtils.getObjectById(sess, obj21Id);
			
			// メソッドの実行
			AttributeUtil.updateAttributeForMove(sess, objA, obj21, getPath(sess, obj21), true);
		
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
		 * 異常(3)
		 * 
		 * ・WFフォルダの下にWFドキュメントを貼り付ける場合
		 */
		try {
			sess = TestSessionUtil.createEIMSession();
			setEIMSessionForUnitTest(sess);
			
			// テストデータ準備
			EIMObject objA = createTestData11(sess);	// [Sec2] objA - (WF)doc1　
			createTestData8(sess);						// [Sec1] obj1 - obj21 - (WF)obj3
			int objAId = objA.getId();

			EIMObject obj3 = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, WF_FOLDER_TYPE_NAME1), WF_FOLDER_NAME_1);
			int obj3Id = obj3.getId();
			
			// 属性情報更新(論理削除モード)
			AttributeUtil.updateAttributeForDelete(sess, objA, null);

			// 削除処理
			objA = ObjectUtils.getObjectById(sess, objAId);
			logicDeleteObject(sess, objA);
			
			// 再取得
			obj3  = ObjectUtils.getObjectById(sess, obj3Id);
			objA  = ObjectUtils.getObjectById(sess, objAId);

			// リレーション設定
			changeRelation(sess, objA, obj3);

			// 再取得
			obj3 = ObjectUtils.getObjectById(sess, obj3Id);
			objA = ObjectUtils.getObjectById(sess, objAId);
			
			// メソッドの実行
			AttributeUtil.updateAttributeForMove(sess, objA, obj3, getPath(sess, obj3), true);
		
		} catch (EIMException eime) {
			isExceptionExist = true;
			assertEquals(eime.getMessage(), EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.CANTSET.UNDERWF.WFDOCFOL"));
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

		
		
/******** ここから下はごみ箱以外からの移動 ****************/		
		
		
		/**
		 * 正常(1)
		 *  ・フォルダ1をフォルダ2の下に移動
		 *  ・新しい引継ぎ属性が適応される
		 *  ・新しいセキュリティが適応される
		 */
		try {
			sess = TestSessionUtil.createEIMSession();
			setEIMSessionForUnitTest(sess);
			
			// テストデータ準備
			EIMObject destObj = createTestData4(sess);
			EIMObject moveObj = createTestData5(sess);
			int destObjId = destObj.getId();
			int moveObjId = moveObj.getId();
			
			// 属性情報の設定(下位引継ぎ属性)
			List itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeStrId, ATTR_VALUE_STR_BY_STRING1, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeIntId, ATTR_VALUE_INT_BY_STRING1, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeDateOneId, ATTR_VALUE_DATE_BY_STRING1_ONE, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeTextOneId, ATTR_VALUE_TEXT_BY_STRING1_ONE, false, true));
			AttributeUpdater updater = new AttributeUpdater(destObj.getId(), destObj.getName(), itemList);
			AttributeUtil.updateAttribute(sess, updater);

			sess.commit();
			
			destObj = ObjectUtils.getObjectById(sess, destObjId);

			// 事前確認
			viewTestData(sess, destObj, "正常(1) - 事前確認 - 貼付け先");
			viewTestData(sess, moveObj, "正常(1) - 事前確認 - 切り取り対象");
			
			// メソッドの実行
			AttributeUtil.updateAttributeForMove(sess, moveObj, destObj, getPath(sess, destObj), false);
			sess.commit();
			
			// 再取得
			destObj = ObjectUtils.getObjectById(sess, destObjId);
			moveObj = ObjectUtils.getObjectById(sess, moveObjId);
			
			// 結果確認
			viewTestData(sess, destObj, "正常(1) - 結果確認 - 貼付け先");
			viewTestData(sess, moveObj, "正常(1) - 結果確認 - 切り取り対象");
			
			//-----------------------------------------------------------------
			// 【貼付け先の引継ぎ属性で上書きされる属性値】
			
			// 文字列型属性
			tmpStrs = AppObjectUtil.getStrAttrs(sess, moveObj, ATTR_NAME_STR);
			assertEquals(tmpStrs.length > 0 ,true);
			for (int i = 0 ; i < tmpStrs.length ; i++) {
				assertEquals(tmpStrs[i], ATTR_VALUE_STR_BY_STRING1[i]);
			}
			
			// 数値型属性
			tmpInts = AppObjectUtil.getIntAttrs(sess, moveObj, ATTR_NAME_INT);
			assertEquals(tmpInts.length > 0 ,true);
			for (int i = 0 ; i < tmpInts.length ; i++) {
				assertEquals(tmpInts[i], Integer.parseInt(ATTR_VALUE_INT_BY_STRING1[i]));
			}
			
			// 日付型属性 (単数値)
			tmpDates = AppObjectUtil.getDateAttrs(sess, moveObj, ATTR_NAME_DATE_ONE);
			assertEquals(tmpDates.length > 0 ,true);
			Date tmp = DateUtils.editExpirationDate(sess, 
					StringUtils.getDateFromString(ATTR_VALUE_DATE_BY_STRING1_ONE[0], 
					EIMResource.getMessage(sess, "EIM.FORMAT.DATE")));
			assertEquals(tmpDates[0], tmp);
			
			// テキスト型属性 (単数値)
			tmpTexts = AppObjectUtil.getTextAttrs(sess, moveObj, ATTR_NAME_TEXT_ONE);
			assertEquals(tmpTexts.length > 0 ,true);
			assertEquals(tmpTexts[0], ATTR_VALUE_TEXT_BY_STRING1_ONE[0]);

			//-----------------------------------------------------------------
			// 【上書きされない属性値】

			// 文字列型属性 (単数値)
			tmpStrs = AppObjectUtil.getStrAttrs(sess, moveObj, ATTR_NAME_STR_ONE);
			assertEquals(tmpStrs.length > 0 ,true);
			assertEquals(tmpStrs[0], ATTR_VALUE_STR2_ONE);
			
			// 数値型属性 (単数値)
			tmpInts = AppObjectUtil.getIntAttrs(sess, moveObj, ATTR_NAME_INT_ONE);
			assertEquals(tmpInts.length > 0 ,true);
			for (int i = 0 ; i < tmpInts.length ; i++) {
				assertEquals(tmpInts[i], ATTR_VALUE_INT2_ONE);
			}
			
			// 日付型属性
			tmpDates = AppObjectUtil.getDateAttrs(sess, moveObj, ATTR_NAME_DATE);
			assertEquals(tmpDates.length > 0 ,true);
			for (int i = 0 ; i < tmpDates.length ; i++) {
				assertEquals(tmpDates[i], ATTR_VALUE_DATE2[i]);
			}
			
			// テキスト型属性
			tmpTexts = AppObjectUtil.getTextAttrs(sess, moveObj, ATTR_NAME_TEXT);
			assertEquals(tmpTexts.length > 0 ,true);
			for (int i = 0 ; i < tmpTexts.length ; i++) {
				assertEquals(tmpTexts[i], ATTR_VALUE_TEXT2[i]);
			}
			
		} catch (EIMException eime) {
			eime.printStackTrace();
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {
			if (sess != null) {
				sess.close();
			}
		}
		
		
		
		
		/**
		 * 正常(2)
		 *  ・「親-子-孫」構成の子フォルダを別構成「WFフォルダ-子」の下に移動
		 *  ・古い引継ぎ属性は削除される
		 *  ・新しい引継ぎ属性が適応される
		 *  ・新しいセキュリティが適応される
		 *  ・上位WFフォルダが更新される
		 */
		try {
			sess = TestSessionUtil.createEIMSession();
			setEIMSessionForUnitTest(sess);
			
			// テストデータ準備
			EIMObject wfObj = createTestData6(sess);
			EIMObject wfChildObj = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, FOLDER_TYPE_NAME1), FOLDER_NAME1);
			
			EIMObject moveParentObj = createTestData7(sess);
			EIMObject moveObj = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, FOLDER_TYPE_NAME1), FOLDER_NAME_B);
			EIMObject moveChildObj = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, FOLDER_TYPE_NAME1), FOLDER_NAME_C);
			
			
			int wfObjId = wfObj.getId();
			int wfChildObjId = wfChildObj.getId();
			int moveParentObjId = moveParentObj.getId();
			int moveObjId = moveObj.getId();
			int moveChildObjId = moveChildObj.getId();
			
			// 属性情報の設定(下位引継ぎ属性)
			// → WFフォルダ
			List itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeStrId, ATTR_VALUE_STR_BY_STRING1, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeIntId, ATTR_VALUE_INT_BY_STRING1, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeDateOneId, ATTR_VALUE_DATE_BY_STRING1_ONE, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeTextOneId, ATTR_VALUE_TEXT_BY_STRING1_ONE, false, true));
			AttributeUpdater updater = new AttributeUpdater(wfObj.getId(), wfObj.getName(), itemList);
			AttributeUtil.updateAttribute(sess, updater);

			// → 親
			itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeStrId, ATTR_VALUE_STR_BY_STRING2, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeTextId, ATTR_VALUE_TEXT_BY_STRING1, false, true));
			updater = new AttributeUpdater(moveParentObj.getId(), moveParentObj.getName(), itemList);
			AttributeUtil.updateAttribute(sess, updater);
			
			// → 子
			itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeIntId, ATTR_VALUE_INT_BY_STRING1, false, false));
			itemList.add(new AttributeUpdaterItem(attrTypeIntOneId, ATTR_VALUE_INT_BY_STRING1_ONE, false, true));
			updater = new AttributeUpdater(moveObj.getId(), moveObj.getName(), itemList);
			AttributeUtil.updateAttribute(sess, updater);
			
			sess.commit();
			
			wfObj = ObjectUtils.getObjectById(sess, wfObjId);
			wfChildObj = ObjectUtils.getObjectById(sess, wfChildObjId);
			moveParentObj = ObjectUtils.getObjectById(sess, moveParentObjId);
			moveObj = ObjectUtils.getObjectById(sess, moveObjId);

			// 事前確認
			viewTestData(sess, wfObj, "正常(2) - 事前確認 - 貼付け先の親");
			viewTestData(sess, moveParentObj, "正常(2) - 事前確認 - 切り取り対象の親");

			// リレーション設定
			changeRelation(sess, moveObj, wfChildObj);
			
			// メソッドの実行
			AttributeUtil.updateAttributeForMove(sess, moveObj, wfChildObj, getPath(sess, wfChildObj), false);
			sess.commit();
			
			// 再取得
			wfObj = ObjectUtils.getObjectById(sess, wfObjId);
			wfChildObj = ObjectUtils.getObjectById(sess, wfChildObjId);
			moveParentObj = ObjectUtils.getObjectById(sess, moveParentObjId);
			moveObj = ObjectUtils.getObjectById(sess, moveObjId);
			moveChildObj = ObjectUtils.getObjectById(sess, moveChildObjId);
			
			// 結果確認
			viewTestData(sess, wfObj, "正常(2) - 結果確認 - 貼付け先の親");
			viewTestData(sess, moveParentObj, "正常(2) - 結果確認 - 切り取り対象の親");
		
			
			//-----------------------------------------------------------------
			// 【古い引継ぎ属性の削除】
			
			// テキスト型属性
			tmpTexts = AppObjectUtil.getTextAttrs(sess, moveChildObj, ATTR_NAME_TEXT);
			assertEquals(tmpTexts == null ,true);
			
			//-----------------------------------------------------------------
			// 【古い引継ぎ属性を新しい引継ぎ属性で上書き】
			
			// 文字列型属性
			tmpStrs = AppObjectUtil.getStrAttrs(sess, moveChildObj, ATTR_NAME_STR);
			assertEquals(tmpStrs.length > 0 ,true);
			for (int i = 0 ; i < tmpStrs.length ; i++) {
				assertEquals(tmpStrs[i], ATTR_VALUE_STR_BY_STRING1[i]);
			}
			
			//-----------------------------------------------------------------
			// 【セキュリティの変更】
			assertEquals(moveChildObj.getSecurity().getDefName(), SECURITY_NAME1);

			//-----------------------------------------------------------------
			// 【上位WFフォルダの変更】
			tmpInts = AppObjectUtil.getIntAttrs(sess, moveChildObj, EIMConfig.get("ATTR_NAME_FOLDER_HIGHER_WF_FOLDER"));
			assertEquals(tmpInts[0], wfObjId);
			
		} catch (EIMException eime) {
			eime.printStackTrace();
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {
			if (sess != null) {
				sess.close();
			}
		}
		

		
		
		/**
		 * 正常(3)
		 *  ・「親1-子-孫WFフォルダ」構成の子フォルダを「親A-子-孫」構成の孫の下に移動
		 */
		try {
			sess = TestSessionUtil.createEIMSession();
			setEIMSessionForUnitTest(sess);
			
			// テストデータ準備
			EIMObject obj1 = createTestData8(sess);
			EIMObject obj21 = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, FOLDER_TYPE_NAME1), FOLDER_NAME21);
			EIMObject wfObj = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, WF_FOLDER_TYPE_NAME1), WF_FOLDER_NAME_1);
			
			EIMObject objA = createTestData7(sess);
			EIMObject objC = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, FOLDER_TYPE_NAME1), FOLDER_NAME_C);
			
			int obj1Id = obj1.getId();
			int obj21Id = obj21.getId();
			int wfObjId = wfObj.getId();
			int objAId = objA.getId();
			int objCId = objC.getId();
			
			// 属性情報の設定(下位引継ぎ属性)
			// → 親1
			List itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeStrId, ATTR_VALUE_STR_BY_STRING1, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeIntId, ATTR_VALUE_INT_BY_STRING1, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeDateOneId, ATTR_VALUE_DATE_BY_STRING1_ONE, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeTextOneId, ATTR_VALUE_TEXT_BY_STRING1_ONE, false, true));
			AttributeUpdater updater = new AttributeUpdater(obj1.getId(), obj1.getName(), itemList);
			AttributeUtil.updateAttribute(sess, updater);

			// → 孫WF
			itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeTextId, ATTR_VALUE_TEXT_BY_STRING1, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeIntOneId, ATTR_VALUE_INT_BY_STRING1_ONE, false, true));
			updater = new AttributeUpdater(wfObj.getId(), wfObj.getName(), itemList);
			AttributeUtil.updateAttribute(sess, updater);
			
			// → 親A
			itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeStrId, ATTR_VALUE_STR_BY_STRING2, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeDateId, ATTR_VALUE_DATE_BY_STRING1, false, true));
			updater = new AttributeUpdater(objA.getId(), objA.getName(), itemList);
			AttributeUtil.updateAttribute(sess, updater);
			
			sess.commit();
			
			obj1  = ObjectUtils.getObjectById(sess, obj1Id);
			obj21 = ObjectUtils.getObjectById(sess, obj21Id);
			objA  = ObjectUtils.getObjectById(sess, objAId);
			objC  = ObjectUtils.getObjectById(sess, objCId);

			// 事前確認
			viewTestData(sess, obj1, "正常(3) - 事前確認 - 切り取り対象の親");
			viewTestData(sess, objA, "正常(3) - 事前確認 - 貼付け先の親");

			// リレーション設定
			changeRelation(sess, obj21, objC);
			
			// メソッドの実行
			AttributeUtil.updateAttributeForMove(sess, obj21, objC, getPath(sess, objC), false);
			sess.commit();
			
			// 再取得
			obj1  = ObjectUtils.getObjectById(sess, obj1Id);
			wfObj = ObjectUtils.getObjectById(sess, wfObjId);
			objA  = ObjectUtils.getObjectById(sess, objAId);
			objC  = ObjectUtils.getObjectById(sess, objCId);
			
			// 結果確認
			viewTestData(sess, obj1, "正常(3) - 結果確認 - 切り取り対象の親");
			viewTestData(sess, objA, "正常(3) - 結果確認 - 貼付け先の親");
		
			//-----------------------------------------------------------------
			// 【古い引継ぎ属性の削除】
			
			// 数値型属性
			tmpInts = AppObjectUtil.getIntAttrs(sess, wfObj, ATTR_NAME_INT);
			assertEquals(tmpInts == null ,true);
			
			// 日付型属性(単数)
			tmpDates = AppObjectUtil.getDateAttrs(sess, wfObj, ATTR_NAME_DATE_ONE);
			assertEquals(tmpDates == null ,true);
			
			// テキスト型属性(単数)
			tmpTexts = AppObjectUtil.getTextAttrs(sess, wfObj, ATTR_NAME_TEXT_ONE);
			assertEquals(tmpTexts == null ,true);
			
			//-----------------------------------------------------------------
			// 【古い引継ぎ属性を新しい引継ぎ属性で上書き】
			
			// 文字列型属性
			tmpStrs = AppObjectUtil.getStrAttrs(sess, wfObj, ATTR_NAME_STR);
			assertEquals(tmpStrs.length > 0 ,true);
			for (int i = 0 ; i < tmpStrs.length ; i++) {
				assertEquals(tmpStrs[i], ATTR_VALUE_STR_BY_STRING2[i]);
			}

			//-----------------------------------------------------------------
			// 【新規に引継ぎ属性として更新】
			
			// 日付型属性
			tmpDates = AppObjectUtil.getDateAttrs(sess, wfObj, ATTR_NAME_DATE);
			assertEquals(tmpDates.length > 0 ,true);
			for (int i = 0 ; i < tmpDates.length ; i++) {
				Date tmp = DateUtils.editExpirationDate(sess, 
						StringUtils.getDateFromString(ATTR_VALUE_DATE_BY_STRING1[i], 
						EIMResource.getMessage(sess, "EIM.FORMAT.DATE")));
				assertEquals(tmpDates[i], tmp);
			}

			//-----------------------------------------------------------------
			// 【自分から始まる引継ぎ属性は元のまま】
			
			// テキスト型属性
			tmpTexts = AppObjectUtil.getTextAttrs(sess, wfObj, ATTR_NAME_TEXT);
			assertEquals(tmpTexts.length > 0 ,true);
			for (int i = 0 ; i < tmpTexts.length ; i++) {
				assertEquals(tmpTexts[i], ATTR_VALUE_TEXT_BY_STRING1[i]);
			}

			// 数値型属性
			tmpInts = AppObjectUtil.getIntAttrs(sess, wfObj, ATTR_NAME_INT_ONE);
			assertEquals(tmpInts.length > 0 ,true);
			for (int i = 0 ; i < tmpInts.length ; i++) {
				assertEquals(tmpInts[i], Integer.parseInt(ATTR_VALUE_INT_BY_STRING1_ONE[i]));
			}
			
			//-----------------------------------------------------------------
			// 【セキュリティの変更】
			
			assertEquals(wfObj.getSecurity().getDefName(), SECURITY_NAME2);
			
		} catch (EIMException eime) {
			eime.printStackTrace();
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {
			if (sess != null) {
				sess.close();
			}
		}
		
		
		
		
		/**
		 * 正常(4)
		 * 
		 * ・ドキュメントを含むフォルダをWFフォルダー下に貼付けする場合
		 */
		try {
			sess = TestSessionUtil.createEIMSession();
			setEIMSessionForUnitTest(sess);
			
			// テストデータ準備
			EIMObject obj1 = createTestData8(sess);
			EIMObject wfObj = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, WF_FOLDER_TYPE_NAME1), WF_FOLDER_NAME_1);

			EIMObject objA = createTestData9(sess);
			EIMObject doc1 = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, DOCUMENT_TYPE_NAME1), DOC_NAME1);
			
			int obj1Id = obj1.getId();
			int wfObjId = wfObj.getId();
			int objAId = objA.getId();
			int doc1Id = doc1.getId();
			
			// 属性情報の設定(下位引継ぎ属性)
			// → 親1
			List itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeStrId, ATTR_VALUE_STR_BY_STRING1, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeIntId, ATTR_VALUE_INT_BY_STRING1, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeDateOneId, ATTR_VALUE_DATE_BY_STRING1_ONE, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeTextOneId, ATTR_VALUE_TEXT_BY_STRING1_ONE, false, true));
			AttributeUpdater updater = new AttributeUpdater(obj1.getId(), obj1.getName(), itemList);
			AttributeUtil.updateAttribute(sess, updater);

			// → 親A
			itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeStrId, ATTR_VALUE_STR_BY_STRING2, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeDateId, ATTR_VALUE_DATE_BY_STRING1, false, true));
			updater = new AttributeUpdater(objA.getId(), objA.getName(), itemList);
			AttributeUtil.updateAttribute(sess, updater);
			
			// → 子ドキュメント
			itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeTextId, ATTR_VALUE_TEXT_BY_STRING1, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeIntOneId, ATTR_VALUE_INT_BY_STRING1_ONE, false, true));
			updater = new AttributeUpdater(doc1.getId(), doc1.getName(), itemList);
			AttributeUtil.updateAttribute(sess, updater);
			
			sess.commit();
			
			obj1  = ObjectUtils.getObjectById(sess, obj1Id);
			wfObj = ObjectUtils.getObjectById(sess, wfObjId);
			objA  = ObjectUtils.getObjectById(sess, objAId);
			doc1  = ObjectUtils.getObjectById(sess, doc1Id);

			// 事前確認
			viewTestData(sess, objA, "正常(4) - 事前確認 - 切り取り対象の親");
			viewTestData(sess, obj1, "正常(4) - 事前確認 - 貼付け先の親");

			// リレーション設定
			changeRelation(sess, doc1, wfObj);
			
			// メソッドの実行
			AttributeUtil.updateAttributeForMove(sess, doc1, wfObj, getPath(sess, wfObj), false);
			sess.commit();
			
			// 再取得
			obj1  = ObjectUtils.getObjectById(sess, obj1Id);
			wfObj = ObjectUtils.getObjectById(sess, wfObjId);
			objA  = ObjectUtils.getObjectById(sess, objAId);
			doc1  = ObjectUtils.getObjectById(sess, doc1Id);
			
			// 結果確認
			viewTestData(sess, objA, "正常(4) - 結果確認 - 切り取り対象の親");
			viewTestData(sess, obj1, "正常(4) - 結果確認 - 貼付け先の親");
		
			//-----------------------------------------------------------------
			// 【古い引継ぎ属性の削除】
			
			// 日付型属性(単数)
			tmpDates = AppObjectUtil.getDateAttrs(sess, wfObj, ATTR_NAME_DATE);
			assertEquals(tmpDates == null ,true);
			
			//-----------------------------------------------------------------
			// 【古い引継ぎ属性を新しい引継ぎ属性で上書き】
			
			// 文字列型属性
			tmpStrs = AppObjectUtil.getStrAttrs(sess, doc1, ATTR_NAME_STR);
			assertEquals(tmpStrs.length > 0 ,true);
			for (int i = 0 ; i < tmpStrs.length ; i++) {
				assertEquals(tmpStrs[i], ATTR_VALUE_STR_BY_STRING1[i]);
			}

			//-----------------------------------------------------------------
			// 【新規に引継ぎ属性として更新】
			
			// 数値型属性
			tmpInts = AppObjectUtil.getIntAttrs(sess, doc1, ATTR_NAME_INT);
			assertEquals(tmpInts.length > 0 ,true);
			for (int i = 0 ; i < tmpInts.length ; i++) {
				assertEquals(tmpInts[i], Integer.parseInt(ATTR_VALUE_INT_BY_STRING1[i]));
			}
			
			// 日付型属性 (単数値)
			tmpDates = AppObjectUtil.getDateAttrs(sess, doc1, ATTR_NAME_DATE_ONE);
			assertEquals(tmpDates.length > 0 ,true);
			Date tmp = DateUtils.editExpirationDate(sess, 
					StringUtils.getDateFromString(ATTR_VALUE_DATE_BY_STRING1_ONE[0], 
					EIMResource.getMessage(sess, "EIM.FORMAT.DATE")));
			assertEquals(tmpDates[0], tmp);
			
			// テキスト型属性 (単数値)
			tmpTexts = AppObjectUtil.getTextAttrs(sess, doc1, ATTR_NAME_TEXT_ONE);
			assertEquals(tmpTexts.length > 0 ,true);
			assertEquals(tmpTexts[0], ATTR_VALUE_TEXT_BY_STRING1_ONE[0]);
			
			//-----------------------------------------------------------------
			// 【自分から始まる引継ぎ属性は元のまま】
			
			// テキスト型属性
			tmpTexts = AppObjectUtil.getTextAttrs(sess, doc1, ATTR_NAME_TEXT);
			assertEquals(tmpTexts.length > 0 ,true);
			for (int i = 0 ; i < tmpTexts.length ; i++) {
				assertEquals(tmpTexts[i], ATTR_VALUE_TEXT_BY_STRING1[i]);
			}

			// 数値型属性
			tmpInts = AppObjectUtil.getIntAttrs(sess, doc1, ATTR_NAME_INT_ONE);
			assertEquals(tmpInts.length > 0 ,true);
			for (int i = 0 ; i < tmpInts.length ; i++) {
				assertEquals(tmpInts[i], Integer.parseInt(ATTR_VALUE_INT_BY_STRING1_ONE[i]));
			}

			
			//-----------------------------------------------------------------
			// 【セキュリティの変更】
			
			assertEquals(doc1.getSecurity().getDefName(), SECURITY_NAME1);

			
		} catch (EIMException eime) {
			eime.printStackTrace();
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {
			if (sess != null) {
				sess.close();
			}
		}
		
		
		
		
		/**
		 * 正常(5)
		 * 
		 * ・ドキュメントを含むフォルダをWFフォルダー下に貼付け、
		 *   その後、WFなしフォルダ下に貼り付けた場合
		 */
		try {
			sess = TestSessionUtil.createEIMSession();
			setEIMSessionForUnitTest(sess);
			
			// テストデータ準備
			EIMObject obj1 = createTestData8(sess);		// obj1 - obj21 - (WF)obj3
			EIMObject wfObj = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, WF_FOLDER_TYPE_NAME1), WF_FOLDER_NAME_1);

			EIMObject objA = createTestData9(sess);		// objA - doc1
			EIMObject doc1 = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, DOCUMENT_TYPE_NAME1), DOC_NAME1);
			
			int obj1Id = obj1.getId();
			int wfObjId = wfObj.getId();
			int objAId = objA.getId();
			int doc1Id = doc1.getId();
			
			// 属性情報の設定(下位引継ぎ属性)
			// → 親1
			List itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeStrId, ATTR_VALUE_STR_BY_STRING1, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeIntId, ATTR_VALUE_INT_BY_STRING1, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeDateOneId, ATTR_VALUE_DATE_BY_STRING1_ONE, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeTextOneId, ATTR_VALUE_TEXT_BY_STRING1_ONE, false, true));
			AttributeUpdater updater = new AttributeUpdater(obj1.getId(), obj1.getName(), itemList);
			AttributeUtil.updateAttribute(sess, updater);

			// → 親A
			itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeStrId, ATTR_VALUE_STR_BY_STRING2, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeDateId, ATTR_VALUE_DATE_BY_STRING1, false, true));
			updater = new AttributeUpdater(objA.getId(), objA.getName(), itemList);
			AttributeUtil.updateAttribute(sess, updater);
			
			// → 子ドキュメント
			itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeTextId, ATTR_VALUE_TEXT_BY_STRING1, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeIntOneId, ATTR_VALUE_INT_BY_STRING1_ONE, false, true));
			updater = new AttributeUpdater(doc1.getId(), doc1.getName(), itemList);
			AttributeUtil.updateAttribute(sess, updater);
			
			sess.commit();
			
			// 再取得
			obj1  = ObjectUtils.getObjectById(sess, obj1Id);
			wfObj = ObjectUtils.getObjectById(sess, wfObjId);
			objA  = ObjectUtils.getObjectById(sess, objAId);
			doc1  = ObjectUtils.getObjectById(sess, doc1Id);

			// リレーション設定
			changeRelation(sess, doc1, wfObj);
			
			// メソッドの実行
			AttributeUtil.updateAttributeForMove(sess, doc1, wfObj, getPath(sess, wfObj), false);
			sess.commit();
			
			EIMObject obj22 = createTestData12(sess);	// obj22
			int obj22Id = obj22.getId();
			
			// 再取得
			doc1  = ObjectUtils.getObjectById(sess, doc1Id);

			// リレーション設定
			changeRelation(sess, doc1, obj22);

			// 再取得
			doc1  = ObjectUtils.getObjectById(sess, doc1Id);
			obj22 = ObjectUtils.getObjectById(sess, obj22Id);
			
			// メソッドの実行
			AttributeUtil.updateAttributeForMove(sess, doc1, obj22, getPath(sess, obj22), false);
			sess.commit();

			// 再取得
			obj22  = ObjectUtils.getObjectById(sess, obj22Id);
			doc1  = ObjectUtils.getObjectById(sess, doc1Id);
			
			// 結果確認
			viewTestData(sess, obj22, "正常(5) - 結果確認");
		
			//-----------------------------------------------------------------
			// 【ステータスが消えていること】
			
			assertEquals(doc1.getStatus() == null, true);

			
		} catch (EIMException eime) {
			eime.printStackTrace();
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {
			if (sess != null) {
				sess.close();
			}
		}
		

		
		
		/**
		 * 異常(1)
		 * 
		 * ・切り取り対象オブジェクトに対する権限がない場合
		 */
		try {
			isExceptionExist = false;

			sess = new EIMSession();
			sess.setConsole();
			
			// テストデータ準備
			EIMObject destObj = createTestData7(sess);
			EIMObject moveObj = createTestData8(sess);
			
			EIMUser nonAuthUser = UserUtils.getUserByCode(sess, NON_AUTH_SEC1_USER_CODE);
			sess.setUser(nonAuthUser);
			
			// メソッドの実行
			AttributeUtil.updateAttributeForMove(sess, moveObj, destObj, getPath(sess, destObj), false);
		
		} catch (EIMException eime) {

			isExceptionExist = true;
			assertEquals(eime.getMessage(), EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOCUTANDPASTEROLE"));
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
		 * 異常(2)
		 * 
		 * ・貼付け先オブジェクトに対する権限がない場合
		 */
		try {
			isExceptionExist = false;

			sess = new EIMSession();
			sess.setConsole();
			
			// テストデータ準備
			EIMObject destObj = createTestData7(sess);
			EIMObject moveObj = createTestData8(sess);
			
			EIMUser nonAuthUser = UserUtils.getUserByCode(sess, NON_AUTH_SEC2_USER_CODE);
			sess.setUser(nonAuthUser);
			
			// メソッドの実行
			AttributeUtil.updateAttributeForMove(sess, moveObj, destObj, getPath(sess, destObj), false);
		
		} catch (EIMException eime) {

			isExceptionExist = true;
			assertEquals(eime.getMessage(), EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOCUTANDPASTEROLE"));
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
		 * 異常(3)
		 * 
		 * ・切り取り対象オブジェクトの親に対するフォルダ構成管理権限がない場合
		 */
		try {
			isExceptionExist = false;

			sess = new EIMSession();
			sess.setConsole();
			
			// テストデータ準備
			EIMObject obj1 = createTestData8(sess);
			EIMObject obj21 = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, FOLDER_TYPE_NAME1), FOLDER_NAME21);
			
			createTestData7(sess);
			EIMObject objC = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, FOLDER_TYPE_NAME1), FOLDER_NAME_C);
			
			// フォルダ構成管理権限を設定
			EIMSecurity sec = SecurityUtils.getSecurityByName(sess, SECURITY_NAME1);
			AppObjectUtil.setAttr(sess, obj1, EIMConfig.get("ATTR_NAME_FOLDER_LOW_FOLDER_SEC"), sec.getId());	// 下位フォルダ管理セキュリティ
			
			sess.commit();
			
			EIMUser nonAuthUser = UserUtils.getUserByCode(sess, NON_AUTH_SEC2_USER_CODE);
			sess.setUser(nonAuthUser);
			
			// メソッドの実行
			AttributeUtil.updateAttributeForMove(sess, obj21, objC, "", false);
		
		} catch (EIMException eime) {
			isExceptionExist = true;
			assertEquals(eime.getMessage(), EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOCUTANDPASTEROLE"));
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
		 * 異常(4)
		 * ・貼付け対象オブジェクトに対するフォルダ構成管理権限がない場合
		 */
		try {
			isExceptionExist = false;

			sess = new EIMSession();
			sess.setConsole();
			
			// テストデータ準備
			createTestData8(sess);
			EIMObject obj21 = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, FOLDER_TYPE_NAME1), FOLDER_NAME21);
			createTestData7(sess);
			EIMObject objC = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, FOLDER_TYPE_NAME1), FOLDER_NAME_C);
			
			// フォルダ構成管理権限を設定
			EIMSecurity sec = SecurityUtils.getSecurityByName(sess, SECURITY_NAME3);
			AppObjectUtil.setAttr(sess, objC, EIMConfig.get("ATTR_NAME_FOLDER_LOW_FOLDER_SEC"), sec.getId());	// 下位フォルダ管理セキュリティ

			EIMUser nonAuthUser = UserUtils.getUserByCode(sess, NON_AUTH_SEC2_USER_CODE);
			sess.setUser(nonAuthUser);
			
			// メソッドの実行
			AttributeUtil.updateAttributeForMove(sess, obj21, objC, "", false);
		
		} catch (EIMException eime) {
			isExceptionExist = true;
			assertEquals(eime.getMessage(), EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOCUTANDPASTEROLE"));
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
		 * 異常(5)
		 * ・名称割当て属性と下位引継ぎ属性が重複してる場合
		 */
		try {
			isExceptionExist = false;

			sess = TestSessionUtil.createEIMSession();
			setEIMSessionForUnitTest(sess);
			
			// テストデータ準備
			createTestData8(sess);
			EIMObject obj21 = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, FOLDER_TYPE_NAME1), FOLDER_NAME21);
			int obj21Id = obj21.getId();
			
			createTestData7(sess);
			EIMObject objC = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, FOLDER_TYPE_NAME1), FOLDER_NAME_C);
			int objCId = objC.getId();

			// 対象オブジェクトの名称割当て属性と親オブジェクトの下位引継ぎ属性が重複した場合はエラー
			List itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeStrId, ATTR_VALUE_STR_BY_STRING1, false, true));
			AttributeUpdater updater = new AttributeUpdater(obj21.getId(), obj21.getName(), itemList);
			AttributeUtil.updateAttribute(sess, updater);
			
			itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeStrId, ATTR_VALUE_STR_BY_STRING2, true, true));
			updater = new AttributeUpdater(objC.getId(), "NAME------", itemList);
			AttributeUtil.updateAttribute(sess, updater);
			
			sess.commit();
			
			objC  = ObjectUtils.getObjectById(sess, objCId);
			obj21 = ObjectUtils.getObjectById(sess, obj21Id);

			// リレーション設定
			changeRelation(sess, objC, obj21);
			
			// メソッドの実行
			AttributeUtil.updateAttributeForMove(sess, objC, obj21,  "XXX", false);
			
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
		 * 異常(6)
		 * ・WFの下にWFの場合
		 */
		try {
			isExceptionExist = false;

			sess = TestSessionUtil.createEIMSession();
			setEIMSessionForUnitTest(sess);
			
			// テストデータ準備
			createTestData6(sess);
			EIMObject obj1 = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, FOLDER_TYPE_NAME1), FOLDER_NAME1);
			
			createTestData8(sess);
			EIMObject wfObj = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, WF_FOLDER_TYPE_NAME1), WF_FOLDER_NAME_1);

			// リレーション設定
			changeRelation(sess, wfObj, obj1);
			
			// メソッドの実行
			AttributeUtil.updateAttributeForMove(sess, wfObj, obj1,  "", false);
			
		} catch (EIMException eime) {
			isExceptionExist = true;
			assertEquals(eime.getMessage(), EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.CANTSET.UNDERWF.WFDOCFOL"));
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
		 * 異常(7)
		 * ・貼付け先の下位引継ぎ属性と、切り取り対象の下位引継ぎ属性が重複してる場合
		 */
		try {
			isExceptionExist = false;

			sess = TestSessionUtil.createEIMSession();
			setEIMSessionForUnitTest(sess);
			
			// テストデータ準備
			createTestData8(sess);	// obj1 - ojj21 - (WF)obj3
			EIMObject obj21 = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, FOLDER_TYPE_NAME1), FOLDER_NAME21);
			int obj21Id = obj21.getId();
			
			createTestData7(sess);	//	objA - objB - objC
			EIMObject objC = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, FOLDER_TYPE_NAME1), FOLDER_NAME_C);
			int objCId = objC.getId();

			List itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeStrId, ATTR_VALUE_STR_BY_STRING1, false, true));
			AttributeUpdater updater = new AttributeUpdater(obj21.getId(), obj21.getName(), itemList);
			AttributeUtil.updateAttribute(sess, updater);
			
			itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeStrId, ATTR_VALUE_STR_BY_STRING2, false, true));
			updater = new AttributeUpdater(objC.getId(), objC.getName(), itemList);
			AttributeUtil.updateAttribute(sess, updater);
			
			sess.commit();
			
			objC  = ObjectUtils.getObjectById(sess, objCId);
			obj21 = ObjectUtils.getObjectById(sess, obj21Id);

			// リレーション設定
			changeRelation(sess, objC, obj21);
			
			// メソッドの実行
			AttributeUtil.updateAttributeForMove(sess, objC, obj21,  "XXX", false);
			
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
	 * AttributeUtil.updateAttribute(EIMSession, AttributeUpdater)のためのテスト・メソッド
	 * 
	 * @throws Exception 
	 */	
	public void testUpdateAttribute() throws Exception {
		

		if (!isPreparationOK) return;
		
		EIMSession sess = null;
		
		String[] tmpStrs = null; 
		int[] tmpInts = null;
		Date[] tmpDates = null;
		String[] tmpTexts = null;
		
		boolean isExceptionExist = false;
		
		/**
		 * 正常(1)
		 *  ・親フォルダに複数値属性を更新
		 *  ・子フォルダ、孫フォルダへ複数値属性の引継ぎ
		 */
		try {
			sess = TestSessionUtil.createEIMSession();
			setEIMSessionForUnitTest(sess);
			
			// テストデータ準備
			EIMObject baseObj = createTestData1(sess);
			int baseObjId = baseObj.getId();
			
			// 事前確認
			viewTestData(sess, baseObj, "正常(1) - 事前確認");
			
			// 属性情報の設定
			List itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeStrId, ATTR_VALUE_STR_BY_STRING1, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeIntId, ATTR_VALUE_INT_BY_STRING1, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeDateId, ATTR_VALUE_DATE_BY_STRING1, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeTextId, ATTR_VALUE_TEXT_BY_STRING1, false, true));
			AttributeUpdater updater = new AttributeUpdater(baseObj.getId(), baseObj.getName(), itemList);
			
			// メソッドの実行
			AttributeUtil.updateAttribute(sess, updater);
			sess.commit();
			
			// 再取得
			baseObj = ObjectUtils.getObjectById(sess, baseObjId);
			
			// 結果確認
			viewTestData(sess, baseObj, "正常(1) - 結果確認");

			//-----------------------------------------------------------------
			// 【親オブジェクト 属性の更新を確認】
			
			// 文字列型属性
			tmpStrs = AppObjectUtil.getStrAttrs(sess, baseObj, ATTR_NAME_STR);
			assertEquals(tmpStrs.length > 0 ,true);
			for (int i = 0 ; i < tmpStrs.length ; i++) {
				assertEquals(tmpStrs[i], ATTR_VALUE_STR_BY_STRING1[i]);
			}
			
			// 数値型属性
			tmpInts = AppObjectUtil.getIntAttrs(sess, baseObj, ATTR_NAME_INT);
			assertEquals(tmpInts.length > 0 ,true);
			for (int i = 0 ; i < tmpInts.length ; i++) {
				assertEquals(tmpInts[i], Integer.parseInt(ATTR_VALUE_INT_BY_STRING1[i]));
			}
			
			// 日付型属性
			tmpDates = AppObjectUtil.getDateAttrs(sess, baseObj, ATTR_NAME_DATE);
			assertEquals(tmpDates.length > 0 ,true);
			for (int i = 0 ; i < tmpDates.length ; i++) {
				Date tmp = DateUtils.editExpirationDate(sess, 
						StringUtils.getDateFromString(ATTR_VALUE_DATE_BY_STRING1[i], 
						EIMResource.getMessage(sess, "EIM.FORMAT.DATE")));
				assertEquals(tmpDates[i], tmp);
			}
			
			// テキスト型属性
			tmpTexts = AppObjectUtil.getTextAttrs(sess, baseObj, ATTR_NAME_TEXT);
			assertEquals(tmpTexts.length > 0 ,true);
			for (int i = 0 ; i < tmpTexts.length ; i++) {
				assertEquals(tmpTexts[i], ATTR_VALUE_TEXT_BY_STRING1[i]);
			}
			
			//-----------------------------------------------------------------
			// 【子オブジェクト 属性の更新を確認】
			
			EIMObject childObj = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, FOLDER_TYPE_NAME1), FOLDER_NAME21);
			
			// 文字列型属性
			tmpStrs = AppObjectUtil.getStrAttrs(sess, childObj, ATTR_NAME_STR);
			assertEquals(tmpStrs.length > 0 ,true);
			for (int i = 0 ; i < tmpStrs.length ; i++) {
				assertEquals(tmpStrs[i], ATTR_VALUE_STR_BY_STRING1[i]);
			}
			
			// 数値型属性
			tmpInts = AppObjectUtil.getIntAttrs(sess, childObj, ATTR_NAME_INT);
			assertEquals(tmpInts.length > 0 ,true);
			for (int i = 0 ; i < tmpInts.length ; i++) {
				assertEquals(tmpInts[i], Integer.parseInt(ATTR_VALUE_INT_BY_STRING1[i]));
			}
			
			// 日付型属性
			tmpDates = AppObjectUtil.getDateAttrs(sess, childObj, ATTR_NAME_DATE);
			assertEquals(tmpDates.length > 0 ,true);
			for (int i = 0 ; i < tmpDates.length ; i++) {
				Date tmp = DateUtils.editExpirationDate(sess, 
						StringUtils.getDateFromString(ATTR_VALUE_DATE_BY_STRING1[i], 
						EIMResource.getMessage(sess, "EIM.FORMAT.DATE")));
				assertEquals(tmpDates[i], tmp);
			}
			
			// テキスト型属性
			tmpTexts = AppObjectUtil.getTextAttrs(sess, childObj, ATTR_NAME_TEXT);
			assertEquals(tmpTexts.length > 0 ,true);
			for (int i = 0 ; i < tmpTexts.length ; i++) {
				assertEquals(tmpTexts[i], ATTR_VALUE_TEXT_BY_STRING1[i]);
			}
			
			//-----------------------------------------------------------------
			// 【孫オブジェクト 属性の更新を確認】
			
			EIMObject magoObj = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, FOLDER_TYPE_NAME1), FOLDER_NAME3);
			
			// 文字列型属性
			tmpStrs = AppObjectUtil.getStrAttrs(sess, magoObj, ATTR_NAME_STR);
			assertEquals(tmpStrs.length > 0 ,true);
			for (int i = 0 ; i < tmpStrs.length ; i++) {
				assertEquals(tmpStrs[i], ATTR_VALUE_STR_BY_STRING1[i]);
			}
			
			// 数値型属性
			tmpInts = AppObjectUtil.getIntAttrs(sess, magoObj, ATTR_NAME_INT);
			assertEquals(tmpInts.length > 0 ,true);
			for (int i = 0 ; i < tmpInts.length ; i++) {
				assertEquals(tmpInts[i], Integer.parseInt(ATTR_VALUE_INT_BY_STRING1[i]));
			}
			
			// 日付型属性
			tmpDates = AppObjectUtil.getDateAttrs(sess, magoObj, ATTR_NAME_DATE);
			assertEquals(tmpDates.length > 0 ,true);
			for (int i = 0 ; i < tmpDates.length ; i++) {
				Date tmp = DateUtils.editExpirationDate(sess, 
						StringUtils.getDateFromString(ATTR_VALUE_DATE_BY_STRING1[i], 
						EIMResource.getMessage(sess, "EIM.FORMAT.DATE")));
				assertEquals(tmpDates[i], tmp);
			}
			
			// テキスト型属性
			tmpTexts = AppObjectUtil.getTextAttrs(sess, magoObj, ATTR_NAME_TEXT);
			assertEquals(tmpTexts.length > 0 ,true);
			for (int i = 0 ; i < tmpTexts.length ; i++) {
				assertEquals(tmpTexts[i], ATTR_VALUE_TEXT_BY_STRING1[i]);
			}
			
		} catch (EIMException eime) {
			eime.printStackTrace();
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {
			if (sess != null) {
				sess.close();
			}
		}
		
		
		
		/**
		 * 正常(2)
		 *  ・親フォルダに複数値属性を更新
		 *  ・子フォルダ、孫フォルダへ複数値属性を引継がない
		 */
		try {
			sess = TestSessionUtil.createEIMSession();
			setEIMSessionForUnitTest(sess);
			
			// テストデータ準備
			EIMObject baseObj = createTestData1(sess);
			int baseObjId = baseObj.getId();
			
			// 事前確認
			viewTestData(sess, baseObj, "正常(2) - 事前確認");
			
			// 属性情報の設定
			List itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeStrId, ATTR_VALUE_STR_BY_STRING1, false, false));
			itemList.add(new AttributeUpdaterItem(attrTypeIntId, ATTR_VALUE_INT_BY_STRING1, false, false));
			itemList.add(new AttributeUpdaterItem(attrTypeDateId, ATTR_VALUE_DATE_BY_STRING1, false, false));
			itemList.add(new AttributeUpdaterItem(attrTypeTextId, ATTR_VALUE_TEXT_BY_STRING1, false, false));
			AttributeUpdater updater = new AttributeUpdater(baseObj.getId(), baseObj.getName(), itemList);
			
			// メソッドの実行
			AttributeUtil.updateAttribute(sess, updater);
			sess.commit();
			
			// 再取得
			baseObj = ObjectUtils.getObjectById(sess, baseObjId);
			
			// 結果確認
			viewTestData(sess, baseObj, "正常(2) - 結果確認");

			//-----------------------------------------------------------------
			// 【親オブジェクト 属性の更新を確認】
			
			// 文字列型属性
			tmpStrs = AppObjectUtil.getStrAttrs(sess, baseObj, ATTR_NAME_STR);
			assertEquals(tmpStrs.length > 0 ,true);
			for (int i = 0 ; i < tmpStrs.length ; i++) {
				assertEquals(tmpStrs[i], ATTR_VALUE_STR_BY_STRING1[i]);
			}
			
			// 数値型属性
			tmpInts = AppObjectUtil.getIntAttrs(sess, baseObj, ATTR_NAME_INT);
			assertEquals(tmpInts.length > 0 ,true);
			for (int i = 0 ; i < tmpInts.length ; i++) {
				assertEquals(tmpInts[i], Integer.parseInt(ATTR_VALUE_INT_BY_STRING1[i]));
			}
			
			// 日付型属性
			tmpDates = AppObjectUtil.getDateAttrs(sess, baseObj, ATTR_NAME_DATE);
			assertEquals(tmpDates.length > 0 ,true);
			for (int i = 0 ; i < tmpDates.length ; i++) {
				Date tmp = DateUtils.editExpirationDate(sess, 
						StringUtils.getDateFromString(ATTR_VALUE_DATE_BY_STRING1[i], 
						EIMResource.getMessage(sess, "EIM.FORMAT.DATE")));
				assertEquals(tmpDates[i], tmp);
			}
			
			// テキスト型属性
			tmpTexts = AppObjectUtil.getTextAttrs(sess, baseObj, ATTR_NAME_TEXT);
			assertEquals(tmpTexts.length > 0 ,true);
			for (int i = 0 ; i < tmpTexts.length ; i++) {
				assertEquals(tmpTexts[i], ATTR_VALUE_TEXT_BY_STRING1[i]);
			}
			
			//-----------------------------------------------------------------
			// 【子オブジェクト 属性の更新を確認】
			
			EIMObject childObj = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, FOLDER_TYPE_NAME1), FOLDER_NAME21);
			
			// 文字列型属性
			tmpStrs = AppObjectUtil.getStrAttrs(sess, childObj, ATTR_NAME_STR);
			assertEquals(tmpStrs == null ,true);
			
			// 数値型属性
			tmpInts = AppObjectUtil.getIntAttrs(sess, childObj, ATTR_NAME_INT);
			assertEquals(tmpInts == null ,true);
			
			// 日付型属性
			tmpDates = AppObjectUtil.getDateAttrs(sess, childObj, ATTR_NAME_DATE);
			assertEquals(tmpDates == null ,true);
			
			// テキスト型属性
			tmpTexts = AppObjectUtil.getTextAttrs(sess, childObj, ATTR_NAME_TEXT);
			assertEquals(tmpTexts == null ,true);
			
			//-----------------------------------------------------------------
			// 【孫オブジェクト 属性の更新を確認】
			
			EIMObject magoObj = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, FOLDER_TYPE_NAME1), FOLDER_NAME3);
			
			// 文字列型属性
			tmpStrs = AppObjectUtil.getStrAttrs(sess, magoObj, ATTR_NAME_STR);
			assertEquals(tmpStrs == null ,true);
			
			// 数値型属性
			tmpInts = AppObjectUtil.getIntAttrs(sess, magoObj, ATTR_NAME_INT);
			assertEquals(tmpInts == null ,true);
			
			// 日付型属性
			tmpDates = AppObjectUtil.getDateAttrs(sess, magoObj, ATTR_NAME_DATE);
			assertEquals(tmpDates == null ,true);
			
			// テキスト型属性
			tmpTexts = AppObjectUtil.getTextAttrs(sess, magoObj, ATTR_NAME_TEXT);
			assertEquals(tmpTexts == null ,true);
			
		} catch (EIMException eime) {
			eime.printStackTrace();
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {
			if (sess != null) {
				sess.close();
			}
		}
		
		
		
		/**
		 * 正常(3)
		 *  ・親フォルダに単数値属性を更新
		 *  ・子フォルダ、孫フォルダへ単数値属性の引継ぎ
		 */
		try {
			sess = TestSessionUtil.createEIMSession();
			setEIMSessionForUnitTest(sess);
			
			// テストデータ準備
			EIMObject baseObj = createTestData1(sess);
			int baseObjId = baseObj.getId();
			
			// 事前確認
			viewTestData(sess, baseObj, "正常(3) - 事前確認");
			
			// 属性情報の設定
			List itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeStrOneId, ATTR_VALUE_STR_BY_STRING1_ONE, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeIntOneId, ATTR_VALUE_INT_BY_STRING1_ONE, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeDateOneId, ATTR_VALUE_DATE_BY_STRING1_ONE, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeTextOneId, ATTR_VALUE_TEXT_BY_STRING1_ONE, false, true));
			AttributeUpdater updater = new AttributeUpdater(baseObj.getId(), baseObj.getName(), itemList);
			
			// メソッドの実行
			AttributeUtil.updateAttribute(sess, updater);
			sess.commit();
			
			// 再取得
			baseObj = ObjectUtils.getObjectById(sess, baseObjId);
			
			// 結果確認
			viewTestData(sess, baseObj, "正常(3) - 結果確認");

			//-----------------------------------------------------------------
			// 【親オブジェクト 属性の更新を確認】
			
			// 文字列型属性
			tmpStrs = AppObjectUtil.getStrAttrs(sess, baseObj, ATTR_NAME_STR_ONE);
			assertEquals(tmpStrs.length > 0 ,true);
			for (int i = 0 ; i < tmpStrs.length ; i++) {
				assertEquals(tmpStrs[i], ATTR_VALUE_STR_BY_STRING1_ONE[i]);
			}
			
			// 数値型属性
			tmpInts = AppObjectUtil.getIntAttrs(sess, baseObj, ATTR_NAME_INT_ONE);
			assertEquals(tmpInts.length > 0 ,true);
			for (int i = 0 ; i < tmpInts.length ; i++) {
				assertEquals(tmpInts[i], Integer.parseInt(ATTR_VALUE_INT_BY_STRING1_ONE[i]));
			}
			
			// 日付型属性
			tmpDates = AppObjectUtil.getDateAttrs(sess, baseObj, ATTR_NAME_DATE_ONE);
			assertEquals(tmpDates.length > 0 ,true);
			for (int i = 0 ; i < tmpDates.length ; i++) {
				Date tmp = DateUtils.editExpirationDate(sess, 
						StringUtils.getDateFromString(ATTR_VALUE_DATE_BY_STRING1_ONE[i], 
						EIMResource.getMessage(sess, "EIM.FORMAT.DATE")));
				assertEquals(tmpDates[i], tmp);
			}
			
			// テキスト型属性
			tmpTexts = AppObjectUtil.getTextAttrs(sess, baseObj, ATTR_NAME_TEXT_ONE);
			assertEquals(tmpTexts.length > 0 ,true);
			for (int i = 0 ; i < tmpTexts.length ; i++) {
				assertEquals(tmpTexts[i], ATTR_VALUE_TEXT_BY_STRING1_ONE[i]);
			}
			
			//-----------------------------------------------------------------
			// 【子オブジェクト 属性の更新を確認】
			
			EIMObject childObj = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, FOLDER_TYPE_NAME1), FOLDER_NAME21);
			
			// 文字列型属性
			tmpStrs = AppObjectUtil.getStrAttrs(sess, childObj, ATTR_NAME_STR_ONE);
			assertEquals(tmpStrs.length > 0 ,true);
			for (int i = 0 ; i < tmpStrs.length ; i++) {
				assertEquals(tmpStrs[i], ATTR_VALUE_STR_BY_STRING1_ONE[i]);
			}
			
			// 数値型属性
			tmpInts = AppObjectUtil.getIntAttrs(sess, childObj, ATTR_NAME_INT_ONE);
			assertEquals(tmpInts.length > 0 ,true);
			for (int i = 0 ; i < tmpInts.length ; i++) {
				assertEquals(tmpInts[i], Integer.parseInt(ATTR_VALUE_INT_BY_STRING1_ONE[i]));
			}
			
			// 日付型属性
			tmpDates = AppObjectUtil.getDateAttrs(sess, childObj, ATTR_NAME_DATE_ONE);
			assertEquals(tmpDates.length > 0 ,true);
			for (int i = 0 ; i < tmpDates.length ; i++) {
				Date tmp = DateUtils.editExpirationDate(sess, 
						StringUtils.getDateFromString(ATTR_VALUE_DATE_BY_STRING1_ONE[i], 
						EIMResource.getMessage(sess, "EIM.FORMAT.DATE")));
				assertEquals(tmpDates[i], tmp);
			}
			
			// テキスト型属性
			tmpTexts = AppObjectUtil.getTextAttrs(sess, childObj, ATTR_NAME_TEXT_ONE);
			assertEquals(tmpTexts.length > 0 ,true);
			for (int i = 0 ; i < tmpTexts.length ; i++) {
				assertEquals(tmpTexts[i], ATTR_VALUE_TEXT_BY_STRING1_ONE[i]);
			}
			
			//-----------------------------------------------------------------
			// 【孫オブジェクト 属性の更新を確認】
			
			EIMObject magoObj = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, FOLDER_TYPE_NAME1), FOLDER_NAME3);
			
			// 文字列型属性
			tmpStrs = AppObjectUtil.getStrAttrs(sess, magoObj, ATTR_NAME_STR_ONE);
			assertEquals(tmpStrs.length > 0 ,true);
			for (int i = 0 ; i < tmpStrs.length ; i++) {
				assertEquals(tmpStrs[i], ATTR_VALUE_STR_BY_STRING1_ONE[i]);
			}
			
			// 数値型属性
			tmpInts = AppObjectUtil.getIntAttrs(sess, magoObj, ATTR_NAME_INT_ONE);
			assertEquals(tmpInts.length > 0 ,true);
			for (int i = 0 ; i < tmpInts.length ; i++) {
				assertEquals(tmpInts[i], Integer.parseInt(ATTR_VALUE_INT_BY_STRING1_ONE[i]));
			}
			
			// 日付型属性
			tmpDates = AppObjectUtil.getDateAttrs(sess, magoObj, ATTR_NAME_DATE_ONE);
			assertEquals(tmpDates.length > 0 ,true);
			for (int i = 0 ; i < tmpDates.length ; i++) {
				Date tmp = DateUtils.editExpirationDate(sess, 
						StringUtils.getDateFromString(ATTR_VALUE_DATE_BY_STRING1_ONE[i], 
						EIMResource.getMessage(sess, "EIM.FORMAT.DATE")));
				assertEquals(tmpDates[i], tmp);
			}
			
			// テキスト型属性
			tmpTexts = AppObjectUtil.getTextAttrs(sess, magoObj, ATTR_NAME_TEXT_ONE);
			assertEquals(tmpTexts.length > 0 ,true);
			for (int i = 0 ; i < tmpTexts.length ; i++) {
				assertEquals(tmpTexts[i], ATTR_VALUE_TEXT_BY_STRING1_ONE[i]);
			}
			
		} catch (EIMException eime) {
			eime.printStackTrace();
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {
			if (sess != null) {
				sess.close();
			}
		}
		
		
		
		/**
		 * 正常(4)
		 *  ・親フォルダに単数値属性を更新
		 *  ・子フォルダ、孫フォルダへ単数値属性は引継がない
		 */
		try {
			sess = TestSessionUtil.createEIMSession();
			setEIMSessionForUnitTest(sess);
			
			// テストデータ準備
			EIMObject baseObj = createTestData1(sess);
			int baseObjId = baseObj.getId();
			
			// 事前確認
			viewTestData(sess, baseObj, "正常(4) - 事前確認");
			
			// 属性情報の設定
			List itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeStrOneId, ATTR_VALUE_STR_BY_STRING1_ONE, false, false));
			itemList.add(new AttributeUpdaterItem(attrTypeIntOneId, ATTR_VALUE_INT_BY_STRING1_ONE, false, false));
			itemList.add(new AttributeUpdaterItem(attrTypeDateOneId, ATTR_VALUE_DATE_BY_STRING1_ONE, false, false));
			itemList.add(new AttributeUpdaterItem(attrTypeTextOneId, ATTR_VALUE_TEXT_BY_STRING1_ONE, false, false));
			AttributeUpdater updater = new AttributeUpdater(baseObj.getId(), baseObj.getName(), itemList);
			
			// メソッドの実行
			AttributeUtil.updateAttribute(sess, updater);
			sess.commit();
			
			// 再取得
			baseObj = ObjectUtils.getObjectById(sess, baseObjId);
			
			// 結果確認
			viewTestData(sess, baseObj, "正常(4) - 結果確認");

			//-----------------------------------------------------------------
			// 【親オブジェクト 属性の更新を確認】
			
			// 文字列型属性
			tmpStrs = AppObjectUtil.getStrAttrs(sess, baseObj, ATTR_NAME_STR_ONE);
			assertEquals(tmpStrs.length > 0 ,true);
			for (int i = 0 ; i < tmpStrs.length ; i++) {
				assertEquals(tmpStrs[i], ATTR_VALUE_STR_BY_STRING1_ONE[i]);
			}
			
			// 数値型属性
			tmpInts = AppObjectUtil.getIntAttrs(sess, baseObj, ATTR_NAME_INT_ONE);
			assertEquals(tmpInts.length > 0 ,true);
			for (int i = 0 ; i < tmpInts.length ; i++) {
				assertEquals(tmpInts[i], Integer.parseInt(ATTR_VALUE_INT_BY_STRING1_ONE[i]));
			}
			
			// 日付型属性
			tmpDates = AppObjectUtil.getDateAttrs(sess, baseObj, ATTR_NAME_DATE_ONE);
			assertEquals(tmpDates.length > 0 ,true);
			for (int i = 0 ; i < tmpDates.length ; i++) {
				Date tmp = DateUtils.editExpirationDate(sess, 
						StringUtils.getDateFromString(ATTR_VALUE_DATE_BY_STRING1_ONE[i], 
						EIMResource.getMessage(sess, "EIM.FORMAT.DATE")));
				assertEquals(tmpDates[i], tmp);
			}
			
			// テキスト型属性
			tmpTexts = AppObjectUtil.getTextAttrs(sess, baseObj, ATTR_NAME_TEXT_ONE);
			assertEquals(tmpTexts.length > 0 ,true);
			for (int i = 0 ; i < tmpTexts.length ; i++) {
				assertEquals(tmpTexts[i], ATTR_VALUE_TEXT_BY_STRING1_ONE[i]);
			}
			
			//-----------------------------------------------------------------
			// 【子オブジェクト 属性の更新を確認】
			
			EIMObject childObj = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, FOLDER_TYPE_NAME1), FOLDER_NAME21);
			
			// 文字列型属性
			tmpStrs = AppObjectUtil.getStrAttrs(sess, childObj, ATTR_NAME_STR_ONE);
			assertEquals(tmpStrs == null ,true);
			
			// 数値型属性
			tmpInts = AppObjectUtil.getIntAttrs(sess, childObj, ATTR_NAME_INT_ONE);
			assertEquals(tmpInts == null ,true);
			
			// 日付型属性
			tmpDates = AppObjectUtil.getDateAttrs(sess, childObj, ATTR_NAME_DATE_ONE);
			assertEquals(tmpDates == null ,true);
			
			// テキスト型属性
			tmpTexts = AppObjectUtil.getTextAttrs(sess, childObj, ATTR_NAME_TEXT_ONE);
			assertEquals(tmpTexts == null ,true);
			
			//-----------------------------------------------------------------
			// 【孫オブジェクト 属性の更新を確認】
			
			EIMObject magoObj = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, FOLDER_TYPE_NAME1), FOLDER_NAME3);
			
			// 文字列型属性
			tmpStrs = AppObjectUtil.getStrAttrs(sess, magoObj, ATTR_NAME_STR_ONE);
			assertEquals(tmpStrs == null ,true);
			
			// 数値型属性
			tmpInts = AppObjectUtil.getIntAttrs(sess, magoObj, ATTR_NAME_INT_ONE);
			assertEquals(tmpInts == null ,true);
			
			// 日付型属性
			tmpDates = AppObjectUtil.getDateAttrs(sess, magoObj, ATTR_NAME_DATE_ONE);
			assertEquals(tmpDates == null ,true);
			
			// テキスト型属性
			tmpTexts = AppObjectUtil.getTextAttrs(sess, magoObj, ATTR_NAME_TEXT_ONE);
			assertEquals(tmpTexts == null ,true);
			
		} catch (EIMException eime) {
			eime.printStackTrace();
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {
			if (sess != null) {
				sess.close();
			}
		}

		
		
		/**
		 * 正常(5)
		 * 
		 *  ・孫ドキュメントへ複数値属性/単数値属性の引継ぎ
		 */
		try {
			sess = TestSessionUtil.createEIMSession();
			setEIMSessionForUnitTest(sess);
			
			// テストデータ準備
			EIMObject baseObj = createTestData2(sess);
			int baseObjId = baseObj.getId();
			
			// 事前確認
			viewTestData(sess, baseObj, "正常(5) - 事前確認");
			
			// 属性情報の設定
			List itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeStrId, ATTR_VALUE_STR_BY_STRING1, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeIntId, ATTR_VALUE_INT_BY_STRING1, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeDateId, ATTR_VALUE_DATE_BY_STRING1, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeTextId, ATTR_VALUE_TEXT_BY_STRING1, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeStrOneId, ATTR_VALUE_STR_BY_STRING1_ONE, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeIntOneId, ATTR_VALUE_INT_BY_STRING1_ONE, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeDateOneId, ATTR_VALUE_DATE_BY_STRING1_ONE, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeTextOneId, ATTR_VALUE_TEXT_BY_STRING1_ONE, false, true));
			AttributeUpdater updater = new AttributeUpdater(baseObj.getId(), baseObj.getName(), itemList);
			
			// メソッドの実行
			AttributeUtil.updateAttribute(sess, updater);
			sess.commit();
			
			// 再取得
			baseObj = ObjectUtils.getObjectById(sess, baseObjId);
			
			// 結果確認
			viewTestData(sess, baseObj, "正常(5) - 結果確認");

			//-----------------------------------------------------------------
			// 【孫ドキュメント 属性の更新を確認】
			
			EIMObject magoObj = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, DOCUMENT_TYPE_NAME1), DOC_NAME1);
			
			// 文字列型属性
			tmpStrs = AppObjectUtil.getStrAttrs(sess, magoObj, ATTR_NAME_STR);
			assertEquals(tmpStrs.length > 0 ,true);
			for (int i = 0 ; i < tmpStrs.length ; i++) {
				assertEquals(tmpStrs[i], ATTR_VALUE_STR_BY_STRING1[i]);
			}
			
			// 数値型属性
			tmpInts = AppObjectUtil.getIntAttrs(sess, magoObj, ATTR_NAME_INT);
			assertEquals(tmpInts.length > 0 ,true);
			for (int i = 0 ; i < tmpInts.length ; i++) {
				assertEquals(tmpInts[i], Integer.parseInt(ATTR_VALUE_INT_BY_STRING1[i]));
			}
			
			// 日付型属性
			tmpDates = AppObjectUtil.getDateAttrs(sess, magoObj, ATTR_NAME_DATE);
			assertEquals(tmpDates.length > 0 ,true);
			for (int i = 0 ; i < tmpDates.length ; i++) {
				Date tmp = DateUtils.editExpirationDate(sess, 
						StringUtils.getDateFromString(ATTR_VALUE_DATE_BY_STRING1[i], 
						EIMResource.getMessage(sess, "EIM.FORMAT.DATE")));
				assertEquals(tmpDates[i], tmp);
			}
			
			// テキスト型属性
			tmpTexts = AppObjectUtil.getTextAttrs(sess, magoObj, ATTR_NAME_TEXT);
			assertEquals(tmpTexts.length > 0 ,true);
			for (int i = 0 ; i < tmpTexts.length ; i++) {
				assertEquals(tmpTexts[i], ATTR_VALUE_TEXT_BY_STRING1[i]);
			}
			
			// 文字列型属性 (単数値)
			tmpStrs = AppObjectUtil.getStrAttrs(sess, magoObj, ATTR_NAME_STR_ONE);
			assertEquals(tmpStrs.length > 0 ,true);
			assertEquals(tmpStrs[0], ATTR_VALUE_STR_BY_STRING1_ONE[0]);
			
			// 数値型属性 (単数値)
			tmpInts = AppObjectUtil.getIntAttrs(sess, magoObj, ATTR_NAME_INT_ONE);
			assertEquals(tmpInts.length > 0 ,true);
			for (int i = 0 ; i < tmpInts.length ; i++) {
				assertEquals(tmpInts[i], Integer.parseInt(ATTR_VALUE_INT_BY_STRING1_ONE[i]));
			}
			
			// 日付型属性 (単数値)
			tmpDates = AppObjectUtil.getDateAttrs(sess, magoObj, ATTR_NAME_DATE_ONE);
			assertEquals(tmpDates.length > 0 ,true);
			Date tmp = DateUtils.editExpirationDate(sess, 
					StringUtils.getDateFromString(ATTR_VALUE_DATE_BY_STRING1_ONE[0], 
					EIMResource.getMessage(sess, "EIM.FORMAT.DATE")));
			assertEquals(tmpDates[0], tmp);
			
			// テキスト型属性 (単数値)
			tmpTexts = AppObjectUtil.getTextAttrs(sess, magoObj, ATTR_NAME_TEXT_ONE);
			assertEquals(tmpTexts.length > 0 ,true);
			assertEquals(tmpTexts[0], ATTR_VALUE_TEXT_BY_STRING1_ONE[0]);
			
		} catch (EIMException eime) {
			eime.printStackTrace();
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {
			if (sess != null) {
				sess.close();
			}
		}
		
		
		
		/**
		 * 正常(6)
		 * 
		 *  ・孫ドキュメントへ複数値属性/単数値属性を引継がない
		 */
		try {
			sess = TestSessionUtil.createEIMSession();
			setEIMSessionForUnitTest(sess);
			
			// テストデータ準備
			EIMObject baseObj = createTestData2(sess);
			int baseObjId = baseObj.getId();
			
			// 事前確認
			viewTestData(sess, baseObj, "正常(6) - 事前確認");
			
			// 属性情報の設定
			List itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeStrId, ATTR_VALUE_STR_BY_STRING1, false, false));
			itemList.add(new AttributeUpdaterItem(attrTypeIntId, ATTR_VALUE_INT_BY_STRING1, false, false));
			itemList.add(new AttributeUpdaterItem(attrTypeDateId, ATTR_VALUE_DATE_BY_STRING1, false, false));
			itemList.add(new AttributeUpdaterItem(attrTypeTextId, ATTR_VALUE_TEXT_BY_STRING1, false, false));
			itemList.add(new AttributeUpdaterItem(attrTypeStrOneId, ATTR_VALUE_STR_BY_STRING1_ONE, false, false));
			itemList.add(new AttributeUpdaterItem(attrTypeIntOneId, ATTR_VALUE_INT_BY_STRING1_ONE, false, false));
			itemList.add(new AttributeUpdaterItem(attrTypeDateOneId, ATTR_VALUE_DATE_BY_STRING1_ONE, false, false));
			itemList.add(new AttributeUpdaterItem(attrTypeTextOneId, ATTR_VALUE_TEXT_BY_STRING1_ONE, false, false));
			AttributeUpdater updater = new AttributeUpdater(baseObj.getId(), baseObj.getName(), itemList);
			
			// メソッドの実行
			AttributeUtil.updateAttribute(sess, updater);
			sess.commit();
			
			// 再取得
			baseObj = ObjectUtils.getObjectById(sess, baseObjId);
			
			// 結果確認
			viewTestData(sess, baseObj, "正常(6) - 結果確認");

			//-----------------------------------------------------------------
			// 【孫ドキュメント 属性の更新を確認】
			
			EIMObject magoObj = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, DOCUMENT_TYPE_NAME1), DOC_NAME1);
			
			// 文字列型属性
			tmpStrs = AppObjectUtil.getStrAttrs(sess, magoObj, ATTR_NAME_STR);
			assertEquals(tmpStrs.length > 0 ,true);
			for (int i = 0 ; i < tmpStrs.length ; i++) {
				assertEquals(tmpStrs[i], ATTR_VALUE_STR1[i]);
			}
			
			// 数値型属性
			tmpInts = AppObjectUtil.getIntAttrs(sess, magoObj, ATTR_NAME_INT);
			assertEquals(tmpInts.length > 0 ,true);
			for (int i = 0 ; i < tmpInts.length ; i++) {
				assertEquals(tmpInts[i], ATTR_VALUE_INT1[i]);
			}
			
			// 日付型属性
			tmpDates = AppObjectUtil.getDateAttrs(sess, magoObj, ATTR_NAME_DATE);
			assertEquals(tmpDates.length > 0 ,true);
			for (int i = 0 ; i < tmpDates.length ; i++) {
				assertEquals(getDateString(sess, tmpDates[i]), getDateString(sess, ATTR_VALUE_DATE1[i]));
			}
			
			// テキスト型属性
			tmpTexts = AppObjectUtil.getTextAttrs(sess, magoObj, ATTR_NAME_TEXT);
			assertEquals(tmpTexts.length > 0 ,true);
			for (int i = 0 ; i < tmpTexts.length ; i++) {
				assertEquals(tmpTexts[0], ATTR_VALUE_TEXT1[i]);
			}

			// 文字列型属性 (単数値)
			tmpStrs = AppObjectUtil.getStrAttrs(sess, magoObj, ATTR_NAME_STR_ONE);
			assertEquals(tmpStrs.length > 0 ,true);
			assertEquals(tmpStrs[0], ATTR_VALUE_STR1_ONE);
			
			// 数値型属性 (単数値)
			tmpInts = AppObjectUtil.getIntAttrs(sess, magoObj, ATTR_NAME_INT_ONE);
			assertEquals(tmpInts.length > 0 ,true);
			assertEquals(tmpInts[0], ATTR_VALUE_INT1_ONE);
			
			// 日付型属性 (単数値)
			tmpDates = AppObjectUtil.getDateAttrs(sess, magoObj, ATTR_NAME_DATE_ONE);
			assertEquals(tmpDates.length > 0 ,true);
			assertEquals(getDateString(sess, tmpDates[0]), getDateString(sess, ATTR_VALUE_DATE1_ONE));
			
			// テキスト型属性 (単数値)
			tmpTexts = AppObjectUtil.getTextAttrs(sess, magoObj, ATTR_NAME_TEXT_ONE);
			assertEquals(tmpTexts.length > 0 ,true);
			assertEquals(tmpTexts[0], ATTR_VALUE_TEXT1_ONE);
			
		} catch (EIMException eime) {
			eime.printStackTrace();
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {
			if (sess != null) {
				sess.close();
			}
		}
		
		
		
		
		/**
		 * 正常(7)
		 * 
		 *  ・名称割当て、かつ、下位引継ぎ属性
		 */
		try {
			sess = TestSessionUtil.createEIMSession();
			setEIMSessionForUnitTest(sess);
			
			// テストデータ準備
			EIMObject baseObj = createTestData2(sess);
			int baseObjId = baseObj.getId();
			
			EIMObject childObj = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, FOLDER_TYPE_NAME1), FOLDER_NAME21);
			String childObjNameBefore = childObj.getName();	// 元々の子オブジェクトの名前
			
			EIMObject magoObj = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, DOCUMENT_TYPE_NAME1), DOC_NAME1);
			String magoObjNameBefore = magoObj.getName();	// 元々の孫オブジェクトの名前

			
			// 事前確認
			viewTestData(sess, baseObj, "正常(7) - 事前確認");
			
			// 属性情報の設定
			List itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeStrId, ATTR_VALUE_STR_BY_STRING1, false, false));
			itemList.add(new AttributeUpdaterItem(attrTypeIntId, ATTR_VALUE_INT_BY_STRING1, false, false));
			itemList.add(new AttributeUpdaterItem(attrTypeDateId, ATTR_VALUE_DATE_BY_STRING1, false, false));
			itemList.add(new AttributeUpdaterItem(attrTypeTextId, ATTR_VALUE_TEXT_BY_STRING1, false, false));
			itemList.add(new AttributeUpdaterItem(attrTypeStrOneId, ATTR_VALUE_STR_BY_STRING1_ONE, true, true));
			itemList.add(new AttributeUpdaterItem(attrTypeIntOneId, ATTR_VALUE_INT_BY_STRING1_ONE, false, false));
			itemList.add(new AttributeUpdaterItem(attrTypeDateOneId, ATTR_VALUE_DATE_BY_STRING1_ONE, false, false));
			itemList.add(new AttributeUpdaterItem(attrTypeTextOneId, ATTR_VALUE_TEXT_BY_STRING1_ONE, false, false));
			AttributeUpdater updater = new AttributeUpdater(baseObj.getId(), ATTR_VALUE_STR_BY_STRING1_ONE[0], itemList);
			
			// メソッドの実行
			AttributeUtil.updateAttribute(sess, updater);
			sess.commit();
			
			// 再取得
			baseObj = ObjectUtils.getObjectById(sess, baseObjId);
			
			// 結果確認
			viewTestData(sess, baseObj, "正常(7) - 結果確認");

			
			//-----------------------------------------------------------------
			// 【親フォルダ 属性の更新を確認】
			
			// オブジェクト名
			assertEquals(baseObj.getName(), ATTR_VALUE_STR_BY_STRING1_ONE[0]);
			
			// 名称割当て属性
			tmpInts = AppObjectUtil.getIntAttrs(sess, baseObj, EIMConfig.get("ATTR_NAME_FOLDER_NAME_ATTR"));
			assertEquals(tmpInts.length > 0 ,true);
			assertEquals(tmpInts[0], attrTypeStrOneId);
			
			// 文字列型属性 (単数値)
			tmpStrs = AppObjectUtil.getStrAttrs(sess, baseObj, ATTR_NAME_STR_ONE);
			assertEquals(tmpStrs.length > 0 ,true);
			assertEquals(tmpStrs[0], ATTR_VALUE_STR_BY_STRING1_ONE[0]);
			
			//-----------------------------------------------------------------
			// 【子フォルダ 属性の更新を確認】
			
			childObj = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, FOLDER_TYPE_NAME1), FOLDER_NAME21);

			// オブジェクト名
			assertEquals(childObj.getName(), childObjNameBefore);

			// 名称割当て属性
			tmpInts = AppObjectUtil.getIntAttrs(sess, childObj, EIMConfig.get("ATTR_NAME_FOLDER_NAME_ATTR"));
			assertNull(tmpInts);
			
			// 文字列型属性 (単数値)
			tmpStrs = AppObjectUtil.getStrAttrs(sess, childObj, ATTR_NAME_STR_ONE);
			assertEquals(tmpStrs.length > 0 ,true);
			assertEquals(tmpStrs[0], ATTR_VALUE_STR_BY_STRING1_ONE[0]);
			
			//-----------------------------------------------------------------
			// 【孫ドキュメント 属性の更新を確認】
			
			magoObj = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, DOCUMENT_TYPE_NAME1), DOC_NAME1);

			// オブジェクト名
			assertEquals(magoObj.getName(), magoObjNameBefore);
			
			// 名称割当て属性
			tmpInts = AppObjectUtil.getIntAttrs(sess, magoObj, EIMConfig.get("ATTR_NAME_FOLDER_NAME_ATTR"));
			assertNull(tmpInts);
			
			// 文字列型属性 (単数値)
			tmpStrs = AppObjectUtil.getStrAttrs(sess, magoObj, ATTR_NAME_STR_ONE);
			assertEquals(tmpStrs.length > 0 ,true);
			assertEquals(tmpStrs[0], ATTR_VALUE_STR_BY_STRING1_ONE[0]);
			
		} catch (EIMException eime) {
			eime.printStackTrace();
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {
			if (sess != null) {
				sess.close();
			}
		}
		
		
		
		
		/**
		 * 正常(8)
		 * 
		 *  ・属性値を空欄で入力、かつ、孫ドキュメントまで複数値属性/単数値属性を引継ぐ
		 */
		try {
			sess = TestSessionUtil.createEIMSession();
			setEIMSessionForUnitTest(sess);
			
			// テストデータ準備
			EIMObject baseObj = createTestData2(sess);
			int baseObjId = baseObj.getId();
			
			// 事前確認
			viewTestData(sess, baseObj, "正常(8) - 事前確認");
			
			// 属性情報の設定
			List itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeStrId, ATTR_VALUE_BLANK, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeIntId, ATTR_VALUE_BLANK, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeDateId, ATTR_VALUE_BLANK, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeTextId, ATTR_VALUE_BLANK, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeStrOneId, ATTR_VALUE_BLANK, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeIntOneId, ATTR_VALUE_BLANK, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeDateOneId, ATTR_VALUE_BLANK, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeTextOneId, ATTR_VALUE_BLANK, false, true));
			AttributeUpdater updater = new AttributeUpdater(baseObj.getId(), baseObj.getName(), itemList);
			
			// メソッドの実行
			AttributeUtil.updateAttribute(sess, updater);
			sess.commit();
			
			// 再取得
			baseObj = ObjectUtils.getObjectById(sess, baseObjId);
			
			// 結果確認
			viewTestData(sess, baseObj, "正常(8) - 結果確認");

			//-----------------------------------------------------------------
			// 【孫ドキュメント 属性の更新を確認】
			
			EIMObject magoObj = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, DOCUMENT_TYPE_NAME1), DOC_NAME1);
			
			// 文字列型属性
			tmpStrs = AppObjectUtil.getStrAttrs(sess, magoObj, ATTR_NAME_STR);
			assertNull(tmpStrs);
			
			// 数値型属性
			tmpInts = AppObjectUtil.getIntAttrs(sess, magoObj, ATTR_NAME_INT);
			assertNull(tmpInts);
			
			// 日付型属性
			tmpDates = AppObjectUtil.getDateAttrs(sess, magoObj, ATTR_NAME_DATE);
			assertNull(tmpDates);
			
			// テキスト型属性
			tmpTexts = AppObjectUtil.getTextAttrs(sess, magoObj, ATTR_NAME_TEXT);
			assertNull(tmpTexts);

			// 文字列型属性 (単数値)
			tmpStrs = AppObjectUtil.getStrAttrs(sess, magoObj, ATTR_NAME_STR_ONE);
			assertNull(tmpStrs);
			
			// 数値型属性 (単数値)
			tmpInts = AppObjectUtil.getIntAttrs(sess, magoObj, ATTR_NAME_INT_ONE);
			assertNull(tmpInts);
			
			// 日付型属性 (単数値)
			tmpDates = AppObjectUtil.getDateAttrs(sess, magoObj, ATTR_NAME_DATE_ONE);
			assertNull(tmpDates);
			
			// テキスト型属性 (単数値)
			tmpTexts = AppObjectUtil.getTextAttrs(sess, magoObj, ATTR_NAME_TEXT_ONE);
			assertNull(tmpTexts);
			
		} catch (EIMException eime) {
			eime.printStackTrace();
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {
			if (sess != null) {
				sess.close();
			}
		}
		
		
		
		
		/**
		 * 正常(9)
		 * 
		 *  ・親オブジェクトからの引継ぎ属性の属性タイプを孫ドキュメントが持たない場合
		 */
		try {
			sess = TestSessionUtil.createEIMSession();
			setEIMSessionForUnitTest(sess);
			
			// テストデータ準備
			EIMObject baseObj = createTestData2(sess);
			int baseObjId = baseObj.getId();
			
			// 事前確認
			viewTestData(sess, baseObj, "正常(9) - 事前確認");
			
			// 属性情報の設定
			List itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeStrId, ATTR_VALUE_STR_BY_STRING1, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeStrOneId, ATTR_VALUE_STR_BY_STRING1_ONE, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeOnlyFolderId, ATTR_VALUE_STR_BY_STRING2, false, true));
			AttributeUpdater updater = new AttributeUpdater(baseObj.getId(), baseObj.getName(), itemList);
			
			// メソッドの実行
			AttributeUtil.updateAttribute(sess, updater);
			sess.commit();
			
			// 再取得
			baseObj = ObjectUtils.getObjectById(sess, baseObjId);
			
			// 結果確認
			viewTestData(sess, baseObj, "正常(9) - 結果確認");

			//-----------------------------------------------------------------
			// 【孫ドキュメント 属性の更新を確認】
			
			EIMObject magoObj = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, DOCUMENT_TYPE_NAME1), DOC_NAME1);
			
			// フォルダ専用の属性
			tmpStrs = AppObjectUtil.getStrAttrs(sess, magoObj, ATTR_NAME_BELONG_ONLY_FOLDER);
			assertNull(tmpStrs);
			
		} catch (EIMException eime) {
			eime.printStackTrace();
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {
			if (sess != null) {
				sess.close();
			}
		}
		
		
		
		
		/**
		 * 正常(10)
		 * 
		 *  ・既に親から孫ドキュメントまで引継いだ状態で、続いて子から孫ドキュメントへ別の属性の引継ぎ
		 */
		try {
			sess = TestSessionUtil.createEIMSession();
			setEIMSessionForUnitTest(sess);
			
			// テストデータ準備
			EIMObject baseObj = createTestData2(sess);
			int baseObjId = baseObj.getId();
			
			// 事前確認
			viewTestData(sess, baseObj, "正常(10) - 事前確認(1)");
			
			// 属性情報の設定
			List itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeStrId, ATTR_VALUE_STR_BY_STRING1, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeIntId, ATTR_VALUE_INT_BY_STRING1, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeDateOneId, ATTR_VALUE_DATE_BY_STRING1_ONE, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeTextOneId, ATTR_VALUE_TEXT_BY_STRING1_ONE, false, true));
			AttributeUpdater updater = new AttributeUpdater(baseObj.getId(), baseObj.getName(), itemList);
			
			// メソッドの実行
			AttributeUtil.updateAttribute(sess, updater);
			sess.commit();
			
			// 再取得
			baseObj = ObjectUtils.getObjectById(sess, baseObjId);

			// 事前確認
			viewTestData(sess, baseObj, "正常(10) - 事前確認(2)");

			// 子オブジェクトの取得
			EIMObject childObj = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, FOLDER_TYPE_NAME1), FOLDER_NAME21);
			
			// 属性情報の設定
			List itemList2 = new ArrayList();
			itemList2.add(new AttributeUpdaterItem(attrTypeDateId, ATTR_VALUE_DATE_BY_STRING1, false, true));
			itemList2.add(new AttributeUpdaterItem(attrTypeTextId, ATTR_VALUE_TEXT_BY_STRING1, false, true));
			itemList2.add(new AttributeUpdaterItem(attrTypeStrOneId, ATTR_VALUE_STR_BY_STRING1_ONE, false, true));
			itemList2.add(new AttributeUpdaterItem(attrTypeIntOneId, ATTR_VALUE_INT_BY_STRING1_ONE, false, true));
			AttributeUpdater updater2 = new AttributeUpdater(childObj.getId(), childObj.getName(), itemList2);
			
			// メソッドの実行
			AttributeUtil.updateAttribute(sess, updater2);
			sess.commit();
			
			// 再取得
			baseObj = ObjectUtils.getObjectById(sess, baseObjId);
			
			// 結果確認
			viewTestData(sess, baseObj, "正常(10) - 結果確認");

			//-----------------------------------------------------------------
			// 【孫ドキュメント 属性の更新を確認】
			
			EIMObject magoObj = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, DOCUMENT_TYPE_NAME1), DOC_NAME1);
			
			// 文字列型属性
			tmpStrs = AppObjectUtil.getStrAttrs(sess, magoObj, ATTR_NAME_STR);
			assertEquals(tmpStrs.length > 0 ,true);
			for (int i = 0 ; i < tmpStrs.length ; i++) {
				assertEquals(tmpStrs[i], ATTR_VALUE_STR_BY_STRING1[i]);
			}
			
			// 数値型属性
			tmpInts = AppObjectUtil.getIntAttrs(sess, magoObj, ATTR_NAME_INT);
			assertEquals(tmpInts.length > 0 ,true);
			for (int i = 0 ; i < tmpInts.length ; i++) {
				assertEquals(tmpInts[i], Integer.parseInt(ATTR_VALUE_INT_BY_STRING1[i]));
			}
			
			// 日付型属性
			tmpDates = AppObjectUtil.getDateAttrs(sess, magoObj, ATTR_NAME_DATE);
			assertEquals(tmpDates.length > 0 ,true);
			for (int i = 0 ; i < tmpDates.length ; i++) {
				Date tmp = DateUtils.editExpirationDate(sess, 
						StringUtils.getDateFromString(ATTR_VALUE_DATE_BY_STRING1[i], 
						EIMResource.getMessage(sess, "EIM.FORMAT.DATE")));
				assertEquals(tmpDates[i], tmp);
			}
			
			// テキスト型属性
			tmpTexts = AppObjectUtil.getTextAttrs(sess, magoObj, ATTR_NAME_TEXT);
			assertEquals(tmpTexts.length > 0 ,true);
			for (int i = 0 ; i < tmpTexts.length ; i++) {
				assertEquals(tmpTexts[i], ATTR_VALUE_TEXT_BY_STRING1[i]);
			}
			
			// 文字列型属性 (単数値)
			tmpStrs = AppObjectUtil.getStrAttrs(sess, magoObj, ATTR_NAME_STR_ONE);
			assertEquals(tmpStrs.length > 0 ,true);
			assertEquals(tmpStrs[0], ATTR_VALUE_STR_BY_STRING1_ONE[0]);
			
			// 数値型属性 (単数値)
			tmpInts = AppObjectUtil.getIntAttrs(sess, magoObj, ATTR_NAME_INT_ONE);
			assertEquals(tmpInts.length > 0 ,true);
			for (int i = 0 ; i < tmpInts.length ; i++) {
				assertEquals(tmpInts[i], Integer.parseInt(ATTR_VALUE_INT_BY_STRING1_ONE[i]));
			}
			
			// 日付型属性 (単数値)
			tmpDates = AppObjectUtil.getDateAttrs(sess, magoObj, ATTR_NAME_DATE_ONE);
			assertEquals(tmpDates.length > 0 ,true);
			Date tmp = DateUtils.editExpirationDate(sess, 
					StringUtils.getDateFromString(ATTR_VALUE_DATE_BY_STRING1_ONE[0], 
					EIMResource.getMessage(sess, "EIM.FORMAT.DATE")));
			assertEquals(tmpDates[0], tmp);
			
			// テキスト型属性 (単数値)
			tmpTexts = AppObjectUtil.getTextAttrs(sess, magoObj, ATTR_NAME_TEXT_ONE);
			assertEquals(tmpTexts.length > 0 ,true);
			assertEquals(tmpTexts[0], ATTR_VALUE_TEXT_BY_STRING1_ONE[0]);
			
		} catch (EIMException eime) {
			eime.printStackTrace();
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {
			if (sess != null) {
				sess.close();
			}
		}
		
		
		
		
		/**
		 * 正常(11)
		 * 
		 *  ・名称割当てを設定した後、名称割当てを解除する
		 */
		try {
			sess = TestSessionUtil.createEIMSession();
			setEIMSessionForUnitTest(sess);
			
			// テストデータ準備
			EIMObject baseObj = createTestData2(sess);
			int baseObjId = baseObj.getId();
			
			// 事前確認
			viewTestData(sess, baseObj, "正常(11) - 事前確認");
			
			// 属性情報の設定
			List itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeStrOneId, ATTR_VALUE_STR_BY_STRING1_ONE, true, false));
			AttributeUpdater updater = new AttributeUpdater(baseObj.getId(), ATTR_VALUE_STR_BY_STRING1_ONE[0], itemList);
			
			// メソッドの実行
			AttributeUtil.updateAttribute(sess, updater);
			sess.commit();
			
			// 再取得
			baseObj = ObjectUtils.getObjectById(sess, baseObjId);
			
			itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeStrOneId, ATTR_VALUE_STR_BY_STRING1_ONE, false, false));
			updater = new AttributeUpdater(baseObj.getId(), baseObj.getName(), itemList);

			// メソッドの実行
			AttributeUtil.updateAttribute(sess, updater);
			sess.commit();
			
			// 再取得
			baseObj = ObjectUtils.getObjectById(sess, baseObjId);
			
			// 結果確認
			viewTestData(sess, baseObj, "正常(11) - 結果確認");

			//-----------------------------------------------------------------
			// 【名称割当て属性が空になる】
			
			// 名称割当て属性
			tmpInts = AppObjectUtil.getIntAttrs(sess, baseObj, EIMConfig.get("ATTR_NAME_FOLDER_NAME_ATTR"));
			assertEquals(tmpInts == null ,true);
			
		} catch (EIMException eime) {
			eime.printStackTrace();
			
		} catch (Exception e) {
			e.printStackTrace();
			
		} finally {
			if (sess != null) {
				sess.close();
			}
		}

		
		
		
		/**
		 * 異常(1)
		 * 
		 * ・引数updaterがnullの場合
		 */
		try {
			isExceptionExist = false;
			
			sess = TestSessionUtil.createEIMSession();
			setEIMSessionForUnitTest(sess);
			
			// メソッドの実行
			AttributeUtil.updateAttribute(sess, null);
			
		} catch (EIMException eime) {
			
			isExceptionExist = true;
			assertEquals(eime.getMessage(), EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOTEXIST.INPUTDATA"));
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
		 * 異常(2)
		 * 
		 * ・DB上に存在しないオブジェクトを指定
		 */
		try {
			isExceptionExist = false;
			
			sess = TestSessionUtil.createEIMSession();
			setEIMSessionForUnitTest(sess);
			
			AttributeUpdater updater = new AttributeUpdater(Integer.MIN_VALUE, "存在しないEIMObject", null);
			
			// メソッドの実行
			AttributeUtil.updateAttribute(sess, updater);
			
		} catch (EIMException eime) {

			isExceptionExist = true;
			assertEquals(eime.getMessage(), EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOTEXIST.DOCFOL"));
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
		 * 異常(3)
		 * 
		 * ・対象オブジェクトのタイプには設定されていない属性を入力
		 */
		try {
			isExceptionExist = false;

			sess = TestSessionUtil.createEIMSession();
			setEIMSessionForUnitTest(sess);
			
			// テストデータ準備
			EIMObject baseObj = createTestData3(sess);
						
			// 属性情報の設定
			List itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeOnlyFolderId, ATTR_VALUE_STR_BY_STRING1, false, true));
			AttributeUpdater updater = new AttributeUpdater(baseObj.getId(), baseObj.getName(), itemList);
			
			// メソッドの実行
			AttributeUtil.updateAttribute(sess, updater);
			
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
		 * 異常(4)
		 * 
		 * ・入力した「名称割当て属性」と親の「下位引継ぎ属性」が重複
		 */
		try {
			isExceptionExist = false;

			sess = TestSessionUtil.createEIMSession();
			setEIMSessionForUnitTest(sess);
			
			// テストデータ準備
			EIMObject baseObj = createTestData2(sess);
			int baseObjId = baseObj.getId();
			
			// 事前確認
			viewTestData(sess, baseObj, "異常(4) - 事前確認(1)");
			
			// 属性情報の設定
			List itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeStrId, ATTR_VALUE_STR_BY_STRING1, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeIntId, ATTR_VALUE_INT_BY_STRING1, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeDateOneId, ATTR_VALUE_DATE_BY_STRING1_ONE, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeTextOneId, ATTR_VALUE_TEXT_BY_STRING1_ONE, false, true));
			AttributeUpdater updater = new AttributeUpdater(baseObj.getId(), baseObj.getName(), itemList);
			
			// メソッドの実行
			AttributeUtil.updateAttribute(sess, updater);
			sess.commit();
			
			// 再取得
			baseObj = ObjectUtils.getObjectById(sess, baseObjId);

			// 事前確認
			viewTestData(sess, baseObj, "異常(4) - 事前確認(2)");

			// 子オブジェクトの取得
			EIMObject childObj = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, FOLDER_TYPE_NAME1), FOLDER_NAME21);
			
			// 属性情報の設定
			List itemList2 = new ArrayList();
			itemList2.add(new AttributeUpdaterItem(attrTypeStrId, ATTR_VALUE_STR_BY_STRING2, true, true));
			AttributeUpdater updater2 = new AttributeUpdater(childObj.getId(), childObj.getName(), itemList2);
			
			// メソッドの実行
			AttributeUtil.updateAttribute(sess, updater2);
			
		} catch (EIMException eime) {

			isExceptionExist = true;
			assertEquals(eime.getMessage(), EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.ATTRINFO.REPETITION.NAME.ALLOCATION.REPETITION"));
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
		 * 異常(5)
		 * 
		 * ・入力した「属性」と親の「下位引継ぎ属性」が重複
		 */
		try {
			isExceptionExist = false;

			sess = TestSessionUtil.createEIMSession();
			setEIMSessionForUnitTest(sess);
			
			// テストデータ準備
			EIMObject baseObj = createTestData2(sess);
			int baseObjId = baseObj.getId();
			
			// 事前確認
			viewTestData(sess, baseObj, "異常(5) - 事前確認(1)");
			
			// 属性情報の設定
			List itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeStrId, ATTR_VALUE_STR_BY_STRING1, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeIntId, ATTR_VALUE_INT_BY_STRING1, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeDateOneId, ATTR_VALUE_DATE_BY_STRING1_ONE, false, true));
			itemList.add(new AttributeUpdaterItem(attrTypeTextOneId, ATTR_VALUE_TEXT_BY_STRING1_ONE, false, true));
			AttributeUpdater updater = new AttributeUpdater(baseObj.getId(), baseObj.getName(), itemList);
			
			// メソッドの実行
			AttributeUtil.updateAttribute(sess, updater);
			sess.commit();
			
			// 再取得
			baseObj = ObjectUtils.getObjectById(sess, baseObjId);

			// 事前確認
			viewTestData(sess, baseObj, "異常(5) - 事前確認(2)");

			// 子オブジェクトの取得
			EIMObject childObj = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, FOLDER_TYPE_NAME1), FOLDER_NAME21);
			
			// 属性情報の設定
			List itemList2 = new ArrayList();
			itemList2.add(new AttributeUpdaterItem(attrTypeStrId, ATTR_VALUE_STR_BY_STRING2, false, false));
			AttributeUpdater updater2 = new AttributeUpdater(childObj.getId(), childObj.getName(), itemList2);
			
			// メソッドの実行
			AttributeUtil.updateAttribute(sess, updater2);
			
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
		 * 異常(6)
		 * 
		 * ・入力した「下位引継ぎ属性」と子の「名称割当て属性」が重複
		 */
		try {
			isExceptionExist = false;

			sess = TestSessionUtil.createEIMSession();
			setEIMSessionForUnitTest(sess);
			
			// テストデータ準備
			EIMObject baseObj = createTestData2(sess);

			// 子オブジェクトの取得
			EIMObject childObj = ObjectUtils.getObjectByTypeAndName(sess, 
					ObjectUtils.getObjectTypeByName(sess, FOLDER_TYPE_NAME1), FOLDER_NAME21);

			// 事前確認
			viewTestData(sess, baseObj, "異常(6) - 事前確認(1)");
			
			// 属性情報の設定
			List itemList = new ArrayList();
			itemList.add(new AttributeUpdaterItem(attrTypeStrOneId, ATTR_VALUE_STR_BY_STRING1_ONE, true, false));
			itemList.add(new AttributeUpdaterItem(attrTypeIntId, ATTR_VALUE_INT_BY_STRING1, false, false));
			itemList.add(new AttributeUpdaterItem(attrTypeDateOneId, ATTR_VALUE_DATE_BY_STRING1_ONE, false, false));
			itemList.add(new AttributeUpdaterItem(attrTypeTextOneId, ATTR_VALUE_TEXT_BY_STRING1_ONE, false, false));
			AttributeUpdater updater = new AttributeUpdater(childObj.getId(), ATTR_VALUE_STR_BY_STRING1_ONE[0], itemList);
			
			// メソッドの実行
			AttributeUtil.updateAttribute(sess, updater);
			sess.commit();
			
			// 事前確認
			viewTestData(sess, baseObj, "異常(6) - 事前確認(2)");

			// 子オブジェクトの取得
			
			// 属性情報の設定
			List itemList2 = new ArrayList();
			itemList2.add(new AttributeUpdaterItem(attrTypeStrOneId, ATTR_VALUE_STR_BY_STRING2_ONE, false, true));
			AttributeUpdater updater2 = new AttributeUpdater(baseObj.getId(), baseObj.getName(), itemList2);
			
			// メソッドの実行
			AttributeUtil.updateAttribute(sess, updater2);
			
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
	 * テストデータ作成(1)  フォルダのみ存在、親のみ属性保持
	 * 
	 * @param sess EIMSessionインスタンス
	 * @return 起点となる親オブジェクト
	 * @throws Exception
	 */
	private EIMObject createTestData1(EIMSession sess) throws Exception {
		
		/* [構成]
		 * 
		 * obj1
		 *  |--obj21
		 *  |--obj22
		 *      |--obj3
		 */
		date = new Date();
		FOLDER_NAME1 = "フォルダ_1" + date;
		FOLDER_NAME21 = "フォルダ_21_" + date;
		FOLDER_NAME22 = "フォルダ_22_" + date;
		FOLDER_NAME3 = "フォルダ_3_" + date;
		
		// オブジェクト作成
		EIMObject obj1  = AppObjectUtil.createObject(sess, FOLDER_TYPE_NAME1, FOLDER_NAME1);
		EIMObject obj21 = AppObjectUtil.createObject(sess, FOLDER_TYPE_NAME1, FOLDER_NAME21);
		EIMObject obj22 = AppObjectUtil.createObject(sess, FOLDER_TYPE_NAME1, FOLDER_NAME22);
		EIMObject obj3  = AppObjectUtil.createObject(sess, FOLDER_TYPE_NAME1, FOLDER_NAME3);
		
		// 属性設定
		AppObjectUtil.setAttr(sess, obj1, ATTR_NAME_STR,  ATTR_VALUE_STR1);
		AppObjectUtil.setAttr(sess, obj1, ATTR_NAME_INT,  ATTR_VALUE_INT1);
		AppObjectUtil.setAttr(sess, obj1, ATTR_NAME_DATE, ATTR_VALUE_DATE1);
		AppObjectUtil.setAttr(sess, obj1, ATTR_NAME_TEXT, ATTR_VALUE_TEXT1);
		AppObjectUtil.setAttr(sess, obj1, ATTR_NAME_STR_ONE,  ATTR_VALUE_STR1_ONE);
		AppObjectUtil.setAttr(sess, obj1, ATTR_NAME_INT_ONE,  ATTR_VALUE_INT1_ONE);
		AppObjectUtil.setAttr(sess, obj1, ATTR_NAME_DATE_ONE, ATTR_VALUE_DATE1_ONE);
		AppObjectUtil.setAttr(sess, obj1, ATTR_NAME_TEXT_ONE, ATTR_VALUE_TEXT1_ONE);
		
		int objId = obj1.getId();
		
		// ドキュメントのリレーションタイプ取得
		EIMRelationType relType = RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));
		RelationUtils.createRelation(sess, relType, obj1, obj21);
		RelationUtils.createRelation(sess, relType, obj1, obj22);
		RelationUtils.createRelation(sess, relType, obj21, obj3);
		
		sess.commit();

		return ObjectUtils.getObjectById(sess, objId);
	}

	/**
	 * テストデータ作成(2)  フォルダの下にドキュメントが存在、みんな属性保持
	 * 
	 * @param sess EIMSessionインスタンス
	 * @return 起点となる親オブジェクト
	 * @throws Exception
	 */
	private EIMObject createTestData2(EIMSession sess) throws Exception {
		
		/* [構成]
		 * 
		 * obj1
		 *  |--obj2
		 *      |--doc1
		 */
		date = new Date();
		FOLDER_NAME1 = "フォルダ_1" + date;
		FOLDER_NAME21 = "フォルダ_21_" + date;
		DOC_NAME1 = "ドキュメント_1_" + date;
		
		// オブジェクト作成
		List objList = new ArrayList();
		EIMObject obj1  = AppObjectUtil.createObject(sess, FOLDER_TYPE_NAME1, FOLDER_NAME1);
		objList.add(obj1);
		EIMObject obj21 = AppObjectUtil.createObject(sess, FOLDER_TYPE_NAME1, FOLDER_NAME21);
		objList.add(obj21);
		EIMObject doc1  = AppObjectUtil.createObject(sess, DOCUMENT_TYPE_NAME1, DOC_NAME1);
		objList.add(doc1);
		
		// 属性設定
		String path = "/";
		for (Iterator iter = objList.iterator(); iter.hasNext();) {
			EIMObject obj = (EIMObject) iter.next();
			AppObjectUtil.setAttr(sess, obj, ATTR_NAME_STR,  ATTR_VALUE_STR1);
			AppObjectUtil.setAttr(sess, obj, ATTR_NAME_INT,  ATTR_VALUE_INT1);
			AppObjectUtil.setAttr(sess, obj, ATTR_NAME_DATE, ATTR_VALUE_DATE1);
			AppObjectUtil.setAttr(sess, obj, ATTR_NAME_TEXT, ATTR_VALUE_TEXT1);
			AppObjectUtil.setAttr(sess, obj, ATTR_NAME_STR_ONE,  ATTR_VALUE_STR1_ONE);
			AppObjectUtil.setAttr(sess, obj, ATTR_NAME_INT_ONE,  ATTR_VALUE_INT1_ONE);
			AppObjectUtil.setAttr(sess, obj, ATTR_NAME_DATE_ONE, ATTR_VALUE_DATE1_ONE);
			AppObjectUtil.setAttr(sess, obj, ATTR_NAME_TEXT_ONE, ATTR_VALUE_TEXT1_ONE);
			
			// パスの設定
			AppObjectUtil.setAttr(sess, obj, EIMConfig.get("ATTR_NAME_DOCUMENT_PASS"), path);
			path += obj.getName() + "/";
		}
		int objId = obj1.getId();
		
		// ドキュメントのリレーションタイプ取得
		EIMRelationType relType = RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));
		RelationUtils.createRelation(sess, relType, obj1, obj21);
		RelationUtils.createRelation(sess, relType, obj21, doc1);
		
		sess.commit();

		return ObjectUtils.getObjectById(sess, objId);
	}

	/**
	 * テストデータ作成(3)  ドキュメントのみ作成
	 * 
	 * @param sess EIMSessionインスタンス
	 * @return ドキュメントのオブジェクト
	 * @throws Exception
	 */
	private EIMObject createTestData3(EIMSession sess) throws Exception {
		
		/* [構成]
		 * 
		 * doc1
		 * 
		 */
		date = new Date();
		DOC_NAME1 = "ドキュメント_1_" + date;
		
		// オブジェクト作成
		List objList = new ArrayList();
		EIMObject doc1  = AppObjectUtil.createObject(sess, DOCUMENT_TYPE_NAME1, DOC_NAME1);
		objList.add(doc1);
		
		// 属性設定
		String path = "/";
		for (Iterator iter = objList.iterator(); iter.hasNext();) {
			EIMObject obj = (EIMObject) iter.next();
			AppObjectUtil.setAttr(sess, obj, ATTR_NAME_STR,  ATTR_VALUE_STR1);
			AppObjectUtil.setAttr(sess, obj, ATTR_NAME_INT,  ATTR_VALUE_INT1);
			AppObjectUtil.setAttr(sess, obj, ATTR_NAME_DATE, ATTR_VALUE_DATE1);
			AppObjectUtil.setAttr(sess, obj, ATTR_NAME_TEXT, ATTR_VALUE_TEXT1);
			AppObjectUtil.setAttr(sess, obj, ATTR_NAME_STR_ONE,  ATTR_VALUE_STR1_ONE);
			AppObjectUtil.setAttr(sess, obj, ATTR_NAME_INT_ONE,  ATTR_VALUE_INT1_ONE);
			AppObjectUtil.setAttr(sess, obj, ATTR_NAME_DATE_ONE, ATTR_VALUE_DATE1_ONE);
			AppObjectUtil.setAttr(sess, obj, ATTR_NAME_TEXT_ONE, ATTR_VALUE_TEXT1_ONE);
			
			// パスの設定
			AppObjectUtil.setAttr(sess, obj, EIMConfig.get("ATTR_NAME_DOCUMENT_PASS"), path);
			path += obj.getName() + "/";
		}
		int objId = doc1.getId();
		
		sess.commit();

		return ObjectUtils.getObjectById(sess, objId);
	}

	/**
	 * テストデータ作成(4)  フォルダのみ作成[1]
	 * 
	 * @param sess EIMSessionインスタンス
	 * @return ドキュメントのオブジェクト
	 * @throws Exception
	 */
	private EIMObject createTestData4(EIMSession sess) throws Exception {
		
		/* [構成]
		 * 
		 * folder1
		 * 
		 */
		date = new Date();
		FOLDER_NAME1 = "フォルダ_1" + date;
		
		// オブジェクト作成
		List objList = new ArrayList();
		EIMObject obj1  = AppObjectUtil.createObject(sess, FOLDER_TYPE_NAME1, FOLDER_NAME1);
		objList.add(obj1);
		
		// 属性設定
		String path = "/";
		for (Iterator iter = objList.iterator(); iter.hasNext();) {
			EIMObject obj = (EIMObject) iter.next();
			AppObjectUtil.setAttr(sess, obj, ATTR_NAME_STR,  ATTR_VALUE_STR1);
			AppObjectUtil.setAttr(sess, obj, ATTR_NAME_INT,  ATTR_VALUE_INT1);
			AppObjectUtil.setAttr(sess, obj, ATTR_NAME_DATE, ATTR_VALUE_DATE1);
			AppObjectUtil.setAttr(sess, obj, ATTR_NAME_TEXT, ATTR_VALUE_TEXT1);
			AppObjectUtil.setAttr(sess, obj, ATTR_NAME_STR_ONE,  ATTR_VALUE_STR1_ONE);
			AppObjectUtil.setAttr(sess, obj, ATTR_NAME_INT_ONE,  ATTR_VALUE_INT1_ONE);
			AppObjectUtil.setAttr(sess, obj, ATTR_NAME_DATE_ONE, ATTR_VALUE_DATE1_ONE);
			AppObjectUtil.setAttr(sess, obj, ATTR_NAME_TEXT_ONE, ATTR_VALUE_TEXT1_ONE);
			
			// パスの設定
			AppObjectUtil.setAttr(sess, obj, EIMConfig.get("ATTR_NAME_DOCUMENT_PASS"), path);
			path += obj.getName() + "/";
		}
		int objId = obj1.getId();
		
		sess.commit();

		return ObjectUtils.getObjectById(sess, objId);
	}

	/**
	 * テストデータ作成(5)  フォルダのみ作成[2]
	 * 
	 * @param sess EIMSessionインスタンス
	 * @return ドキュメントのオブジェクト
	 * @throws Exception
	 */
	private EIMObject createTestData5(EIMSession sess) throws Exception {
		
		/* [構成]
		 * 
		 * folder21
		 * 
		 */
		date = new Date();
		FOLDER_NAME1 = "フォルダ_21" + date;
		
		// オブジェクト作成
		List objList = new ArrayList();
		EIMObject obj1  = AppObjectUtil.createObject(sess, FOLDER_TYPE_NAME1, FOLDER_NAME1);
		objList.add(obj1);
		
		// 属性設定
		String path = "/";
		for (Iterator iter = objList.iterator(); iter.hasNext();) {
			EIMObject obj = (EIMObject) iter.next();
			AppObjectUtil.setAttr(sess, obj, ATTR_NAME_STR,  ATTR_VALUE_STR2);
			AppObjectUtil.setAttr(sess, obj, ATTR_NAME_INT,  ATTR_VALUE_INT2);
			AppObjectUtil.setAttr(sess, obj, ATTR_NAME_DATE, ATTR_VALUE_DATE2);
			AppObjectUtil.setAttr(sess, obj, ATTR_NAME_TEXT, ATTR_VALUE_TEXT2);
			AppObjectUtil.setAttr(sess, obj, ATTR_NAME_STR_ONE,  ATTR_VALUE_STR2_ONE);
			AppObjectUtil.setAttr(sess, obj, ATTR_NAME_INT_ONE,  ATTR_VALUE_INT2_ONE);
			AppObjectUtil.setAttr(sess, obj, ATTR_NAME_DATE_ONE, ATTR_VALUE_DATE2_ONE);
			AppObjectUtil.setAttr(sess, obj, ATTR_NAME_TEXT_ONE, ATTR_VALUE_TEXT2_ONE);
			
			// パスの設定
			AppObjectUtil.setAttr(sess, obj, EIMConfig.get("ATTR_NAME_DOCUMENT_PASS"), path);
			path += obj.getName() + "/";
		}
		int objId = obj1.getId();
		
		sess.commit();

		return ObjectUtils.getObjectById(sess, objId);
	}

	/**
	 * テストデータ作成(6)  フォルダ「WF付-子」それぞれに属性値なし
	 * 
	 * @param sess EIMSessionインスタンス
	 * @return 起点となる親オブジェクト
	 * @throws Exception
	 */
	private EIMObject createTestData6(EIMSession sess) throws Exception {
		
		/* [構成]
		 * 
		 * (WF)obj1
		 *  |---------obj1
		 */
		date = new Date();
		WF_FOLDER_NAME_1 = "WFフォルダ_1_" + date;
		FOLDER_NAME1 = "フォルダ_1_" + date;
		
		// オブジェクト作成
		EIMObject objWf = AppObjectUtil.createObject(sess, WF_FOLDER_TYPE_NAME1, WF_FOLDER_NAME_1);
		EIMObject objA  = AppObjectUtil.createObject(sess, FOLDER_TYPE_NAME1, FOLDER_NAME1);
				
		int objId = objWf.getId();
		
		// ドキュメントのリレーションタイプ取得
		EIMRelationType relType = RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));
		RelationUtils.createRelation(sess, relType, objWf, objA);
		
		List objList = new ArrayList();
		objList.add(objWf);
		objList.add(objA);

		// ステータス/セキュリティ/上位WF付きフォルダ
		setChildObjStatusSecurity(sess, objList, SECURITY_NAME1);
		
		sess.commit();

		return ObjectUtils.getObjectById(sess, objId);
	}

	/**
	 * テストデータ作成(7)  フォルダ「親-子-孫」それぞれに属性値なし
	 * 
	 * @param sess EIMSessionインスタンス
	 * @return 起点となる親オブジェクト
	 * @throws Exception
	 */
	private EIMObject createTestData7(EIMSession sess) throws Exception {
		
		/* [構成]
		 * 
		 * objA
		 *  |--objB
		 *      |--objC
		 */
		date = new Date();
		FOLDER_NAME_A = "フォルダ_A_" + date;
		FOLDER_NAME_B = "フォルダ_B_" + date;
		FOLDER_NAME_C = "フォルダ_C_" + date;
		
		// オブジェクト作成
		EIMObject objA  = AppObjectUtil.createObject(sess, FOLDER_TYPE_NAME1, FOLDER_NAME_A);
		EIMObject objB = AppObjectUtil.createObject(sess, FOLDER_TYPE_NAME1, FOLDER_NAME_B);
		EIMObject objC  = AppObjectUtil.createObject(sess, FOLDER_TYPE_NAME1, FOLDER_NAME_C);
				
		int objId = objA.getId();
		
		// ドキュメントのリレーションタイプ取得
		EIMRelationType relType = RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));
		RelationUtils.createRelation(sess, relType, objA, objB);
		RelationUtils.createRelation(sess, relType, objB, objC);

		List objList = new ArrayList();
		objList.add(objA);
		objList.add(objB);
		objList.add(objC);

		// ステータス/セキュリティ/上位WF付きフォルダ
		setChildObjStatusSecurity(sess, objList, SECURITY_NAME2);
		
		sess.commit();

		return ObjectUtils.getObjectById(sess, objId);
	}
	
	/**
	 * テストデータ作成(8)  フォルダ「親-子-WF孫」それぞれに属性値なし
	 * 
	 * @param sess EIMSessionインスタンス
	 * @return 起点となる親オブジェクト
	 * @throws Exception
	 */
	private EIMObject createTestData8(EIMSession sess) throws Exception {
		
		/* [構成]
		 * 
		 * obj1
		 *  |--obj21
		 *       |--(WF)obj3
		 */
		date = new Date();
		FOLDER_NAME1 = "フォルダ_1_" + date;
		FOLDER_NAME21 = "フォルダ_21_" + date;
		WF_FOLDER_NAME_1 = "WFフォルダ_3_" + date;
		
		// オブジェクト作成
		EIMObject obj1  = AppObjectUtil.createObject(sess, FOLDER_TYPE_NAME1, FOLDER_NAME1);
		EIMObject obj21 = AppObjectUtil.createObject(sess, FOLDER_TYPE_NAME1, FOLDER_NAME21);
		EIMObject obj3  = AppObjectUtil.createObject(sess, WF_FOLDER_TYPE_NAME1, WF_FOLDER_NAME_1);
				
		int objId = obj1.getId();
		
		// ドキュメントのリレーションタイプ取得
		EIMRelationType relType = RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));
		RelationUtils.createRelation(sess, relType, obj1,  obj21);
		RelationUtils.createRelation(sess, relType, obj21, obj3);
		
		List objList = new ArrayList();
		objList.add(obj1);
		objList.add(obj21);
		objList.add(obj3);

		// ステータス/セキュリティ/上位WF付きフォルダ
		setChildObjStatusSecurity(sess, objList, SECURITY_NAME1);
		
		sess.commit();

		return ObjectUtils.getObjectById(sess, objId);
	}

	
	/**
	 * テストデータ作成(9)  「親フォルダ-子ドキュメント」それぞれに属性値なし
	 * 
	 * @param sess EIMSessionインスタンス
	 * @return 起点となる親オブジェクト
	 * @throws Exception
	 */
	private EIMObject createTestData9(EIMSession sess) throws Exception {
		
		/* [構成]
		 * 
		 * objA
		 *  |--doc1
		 */
		date = new Date();
		FOLDER_NAME_A = "フォルダ_A_" + date;
		DOC_NAME1 = "ドキュメント1_" + date;
		
		// オブジェクト作成
		EIMObject objA  = AppObjectUtil.createObject(sess, FOLDER_TYPE_NAME1, FOLDER_NAME_A);
		EIMObject doc1 = AppObjectUtil.createObject(sess, DOCUMENT_TYPE_NAME1, DOC_NAME1);
				
		int objId = objA.getId();
		
		// ドキュメントのリレーションタイプ取得
		EIMRelationType relType = RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));
		RelationUtils.createRelation(sess, relType, objA, doc1);

		List objList = new ArrayList();
		objList.add(objA);
		objList.add(doc1);

		// ステータス/セキュリティ/上位WF付きフォルダ
		setChildObjStatusSecurity(sess, objList, SECURITY_NAME2);
		
		sess.commit();

		return ObjectUtils.getObjectById(sess, objId);
	}

	
	/**
	 * テストデータ作成(10)  フォルダ「親-子」それぞれに属性値なし
	 * 
	 * @param sess EIMSessionインスタンス
	 * @return 起点となる親オブジェクト
	 * @throws Exception
	 */
	private EIMObject createTestData10(EIMSession sess) throws Exception {
		
		/* [構成]
		 * 
		 * obj1
		 *  |--obj21
		 */
		date = new Date();
		FOLDER_NAME1 = "フォルダ_1_" + date;
		FOLDER_NAME21 = "フォルダ_21_" + date;
		
		// オブジェクト作成
		EIMObject obj1  = AppObjectUtil.createObject(sess, FOLDER_TYPE_NAME1, FOLDER_NAME1);
		EIMObject obj21 = AppObjectUtil.createObject(sess, FOLDER_TYPE_NAME1, FOLDER_NAME21);
				
		int objId = obj1.getId();
		
		// ドキュメントのリレーションタイプ取得
		EIMRelationType relType = RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));
		RelationUtils.createRelation(sess, relType, obj1,  obj21);
		
		List objList = new ArrayList();
		objList.add(obj1);
		objList.add(obj21);

		// ステータス/セキュリティ/上位WF付きフォルダ
		setChildObjStatusSecurity(sess, objList, SECURITY_NAME1);
		
		sess.commit();

		return ObjectUtils.getObjectById(sess, objId);
	}

	
	
	/**
	 * テストデータ作成(11)  「親フォルダ-子WFドキュメント」それぞれに属性値なし
	 * 
	 * @param sess EIMSessionインスタンス
	 * @return 起点となる親オブジェクト
	 * @throws Exception
	 */
	private EIMObject createTestData11(EIMSession sess) throws Exception {
		
		/* [構成]
		 * 
		 * objA
		 *  |--(WF)doc1
		 */
		date = new Date();
		FOLDER_NAME_A = "フォルダ_A_" + date;
		WF_DOCUMENT_NAME_1 = "WFドキュメント1_" + date;
		
		// オブジェクト作成
		EIMObject objA  = AppObjectUtil.createObject(sess, FOLDER_TYPE_NAME1, FOLDER_NAME_A);
		EIMObject doc1 = AppObjectUtil.createObject(sess, WF_DOCUMENT_TYPE_NAME1, WF_DOCUMENT_NAME_1);
				
		int objId = objA.getId();
		
		// ドキュメントのリレーションタイプ取得
		EIMRelationType relType = RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));
		RelationUtils.createRelation(sess, relType, objA, doc1);

		List objList = new ArrayList();
		objList.add(objA);
		objList.add(doc1);

		// ステータス/セキュリティ/上位WF付きフォルダ
		setChildObjStatusSecurity(sess, objList, SECURITY_NAME2);
		
		sess.commit();

		return ObjectUtils.getObjectById(sess, objId);
	}

	
	
	/**
	 * テストデータ作成(12)  フォルダのみ作成[3]
	 * 
	 * @param sess EIMSessionインスタンス
	 * @return ドキュメントのオブジェクト
	 * @throws Exception
	 */
	private EIMObject createTestData12(EIMSession sess) throws Exception {
		
		/* [構成]
		 * 
		 * folder22
		 * 
		 */
		date = new Date();
		FOLDER_NAME22 = "フォルダ_22" + date;
		
		// オブジェクト作成
		List objList = new ArrayList();
		EIMObject obj1  = AppObjectUtil.createObject(sess, FOLDER_TYPE_NAME1, FOLDER_NAME22);
		objList.add(obj1);
		
		// 属性設定
		String path = "/";
		for (Iterator iter = objList.iterator(); iter.hasNext();) {
			EIMObject obj = (EIMObject) iter.next();
			AppObjectUtil.setAttr(sess, obj, ATTR_NAME_STR,  ATTR_VALUE_STR2);
			AppObjectUtil.setAttr(sess, obj, ATTR_NAME_INT,  ATTR_VALUE_INT2);
			AppObjectUtil.setAttr(sess, obj, ATTR_NAME_DATE, ATTR_VALUE_DATE2);
			AppObjectUtil.setAttr(sess, obj, ATTR_NAME_TEXT, ATTR_VALUE_TEXT2);
			AppObjectUtil.setAttr(sess, obj, ATTR_NAME_STR_ONE,  ATTR_VALUE_STR2_ONE);
			AppObjectUtil.setAttr(sess, obj, ATTR_NAME_INT_ONE,  ATTR_VALUE_INT2_ONE);
			AppObjectUtil.setAttr(sess, obj, ATTR_NAME_DATE_ONE, ATTR_VALUE_DATE2_ONE);
			AppObjectUtil.setAttr(sess, obj, ATTR_NAME_TEXT_ONE, ATTR_VALUE_TEXT2_ONE);
			
			// パスの設定
			AppObjectUtil.setAttr(sess, obj, EIMConfig.get("ATTR_NAME_DOCUMENT_PASS"), path);
			path += obj.getName() + "/";
		}
		int objId = obj1.getId();
		
		sess.commit();

		return ObjectUtils.getObjectById(sess, objId);
	}


	
	/**
	 * デバッグ用のテスト結果の表示
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param baseObj 起点となるオブジェクト
	 * @param message コンソールに出力するメッセージ
	 * @throws Exception
	 */
	private void viewTestData(EIMSession sess, EIMObject baseObj, String message) throws Exception {
		
		if (!_viewTestData) {
			return;
		}
		
		List objList = new ArrayList();
		getChildObjList(sess, baseObj, objList);
		
		System.out.println("<< " + message + " >>-----------------------------------");
		if (objList != null) {
			for (Iterator iter = objList.iterator(); iter.hasNext();) {
				
				EIMObject obj = (EIMObject) iter.next();
				System.out.println("EIMObject/" + obj.getId() + "/"+ obj.getName());
				
				System.out.print("ステータス: ");
				if (obj.getStatus() != null) {
					System.out.println(obj.getStatus().getType().getId() + "/" + obj.getStatus().getType().getName());
				} else {System.out.println("");}

				System.out.print("セキュリティ: ");
				if (obj.getSecurity() != null) {
					System.out.println(obj.getSecurityId() + "/" + obj.getSecurity().getDefName());
				} else {System.out.println("");}

				List attrTypeList = ObjectAttributeUtils.getAttributeTypeList(sess, obj.getType());
				if (attrTypeList != null) {
					System.out.println("属性数: "+ attrTypeList.size());
					for (int i = 0 ; i < attrTypeList.size() ; i++) {
						
						EIMAttributeType attrType = (EIMAttributeType)attrTypeList.get(i);
						System.out.print("[" + attrType.getId() + "/" + attrType.getDefaultName() + "] ");
						
						switch (attrType.getValueType().getId()) {
						
							// 文字列型の場合
							case EIMValueType.STRING:
								String strAttrs[] = AppObjectUtil.getStrAttrs(sess, obj, attrType.getDefaultName());
								if (strAttrs != null) {
									for (int j = 0 ; j < strAttrs.length ; j++) {
										System.out.print(strAttrs[j] + ", ");
									}
								}
								break;
								
							// 数値型の場合
							case EIMValueType.INTEGER:		
								int intAttrs[] = AppObjectUtil.getIntAttrs(sess, obj, attrType.getDefaultName());
								if (intAttrs != null) {
									for (int j = 0 ; j < intAttrs.length ; j++) {
										System.out.print(intAttrs[j] + ", ");
									}
								}
								break;
								
							// 日付型の場合
							case EIMValueType.DATE:
								Date dateAttrs[] = AppObjectUtil.getDateAttrs(sess, obj, attrType.getDefaultName());
								if (dateAttrs != null) {
									for (int j = 0 ; j < dateAttrs.length ; j++) {
										System.out.print(dateAttrs[j] + ", ");
									}
								}
								break;
								
							// テキスト型の場合
							case EIMValueType.TEXT:
								String textAttrs[] = AppObjectUtil.getTextAttrs(sess, obj, attrType.getDefaultName());
								if (textAttrs != null) {
									for (int j = 0 ; j < textAttrs.length ; j++) {
										System.out.print(textAttrs[j] + ", ");
									}
								}
								break;
						}
						System.out.println("");
					}
					System.out.println("");
				}
			}
		}
	}
	
	private void getChildObjList(EIMSession sess, EIMObject obj, List retList) throws Exception {
		
		retList.add(obj);
		// 子オブジェクトのリレーションリストを取得
		List childRelList = RelationUtils.getChildRelationList(sess, obj);
		if (childRelList != null) {
			for (Iterator iter = childRelList.iterator(); iter.hasNext();) {
				// 子オブジェクトの取得
				EIMObject childObj = ((EIMRelation)iter.next()).getChild();
				getChildObjList(sess, ObjectUtils.getObjectById(sess, childObj.getId()), retList);
			}
		}
	}
	
	private void setEIMSessionForUnitTest(EIMSession sess) throws Exception {
	
		// JUnit用にダミーの時差を作成
		sess.setAttribute("clTzOffset", "-32400000");
		String dbTzOffset = String.valueOf(eim.util.DateUtils.selectDBTzOffset(sess));
		sess.setAttribute("dbTzOffset", dbTzOffset);
	}
	
	private String getDateString(EIMSession sess, Date date) throws Exception {
		
		String format = EIMResource.getMessage(sess, "EIM.FORMAT.DATE");
		return StringUtils.getDateStringByFormat(date, format);
	}
	
	/**
	 * リストの先頭のEIMObjectのステータスを引き継ぎ、セキュリティ、WFフォルダを設定します
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param objList EIMObjectのリスト
	 * @param secName セキュリティ名
	 * @throws Exception
	 */
	private void setChildObjStatusSecurity(EIMSession sess, List objList, String secName) throws Exception {
		
		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
		
		EIMStatus sts = null;
		EIMSecurity sec = SecurityUtils.getSecurityByName(sess, secName);
		int wfObjId = Integer.MIN_VALUE;
		
		for (Iterator iter = objList.iterator(); iter.hasNext();) {
			EIMObject obj = (EIMObject) iter.next();
			// ステータスの引継ぎ
			if (sts == null) {
				sts = obj.getStatus();
			} else {
				WorkFlowUtils.updateObjectStatus(sess, obj, sts);
			}
			// セキュリティの設定
			SecurityUtils.setSecurity(sess, obj, sec);
			
			// 上位WF付きフォルダの設定
			if (helper.isTypeOfFolderWithWorkflow(obj)) {
				wfObjId = obj.getId();
			} else if (wfObjId != Integer.MIN_VALUE) {
				AppObjectUtil.setAttr(sess, obj, EIMConfig.get("ATTR_NAME_FOLDER_HIGHER_WF_FOLDER"), wfObjId);
			}
		}
	}
	
	/**
	 * リレーションの変更を実施します。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param object 移動対象オブジェクト
	 * @param parentObj 親オブジェクト
	 * @throws Exception
	 */
	private void changeRelation(EIMSession sess, EIMObject object, EIMObject parentObj) throws Exception {
		
		EIMRelationType relType = RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));

		//Get Parent Relation And Delete
		List parentRelList = RelationUtils.getParentRelationListByRelType(sess, object, relType);
		for(int i = 0; i < parentRelList.size(); i++)
		{
			RelationUtils.deleteRelation(sess, (EIMRelation)parentRelList.get(i));
		}
		//Create Relation
		RelationUtils.createRelation(sess, relType, parentObj, object, EIMConstant.DEPU_CHECK_NAME_REV);
	}
	
	/**
	 * オブジェクトをコピーして、コピー先オブジェクトとリレーション設定した後、オブジェクトを返却します。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param object コピー対象オブジェクト
	 * @param parentObj コピー先オブジェクト
	 * @return 生成したEIMObject
	 * @throws Exception
	 */
	private EIMObject copyObject(EIMSession sess, EIMObject object, EIMObject parentObj) throws Exception {
		
		//Relation Type
		EIMRelationType relType = RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));
		
		//New Object Name
		String newObjName = object.getName();
		
		//Create Object
		EIMObject newObj = ObjectUtils.createObject(sess, object.getType(), newObjName);
		int newObjId =newObj.getId();
		
		//Create Relation
		while(true) {
			try {
				RelationUtils.createRelation(sess, relType, parentObj, newObj, EIMConstant.DEPU_CHECK_NAME_REV);
			} catch(EIMException ecp) {
				int errCode = ecp.getCode();
				if(errCode == 925) {				
					String langId = sess.getLangId();
					newObjName = EIMConfig.get("COPY_FILENAME_PREFIX_" + langId)
							+ " - " + newObjName;

					ObjectUtils.rename(sess, newObj, newObjName);
					continue;
				}
			}
			break;
		}
		
		//Inherit Attribute
		ObjectAttributeUtils.inheritAttribute(sess, object, newObj);

		return ObjectUtils.getObjectById(sess, newObjId);
	}
	
	
	/**
	 * オブジェクトの論理削除を実施します。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param obj 削除対象オブジェクト
	 * @throws Exception
	 */
	private void logicDeleteObject(EIMSession sess, EIMObject obj) throws Exception {

		//Relation Type
		EIMRelationType relType = RelationUtils.getRelationTypeByName(
				sess, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));
		
		/* リレーションの削除 */
		List parentRelList = RelationUtils.getParentRelationListByRelType(sess, obj, relType);
		for (int i = 0; i < parentRelList.size(); i++) {
			RelationUtils.deleteRelation(sess, (EIMRelation)parentRelList.get(i));
		}

		// ごみ箱オブジェクト
		EIMObject recycleObj = AppObjectUtil.getObject(sess, 
				EIMConfig.get("OBJECT_TYPE_NAME_RECYCLE"), EIMConfig.get("OBJECT_TYPE_NAME_RECYCLE"));
		
		/* ごみ箱への関連付け */
		RelationUtils.createRelation(sess, relType, recycleObj, obj, EIMConstant.DEPU_CHECK_NONE);
	}
	
	
	/**
	 * 親オブジェクトを元に設定すべきパスを生成します。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param parentObj 親オブジェクト
	 * @return パス
	 * @throws Exception
	 */
	private String getPath(EIMSession sess, EIMObject parentObj) throws Exception {
	
		String path = "";
		if(parentObj.getAttribute("パス") != null)
		{
			path = parentObj.getAttribute("パス").getString();
		} else {
			// ワークスペースの場合、パス属性の値を保持していない
			path = "/";
		}
		path += parentObj.getName() + "/";
		return path;
	}
}