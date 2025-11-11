package common.util;

import java.util.Iterator;
import java.util.List;
import java.util.Map;

import junit.extensions.TestSetup;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import eim.bo.EIMAccessRole;
import eim.bo.EIMException;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMSecurity;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.ObjectUtils;
import eim.util.RelationUtils;
import eim.util.SecurityUtils;
import eim.util.UserUtils;
import eimtest.app.util.CreateFolderTreeData;
import eimtest.app.util.TestAppDBUtil;
import eimtest.app.util.TestAppObjectUtil;
import eimtest.util.TestClassUtils;
import eimtest.util.TestSessionUtil;

/** */
public class AppObjectConditionHelperTest extends TestCase {
	/**
	 * 
	 * @return o
	 * @throws Exception
	 */
	public static Test suite() throws Exception {
		TestSuite suite = new TestSuite(AppObjectConditionHelperTest.class);
		TestSetup w = new TestSetup(suite) {

			/** */
			public void setUp() throws Exception {
				if (true) {
					TestAppDBUtil.loadPrimitiveData();
					EIMSession sess = TestSessionUtil.createEIMSession();
					new CreateFolderTreeData(sess, null).process();// テスト用フォルダツリーを作る
					sess.commit();
				}
			}
		};
		return w;
	}

	/** */
	EIMSession sess;

	/** */
	AppObjectConditionHelper target;

	public void setUp() throws Exception {
		sess = TestSessionUtil.createEIMSession(null);
		target = new AppObjectConditionHelper(sess);
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testGetAttrTypeOfFromHighAttr() throws Exception {
		assertEquals(target.getAttrNameOfFromHighAttr(), target.getAttrTypeOfFromHighAttr().getDefName());
		assertSame(target.getAttrTypeOfFromHighAttr(), target.getAttrTypeOfFromHighAttr());
	}

	/** */
	public void testGetAttrNameOfFromHighAttr() {
		assertEquals("上位からの引継ぎ属性", target.getAttrNameOfFromHighAttr());
	}
	
	/**
	 * 
	 * 
	 */
	public void testAppObjectConditionHelper() {
		assertSame(sess, TestClassUtils.get(target.getClass(), "_sess", target));
		assertSame(sess, target.getSession());
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testCheckUpdatableToFolder() throws Exception {
		sess.setAttribute(EIMSession.USER, UserUtils.getUserByCode(sess, "tu2"));
		EIMObject obj = TestAppObjectUtil.getObj(sess, "F編集中");
		assertTrue(target.checkUpdatableToFolder(obj, -1));
		assertTrue(target.checkUpdatableToFolder(obj, EIMAccessRole.READ));
		assertTrue(target.checkUpdatableToFolder(obj, EIMAccessRole.STATUS_UP));
		assertFalse(target.checkUpdatableToFolder(obj, EIMAccessRole.UPDATE));

		// 下位フォルダ管理セキュリティによる制限-NG
		EIMSecurity sec = SecurityUtils.getSecurityByName(sess, "ツリーテスト用セキュリティ");
		sess.setAttribute(EIMSession.USER, UserUtils.getUserByCode(sess, "tu1"));// データ更新の為にユーザー変更・・・
		AppObjectUtil.setAttr(sess, obj, "下位フォルダ管理セキュリティ", sec.getId());
		obj = ObjectUtils.getObjectById(sess, obj.getId());
		sess.setAttribute(EIMSession.USER, UserUtils.getUserByCode(sess, "tu2"));
		assertFalse(target.checkUpdatableToFolder(obj, EIMAccessRole.STATUS_UP));

		// 下位フォルダ管理セキュリティによる制限-OK
		sess.setAttribute(EIMSession.USER, UserUtils.getUserByCode(sess, "tu1"));
		assertTrue(target.checkUpdatableToFolder(obj, EIMAccessRole.STATUS_UP));
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testGetAttrTypeOfHigherWFFolder() throws Exception {
		assertEquals("上位WFフォルダ", target.getAttrTypeOfHigherWFFolder().getDefName());
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testIsTypeOfFolderWithWorkflow() throws Exception {
		assertTrue(target.isTypeOfFolderWithWorkflow(TestAppObjectUtil.getObj(sess, "F編集中")));
		assertFalse(target.isTypeOfFolderWithWorkflow(TestAppObjectUtil.getObj(sess, "F編集中_F")));
		assertFalse(target.isTypeOfFolderWithWorkflow(TestAppObjectUtil.getObj(sess, "F普通のフォルダ")));
		assertFalse(target.isTypeOfFolderWithWorkflow(TestAppObjectUtil.getObj(sess, "公開中1.txt")));
		assertFalse(target.isTypeOfFolderWithWorkflow(TestAppObjectUtil.getObj(sess, "ステータスなし1.txt")));
		assertFalse(target.isTypeOfFolderWithWorkflow(TestAppObjectUtil.getObj(sess, "プロジェクトX")));
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testIsTypeOfFolderUnderFolderWithWorkflow() throws Exception {
		assertFalse(target.isTypeOfFolderUnderFolderWithWorkflow(TestAppObjectUtil.getObj(sess,
			"F編集中")));
		assertTrue(target.isTypeOfFolderUnderFolderWithWorkflow(TestAppObjectUtil.getObj(sess,
			"F編集中_F")));
		assertFalse(target.isTypeOfFolderUnderFolderWithWorkflow(TestAppObjectUtil.getObj(sess,
			"F普通のフォルダ")));
		assertFalse(target.isTypeOfFolderUnderFolderWithWorkflow(TestAppObjectUtil.getObj(sess,
			"公開中1.txt")));
		assertFalse(target.isTypeOfFolderUnderFolderWithWorkflow(TestAppObjectUtil.getObj(sess,
			"ステータスなし1.txt")));
		assertFalse(target.isTypeOfFolderUnderFolderWithWorkflow(TestAppObjectUtil.getObj(sess,
			"プロジェクトX")));
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testIsUnderFolderWithWorkflow() throws Exception {
		assertFalse(target.isUnderFolderWithWorkflow(TestAppObjectUtil.getObj(sess, "F編集中")));
		assertTrue(target.isUnderFolderWithWorkflow(TestAppObjectUtil.getObj(sess, "F編集中_F")));
		assertFalse(target.isUnderFolderWithWorkflow(TestAppObjectUtil.getObj(sess, "F普通のフォルダ")));
		assertFalse(target.isUnderFolderWithWorkflow(TestAppObjectUtil.getObj(sess, "公開中1.txt")));
		assertFalse(target.isUnderFolderWithWorkflow(TestAppObjectUtil.getObj(sess, "ステータスなし1.txt")));
		assertFalse(target.isUnderFolderWithWorkflow(TestAppObjectUtil.getObj(sess, "プロジェクトX")));
		assertTrue(target.isUnderFolderWithWorkflow(TestAppObjectUtil.getObj(sess, "F編集中1.txt")));
		assertTrue(target.isUnderFolderWithWorkflow(TestAppObjectUtil.getObj(sess, "F編集中3.txt")));
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testGetUpperFolderWithWorkflowObjId() throws Exception {
		assertEquals(TestAppObjectUtil.getObj(sess, "F編集中").getId(),
			target.getUpperFolderWithWorkflowObjId(TestAppObjectUtil.getObj(sess, "F編集中_F")));
		assertEquals(-1, target.getUpperFolderWithWorkflowObjId(TestAppObjectUtil.getObj(sess,
			"F編集中")));
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testCheckAccessibleStatusSelf() throws Exception {
		target.checkAccessibleStatusSelf(TestAppObjectUtil.getObj(sess, "F編集中_F"), true);// no
																							// exception

		checkAccess("tu1",// 書込権
			new boolean[] { true // "F編集中"
					, true // "F承認依頼中"
					, true// "F公開処理中"
					, true// "F公開処理中で失敗"
					, true// "F公開中"
					, true// "F普通のフォルダ"//フォルダ

					, true// "F編集中1.txt"
					, true// "F承認依頼中1.txt"
					, true// "F公開処理中1.txt"
					, true// "F公開処理中で失敗1.txt"
					, true// "F公開中1.txt"
					, true// "F普通のフォルダ_1.txt"//フォルダの下のドキュメント

					, true// "編集中1.txt"
					, true// "承認依頼中1.txt"
					, true// "公開処理中1.txt"
					, true// "公開処理中で失敗1.txt"
					, true // "公開中1.txt"//ワークフロー付きフォルダの下ではないドキュメント });
			});
		checkAccess("tu2",// ステータス変更権
			new boolean[] { true // "F編集中"
					, true // "F承認依頼中"
					, true// "F公開処理中"
					, true// "F公開処理中で失敗"
					, true// "F公開中"
					, true// "F普通のフォルダ"//フォルダ

					, true// "F編集中1.txt"
					, true// "F承認依頼中1.txt"
					, true// "F公開処理中1.txt"
					, true// "F公開処理中で失敗1.txt"
					, true// "F公開中1.txt"
					, true// "F普通のフォルダ_1.txt"//フォルダの下のドキュメント

					, true// "編集中1.txt"
					, true// "承認依頼中1.txt"
					, true// "公開処理中1.txt"
					, true// "公開処理中で失敗1.txt"
					, true // "公開中1.txt"//ワークフロー付きフォルダの下ではないドキュメント });
			});
		checkAccess("tu3",// 常時読取権
			new boolean[] { true // "F編集中"
					, true // "F承認依頼中"
					, true// "F公開処理中"
					, true// "F公開処理中で失敗"
					, true// "F公開中"
					, true// "F普通のフォルダ"//フォルダ

					, true// "F編集中1.txt"
					, true// "F承認依頼中1.txt"
					, true// "F公開処理中1.txt"
					, true// "F公開処理中で失敗1.txt"
					, true// "F公開中1.txt"
					, true// "F普通のフォルダ_1.txt"//フォルダの下のドキュメント

					, true// "編集中1.txt"
					, true// "承認依頼中1.txt"
					, true// "公開処理中1.txt"
					, true// "公開処理中で失敗1.txt"
					, true // "公開中1.txt"//ワークフロー付きフォルダの下ではないドキュメント });
			});
		checkAccess("tu4",// 公開読取権
			new boolean[] { false // "F編集中"
					, false // "F承認依頼中"
					, false// "F公開処理中"
					, false// "F公開処理中で失敗"
					, true// "F公開中"
					, true// "F普通のフォルダ"//フォルダ

					, false// "F編集中1.txt"
					, false// "F承認依頼中1.txt"
					, false// "F公開処理中1.txt"
					, false// "F公開処理中で失敗1.txt"
					, true// "F公開中1.txt"
					, true// "F普通のフォルダ_1.txt"//フォルダの下のドキュメント

					, false// "編集中1.txt"
					, false// "承認依頼中1.txt"
					, false// "公開処理中1.txt"
					, false// "公開処理中で失敗1.txt"
					, true // "公開中1.txt"//ワークフロー付きフォルダの下ではないドキュメント });
			});

		try {
			target.checkAccessibleStatusSelf(TestAppObjectUtil.getObj(sess, "F編集中_F"), true);// login
																								// is
																								// tu4.
																								// exception
			// happen
			fail();
		} catch (EIMException e) {
			assertEquals("EIM.ERROR.LOGIC.NOACCESS", e.getMessageKey());
		}

	}

	/**
	 * 
	 * @param userCode
	 * @param accessFlags
	 * @throws Exception
	 */
	private void checkAccess(String userCode, boolean[] accessFlags) throws Exception {
		sess.setAttribute(EIMSession.USER, UserUtils.getUserByCode(sess, userCode));
		target = new AppObjectConditionHelper(sess);
		String[] objNames = {//
		"F編集中", "F承認依頼中", "F公開処理中", "F公開処理中で失敗", "F公開中",
				"F普通のフォルダ"// フォルダ
				, "F編集中1.txt", "F承認依頼中1.txt", "F公開処理中1.txt", "F公開処理中で失敗1.txt", "F公開中1.txt",
				"F普通のフォルダ_1.txt"// フォルダの下のドキュメント
				, "編集中1.txt", "承認依頼中1.txt", "公開処理中1.txt", "公開処理中で失敗1.txt", "公開中1.txt"// ワークフロー付きフォルダの下ではないドキュメント
		};
		for (int i = 0; i < objNames.length; i++) {
			assertTrue(userCode + ":" + objNames[i] + ":" + accessFlags[i],
				accessFlags[i] == target.checkAccessibleStatusSelf(TestAppObjectUtil.getObj(sess,
					objNames[i]), false));
		}
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testGetUpperObject() throws Exception {
		assertEquals("F編集中",
			target.getUpperObject(TestAppObjectUtil.getObj(sess, "F編集中1.txt")).getName());
		assertEquals(TestAppObjectUtil.getObj(sess, "%-WFステータステスト").getId(), target.getUpperObject(
			TestAppObjectUtil.getObj(sess, "プロジェクトX")).getId());
		assertNull(target.getUpperObject(TestAppObjectUtil.getObj(sess, "%-WFステータステスト")));
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testGetUpperObjectsToToplevel() throws Exception {
		List uppers = target.getUpperObjectsToToplevel(TestAppObjectUtil.getObj(sess, "F編集中_F"));
		assertEquals(3, uppers.size());
		EIMObject u1 = (EIMObject) uppers.get(0);
		EIMObject u2 = (EIMObject) uppers.get(1);
		EIMObject u3 = (EIMObject) uppers.get(2);
		assertEquals("F編集中", u1.getName());
		assertEquals("プロジェクトX", u2.getName());
		assertTrue(u3.getName().endsWith("WFステータステスト"));
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testIsTypeOfDocument() throws Exception {
		assertTrue(target.isTypeOfDocument(TestAppObjectUtil.getObj(sess, "F編集中1.txt").getType()));
		assertFalse(target.isTypeOfDocument(TestAppObjectUtil.getObj(sess, "F編集中").getType()));
		assertFalse(target.isTypeOfDocument(TestAppObjectUtil.getObj(sess, "%-WFステータステスト").getType()));
		assertFalse(target.isTypeOfDocument(ObjectUtils.getObjectTypeByName(sess, "マイドキュメント")));
		assertFalse(target.isTypeOfDocument(TestAppObjectUtil.getObj(sess, "ごみ箱").getType()));
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testIsTypeOfFolder() throws Exception {
		assertFalse(target.isTypeOfFolder(TestAppObjectUtil.getObj(sess, "F編集中1.txt").getType()));
		assertTrue(target.isTypeOfFolder(TestAppObjectUtil.getObj(sess, "F編集中").getType()));
		assertFalse(target.isTypeOfFolder(TestAppObjectUtil.getObj(sess, "%-WFステータステスト").getType()));
		assertFalse(target.isTypeOfFolder(ObjectUtils.getObjectTypeByName(sess, "マイドキュメント")));
		assertFalse(target.isTypeOfFolder(TestAppObjectUtil.getObj(sess, "ごみ箱").getType()));
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testIsTypeOfWorkspace() throws Exception {
		assertFalse(target.isTypeOfWorkspace(TestAppObjectUtil.getObj(sess, "F編集中1.txt").getType()));
		assertFalse(target.isTypeOfWorkspace(TestAppObjectUtil.getObj(sess, "F編集中").getType()));
		assertTrue(target.isTypeOfWorkspace(TestAppObjectUtil.getObj(sess, "%-WFステータステスト").getType()));
		assertFalse(target.isTypeOfWorkspace(ObjectUtils.getObjectTypeByName(sess, "マイドキュメント")));
		assertFalse(target.isTypeOfWorkspace(TestAppObjectUtil.getObj(sess, "ごみ箱").getType()));
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testGetTypeOfWorkspace() throws Exception {
		assertEquals(ObjectUtils.getObjectTypeByName(sess, "ワークスペース").getId(),
			target.getTypeOfWorkspace().getId());
		EIMObjectType t = target.getTypeOfWorkspace();
		assertSame(t, target.getTypeOfWorkspace());
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testIsTypeOfRecycle() throws Exception {
		assertFalse(target.isTypeOfRecycle(TestAppObjectUtil.getObj(sess, "F編集中1.txt").getType()));
		assertFalse(target.isTypeOfRecycle(TestAppObjectUtil.getObj(sess, "F編集中").getType()));
		assertFalse(target.isTypeOfRecycle(TestAppObjectUtil.getObj(sess, "%-WFステータステスト").getType()));
		assertFalse(target.isTypeOfRecycle(ObjectUtils.getObjectTypeByName(sess, "マイドキュメント")));
		assertTrue(target.isTypeOfRecycle(TestAppObjectUtil.getObj(sess, "ごみ箱").getType()));
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testGetTypeOfRecycle() throws Exception {
		assertEquals(ObjectUtils.getObjectTypeByName(sess, "ごみ箱").getId(),
			target.getTypeOfRecycle().getId());
		EIMObjectType t = target.getTypeOfRecycle();
		assertSame(t, target.getTypeOfRecycle());
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testIsTypeOfMyDocument() throws Exception {
		assertFalse(target.isTypeOfMyDocument(TestAppObjectUtil.getObj(sess, "F編集中1.txt").getType()));
		assertFalse(target.isTypeOfMyDocument(TestAppObjectUtil.getObj(sess, "F編集中").getType()));
		assertFalse(target.isTypeOfMyDocument(TestAppObjectUtil.getObj(sess, "%-WFステータステスト").getType()));
		assertTrue(target.isTypeOfMyDocument(ObjectUtils.getObjectTypeByName(sess, "マイドキュメント")));
		assertFalse(target.isTypeOfMyDocument(TestAppObjectUtil.getObj(sess, "ごみ箱").getType()));
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testGetTypeOfMyDocument() throws Exception {
		assertEquals(ObjectUtils.getObjectTypeByName(sess, "マイドキュメント").getId(),
			target.getTypeOfMyDocument().getId());
		EIMObjectType t = target.getTypeOfMyDocument();
		assertSame(t, target.getTypeOfMyDocument());
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testGetTypeOfFolder() throws Exception {
		assertEquals(ObjectUtils.getObjectTypeByName(sess, "フォルダ").getId(),
			target.getTypeOfFolder().getId());
		EIMObjectType t = target.getTypeOfFolder();
		assertSame(t, target.getTypeOfFolder());
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testIsTypeOfToplevelTypes() throws Exception {
		assertFalse(target.isTypeOfToplevelTypes(TestAppObjectUtil.getObj(sess, "F編集中1.txt").getType()));
		assertFalse(target.isTypeOfToplevelTypes(target.getTypeOfFolder()));
		assertTrue(target.isTypeOfToplevelTypes(target.getTypeOfMyDocument()));
		assertTrue(target.isTypeOfToplevelTypes(target.getTypeOfRecycle()));
		assertTrue(target.isTypeOfToplevelTypes(target.getTypeOfWorkspace()));
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testIsReadOnlyAccess() throws Exception {
		isReadonlyAccess("tu1", new boolean[] { false, false, false, false });
		isReadonlyAccess("tu2", new boolean[] { false, false, false, false });
		isReadonlyAccess("tu3", new boolean[] { false, false, false, false });
		isReadonlyAccess("tu4", new boolean[] { true, true, true, true });
	}

	/**
	 * 
	 * @param userCode
	 * @param accessFlags
	 * @throws Exception
	 */
	private void isReadonlyAccess(String userCode, boolean[] accessFlags) throws Exception {
		sess.setAttribute(EIMSession.USER, UserUtils.getUserByCode(sess, userCode));
		target = new AppObjectConditionHelper(sess);
		String[] objNames = {//
		"F編集中1.txt", "F編集中", "プロジェクトX", "%-WFステータステスト" };
		for (int i = 0; i < objNames.length; i++) {
			assertTrue(userCode + ":" + objNames[i] + ":" + accessFlags[i],
				accessFlags[i] == target.isReadOnlyAccess(TestAppObjectUtil.getObj(sess,
					objNames[i])));
		}
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testGetReadOnlySecurities() throws Exception {
		getReadOnlySechrities("tu1", new String[] {});
		getReadOnlySechrities("tu2", new String[] {});
		getReadOnlySechrities("tu3", new String[] {});
		getReadOnlySechrities("tu4", new String[] { "ツリーテスト用セキュリティ" });
	}

	/**
	 * 
	 * @param userCode
	 * @param secNames
	 * @throws Exception
	 */
	private void getReadOnlySechrities(String userCode, String[] secNames) throws Exception {
		sess.setAttribute(EIMSession.USER, UserUtils.getUserByCode(sess, userCode));
		target = new AppObjectConditionHelper(sess);
		Map secs = target.getReadOnlySecurities();
		assertEquals(secNames.length, secs.size());
		for (int i = 0; i < secNames.length; i++) {
			EIMSecurity sec = SecurityUtils.getSecurityByName(sess, secNames[i]);
			assertTrue(userCode + ":" + secNames[i], secs.containsKey(new Integer(sec.getId())));
		}
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testGetRelationTypeOfDocument() throws Exception {
		assertEquals(RelationUtils.getRelationTypeByName(sess, "ドキュメント").getId(),
			target.getRelationTypeOfDocument().getId());
		assertSame(target.getRelationTypeOfDocument(), target.getRelationTypeOfDocument());
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testGetChildObjectsInAccessibleStatus() throws Exception {
		List objs = target.getChildObjectsInAccessibleStatus(
			TestAppObjectUtil.getObj(sess, "F編集中"), true);
		assertEquals(1, objs.size());
		assertEquals("F編集中_F", ((EIMObject) objs.get(0)).getName());

		getChildObjectsInAccessibleStatus("tu1", new String[][] {
				{ "F編集中1.txt", "F編集中2.txt", "F編集中_F" }// F編集中
				,
				{ "F公開中1.txt", "F公開中2.txt", "F公開中_F" }// F公開中
				,
				{ "F普通のフォルダ_編集中1.txt", "F普通のフォルダ_承認依頼中1.txt", "F普通のフォルダ_公開処理中1.txt",
						"F普通のフォルダ_公開処理中で失敗1.txt", "F普通のフォルダ_公開中1.txt", "F普通のフォルダ_1.txt",
						"F普通のフォルダ_F" } // F普通のフォルダ
			});
		getChildObjectsInAccessibleStatus("tu2", new String[][] {
				{ "F編集中1.txt", "F編集中2.txt", "F編集中_F" }// F編集中
				,
				{ "F公開中1.txt", "F公開中2.txt", "F公開中_F" }// F公開中
				,
				{ "F普通のフォルダ_編集中1.txt", "F普通のフォルダ_承認依頼中1.txt", "F普通のフォルダ_公開処理中1.txt",
						"F普通のフォルダ_公開処理中で失敗1.txt", "F普通のフォルダ_公開中1.txt", "F普通のフォルダ_1.txt",
						"F普通のフォルダ_F" } // F普通のフォルダ
			});
		getChildObjectsInAccessibleStatus("tu3", new String[][] {
				{ "F編集中1.txt", "F編集中2.txt", "F編集中_F" }// F編集中
				,
				{ "F公開中1.txt", "F公開中2.txt", "F公開中_F" }// F公開中
				,
				{ "F普通のフォルダ_編集中1.txt", "F普通のフォルダ_承認依頼中1.txt", "F普通のフォルダ_公開処理中1.txt",
						"F普通のフォルダ_公開処理中で失敗1.txt", "F普通のフォルダ_公開中1.txt", "F普通のフォルダ_1.txt",
						"F普通のフォルダ_F" } // F普通のフォルダ
			});
		getChildObjectsInAccessibleStatus("tu4", new String[][] { {}// F編集中
				, { "F公開中1.txt", "F公開中2.txt", "F公開中_F" }// F公開中
				, { "F普通のフォルダ_公開中1.txt", "F普通のフォルダ_1.txt", "F普通のフォルダ_F" } // F普通のフォルダ
			});

	}

	/**
	 * 
	 * @param userCode
	 * @param objNames
	 * @throws Exception
	 */
	void getChildObjectsInAccessibleStatus(String userCode, String[][] objNames) throws Exception {
		sess.setAttribute(EIMSession.USER, UserUtils.getUserByCode(sess, userCode));
		target = new AppObjectConditionHelper(sess);
		List objs = target.getChildObjectsInAccessibleStatus(
			TestAppObjectUtil.getObj(sess, "F編集中"), false);
		assertObjByName(objNames[0], objs);
		objs = target.getChildObjectsInAccessibleStatus(TestAppObjectUtil.getObj(sess, "F公開中"),
			false);
		assertObjByName(objNames[1], objs);
		objs = target.getChildObjectsInAccessibleStatus(TestAppObjectUtil.getObj(sess, "F普通のフォルダ"),
			false);
		assertObjByName(objNames[2], objs);
	}

	/**
	 * 
	 * 
	 */
	public void testGetObjTypeNameFolderXmlEscaped() {
		assertEquals("フォルダ", target.getObjTypeNameFolderXmlEscaped());
	}

	/** */
	public void testGetAttrNameDocumentHigherWFFolder() {
		assertEquals(EIMConfig.get("ATTR_NAME_DOCUMENT_HIGHER_WF_FOLDER"),
			target.getAttrNameDocumentHigherWFFolder());
	}

	/**
	 * 
	 * @param names
	 * @param objs
	 */
	void assertObjByName(String[] names, List objs) {
		assertEquals(names.length, objs.size());
		for (int i = 0; i < names.length; i++) {
			EIMObject found = null;
			for (Iterator j = objs.iterator(); j.hasNext();) {
				EIMObject o = (EIMObject) j.next();
				if (o.getName().equals(names[i])) {
					found = o;
					break;
				}
			}
			if (found == null)
				fail();
		}
	}
}
