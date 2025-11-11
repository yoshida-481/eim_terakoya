package app.document.folder;

import java.util.List;
import java.util.TreeMap;

import junit.extensions.TestSetup;
import junit.framework.Test;
import junit.framework.TestSuite;

import common.util.AppConstant;
import common.util.AppObjectConditionHelper;
import common.util.AppObjectUtil;

import eim.bo.EIMAccess;
import eim.bo.EIMObject;
import eim.bo.EIMRelation;
import eim.bo.EIMResource;
import eim.net.EIMSession;
import eim.util.AccessUtils;
import eim.util.ObjectUtils;
import eim.util.RelationUtils;
import eim.util.SecurityUtils;
import eimtest.app.util.CreateFolderTreeData;
import eimtest.app.util.JSPTestCase;
import eimtest.app.util.TestAppDBUtil;
import eimtest.app.util.TestAppMisc;
import eimtest.app.util.TestAppObjectUtil;
import eimtest.app.util.TestAppOpeHistUtil;
import eimtest.app.util.net.EIMServerResponseError;
import eimtest.util.TestSessionUtil;

/** */
public class ActDuplicateFolderTreeTest extends JSPTestCase {
	/**
	 * 
	 * @return o
	 * @throws Exception
	 */
	public static Test suite() throws Exception {
		TestSuite suite = new TestSuite(ActDuplicateFolderTreeTest.class);
		TestSetup w = new TestSetup(suite) {

			/** */
			public void setUp() throws Exception {
				if (true) {
					TestAppDBUtil.loadPrimitiveData();
					EIMSession sess = TestSessionUtil.createEIMSession();
					EIMObject obj = TestAppObjectUtil.getObj(sess, "F1");
					ObjectUtils.rename(sess, obj, "WS1F1");
					new CreateFolderTreeData(sess, null).process();
					new CreateFolderTreeData(sess, TestAppMisc.getAppFilePath(
						ActDuplicateFolderTreeTest.class, "_01.xls")).process();
					obj = TestAppObjectUtil.getObj(sess, "%-WFステータステスト");
					ObjectUtils.rename(sess, obj, "A-WFステータステスト");
					sess.commit();
				}
			}
		};
		return w;
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testDupF17() throws Exception {
		// TestCase:ワークフロー付きフォルダの複製
		EIMObject f17 = TestAppObjectUtil.findObj("F17", objs);
		EIMObject f171 = TestAppObjectUtil.findObj("F171", objs);
		EIMObject f1711 = TestAppObjectUtil.findObj("F1711", objs);
		EIMObject f172 = TestAppObjectUtil.findObj("F172", objs);
		EIMObject f1721 = TestAppObjectUtil.findObj("F1721", objs);
		EIMObject f173 = TestAppObjectUtil.findObj("F173", objs);
		EIMObject f1731 = TestAppObjectUtil.findObj("F1731", objs);
		EIMObject f174 = TestAppObjectUtil.findObj("F174", objs);
		EIMObject f1741 = TestAppObjectUtil.getObj(sess, "F1741");
		EIMObject f17411 = TestAppObjectUtil.getObj(sess, "F17411");
		// 複製実行
		ds.folder_actDuplicateFolderTree(f17.getId());
		EIMObject dupf17 = TestAppObjectUtil.getObj(sess, "コピー - F17");
		List dupcs = RelationUtils.getChildRelationList(sess, dupf17);
		EIMObject dupf171 = ((EIMRelation) dupcs.get(0)).getChild();
		EIMObject dupf172 = ((EIMRelation) dupcs.get(1)).getChild();
		EIMObject dupf173 = ((EIMRelation) dupcs.get(2)).getChild();
		EIMObject dupf174 = ((EIMRelation) dupcs.get(3)).getChild();
		EIMObject dupf1711 = ((EIMRelation) RelationUtils.getChildRelationList(sess, dupf171).get(0)).getChild();
		EIMObject dupf1721 = ((EIMRelation) RelationUtils.getChildRelationList(sess, dupf172).get(0)).getChild();
		EIMObject dupf1731 = ((EIMRelation) RelationUtils.getChildRelationList(sess, dupf173).get(0)).getChild();
		EIMObject dupf1741 = ((EIMRelation) RelationUtils.getChildRelationList(sess, dupf174).get(0)).getChild();
		EIMObject dupf17411 = ((EIMRelation) RelationUtils.getChildRelationList(sess, dupf1741).get(
			0)).getChild();

		// F17と同じ属性値。
		TreeMap m = TestAppObjectUtil.getObjAttrMap(f17);
		assertEquals(TestAppMisc.mapToString(m),
			TestAppMisc.mapToString(TestAppObjectUtil.getObjAttrMap(dupf17)));
		assertNull(f17.getStatus());

		// 複製F171は編集中になる
		m = TestAppObjectUtil.getObjAttrMap(f171);
		replacePathValue(m, f17, dupf17);
		assertEquals(AppConstant.STATUS_TYPE_KIND_ID_EDITTING, dupf171.getStatus().getType().getKind());
		assertEquals(TestAppMisc.mapToString(m),
			TestAppMisc.mapToString(TestAppObjectUtil.getObjAttrMap(dupf171)));

		// 複製F1711の上位WFフォルダは複製F171になり、ステータスは複製F171と同じものを持つ
		m = TestAppObjectUtil.getObjAttrMap(f1711);
		replacePathValue(m, f17, dupf17);
		m.put("上位WFフォルダ", "[" + dupf171.getId() + "]");
		assertEquals(dupf171.getStatus().getId(), dupf1711.getStatus().getId());
		assertEquals(TestAppMisc.mapToString(m),
			TestAppMisc.mapToString(TestAppObjectUtil.getObjAttrMap(dupf1711)));

		// 複製F172は編集中になる
		m = TestAppObjectUtil.getObjAttrMap(f172);
		replacePathValue(m, f17, dupf17);
		assertEquals(AppConstant.STATUS_TYPE_KIND_ID_EDITTING, dupf172.getStatus().getType().getKind());
		assertEquals(TestAppMisc.mapToString(m),
			TestAppMisc.mapToString(TestAppObjectUtil.getObjAttrMap(dupf172)));

		// 複製F1721の上位WFフォルダは複製F172になり、ステータスは複製F172と同じものを持つ
		m = TestAppObjectUtil.getObjAttrMap(f1721);
		replacePathValue(m, f17, dupf17);
		m.put("上位WFフォルダ", "[" + dupf172.getId() + "]");
		assertEquals(dupf172.getStatus().getId(), dupf1721.getStatus().getId());
		assertEquals(TestAppMisc.mapToString(m),
			TestAppMisc.mapToString(TestAppObjectUtil.getObjAttrMap(dupf1721)));

		// 複製F173は編集中になる
		m = TestAppObjectUtil.getObjAttrMap(f173);
		replacePathValue(m, f17, dupf17);
		assertEquals(AppConstant.STATUS_TYPE_KIND_ID_EDITTING, dupf173.getStatus().getType().getKind());
		assertEquals(TestAppMisc.mapToString(m),
			TestAppMisc.mapToString(TestAppObjectUtil.getObjAttrMap(dupf173)));

		// 複製F1731の上位WFフォルダは複製F173になり、ステータスは複製F173と同じものを持つ
		m = TestAppObjectUtil.getObjAttrMap(f1731);
		replacePathValue(m, f17, dupf17);
		m.put("上位WFフォルダ", "[" + dupf173.getId() + "]");
		assertEquals(dupf173.getStatus().getId(), dupf1731.getStatus().getId());
		assertEquals(TestAppMisc.mapToString(m),
			TestAppMisc.mapToString(TestAppObjectUtil.getObjAttrMap(dupf1731)));

		// 複製F174は編集中になる
		m = TestAppObjectUtil.getObjAttrMap(f174);
		replacePathValue(m, f17, dupf17);
		// F174が持つ有効期限はコピーされない
		assertTrue(m.containsKey("有効期限"));
		m.remove("有効期限");
		assertEquals(AppConstant.STATUS_TYPE_KIND_ID_EDITTING, dupf174.getStatus().getType().getKind());
		assertEquals(TestAppMisc.mapToString(m),
			TestAppMisc.mapToString(TestAppObjectUtil.getObjAttrMap(dupf174)));

		// 複製F1741の上位WFフォルダは複製F174になり、ステータスは複製F174と同じものを持つ
		m = TestAppObjectUtil.getObjAttrMap(f1741);
		replacePathValue(m, f17, dupf17);
		m.put("上位WFフォルダ", "[" + dupf174.getId() + "]");
		assertEquals(dupf174.getStatus().getId(), dupf1741.getStatus().getId());
		assertEquals(TestAppMisc.mapToString(m),
			TestAppMisc.mapToString(TestAppObjectUtil.getObjAttrMap(dupf1741)));

		// 複製F17411の上位WFフォルダは複製F174になり、ステータスは複製F174と同じものを持つ
		m = TestAppObjectUtil.getObjAttrMap(f17411);
		replacePathValue(m, f17, dupf17);
		m.put("上位WFフォルダ", "[" + dupf174.getId() + "]");
		assertEquals(dupf174.getStatus().getId(), dupf17411.getStatus().getId());
		assertEquals(TestAppMisc.mapToString(m),
			TestAppMisc.mapToString(TestAppObjectUtil.getObjAttrMap(dupf17411)));
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testDupF181() throws Exception {
		// TestCase:ワークフロー付きフォルダの下のフォルダの複製
		EIMObject f18 = TestAppObjectUtil.findObj("F18", objs);
		EIMObject f181 = TestAppObjectUtil.findObj("F181", objs);
		EIMObject f1811 = TestAppObjectUtil.findObj("F1811", objs);
		// 複製実行
		ds.folder_actDuplicateFolderTree(f181.getId());
		EIMObject dupf181 = TestAppObjectUtil.getObj(sess, "コピー - F181");
		EIMObject dupf1811 = ((EIMRelation) RelationUtils.getChildRelationList(sess, dupf181).get(0)).getChild();

		// 複製F181はF181と同じ属性値。上位WFフォルダ属性値も同じ。ステータスも同じ
		TreeMap m = TestAppObjectUtil.getObjAttrMap(f181);
		assertEquals(TestAppMisc.mapToString(m),
			TestAppMisc.mapToString(TestAppObjectUtil.getObjAttrMap(dupf181)));
		assertEquals(f18.getId(), dupf181.getAttribute("上位WFフォルダ").getInt());// 念のため
		assertEquals(f181.getStatus().getId(), dupf181.getStatus().getId());
		assertEquals(f18.getStatus().getId(), dupf181.getStatus().getId());// 念のため

		// 複製F1811はF1811と同じ属性値。上位WFフォルダ属性値も同じ。ステータスも同じ
		m = TestAppObjectUtil.getObjAttrMap(f1811);
		replacePathValue(m, f181, dupf181);
		assertEquals(TestAppMisc.mapToString(m),
			TestAppMisc.mapToString(TestAppObjectUtil.getObjAttrMap(dupf1811)));
		assertEquals(f18.getId(), dupf1811.getAttribute("上位WFフォルダ").getInt());// 念のため
		assertEquals(f1811.getStatus().getId(), dupf1811.getStatus().getId());
		assertEquals(f18.getStatus().getId(), dupf1811.getStatus().getId());// 念のため
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testDupF16() throws Exception {
		// TestCase:ワークフロー付きフォルダの複製
		EIMObject f16 = TestAppObjectUtil.findObj("F16", objs);
		EIMObject f161 = TestAppObjectUtil.findObj("F161", objs);
		// 複製実行
		ds.folder_actDuplicateFolderTree(f16.getId());
		EIMObject dupf16 = TestAppObjectUtil.getObj(sess, "コピー - F16");
		List dupcs = RelationUtils.getChildRelationList(sess, dupf16);
		assertEquals(1, dupcs.size());
		EIMObject dupf161 = ((EIMRelation) dupcs.get(0)).getChild();

		// F16と同じ属性値。
		TreeMap m = TestAppObjectUtil.getObjAttrMap(f16);
		assertEquals(TestAppMisc.mapToString(m),
			TestAppMisc.mapToString(TestAppObjectUtil.getObjAttrMap(dupf16)));
		// ステータスは違う。編集中になる
		assertTrue(f16.getStatus().getId() != dupf16.getStatus().getId());
		assertEquals(AppConstant.STATUS_TYPE_KIND_ID_EDITTING, dupf16.getStatus().getType().getKind());

		// F161の上位ワークフローフォルダ属性は異なる
		m = TestAppObjectUtil.getObjAttrMap(f161);
		replacePathValue(m, f16, dupf16);
		m.put("上位WFフォルダ", "[" + dupf16.getId() + "]");
		assertEquals(dupf16.getStatus().getId(), dupf161.getStatus().getId());
		assertEquals(TestAppMisc.mapToString(m),
			TestAppMisc.mapToString(TestAppObjectUtil.getObjAttrMap(dupf161)));
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testDupF15() throws Exception {
		// TestCase:下位に2つありの複製。名称割り当ては自身には無く、下位はある
		EIMObject f15 = TestAppObjectUtil.findObj("F15", objs);
		EIMObject f151 = TestAppObjectUtil.findObj("F151", objs);
		// f151に名称割り当て下位引継ぎの断絶ポイントを無理やり作る
		AppObjectUtil.setAttr(sess, f151, "t文字1", (String) null);
		sess.getDBConnection().createStatement().execute(
			"update EIMOBJ set type=" + ObjectUtils.getObjectTypeByName(sess, "フォルダ").getId()
					+ " where id=" + f151.getId());
		f151 = ObjectUtils.getObjectById(sess, f151.getId());
		EIMObject f1511 = TestAppObjectUtil.findObj("F1511", objs);
		AppObjectUtil.setAttr(sess, f1511, "t文字1", "hoge");
		f1511 = ObjectUtils.getObjectById(sess, f1511.getId());
		EIMObject f152 = TestAppObjectUtil.findObj("F152", objs);
		EIMObject f1521 = TestAppObjectUtil.findObj("F1521", objs);
		sess.commit();

		// 複製実行
		ds.folder_actDuplicateFolderTree(f15.getId());
		EIMObject dupf15 = TestAppObjectUtil.getObj(sess, "コピー - F15");
		List dupcs = RelationUtils.getChildRelationList(sess, dupf15);
		assertEquals(2, dupcs.size());
		EIMObject dupf151 = ((EIMRelation) dupcs.get(0)).getChild();
		EIMObject dupf152 = ((EIMRelation) dupcs.get(1)).getChild();
		dupcs = RelationUtils.getChildRelationList(sess, dupf151);
		assertEquals(1, dupcs.size());
		EIMObject dupf1511 = ((EIMRelation) dupcs.get(0)).getChild();
		dupcs = RelationUtils.getChildRelationList(sess, dupf152);
		assertEquals(1, dupcs.size());
		EIMObject dupf1521 = ((EIMRelation) dupcs.get(0)).getChild();

		// F15と同じ属性値。ただし名称割り当て属性は新フォルダ名
		TreeMap m = TestAppObjectUtil.getObjAttrMap(f15);
		m.put("t文字1", "[" + dupf15.getName() + "]");
		assertEquals(TestAppMisc.mapToString(m),
			TestAppMisc.mapToString(TestAppObjectUtil.getObjAttrMap(dupf15)));

		// 複製したF151には「t文字」属性は無い
		assertNull(dupf151.getAttribute("t文字1"));
		m = TestAppObjectUtil.getObjAttrMap(f151);
		replacePathValue(m, f15, dupf15);
		assertEquals("フォルダ", dupf151.getType().getName());
		assertEquals(TestAppMisc.mapToString(m),
			TestAppMisc.mapToString(TestAppObjectUtil.getObjAttrMap(dupf151)));

		// 下位の複製F1511のt文字=hogeは上書きされていない
		assertEquals("hoge", dupf1511.getAttribute("t文字1").getString());
		m = TestAppObjectUtil.getObjAttrMap(f1511);
		replacePathValue(m, f15, dupf15);
		assertEquals(TestAppMisc.mapToString(m),
			TestAppMisc.mapToString(TestAppObjectUtil.getObjAttrMap(dupf1511)));

		// 断絶の無いF152は新フォルダ名称が「t文字1」属性に入っている
		m = TestAppObjectUtil.getObjAttrMap(f152);
		m.put("t文字1", "[" + dupf15.getName() + "]");
		replacePathValue(m, f15, dupf15);
		assertEquals(TestAppMisc.mapToString(m),
			TestAppMisc.mapToString(TestAppObjectUtil.getObjAttrMap(dupf152)));

		// 断絶の無いF1521にも新フォルダ名称が「t文字1」属性に入っている
		m = TestAppObjectUtil.getObjAttrMap(f1521);
		m.put("t文字1", "[" + dupf15.getName() + "]");
		replacePathValue(m, f15, dupf15);
		assertEquals(TestAppMisc.mapToString(m),
			TestAppMisc.mapToString(TestAppObjectUtil.getObjAttrMap(dupf1521)));
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testDupF14() throws Exception {
		// TestCase:下位に2つありの複製。名称割り当ては自身には無く、下位はある
		EIMObject f14 = TestAppObjectUtil.findObj("F14", objs);
		EIMObject f141 = TestAppObjectUtil.findObj("F141", objs);
		EIMObject f1411 = TestAppObjectUtil.findObj("F1411", objs);
		// 複製実行
		ds.folder_actDuplicateFolderTree(f14.getId());
		EIMObject dupf14 = TestAppObjectUtil.getObj(sess, "コピー - F14");

		TreeMap m = TestAppObjectUtil.getObjAttrMap(f14);
		// F14と同じ属性値。ただし名称割り当て属性は新フォルダ名
		assertEquals(TestAppMisc.mapToString(m),
			TestAppMisc.mapToString(TestAppObjectUtil.getObjAttrMap(dupf14)));

		// 下位
		// F141
		List dupocs = RelationUtils.getChildRelationList(sess, dupf14);
		assertEquals(1, dupocs.size());
		EIMObject dupf141 = ((EIMRelation) dupocs.get(0)).getChild();
		m = TestAppObjectUtil.getObjAttrMap(f141);
		replacePathValue(m, f14, dupf14);
		assertEquals(TestAppMisc.mapToString(m),
			TestAppMisc.mapToString(TestAppObjectUtil.getObjAttrMap(dupf141)));

		// F1411
		dupocs = RelationUtils.getChildRelationList(sess, dupf141);
		assertEquals(1, dupocs.size());
		EIMObject dupf1411 = ((EIMRelation) dupocs.get(0)).getChild();
		m = TestAppObjectUtil.getObjAttrMap(f1411);
		replacePathValue(m, f14, dupf14);
		assertEquals(TestAppMisc.mapToString(m),
			TestAppMisc.mapToString(TestAppObjectUtil.getObjAttrMap(dupf1411)));
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testDupF13() throws Exception {
		// TestCase:下位に2つありの複製。名称割り当て属性なし
		EIMObject f13 = TestAppObjectUtil.findObj("F13", objs);
		EIMObject f131 = TestAppObjectUtil.findObj("F131", objs);
		EIMObject f132 = TestAppObjectUtil.findObj("F132", objs);
		// 複製実行
		ds.folder_actDuplicateFolderTree(f13.getId());
		EIMObject dupf13 = TestAppObjectUtil.getObj(sess, "コピー - F13");

		TreeMap m = TestAppObjectUtil.getObjAttrMap(f13);
		m.put("t文字1", "[" + dupf13.getName() + "]");
		// F13と同じ属性値。ただし名称割り当て属性は新フォルダ名
		assertEquals(TestAppMisc.mapToString(m),
			TestAppMisc.mapToString(TestAppObjectUtil.getObjAttrMap(dupf13)));

		// 下位
		// F131
		List dupocs = RelationUtils.getChildRelationList(sess, dupf13);
		EIMObject dupf131 = ((EIMRelation) dupocs.get(0)).getChild();
		EIMObject dupf132 = ((EIMRelation) dupocs.get(1)).getChild();
		m = TestAppObjectUtil.getObjAttrMap(f131);
		m.put("t文字1", "[" + dupf13.getName() + "]");
		replacePathValue(m, f13, dupf13);
		assertEquals(TestAppMisc.mapToString(m),
			TestAppMisc.mapToString(TestAppObjectUtil.getObjAttrMap(dupf131)));
		// F132
		m = TestAppObjectUtil.getObjAttrMap(f132);
		m.put("t文字1", "[" + dupf13.getName() + "]");
		replacePathValue(m, f13, dupf13);
		assertEquals(TestAppMisc.mapToString(m),
			TestAppMisc.mapToString(TestAppObjectUtil.getObjAttrMap(dupf132)));
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testDupF11() throws Exception {
		// TestCase:下位なしの複製。名称割り当て属性有り
		EIMObject f11 = TestAppObjectUtil.findObj("F11", objs);
		// 複製実行
		ds.folder_actDuplicateFolderTree(f11.getId());
		EIMObject dupf11 = TestAppObjectUtil.getObj(sess, "コピー - F11");
		assertNotNull(dupf11);
		assertEquals(f11.getType().getId(), dupf11.getType().getId());
		assertEquals(f11.getSecurityId(), dupf11.getSecurityId());
		TreeMap m = TestAppObjectUtil.getObjAttrMap(f11);
		m.put("t文字1", "[" + dupf11.getName() + "]");
		assertEquals(TestAppMisc.mapToString(m),
			TestAppMisc.mapToString(TestAppObjectUtil.getObjAttrMap(dupf11)));
		assertEquals(0, RelationUtils.getChildRelationList(sess, dupf11).size());// ドキュメントなし
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testDupF12() throws Exception {
		// TestCase:下位に2つありの複製。名称割り当て属性なし
		EIMObject f12 = TestAppObjectUtil.findObj("F12", objs);
		EIMObject f121 = TestAppObjectUtil.findObj("F121", objs);
		EIMObject f122 = TestAppObjectUtil.findObj("F122", objs);
		// f122のセキュリティを変更
		SecurityUtils.setSecurity(sess, f122, SecurityUtils.getSecurityByName(sess, "sec"));
		sess.commit();

		// 複製実行
		ds.folder_actDuplicateFolderTree(f12.getId());
		EIMObject dupf12 = TestAppObjectUtil.getObj(sess, "コピー - F12");
		// F12と同じタイプ、セキュリティ
		assertEquals(f12.getType().getId(), dupf12.getType().getId());
		assertEquals(f12.getSecurityId(), dupf12.getSecurityId());
		// F12と同じ属性値
		assertEquals(TestAppMisc.mapToString(TestAppObjectUtil.getObjAttrMap(f12)),
			TestAppMisc.mapToString(TestAppObjectUtil.getObjAttrMap(dupf12)));
		// 下位
		List dupocs = RelationUtils.getChildRelationList(sess, dupf12);
		assertEquals(2, dupocs.size());
		EIMObject dupf121 = ((EIMRelation) dupocs.get(0)).getChild();
		EIMObject dupf122 = ((EIMRelation) dupocs.get(1)).getChild();
		// F121と同じタイプ、セキュリティ、属性値
		assertEquals(f121.getType().getId(), dupf121.getType().getId());
		assertEquals(f121.getSecurityId(), dupf121.getSecurityId());
		TreeMap m = TestAppObjectUtil.getObjAttrMap(f121);
		replacePathValue(m, f12, dupf12);
		assertEquals(TestAppMisc.mapToString(m),
			TestAppMisc.mapToString(TestAppObjectUtil.getObjAttrMap(dupf121)));
		// F122と同じタイプ、セキュリティ"sec"、属性値
		assertEquals(f122.getType().getId(), dupf122.getType().getId());
		assertEquals("sec", dupf122.getSecurity().getDefName());
		m = TestAppObjectUtil.getObjAttrMap(f122);
		replacePathValue(m, f12, dupf12);
		assertEquals(TestAppMisc.mapToString(m),
			TestAppMisc.mapToString(TestAppObjectUtil.getObjAttrMap(dupf122)));
	}

	/**
	 * 
	 * @param m
	 * @param o
	 * @param dupo
	 */
	void replacePathValue(TreeMap m, EIMObject o, EIMObject dupo) {
		m.put("パス", ((String) m.get("パス")).replaceAll("\\/" + o.getName() + "\\/", "\\/"
				+ dupo.getName() + "\\/"));
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testSimple() throws Exception {
		ds.switchUser("tu1");// 書き込み権ユーザー

		EIMObject o = TestAppObjectUtil.findObj("F編集中Y", objs);

		// TestCase:<OK></OK>を返却
		assertEquals("OK",
			xu.toDOM(ds.folder_actDuplicateFolderTree(o.getId())).getFirstChild().getNodeName());

		// TestCase:複製結果 「コピーF編集中Y」が出来ているか
		EIMObject dupo = TestAppObjectUtil.getObj(sess, "コピー - F編集中Y");
		assertNotNull(dupo);

		// アクセスログが出来ているか
		List als = AccessUtils.getAccessList(sess, dupo);
		assertEquals(1, als.size());
		EIMAccess al = (EIMAccess) als.get(0);
		assertEquals("フォルダツリー複製", EIMResource.getMessage("JA", al.getAction()));
		assertEquals("Copy folder tree", EIMResource.getMessage("EN", al.getAction()));
		assertEquals("tu1", al.getUser().getCode());

		// 操作履歴が出来ているか
		TestAppOpeHistUtil.assetOpeHist(sess, "tu1"//
			, "ドキュメント管理"//
			, "フォルダツリー複製"//
			, "複製対象"//
			, "オブジェクト"//
			, dupo.getId()//
			, dupo.getName()//
			, null//
			, null//
			, 0//
			, null//
			, AppObjectUtil.getStrAttr(sess, o, "パス")//
		);

		// TestCase:英語で実行すると、「copyF編集中Y」となる
		ds.switchUser("tu1", null, "EN", null);
		assertEquals("OK",
			xu.toDOM(ds.folder_actDuplicateFolderTree(o.getId())).getFirstChild().getNodeName());
		dupo = TestAppObjectUtil.getObj(sess, "copy - F編集中Y");
		assertNotNull(dupo);

		// TestCase:フォルダ構成管理権限の範囲
		ds.switchUser("tu1");// 書き込み権ユーザー
		EIMObject po = TestAppObjectUtil.getObj(sess, "F編集中");
		AppObjectUtil.setAttr(sess, po, "下位フォルダ管理セキュリティ", SecurityUtils.getSecurityByName(sess,
			"ツリーテスト用セキュリティ").getId());
		sess.commit();

		ds.folder_actDuplicateFolderTree(o.getId());
		dupo = TestAppObjectUtil.getObj(sess, "コピー - コピー - F編集中Y");
		assertNotNull(dupo);
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testAbnormal2() throws Exception {
		// TestCase:ステータス変更権で複製はNG
		ds.switchUser("tu2");
		try {
			ds.folder_actDuplicateFolderTree(TestAppObjectUtil.findObj("F承認依頼中_F", objs).getId());
			fail();
		} catch (EIMServerResponseError e) {
			assertEquals(EIMResource.getMessage("JA", "EIM.ERROR.LOGIC.NOCOPYFOLDERTREEROLE"),
				e.getMessage());
		}

		// TestCase:常時読み取り権で複製はNG
		ds.switchUser("tu3");
		try {
			ds.folder_actDuplicateFolderTree(TestAppObjectUtil.findObj("F承認依頼中_F", objs).getId());
			fail();
		} catch (EIMServerResponseError e) {
			assertEquals(EIMResource.getMessage("JA", "EIM.ERROR.LOGIC.NOCOPYFOLDERTREEROLE"),
				e.getMessage());
		}

		// TestCase:公開読み取り権で複製はNG
		ds.switchUser("tu4");
		try {
			ds.folder_actDuplicateFolderTree(TestAppObjectUtil.findObj("F承認依頼中_F", objs).getId());
			fail();
		} catch (EIMServerResponseError e) {
			assertEquals(EIMResource.getMessage("JA", "EIM.ERROR.LOGIC.NOCOPYFOLDERTREEROLE"),
				e.getMessage());
		}

		// TestCase:フォルダ構成管理権限なしで複製はNG
		ds.switchUser("tu1");
		try {
			EIMObject o = TestAppObjectUtil.findObj("F編集中_F", objs);
			EIMObject po = TestAppObjectUtil.findObj("F編集中", objs);
			AppObjectUtil.setAttr(sess, po, "下位フォルダ管理セキュリティ", SecurityUtils.getSecurityByName(sess,
				"sec").getId());
			sess.commit();

			ds.folder_actDuplicateFolderTree(o.getId());
			fail();
		} catch (EIMServerResponseError e) {
			assertEquals(EIMResource.getMessage("JA", "EIM.ERROR.LOGIC.NOCOPYFOLDERTREEROLE"),
				e.getMessage());
		}

		// TestCase:承認依頼中フォルダの下で複製はNG
		try {
			ds.folder_actDuplicateFolderTree(TestAppObjectUtil.findObj("F承認依頼中_F", objs).getId());
			fail();
		} catch (EIMServerResponseError e) {
			assertEquals(EIMResource.getMessage("JA", "EIM.ERROR.LOGIC.NOCOPYFOLDERTREEROLE"),
				e.getMessage());
		}

		// TestCase:公開処理中フォルダの下で複製はNG
		try {
			ds.folder_actDuplicateFolderTree(TestAppObjectUtil.findObj("F公開処理中_F", objs).getId());
			fail();
		} catch (EIMServerResponseError e) {
			assertEquals(EIMResource.getMessage("JA", "EIM.ERROR.LOGIC.NOCOPYFOLDERTREEROLE"),
				e.getMessage());
		}

		// TestCase:公開処理中フォルダの下で複製はNG
		try {
			ds.folder_actDuplicateFolderTree(TestAppObjectUtil.findObj("F公開処理中_F", objs).getId());
			fail();
		} catch (EIMServerResponseError e) {
			assertEquals(EIMResource.getMessage("JA", "EIM.ERROR.LOGIC.NOCOPYFOLDERTREEROLE"),
				e.getMessage());
		}

		// TestCase:公開中フォルダの下で複製はNG
		try {
			ds.folder_actDuplicateFolderTree(TestAppObjectUtil.findObj("F公開中_F", objs).getId());
			fail();
		} catch (EIMServerResponseError e) {
			assertEquals(EIMResource.getMessage("JA", "EIM.ERROR.LOGIC.NOCOPYFOLDERTREEROLE"),
				e.getMessage());
		}
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testAbnormal() throws Exception {
		// TestCase:指定フォルダが存在しない
		try {
			ds.folder_actDuplicateFolderTree(-1);
			fail();
		} catch (EIMServerResponseError e) {
			assertEquals(EIMResource.getMessage("JA", "EIM.ERROR.LOGIC.NOFOLDER"), e.getMessage());
		}

		// TestCase:親が無い
		AppObjectConditionHelper h = new AppObjectConditionHelper(sess);
		EIMObject o = TestAppObjectUtil.findObj("F編集中Y", objs);
		EIMObject po = TestAppObjectUtil.findObj("プロジェクトY", objs);
		RelationUtils.deleteRelation(sess, RelationUtils.getRelationByParentAndChild(sess,
			h.getRelationTypeOfDocument(), po, o));
		sess.commit();
		try {
			ds.folder_actDuplicateFolderTree(o.getId());
			fail();
		} catch (EIMServerResponseError e) {
			assertEquals(EIMResource.getMessage("JA", "EIM.ERROR.LOGIC.NOTEXIST.PARENT.FOLDAER"),
				e.getMessage());
		}
		RelationUtils.createRelation(sess, h.getRelationTypeOfDocument(), po, o);
		sess.commit();

		// TestCase:上位ワークスペースが無い
		EIMObject ws = h.getUpperObject(po);
		RelationUtils.deleteRelation(sess, RelationUtils.getRelationByParentAndChild(sess,
			h.getRelationTypeOfDocument(), ws, po));
		sess.commit();
		try {
			ds.folder_actDuplicateFolderTree(o.getId());
			fail();
		} catch (EIMServerResponseError e) {
			assertEquals(EIMResource.getMessage("JA", "EIM.ERROR.LOGIC.NOTEXIST.PARENT.WORKSPACE"),
				e.getMessage());
		}
		RelationUtils.createRelation(sess, h.getRelationTypeOfDocument(), ws, po);
		sess.commit();
	}

}