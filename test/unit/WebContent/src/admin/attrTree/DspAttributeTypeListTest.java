package admin.attrTree;

import junit.framework.TestCase;

import org.apache.commons.lang.ClassUtils;
import org.apache.xpath.XPathAPI;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import admin.AdminService;
import eim.bo.EIMAttributeType;
import eim.bo.EIMResource;
import eim.bo.EIMUser;
import eim.bo.EIMValueType;
import eim.net.EIMSession;
import eim.util.AttributeUtils;
import eim.util.ObjectAttributeUtils;
import eim.util.ObjectUtils;
import eim.util.UserUtils;
import eimtest.app.util.TestAppDBUtil;
import eimtest.app.util.XMLUtil;
import eimtest.app.util.net.EIMServerResponseError;
import eimtest.util.Misc;
import eimtest.util.TestSessionUtil;

/**
 * 属性選択クラスのテスト
 */
public class DspAttributeTypeListTest extends TestCase {
	/** */
	AdminService as;

	/** */
	XMLUtil xu;

	/** */
	EIMSession sess;

	public void setUp() throws Exception {
		as = new AdminService(false);
		xu = new XMLUtil();
		sess = TestSessionUtil.createEIMSession();
		TestAppDBUtil.loadPrimitiveData();
	}

	protected void tearDown() throws Exception {
		sess.close();
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testDocumentAdmin() throws Exception {
		// ESSENTIAL_ATTRIBUTE_DEFNAMEから「作成者名」削除をテスト
		// @Test: jsp call without creator name attribute
		as.attrTree_dspAttributeTypeList("作成者名");
		// no error

		EIMAttributeType attrTypeCreatorName = createAttrTypeCreatorName();

		Document xdoc = xu.toDOM(as.attribute_dspAttributeTypeList());
		assertEquals(
			"作成者名",
			XPathAPI.selectSingleNode(xdoc,
				"/attTypes/attType[@attTypeId='" + attrTypeCreatorName.getId() + "']/@attTypeName").getNodeValue());
	}

	/**
	 * 正常系 属性が正常に選択されること。
	 * 
	 * @throws Exception
	 */
	public void testMe() throws Exception {
		// TestCase:検索条件なしで属性リスト
		// 検証用雛形XMLをテキストファイルから読み込み
		String assertText = Misc.loadFile(this.getClass().getResource(
			ClassUtils.getShortClassName(this.getClass()) + ".list_1.txt").getPath());
		assertEquals(assertText, xu.toStr(xu.toDOM(as.attrTree_dspAttributeTypeList(""))));

		// TestCase:カスタム属性を追加して、属性リストに増えるか確認
		EIMAttributeType attAdd = AttributeUtils.createAttributeType(sess, "ja:ATTR_HOGE",
			new EIMValueType(sess, EIMValueType.DATE));
		AttributeUtils.addOtherAttributeTypeName(sess, attAdd.getId(), "JA", "ja:ATTR_HOGE");
		AttributeUtils.addOtherAttributeTypeName(sess, attAdd.getId(), "EN", "en:ATTR_HOGE");

		ObjectAttributeUtils.applyAttributeType(sess, ObjectUtils.getObjectTypeByName(sess,
			"ドキュメント"), attAdd);
		sess.commit();

		// 検証用XML生成
		Document assertXdoc = xu.toDOM(assertText);
		Element elm = assertXdoc.createElement("attType");
		elm.setAttribute("attTypeId", String.valueOf(attAdd.getId()));
		elm.setAttribute("attTypeName", attAdd.getName());
		elm.setAttribute("inputRuleValue", "なし");
		elm.setAttribute("isMultipleValue", "なし");
		elm.setAttribute("valTypeName", "日付型");
		elm.appendChild(assertXdoc.createTextNode("\n"));
		assertXdoc.getFirstChild().insertBefore(elm, assertXdoc.getFirstChild().getFirstChild());
		assertXdoc.getFirstChild().insertBefore(assertXdoc.createTextNode("\n"),
			assertXdoc.getFirstChild().getFirstChild());
		assertEquals(xu.toStr(assertXdoc), xu.toStr(xu.toDOM(as.attrTree_dspAttributeTypeList(""))));

		// TestCase:検索キー"有効*"による絞込み。有効期限だけがリストされる。
		// 検証用XMLを"有効期限"だけにする
		Node att = XPathAPI.selectSingleNode(assertXdoc, "/attTypes/attType[@attTypeName=\"有効期限\"]");
		assertXdoc.removeChild(assertXdoc.getFirstChild());
		Element attTypes = assertXdoc.createElement("attTypes");
		attTypes.appendChild(assertXdoc.createTextNode("\n"));
		attTypes.appendChild(att);
		attTypes.appendChild(assertXdoc.createTextNode("\n"));
		assertXdoc.appendChild(attTypes);

		assertEquals(xu.toStr(assertXdoc),
			xu.toStr(xu.toDOM(as.attrTree_dspAttributeTypeList("有効*"))));
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
			as.attrTree_dspAttributeTypeList("1");
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
			as.attrTree_dspAttributeTypeList("1");
			fail();
		} catch (EIMServerResponseError e) {
			assertEquals(EIMResource.getMessage("JA", "EIM.ERROR.NOTADMINISTRATOR"), e.getMessage());
		}
	}

	/**
	 * 
	 * @return o
	 * @throws Exception
	 */
	EIMAttributeType createAttrTypeCreatorName() throws Exception {
		EIMAttributeType attrTypeCreatorName = AttributeUtils.createAttributeType(sess, "作成者名",
			EIMValueType.getTypeById(sess, EIMValueType.STRING));
		AttributeUtils.addOtherAttributeTypeName(sess, attrTypeCreatorName.getId(), "JA", "作成者名");
		AttributeUtils.addOtherAttributeTypeName(sess, attrTypeCreatorName.getId(), "EN",
			"Creator name");
		sess.commit();
		return attrTypeCreatorName;
	}
}
