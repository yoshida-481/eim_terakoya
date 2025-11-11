package app.document.attrTree;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang.ClassUtils;

import common.bo.AttributeTree;
import common.bo.AttributeTreeItem;
import common.util.AttributeTreeUtil;

import eim.net.EIMSession;
import eim.util.AttributeUtils;
import eimtest.app.util.CreateFolderTreeData;
import eimtest.app.util.TestAppMisc;

/** */
public class TestAttrTreeUtil {
	/**
	 * 
	 * @param sess
	 * @param defName
	 * @param enName
	 * @param classifyTarget
	 * @param items
	 * @return o
	 * @throws Exception
	 */
	static AttributeTree createAttrTree(EIMSession sess, String defName, String enName,
			int classifyTarget, Object[][] items) throws Exception {
		AttributeTree attrt = AttributeTreeUtil.createAttributeTree(sess, defName, classifyTarget);
		AttributeTreeUtil.addOtherAttributeTreeName(sess, attrt.getId(), "JA", defName);
		AttributeTreeUtil.addOtherAttributeTreeName(sess, attrt.getId(), "EN", enName);
		List atttItems = new ArrayList();
		for (int i = 0; i < items.length; i++) {
			atttItems.add(new AttributeTreeItem(-1, AttributeUtils.getAttributeTypeByName(sess,
				(String) items[i][0]), ((Boolean) items[i][1]).booleanValue()));
		}

		AttributeTreeUtil.setTreeItems(sess, attrt, atttItems);

		attrt = AttributeTreeUtil.getAttributeTreeById(sess, attrt.getId());
		sess.commit();
		return attrt;
	}

	/**
	 * 
	 * @param sess
	 * @return o
	 * @throws Exception
	 */
	public static AttributeTree createAttrTree1(EIMSession sess) throws Exception {
		return createAttrTree(sess, "t数値-t日付", "en:t数値-t日付", 0, new Object[][] {
				{ "t数値", Boolean.TRUE }//
				, { "t日付", Boolean.TRUE } //
			});
	}

	/**
	 * 
	 * @param sess
	 * @return o
	 * @throws Exception
	 */
	public static AttributeTree createAttrTree2(EIMSession sess) throws Exception {
		return createAttrTree(sess, "t文字-tテキスト", "en:t文字-tテキスト", 1, new Object[][] {
				{ "t文字", Boolean.FALSE }//
				, { "tテキスト", Boolean.FALSE } //
			});
	}

	/**
	 * 
	 * @param sess
	 * @throws Exception
	 */
	public static void createFolderTree(EIMSession sess) throws Exception {
		new CreateFolderTreeData(sess, DspAttrTreeTest.class.getResource(
			ClassUtils.getShortClassName(DspAttrTreeTest.class) + ".folderTree.xls").getPath()).process();// テスト用フォルダツリーを作る
	}

	/**
	 * 
	 * @param sess
	 * @param assertText
	 * @param attrt1
	 * @param attrt2
	 * @param objs
	 * @return o
	 * @throws Exception
	 */
	public static String replaceAssertText(EIMSession sess, String assertText,
			AttributeTree attrt1, AttributeTree attrt2, List objs) throws Exception {
		if (attrt1 != null)
			assertText = assertText.replaceAll("\\[id\\_1\\]", String.valueOf(attrt1.getId()))//
			.replaceAll("\\[settings\\_1\\]",
				AttributeTreeUtil.getAttrTreeSettingsStr(sess, attrt1))//
			.replaceAll("\\[name\\_1\\]", attrt1.getName())//
			;
		if (attrt2 != null)
			assertText = assertText.replaceAll("\\[id\\_2\\]", String.valueOf(attrt2.getId()))//
			.replaceAll("\\[settings\\_2\\]",
				AttributeTreeUtil.getAttrTreeSettingsStr(sess, attrt2))//
			.replaceAll("\\[name\\_2\\]", attrt2.getName())//
			;

		return TestAppMisc.replaceAssertText(assertText, objs);
	}

}
