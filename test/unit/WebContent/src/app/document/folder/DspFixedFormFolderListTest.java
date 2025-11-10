package app.document.folder;

import org.apache.commons.lang.ClassUtils;

import eim.bo.EIMObjectType;
import eim.util.EIMConfig;
import eim.util.ObjectUtils;
import eimtest.app.util.JSPTestCase;
import eimtest.app.util.TestAppDBUtil;
import eimtest.util.Misc;

/** */
public class DspFixedFormFolderListTest extends JSPTestCase {
	public void setUp() throws Exception {
		super.setUp();
		TestAppDBUtil.loadPrimitiveData();
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testMe() throws Exception {
		// フォルダタイプツリーを作成
		EIMObjectType folderType = ObjectUtils.getObjectTypeByName(sess,
			EIMConfig.get("OBJECT_TYPE_NAME_FOLDER"));
		EIMObjectType folder1Type = ObjectUtils.createObjectType(sess, "folder1", folderType);
		EIMObjectType folder11Type = ObjectUtils.createObjectType(sess, "folder11", folder1Type);
		EIMObjectType folder12Type = ObjectUtils.createObjectType(sess, "folder12", folder1Type);
		EIMObjectType folder2Type = ObjectUtils.createObjectType(sess, "folder2", folderType);
		EIMObjectType folder21Type = ObjectUtils.createObjectType(sess, "folder21", folder2Type);
		EIMObjectType folder22Type = ObjectUtils.createObjectType(sess, "folder22", folder2Type);
		EIMObjectType folder221Type = ObjectUtils.createObjectType(sess, "folder221", folder22Type);
		EIMObjectType folder222Type = ObjectUtils.createObjectType(sess, "folder222", folder22Type);
		sess.commit();

		// 検証用雛形XMLを作成
		String assertText = Misc.loadFile(this.getClass().getResource(
			ClassUtils.getShortClassName(this.getClass()) + "_01.txt").getPath());
		String[][] repStr = { { folder1Type.getName(), String.valueOf(folder1Type.getId()) }//
				, { folder11Type.getName(), String.valueOf(folder11Type.getId()) }//
				, { folder12Type.getName(), String.valueOf(folder12Type.getId()) }//
				, { folder2Type.getName(), String.valueOf(folder2Type.getId()) }//
				, { folder21Type.getName(), String.valueOf(folder21Type.getId()) }//
				, { folder22Type.getName(), String.valueOf(folder22Type.getId()) }//
				, { folder221Type.getName(), String.valueOf(folder221Type.getId()) }//
				, { folder222Type.getName(), String.valueOf(folder222Type.getId()) } //
		};
		for (int i = 0; i < repStr.length; i++) {
			assertText = assertText.replaceAll("\\[tid\\_" + repStr[i][0] + "\\]", repStr[i][1]);
		}
		// JSP返却結果XMLを検証
		assertEquals(assertText, xu.toStr(xu.toDOM(ds.folder_dspFixedFormFolderList())));
	}
}
