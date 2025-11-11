package app.document.folder;

import java.io.File;

import org.apache.commons.lang.ClassUtils;
import org.w3c.dom.Document;

import eim.bo.EIMAttributeType;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMSearchSelectEIMObject;
import eim.bo.EIMUser;
import eim.bo.EIMSearchSelectEIMObject.PsedoAttributeTypeEnum;
import eim.bo.EIMSearchSelectEIMObject.SearchConditionBuildHelper;
import eim.util.AttributeUtils;
import eim.util.ObjectUtils;
import eim.util.SearchUtils;
import eim.util.UserUtils;
import eimtest.app.util.JSPTestCase;
import eimtest.app.util.TestAppDBUtil;
import eimtest.app.util.TestAppObjectUtil;

/** */
public class DspChildObjectTest extends JSPTestCase {
	public void setUp() throws Exception {
		super.setUp();
		TestAppDBUtil.loadPrimitiveData();
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testMe() throws Exception {
		// login is 'system'
		File file = new File(this.getClass().getResource(
			ClassUtils.getShortClassName(this.getClass()) + ".class").getPath());
		EIMUser u1 = UserUtils.getUserByCode(sess, "u1");
		EIMObjectType docType = ObjectUtils.getObjectTypeByName(sess, "ドキュメント");
		// create document as u1
		ds.object_actCreateDocument("WS1", docType, "pp"//
			, u1.getId()//
			, "", file);

		// @Test: テーブル表示用のattType_[「作成者」属性タイプID]に、フォルダの作成者名とドキュメントの作成者名がセットされている
		EIMSearchSelectEIMObject s = new EIMSearchSelectEIMObject();
		SearchConditionBuildHelper h = new SearchConditionBuildHelper();
		s.setCondition(h.group(h.opAnd())//
		.addCondition(h.eq(h.opAnd(), AttributeUtils.getAttributeTypeByName(sess, "パス"), "/WS1/"))//
		.addCondition(h.eq(h.opAnd(), PsedoAttributeTypeEnum.NAME, file.getName())//
		));
		EIMObject folderObj = TestAppObjectUtil.getFolderObject("F1");
		EIMObject docObj = (EIMObject) SearchUtils.searchObjects(sess, s, null).get(0);
		EIMAttributeType attrTypeOfCreaterId = AttributeUtils.getAttributeTypeByName(sess, "作成者");
		Document xdoc = xu.toDOM(ds.folder_dspChildObject("WS1", -1, null, null, null));
		xu.assertNodeValues(new String[] { String.valueOf(folderObj.getId()),
				String.valueOf(docObj.getId()) }, xdoc, "/objList/object/@objId");
		xu.assertNodeValues(//
			new String[] { "システム管理者", u1.getName() }//
			, xdoc, "/objList/object/@attType_" + attrTypeOfCreaterId.getId());
	}
}
