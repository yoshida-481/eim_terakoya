package eimtest.app.util;

import java.io.File;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.TreeMap;

import junit.framework.Assert;

import org.apache.commons.lang.ArrayUtils;

import app.document.DocumentService;

import common.util.AppObjectConditionHelper;

import eim.bo.EIMAttribute;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMSearchSelectEIMObject;
import eim.bo.EIMUser;
import eim.bo.EIMValueType;
import eim.bo.EIMSearchSelectEIMObject.PsedoAttributeTypeEnum;
import eim.bo.EIMSearchSelectEIMObject.SearchConditionBuildHelper;
import eim.net.EIMSession;
import eim.util.AttributeUtils;
import eim.util.ObjectUtils;
import eim.util.SearchUtils;
import eimtest.util.TestSessionUtil;

/** */
public class TestAppObjectUtil {
	/**
	 * 
	 * @param folderName
	 * @return o
	 * @throws Exception
	 */
	public static EIMObject getFolderObject(String folderName) throws Exception {
		EIMSession sess = TestSessionUtil.createEIMSession();
		EIMSearchSelectEIMObject s = new EIMSearchSelectEIMObject();
		SearchConditionBuildHelper h = new SearchConditionBuildHelper();
		s.setCondition(h.group(h.opAnd())//
		.addCondition(
			h.eqObjTypeWithSubClasses(h.opAnd(),
				ObjectUtils.getObjectTypeByName(sess, "ワークスペース").getId(), sess))//
		.addCondition(h.eq(h.opAnd(), PsedoAttributeTypeEnum.NAME, folderName)));
		EIMObject folder = (EIMObject) SearchUtils.searchObjects(sess, s, null).get(0);
		sess.close();
		return folder;
	}

	/**
	 * 
	 * @param creatorUser
	 * @param file
	 * @return o
	 * @throws Exception
	 */
	public static EIMObject createObject(EIMUser creatorUser, File file) throws Exception {
		EIMSession sess = TestSessionUtil.createEIMSession();
		EIMObjectType docType = ObjectUtils.getObjectTypeByName(sess, "ドキュメント");
		new DocumentService().object_actCreateDocument("F1", docType, "pp"//
			, creatorUser.getId()//
			, "", file);
		EIMSearchSelectEIMObject s = new EIMSearchSelectEIMObject();
		SearchConditionBuildHelper h = new SearchConditionBuildHelper();
		s.setCondition(h.group(h.opAnd())//
		.addCondition(
			h.eq(h.opAnd(), AttributeUtils.getAttributeTypeByName(sess, "パス"), "/WS1/F1/"))//
		.addCondition(h.eq(h.opAnd(), PsedoAttributeTypeEnum.NAME, file.getName())//
		));
		EIMObject ret = (EIMObject) SearchUtils.searchObjects(sess, s, null).get(0);
		sess.close();
		return ret;
	}

	/**
	 * 
	 * @param obj
	 * @return o
	 * @throws Exception
	 */
	public static EIMObject reget(EIMObject obj) throws Exception {
		EIMSession sess = TestSessionUtil.createEIMSession();
		EIMSearchSelectEIMObject s = new EIMSearchSelectEIMObject();
		SearchConditionBuildHelper h = new SearchConditionBuildHelper();
		s.setCondition(h.group(h.opAnd()).addCondition(
			h.eq(h.opAnd(), PsedoAttributeTypeEnum.ID, obj.getId())));
		List hit = SearchUtils.searchObjects(sess, s, null);
		return (hit.size() == 0) ? null : (EIMObject) hit.get(0);
	}

	/**
	 * 
	 * @param sess
	 * @return o
	 * @throws Exception
	 */
	public static List getFolderAndDocObjs(EIMSession sess) throws Exception {
		AppObjectConditionHelper h = new AppObjectConditionHelper(sess);
		List objs = SearchUtils.searchObject(sess, ObjectUtils.getObjectTypeByName(sess, "ドキュメント"),
			null, true, false, -1, null, null, null, null, null, null, null, null, null, null);
		objs.addAll(SearchUtils.searchObject(sess, h.getTypeOfFolder(), null, true, false, -1,
			null, null, null, null, null, null, null, null, null, null));
		objs.addAll(SearchUtils.searchObject(sess, h.getTypeOfWorkspace(), null, true, true, -1,
			null, null, null, null, null, null, null, null, null, null));
		objs.addAll(SearchUtils.searchObject(sess, h.getTypeOfRecycle(), null, true, true, -1,
			null, null, null, null, null, null, null, null, null, null));
		return objs;
	}

	/**
	 * 
	 * @param sess
	 * @param objName
	 * @return o
	 * @throws Exception
	 */
	public static EIMObject getObj(EIMSession sess, String objName) throws Exception {
		List objs = SearchUtils.searchObject(sess, null, objName, false, false, -1, null, null,
			null, null, null, null, null, null, null);
		return (objs.size() > 0) ? (EIMObject) objs.get(0) : null;
	}

	/**
	 * 
	 * @param name
	 * @param objs
	 * @return o
	 */
	public static EIMObject findObj(String name, List objs) {
		for (Iterator i = objs.iterator(); i.hasNext();) {
			EIMObject obj = (EIMObject) i.next();
			if (obj.getName().equals(name))
				return obj;
		}
		return null;
	}

	/**
	 * 
	 * @param obj
	 * @return o
	 */
	public static TreeMap getObjAttrMap(EIMObject obj) {
		TreeMap ret = new TreeMap();
		for (Iterator i = obj.getAttributeList().iterator(); i.hasNext();) {
			EIMAttribute attr = (EIMAttribute) i.next();
			ret.put(attr.getType().getDefName(), attrToString(attr));
		}
		return ret;
	}

	/** */
	static DateFormat df = new SimpleDateFormat("yyyy/MM/dd");

	/**
	 * 
	 * @param a
	 * @return o
	 */
	public static String attrToString(EIMAttribute a) {
		switch (a.getType().getValueType().getId()) {
		case EIMValueType.INTEGER:
			return Arrays.asList(ArrayUtils.toObject(a.getInts())).toString();
		case EIMValueType.DATE:
			Date[] dt = a.getDates();
			StringBuffer sb = new StringBuffer("[");
			for (int i = 0; i < dt.length; i++) {
				if (i > 0)
					sb.append(",");
				sb.append(df.format(dt[i]));
			}
			sb.append("]");
			return sb.toString();
		case EIMValueType.STRING:
			return Arrays.asList(a.getStrings()).toString();
		case EIMValueType.TEXT:
			return Arrays.asList(a.getTexts()).toString();
		}
		return "";
	}

	/**
	 * 
	 * @param o1
	 * @param o2
	 * @param attrNames
	 */
	public static void assertEqualsAttr(EIMObject o1, EIMObject o2, String[] attrNames) {
		for (int i = 0; i < attrNames.length; i++) {
			assertEqualsAttr(o1, o2, attrNames[i], attrNames[i]);
		}
	}

	/**
	 * 
	 * @param o1
	 * @param o2
	 * @param an1
	 * @param an2
	 */
	public static void assertEqualsAttr(EIMObject o1, EIMObject o2, String an1, String an2) {
		EIMAttribute a1 = o1.getAttribute(an1);
		EIMAttribute a2 = o2.getAttribute(an2);
		if (a1 == null && a2 != null)
			Assert.fail("a2:" + a2.getType().getName() + " is not null");
		if (a1 != null && a2 == null)
			Assert.fail("a1:" + a1.getType().getName() + " is not null");
		if (a1 == null)
			Assert.fail(an1 + " is null");
		List l1 = null;
		List l2 = null;
		switch (a1.getType().getValueType().getId()) {
		case EIMValueType.INTEGER:
			l1 = Arrays.asList(ArrayUtils.toObject(a1.getInts()));
			l2 = Arrays.asList(ArrayUtils.toObject(a2.getInts()));
			break;
		case EIMValueType.DATE:
			l1 = Arrays.asList(a1.getDates());
			l2 = Arrays.asList(a2.getDates());
			break;
		case EIMValueType.STRING:
			l1 = Arrays.asList(a1.getStrings());
			l2 = Arrays.asList(a2.getStrings());
			break;
		case EIMValueType.TEXT:
			l1 = Arrays.asList(a1.getTexts());
			l2 = Arrays.asList(a2.getTexts());
			break;
		}
		Assert.assertEquals(a1.getType().getName(), l1.toString(), l2.toString());
	}

}
