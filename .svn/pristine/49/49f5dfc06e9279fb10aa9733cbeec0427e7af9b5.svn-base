package common.util;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.springframework.context.ApplicationContext;

import app.document.approve.ApproveCommonUtil;
import eim.bo.EIMAccessRole;
import eim.bo.EIMAttribute;
import eim.bo.EIMAttributeType;
import eim.bo.EIMException;
import eim.bo.EIMGroup;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMRelation;
import eim.bo.EIMRelationType;
import eim.bo.EIMResource;
import eim.bo.EIMRole;
import eim.bo.EIMSearchConditionCompare;
import eim.bo.EIMSearchConditionGroup;
import eim.bo.EIMSearchConditionIn;
import eim.bo.EIMSearchLimitCountCondition;
import eim.bo.EIMSearchOperatorEnum;
import eim.bo.EIMSearchResultList;
import eim.bo.EIMSearchSelectEIMObject;
import eim.bo.EIMSearchSelectEIMObject.PsedoAttributeTypeEnum;
import eim.bo.EIMStatus;
import eim.bo.EIMStatusType;
import eim.bo.EIMValueType;
import eim.bo.EIMVersion;
import eim.bo.EIMWorkFlow;
import eim.net.EIMSession;
import eim.util.AttributeUtils;
import eim.util.DateUtils;
import eim.util.EIMConfig;
import eim.util.EIMConstant;
import eim.util.GroupUtils;
import eim.util.ObjectAttributeUtils;
import eim.util.ObjectUtils;
import eim.util.RelationUtils;
import eim.util.RoleUtils;
import eim.util.SearchUtils;
import eim.util.StringUtils;
import eim.util.TypeConvertUtils;
import eim.util.UserUtils;
import eim.util.WorkFlowUtils;
import eim.util.internal.search.sql.SearchSqlUtil;
import jp.co.ctc_g.eim.app.document.business.service.PublicDocumentService;
import jp.co.ctc_g.eim.framework.common.util.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework2.business.dao.ObjectDao;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.WorkflowDomain;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchConditionIn;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchLimitCountCondition;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchSelectObject;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchSelectObject.SearchConditionBuildHelper;
import jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.common.context.TransactionContext;
import jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils;

/**
 *
 * (ドキュメント管理用)オブジェクト関連クラス
 *
 */
public class AppObjectUtil {

	/**
	 * 名称を元にオブジェクトを取得します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param typeName オブジェクトタイプ名称
	 * @param name オブジェクト名称
	 * @return 取得したオブジェクト
	 * @throws Exception
	 */
	public 	static EIMObject getObject(EIMSession sess, String typeName, String name) throws Exception
	{
		EIMObject object = ObjectUtils.getObjectByTypeAndName(sess, ObjectUtils.getObjectTypeByName(sess, typeName), name);
		return object;
	}

	/**
	 * 名称を元にオブジェクトのリストを取得します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param typeName オブジェクトタイプ名称
	 * @param name オブジェクト名称
	 * @return 取得したオブジェクト
	 * @throws Exception
	 */
	public static List getObjectList(EIMSession sess, String typeName, String name) throws Exception
	{
		List result = ObjectUtils.getObjectListByTypeAndName(sess, ObjectUtils.getObjectTypeByName(sess, typeName), name,EIMAccessRole.READ);
		if (result == null) result = new ArrayList();
		return(result);
	}

	/**
	 * 名称を元にオブジェクトを作成します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param typeName オブジェクトタイプ名称
	 * @param name オブジェクト名称
	 * @return 作成したオブジェクト
	 * @throws Exception
	 */
	public static 	EIMObject createObject(EIMSession sess, String typeName, String name) throws Exception
	{
		return(ObjectUtils.createObject(sess, ObjectUtils.getObjectTypeByName(sess, typeName), name));
	}

	/**
	 * オブジェクトを属性も含めて削除します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param typeName オブジェクトタイプ名称
	 * @param name オブジェクト名称
	 * @throws Exception
	 */
	public static void deleteObject(EIMSession sess, String typeName, String name) throws Exception
	{
		List objList = getObjectList(sess, typeName, name);
		if (objList != null)
		{
			for (int i = 0 ; i < objList.size(); i++)
			{
				EIMObject obj = (EIMObject)objList.get(i);
				ObjectAttributeUtils.deleteAllAttributes(sess, obj);
				ObjectUtils.deleteObject(sess, obj);
			}
		}
	}

	/**
	 * 属性値を削除します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param obj 属性を削除するオブジェクト
	 * @param attrName 属性名称
	 * @throws Exception
	 */
	public static void deleteAttribute(EIMSession sess, EIMObject obj, String attrName) throws Exception
	{
		EIMAttributeType attType = AttributeUtils.getAttributeTypeByName(sess, attrName);
		ObjectAttributeUtils.deleteAttribute(sess, obj, attType);
	}

	/**
	 * オブジェクト属性(数値)を取得します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param obj 属性を取得するオブジェクト
	 * @param attrName 属性名称
	 * @param inCaseNullNumber 属性値がnullの場合に返す値を設定します。(例:Integer.MIN_VALUE)
	 * @return 属性値
	 * @throws Exception
	 */
	public static long getIntAttr(EIMSession sess, EIMObject obj, String attrName, int inCaseNullNumber) throws Exception
	{
		if(obj.getAttribute(attrName) != null) {
			return obj.getAttribute(attrName).getInt();
		} else {
			return inCaseNullNumber;
		}
	}

	/**
	 * オブジェクト属性(数値)を配列で取得します。
	 *
	 * <li>複数値の場合は明細キー順に配列に格納して返します。
	 * <li>単数値の場合は長さ1の配列に格納して返します。
	 * <li>属性値がnull(存在しない)の場合はnullを返します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param obj 属性を取得するオブジェクト
	 * @param attrName 属性名称
	 * @return 属性値を格納した配列
	 * @throws Exception
	 */
	public static long[] getIntAttrs(EIMSession sess, EIMObject obj, String attrName) throws Exception
	{
		long[] ret = null;
		EIMAttribute attr = obj.getAttribute(attrName);
		if (attr != null) {
			try {
				ret = TypeConvertUtils.convertToLongArray(attr.getInts());
			} catch (UnsupportedOperationException e) {
				// 値が存在しない場合nullを返却
			}
		}
		return ret;
	}

	/**
	 * オブジェクト属性(double数値)を取得します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param obj 属性を取得するオブジェクト
	 * @param attrName 属性名称
	 * @param inCaseNullNumber 属性値がnullの場合に返す値を設定します。(例:Double.MIN_VALUE)
	 * @return 属性値
	 * @throws Exception
	 */
	public static double getDoubleAttr(EIMSession sess, EIMObject obj, String attrName, double inCaseNullNumber) throws Exception
	{
		if(obj.getAttribute(attrName) != null) {
			return obj.getAttribute(attrName).getDouble();
		} else {
			return inCaseNullNumber;
		}
	}

	/**
	 * オブジェクト属性(double数値)を配列で取得します。
	 *
	 * <li>複数値の場合は明細キー順に配列に格納して返します。
	 * <li>単数値の場合は長さ1の配列に格納して返します。
	 * <li>属性値がnull(存在しない)の場合はnullを返します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param obj 属性を取得するオブジェクト
	 * @param attrName 属性名称
	 * @return 属性値を格納した配列
	 * @throws Exception
	 */
	public static double[] getDoubleAttrs(EIMSession sess, EIMObject obj, String attrName) throws Exception
	{
		double[] ret = null;
		EIMAttribute attr = obj.getAttribute(attrName);
		if (attr != null) {
			try {
				ret = attr.getDoubles();
			} catch (UnsupportedOperationException e) {
				// 値が存在しない場合nullを返却
			}
		}
		return ret;
	}

	/**
	 * オブジェクト属性(文字列)を取得します。
	 *
	 * <li>属性値がnull(存在しない)の場合はnullを返します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param obj 属性を取得するオブジェクト
	 * @param attrName 属性名称
	 * @return 属性値
	 * @throws Exception
	 */
	public static String getStrAttr(EIMSession sess, EIMObject obj, String attrName) throws Exception
	{
		String attr = null;
		if(obj.getAttribute(attrName) != null) attr = obj.getAttribute(attrName).getString();
		return(attr);
	}

	/**
	 * オブジェクト属性(文字列)を配列で取得します。
	 *
	 * <li>複数値の場合は明細キー順に配列に格納して返します。
	 * <li>単数値の場合は長さ1の配列に格納して返します。
	 * <li>属性値がnull(存在しない)の場合はnullを返します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param obj 属性を取得するオブジェクト
	 * @param attrName 属性名称
	 * @return 属性値を格納した配列
	 * @throws Exception
	 */
	public static String[] getStrAttrs(EIMSession sess, EIMObject obj, String attrName) throws Exception
	{
		String[] ret = null;
		EIMAttribute attr = obj.getAttribute(attrName);
		if (attr != null) {
			try {
				ret = attr.getStrings();
			} catch (UnsupportedOperationException e) {
				// 値が存在しない場合nullを返却
			}
		}
		return ret;
	}

	/**
	 * オブジェクト属性(日付)を取得します。
	 *
	 * <li>属性値がnull(存在しない)の場合はnullを返します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param obj 属性を取得するオブジェクト
	 * @param attrName 属性名称
	 * @return 属性値
	 * @throws Exception
	 */
	public 	static Date getDateAttr(EIMSession sess, EIMObject obj, String attrName) throws Exception
	{
		Date attr = null;
		if(obj.getAttribute(attrName) != null) attr = obj.getAttribute(attrName).getDate();
		return(attr);
	}

	/**
	 * オブジェクト属性(日付)を配列で取得します。
	 *
	 * <li>複数値の場合は明細キー順に配列に格納して返します。
	 * <li>単数値の場合は長さ1の配列に格納して返します。
	 * <li>属性値がnull(存在しない)の場合はnullを返します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param obj 属性を取得するオブジェクト
	 * @param attrName 属性名称
	 * @return 属性値を格納した配列
	 * @throws Exception
	 */
	public 	static Date[] getDateAttrs(EIMSession sess, EIMObject obj, String attrName) throws Exception
	{
		Date[] ret = null;
		EIMAttribute attr = obj.getAttribute(attrName);
		if (attr != null) {
			try {
				ret = attr.getDates();
			} catch (UnsupportedOperationException e) {
				// 値が存在しない場合nullを返却
			}
		}
		return ret;
	}

	/**
	 * オブジェクト属性(テキスト)を取得します。
	 *
	 * <li>属性値がnull(存在しない)の場合はnullを返します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param obj 属性を取得するオブジェクト
	 * @param attrName 属性名称
	 * @return 属性値
	 * @throws Exception
	 */
	public static String getTextAttr(EIMSession sess, EIMObject obj, String attrName) throws Exception
	{
		String attr = null;
		if(obj.getAttribute(attrName) != null) attr = obj.getAttribute(attrName).getText();
		return(attr);
	}

	/**
	 * オブジェクト属性(テキスト)を配列で取得します。
	 *
	 * <li>複数値の場合は明細キー順に配列に格納して返します。
	 * <li>単数値の場合は長さ1の配列に格納して返します。
	 * <li>属性値がnull(存在しない)の場合はnullを返します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param obj 属性を取得するオブジェクト
	 * @param attrName 属性名称
	 * @return 属性値を格納した配列
	 * @throws Exception
	 */
	public static String[] getTextAttrs(EIMSession sess, EIMObject obj, String attrName) throws Exception
	{
		String[] ret = null;
		EIMAttribute attr = obj.getAttribute(attrName);
		if (attr != null) {
			try {
				ret = attr.getTexts();
			} catch (UnsupportedOperationException e) {
				// 値が存在しない場合nullを返却
			}
		}
		return ret;
	}

	/**
	 * オブジェクト属性(数値)を作成します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param obj 属性を付加するオブジェクト
	 * @param attrName 属性名称
	 * @param value 属性値
	 * @throws Exception
	 */
	public 	static void setAttr(EIMSession sess, EIMObject obj, String attrName, int value) throws Exception
	{
		ObjectAttributeUtils.setAttribute(sess, obj, AttributeUtils.getAttributeTypeByName(sess, attrName), value);
	}

	/**
	 * オブジェクト属性(数値)を複数値で作成します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param obj 属性を付加するオブジェクト
	 * @param attrName 属性名称
	 * @param values 属性値の配列(明細キー順に格納する)
	 * @throws Exception
	 */
	public 	static void setAttr(EIMSession sess, EIMObject obj, String attrName, long[] values) throws Exception
	{
		ObjectAttributeUtils.setAttribute(sess, obj, AttributeUtils.getAttributeTypeByName(sess, attrName)
				, TypeConvertUtils.convertToBuildTypeArray(values));
	}

	/**
	 * オブジェクト属性(long型)を作成します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param obj 属性を付加するオブジェクト
	 * @param attrName 属性名称
	 * @param value 属性値
	 * @throws Exception
	 */
	public 	static void setAttr(EIMSession sess, EIMObject obj, String attrName, long value) throws Exception
	{
		ObjectAttributeUtils.setAttribute(sess, obj, AttributeUtils.getAttributeTypeByName(sess, attrName), value);
	}

	/**
	 * オブジェクト属性(文字列)を作成します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param obj 属性を付加するオブジェクト
	 * @param attrName 属性名称
	 * @param value 属性値
	 * @throws Exception
	 */
	public 	static void setAttr(EIMSession sess, EIMObject obj, String attrName, String value) throws Exception
	{
		ObjectAttributeUtils.setAttribute(sess, obj, AttributeUtils.getAttributeTypeByName(sess, attrName), value);
	}

	/**
	 * オブジェクト属性(文字列)を複数値で作成します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param obj 属性を付加するオブジェクト
	 * @param attrName 属性名称
	 * @param values 属性値の配列(明細キー順に格納する)
	 * @throws Exception
	 */
	public 	static void setAttr(EIMSession sess, EIMObject obj, String attrName, String[] values) throws Exception
	{
		ObjectAttributeUtils.setAttribute(sess, obj, AttributeUtils.getAttributeTypeByName(sess, attrName), values);
	}

	/**
	 * オブジェクト属性(日付)を作成します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param obj 属性を付加するオブジェクト
	 * @param attrName 属性名称
	 * @param value 属性値
	 * @throws Exception
	 */
	public 	static void setAttr(EIMSession sess, EIMObject obj, String attrName, Date value) throws Exception
	{
		ObjectAttributeUtils.setAttribute(sess, obj, AttributeUtils.getAttributeTypeByName(sess, attrName), value);
	}

	/**
	 * オブジェクト属性(日付)を複数値で作成します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param obj 属性を付加するオブジェクト
	 * @param attrName 属性名称
	 * @param values 属性値の配列(明細キー順に格納する)
	 * @throws Exception
	 */
	public 	static void setAttr(EIMSession sess, EIMObject obj, String attrName, Date[] values) throws Exception
	{
		ObjectAttributeUtils.setAttribute(sess, obj, AttributeUtils.getAttributeTypeByName(sess, attrName), values);
	}

	/**
	 * オブジェクト属性(文字列)を強制的に設定します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param obj 属性を付加するオブジェクト
	 * @param attrName 属性名称
	 * @param value 属性値
	 * @throws Exception
	 */
	public 	static void setAttrForce(EIMSession sess, EIMObject obj, String attrName, String value) throws Exception
	{
		ObjectAttributeUtils.setAttribute(sess, obj, AttributeUtils.getAttributeTypeByName(sess, attrName), value);
	}

	/**
	 * オブジェクト属性(文字列)を複数値で強制的に設定します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param obj 属性を付加するオブジェクト
	 * @param attrName 属性名称
	 * @param values 属性値の配列(明細キー順に格納する)
	 * @throws Exception
	 */
	public 	static void setAttrForce(EIMSession sess, EIMObject obj, String attrName, String[] values) throws Exception
	{
		ObjectAttributeUtils.setAttribute(sess, obj, AttributeUtils.getAttributeTypeByName(sess, attrName), values);
	}

	/**
	 * オブジェクト属性(数値)を更新します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param obj 属性を更新するオブジェクト
	 * @param attrName 属性名称
	 * @param value 属性値
	 * @throws Exception
	 */
	public 	static void updateAttr(EIMSession sess, EIMObject obj, String attrName, int value) throws Exception
	{
		EIMAttributeType attType = AttributeUtils.getAttributeTypeByName(sess, attrName);
		ObjectAttributeUtils.deleteAttribute(sess, obj, attType);
		ObjectAttributeUtils.setAttribute(sess, obj, attType, value);
	}

	/**
	 * オブジェクト属性(数値)を複数値で更新します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param obj 属性を更新するオブジェクト
	 * @param attrName 属性名称
	 * @param values 属性値の配列(明細キー順に格納する)
	 * @throws Exception
	 */
	public 	static void updateAttr(EIMSession sess, EIMObject obj, String attrName, long[] values) throws Exception
	{
		EIMAttributeType attType = AttributeUtils.getAttributeTypeByName(sess, attrName);
		ObjectAttributeUtils.deleteAttribute(sess, obj, attType);
		ObjectAttributeUtils.setAttribute(sess, obj, attType, TypeConvertUtils.convertToBuildTypeArray(values));
	}

	/**
	 * オブジェクト属性(文字列)を更新します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param obj 属性を更新するオブジェクト
	 * @param attrName 属性名称
	 * @param value 属性値
	 * @throws Exception
	 */
	public 	static void updateAttr(EIMSession sess, EIMObject obj, String attrName, String value) throws Exception
	{
		EIMAttributeType attType = AttributeUtils.getAttributeTypeByName(sess, attrName);
		ObjectAttributeUtils.deleteAttribute(sess, obj, attType);
		ObjectAttributeUtils.setAttribute(sess, obj, attType, value);
	}

	/**
	 * オブジェクト属性(文字列)を複数値で更新します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param obj 属性を更新するオブジェクト
	 * @param attrName 属性名称
	 * @param values 属性値の配列(明細キー順に格納する)
	 * @throws Exception
	 */
	public 	static void updateAttr(EIMSession sess, EIMObject obj, String attrName, String[] values) throws Exception
	{
		EIMAttributeType attType = AttributeUtils.getAttributeTypeByName(sess, attrName);
		ObjectAttributeUtils.deleteAttribute(sess, obj, attType);
		ObjectAttributeUtils.setAttribute(sess, obj, attType, values);
	}

	/**
	 * オブジェクト属性(日付)を更新します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param obj 属性を更新するオブジェクト
	 * @param attrName 属性名称
	 * @param value 属性値
	 * @throws Exception
	 */
	public static void updateAttr(EIMSession sess, EIMObject obj, String attrName, Date value) throws Exception
	{
		EIMAttributeType attType = AttributeUtils.getAttributeTypeByName(sess, attrName);
		ObjectAttributeUtils.deleteAttribute(sess, obj, attType);
		ObjectAttributeUtils.setAttribute(sess, obj, attType, value);
	}

	/**
	 * オブジェクト属性(日付)を複数値で更新します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param obj 属性を更新するオブジェクト
	 * @param attrName 属性名称
	 * @param values 属性値の配列(明細キー順に格納する)
	 * @throws Exception
	 */
	public static void updateAttr(EIMSession sess, EIMObject obj, String attrName, Date[] values) throws Exception
	{
		EIMAttributeType attType = AttributeUtils.getAttributeTypeByName(sess, attrName);
		ObjectAttributeUtils.deleteAttribute(sess, obj, attType);
		ObjectAttributeUtils.setAttribute(sess, obj, attType, values);
	}

	/**
	 * リレーションタイプを指定して親のEIMObjectの配列を取得します。
	 *
	 * @param sess	EIMSessionインスタンス
	 * @param childObject 起点となるオブジェクト
	 * @param relTypeStr リレーションタイプ文字列
	 * @return	親のオブジェクトの配列
	 * @throws Exception
	 */
	public static EIMObject[] getParentEIMObject(EIMSession sess,EIMObject childObject, String relTypeStr) throws Exception{
		List objects = new ArrayList();
		if(childObject != null){
			EIMRelationType relType = RelationUtils.getRelationTypeByName(sess,relTypeStr);
			List relList = RelationUtils.getParentRelationListByRelType(sess,childObject,relType,EIMAccessRole.READ);
			for (int i = 0; i < relList.size(); i++) {
				EIMRelation rel = (EIMRelation)relList.get(i);
				EIMObject parentObject = rel.getParent();
				objects.add(parentObject);
			}
		}
		return (EIMObject[])objects.toArray(new EIMObject[0]);
	}

	/**
	 * リレーションタイプを指定して子のEIMObjectの配列を取得します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param parentObject 起点となるオブジェクト
	 * @param relTypeStr リレーションタイプ文字列
	 * @return 子のオブジェクトの配列
	 * @throws Exception
	 */
	public static EIMObject[] getChildEIMObject(EIMSession sess,EIMObject parentObject, String relTypeStr) throws Exception{
		ArrayList objects = new ArrayList();
		if(parentObject != null){
			EIMRelationType relType = RelationUtils.getRelationTypeByName(sess,relTypeStr);
			List relList = RelationUtils.getChildRelationListByRelType(sess,parentObject,relType,EIMAccessRole.READ);
			for (int i = 0; i < relList.size(); i++) {
				EIMRelation rel = (EIMRelation)relList.get(i);
				EIMObject childObject = rel.getChild();
				objects.add(childObject);
			}
		}
		return (EIMObject[])objects.toArray(new EIMObject[0]);
	}

	/**
	 * リレーションタイプを指定して子のEIMObjectの配列を取得します。
	 * 取得するEIMObjectは、子EIMObjectがなくなるまで再帰的に取得します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param parentObject 起点となるオブジェクト
	 * @param relTypeStr リレーションタイプ文字列
	 * @return 子のオブジェクトのList
	 * @throws Exception
	 */
	public static List getChildEIMObjectRecurrently(EIMSession sess,EIMObject parentObject, String relTypeStr) throws Exception{
		List objects = new ArrayList();
		if(parentObject != null){
			EIMRelationType relType = RelationUtils.getRelationTypeByName(sess,relTypeStr);
			List relList = RelationUtils.getChildRelationListByRelType(sess,parentObject,relType,EIMAccessRole.READ);
			for (int i = 0; i < relList.size(); i++) {
				EIMRelation rel = (EIMRelation)relList.get(i);
				EIMObject childObject = rel.getChild();
				objects.add(childObject);

				objects.addAll(getChildEIMObjectRecurrently(sess, childObject, relTypeStr));
			}
		}
		return objects;
	}

	/**
	 * リレーションタイプを指定して親のEIMObjectの配列を取得します。
	 * 取得するEIMObjectは、親EIMObjectがなくなるまで再帰的に取得します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param childObject 起点となるオブジェクト
	 * @param relTypeStr リレーションタイプ文字列
	 * @return 親のオブジェクトのList
	 * @throws Exception
	 */
	public static List getParentEIMObjectRecurrently(EIMSession sess,EIMObject childObject, String relTypeStr) throws Exception{
		List objects = new ArrayList();
		if(childObject != null){
			EIMRelationType relType = RelationUtils.getRelationTypeByName(sess,relTypeStr);
			List relList = RelationUtils.getParentRelationListByRelType(sess, childObject, relType);
			for (int i = 0; i < relList.size(); i++) {
				EIMRelation rel = (EIMRelation)relList.get(i);
				EIMObject parentObject = rel.getParent();
				objects.add(parentObject);

				objects.addAll(getParentEIMObjectRecurrently(sess, parentObject, relTypeStr));
			}
		}
		return objects;
	}


	/**
	 * リストを与えられた文字列取得のメソッド名の結果を整数に変換してソートします。
	 *
	 * <li>メソッド名は、Listの要素オブジェクトに定義されており、かつStringを返す引数なしのgetterメソッドである必要があります。
	 *
	 * @param list	ソート前リスト
	 * @param sortmethodName メソッド名
	 * @param isUp trueの場合は昇順、falseの場合は降順
	 * @return	ソート後リスト
	 * @throws Exception
	 */
	public static List getStrByIntSortedList(List list ,String sortmethodName,boolean isUp) {
		Object[] oarray = list.toArray();
		Arrays.sort(oarray,new AppObjectUtil().new StrByIntMethodResultComparator(sortmethodName,isUp));
		return new ArrayList(Arrays.asList(oarray));
	}

	/**
	 * リストを与えられた整数取得のメソッド名の結果によりソートします。
	 *
	 * <li>メソッド名は、Listの要素オブジェクトに定義されており、かつStringを返す引数なしのgetterメソッドである必要があります。
	 *
	 * @param list	ソート前リスト
	 * @param sortmethodName メソッド名
	 * @param isUp trueの場合は昇順、falseの場合は降順
	 * @return	ソート後リスト
	 * @throws Exception
	 */
	public static List getIntSortedList(List list ,String sortmethodName,boolean isUp) {
		Object[] oarray = list.toArray();
		Arrays.sort(oarray,new AppObjectUtil().new IntMethodResultComparator(sortmethodName,isUp));
		return new ArrayList(Arrays.asList(oarray));

	}

	/**
	 * リストを与えられた実数取得のメソッド名の結果によりソートします。
	 *
	 * <li>メソッド名は、Listの要素オブジェクトに定義されており、かつStringを返す引数なしのgetterメソッドである必要があります。
	 *
	 * @param list	ソート前リスト
	 * @param sortmethodName メソッド名
	 * @param isUp trueの場合は昇順、falseの場合は降順
	 * @return	ソート後リスト
	 * @throws Exception
	 */
	public static List getLongSortedList(List list ,String sortmethodName,boolean isUp) {
		Object[] oarray = list.toArray();
		Arrays.sort(oarray,new AppObjectUtil().new LongMethodResultComparator(sortmethodName,isUp));
		return new ArrayList(Arrays.asList(oarray));

	}
	/**
	 * リストを与えられた文字列取得のメソッド名の結果によりソートします。
	 *
	 * <li>メソッド名は、Listの要素オブジェクトに定義されており、かつStringを返す引数なしのgetterメソッドである必要があります。
	 *
	 * @param list	ソート前リスト
	 * @param sortmethodName メソッド名
	 * @param isUp trueの場合は昇順、falseの場合は降順
	 * @return	ソート後リスト
	 * @throws Exception
	 */
	public static List getStrSortedList(List list ,String sortmethodName,boolean isUp) {
		Object[] oarray = list.toArray();
		Arrays.sort(oarray,new AppObjectUtil().new StrMethodResultComparator(sortmethodName,isUp));
		return new ArrayList(Arrays.asList(oarray));

	}

	/**
	 * リストを与えられたData取得のメソッド名の結果によりソートします。
	 *
	 * <li>メソッド名は、Listの要素オブジェクトに定義されており、かつStringを返す引数なしのgetterメソッドである必要があります。
	 *
	 * @param list	ソート前リスト
	 * @param sortmethodName メソッド名
	 * @param isUp trueの場合は昇順、falseの場合は降順
	 * @return	ソート後リスト
	 * @throws Exception
	 */
	public static List getDateSortedList(List list ,String sortmethodName,boolean isUp) {
		Object[] oarray = list.toArray();
		Arrays.sort(oarray,new AppObjectUtil().new DateMethodResultComparator(sortmethodName,isUp));
		return new ArrayList(Arrays.asList(oarray));
	}

	/**
	 * メソッド名で与えられたメソッドの実行結果から比較するためのComparetorです。
	 *
	 * <li>Stringをintでソートします。格納されている値がintに変換不可能な場合は例外を返します。
	 */
	private class StrByIntMethodResultComparator implements java.util.Comparator{
		private String sortMethodName;
		private boolean isUp;

		/**
		 * Comparetor
		 * @param _sortMethodName
		 * @param _isUp
		 */
		public StrByIntMethodResultComparator(String _sortMethodName,boolean _isUp){
			sortMethodName = _sortMethodName;
			isUp = _isUp;
		}
		public int compare(Object o1, Object o2){
			int ret=0;
			try {
				Method m1 = o1.getClass().getMethod(sortMethodName,(Class[])null);
				Method m2 = o2.getClass().getMethod(sortMethodName,(Class[])null);

				if(isUp){
					ret = (Long.valueOf(m1.invoke(o1,(Object[])null).toString())).compareTo(Long.valueOf(m2.invoke(o2,(Object[])null).toString()));
				}else{
					ret = (Long.valueOf(m2.invoke(o2,(Object[])null).toString())).compareTo(Long.valueOf(m1.invoke(o1,(Object[])null).toString()));
				}
			} catch (Exception e) {
				throw new RuntimeException(e);
			}
		    return ret;
		}
	}

	/**
	 * メソッド名で与えられたメソッドの実行結果から比較するためのComparetorです(String)。
	 *
	 */
	private class StrMethodResultComparator implements java.util.Comparator{
		private String sortMethodName;
		private boolean isUp;

		/**
		 * Comparetor
		 * @param _sortMethodName
		 * @param _isUp
		 */
		public StrMethodResultComparator(String _sortMethodName,boolean _isUp){
			sortMethodName = _sortMethodName;
			isUp = _isUp;
		}
		public int compare(Object o1, Object o2){
			int ret=0;
			try {
				Method m1 = o1.getClass().getMethod(sortMethodName,(Class[])null);
				Method m2 = o2.getClass().getMethod(sortMethodName,(Class[])null);
				
				String s1 = ((String)m1.invoke(o1,(Object[])null));
				String s2 = (String)m2.invoke(o2,(Object[])null);
				
				if(isUp){
					if (s1 == null && s2 == null) {
						ret = 0;
					} else if (s1 == null) {
						ret = 1;
					} else if (s2 == null) {
						ret = -1;
					} else {
						ret = (s1).compareTo(s2);
					}
				}else{
					if (s1 == null && s2 == null) {
						ret = 0;
					} else if (s2 == null) {
						ret = 1;
					} else if (s1 == null) {
						ret = -1;
					} else {
						ret = (s2).compareTo(s1);
					}
				}
			} catch (Exception e) {
				throw new RuntimeException(e);
			}
		    return ret;
		}
	}

	/**
	 * メソッド名で与えられたメソッドの実行結果から比較するためのComparetorです(int)。
	 *
	 */
	private class IntMethodResultComparator implements java.util.Comparator{
		private String sortMethodName;
		private boolean isUp;

		/**
		 * Comparetor
		 * @param _sortMethodName
		 * @param _isUp
		 */
		public IntMethodResultComparator(String _sortMethodName,boolean _isUp){
			sortMethodName = _sortMethodName;
			isUp = _isUp;
		}
		public int compare(Object o1, Object o2){
			int ret=0;
			try {
				Method m1 = o1.getClass().getMethod(sortMethodName,(Class[])null);
				Method m2 = o2.getClass().getMethod(sortMethodName,(Class[])null);
				if(isUp){
					ret = (Long.valueOf(m1.invoke(o1,(Object[])null).toString())).compareTo(Long.valueOf(m2.invoke(o2,(Object[])null).toString()));
				}else{
					ret = (Long.valueOf(m2.invoke(o2,(Object[])null).toString())).compareTo(Long.valueOf(m1.invoke(o1,(Object[])null).toString()));
				}
			} catch (Exception e) {
				throw new RuntimeException(e);
			}
		    return ret;
		}
	}

	/**
	 * メソッド名で与えられたメソッドの実行結果から比較するためのComparetorです(long)。
	 *
	 */
	private class LongMethodResultComparator implements java.util.Comparator{
		private String sortMethodName;
		private boolean isUp;

		/**
		 * Comparetor
		 * @param _sortMethodName
		 * @param _isUp
		 * @since Ver 6.5
		 */
		public LongMethodResultComparator(String _sortMethodName,boolean _isUp){
			sortMethodName = _sortMethodName;
			isUp = _isUp;
		}
		/**
		 * compare
		 * @param o1
		 * @param o2
		 * @return ret
		 * @since Ver 6.5
		 */
		public int compare(Object o1, Object o2){
			int ret=0;
			try {
				Method m1 = o1.getClass().getMethod(sortMethodName,(Class[])null);
				Method m2 = o2.getClass().getMethod(sortMethodName,(Class[])null);
				if(isUp){
					ret = ((Long)m1.invoke(o1,(Object[])null)).compareTo((Long)m2.invoke(o2,(Object[])null));
				}else{
					ret = ((Long)m2.invoke(o2,(Object[])null)).compareTo((Long)m1.invoke(o1,(Object[])null));
				}
			} catch (Exception e) {
				throw new RuntimeException(e);
			}
		    return ret;
		}
	}

	/**
	 * メソッド名で与えられたメソッドの実行結果から比較するためのComparetorです(Date)。
	 *
	 */
	private class DateMethodResultComparator implements java.util.Comparator{
		private String sortMethodName;
		private boolean isUp;

		/**
		 * Comparetor
		 * @param _sortMethodName
		 * @param _isUp
		 */
		public DateMethodResultComparator(String _sortMethodName,boolean _isUp){
			sortMethodName = _sortMethodName;
			isUp = _isUp;
		}
		public int compare(Object o1, Object o2){
			int ret=0;
			try {
				Method m1 = o1.getClass().getMethod(sortMethodName,(Class[])null);
				Method m2 = o2.getClass().getMethod(sortMethodName,(Class[])null);
				if(isUp){
					ret = ((Date)m1.invoke(o1,(Object[])null)).compareTo((Date)m2.invoke(o2,(Object[])null));
				}else{
					ret = ((Date)m2.invoke(o2,(Object[])null)).compareTo((Date)m1.invoke(o1,(Object[])null));
				}
			} catch (Exception e) {
				throw new RuntimeException(e);
			}
		    return ret;
		}
	}

	/**
	 * 入力文字列にWindows禁止文字が含まれる場合にはEIMExceptionを返します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param name ファイル名、または、フォルダ名
	 * @throws EIMException
	 */
	public static void checkValidateFName(EIMSession sess, String name) throws EIMException {
		if (!StringUtils.isBlank(name)) {
			for (int i = 0 ; AppConstant.INVALID_NAME_CHAR.length > i ; i++) {
				if (name.indexOf(AppConstant.INVALID_NAME_CHAR[i]) != -1) {
					// 名前には次の文字は使えません。\n \ / : * ? " < > |
					throw new EIMException(sess, "EIM.WARN.LOGIC.INVALIDNAME");
				}
			}
		}
	}

	/**
	 * 入力文字列にASCII制御文字が含まれる場合空文字に置き換える。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param name ファイル名、または、フォルダ名
	 * @return escapeName ASCII制御文字を空文字に置き換えたファイル名、フォルダ名
	 * @throws EIMException
	 */
	public static String escapeAsciiControlCode(EIMSession sess, String name) throws EIMException {
		if (!StringUtils.isBlank(name)) {
			name = name.replaceAll("[\\00-\\x1f\\x7f]", "");
		}
		return name;
	}

	/**
	 * 指定のオブジェクトがごみ箱またはワークスペース固有ごみ箱に属するか否かを判定します。
	 *
	 * <li>指定のオブジェクトがごみ箱、またはごみ箱配下に存在する場合、trueを返します。
	 * <li>指定のオブジェクトがワークスペース固有ごみ箱配下に存在する場合、trueを返します。
	 * <li>指定のオブジェクトがnullの場合もtrueを返します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param obj 指定のオブジェクト
	 * @return ごみ箱に属するか否か
	 * @throws Exception
	 */
	public static boolean isObjectInRecycle(EIMSession sess, EIMObject obj) throws Exception {

		if (obj == null) {
			return true;
		}
		EIMObjectType recycleObjType = ObjectUtils.getObjectTypeByName(sess,
				EIMConfig.get("OBJECT_TYPE_NAME_RECYCLE"));		// ごみ箱のオブジェクトタイプ

		// 指定のオブジェクト自身がごみ箱の場合
		if (obj.getType().getId() == recycleObjType.getId()) {
			return true;
		}

		EIMRelationType relType = RelationUtils.getRelationTypeByName(sess,
				EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));						// ドキュメントのリレーションタイプ
		EIMObject rootObj = ObjectUtils.getRootObject(sess, obj.getId(), relType);	// ルートオブジェクト

		// ワークスペース固有ごみ箱の判定
		EIMObject[] orgParentList = AppObjectUtil.getParentEIMObject(sess, obj, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));
		for (int i = 0; i < orgParentList.length; i++) {
			if(orgParentList[i].getType().getDefName().equals(EIMConfig.getValue("OBJECT_TYPE_NAME_WORKSPACERECYCLE"))){
				return true;
			}
		}

		// 指定のオブジェクトのルートに存在するオブジェクトがごみ箱の場合
		if (rootObj != null && recycleObjType.getId() == rootObj.getType().getId()) {
			return true;
		} else {
			return false;
		}
	}

	/**
	 * 指定のオブジェクトがシステムごみ箱に属するか否かを判定します。
	 *
	 * <li>指定のオブジェクトがシステムごみ箱、またはシステムごみ箱配下に存在する場合、trueを返します。
	 * <li>指定のオブジェクトがnullの場合もtrueを返します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param obj 指定のオブジェクト
	 * @return ごみ箱に属するか否か
	 * @throws Exception
	 */
	public static boolean isObjectInSystemRecycle(EIMSession sess, EIMObject obj) throws Exception {

		if (obj == null) {
			return true;
		}
		EIMObjectType recycleObjType = ObjectUtils.getObjectTypeByName(sess,
				EIMConfig.get("OBJECT_TYPE_NAME_RECYCLE"));		// ごみ箱のオブジェクトタイプ

		// 指定のオブジェクト自身がごみ箱の場合
		if (obj.getType().getId() == recycleObjType.getId()) {
			return true;
		}

		EIMRelationType relType = RelationUtils.getRelationTypeByName(sess,
				EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));						// ドキュメントのリレーションタイプ
		EIMObject rootObj = ObjectUtils.getRootObject(sess, obj.getId(), relType);	// ルートオブジェクト

		// 指定のオブジェクトのルートに存在するオブジェクトがごみ箱の場合
		if (rootObj != null && recycleObjType.getId() == rootObj.getType().getId()) {
			return true;
		} else {
			return false;
		}
	}
	
	/**
	 * 指定のオブジェクトがごみ箱またはワークスペース固有ごみ箱に属するか否かを判定します。
	 *
	 * <li>指定のオブジェクトがごみ箱、またはごみ箱配下に存在する場合、trueを返します。
	 * <li>指定のオブジェクトがnullの場合はtrueを返します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param obj 指定のオブジェクト
	 * @return ごみ箱に属するか否か
	 * @throws Exception
	 */
	public static boolean isObjectInRecycleWithoutRecycle(EIMSession sess, EIMObject obj) throws Exception {

		if (obj == null) {
			return true;
		}
		EIMObjectType recycleObjType = ObjectUtils.getObjectTypeByName(sess,
				EIMConfig.get("OBJECT_TYPE_NAME_RECYCLE"));		// ごみ箱のオブジェクトタイプ

		EIMRelationType relType = RelationUtils.getRelationTypeByName(sess,
				EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));						// ドキュメントのリレーションタイプ
		EIMObject rootObj = ObjectUtils.getRootObject(sess, obj.getId(), relType);	// ルートオブジェクト

		// ワークスペース固有ごみ箱の判定
		EIMObject[] orgParentList = AppObjectUtil.getParentEIMObject(sess, obj, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));
		for (int i = 0; i < orgParentList.length; i++) {
			if(orgParentList[i].getType().getDefName().equals(EIMConfig.getValue("OBJECT_TYPE_NAME_WORKSPACERECYCLE"))){
				return true;
			}
		}

		// 指定のオブジェクトのルートに存在するオブジェクトがごみ箱の場合
		if (rootObj != null && recycleObjType.getId() == rootObj.getType().getId()) {
			return true;
		} else {
			return false;
		}
	}

	/**
	 * 指定のオブジェクトがワークスペースごみ箱に属するか否かを判定します。
	 *
	 * <li>指定のオブジェクトがごみ箱、またはごみ箱配下に存在する場合、trueを返します。
	 * <li>指定のオブジェクトがnullの場合もtrueを返します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param obj 指定のオブジェクト
	 * @return ごみ箱に属するか否か
	 * @throws Exception
	 */
	public static boolean isObjectInWsRecycle(EIMSession sess, EIMObject obj) throws Exception {
		if (obj == null) {
			return true;
		}
		// ワークスペース固有ごみ箱の判定
		EIMObject[] orgParentList = AppObjectUtil.getParentEIMObject(sess, obj, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));
		for (int i = 0; i < orgParentList.length; i++) {
			if(orgParentList[i].getType().getDefName().equals(EIMConfig.getValue("OBJECT_TYPE_NAME_WORKSPACERECYCLE"))){
				return true;
			}
		}
		return false;
	}
	
	/**
	 * 指定のオブジェクトがワークスペースごみ箱かどうかを判定します。
	 *
	 * <li>指定のオブジェクトがワークスペースごみ箱の場合、trueを返します。
	 * <li>指定のオブジェクトがnullの場合もtrueを返します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param obj 指定のオブジェクト
	 * @return ワークスペースごみ箱か否か
	 * @throws Exception
	 */
	public static boolean isWsRecycleObject(EIMSession sess, EIMObject obj) throws Exception {
		if (obj == null) {
			return true;
		}
		
		EIMObjectType wsRecycleObjType = ObjectUtils.getObjectTypeByName(sess,
				EIMConfig.get("OBJECT_TYPE_NAME_WORKSPACERECYCLE"));		// ワークスペースごみ箱のオブジェクトタイプ
		
		// 指定のオブジェクト自身がワークスペースごみ箱の場合
		if (obj.getType().getId() == wsRecycleObjType.getId()) {
			return true;
		}
		return false;
	}
	
	/**
	 * 指定のオブジェクトがごみ箱に属するか否かを判定します。
	 * ※ パス属性だけで判定する軽量版です
	 *
	 * <li>指定のオブジェクトがごみ箱、またはごみ箱配下に存在する場合、trueを返します。
	 * <li>指定のオブジェクトがnullの場合もtrueを返します。
	 *
	 * @param obj 指定のオブジェクト
	 * @return ごみ箱に属するか否か
	 * @throws Exception
	 */
	public static boolean isObjectInRecycleLite(EIMObject obj) throws Exception {

		if (obj == null) {
			return true;
		}

		String tmpPath = AppObjectUtil.getPath(obj);
		if( tmpPath == null ) {
			tmpPath = "/";	//ワークスペースの場合
		}
		//指定オブジェクトのパス属性と「/ごみ箱」を前文一致で比較する
		//「ごみ箱」の前文一致の結果を返却する
		return isPathInRecycle(tmpPath);
	}

	/**
	 * 指定のパスがごみ箱またはワークスペース固有ごみ箱に属するか否かを判定します。
	 *
	 * <li>指定のパスがごみ箱、またはごみ箱配下に存在する場合、trueを返します。
	 * <li>指定のパスがnullの場合もtrueを返します。
	 *
	 * @param path 指定のパス
	 * @return ごみ箱に属するか否か
	 * @throws Exception
	 */
	public static boolean isPathInRecycle(String path) throws Exception {

		if (path == null) {
			return true;
		}
		//「ごみ箱」の前文一致の結果を返却する
		if( path != null && path.startsWith("/" + EIMConfig.get("OBJECT_TYPE_NAME_RECYCLE") + "/") )
		{
			return true;
		}
		// 「/ワークスペース名/ごみ箱/」と第二階層目に「ごみ箱」の文字列が入っている結果を返却する
		String [] strs = path.split("/");
		if (strs.length >= 3) {
			String wsRecyclePath = strs[2];
			if (wsRecyclePath.equals(EIMConfig.get("OBJECT_TYPE_NAME_RECYCLE")))
			{
				return true;
			}
		}

		return false;
	}

	/**
	 * 指定のオブジェクトが指定の親オブジェクトに属するか否かを判定します。
	 *
	 * <li>指定のオブジェクトが指定の親オブジェクトに属する場合trueを返します。
	 * <li>指定の親オブジェクトがnullの場合はfalseを返します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param parentObjId 指定の親オブジェクトID
	 * @param childObjId 親オブジェクトに属するか判定するオブジェクトID
	 * @return 指定の親オブジェクトに属するか否か
	 * @throws Exception
	 */
	public static boolean isObjectInParent(EIMSession sess, long parentObjId, long childObjId) throws Exception {

		//子オブジェクトの取得
		EIMObject childObj = ObjectUtils.getObjectById(sess, childObjId);
		if(childObj == null)
		{
			return false;
		}

		//Parent Relation
		List parentRelList = new ArrayList();
		parentRelList = RelationUtils.getParentRelationList(sess, childObj, EIMAccessRole.READ);
		
		boolean isChildInParent = false;

		for(int i = 0; i < parentRelList.size(); i++)
		{
			//Relation
			EIMRelation relation = (EIMRelation)parentRelList.get(i);

			//Child Object
			EIMObject parentObj = relation.getParent();

			if(parentObj.getId() == parentObjId){
				isChildInParent = true;
				break;
			}
		}

		return isChildInParent;
	}

	/**
	 * 指定オブジェクトタイプ名・オブジェクト名のオブジェクト全てにおける、
	 * 指定属性タイプ名の属性をリストで返します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param objectTypeName 対象オブジェクト
	 * @param objectName 対象オブジェクト
	 * @param attrTypeName 対象属性タイプ
	 * @return 対象属性の値（String）のリスト
	 * @throws Exception
	 */
	public static List getStrAttrList(EIMSession sess, String objectTypeName, String objectName, String attrTypeName) throws Exception
	{
		List result = new ArrayList();
		List objectList = getObjectList(sess, objectTypeName, objectName);
		for (int i = 0 ; i < objectList.size() ; i++)
		{
			result.add(getStrAttr(sess, (EIMObject)objectList.get(i), attrTypeName));
		}
		return(result);
	}

	/**
	 * 指定ステータスの承認済ユーザ一覧を返します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param status 対象ステータス
	 * @return 承認済みユーザ（EIMUser）のリスト
	 * @throws Exception
	 */
	public static List getApprovedList(EIMSession sess, EIMStatus status) throws Exception
	{
		List dateList = new ArrayList();
		List commentList = new ArrayList();
		return(getApprovedList(sess, status, dateList, commentList));
	}

	/**
	 * 指定ステータスの承認済ユーザ一覧を承認日・コメントと共に返します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param status 対象ステータス
	 * @param dateList 承認日リスト返り値（Date）
	 * @param commentList コメントリスト返り値（String）
	 * @return 承認済みユーザ（EIMUser）のリスト
	 * @throws Exception
	 */
	public static List getApprovedList(EIMSession sess, EIMStatus status, List dateList, List commentList) throws Exception
	{
		List result = new ArrayList();
		dateList.clear();
		commentList.clear();
		List approvedList = getObjectList(sess, "承認者", String.valueOf(status.getId()));
		for (int i = 0 ; i < approvedList.size() ; i++)
		{
			result.add(UserUtils.getUserById(sess, getIntAttr(sess, (EIMObject)approvedList.get(i), "承認者", -1)));
			dateList.add(getDateAttr(sess, (EIMObject)approvedList.get(i), "承認日"));
			commentList.add(getTextAttr(sess, (EIMObject)approvedList.get(i), "コメント"));
		}
		return(result);
	}

	/**
	 * 指定ステータスの承認依頼者ユーザ一覧を返します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param status 対象ステータス
	 * @return 承認依頼者ユーザ（EIMUser）のリスト
	 * @throws Exception
	 */
	public static List getRequestList(EIMSession sess, EIMStatus status) throws Exception
	{
		List commentList = new ArrayList();
		return(getRequestList(sess, status, commentList));
	}

	/**
	 * 指定ステータスの承認依頼者ユーザ一覧をコメントと共に返します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param status 対象ステータス
	 * @param commentList コメントリスト返り値（String）
	 * @return 承認依頼者ユーザ（EIMUser）のリスト
	 * @throws Exception
	 */
	public static List getRequestList(EIMSession sess, EIMStatus status, List commentList) throws Exception
	{
		List result = new ArrayList();
		commentList.clear();
		List approveList = getObjectList(sess, "承認依頼者", String.valueOf(status.getId()));
		for (int i = 0 ; i < approveList.size() ; i++)
		{
			result.add(UserUtils.getUserById(sess, getIntAttr(sess, (EIMObject)approveList.get(i), "承認依頼者", -1)));
			commentList.add(getTextAttr(sess, (EIMObject)approveList.get(i), "コメント"));
		}
		return(result);
	}

	/**
	 * 指定オブジェクトの差戻しユーザ一覧を返します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param status 対象ステータス
	 * @return 差戻しユーザ（EIMUser）のリスト
	 * @throws Exception
	 */
	public static List getReturnList(EIMSession sess, EIMStatus status) throws Exception
	{
		List commentList = new ArrayList();
		return(getReturnList(sess, status, commentList));
	}

	/**
	 * 指定オブジェクトの差戻しユーザ一覧をコメントと共に返します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param status 対象ステータス
	 * @param commentList コメントリスト返り値（String）
	 * @return 差戻しユーザ（EIMUser）のリスト
	 * @throws Exception
	 */
	public static List getReturnList(EIMSession sess, EIMStatus status, List commentList) throws Exception
	{
		List result = new ArrayList();
		commentList.clear();
		List returnList = getObjectList(sess, "差戻し", String.valueOf(status.getId()));
		for (int i = 0 ; i < returnList.size() ; i++)
		{
			result.add(UserUtils.getUserById(sess, getIntAttr(sess, (EIMObject)returnList.get(i), "差戻しユーザ", -1)));
			commentList.add(getTextAttr(sess, (EIMObject)returnList.get(i), "コメント"));
		}
		return(result);
	}

	/**
	 * 指定ステータスの被承認依頼者ユーザ一覧を返します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param status 対象ステータス
	 * @return 被承認依頼者ユーザ（EIMUser）のリスト
	 * @throws Exception
	 */
	public static List getApproveList(EIMSession sess, EIMStatus status) throws Exception
	{
		List result = new ArrayList();
		List requestList = getObjectList(sess, "被承認依頼者", String.valueOf(status.getId()));
		for (int i = 0 ; i < requestList.size() ; i++)
		{
			result.addAll(ApproveCommonUtil.getUserFromCode(sess, getStrAttr(sess, (EIMObject)requestList.get(i), "被承認依頼者種別：ID")));
		}
		return(result);
	}

	/**
	 * 対象オブジェクトのワークフロー関連のオブジェクトを削除します。
	 * <br>
	 * 対象オブジェクトがnullの場合は何もしません。<br>
	 * 以下のオブジェクトを削除します。
	 * <li>承認依頼
	 * <li>承認依頼通知
	 * <li>公開通知
	 * <li>通知先
	 * <li>受信確認
	 *
	 * @param sess EIMSessionインスタンス
	 * @param object 対象オブジェクト
	 * @throws Exception
	 */
	public static void deleteWFRelatedObject(EIMSession sess, EIMObject object) throws Exception {

		if (object == null) {
			return;
		}

		String objId = String.valueOf(object.getId());
		//FIXME 下記4行はオブジェクトタイプ自体が存在しないため削除できる
		deleteObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_REQUEST"), objId);		//承認依頼
		deleteObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_REQUESTMAIL"), objId);	//承認依頼通知
		deleteObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_PUBLICMAIL"), objId);	//公開通知
		deleteObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_NOTIFY_TO"), objId);		//通知先
		//FIXME ここまで
		List receiveList = AppSearchUtils.searchObject(
			sess,
			ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_RECEIVE")),	//受信確認
			object.getId() + ".%",
			false,
			true,
			-1,
			null, null, null, null, null, null, null, null, null, null,EIMAccessRole.READ
		);
		for (int i = 0 ; i < receiveList.size() ; i++)
		{
			ObjectUtils.deleteObject(sess, (EIMObject)receiveList.get(i));
		}
	}

	/**
	 * 対象オブジェクトの受信確認オブジェクトを削除します。
	 * <p>
	 * 対象オブジェクトが{@code null}の場合は何もしません。
	 * </p>
	 *
	 * @param sess セッション
	 * @param object 対象オブジェクト
	 * @throws Exception
	 */
	public static void deleteReceiveObject(EIMSession sess, EIMObject object) throws Exception {

		if (object == null) {
			return;
		}

		String objIdStr = String.valueOf(object.getId());

		// 受信確認オブジェクトを削除
		List receiveList = AppSearchUtils.searchObject(
			sess,
			ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_RECEIVE")),	//受信確認
			objIdStr + ".%",
			false,
			true,
			-1,
			null, null, null, null, null, null, null, null, null, null, EIMAccessRole.READ
		);
		for (int i = 0; i < receiveList.size(); i++) {
			ObjectUtils.deleteObject(sess, (EIMObject)receiveList.get(i));
		}
	}

	/**
	 * 対象ドキュメントがPDF変換に失敗したオブジェクトか否かを判定します。
	 *
	 * <li>PDF変換失敗とする条件は、「ステータスが公開処理中」かつ「PDF変換オブジェクトが存在しない」です。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param object 対象ドキュメント
	 * @param objTypePDFConv PDF変換オブジェクトのオブジェクトタイプ
	 * @return PDF変換に失敗していればtrue、そうでなければfalse
	 * @throws Exception
	 */
	public static boolean isPDFConvertFailed(EIMSession sess, EIMObject object, EIMObjectType objTypePDFConv) throws Exception {

		if (object != null && objTypePDFConv != null) {
			// 公開処理中でなければ対象外
			if (object.getStatus().getType().getKind() == AppConstant.STATUS_TYPE_KIND_ID_PROCESSING_PUBLIC) {
				// PDF変換オブジェクトの取得
				EIMObject pdfObj = ObjectUtils.getObjectByTypeAndName(sess, objTypePDFConv, String.valueOf(object.getId()));
				if (pdfObj == null) {
					return true;
				}
			}
		}
		return false;
	}

	/**
	 * 比較元グループが探索対象グループに属するか否かを判定します。
	 *
	 * <li>再帰的に探索します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param baseGroup 比較元グループ
	 * @param targetGroup 探索対象グループ
	 * @return 比較元グループが探索対象グループに属する場合はtrue、属しない場合はfalse
	 * @throws Exception
	 */
	public static boolean isChildGroup(EIMSession sess, EIMGroup baseGroup, EIMGroup targetGroup) throws Exception {

		if (baseGroup != null && targetGroup != null) {
			// 子グループ一覧の取得
			List childGroupList = GroupUtils.getChildGroupList(sess, targetGroup);
			if (childGroupList != null && childGroupList.size() > 0) {
				for (Iterator childIter = childGroupList.iterator(); childIter.hasNext();) {
					EIMGroup childGroup = (EIMGroup) childIter.next();

					// 子グループとIDが一致したらtrueを返却、そうでない場合は再帰的に子グループを判定
					if (childGroup.getId() == baseGroup.getId() || isChildGroup(sess, baseGroup, childGroup)) {
						return true;
					}
				}
			}
		}
		return false;
	}

	/**
	 * 比較元ロールが探索対象ロールに属するか否かを判定します。
	 *
	 * <li>再帰的に探索します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param baseRole 比較元ロール
	 * @param targetRole 探索対象ロール
	 * @return 比較元ロールが探索対象ロールに属する場合はtrue、属しない場合はfalse
	 * @throws Exception
	 */
	public static boolean isChildRole(EIMSession sess, EIMRole baseRole, EIMRole targetRole) throws Exception {

		if (baseRole != null && targetRole != null) {
			// 子ロール一覧の取得
			List childRoleList = RoleUtils.getChildRoleList(sess, targetRole);
			if (childRoleList != null && childRoleList.size() > 0) {
				for (Iterator childIter = childRoleList.iterator(); childIter.hasNext();) {
					EIMRole childRole = (EIMRole) childIter.next();

					// 子ロールとIDが一致したらtrueを返却、そうでない場合は再帰的に子ロールを判定
					if (childRole.getId() == baseRole.getId() || isChildRole(sess, baseRole, childRole)) {
						return true;
					}
				}
			}
		}
		return false;
	}

	/**
	 * 比較元フォルダが探索対象フォルダに属するか否かを判定します。
	 *
	 * <li>再帰的に探索します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param baseFolder 比較元フォルダ
	 * @param targetFolder 探索対象フォルダ
	 * @param helper ヘルパーインスタンス
	 * @return 比較元フォルダが探索対象フォルダに属する場合はtrue、属しない場合はfalse
	 * @throws Exception
	 */
	public static boolean isChildFolder(EIMSession sess, EIMObject baseFolder, EIMObject targetFolder, AppObjectConditionHelper helper) throws Exception {

		// 対象がフォルダの場合のみ判定
		if (baseFolder != null && targetFolder != null
				&& targetFolder.getType().getDefName().equals(EIMConfig.get("OBJECT_TYPE_NAME_FOLDER"))) {

			// 子フォルダ一覧の取得
			List<EIMRelation> relation = helper.getChildRelationListByRelType(targetFolder, helper.getRelationTypeOfDocument(), EIMAccessRole.READ);
			EIMObject[] childFolders = relation.stream().map(rel -> rel.getChild()).collect(Collectors.toList()).toArray(new EIMObject[0]);
			if (childFolders.length > 0) {
				for (int i = 0 ; childFolders.length > i ; i++) {

					// 子フォルダとIDが一致したらtrueを返却、そうでない場合は再帰的に子フォルダを判定
					if (childFolders[i].getId() == baseFolder.getId() || isChildFolder(sess, baseFolder, childFolders[i], helper)) {
						return true;
					}
				}
			}
		}
		return false;
	}

	/**
	 * 指定したオブジェクトより下の階層に存在するドキュメントのステータスを更新します
	 *
	 * <li>再帰的に更新します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param parentObj 最上位のEIMObjectインスタンス
	 * @param status 更新するステータス
	 * @throws Exception
	 */
	public static void updateStatusRecurrently(EIMSession sess, EIMObject parentObj, EIMStatus status) throws Exception	{
		//初期設定
		List objectList = getChildEIMObjectRecurrently(sess, parentObj, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));
		for(int i = 0; i < objectList.size(); i++) {

			EIMObject childObj = (EIMObject)objectList.get(i);

			WorkFlowUtils.updateObjectStatus(sess, childObj, status);

		}

	}

	/**
	 * 指定したオブジェクトがWF付きフォルダかどうか識別します
	 *
	 * @param sess EIMSessionインスタンス
	 * @param object 識別対象のEIMObjectインスタンス
	 * @return WF付フォルダの場合true、それ以外の場合false
	 * @throws Exception
	 */
	public static boolean isWFFolder(EIMSession sess, EIMObject object) throws Exception {

		if (object.getStatus() == null) {
			//一般フォルダ
			return false;

		}

		long higherFolderFlg = AppObjectUtil.getIntAttr(sess, object, EIMConfig.get("ATTR_NAME_FOLDER_HIGHER_WF_FOLDER"), -1);
		if (higherFolderFlg != -1) {
			return false;
		}

		return true;
	}

	/**
	 * オブジェクト属性を作成します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param object 属性を付加するオブジェクト
	 * @param attr 属性値
	 * @throws Exception
	 */
	public static void setAttr(EIMSession sess, EIMObject object, EIMAttribute attr)
			throws Exception {
		switch (attr.getType().getValueType().getId()) {
		case EIMValueType.INTEGER:
			ObjectAttributeUtils.setAttribute(sess, object, attr.getType(), attr.getInts());
			break;
		case EIMValueType.DATE:
			ObjectAttributeUtils.setAttribute(sess, object, attr.getType(), attr.getDates());
			break;
		case EIMValueType.STRING:
			ObjectAttributeUtils.setAttribute(sess, object, attr.getType(), attr.getStrings());
			break;
		case EIMValueType.TEXT:
			ObjectAttributeUtils.setAttribute(sess, object, attr.getType(), attr.getTexts());
			break;
		case EIMValueType.DOUBLE:
			ObjectAttributeUtils.setAttribute(sess, object, attr.getType(), attr.getDoubles());
			break;
		}
	}

	/**
	 * オブジェクト名のリストを指定してオブジェクトを検索します。無効なオブジェクトも含まれます。
	 *
	 * <li>検索条件：オブジェクトタイプ、オブジェクト名が一致するもの
	 *
	 * @param sess セッション情報
	 * @param objTypeName オブジェクトタイプ名
	 * @param values オブジェクト名の配列
	 * @return 検索結果のオブジェクト(EIMObject)のリスト
	 * @throws Exception
	 */
	public static List searchObjectsByNames(EIMSession sess, String objTypeName, String[] values) throws Exception {

		// タイプの取得
		EIMObjectType objType = ObjectUtils.getObjectTypeByName(sess, objTypeName);

		// 検索条件の設定
		EIMSearchSelectEIMObject selectTarget = new EIMSearchSelectEIMObject();
		EIMSearchSelectEIMObject.SearchConditionBuildHelper h = new EIMSearchSelectEIMObject.SearchConditionBuildHelper();

		selectTarget.setCondition(h.group(h.opAnd())
				.addCondition(
						h.eq(h.opAnd(),
							 PsedoAttributeTypeEnum.TYPE,
							 objType.getId())
				)
				.addCondition(
						h.in(h.opAnd(),
							 PsedoAttributeTypeEnum.NAME,
							 h.opIn(),
							 values)
				)
		);

		// 検索の実行
		selectTarget.setRole(EIMAccessRole.READ);
		List resultList = SearchUtils.searchObjects(sess, selectTarget,
				new EIMSearchLimitCountCondition(EIMSearchLimitCountCondition.NOT_SPECIFIED, false));
		return resultList;
	}

	/**
	 * オブジェクトのリストからString型に変換されたオブジェクトIDの配列を抽出し返却する。
	 *
	 * @param objectList EIMObjectのリスト
	 * @return オブジェクトIDの配列
	 */
	public static String[] toObjIdStrArray(List objectList) {
		String[] names = new String[objectList.size()];

		// オブジェクトIDの取得
		for (int i = 0; i < names.length; i++)
		{
			EIMObject object = (EIMObject)objectList.get(i);
			names[i] = String.valueOf(object.getId());
		}

		return names;
	}

	/**
	 * オブジェクトのリストから指定された数値属性の配列を抽出し返却する。
	 *
	 * @param objectList EIMObjectのリスト
	 * @param attributeName 属性名
	 * @param inCaseNullNumber 属性値がnullの場合に返す値を設定します。(例:Integer.MIN_VALUE)
	 * @return 数値属性の配列
	 */
	public static String[] toIntAttributeStrArray(List objectList, String attributeName, int inCaseNullNumber) {
		String[] values = new String[objectList.size()];

		// 数値属性の取得
		for (int i = 0; i < values.length; i++) {
			EIMObject object = (EIMObject)objectList.get(i);
			EIMAttribute attribute = object.getAttribute(attributeName);
			if (attribute != null) {
				values[i] = String.valueOf(attribute.getInt());
			}else{
				values[i] = String.valueOf(inCaseNullNumber);
			}
		}

		return values;
	}

	/**
	 * オブジェクトのリストからString型に変換されたステータスIDの配列を抽出し返却する。
	 *
	 * @param objectList EIMObjectのリスト
	 * @return ステータスIDの配列
	 */
	public static String[] toStatusIdStrArray(List objectList) {
		String[] statusIds = new String[objectList.size()];

		// オブジェクトIDの取得
		for (int i = 0; i < statusIds.length; i++)
		{
			EIMObject object = (EIMObject)(objectList.get(i));
			EIMStatus status = object.getStatus();
			if(status != null){
				statusIds[i] = String.valueOf(status.getId());
			}
		}

		return statusIds;
	}

	/**
	 * ドキュメントもしくはフォルダオブジェクトからパスを取得し、返却する。
	 *
	 * <li>ワークスペースの場合はパスを保持しないため「null」を返却 (関数呼び出し元で期待されるパス属性値を用意すること)
	 *
	 * @param object ドキュメントもしくはフォルダオブジェクト
	 * @return パス
	 */
	public static String getPath(EIMObject object){

		String path = null;

		if (object.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_PASS")) != null){
			// 先頭のパス属性を返却する
			path = object.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_PASS")).getStrings()[0];
		}
		return path;
	}

	/**
	 * ドキュメントもしくはフォルダオブジェクトにパス属性を設定する。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param object ドキュメントもしくはフォルダオブジェクト
	 * @param path 設定するパス
	 * @throws Exception
	 */
	public static void setPath(EIMSession sess, EIMObject object, String path) throws Exception {

		String values[] = {path};

		if(path.equals("")){
			deleteAttribute(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_PASS"));
		} else {
			setAttrForce(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_PASS"), values);
		}
	}

	/**
	 * ドキュメントもしくはフォルダオブジェクトのパス属性(複数値)の先頭を入れ替える。
	 * ※ ドキュメントの全バージョンについて行う
	 *
	 * @param sess EIMSessionインスタンス
	 * @param object ドキュメントもしくはフォルダオブジェクト
	 * @param path 設定するパス
	 * @param helper ヘルパーインスタンス
	 * @throws Exception
	 */
	public static void replaceFirstPath(EIMSession sess, EIMObject object, String path, AppObjectConditionHelper helper) throws Exception {

		//バージョンを取得
		EIMVersion version = helper.getVersion(object);

		//バージョン分繰り返す
		EIMAttributeType attrTypePath = helper.getAttrType(EIMConfig.get("ATTR_NAME_DOCUMENT_PASS"));
		for (Iterator j = version.getList().iterator(); j.hasNext();) {
			EIMObject repObj = (EIMObject) j.next();
			EIMObjectType objType = repObj.getType();
			// 「パス」属性(複数値)を取得
			String values[] = helper.getStrAttrsPath(repObj);

			if( values == null ) {
				if( !path.equals("") ){
					values = new String[1];
					values[0] = path;
					// 「パス」属性(複数値)を更新
					helper.setAttrPath(repObj, values);

					// SearchFramework 更新通知は過去バージョンドキュメントのみ
					if( helper.isTypeOfDocument(objType) && j.hasNext() )
					{
						AppUpdateNoticeUtils.updateNoticeInsert(repObj.getId(), "SEARCHFW_UPDATE_PATH_DOCUMENT");
					}
				}
			}
			else {
				if( path.equals("") ) {
					deleteAttribute(sess, repObj, EIMConfig.get("ATTR_NAME_DOCUMENT_PASS"));
				}
				else {
					// パス属性の先頭のみ置き換える
					values[0] = path;
					// 「パス」属性(複数値)を更新
					helper.setAttrPath(repObj, values);
				}
				// SearchFramework 更新通知は過去バージョンドキュメントのみ
				if( helper.isTypeOfDocument(objType) && j.hasNext() )
				{
					AppUpdateNoticeUtils.updateNoticeInsert(repObj.getId(), "SEARCHFW_UPDATE_PATH_DOCUMENT");
				}
			}
		}
	}

	/**
	 * ドキュメント、フォルダに削除日を設定する。
	 * ※ ドキュメントの全バージョンについて行う
	 * @param sess EIMSessionインスタンス
	 * @param object ドキュメントもしくはフォルダオブジェクト
	 * @param date 設定する削除日時
	 * @param helper ヘルパーインスタンス
	 * @throws Exception
	 */
	public static void setAttrDeleteDate(EIMSession sess, EIMObject object, Date date, AppObjectConditionHelper helper) throws Exception {

		//バージョンを取得
		EIMVersion version = helper.getVersion(object);

		//バージョン分繰り返す
		for (Iterator j = version.getList().iterator(); j.hasNext();) {
			EIMObject repObj = (EIMObject) j.next();
			// 削除日時を設定
			AppObjectUtil.setAttr(sess, repObj, EIMConfig.get("ATTR_NAME_DOCUMENT_DELETE_DATE"), date);
		}
	}

	/**
	 * ドキュメント、フォルダから属性:削除日を削除する。
	 * ※ ドキュメントの全バージョンについて行う
	 * @param sess EIMSessionインスタンス
	 * @param object ドキュメントもしくはフォルダオブジェクト
	 * @param helper ヘルパーインスタンス
	 * @throws Exception
	 */
	public static void deleteAttrDeleteDate(EIMSession sess, EIMObject object, AppObjectConditionHelper helper) throws Exception {

		//バージョンを取得
		EIMVersion version = helper.getVersion(object);

		//バージョン分繰り返す
		for (Iterator j = version.getList().iterator(); j.hasNext();) {
			EIMObject repObj = (EIMObject) j.next();
			// 削除日時を削除
			AppObjectUtil.deleteAttribute(sess, repObj, EIMConfig.get("ATTR_NAME_DOCUMENT_DELETE_DATE"));
		}
	}

	/**
	 * フルパスを元にオブジェクトを取得します。
	 *
	 * <li>フルパス文字列、タイプ文字列が不正な場合はnullを返します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param fullpath パス
	 * @param strObjType オブジェクトのタイプ("ドキュメント"、"フォルダ"、"タグ"の三択)
	 * @return 取得したオブジェクト
	 * @throws Exception
	 */
	public static EIMObject getObjListByFullPass(EIMSession sess, String fullpath, String strObjType) throws Exception
	{
		// 最初の文字が「/」で無かったり、長さが2以上で無い場合は検索の対象外(フルパス不正)
		if( fullpath == null || fullpath.length() < 2 || fullpath.charAt(0) != '/') {
			return null;
		}

		// 検索条件ヘルパー生成
		EIMSearchSelectEIMObject.SearchConditionBuildHelper h = new EIMSearchSelectEIMObject.SearchConditionBuildHelper();

		// 検索条件グループ作成
		EIMSearchConditionGroup conds = h.group(h.opAnd());

		conds.addCondition( h.latest(h.opAnd()) );	// latest条件

		// 分離したパスとオブジェクト名で検索を行う
		// パスの取得
		String path = getPathFromFullPath(fullpath);
		// オブジェクト名の取得
		String name = getNameFromFullPath(fullpath);

		// 検索条件生成
		conds = getObjListByFullPassCondition(sess, path, name, strObjType, conds, h);
		if (conds == null) {
			return null;
		}

		// 検索条件項目・返却項目指定インスタンスに条件グループを設定
		EIMSearchSelectEIMObject selectTarget = new EIMSearchSelectEIMObject();
		selectTarget.setCondition(conds);

		// 検索実行
		selectTarget.setRole(EIMAccessRole.READ);
		List result = SearchUtils.searchObjects(sess, selectTarget, new EIMSearchLimitCountCondition(SearchUtils.NOT_SPECIFIED, true));

		// 検索結果
		EIMObject retObj = null;
		if( result != null && result.size() > 1 ) {	//Hitしたが複数Hitしてしまった場合(リンクのあるドキュメントを想定)
			for (Iterator i = result.iterator(); i.hasNext();) {
				EIMObject tmpObj = (EIMObject) i.next();
				String tmpPath = getPath(tmpObj);
				if( tmpPath == null ) {
					tmpPath = "/";	//ワークスペースの場合
				}
				if( tmpPath.equals(path) ) {	//パス属性の先頭が合致するものを採用
					retObj = tmpObj;
					break;
				}
			}
		}
		else if( result != null && result.size() == 1 ) {
			retObj = (EIMObject) result.get(0);
		}

		return retObj;
	}

	/**
	 * 複数フルパスを元に複数オブジェクトを取得します。
	 *
	 * <li>フルパス文字列、タイプ文字列が不正な場合はnullを返します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param fullpath パスの配列
	 * @param strObjType オブジェクトのタイプ("ドキュメント"、"フォルダ"、"タグ"の三択)の配列
	 * @return 取得したオブジェクトのリスト
	 * @throws Exception
	 */
	public static List getObjListByFullPass(EIMSession sess, String[] fullpath, String[] strObjType) throws Exception {

		HashSet pathNameSet = new HashSet();	// <パス> + <オブジェクト名>の文字列格納用

		// フルパス文字列、タイプ文字列がない、または個数が異なる場合
		if (fullpath == null || fullpath.length == 0
				|| strObjType == null  || strObjType.length == 0
				|| fullpath.length != strObjType.length) {
			return null;
		}

		for (int i = 0 ; i < fullpath.length ; i++) {
			// 最初の文字が「/」で無かったり、長さが2以上で無い場合は検索の対象外(フルパス不正)
			if( fullpath[i] == null || fullpath[i].length() < 2 || fullpath[i].charAt(0) != '/') {
				return null;
			}
		}

		// 検索条件ヘルパー生成
		EIMSearchSelectEIMObject.SearchConditionBuildHelper h = new EIMSearchSelectEIMObject.SearchConditionBuildHelper();

		// 検索条件グループ作成
		EIMSearchConditionGroup conds = h.group(h.opAnd());

		conds.addCondition( h.latest(h.opAnd()) );	// latest条件

		// 検索条件グループ2作成
		EIMSearchConditionGroup conds2 = h.group(h.opAnd());

		// 検索条件生成
		for (int i = 0 ; i < fullpath.length ; i++) {
			// フルパス毎の検索条件グループ
			EIMSearchConditionGroup condsPath = null;
			if (i == 0) {
				condsPath = h.group(h.opAnd());
			} else {
				condsPath = h.group(h.opOr());
			}

			// 分離したパスとオブジェクト名で検索を行う
			// パスの取得
			String path = getPathFromFullPath(fullpath[i]);
			// オブジェクト名の取得
			String name = getNameFromFullPath(fullpath[i]);

			pathNameSet.add(path + name);

			// 検索条件生成
			condsPath = getObjListByFullPassCondition(sess, path, name, strObjType[i], condsPath, h);
			if (condsPath == null) {
				return null;
			}

			// 検索条件の結合
			conds2.addCondition(condsPath);
		}
		// 最終的な検索条件が完成  「latest条件 and ((パス条件) or (パス条件) or …)」
		conds.addCondition(conds2);

		// 検索条件項目・返却項目指定インスタンスに条件グループを設定
		EIMSearchSelectEIMObject selectTarget = new EIMSearchSelectEIMObject();
		selectTarget.setCondition(conds);

		// 検索実行
		selectTarget.setRole(EIMAccessRole.READ);
		List result = SearchUtils.searchObjects(sess, selectTarget, new EIMSearchLimitCountCondition(SearchUtils.NOT_SPECIFIED, true));

		// 検索結果
		List retList = new ArrayList();
		if (result != null && result.size() != 0) {
			// ドキュメントリンク用のパスで該当したものは除外する
			for (Iterator i = result.iterator(); i.hasNext();) {
				EIMObject tmpObj = (EIMObject) i.next();
				String tmpPath = getPath(tmpObj);
				if( tmpPath == null ) {
					tmpPath = "/";	//ワークスペースの場合
				}
				if (pathNameSet.contains(tmpPath+ tmpObj.getName())) {
					retList.add(tmpObj);
				}
			}
		}
		return retList;
	}

	/**
	 * 複数フルパスを元に複数オブジェクトを取得し、フルパスをキーとしたHashMapを返却する
	 *
	 * <li>フルパス文字列、タイプ文字列が不正な場合はnullを返します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param fullpath パスの配列
	 * @return 取得したオブジェクトのマップ
	 * @throws Exception
	 */
	public static HashMap getFolderObjMapByFullPath(EIMSession sess, String[] fullpath) throws Exception
	{
		HashSet pathNameSet = new HashSet();	// <パス> + <オブジェクト名>の文字列格納用

		// フルパス文字列ない場合
		if (fullpath == null || fullpath.length == 0) {
			return null;
		}

		for (int i = 0 ; i < fullpath.length ; i++) {
			// 最初の文字が「/」で無かったり、長さが2以上で無い場合は検索の対象外(フルパス不正)
			if( fullpath[i] == null || fullpath[i].length() < 2 || fullpath[i].charAt(0) != '/') {
				return null;
			}
		}

		// 検索条件ヘルパー生成
		EIMSearchSelectEIMObject.SearchConditionBuildHelper h = new EIMSearchSelectEIMObject.SearchConditionBuildHelper();

		// 検索条件グループ作成
		EIMSearchConditionGroup conds = h.group(h.opAnd());

		conds.addCondition( h.latest(h.opAnd()) );	// latest条件

		// 検索条件グループ2作成
		EIMSearchConditionGroup conds2 = h.group(h.opAnd());

		// オブジェクトタイプは必ず "フォルダ"

		// 検索条件生成
		String FOLDER_TYPE = EIMConfig.get("OBJECT_TYPE_NAME_FOLDER");
		for (int i = 0 ; i < fullpath.length ; i++) {
			// 分離したパスとオブジェクト名で検索を行う
			// パスの取得
			String path = getPathFromFullPath(fullpath[i]);
			// オブジェクト名の取得
			String name = getNameFromFullPath(fullpath[i]);

			if(pathNameSet.contains(path + name)) {
				continue;
			}
			else {
				pathNameSet.add(path + name);
			}

			// フルパス毎の検索条件グループ
			EIMSearchConditionGroup condsPath = null;
			if (i == 0) {
				condsPath = h.group(h.opAnd());
			} else {
				condsPath = h.group(h.opOr());
			}

			// 検索条件生成
			condsPath = getObjListByFullPassCondition(sess, path, name, FOLDER_TYPE, condsPath, h);
			if (condsPath == null) {
				return null;
			}

			// 検索条件の結合
			conds2.addCondition(condsPath);
		}
		// 最終的な検索条件が完成  「latest条件 and ((パス条件) or (パス条件) or …)」
		conds.addCondition(conds2);

		// 検索条件項目・返却項目指定インスタンスに条件グループを設定
		EIMSearchSelectEIMObject selectTarget = new EIMSearchSelectEIMObject();
		selectTarget.setCondition(conds);

		// 検索実行
		selectTarget.setRole(EIMAccessRole.READ);
		List result = SearchUtils.searchObjects(sess, selectTarget, new EIMSearchLimitCountCondition(SearchUtils.NOT_SPECIFIED, true));

		// 検索結果
		HashMap retMap = new HashMap();
		if (result != null && result.size() != 0) {
			// ドキュメントリンク用のパスで該当したものは除外する
			for (Iterator i = result.iterator(); i.hasNext();) {
				EIMObject tmpObj = (EIMObject) i.next();
				String tmpPath = getPath(tmpObj);
				if( tmpPath == null ) {
					tmpPath = "/";	//ワークスペースの場合
				}
				if (pathNameSet.contains(tmpPath+ tmpObj.getName())) {
					retMap.put(tmpPath+ tmpObj.getName(), tmpObj);
				}
			}
		}
		return retMap;
	}

	/**
	 * リスト値に指定属性値が含まれているかどうかチェックします。
	 * trueの場合：リスト値に含まれている、またはリスト定義されていない
	 * falseの場合：リスト定義されており、かつリスト値に含まれない
	 * @param sess EIMSessionインスタンス
	 * @param type 属性タイプ
	 * @param value 属性値
	 */
	public static boolean doesExistAttrValueInMasterValues(EIMSession sess, EIMAttributeType type, String value) throws Exception
	{
		EIMObject object = AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRMST"), String.valueOf(type.getId()));

		if(object == null) {
			return true;
		}

		switch(type.getValueType().getId()) {
			case EIMValueType.STRING:
				String[] mstStrArray = AppObjectUtil.getStrAttrs(sess, object, EIMConfig.get("ATTR_NAME_ATTRMST_ATTR_STR_LIST"));
				if(mstStrArray != null) {
					int iii;
					for(iii = 0; iii < mstStrArray.length; iii++) {
						if(mstStrArray[iii].equals(value))
							break;
					}
					if(iii == mstStrArray.length) {
						return false;
					}
				}
				break;
			case EIMValueType.TEXT:
				String[] mstTextArray = AppObjectUtil.getTextAttrs(sess, object, EIMConfig.get("ATTR_NAME_ATTRMST_ATTR_TEXT_LIST"));
				if(mstTextArray != null) {
					int iii;
					for(iii = 0; iii < mstTextArray.length; iii++) {
						if(StringUtils.convertReturnCede(mstTextArray[iii]).equals(StringUtils.convertReturnCede(value)))
							break;
					}
					if(iii == mstTextArray.length) {
						return false;
					}
				}
				break;
			case EIMValueType.INTEGER:
				int iVal = Integer.parseInt(value);
				long[] mstIntArray = AppObjectUtil.getIntAttrs(sess, object, EIMConfig.get("ATTR_NAME_ATTRMST_ATTR_NUM_LIST"));
				if(mstIntArray != null) {
					int iii;
					for(iii = 0; iii < mstIntArray.length; iii++) {
						if(mstIntArray[iii] == iVal)
							break;
					}
					if(iii == mstIntArray.length) {
						return false;
					}
				}
				break;
			case EIMValueType.DATE:
				Date dVal = DateUtils.editExpirationDate(sess, StringUtils.getDateFromString(value,
						EIMResource.getMessage(sess, "EIM.FORMAT.DATE")));
				Date[] mstDateArray = AppObjectUtil.getDateAttrs(sess, object, EIMConfig.get("ATTR_NAME_ATTRMST_ATTR_DATE_LIST"));
				if(mstDateArray != null) {
					int iii;
					for(iii = 0; iii < mstDateArray.length; iii++) {
						if(mstDateArray[iii].getTime() == dVal.getTime())
							break;
					}
					if(iii == mstDateArray.length) {
						return false;
					}
				}
				break;
			case EIMValueType.DOUBLE:
				double iVal2 = Double.parseDouble(value);
				double[] mstDoubleArray = AppObjectUtil.getDoubleAttrs(sess, object, EIMConfig.get("ATTR_NAME_ATTRMST_ATTR_DOUBLE_LIST"));
				if(mstDoubleArray != null) {
					int iii;
					for(iii = 0; iii < mstDoubleArray.length; iii++) {
						if(mstDoubleArray[iii] == iVal2)
							break;
					}
					if(iii == mstDoubleArray.length) {
						return false;
					}
				}
				break;
		}

		return true;
	}

	/**
	 * オブジェクト名とパス属性を検索条件に追加したEIMSearchConditionGroupクラスを返します。
	 *
	 * <li>タイプ指定が不正な場合はnullを返します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param path パス属性の値
	 * @param name オブジェクト名
	 * @param strObjType オブジェクトのタイプ("ドキュメント"、"フォルダ"、"タグ"の三択)
	 * @param conds 検索条件を追加するEIMSearchConditionGroupクラス
	 * @param h 検索条件ヘルパー生成
	 * @return オブジェクト名とパス属性を検索条件に追加したEIMSearchConditionGroupクラス
	 * @throws Exception
	 */
	private static EIMSearchConditionGroup getObjListByFullPassCondition(EIMSession sess, String path, String name, String strObjType,
			EIMSearchConditionGroup conds, EIMSearchSelectEIMObject.SearchConditionBuildHelper h) throws Exception {

		EIMObjectType objType = null;
		//ドキュメントタイプが指定された場合
		if( strObjType.equals(EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT")) ||
				strObjType.equals(EIMConfig.get("OBJECT_TYPE_NAME_TAG")) ) {
			// 対象はオブジェクトタイプが「ドキュメント」以下のもの
			objType = ObjectUtils.getObjectTypeByName(sess, strObjType);
			EIMSearchConditionIn objTypeCond = h.eqObjTypeWithSubClasses(h.opAnd(),objType.getId(), sess);
			conds.addCondition(objTypeCond);
			// 名称条件
			EIMSearchConditionCompare objNameCond = h.eq(h.opAnd(), EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.NAME, name);
			conds.addCondition(objNameCond);
			// パス条件
			EIMSearchConditionCompare pathCond = h.eq(h.opAnd(), AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_PASS")), path);
			conds.addCondition(pathCond);
		}
		//フォルダタイプが指定された場合
		else if ( strObjType.equals(EIMConfig.get("OBJECT_TYPE_NAME_FOLDER")) ) {
			if( !path.equals("/") ) {
				// 対象はオブジェクトタイプが「フォルダ」以下のもの
				objType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_FOLDER"));
				EIMSearchConditionIn objTypeCond = h.eqObjTypeWithSubClasses(h.opAnd(),objType.getId(), sess);
				conds.addCondition(objTypeCond);
				// 名称条件
				EIMSearchConditionCompare objNameCond = h.eq(h.opAnd(), EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.NAME, name);
				conds.addCondition(objNameCond);
				// パス条件
				EIMSearchConditionCompare pathCond = h.eq(h.opAnd(), AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_PASS")), path);
				conds.addCondition(pathCond);
			}
			else
			{
				// 対象はオブジェクトタイプが「ワークスペース」のもの
				objType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_WORKSPACE"));
				EIMSearchConditionCompare objTypeCond = h.eq(h.opAnd(), EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.TYPE, objType.getId());
				conds.addCondition(objTypeCond);
				// 名称条件
				EIMSearchConditionCompare objNameCond = h.eq(h.opAnd(), EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.NAME, name);
				conds.addCondition(objNameCond);
			}
		}
		else {	//タイプ指定が不正
			return null;
		}

		return conds;
	}

	/**
	 * フルパスの文字列からパスを取得します。
	 *
	 * <li>文字列末尾の<オブジェクト名>を除去します。
	 * <li>フルパス文字列は事前にチェック済みのものとします。
	 *
	 * @param fullpath フルパス文字列
	 * @return パス
	 */
	private static String getPathFromFullPath(String fullpath) {

		// 最後の「/」は除去
		if( fullpath.charAt(fullpath.length() - 1) == '/' ) {
			fullpath = fullpath.substring(0, fullpath.length() - 1);
		}

		int pos = fullpath.lastIndexOf("/");
		String path = "";

		if( pos > -1 ) {
			path = fullpath.substring(0, pos);
		}

		if (!path.endsWith("/")) {
			path += "/";
		}
		return path;
	}

	/**
	 * フルパス文字列からオブジェクト名を取得します。
	 *
	 * <li>フルパス文字列は事前にチェック済みのものとします。
	 *
	 * @param fullpath フルパス文字列
	 * @return オブジェクト名
	 */
	private static String getNameFromFullPath(String fullpath) {

		// 最後の「/」は除去
		if( fullpath.charAt(fullpath.length() - 1) == '/' ) {
			fullpath = fullpath.substring(0, fullpath.length() - 1);
		}
		return fullpath.substring(fullpath.lastIndexOf("/") + 1);
	}

	//WFPUB
	/**
	 * ワークフロー公開処理オブジェクトを取得します。
	 *
	 */
	public static EIMObject getWorkFlowProcessing(EIMSession sess, EIMObject object) throws Exception {
		EIMObject wfpubObj = null;
		EIMWorkFlow workflow = WorkFlowUtils.getWorkFlowByStatusType(sess, object.getStatus().getType());
		List statusTypeList = workflow.getStatusTypeList();

		for(int i = 0; i < statusTypeList.size(); i++){
			EIMStatusType statusType = (EIMStatusType)statusTypeList.get(i);
			if(statusType.getKind() == AppConstant.STATUS_TYPE_KIND_ID_PROCESSING_PUBLIC){
				wfpubObj = AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_WFPUB"), String.valueOf(statusType.getId()));
				break;
			}
		}

		return wfpubObj;
	}

	/**
	 * 対象オブジェクトを行ロックします。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param objIds 行ロックするオブジェクトのIDの配列
	 * @throws Exception
	 */
	public static void lockObjectById(EIMSession sess, long[] objIds) throws Exception {

		// SearchUtils.searchObjects()を使用して、排他ロックをかける
		EIMSearchSelectEIMObject selectTarget = new EIMSearchSelectEIMObject();
		selectTarget.setLockMode(EIMConstant.LockMode.ROWSHARE);
		EIMSearchConditionIn cond = new EIMSearchConditionIn(
				EIMSearchOperatorEnum.OR
				, EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.ID
				, EIMSearchOperatorEnum.IN
				, TypeConvertUtils.convertToBuildTypeArray(objIds));
		EIMSearchConditionGroup conds = new EIMSearchConditionGroup(EIMSearchOperatorEnum.OR);
		conds.addCondition(cond);
		selectTarget.setCondition(conds);
		List<EIMAttributeType> resultAttrs = new ArrayList<EIMAttributeType>();
		selectTarget.setResultAttrs(resultAttrs);
		EIMSearchResultList list = SearchUtils.searchObjects(sess, selectTarget, new EIMSearchLimitCountCondition(EIMSearchLimitCountCondition.NOT_SPECIFIED, false));
	}

	/**
	 * ドキュメント管理用
	 *		ワークフロー設定オブジェクトをコピーします。
	 * @param sess
	 * @param srcWorkflowId		コピー元ワークフローID
	 * @param destWorkflowId	コピー先ワークフローID
	 * @throws Exception
	 */
	public static void copyDocWorkFlowSettingObject(EIMSession sess, long srcWorkflowId, long destWorkflowId) throws Exception {

		//Update Work Flow Setting Object
		EIMObject srcObj = AppObjectUtil.getObject(sess,
											EIMConfig.get("OBJECT_TYPE_NAME_WFSETTING"),
											String.valueOf(srcWorkflowId));

		EIMObject destObj = AppObjectUtil.createObject(sess,
											EIMConfig.get("OBJECT_TYPE_NAME_WFSETTING"),
											String.valueOf(destWorkflowId));


		ObjectAttributeUtils.inheritAttribute(sess, srcObj, destObj);
	}

	/**
	 * ドキュメント管理用
	 *		ワークフロー設定オブジェクト(ステータス毎に設定されている属性)をコピーします。
	 * @param sess セッション
	 * @param srcWorkflowDomain　コピー元ワークフロードメイン
	 * @param destWorkflowDomain　コピー先ワークフロードメイン
	 * @throws Exception
	 */
	public static void copyDocWorkFlowSettingObjectForStatusType(EIMSession sess, WorkflowDomain srcWorkflowDomain, WorkflowDomain destWorkflowDomain) throws Exception {
		// コピー元のワークフロー設定情報取得
		EIMObject srcWorkFlowSettingObj = AppObjectUtil.getObject(sess,EIMConfig.get("OBJECT_TYPE_NAME_WFSETTING"),String.valueOf(srcWorkflowDomain.getId()));

		EIMObject destWorkFlowSettingObj = AppObjectUtil.getObject(sess,EIMConfig.get("OBJECT_TYPE_NAME_WFSETTING"),String.valueOf(destWorkflowDomain.getId()));

		// ステータスタイプリストがのため再取得
		EIMWorkFlow destworkFlow = WorkFlowUtils.getWorkFlowById(sess, destWorkflowDomain.getId());

		HashSet<Long> srcEnableCheckinStatusTypeSet = new HashSet<Long>();
		HashSet<Long> srcBossOnlyStatusTypeSet = new HashSet<Long>();

		EIMAttribute srcEnableCheckinAttribute  =  srcWorkFlowSettingObj.getAttribute(EIMConfig.get("ATTR_NAME_ENABLE_CHECKIN_STATUS"));
		EIMAttribute srcBossOnlyAttribute  =  srcWorkFlowSettingObj.getAttribute(EIMConfig.get("ATTR_NAME_BOSS_ONLY_DEFAULT_STATUS"));

		// 承認依頼中チェックイン可否情報取得
		long[] newEnableCheckinStatusTypeArr = null;
		if( OptionConfData.getInstance().enableApproverCheckin && srcEnableCheckinAttribute!= null) {
			long[] srcEnableCheckinStatusTypeArr = TypeConvertUtils.convertToLongArray(srcEnableCheckinAttribute.getInts());
			newEnableCheckinStatusTypeArr = new long[srcEnableCheckinStatusTypeArr.length];

			for(long statusTypeId : srcEnableCheckinStatusTypeArr) {
				srcEnableCheckinStatusTypeSet.add(statusTypeId);
			}
		}
		// 上長のみ表示設定情報取得
		long[] newBossOnlyStatusTypeArr = null;
		if ( srcBossOnlyAttribute != null ) {
			long[] srcBossOnlyStatusTypeArr = TypeConvertUtils.convertToLongArray(srcBossOnlyAttribute.getInts());
			newBossOnlyStatusTypeArr = new long[srcBossOnlyStatusTypeArr.length];

			for(long statusTypeId : srcBossOnlyStatusTypeArr) {
				srcBossOnlyStatusTypeSet.add(statusTypeId);
			}
		}

		// コピー先のステータスタイプIDに変換
		int newEnableCheckinCnt = 0;
		int newBossOnlyCnt = 0;
		for(int i = 0; i < srcWorkflowDomain.getStatusTypeList().size(); i++ ) {
			 jp.co.ctc_g.eim.framework2.business.domain.entity.StatusTypeDomain statusTypeDomain = srcWorkflowDomain.getStatusTypeList().get(i);
			// 承認依頼中チェックイン可否複製
			if( srcEnableCheckinStatusTypeSet.contains((long)statusTypeDomain.getId()) ) {
				newEnableCheckinStatusTypeArr[newEnableCheckinCnt] = ((EIMStatusType)destworkFlow.getStatusTypeList().get(i)).getId();
				newEnableCheckinCnt++;
			}

			// 上長のみ表示設定複製
			if( srcBossOnlyStatusTypeSet.contains((long)statusTypeDomain.getId()) ) {
				newBossOnlyStatusTypeArr[newBossOnlyCnt] = ((EIMStatusType)destworkFlow.getStatusTypeList().get(i)).getId();
				newBossOnlyCnt++;
			}
		}

		// 属性更新
		AppObjectUtil.setAttr(sess,
				destWorkFlowSettingObj,
				EIMConfig.get("ATTR_NAME_ENABLE_CHECKIN_STATUS"),
				newEnableCheckinStatusTypeArr);

		AppObjectUtil.setAttr(sess,
				destWorkFlowSettingObj,
				EIMConfig.get("ATTR_NAME_BOSS_ONLY_DEFAULT_STATUS"),
				newBossOnlyStatusTypeArr);
	}

	/**
	 * ドキュメント管理用
	 *		公開通知先エントリーオブジェクトをコピーします。
	 * @param sess
	 * @param srcWorkflowId		コピー元ワークフローID
	 * @param destWorkflowId	コピー先ワークフローID
	 * @return
	 * @throws Exception
	 */
	public static void copyDocWorkflowPublishNotifyObject(EIMSession sess, long srcWorkflowId, long destWorkflowId) throws Exception {

		//Update Work Flow Setting Object
		EIMObject srcObj = AppObjectUtil.getObject(sess,
											EIMConfig.get("OBJECT_TYPE_NAME_WFPUBNOTIFY"),
											String.valueOf(srcWorkflowId));

		EIMObject destObj = AppObjectUtil.createObject(sess,
											EIMConfig.get("OBJECT_TYPE_NAME_WFPUBNOTIFY"),
											String.valueOf(destWorkflowId));

		ObjectAttributeUtils.inheritAttribute(sess, srcObj, destObj);

	}


	/**
	 * ドキュメント管理用
	 *		ワークフロー公開処理オブジェクトをコピーします。
	 * @param sess
	 * @param srcWorkflowId		コピー元ワークフローID
	 * @param destWorkflowId	コピー先ワークフローID
	 * @return
	 * @throws Exception
	 */
	public static void copyDocWorkflowPublishObject(EIMSession sess, long srcWorkflowId, long destWorkflowId, boolean createFlag) throws Exception {

		// 「公開」ステータスタイプを取得する
		EIMWorkFlow srcWorkflow = WorkFlowUtils.getWorkFlowById(sess, srcWorkflowId);
		if(srcWorkflow == null) {
			// Error ワークフローが取得できません。
			throw new EIMException(sess, "EIM.ERROR.LOGIC.WORKFLOW.NOTFOUND");
		}
		List<EIMStatusType> srcStatusTypeList = srcWorkflow.getStatusTypeList();

		EIMStatusType srcStatusType = null;
		// ステータスタイプ｢公開処理中｣を取得
		for(EIMStatusType stty:srcStatusTypeList) {
			if(stty.getKind() == AppConstant.STATUS_TYPE_KIND_ID_PROCESSING_PUBLIC) {
				srcStatusType = stty;
				break;
			}
		}
		if(srcStatusType==null) {
			// Error ステータスタイプが取得できません。
			throw new EIMException(sess, "EIM.ERROR.LOGIC.STATUSTYPE.NOTFOUND");
		}


		EIMWorkFlow destWorkflow = WorkFlowUtils.getWorkFlowById(sess, destWorkflowId);
		if(destWorkflow == null) {
			// Error ワークフローが取得できません。
			throw new EIMException(sess, "EIM.ERROR.LOGIC.WORKFLOW.NOTFOUND");
		}
		List<EIMStatusType> destStatusTypeList = destWorkflow.getStatusTypeList();

		EIMStatusType destStatusType = null;
		// ステータスタイプ｢公開処理中｣を取得
		for(EIMStatusType stty:destStatusTypeList) {
			if(stty.getKind() == AppConstant.STATUS_TYPE_KIND_ID_PROCESSING_PUBLIC) {
				destStatusType = stty;
				break;
			}
		}
		if(destStatusType==null) {
			// Error ステータスタイプが取得できません。
			throw new EIMException(sess, "EIM.ERROR.LOGIC.STTYPE.NOTFOUNDD");
		}

		EIMObject srcObj = AppObjectUtil.getObject(sess,
											EIMConfig.get("OBJECT_TYPE_NAME_WFPUB"),
											String.valueOf(srcStatusType.getId()));

		EIMObject destObj = null;
		if(createFlag) {
			destObj = AppObjectUtil.createObject(sess,
					EIMConfig.get("OBJECT_TYPE_NAME_WFPUB"),
					String.valueOf(destStatusType.getId()));
		}else{
			destObj = AppObjectUtil.getObject(sess,
					EIMConfig.get("OBJECT_TYPE_NAME_WFPUB"),
					String.valueOf(destStatusType.getId()));
		}

		ObjectAttributeUtils.inheritAttribute(sess, srcObj, destObj);

	}

	/**
	 * EIMObjectのリストからオブジェクトIDのリストを返却します。
	 * @param objectList EIMObjectのリスト
	 * @return オブジェクトIDのリスト
	 */
	public static List<Long> getIdList(List<EIMObject> objectList) {
		List<Long> idList = new ArrayList<Long>();
		for (int i = 0; i < objectList.size(); i++) {
			idList.add((long)objectList.get(i).getId());
		}

		return idList;
	}

	/**
	 * 指定オブジェクトが公開処理失敗ドキュメントかどうかを返却します。
	 * @param object 判定対象のオブジェクト（公開処理失敗属性、PDF変換処理実行日時属性が設定されていること）
	 * @return 公開処理失敗ドキュメントの場合true
	 */
	public static boolean isProcFailDocument(EIMObject object) throws Exception
	{
		// 「公開処理失敗」取得
		EIMAttribute attrPubProcFail = object.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_PUB_PROC_FAIL"));
		if(attrPubProcFail == null || attrPubProcFail.getInt() == 0) {
			return false;
		}

		// 「PDF変換処理実行日時」取得
		EIMAttribute attrPDFConvExecDate = object.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_PDF_CONV_EXEC_DATE"));
		if (attrPDFConvExecDate != null && attrPDFConvExecDate.getDate() != null && attrPDFConvExecDate.getDate().getTime() <= object.getModifyDate().getTime()) {
			return false;
		}

		// PDF変換中かどうか判定
		PublicDocumentService publicDocumentService = (PublicDocumentService)ApplicationContextLoader.getContext().getBean("publicDocumentService");
		Set<Long> idSet = publicDocumentService.getPDFConversionProcessingObjectIdSet(Arrays.asList((long)object.getId()));
		if (idSet.contains((long)object.getId())) {
			return false;
		}

		return true;
	}

	/**
	 * 指定されたオブジェクト一覧に対応するバージョンを、それが保持する全てのオブジェクトIDとバージョンのMapとして一括取得します。
	 * @param sess セッション
	 * @param objects オブジェクト一覧
	 * @return 全てのオブジェクトIDとバージョンのMap
	 * @throws Exception
	 */
	public static Map<Long, EIMVersion> getVersionMapByObjects(EIMSession sess, List<EIMObject> objects) throws Exception {
		Map resultMap = new HashMap<>();

		if (objects == null || objects.size() == 0) {
			return resultMap;
		}

		// バージョン一覧取得
		List<EIMVersion> versionList = getVersionListByObjects(sess, objects);

		// EIMVersionをMapに保管
		for (EIMVersion version : versionList) {
			for (Object obj : version.getList()) {
				EIMObject eimObj = (EIMObject) obj;
				resultMap.put(new Long(eimObj.getId()), version);
			}
		}

		return resultMap;
	}

	/**
	 * 指定されたオブジェクト一覧に対応するバージョン一括取得します。
	 * @param sess セッション
	 * @param objects オブジェクト一覧
	 * @return バージョン一覧
	 * @throws Exception
	 */
	public static List<EIMVersion> getVersionListByObjects(EIMSession sess, List<EIMObject> objects) throws Exception {

		if (objects == null || objects.size() == 0) {
			return new ArrayList<>();
		}

		TransactionContext orgTran = EIMThreadContext.getTransactionContext();

		try {
			// TransactionContext生成
			// getSeriesObjectListByObjectIds()とConvertUtils.toEIMObject()でFW2を利用するため
			if (orgTran == null) {
				TransactionContext tran = new TransactionContext();
				EIMThreadContext.putTransactionContext(tran);
				tran.setLangId(sess.getLangId());
				tran.setUser(ConvertUtils.toUserDomain(sess.getUser()));
				tran.setDBConnection(sess.getDBConnection());
			}

			// 指定されたオブジェクト一覧と同一シリーズ(リビジョングループIDが同一)のオブジェクト一覧を一括取得
			List<Long> idList = objects.stream().map(obj -> (long)obj.getId()).collect(Collectors.toList());
			List<ObjectDomain> seriesObjectList = getSeriesObjectListByObjectIds(idList);

			// EIMVersionリスト生成
			List<EIMVersion> versionList = new ArrayList<>();
			Map<Long, List<EIMObject>> revGId_ObjectListMap = new HashMap<>();
			for (ObjectDomain object : seriesObjectList) {
				long revGId = object.getRevisionGroupId();
				if (!revGId_ObjectListMap.containsKey(revGId)) {
					List<EIMObject> objectList = new ArrayList<>();
					revGId_ObjectListMap.put(revGId, objectList);

					// EIMVersion生成
					EIMVersion version = new EIMVersion(revGId, objectList);
					versionList.add(version);
				}

				// EIMObjectに変換してEIMVersionに追加
				EIMObject eimObject = ConvertUtils.toEIMObject(object);
				revGId_ObjectListMap.get(revGId).add(eimObject);
			}

			return versionList;
		} finally {
			if (orgTran == null) {
				EIMThreadContext.removeTransactionContext();
			}
		}
	}

	/**
	 * 指定されたオブジェクトID一覧と同一シリーズ(リビジョングループIDが一致)のオブジェクト一覧を取得します。<br>
	 * リビジョングループID->リビジョンID昇順でソートします。
	 * @param sess
	 * @param objectIds オブジェクトID一覧
	 * @return オブジェクト一覧
	 * @throws Exception 
	 */
	private static List<ObjectDomain> getSeriesObjectListByObjectIds(List<Long> objectIds) throws Exception {

		// 検索条件
		SearchSelectObject selectObject = new SearchSelectObject();
		SearchConditionBuildHelper h = new SearchConditionBuildHelper();

		// 条件指定されたオブジェクトID一覧を文字列のリストに変換
		List<String> idStringList = objectIds.stream().map(id -> String.valueOf(id)).collect(Collectors.toList());

		// リビジョングループID(EIMVER.vid)に対する検索条件式 (EIMVER.oidが指定されたオブジェクトID配列)
		StringBuffer sb = new StringBuffer();
		SearchSqlUtil.expandListToIn(sb, idStringList, "oid");
		selectObject.setCondition(h.group(h.opAnd())
				.addCondition(new SearchConditionIn(h.opAnd(),
						jp.co.ctc_g.eim.framework2.business.domain.search.SearchSelectObject.PsedoAttributeTypeEnum.REVISION_GROUP_ID,
						h.opIn(),
						"select vid from EIMVER where " + sb.toString())));

		// オブジェクト一覧取得
		ApplicationContext context = ApplicationContextLoader.getContext();
		ObjectDao objectDao = (ObjectDao) context.getBean("objectDao2");
		List<ObjectDomain> resultList =
				objectDao.getList(selectObject, new SearchLimitCountCondition(SearchLimitCountCondition.UNLIMITED, false));

		// リビジョングループID->リビジョンID昇順でソート
		List<ObjectDomain> sortedResultList = resultList.stream()
		        .sorted(Comparator.comparing(ObjectDomain::getRevisionGroupId)
		        		.thenComparing(ObjectDomain::getRevision))
		        .collect(Collectors.toList());

		return sortedResultList;
	}

}