package eim.command.common.util;

import java.util.Date;
import java.util.List;

import jakarta.servlet.http.HttpServletRequest;

import common.util.AppObjectUtil;
import eim.bo.EIMAttribute;
import eim.bo.EIMAttributeType;
import eim.bo.EIMObject;
import eim.bo.EIMResource;
import eim.bo.EIMValueType;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.EIMUtils;
import eim.util.StringUtils;

/**
 * 
 * 日付型表示色オブジェクトを時分秒を無視せず比較できるよう、しかたなく独自実装。
 * @see common.util.DisplayColorUtil
 *
 */
public class EIMCommandDisplayColorUtils {

	/**
	 * 属性の引継時に表示色オブジェクトを更新します。
	 * @param sess EIMSessionインスタンス
	 * @param request 属性更新情報の管理クラス
	 * @param obj 属性情報設定対象のオブジェクト
	 * @param attrType 属性タイプ
	 * @param value 属性値 ※日付型の場合はオーバーロードした別のupdateDisplayColor()を使用してください！！
	 * @throws Exception
	 */
	public static void updateDisplayColor(EIMSession sess, HttpServletRequest request, EIMObject obj, EIMAttributeType attrType, String value) throws Exception
	{
		String objName = getObjName(obj, attrType.getId());
		String color = getColor(sess, request, String.valueOf(attrType.getId()), 0, value);
		if(color == null) {
			return;
		}

		update(sess, objName, color);
	}

	/**
	 * 属性の引継時に表示色オブジェクトを更新します。
	 * @param sess EIMSessionインスタンス
	 * @param request 属性更新情報の管理クラス
	 * @param obj 属性情報設定対象のオブジェクト
	 * @param attrType 属性タイプ
	 * @param value 属性値 ※日付型の場合は
	 * @throws Exception
	 */
	public static void updateDisplayColor(EIMSession sess, HttpServletRequest request, EIMObject obj, EIMAttributeType attrType, Date value) throws Exception
	{
		String objName = getObjName(obj, attrType.getId());
		String color = getColor(sess, request, String.valueOf(attrType.getId()), 0, value);
		if(color == null) {
			return;
		}

		update(sess, objName, color);
	}

	/**
	 * 属性表示色オブジェクトのオブジェクト名を返す
	 * @param obj オブジェクト
	 * @param aId 属性ID
	 */
	private static String getObjName(EIMObject obj, long aId) throws Exception
	{
		return getObjName(obj, String.valueOf(aId));
	}

	/**
	 * 属性表示色オブジェクトのオブジェクト名を返す
	 * @param obj オブジェクト
	 * @param aId 属性ID
	 */
	private static String getObjName(EIMObject obj, String aId) throws Exception
	{
		return String.valueOf(obj.getId()) + "_" + aId;
	}

	/**
	 * 表示色を取得する
	 * <p>
	 * 
	 * </p>
	 * @param sess EIMSessionインスタンス
	 * @param request リクエスト情報
	 * @param attrId 属性ID
	 * @param pos 配列のインデックス
	 * @param value 属性の値
	 * @return 表示色（見つからない場合はnullを返却）
	 */
	@SuppressWarnings("unchecked")
	private static String getColor(
			EIMSession sess, HttpServletRequest request,
			String attrId, int pos, String value) throws Exception
	{
		if(value == null) {
			return null;
		}
		else if (StringUtils.isBlank(value)) {
			// 値を空に更新しようとしている場合は表示色をなしにする
			return "-";
		}

		if(request != null) {
			String color = EIMUtils.getParameter(request, "attType_" + attrId + "_" + pos + "_color");
			if(color != null) {
				return color;
			}
		}

		/*
		 * フォルダ等の新規作成時は色データを保持していないので、DBを検索する必要がある
		 */

		// 属性IDからオブジェクトを取得
		EIMObject o = AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRMST"), attrId);
		if(o == null) {
			return null;
		}

		List attributeList = o.getAttributeList();

		// データを格納している属性を検索する
		EIMAttribute dataAttr = null;
		for(int ii = 0; ii < attributeList.size(); ii++) {
			dataAttr = (EIMAttribute)attributeList.get(ii);
			String typeName = dataAttr.getType().getDefName();
			if(typeName.equals(EIMConfig.get("ATTR_NAME_ATTRMST_ATTR_ATTRCOLOR_LIST")) == false
				&& typeName.equals(EIMConfig.get("ATTR_NAME_ATTRMST_ATTR_ATTRSETTING_LIST")) == false)
			{
				//データはタイプ名が「属性タイプ値マスター表示色リスト」や「属性タイプ値マスター表示設定リスト」以外
				break;
			}
		}

		if(dataAttr == null) {
			return null;
		}

		// データはリストの何番目に格納されているか
		int valType = dataAttr.getType().getValueType().getId();
		int dataIndex = 0;
		boolean dataFound = false;
		switch (valType) {
			case EIMValueType.INTEGER:
				for(; dataIndex < dataAttr.getInts().length; dataIndex++) {
					if(value.equals(String.valueOf(dataAttr.getInts()[dataIndex]))) {
						dataFound = true;
						break;
					}
				}
				break;
			case EIMValueType.DOUBLE:
				for(; dataIndex < dataAttr.getDoubles().length; dataIndex++) {
					if(value.equals(String.valueOf(dataAttr.getDoubles()[dataIndex]))) {
						dataFound = true;
						break;
					}
				}
				break;
			case EIMValueType.STRING:
				for(; dataIndex < dataAttr.getStrings().length; dataIndex++) {
					if(value.equals(dataAttr.getStrings()[dataIndex])) {
						dataFound = true;
						break;
					}
				}
				break;
			case EIMValueType.DATE:
				for(; dataIndex < dataAttr.getDates().length; dataIndex++) {
					String dValue = StringUtils.getDateStringByFormat(dataAttr.getDates()[dataIndex], EIMResource.getMessage(sess, "EIM.FORMAT.DATE"));
					if(value.equals(dValue)) {
						dataFound = true;
						break;
					}
				}
				break;
			case EIMValueType.TEXT:
				for(; dataIndex < dataAttr.getTexts().length; dataIndex++) {
					if(StringUtils.convertReturnCede(value).equals(StringUtils.convertReturnCede(dataAttr.getTexts()[dataIndex]))) {
						dataFound = true;
						break;
					}
				}
				break;
			default:
				break;
		}
		if (!dataFound) {
			// 該当するデータがリストに存在しない場合は表示色オブジェクトを削除
			return "-";
		}

		// 表示色情報を格納している属性を検索する
		EIMAttribute colorAttr = null;
		for(int ii = 0; ii < attributeList.size(); ii++) {
			colorAttr = (EIMAttribute)attributeList.get(ii);
			String typeName = colorAttr.getType().getDefName();
			if(typeName.equals(EIMConfig.get("ATTR_NAME_ATTRMST_ATTR_ATTRCOLOR_LIST"))) {
				//タイプ名が「属性タイプ値マスター表示色リスト」
				break;
			}
		}

		if(colorAttr.getStrings().length <= dataIndex) {
			return null;
		}

		return colorAttr.getStrings()[dataIndex];
	}

	/**
	 * 表示色を取得する
	 * <p>
	 * 
	 * </p>
	 * @param sess EIMSessionインスタンス
	 * @param request リクエスト情報
	 * @param attrId 属性ID
	 * @param pos 配列のインデックス
	 * @param value 属性の値(日付型)
	 * @return 表示色（見つからない場合はnullを返却）
	 */
	@SuppressWarnings("unchecked")
	private static String getColor(
			EIMSession sess, HttpServletRequest request,
			String attrId, int pos, Date value) throws Exception
	{
		if(value == null) {
			return null;
		}

		if(request != null) {
			String color = EIMUtils.getParameter(request, "attType_" + attrId + "_" + pos + "_color");
			if(color != null) {
				return color;
			}
		}

		/*
		 * フォルダ等の新規作成時は色データを保持していないので、DBを検索する必要がある
		 */

		// 属性IDからオブジェクトを取得
		EIMObject o = AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRMST"), attrId);
		if(o == null) {
			return null;
		}

		List attributeList = o.getAttributeList();

		// データを格納している属性を検索する
		EIMAttribute dataAttr = null;
		for(int ii = 0; ii < attributeList.size(); ii++) {
			dataAttr = (EIMAttribute)attributeList.get(ii);
			String typeName = dataAttr.getType().getDefName();
			if(typeName.equals(EIMConfig.get("ATTR_NAME_ATTRMST_ATTR_ATTRCOLOR_LIST")) == false
				&& typeName.equals(EIMConfig.get("ATTR_NAME_ATTRMST_ATTR_ATTRSETTING_LIST")) == false)
			{
				//データはタイプ名が「属性タイプ値マスター表示色リスト」や「属性タイプ値マスター表示設定リスト」以外
				break;
			}
		}

		if(dataAttr == null) {
			return null;
		}

		// データはリストの何番目に格納されているか
		int valType = dataAttr.getType().getValueType().getId();
		int dataIndex = 0;
		boolean dataFound = false;
		switch (valType) {
			case EIMValueType.DATE:
				for(; dataIndex < dataAttr.getDates().length; dataIndex++) {
					if(value.equals(dataAttr.getDates()[dataIndex])) {
						dataFound = true;
						break;
					}
				}
				break;
			default:
				break;
		}
		if (!dataFound) {
			// 該当するデータがリストに存在しない場合は表示色オブジェクトを削除
			return "-";
		}

		// 表示色情報を格納している属性を検索する
		EIMAttribute colorAttr = null;
		for(int ii = 0; ii < attributeList.size(); ii++) {
			colorAttr = (EIMAttribute)attributeList.get(ii);
			String typeName = colorAttr.getType().getDefName();
			if(typeName.equals(EIMConfig.get("ATTR_NAME_ATTRMST_ATTR_ATTRCOLOR_LIST"))) {
				//タイプ名が「属性タイプ値マスター表示色リスト」
				break;
			}
		}

		if(colorAttr.getStrings().length <= dataIndex) {
			return null;
		}

		return colorAttr.getStrings()[dataIndex];
	}

	/**
	 * 属性表示色オブジェクトを更新する
	 *   【処理一覧】
	 *               | 属性が設定された   | 属性が設定されていない
	 * -------------+-----------------+-------------------
	 *   オブジェクトあり | （１）オブジェクトを更新 | （２）オブジェクトを削除
	 * -------------+-----------------+-------------------
	 *   オブジェクトなし | （３）オブジェクトを生成 | （４）何もしない
	 *
	 * @param sess EIMSessionインスタンス
	 * @param objName オブジェクト名
	 * @param color 表示色
	 * @throws Exception
	 */
	private static void update(EIMSession sess, String objName, String color) throws Exception
	{
	    EIMObject objDispColor = AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_DISPCOLOR"), objName);
		if (objDispColor == null) {
			if(color.equals("-")) {
				//上記（４）
			} else {
				//上記（３）
				createColorObject(sess, objName, color);
			}
		} else {
			if(color.equals("-")) {
				//上記（２）
				AppObjectUtil.deleteObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_DISPCOLOR"), objName);
			} else {
				//上記（１）
				AppObjectUtil.updateAttr(sess, objDispColor, EIMConfig.get("ATTR_NAME_VALDISPCOLOR_DISPCOLOR"), color);
			}
		}
	}
	
	/**
	 * 属性表示色オブジェクトを作成する
	 * @param sess EIMSesssionインスタンス
	 * @param objName オブジェクト名
	 * @param color 表示色
	 */
	private static void createColorObject(EIMSession sess, String objName, String color) throws Exception
	{
		EIMObject newObj = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_DISPCOLOR"), objName);
		AppObjectUtil.setAttr(sess, newObj, EIMConfig.get("ATTR_NAME_VALDISPCOLOR_DISPCOLOR"), color);
	}

}
