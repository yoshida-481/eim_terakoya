package common.util;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import jakarta.servlet.http.HttpServletRequest;

import eim.bo.EIMAccessRole;
import eim.bo.EIMAttribute;
import eim.bo.EIMAttributeType;
import eim.bo.EIMException;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMResource;
import eim.bo.EIMSearchConditionCompare;
import eim.bo.EIMSearchConditionGroup;
import eim.bo.EIMSearchConditionIn;
import eim.bo.EIMSearchConditionLike;
import eim.bo.EIMSearchOperatorEnum;
import eim.bo.EIMSearchSelectEIMObject;
import eim.bo.EIMValueType;
import eim.net.EIMSession;
import eim.util.DateUtils;
import eim.util.EIMConfig;
import eim.util.EIMUtils;
import eim.util.ObjectAttributeUtils;
import eim.util.ObjectUtils;
import eim.util.SearchUtils;
import eim.util.StringUtils;

/**
 *
 * 属性表示色に関するユーティリティクラス
 *
 */
public class DisplayColorUtil
{
	private final static int ERROR_ATTR_VALUE_NOT_REGIST = 214;

	/**
	 * 属性情報画面で入力された属性リスト値の表示色オブジェクトを更新します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param request 属性更新情報の管理クラス
	 * @param object 属性情報設定対象のオブジェクト
	 * @throws Exception
	 */
	public static void updateDisplayColor(EIMSession sess, HttpServletRequest request, EIMObject object) throws Exception
	{
		//Parameter
		String prmObjTypeId = EIMUtils.getParameter(request, "objTypeId");

		//Object Type
		EIMObjectType objType = null;
		if (prmObjTypeId != null && prmObjTypeId.length() > 0) {
			objType = ObjectUtils.getObjectTypeById(sess, Long.parseLong(prmObjTypeId));
		} else {
			objType = ObjectUtils.getObjectTypeById(sess, object.getType().getId());
		}

		//Attribute Types
		List attTypeList = ObjectAttributeUtils.getAttributeTypeList(sess, objType);

		EIMAttributeType attType = null;
		List objNameList = new ArrayList();
		List colorList = new ArrayList();

		for(int ii = 0; ii < attTypeList.size(); ii++) {
			attType = (EIMAttributeType)attTypeList.get(ii);

			int pos = 0;
			while(true) {
				String param = EIMUtils.getParameter(request, "attType_" + attType.getId() + "_" + pos);
				String color = getColor(sess, request, String.valueOf(attType.getId()), pos, param);
				if (param == null || color == null) {
					break;
				}
				else {
					objNameList.add(getObjName(object, attType.getId()));
					colorList.add(color);
				}

				pos++;
			}
		}

		for(int ii = 0; ii < objNameList.size(); ii++) {
			update(sess, objNameList.get(ii).toString(), colorList.get(ii).toString());
		}
	}
	
	/**
	 * 新規ドキュメント登録で入力された属性リスト値の表示色オブジェクトを更新します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param paramFieldList パラメータフィールドリスト
	 * @param paramValueList パラメータ値リスト
	 * @param object 属性情報設定対象のオブジェクト
	 * @throws Exception
	 */
	public static void updateDisplayColor(EIMSession sess, List paramFieldList,List paramValueList, EIMObject object) throws Exception
	{
		//ListをHashMapに変換する
		HashMap<String ,String> paramMap = new HashMap<String,String>();
		for(int i = 0; i<paramFieldList.size(); i++){
			paramMap.put( (String)paramFieldList.get(i),  (String)paramValueList.get(i));
		}

		//Object Type
		EIMObjectType objType = null;
		objType = object.getType();

		//対象のオブジェクトに付与されているオブジェクトタイプリストを取得
		List<EIMAttributeType> attTypeList = ObjectAttributeUtils.getAttributeTypeList(sess, objType);
		
		EIMAttributeType attType = null;
		List objNameList = new ArrayList();
		List colorList = new ArrayList();
		//オブジェクトタイプリストとパラメータをマッピングし
		for(int ii = 0; ii < attTypeList.size(); ii++) {
			attType = attTypeList.get(ii);

			int pos = 0;
			while(true) {
				String color = paramMap.get("attType_" + attType.getId() + "_" + pos+ "_color" );
				if (color == null) {
					break;
				}
				else {
					objNameList.add(getObjName(object, attType.getId()));
					colorList.add(color);
				}
				pos++;
			}
		}
		for(int ii = 0; ii < objNameList.size(); ii++) {
			update(sess, objNameList.get(ii).toString(), colorList.get(ii).toString());
		}
	}
	

	/**
	 * 属性の引継時に表示色オブジェクトを更新します。
	 * @param sess EIMSessionインスタンス
	 * @param request 属性更新情報の管理クラス
	 * @param obj 属性情報設定対象のオブジェクト
	 * @param attrType 属性タイプ
	 * @param value 属性値
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
	 * @param obj 属性情報設定対象のオブジェクト
	 * @param attrType 属性タイプ
	 * @param value 属性値
	 * @throws Exception
	 */
	public static void updateDisplayColor(EIMSession sess, EIMObject obj, EIMAttributeType attrType, String value) throws Exception
	{
		String objName = getObjName(obj, attrType.getId());
		String color = getColor(sess, String.valueOf(attrType.getId()), value);
		
		// 色が取得できない場合(valueがnullの場合)"-"を指定する
		if(color == null) {
			color = "-";
		}

		// 色を更新する
		// colorが"-"の時、色オブジェクトは生成されない、または削除される
		update(sess, objName, color);
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
	 * 上位引継ぎの属性表示色オブジェクトを作成する
	 * （親オブジェクトが持つ属性表示色オブジェクトをコピーする）
	 *
	 * @param sess EIMSessionインスタンス
	 * @param newObject 作成するオブジェクト
	 * @param attrTypeList 属性タイプ
	 * @param path パス
	 * @param conditionHelper ヘルパーインスタンス
	 * @throws Exception
	 */
	public static void inheritDisplayColor(EIMSession sess, EIMObject newObject, List attrTypeList, String path,
			AppObjectConditionHelper conditionHelper) throws Exception {

		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = conditionHelper;
		if (helper == null) {
			helper = new AppObjectConditionHelper(sess);
		}

		if(path == null || AppObjectUtil.isPathInRecycle(path)) {
			return;
		}

		//パスから親オブジェクトを取得
		String[] array = path.split("/");
		String parentPath = "";
		for(int ii = 0; ii < array.length - 1; ii++) {
			parentPath += array[ii] + "/";
		}

		EIMObject parentObj = helper.getObjectByFullPass(parentPath, EIMConfig.get("OBJECT_TYPE_NAME_FOLDER"));

		if(parentObj == null) {
			return;
		}

		inheritDisplayColor(sess, newObject, attrTypeList, parentObj, helper);
	}

	/**
	 * 上位引継ぎの属性表示色オブジェクトを作成する
	 * （親オブジェクトが持つ属性表示色オブジェクトをコピーする）
	 *
	 * @param sess EIMSessionインスタンス
	 * @param newObject 作成するオブジェクト
	 * @param attrTypeList 属性タイプ
	 * @param parentObj 親オブジェクト
	 * @throws Exception
	 */
	public static void inheritDisplayColor(EIMSession sess, EIMObject newObject, List attrTypeList, EIMObject parentObj)
			throws Exception {
		inheritDisplayColor(sess, newObject, attrTypeList, parentObj, null);
	}

	/**
	 * 上位引継ぎの属性表示色オブジェクトを作成する
	 * （親オブジェクトが持つ属性表示色オブジェクトをコピーする）
	 *
	 * @param sess EIMSessionインスタンス
	 * @param newObject 作成するオブジェクト
	 * @param attrTypeList 属性タイプ
	 * @param parentObj 親オブジェクト
	 * @throws Exception
	 */
	public static void inheritDisplayColor(EIMSession sess, EIMObject newObject, List attrTypeList, EIMObject parentObj,
			AppObjectConditionHelper conditionHelper) throws Exception {
		if(parentObj == null) {
			return;
		}

		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = conditionHelper;
		if (helper == null) {
			helper = new AppObjectConditionHelper(sess);
		}

		// 予め親オブジェクトが保持する属性表示色オブジェクトを一括取得
		HashMap<String, EIMObject> displayColorSet = helper.getDisplayColorSet(parentObj);

		//フォルダ配下
		for(int ii = 0; ii < attrTypeList.size(); ii++) {
			long attrId = ((EIMAttributeType)attrTypeList.get(ii)).getId();
	    	EIMObject colorObj = (EIMObject)displayColorSet.get(getObjName(parentObj, attrId));
	    	//一度古いオブジェクトを削除してから新規作成する（切取り→貼付け用＆表示色のない下位引継ぎ属性用）
	    	String newObjName = getObjName(newObject, attrId);
	    	AppObjectUtil.deleteObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_DISPCOLOR"), newObjName);
			if (colorObj != null) {
				EIMAttribute attrColor = colorObj.getAttribute(EIMConfig.get("ATTR_NAME_VALDISPCOLOR_DISPCOLOR"));
				if(attrColor != null) {
					createColorObject(sess, newObjName, attrColor.getString());
				}
			}
		}
	}

	/**
	 * コピー時に属性リスト値の表示色オブジェクトを新規作成します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param newObject 属性情報設定対象のオブジェクト
	 * @param oldObjId コピー元オブジェクト
	 * @throws Exception
	 */
	public static void copyDisplayColor(EIMSession sess, EIMObject newObject, EIMObject oldObject) throws Exception
	{
		// 予め属性表示色オブジェクトを一括取得
		HashMap displayColorSet = getDisplayColorSet(sess, oldObject);

		// 属性表示色オブジェクトに一致するものがあればコピーする
		List attrList = newObject.getAttributeList();
		for(int ii = 0; ii < attrList.size(); ii++) {
			long attrId = ((EIMAttribute)attrList.get(ii)).getType().getId();
	    	EIMObject colorObj = (EIMObject)displayColorSet.get(getObjName(oldObject, attrId));
			if (colorObj != null) {
				EIMAttribute attrColor = colorObj.getAttribute(EIMConfig.get("ATTR_NAME_VALDISPCOLOR_DISPCOLOR"));
				if(attrColor != null) {
					createColorObject(sess, getObjName(newObject, attrId), attrColor.getString());
				}
			}
		}
	}

	/**
	 * ブランチコピー時に属性リスト値の表示色オブジェクトを新規作成します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param newObject 属性情報設定対象のオブジェクト
	 * @param oldObjId コピー元オブジェクト
	 * @throws Exception
	 */
	public static void branchcopyDisplayColor(EIMSession sess, EIMObject newObject, EIMObject oldObject) throws Exception
	{
		// 予め属性表示色オブジェクトを一括取得
		HashMap displayColorSet = getDisplayColorSet(sess, oldObject);

		// 属性表示色オブジェクトに一致するものがあればコピーする
		List attrList = oldObject.getAttributeList();
		for(int ii = 0; ii < attrList.size(); ii++) {
			long attrId = ((EIMAttribute)attrList.get(ii)).getType().getId();
	    	EIMObject colorObj = (EIMObject)displayColorSet.get(getObjName(oldObject, attrId));
			if (colorObj != null) {
				EIMAttribute attrColor = colorObj.getAttribute(EIMConfig.get("ATTR_NAME_VALDISPCOLOR_DISPCOLOR"));
				if(attrColor != null) {
					createColorObject(sess, getObjName(newObject, attrId), attrColor.getString());
				}
			}
		}
	}

	/**
	 * フォルダツリー複製時に属性リスト値の表示色オブジェクトを新規作成します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param newObjId コピー先オブジェクトのID
	 * @param attr 属性情報
	 * @param oldObj コピー元オブジェクト
	 * @throws Exception
	 */
	public static void copyFolderDuplicateDisplayColor(EIMSession sess, String newObjId, EIMAttribute attr,
			EIMObject oldObject, AppObjectConditionHelper helper) throws Exception {

		// 予め属性表示色オブジェクトを一括取得
		HashMap displayColorSet = helper.getDisplayColorSet(oldObject);

		// 属性表示色オブジェクトに一致するものがあればコピーする
		long attrId = attr.getType().getId();
    	EIMObject colorObj = (EIMObject)displayColorSet.get(getObjName(oldObject, attrId));
		if (colorObj != null) {
			EIMAttribute attrColor = colorObj.getAttribute(EIMConfig.get("ATTR_NAME_VALDISPCOLOR_DISPCOLOR"));
			if(attrColor != null) {
				createColorObject(sess, newObjId + "_" + String.valueOf(attrId), attrColor.getString());
			}
		}
	}

	/**
	 * 外部インタフェースコール時に属性リスト値の表示色オブジェクトを新規作成します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param request 属性更新情報の管理クラス
	 * @param objId オブジェクトのID
	 * @param value 値
	 * @param attType 属性情報
	 * @throws Exception
	 */
	public static void createDisplayColorFromExternalIF(EIMSession sess, HttpServletRequest request, String objId, String value, EIMAttributeType attType) throws Exception
	{
		//属性をリストで保持しない場合は何もしない
		EIMObject o = AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRMST"), String.valueOf(attType.getId()));
		if(o == null) {
			return;
		}

		// 値に一致する色を検索する
		// (1)「属性タイプ値マスター」に属性値があり、かつその属性値に対し「表示色設定あり」かつ「表示色有効」の場合
		// 　→表示色(＃FF0000など)が文字列で返却される
		// (2) 「属性タイプ値マスター」に属性値があり、かつその属性値に対し「表示色設定なし」あるいは「表示色無効」の場合
		// 　→「-」が返却される(※ 色なしの意)
		// (3) 「属性タイプ値マスター」に属性値がない場合
		// 　→nullが返却される
		String color = getColor(sess, request, String.valueOf(attType.getId()), 0, value);
		if (color == null) {
			throw new EIMException(request, ERROR_ATTR_VALUE_NOT_REGIST);
		}
		createColorObject(sess, objId + "_" + attType.getId(), color);
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

	/**
	 * 指定されたオブジェクトが保持する属性表示色オブジェクトを削除する
	 * @param sess 現在のセッション
	 * @param object オブジェクト
	 */
	public static void deleteDisplayColorObject(EIMSession sess, EIMObject object) throws Exception
	{
		// 予めオブジェクトが保持していた属性表示色オブジェクトを一括取得
		List displayColorList = getAllDisplayColorList(sess, object);

		for(int ii = 0; ii < displayColorList.size(); ii++) {
			ObjectUtils.deleteObject(sess, (EIMObject)displayColorList.get(ii));
		}
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
	 * @param sess EIMSessionインスタンス
	 * @param request リクエスト情報
	 * @param attrId 属性ID
	 * @param pos 配列のインデックス
	 * @param value 属性の値
	 * @return 表示色（見つからない場合はnullを返却）
	 */
	private static String getColor(
			EIMSession sess, HttpServletRequest request,
			String attrId, int pos, String value) throws Exception
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
		return getColor(sess, attrId, value);
	}

	/**
	 * 表示色を取得する。DBを検索し、その結果を返却します。
	 * @param sess EIMSessionインスタンス
	 * @param request リクエスト情報
	 * @param attrId 属性ID
	 * @param pos 配列のインデックス
	 * @param value 属性の値
	 * @return 表示色（見つからない場合はnullを返却）
	 */
	private static String getColor(EIMSession sess, String attrId, String value) throws Exception
	{
		if(value == null) {
			return null;
		}

		// 属性IDからオブジェクトを取得
		EIMObject o = AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRMST"), attrId);
		if(o == null) {
			return null;
		}

		@SuppressWarnings("rawtypes")
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
		switch (valType) {
			case EIMValueType.INTEGER:
				for(; dataIndex < dataAttr.getInts().length; dataIndex++) {
					if(value.equals(String.valueOf(dataAttr.getInts()[dataIndex]))) {
						break;
					}
				}
				break;
			case EIMValueType.DOUBLE:
				for(; dataIndex < dataAttr.getDoubles().length; dataIndex++) {
					if(value.equals(String.valueOf(dataAttr.getDoubles()[dataIndex]))) {
						break;
					}
				}
				break;
			case EIMValueType.STRING:
				for(; dataIndex < dataAttr.getStrings().length; dataIndex++) {
					if(value.equals(dataAttr.getStrings()[dataIndex])) {
						break;
					}
				}
				break;
			case EIMValueType.DATE:
				for(; dataIndex < dataAttr.getDates().length; dataIndex++) {
					// Date(TZ変換)→文字列変換
					Date convertedDate = new Date(DateUtils.convDBTzToCLTzTime(sess, dataAttr.getDates()[dataIndex]));
					String dValue = StringUtils.getDateStringByFormat(convertedDate, EIMResource.getMessage(sess, "EIM.FORMAT.DATE"));
					if(value.equals(dValue)) {
						break;
					}
				}
				break;
			case EIMValueType.TEXT:
				for(; dataIndex < dataAttr.getTexts().length; dataIndex++) {
					if(StringUtils.convertReturnCede(value).equals(StringUtils.convertReturnCede(dataAttr.getTexts()[dataIndex]))) {
						break;
					}
				}
				break;
			default:
				break;
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
	 * 指定されたオブジェクトに関連する属性表示色オブジェクトのマップを取得する。
	 * @param sess 現在のセッション
	 * @param oldObject オブジェクト
	 * @return 属性表示色オブジェクトのマップ（キーは "<オブジェクトID>_<属性タイプID>"）
	 * @throws Exception
	 */
	private static HashMap getDisplayColorSet(EIMSession sess, EIMObject oldObject) throws Exception
	{
		ArrayList list = new ArrayList();
		list.add(oldObject);
		return getDisplayColorSet(sess, list);
	}

	/**
	 * 指定されたオブジェクトのリストに関連する属性表示色オブジェクトのマップを取得する。
	 * @param sess 現在のセッション
	 * @param objList オブジェクトのリスト
	 * @return 属性表示色オブジェクトのマップ（キーは "<オブジェクトID>_<属性タイプID>"）
	 * @throws Exception
	 */
	public static HashMap getDisplayColorSet(EIMSession sess, List objList) throws Exception
	{
		// 検索条件項目・返却項目指定オブジェクトを生成
		EIMSearchSelectEIMObject selectTarget = new EIMSearchSelectEIMObject();
		// 検索条件グループ作成
		EIMSearchConditionGroup conds = new EIMSearchConditionGroup(EIMSearchOperatorEnum.AND);
		// 属性表示色オブジェクトタイプを作成
		EIMObjectType objType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_DISPCOLOR"));

		// 検索条件SQL
		// ※ EIMSearchConditionLikeを使ってNAME属性をLIKE検索しようとすると、SearchConditionのネストが深くなり、条件件数が多い場合にStackOverFlowが発生することがあるため利用しない。
		StringBuffer sql = new StringBuffer();
		
		// 検索条件SQL(オブジェクトタイプとオブジェクト名称(オブジェクトID_*))を設定する
		Set<Long> attrTypeIdSet = new HashSet<Long>();
		int count = 0;
		for (Iterator i = objList.iterator(); i.hasNext();) {
			EIMObject object = (EIMObject) i.next();
			List attList = object.getAttributeList();
			if (attList == null) {
				continue;
			}
			
			String objIDCondition = String.valueOf(object.getId()) + "\\_%";
			
			if (count == 0) {
				sql.append("select ID from EIMOBJ where type = " + objType.getId() + " and (NAME like '" + objIDCondition + "' escape '\\'");
			}
			else {
				sql.append(" or NAME like '" + objIDCondition + "' escape '\\'");
			}
			
			count ++;
			
			// 属性タイプをハッシュセットに設定する(重複除去)
			for (Iterator ii = attList.iterator(); ii.hasNext();) {
				EIMAttribute att = (EIMAttribute) ii.next();
				EIMAttributeType attType = att.getType();
				attrTypeIdSet.add((long)attType.getId());
			}
		}
		
		// 検索条件が無い場合は空をリターンする
		if (count == 0)
			return new HashMap<String, EIMObject>();
		
		sql.append(")");
		
		// 検索条件SQL(オブジェクト名称(*_属性タイプID))を設定する
		count = 0;
		for (long attrTypeId : attrTypeIdSet) {
			String attrTypeIdCondition = "%\\_" + String.valueOf(attrTypeId);
			
			if (count == 0) {
				sql.append(" and (NAME like '" + attrTypeIdCondition + "' escape '\\'");
			}
			else {
				sql.append(" or NAME like '" + attrTypeIdCondition + "' escape '\\'");
			}
			
			count ++;
		}
		sql.append(")");
		
		// 検索条件SQLを検索条件に設定する
		EIMSearchConditionIn cond1 = new EIMSearchConditionIn(
					EIMSearchOperatorEnum.AND,								// 前方演算子AND
					EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.ID,		//オブジェクトID
					EIMSearchOperatorEnum.IN,								// in
					sql.toString()											// 検索条件SQL
				);
		
		// 条件設定
		conds.addCondition(cond1);
		selectTarget.setCondition(conds);
		
		// 検索実行
		selectTarget.setRole(EIMAccessRole.READ);
		List retList = SearchUtils.searchObjects(sess, selectTarget, null);

		// HashMapに格納
		HashMap<String, EIMObject> ret = new HashMap<String, EIMObject>();
		for (Iterator iter = retList.iterator(); iter.hasNext();) {
			EIMObject obj = (EIMObject) iter.next();
			ret.put(obj.getName(), obj);
		}

		return ret;
	}

	/**
	 * 指定されたオブジェクトに関連する属性表示色リストを取得する。
	 * @param sess 現在のセッション
	 * @param obj オブジェクト
	 */
	public static List getAllDisplayColorList(EIMSession sess, EIMObject obj) throws Exception
	{
		// 検索条件項目・返却項目指定オブジェクトを生成
		EIMSearchSelectEIMObject selectTarget = new EIMSearchSelectEIMObject();
		// 検索条件グループ作成
		EIMSearchConditionGroup conds = new EIMSearchConditionGroup(EIMSearchOperatorEnum.AND);
		// 属性表示色オブジェクトタイプを作成
		EIMObjectType objType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_DISPCOLOR"));

		// 条件1:対象のオブジェクトタイプを検索
		EIMAttributeType fieldOfObjType = EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.TYPE;
		EIMSearchConditionCompare cond1 = new EIMSearchConditionCompare(
				EIMSearchOperatorEnum.AND		//前方演算子AND
				, fieldOfObjType				//オブジェクトタイプ
				, EIMSearchOperatorEnum.EQ		// =
				, objType.getId() 				//「属性表示色」のオブジェクトタイプID
			);
		// 条件1を条件グループに加える
		conds.addCondition(cond1);

		// 条件2:対象のオブジェクト名を検索
		String objID = String.valueOf(obj.getId()) + "_*";

		EIMAttributeType fieldOfObjName = EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.NAME;
		EIMSearchConditionLike cond2 = new EIMSearchConditionLike(
				EIMSearchOperatorEnum.AND		// 前方演算子AND
				, fieldOfObjName				// オブジェクト名
				, EIMSearchOperatorEnum.LIKE	// like
				, objID							// オブジェクトID
			);
		// 条件2を条件グループに加える
		conds.addCondition(cond2);

		// 検索条件項目・返却項目指定インスタンスに条件グループを設定
		selectTarget.setCondition(conds);

		// 検索実行
		selectTarget.setRole(EIMAccessRole.READ);
		List retList = SearchUtils.searchObjects(sess, selectTarget, null);

		return retList;
	}
}