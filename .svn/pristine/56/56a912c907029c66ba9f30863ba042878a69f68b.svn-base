package common.util;

import java.util.Date;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;

import common.bo.AttributeValueMaster;
import eim.bo.EIMAccessRole;
import eim.bo.EIMAttribute;
import eim.bo.EIMAttributeType;
import eim.bo.EIMException;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMSearchConditionCompare;
import eim.bo.EIMSearchConditionGroup;
import eim.bo.EIMSearchConditionIn;
import eim.bo.EIMSearchOperatorEnum;
import eim.bo.EIMSearchSelectEIMObject;
import eim.bo.EIMValueType;
import eim.net.EIMSession;
import eim.util.AttributeUtils;
import eim.util.EIMConfig;
import eim.util.EIMConstant;
import eim.util.ObjectAttributeUtils;
import eim.util.ObjectUtils;
import eim.util.SearchUtils;
import eim.util.TypeConvertUtils;

/**
 *
 * 属性タイプ値マスター関連クラス
 *
 */
public class AttributeMasterUtil {

	/**
	 *
	 * 指定した属性タイプの属性タイプ値マスターを作成します。
	 *
	 * @param sess
	 *            現在のセッション
	 * @param attType
	 *            属性タイプ
	 *
	 * @throws Exception
	 *
	 * @return 作成した属性タイプ値マスター
	 */
	public static AttributeValueMaster createAttributeValueMaster(EIMSession sess,
			EIMAttributeType attType) throws Exception
	{
		// 属性タイプ値マスターのオブジェクト名(属性タイプID)
		String attrMasterObjName = Long.toString(attType.getId());
		EIMObject attrMasterObj = null;
		EIMObjectType attrMasterObjType = null;

		// 属性タイプ値マスターオブジェクトタイプの取得
		try{
			attrMasterObjType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRMST"));
			if(attrMasterObjType == null) {
				throw new EIMException(sess,"EIM.ERROR.SYSTEMERROR");
			}

			// 属性タイプ値マスターオブジェクトの生成
			attrMasterObj = ObjectUtils.createObject(sess,
					attrMasterObjType,
					attrMasterObjName,
					EIMConstant.DEPU_CHECK_TYPE_NAME);//オブジェクトタイプと名称で重複チェック
		} catch (EIMException eime) {
			String errMessageKey = eime.getMessageKey();
			if(errMessageKey.equals("EIM.ERROR.LOGIC.OBJECT.NAME.DUPLICATE")) {
				// 名称重複
				throw new EIMException(sess, "EIM.ERROR.COMMON.API.ATTRIBUTE.MASTER.EXISTS");
			} else {
				throw eime;
			}
		} catch (Exception e) {
			throw e;
		}

		return new AttributeValueMaster(attrMasterObj.getId(),attType,null,null,null,null,null,null, null);
	}

	/**
	 *
	 * 指定した属性タイプ値マスターを削除します。
	 *
	 * @param sess
	 *            現在のセッション
	 * @param attValueMaster
	 *            属性タイプ値マスター
	 *
	 * @throws Exception
	 *
	 */
	public static void deleteAttributeValueMaster(EIMSession sess,
			AttributeValueMaster attValueMaster) throws Exception
	{
		// 属性タイプ値マスターのオブジェクト取得
		EIMObject attrMasterObj = ObjectUtils.getObjectById(sess,attValueMaster.getId());
		if(attrMasterObj != null)
		{
			// 属性タイプ値マスターのオブジェクト削除
			ObjectUtils.deleteObject(sess, attrMasterObj);
		}
	}

	/**
	 *
	 * 指定された属性IDに紐付く属性タイプ値マスターの一覧を取得します。<br>
	 *
	 * <li>属性タイプ値マスターが存在しない場合、nullを返します。
	 *
	 * @param sess
	 *            現在のセッション
	 * @param attTypeId
	 *            属性タイプID
	 * @throws EIMException
	 * @throws Exception
	 *
	 * @return 属性タイプ値マスターのリスト
	 */
	public static AttributeValueMaster getAttributeValueMasterByAttTypeId(EIMSession sess,
			long attTypeId) throws Exception
	{
		// 属性タイプの取得
		EIMAttributeType attType = AttributeUtils.getAttributeTypeById(sess,attTypeId);
		if(attType == null) {
			return null;
		}

		// 属性タイプ値マスターのオブジェクト名(属性タイプID)
		String attMasterObjName = Long.toString(attTypeId);

		// 属性タイプ値マスターオブジェクトの取得
		EIMObject attMasterObj = ObjectUtils.getObjectByTypeAndName(sess,
				ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRMST")),
				attMasterObjName);

		AttributeValueMaster attValueMaster = null;
		if(attMasterObj != null){
			// 属性タイプ値マスターオブジェクトから定義値取得
			EIMAttribute intAttr = attMasterObj.getAttribute(EIMConfig.get("ATTR_NAME_ATTRMST_ATTR_NUM_LIST"));
			EIMAttribute doubleAttr = attMasterObj.getAttribute(EIMConfig.get("ATTR_NAME_ATTRMST_ATTR_DOUBLE_LIST"));
			EIMAttribute strAttr = attMasterObj.getAttribute(EIMConfig.get("ATTR_NAME_ATTRMST_ATTR_STR_LIST"));
			EIMAttribute dateAttr = attMasterObj.getAttribute(EIMConfig.get("ATTR_NAME_ATTRMST_ATTR_DATE_LIST"));
			EIMAttribute textAttr = attMasterObj.getAttribute(EIMConfig.get("ATTR_NAME_ATTRMST_ATTR_TEXT_LIST"));
			EIMAttribute colorAttr = attMasterObj.getAttribute(EIMConfig.get("ATTR_NAME_ATTRMST_ATTR_ATTRCOLOR_LIST"));
			EIMAttribute settingAttr = attMasterObj.getAttribute(EIMConfig.get("ATTR_NAME_ATTRMST_ATTR_ATTRSETTING_LIST"));

			long[] intValues = null;
			double[] doubleValues = null;
			String[] strValues = null;
			Date[] dateValues = null;
			String[] textValues = null;
			String[] colorValues = null;
			long[] settingValues = null;

			if(intAttr != null){
				intValues = TypeConvertUtils.convertToLongArray(intAttr.getInts());
			}
			if(strAttr != null){
				strValues = strAttr.getStrings();
			}
			if(dateAttr != null){
				dateValues = dateAttr.getDates();
			}
			if(textAttr != null){
				textValues = textAttr.getTexts();
			}
			if(colorAttr != null){
				colorValues = colorAttr.getStrings();
			}
			if(settingAttr != null){
				settingValues = TypeConvertUtils.convertToLongArray(settingAttr.getInts());
			}
			if(doubleAttr != null){
				doubleValues = doubleAttr.getDoubles();
			}

			// 属性タイプ値マスターの生成
			attValueMaster = new AttributeValueMaster(attMasterObj.getId(),attType,intValues,strValues,dateValues,textValues,colorValues,settingValues, doubleValues);
		}

		return attValueMaster;
	}

	/**
	 *
	 * 指定された属性タイプ値マスターに数値リストを登録します。<br>
	 *
	 * <li>指定された属性タイプ値マスターの数値リストの元の値は削除されます
	 * <li>数値リストが指定されなかった場合、属性マスターの数値リストは削除されます
	 *
	 * @param sess
	 *            現在のセッション
	 * @param attValueMaster
	 *            属性タイプ値マスター
	 * @param values
	 *            数値リスト
	 * @throws Exception
	 *
	 */
	public static void setIntAttributeValues(EIMSession sess,
			AttributeValueMaster attValueMaster, long[] values) throws Exception {
		// 型チェック
		if(attValueMaster.getType().getValueType().getId() != EIMValueType.INTEGER){
			throw new EIMException(sess,"EIM.ERROR.COMMON.API.INVALID.ATTRIBUTE.VALUE.TYPE");
		}

		// 属性タイプ値マスターオブジェクトの取得
		EIMObject attMasterObj = ObjectUtils.getObjectById(sess, attValueMaster.getId());
		if(attMasterObj == null){
			throw new EIMException(sess,"EIM.ERROR.COMMON.API.NO.ATTRIBUTE.MASTER.OBJECT");
		}

		// 属性タイプ「属性タイプ値マスター数値リスト」取得
		EIMAttributeType attTypeNum = AttributeUtils.getAttributeTypeByName(sess,EIMConfig.get("ATTR_NAME_ATTRMST_ATTR_NUM_LIST"));
		if(attTypeNum == null){
			throw new EIMException(sess,"EIM.ERROR.SYSTEMERROR");
		}

		// 属性マスターオブジェクトに数値リスト追加
		ObjectAttributeUtils.setAttribute(sess,attMasterObj,attTypeNum, TypeConvertUtils.convertToBuildTypeArray(values));
	}

	/**
	 *
	 * 指定された属性タイプ値マスターに数値リスト(ダブル型)を登録します。<br>
	 *
	 * <li>指定された属性タイプ値マスターの数値リストの元の値は削除されます
	 * <li>数値リストが指定されなかった場合、属性マスターの数値リストは削除されます
	 *
	 * @param sess
	 *            現在のセッション
	 * @param attValueMaster
	 *            属性タイプ値マスター
	 * @param values
	 *            数値リスト
	 * @throws Exception
	 *
	 */
	public static void setDoubleAttributeValues(EIMSession sess,
			AttributeValueMaster attValueMaster, double[] values) throws Exception {
		// 型チェック
		if(attValueMaster.getType().getValueType().getId() != EIMValueType.DOUBLE){
			throw new EIMException(sess,"EIM.ERROR.COMMON.API.INVALID.ATTRIBUTE.VALUE.TYPE");
		}

		// 属性タイプ値マスターオブジェクトの取得
		EIMObject attMasterObj = ObjectUtils.getObjectById(sess, attValueMaster.getId());
		if(attMasterObj == null){
			throw new EIMException(sess,"EIM.ERROR.COMMON.API.NO.ATTRIBUTE.MASTER.OBJECT");
		}

		// 属性タイプ「属性タイプ値マスター数値リスト」取得
		EIMAttributeType attTypeDouble = AttributeUtils.getAttributeTypeByName(sess,EIMConfig.get("ATTR_NAME_ATTRMST_ATTR_DOUBLE_LIST"));
		if(attTypeDouble == null){
			throw new EIMException(sess,"EIM.ERROR.SYSTEMERROR");
		}

		// 属性マスターオブジェクトに数値リスト追加
		ObjectAttributeUtils.setAttribute(sess,attMasterObj,attTypeDouble, values);
	}

	/**
	 *
	 * 指定された属性タイプ値マスターに文字列値リストを登録します。<br>
	 *
	 * <li>指定された属性タイプ値マスターの文字列値リストの元の値は削除されます
	 * <li>文字列値リストが指定されなかった場合、属性マスターの文字列値リストは削除されます
	 *
	 * @param sess
	 *            現在のセッション
	 * @param attValueMaster
	 *            属性タイプ値マスター
	 * @param values
	 *            文字列値リスト
	 * @throws Exception
	 *
	 */
	public static void setStrAttributeValues(EIMSession sess,
			AttributeValueMaster attValueMaster, String[] values) throws Exception {
		// 型チェック
		if(attValueMaster.getType().getValueType().getId() != EIMValueType.STRING){
			throw new EIMException(sess,"EIM.ERROR.COMMON.API.INVALID.ATTRIBUTE.VALUE.TYPE");
		}

		// 属性タイプ値マスターオブジェクトの取得
		EIMObject attMasterObj = ObjectUtils.getObjectById(sess, attValueMaster.getId());
		if(attMasterObj == null){
			throw new EIMException(sess,"EIM.ERROR.COMMON.API.NO.ATTRIBUTE.MASTER.OBJECT");
		}

		// 属性タイプ「属性タイプ値マスター文字列値リスト」取得
		EIMAttributeType attTypeStr = AttributeUtils.getAttributeTypeByName(sess,EIMConfig.get("ATTR_NAME_ATTRMST_ATTR_STR_LIST"));
		if(attTypeStr == null){
			throw new EIMException(sess,"EIM.ERROR.SYSTEMERROR");
		}

		// 属性マスターオブジェクトに文字列値リスト追加
		ObjectAttributeUtils.setAttribute(sess,attMasterObj,attTypeStr, values);
	}
	/**
	 *
	 * 指定された属性タイプ値マスターに日付値リストを登録します。<br>
	 *
	 * <li>指定された属性タイプ値マスターの日付値リストの元の値は削除されます
	 * <li>日付値リストが指定されなかった場合、属性マスターの日付値リストは削除されます
	 *
	 * @param sess
	 *            現在のセッション
	 * @param attValueMaster
	 *            属性タイプ値マスター
	 * @param values
	 *            日付値リスト
	 * @throws Exception
	 *
	 */
	public static void setDateAttributeValues(EIMSession sess,
			AttributeValueMaster attValueMaster, Date[] values) throws Exception {
		// 型チェック
		if(attValueMaster.getType().getValueType().getId() != EIMValueType.DATE){
			throw new EIMException(sess,"EIM.ERROR.COMMON.API.INVALID.ATTRIBUTE.VALUE.TYPE");
		}

		// 属性タイプ値マスターオブジェクトの取得
		EIMObject attMasterObj = ObjectUtils.getObjectById(sess, attValueMaster.getId());
		if(attMasterObj == null){
			throw new EIMException(sess,"EIM.ERROR.COMMON.API.NO.ATTRIBUTE.MASTER.OBJECT");
		}

		// 属性タイプ「属性タイプ値マスター日付値リスト」取得
		EIMAttributeType attTypeDate = AttributeUtils.getAttributeTypeByName(sess,EIMConfig.get("ATTR_NAME_ATTRMST_ATTR_DATE_LIST"));
		if(attTypeDate == null){
			throw new EIMException(sess,"EIM.ERROR.SYSTEMERROR");
		}

		// 属性マスターオブジェクトに日付値リスト追加
		ObjectAttributeUtils.setAttribute(sess,attMasterObj,attTypeDate, values);
	}
	/**
	 *
	 * 指定された属性タイプ値マスターにテキスト値リストを登録します。<br>
	 *
	 * <li>指定された属性タイプ値マスターのテキスト値リストの元の値は削除されます
	 * <li>文字列値リストが指定されなかった場合、属性マスターのテキスト値リストは削除されます
	 *
	 * @param sess
	 *            現在のセッション
	 * @param attValueMaster
	 *            属性タイプ値マスター
	 * @param values
	 *            テキスト値リスト
	 * @throws Exception
	 * @throws EIMException
	 *
	 */
	public static void setTextAttributeValues(EIMSession sess,
			AttributeValueMaster attValueMaster, String[] values) throws Exception {
		// 型チェック
		if(attValueMaster.getType().getValueType().getId() != EIMValueType.TEXT){
			throw new EIMException(sess,"EIM.ERROR.COMMON.API.INVALID.ATTRIBUTE.VALUE.TYPE");
		}

		// 属性タイプ値マスターオブジェクトの取得
		EIMObject attMasterObj = ObjectUtils.getObjectById(sess, attValueMaster.getId());
		if(attMasterObj == null){
			throw new EIMException(sess,"EIM.ERROR.COMMON.API.NO.ATTRIBUTE.MASTER.OBJECT");
		}

		// 属性タイプ「属性タイプ値マスターテキスト値リスト」取得
		EIMAttributeType attTypeText = AttributeUtils.getAttributeTypeByName(sess,EIMConfig.get("ATTR_NAME_ATTRMST_ATTR_TEXT_LIST"));
		if(attTypeText == null){
			throw new EIMException(sess,"EIM.ERROR.SYSTEMERROR");
		}

		// 属性マスターオブジェクトにテキスト値リスト追加
		ObjectAttributeUtils.setAttribute(sess,attMasterObj,attTypeText, values);
	}

	/**
	 *
	 * 指定された属性タイプ値マスターに表示色リストを登録します。<br>
	 *
	 * <li>指定された属性タイプ値マスターの表示色リストの元の値は削除されます
	 * <li>表示色リストが指定されなかった場合、属性マスターの表示色リストは削除されます
	 *
	 * @param sess
	 *            現在のセッション
	 * @param attValueMaster
	 *            属性タイプ値マスター
	 * @param settingList
	 *            表示色リスト（中身はString）
	 * @throws Exception
	 *
	 */
	public static void setColorAttributeValues(EIMSession sess,
			AttributeValueMaster attValueMaster, List colorList) throws Exception
	{
		String[] values =
			(String[])colorList.toArray(new String[colorList.size()]);

		// 属性タイプ値マスターオブジェクトの取得
		EIMObject attMasterObj = ObjectUtils.getObjectById(sess, attValueMaster.getId());
		if(attMasterObj == null){
			throw new EIMException(sess,"EIM.ERROR.COMMON.API.NO.ATTRIBUTE.MASTER.OBJECT");
		}

		// 属性タイプ「属性タイプ値マスター表示色リスト」取得
		EIMAttributeType attTypeColor = AttributeUtils.getAttributeTypeByName(sess,EIMConfig.get("ATTR_NAME_ATTRMST_ATTR_ATTRCOLOR_LIST"));
		if(attTypeColor == null){
			throw new EIMException(sess,"EIM.ERROR.SYSTEMERROR");
		}

		// 属性マスターオブジェクトに数値リスト追加
		ObjectAttributeUtils.setAttribute(sess,attMasterObj,attTypeColor, values);
	}

	/**
	 *
	 * 指定された属性タイプ値マスターに表示設定リストを登録します。<br>
	 *
	 * <li>指定された属性タイプ値マスターの表示設定リストの元の値は削除されます
	 * <li>表示設定リストが指定されなかった場合、属性マスターの表示設定リストは削除されます
	 *
	 * @param sess
	 *            現在のセッション
	 * @param attValueMaster
	 *            属性タイプ値マスター
	 * @param settingList
	 *            表示設定リスト（中身はInteger）
	 * @throws Exception
	 *
	 */
	public static void setSettingAttributeValues(EIMSession sess, AttributeValueMaster attValueMaster, List settingList) throws Exception
	{
		long[] values = new long[settingList.size()];

		for (int i = 0; i < settingList.size(); i++) {
			values[i] = ((Long)settingList.get(i)).longValue();
		}

		// 属性タイプ値マスターオブジェクトの取得
		EIMObject attMasterObj = ObjectUtils.getObjectById(sess, attValueMaster.getId());
		if(attMasterObj == null){
			throw new EIMException(sess,"EIM.ERROR.COMMON.API.NO.ATTRIBUTE.MASTER.OBJECT");
		}

		// 属性タイプ「属性タイプ値マスター数値リスト」取得
		EIMAttributeType attTypeSetting = AttributeUtils.getAttributeTypeByName(sess,EIMConfig.get("ATTR_NAME_ATTRMST_ATTR_ATTRSETTING_LIST"));
		if(attTypeSetting == null){
			throw new EIMException(sess,"EIM.ERROR.SYSTEMERROR");
		}

		// 属性マスターオブジェクトに数値リスト追加
		ObjectAttributeUtils.setAttribute(sess,attMasterObj,attTypeSetting, TypeConvertUtils.convertToBuildTypeArray(values));
	}

	/**
	 * 指定された属性リストがそれぞれ属性タイプ値マスターを持つか否かを判定します。
	 *
	 * @param sess 現在のセッション
	 * @param attTypeList 属性タイプのリスト
	 * @return 属性タイプ値マスターが存在する属性の属性IDをInteger型で格納したHashSet
	 * @throws Exception
	 */
	public static HashSet getMasterExistSetByAttTypeIds(EIMSession sess, List attTypeList) throws Exception {

		HashSet masterExistSet = new HashSet();

		if (attTypeList != null && attTypeList.size() > 0) {

			// 属性IDのString配列を作成
			String[] attTypeIds = new String[attTypeList.size()];
			for (int i = 0 ; attTypeList.size() > i ; i++) {
				EIMAttributeType attrType = (EIMAttributeType)attTypeList.get(i);
				attTypeIds[i] = String.valueOf(attrType.getId());
			}

			// 検索条件項目・返却項目指定オブジェクトを生成
			EIMSearchSelectEIMObject selectTarget = new EIMSearchSelectEIMObject();

			// 検索条件グループ作成
			EIMSearchConditionGroup conds = new EIMSearchConditionGroup(EIMSearchOperatorEnum.AND);

			// 「属性タイプ値マスター」のオブジェクトタイプ取得
			EIMObjectType objType  = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRMST"));

			// 条件1:対象のオブジェクトタイプを検索
			EIMAttributeType fieldOfObjType = EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.TYPE;
			EIMSearchConditionCompare cond1 = new EIMSearchConditionCompare(
					EIMSearchOperatorEnum.AND		//前方演算子AND
					, fieldOfObjType				//オブジェクトタイプ
					, EIMSearchOperatorEnum.EQ		// =
					, objType.getId() 				//「属性タイプ値マスター」のオブジェクトタイプID
				);
			// 条件1を条件グループに加える
			conds.addCondition(cond1);

			// 条件2:対象のオブジェクト名を検索
			EIMAttributeType fieldOfObjName = EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.NAME;
			EIMSearchConditionIn cond2 = new EIMSearchConditionIn(
					EIMSearchOperatorEnum.AND		//前方演算子AND
					, fieldOfObjName				//オブジェクト名
					, EIMSearchOperatorEnum.IN		// in
					, attTypeIds 					//'<属性IDのString配列>'
				);
			// 条件2を条件グループに加える
			conds.addCondition(cond2);

			// 検索条件項目・返却項目指定インスタンスに条件グループを設定
			selectTarget.setCondition(conds);

			// 検索実行
			selectTarget.setRole(EIMAccessRole.READ);
			List retList = SearchUtils.searchObjects(sess, selectTarget, null);

			// DBに存在する属性タイプ値マスターのオブジェクト名(＝属性タイプID)をHashSetに格納
			for (Iterator iter = retList.iterator(); iter.hasNext();) {
				EIMObject attMasterObj = (EIMObject) iter.next();
				masterExistSet.add(new Long(attMasterObj.getName()));
			}
		}
		return masterExistSet;
	}
}