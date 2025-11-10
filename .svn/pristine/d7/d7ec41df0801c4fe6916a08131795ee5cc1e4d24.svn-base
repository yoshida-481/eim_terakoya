package common.util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import common.enumeration.UIControlTypeByValueTypeAndMultipleEnum;

import jp.co.ctc_g.eim.app.form.business.domain.UIControlConfDomain;
import jp.co.ctc_g.eim.app.form.common.enumeration.UIControlTypeEnum;

/**
 * 
 * UIコントロールマスタ情報に関するユーティリティクラス
 *
 */
public class UIControlUtil {
	
	/**
	 * 引数のリストを基に、属性データ型・多重度毎のUIコントロールマスタ情報を返却します。
	 * 
	 * @param uIControlConfDomain UIコントロールマスタ定義情報リスト
	 * @return 属性データ型・多重度毎に分割したUIコントロールマスタ定義情報リストのマップ
	 * (key:属性データ型名称 + "_" + 多重度 value:UIコントロール定義情報リスト)
	 * @throws Exception
	 * @since Ver6.0
	 */
	public static Map<String, List<UIControlConfDomain>> getListDividedIntoValueTypeAndMultiple(
			List<UIControlConfDomain> uIControlConfDomainList) throws Exception {
		
		// UIコントロールマップ
		// key:UIコントロールタイプ名称 value:UIコントロール定義情報リスト
		Map<String, List<UIControlConfDomain>> uIControlMap = convertUIControlListToMap(uIControlConfDomainList);
		
		// 返却用属性タイプ・UIコントロールマップ
		// key:属性データ型名称 value:UIコントロール定義情報リスト
		Map<String, List<UIControlConfDomain>> returnMap = new HashMap<String, List<UIControlConfDomain>>();
		
		// 数値型・単数/複数
		setUIControlConfListMap(UIControlTypeByValueTypeAndMultipleEnum.LONG_SINGLE,
				AppConstant.UICONTROL_VALUE_TYPE_NAME_LONG, false, uIControlMap, returnMap);
		setUIControlConfListMap(UIControlTypeByValueTypeAndMultipleEnum.LONG_MULTIPLE,
				AppConstant.UICONTROL_VALUE_TYPE_NAME_LONG, true, uIControlMap, returnMap);
		
		// 文字列型・単数/複数
		setUIControlConfListMap(UIControlTypeByValueTypeAndMultipleEnum.STRING_SINGLE,
				AppConstant.UICONTROL_VALUE_TYPE_NAME_STRING, false, uIControlMap, returnMap);
		setUIControlConfListMap(UIControlTypeByValueTypeAndMultipleEnum.STRING_MULTIPLE,
				AppConstant.UICONTROL_VALUE_TYPE_NAME_STRING, true, uIControlMap, returnMap);
		
		// 日付型・単数/複数
		setUIControlConfListMap(UIControlTypeByValueTypeAndMultipleEnum.DATE_SINGLE,
				AppConstant.UICONTROL_VALUE_TYPE_NAME_DATE, false, uIControlMap, returnMap);
		setUIControlConfListMap(UIControlTypeByValueTypeAndMultipleEnum.DATE_MULTIPLE,
				AppConstant.UICONTROL_VALUE_TYPE_NAME_DATE, true, uIControlMap, returnMap);
		
		// テキスト型・単数/複数
		setUIControlConfListMap(UIControlTypeByValueTypeAndMultipleEnum.TEXT_SINGLE,
				AppConstant.UICONTROL_VALUE_TYPE_NAME_TEXT, false, uIControlMap, returnMap);
		setUIControlConfListMap(UIControlTypeByValueTypeAndMultipleEnum.TEXT_MULTIPLE,
				AppConstant.UICONTROL_VALUE_TYPE_NAME_TEXT, true, uIControlMap, returnMap);
		
		// 実数型・単数/複数
		setUIControlConfListMap(UIControlTypeByValueTypeAndMultipleEnum.DOUBLE_SINGLE,
				AppConstant.UICONTROL_VALUE_TYPE_NAME_DOUBLE, false, uIControlMap, returnMap);
		setUIControlConfListMap(UIControlTypeByValueTypeAndMultipleEnum.DOUBLE_MULTIPLE,
				AppConstant.UICONTROL_VALUE_TYPE_NAME_DOUBLE, true, uIControlMap, returnMap);
		
		// オブジェクト型・単数/複数
		setUIControlConfListMap(UIControlTypeByValueTypeAndMultipleEnum.OBJECT_SINGLE,
				AppConstant.UICONTROL_VALUE_TYPE_NAME_OBJECT, false, uIControlMap, returnMap);
		setUIControlConfListMap(UIControlTypeByValueTypeAndMultipleEnum.OBJECT_MULTIPLE,
				AppConstant.UICONTROL_VALUE_TYPE_NAME_OBJECT, true, uIControlMap, returnMap);
		
		// ユーザ型・単数/複数
		setUIControlConfListMap(UIControlTypeByValueTypeAndMultipleEnum.USER_SINGLE,
				AppConstant.UICONTROL_VALUE_TYPE_NAME_USER, false, uIControlMap, returnMap);
		setUIControlConfListMap(UIControlTypeByValueTypeAndMultipleEnum.USER_MULTIPLE,
				AppConstant.UICONTROL_VALUE_TYPE_NAME_USER, true, uIControlMap, returnMap);
		
		// コード型・単数/複数
		setUIControlConfListMap(UIControlTypeByValueTypeAndMultipleEnum.CODE_SINGLE,
				AppConstant.UICONTROL_VALUE_TYPE_NAME_CODE, false, uIControlMap, returnMap);
		setUIControlConfListMap(UIControlTypeByValueTypeAndMultipleEnum.CODE_MULTIPLE,
				AppConstant.UICONTROL_VALUE_TYPE_NAME_CODE, true, uIControlMap, returnMap);
		
		return returnMap;
	}
	
	/**
	 * UIコントロール定義情報リストを UIコントロールタイプ名称をキーとするマップに変換します。
	 * 
	 * @param uIControlConfDomainList  UIコントロール定義情報リスト
	 * @return UIコントロールタイプ名称をキーとするマップ(key:UIコントロールタイプ名称 value:UIコントロール定義情報リスト)
	 * @since Ver6.0
	 */
	private static Map<String, List<UIControlConfDomain>> convertUIControlListToMap(List<UIControlConfDomain> uIControlConfDomainList){
		
		Map<String, List<UIControlConfDomain>> returnMap = new HashMap<String, List<UIControlConfDomain>>();
		
		// UIコントロールタイプ名称をキーとするUIControlConfDomainのマップを生成
		for(UIControlConfDomain uIControlConfDomain : uIControlConfDomainList){
			
			UIControlTypeEnum type = uIControlConfDomain.getTypeEnum();
			String typeName = type.name();
			
			if( returnMap.containsKey(typeName) ){
				// すでにマップに該当キー設定済みの場合
				// 値のリストにUIコントロール定義情報を追加
				returnMap.get(typeName).add(uIControlConfDomain);
			} else {
				// マップに該当キー未設定の場合
				// キー、UIコントロール定義情報リストを設定
				List<UIControlConfDomain> valueList = new ArrayList<UIControlConfDomain>();
				valueList.add(uIControlConfDomain);
				returnMap.put(typeName, valueList);
			}
		}
		
		return returnMap;
	}
	
	/**
	 * データ型・多重度で選択可能なUIコントロールマスタの情報を扱う列挙型をもとに
	 * 紐づくUIコントロール定義情報リストを取得し、引数のマップに追加します。<br>
	 * 紐づくUIコントロール定義情報がない場合は、空のリストを値に設定します。<br>
	 * 
	 * @param uIControlTypeEnum データ型・多重度で選択可能なUIコントロールマスタの情報を扱う列挙型
	 * @param uIControlValueTypeName UIコントロール属性データ型名称
	 * @param uIControlIsMultiple 多重度の有無(true:あり、false:なし)
	 * @param uIControlMap UIコントロールマップ(key:UIコントロールタイプ名称 value:UIコントロール定義情報リスト)
	 * @param map リストを追加するマップ(key:UIコントロール属性データ型名称 + "_" + 多重度 value:UIコントロール定義情報リスト)
	 * @since Ver6.0
	 */
	private static void setUIControlConfListMap(
			UIControlTypeByValueTypeAndMultipleEnum uIControlTypeEnum, 
			String uIControlValueTypeName,
			Boolean uIControlIsMultiple,
			Map<String, List<UIControlConfDomain>> uIControlMap,
			Map<String, List<UIControlConfDomain>> map){
		
		List<UIControlConfDomain> valueList = new ArrayList<UIControlConfDomain>();
		
		// UIコントロールタイプ名称をもとに紐づくUIコントロールタイプリストを取得
		String[] uIControlTypeNameList = uIControlTypeEnum.getUiControlTypeNameList();
		
		for(String uIControlTypeName : uIControlTypeNameList){
			
			// タイプ名称をもとに、紐づくUIコントロール定義情報を取得し、追加
			List<UIControlConfDomain> list = uIControlMap.get(uIControlTypeName);
			
			if(list != null){
				valueList.addAll(list);
			}
		}
		
		// キー文字列の生成（UIコントロール属性データ型名称 + "_" + 多重度(single/multiple)）
		String key = uIControlValueTypeName + AppConstant.DELIMITER_UNDER_SCORE;
		if(uIControlIsMultiple){
			// 多重度有りの場合
			key += AppConstant.UICONTROL_MULTIPLE;
		} else {
			// 多重度無しの場合
			key += AppConstant.UICONTROL_SINGLE;
		}
		map.put(key, valueList);
	}
}