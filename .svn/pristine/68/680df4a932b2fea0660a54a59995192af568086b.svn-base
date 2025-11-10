package common.util;

import java.util.HashMap;
import java.util.Map;

import eim.bo.EIMResource;
import eim.bo.EIMValueType;
import eim.net.EIMSession;
import jp.co.ctc_g.eim.framework2.common.enumeration.ValueTypeEnum;

/**
 * 属性データ型関連の共通クラス
 */
public class AppValueTypeUtil {
	
	// 属性データ型名称Map
	private static Map<String, Map<Integer, String>> valueTypeNameListMap = 
		new HashMap<String, Map<Integer, String>>();
	
	/**
	 * 属性データ型の名称を返却します。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param valueType 属性データ型列挙型
	 * @return データ型名称
	 */
	public static String getValueTypeName(EIMSession sess, int valueTypeId) throws Exception {
		
		// 返却値
		String retName = "";
		
		if (valueTypeId == 0) {
			return retName;
		}
		
		// 指定した属性データ型の名称のMap情報が存在する場合
		if (valueTypeNameListMap.containsKey(sess.getLangId())
				&& valueTypeNameListMap.get(sess.getLangId()).containsKey(valueTypeId)) {
			
			// 属性データ型セッション言語名称を取得
			retName = valueTypeNameListMap.get(sess.getLangId()).get(valueTypeId);
			
		} else {
			
			// 属性データ型セッション言語名称を取得して、Mapに情報を追加
			retName = getValueTypeNameAndAddMap(sess, valueTypeId);
		}
		
		return retName;
	}
	
	private static String getValueTypeNameAndAddMap(EIMSession sess, int valueTypeId) throws Exception {
		
		// 返却値
		String retName = "";
		
		// V4以前の列挙型ドメインを取得
		// ※現行V5以降のドメインではデータ型名称を保持していない
		EIMValueType valueTpeEnum = EIMValueType.getTypeById(sess, valueTypeId);
		
		// 属性データ型のセッション言語名称をマッピング
		if (valueTpeEnum != null) {
			
			// 属性データ型名称Mapに情報を追加
			Map<Integer, String> nameMap = new HashMap<Integer, String>();
			nameMap.put(valueTpeEnum.getId(), valueTpeEnum.getName());
			valueTypeNameListMap.put(sess.getLangId(), nameMap);
			
			retName = valueTpeEnum.getName();
			
		} else {
			
			ValueTypeEnum targetValTypeEnum = ValueTypeEnum.getByValue(valueTypeId);
			
			// オブジェクト型、ユーザ型、コード型
			switch (targetValTypeEnum) {
				case OBJECT:
					retName = EIMResource.getMessage(sess, "EIM.VALUE.TYPE.NAME.OBJECT");
					break;
				case USER:
					retName = EIMResource.getMessage(sess, "EIM.VALUE.TYPE.NAME.USER");
					break;
				case CODE:
					retName = EIMResource.getMessage(sess, "EIM.VALUE.TYPE.NAME.CODE");
					break;
			}
		}
		
		return retName;
	}
}