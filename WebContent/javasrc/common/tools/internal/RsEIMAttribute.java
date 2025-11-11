package common.tools.internal;

import java.sql.Timestamp;

/**
 * EIMAttributeを生成する為に必要な情報を一時的に保管しておくクラス。
 *
 */
class RsEIMAttribute {
	/**
	 * 属性タイプ
	 */
	long _attrType;

	/**
	 * 数値
	 */
	long _intValue;

	/**
	 * 数値
	 */
	double _doubleValue;

	/**
	 * 文字列値
	 */
	String _strValue;

	/**
	 * 日付値
	 */
	Timestamp _dateValue;

	/**
	 * テキスト値
	 */
	String _textValue;

	/**
	 * コンストラクタ
	 *
	 * @param attrType EIMOBJテーブルの該当カラム
	 * @param intValue EIMOBJINTテーブルの該当カラム
	 * @param strValue EIMOBJSTRテーブルの該当カラム
	 * @param dateValue EIMOBJDATEテーブルの該当カラム
	 * @param textValue EIMOBJTEXTテーブルの該当カラム
	 */
	RsEIMAttribute(long attrType, long intValue, String strValue, Timestamp dateValue,
			String textValue, double doubleValue) {
		_attrType = attrType;
		_intValue = intValue;
		_strValue = strValue;
		_dateValue = dateValue;
		_textValue = textValue;
		_doubleValue = doubleValue;

	}

}