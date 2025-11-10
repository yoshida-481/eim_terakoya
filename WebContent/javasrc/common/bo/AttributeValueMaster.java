package common.bo;

import java.util.Date;

import eim.bo.EIMAttributeType;
import eim.bo.EIMException;
import eim.bo.EIMValueType;

/**
 *
 * 属性タイプ値マスターのオブジェクトクラス
 *
 */
public class AttributeValueMaster {

	/**
	 * 属性タイプ値マスターのオブジェクトIDを格納するフィールド
	 */
	private long _id = 0;

	/**
	 * 属性タイプ情報を格納するフィールド
	 */
	private EIMAttributeType _attType = null;

	/**
	 * 属性タイプ値マスター数値リストを格納するフィールド
	 */
	private long[] _intValues = null;

	/**
	 * 属性タイプ値マスター数値リストを格納するフィールド
	 */
	private double[] _doubleValues = null;

	/**
	 * 属性タイプ値マスター文字列値リストを格納するフィールド
	 */
	private String[] _strValues = null;

	/**
	 * 属性タイプ値マスター日付値リストを格納するフィールド
	 */
	private Date[] _dateValues = null;

	/**
	 * 属性タイプ値マスターテキスト値リストを格納するフィールド
	 */
	private String[] _textValues = null;

	/**
	 * 属性タイプ値マスター表示色リストを格納するフィールド
	 */
	private String[] _colorValues = null;

	/**
	 * 属性タイプ値マスター表示設定リストを格納するフィールド
	 */
	private long[] _settingValues = null;

	/**
	 *
	 * コンストラクタ
	 * @param id 属性タイプ値マスターのオブジェクトID
	 * @param attType 属性タイプ情報
	 * @param intValues 数値リスト
	 * @param strValues 文字列値リスト
	 * @param dateValues 日付値リスト
	 * @param textValues テキスト値リスト
	 * @param colorValues 表示色リスト
	 * @param settingValues 表示設定リスト
	 */
	public AttributeValueMaster(long id,
			                     EIMAttributeType attType,
			                     long[] intValues,
			                     String[] strValues,
			                     Date[] dateValues,
			                     String[] textValues,
			                     String[] colorValues,
			                     long[] settingValues,
			                     double[] doubleValues)
	{
		_id = id;
		_attType = attType;
		_intValues = intValues;
		_strValues = strValues;
		_dateValues = dateValues;
		_textValues = textValues;
		_colorValues = colorValues;
		_settingValues = settingValues;
		_doubleValues = doubleValues;
	}

	/**
	* 属性タイプ値マスターのオブジェクトIDを取得
	* @return オブジェクトID
	*/
	public long getId()
	{
		return _id;
	}

	/**
	* 属性タイプ情報取得
	* @return 属性タイプ情報
	*/
	public EIMAttributeType getType()
	{
		return _attType;
	}

	/**
	* 数値リスト取得
	* @return 数値リスト
	*
	* @throws Exception
	*             マスター値が設定されていない場合
	*/
	public long[] getInts() throws Exception
	{
		if(_intValues == null)
		{
			throw new EIMException(null,"EIM.ERROR.COMMON.API.NO.ATTRIBUTE.MASTER.VALUE");
		}
		return _intValues;
	}

	/**
	 * 数値リスト取得(double)
	 * @return 数値リスト
	 *
	 * @throws Exception
	 *             マスター値が設定されていない場合
	 */
	public double[] getDoubles() throws Exception
	{
		if(_doubleValues == null)
		{
			throw new EIMException(null,"EIM.ERROR.COMMON.API.NO.ATTRIBUTE.MASTER.VALUE");
		}
		return _doubleValues;
	}

	/**
	* 文字列値リスト取得
	* @return 文字列値リスト
	*
	* @throws Exception
	*             マスター値が設定されていない場合
	*/
	public String[] getStrings() throws Exception
	{
		if(_strValues == null)
		{
			throw new EIMException(null,"EIM.ERROR.COMMON.API.NO.ATTRIBUTE.MASTER.VALUE");
		}
		return _strValues;
	}

	/**
	* 日付値リスト取得
	* @return 日付値リスト
	*
	* @throws Exception
	*             マスター値が設定されていない場合
	*/
	public Date[] getDates() throws Exception
	{
		if (_dateValues == null)
		{
			throw new EIMException(null,"EIM.ERROR.COMMON.API.NO.ATTRIBUTE.MASTER.VALUE");
		}
		return _dateValues;
	}

	/**
	* テキスト値リスト取得
	* @return テキスト値リスト
	*
	* @throws Exception
	*             マスター値が設定されていない場合
	*/
	public String[] getTexts() throws Exception
	{
		if (_textValues == null)
		{
			throw new EIMException(null,"EIM.ERROR.COMMON.API.NO.ATTRIBUTE.MASTER.VALUE");
		}
		return _textValues;
	}

	/**
	* 表示色リスト取得
	* @return 表示色リスト
	*
	* @throws Exception
	*             マスター値が設定されていない場合
	*/
	public String[] getColors() throws Exception
	{
		if(_colorValues == null)
		{
//			throw new EIMException(null,"EIM.ERROR.COMMON.API.NO.ATTRIBUTE.MASTER.VALUE");

			//2009/05/07 fujii modified
			//nullの場合はエラーではなく「表示色なし」として処理を行うことにする。
			//表示色リストは本バージョンで初めて作成され、バージョンアップ前の
			//アプリケーションには存在しないため。

			//返却するリストの長さを取得する
			int length = 1;
			switch(_attType.getValueType().getId()) {
				// 数値型
				case EIMValueType.INTEGER:
					length = _intValues.length;
					break;
				case EIMValueType.STRING:
					length = _strValues.length;
					break;
				case EIMValueType.DATE:
					length = _dateValues.length;
					break;
				case EIMValueType.TEXT:
					length = _textValues.length;
					break;
				case EIMValueType.DOUBLE:
					length = _doubleValues.length;
					break;
			}

			_colorValues = new String[length];
			for(int ii = 0; ii < _colorValues.length; ii++) {
				_colorValues[ii] = "-";
			}
		}
		return _colorValues;
	}

	/**
	* 表示設定リスト取得
	* @return 表示設定リスト
	*
	* @throws Exception
	*             マスター値が設定されていない場合
	*/
	public long[] getSettings() throws Exception
	{
		if(_settingValues == null)
		{
//			throw new EIMException(null,"EIM.ERROR.COMMON.API.NO.ATTRIBUTE.MASTER.VALUE");

			//2009/05/07 fujii modified
			//nullの場合はエラーではなく「表示設定なし」として処理を行うことにする。
			//表示設定リストは本バージョンで初めて作成され、バージョンアップ前の
			//アプリケーションには存在しないため。

			//返却するリストの長さを取得する
			int length = 1;
			switch(_attType.getValueType().getId()) {
				// 数値型
				case EIMValueType.INTEGER:
					length = _intValues.length;
					break;
				case EIMValueType.STRING:
					length = _strValues.length;
					break;
				case EIMValueType.DATE:
					length = _dateValues.length;
					break;
				case EIMValueType.TEXT:
					length = _textValues.length;
					break;
				case EIMValueType.DOUBLE:
					length = _doubleValues.length;
					break;
			}

			_settingValues = new long[length];
			for(int ii = 0; ii < _settingValues.length; ii++) {
				_settingValues[ii] = 0;
			}
		}
		return _settingValues;
	}

}