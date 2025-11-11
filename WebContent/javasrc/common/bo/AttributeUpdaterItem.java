package common.bo;

/**
 * 
 * 属性更新情報のアイテムを管理するクラス
 * 
 */
public class AttributeUpdaterItem {

	/**
	 * 属性タイプIDです。
	 */
	private long _attTypeId = Long.MIN_VALUE;

	/**
	 * 属性値の配列です。
	 */
	private String[] _attValues = null;

	/**
	 * 名称割当て属性フラグ(true：割当対象/false：割当対象以外)です。
	 */
	private boolean _nameAllocate = false;

	/**
	 * 下位引継ぎ属性フラグ(true：引継対象/false：引継対象以外)です。
	 */
	private boolean _lowerSuccession= false;

	/**
	 * コンストラクタ
	 * 
	 */
	public AttributeUpdaterItem(){}
	
	/**
	 * コンストラクタ
	 * 
	 * @param attTypeId 属性タイプID
	 * @param attValues 属性値の配列
	 * @param nameAllocate 名称割当て属性フラグ
	 * @param lowerSuccession 下位引継ぎ属性フラグ
	 */
	public AttributeUpdaterItem(long attTypeId, String[] attValues, boolean nameAllocate, boolean lowerSuccession){
		
		_attTypeId = attTypeId;
		_attValues = attValues;
		_nameAllocate = nameAllocate;
		_lowerSuccession = lowerSuccession;
	}
	
	/**
	 * 属性タイプIDを返します。
	 * 
	 * @return 属性タイプID
	 */
	public long getAttTypeId() {
		return _attTypeId;
	}

	/**
	 * 属性タイプIDを設定します。
	 * 
	 * @param attTypeId 属性タイプID
	 */
	public void setAttTypeId(long attTypeId) {
		this._attTypeId = attTypeId;
	}

	/**
	 * 属性値の配列を返します。
	 * 
	 * @return 属性値の配列
	 */
	public String[] getAttValues() {
		return _attValues;
	}

	/**
	 * 属性値の配列を設定します。
	 * 
	 * @param attValues	属性値の配列
	 */
	public void setAttValues(String[] attValues) {
		this._attValues = attValues;
	}

	/**
	 * 名称割当て属性フラグを返します。
	 * 
	 * @return 名称割当て属性フラグ(true：割当対象/false：割当対象以外)
	 */
	public boolean isNameAllocate() {
		return _nameAllocate;
	}

	/**
	 * 名称割当て属性フラグを設定します。
	 * 
	 * @param nameAllocate 名称割当て属性フラグ(true：割当対象/false：割当対象以外)
	 */
	public void setNameAllocate(boolean nameAllocate) {
		this._nameAllocate = nameAllocate;
	}

	/**
	 * 下位引継ぎ属性フラグを返します。
	 * 
	 * @return 下位引継ぎ属性フラグ(true：引継対象/false：引継対象以外)
	 */
	public boolean isLowerSuccession() {
		return _lowerSuccession;
	}

	/**
	 * 下位引継ぎ属性フラグを設定します。
	 * 
	 * @param lowerSuccession 下位引継ぎ属性フラグ(true：引継対象/false：引継対象以外)
	 */
	public void setLowerSuccession(boolean lowerSuccession) {
		this._lowerSuccession = lowerSuccession;
	}
}