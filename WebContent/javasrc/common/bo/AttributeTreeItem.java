package common.bo;

import eim.bo.EIMAttributeType;


/**
 * 
 * 属性ツリー所属属性のオブジェクトクラス
 *
 * @author Jinya.Noguchi
 * @version 1.0
 */
public class AttributeTreeItem {

	/**
	 * 属性ツリー所属属性のオブジェクトIDを格納するフィールド
	 */	
	private long _id = 0;
	
	/**
	 * 属性タイプ情報を格納するフィールド
	 */
	private EIMAttributeType _attType = null;

	/**
	 * 「属性なし」も表示するか否かのフラグ（false:しない /true:する）
	 */
	private boolean _viewNoValuesFlag;
	
	/**
	 * ポジション
	 * ※表示の時のみ使用
	 */
	private long _position = Integer.MIN_VALUE;
	

	/**
	 * 
	 * コンストラクタ
	 * @param id 属性ツリー所属属性のオブジェクトID
	 * @param attType 属性タイプ情報
	 * @param viewNoValuesFlag 「属性なし」表示フラグ（false:しない /true:する）
	 */		
	public AttributeTreeItem(long id,
			                  EIMAttributeType attType,
			                  boolean viewNoValuesFlag)
	{
		this._id = id;
		this._attType = attType;
		this._viewNoValuesFlag = viewNoValuesFlag;
	}

	/**
	 * 
	 * コンストラクタ
     * ※表示の時のみ使用(ポジションが含まれるため)
	 * @param id 属性ツリー所属属性のオブジェクトID
	 * @param attType 属性タイプ情報
	 * @param viewNoValuesFlag 「属性なし」表示フラグ（false:しない /true:する）
	 * @param position ポジション
	 */		
	public AttributeTreeItem(long id,
			                  EIMAttributeType attType,
			                  boolean viewNoValuesFlag,
			                  long position)
	{
		this._id = id;
		this._attType = attType;
		this._viewNoValuesFlag = viewNoValuesFlag;
		this._position = position;
	}
	
	/**
	* 属性ツリー所属属性のオブジェクトID取得
	* @return オブジェクトID
	*/
	public long getId()
	{
		return this._id;
	}
	
	/**
	* 属性タイプ情報取得
	* @return 属性タイプ情報
	*/
	public EIMAttributeType getType()
	{
		return this._attType;
	}
	
	/**
	* 「属性なし」表示フラグ取得
	* @return 表示フラグ（false:しない /true:する）
	*/
	public boolean isViewNoValues()
	{
		return this._viewNoValuesFlag;
	}
	
	/**
	* 「属性なし」表示フラグ取得
	* @return 表示フラグ（false:しない /true:する）
	*/
	public long getPosition()
	{
		return this._position;
	}
		
}
