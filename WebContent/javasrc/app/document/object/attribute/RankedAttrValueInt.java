/**
 * 
 */
package app.document.object.attribute;


/**
 * 属性ツリービュー属性値INT型
 */
public class RankedAttrValueInt extends RankedAttrValue {
	
	/** 属性値 */
	private int value;
	
	/**
	 * コンストラクタ
	 * @param value 登録する属性値
	 * @param objectId 属性値を持つオブジェクトのID
	 */
	public RankedAttrValueInt(int value, int objectId)
	{
		this.setValue(value);
		this.addObjectId(objectId);
	}

	/**
	 * @see app.document.object.attribute.RankedAttrValue#getHashKey()
	 */
	@Override
	public long getHashKey() {
		return this.getValue();
	}

	public int getValue() {
		return value;
	}

	public void setValue(int value) {
		this.value = value;
	}
	
}
