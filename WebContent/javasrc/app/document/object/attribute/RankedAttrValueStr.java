package app.document.object.attribute;

public class RankedAttrValueStr extends RankedAttrValue {
	
	/** 属性値 */
	private String value;
	
	/**
	 * コンストラクタ
	 * @param value 属性値
	 * @param objectId オブジェクトID
	 */
	public RankedAttrValueStr(String value, int objectId) {
		this.setValue(value);
		this.addObjectId(objectId);
	}
	

	@Override
	public long getHashKey() {
		return this.getValue().hashCode();
	}


	public String getValue() {
		return value;
	}


	public void setValue(String value) {
		this.value = value;
	}
	
}
