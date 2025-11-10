package app.document.object.attribute;

public class RankedAttrValueDouble extends RankedAttrValue {
	
	/** 属性値 */
	private Double value;
	
	/**
	 * コンストラクタ
	 * @param value 属性値
	 * @param objectId オブジェクトID
	 */
	public RankedAttrValueDouble(double value, int objectId) {
		this.addObjectId(objectId);
		this.setValue(value);
	}
	
	

	public Double getValue() {
		return value;
	}

	public void setValue(double value) {
		this.value = value;
	}

	@Override
	public long getHashKey() {
		return this.getValue().longValue();
	}

}
