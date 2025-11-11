package app.document.object.attribute;

import java.util.Date;

public class RankedAttrValueDate extends RankedAttrValue {
	
	/** 属性値 */
	private Date value;
	
	/**
	 * コンストラクタ
	 * @param value 属性値
	 * @param objectId 属性値を保持するオブジェクトのID
	 */
	public RankedAttrValueDate(Date value, int objectId) {
		this.addObjectId(objectId);
		this.setValue(value);
	}
	

	public Date getValue() {
		return value;
	}


	/**
	 * 属性値をセット
	 * @param value Date型の属性値
	 */
	public void setValue(Date value) {
		this.value = value;
	}


	/**
	 * @see app.document.object.attribute.RankedAttrValue#getHashKey()
	 */
	@Override
	public long getHashKey() {
		return this.getValue().getTime();
	}

}
