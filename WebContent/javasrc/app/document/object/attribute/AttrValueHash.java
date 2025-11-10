/**
 * 
 */
package app.document.object.attribute;

import java.util.Hashtable;

/**
 * 属性ツリーにて表示される属性値のリストを管理するハッシュテーブル
 */
public class AttrValueHash extends Hashtable<Long, RankedAttrValue> {
	
	static final long serialVersionUID = 2L;
	
	/**
	 * コンストラクタ
	 */
	public AttrValueHash() {
		super();
	}
	
	/**
	 * ランク付けされた属性値を取得
	 * @param type 属性値種別(RankedAttrValueのクラス変数にて定義)
	 * @param value 属性値情報
	 */
	public void putValue(RankedAttrValue value) {
		
		RankedAttrValue exist = this.getValue(value.getHashKey());
		if (exist == null) {
			this.put(value.getHashKey(), value);
		} else {
			for (Integer objId : value.getObjectMap().values()) {
				exist.addObjectId(objId);
			}
		}
	}
	
	
	/**
	 * ランク付けされた属性値を取得
	 * @param key
	 * @return
	 */
	private RankedAttrValue getValue(long key) {
		return this.get(key);
	}
	

}
