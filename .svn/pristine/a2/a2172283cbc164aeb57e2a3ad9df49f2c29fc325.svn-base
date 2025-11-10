package app.document.object.attribute;

import java.util.HashMap;
import java.util.Map;




/**
 * ランク付けされた属性ツリーリスト値
 */
public abstract class RankedAttrValue {

	
	/** 属性を保持するオブジェクトのID */
	private Map<Integer, Integer> objectMap = new HashMap<Integer, Integer>();
	
	/** 属性のカウント数 */
	private int count = 0;
	
	/**
	 * ハッシューキーの取得
	 * @return 属性値を元にハッシュキーを作成する
	 */
	abstract public long getHashKey();
	
	/**
	 * 属性のカウントを追加
	 * @param objId 該当する属性を保持するオブジェクトID
	 */
	public void addObjectId(int objId) {
		this.getObjectMap().put(objId, objId);
		this.setCount(this.getCount() + 1);
	}
	

	public Map<Integer, Integer> getObjectMap() {
		return objectMap;
	}

	public void setObjectMap(Map<Integer, Integer> objectMap) {
		this.objectMap = objectMap;
	}

	public int getCount() {
		return count;
	}

	public void setCount(int count) {
		this.count = count;
	}
	

}
