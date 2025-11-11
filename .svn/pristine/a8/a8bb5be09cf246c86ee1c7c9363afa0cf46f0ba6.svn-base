package common.bo;

import java.util.ArrayList;
import java.util.List;

/**
 * 
 * 属性更新情報を管理するクラス
 * 
 */
public class AttributeUpdater {

	/**
	 * 対象EIMObjectのオブジェクトIDです。
	 */
	private long _objId = Integer.MIN_VALUE;

	/**
	 * 対象EIMObjectのオブジェクト名です。
	 */
	private String _objName = null;

	/**
	 * AttributeUpdaterItemのリストです。
	 */
	private List _attributeUpdaterItemList = null;

	/**
	 * コンストラクタ
	 *
	 */
	public AttributeUpdater(){}
	
	/**
	 * コンストラクタ
	 * 
	 * @param objId オブジェクトID
	 * @param objName オブジェクト名
	 * @param attributeUpdaterItemList AttributeUpdaterItemのリスト
	 */
	public AttributeUpdater(long objId, String objName, List attributeUpdaterItemList) {
		
		_objId = objId;
		_objName = objName;
		_attributeUpdaterItemList = attributeUpdaterItemList;
	}
	
	/**
	 * 対象EIMObjectのオブジェクトIDを返します。
	 * 
	 * @return 対象EIMObjectのオブジェクトID
	 */
	public long getObjId() {
		return _objId;
	}

	/**
	 * 対象EIMObjectのオブジェクトIDを設定します。
	 * 
	 * @param objId 対象EIMObjectのオブジェクトID
	 */
	public void setObjId(int objId) {
		this._objId = objId;
	}

	/**
	 * 対象EIMObjectのオブジェクト名を返します。
	 * 
	 * @return 対象EIMObjectのオブジェクト名
	 */
	public String getObjName() {
		return _objName;
	}

	/**
	 * 対象EIMObjectのオブジェクト名を設定します。
	 * 
	 * @param objName 対象EIMObjectのオブジェクト名
	 */
	public void setObjName(String objName) {
		this._objName = objName;
	}

	/**
	 * AttributeUpdaterItemのリストを返します。
	 * 
	 * @return AttributeUpdaterItemのリスト
	 */
	public List getAttributeUpdaterItemList() {
		return _attributeUpdaterItemList;
	}

	/**
	 * AttributeUpdaterItemのリストを設定します。
	 * 
	 * @param attributeUpdaterItemList AttributeUpdaterItemのリスト
	 */
	public void setAttributeUpdaterItemList(List attributeUpdaterItemList) {
		this._attributeUpdaterItemList = attributeUpdaterItemList;
	}
	
	/**
	 * AttributeUpdaterItemのリストに引数AttributeUpdaterItemを追加します。
	 * 
	 * <li>初めて追加する場合はメソッド内でnew ArrayList()を実施してから追加します。
	 * 
	 * @param attributeUpdaterItem 属性更新情報のアイテムを管理するクラス
	 */
	public void addAttributeUpdaterItemList(AttributeUpdaterItem attributeUpdaterItem) {
		
		if (this._attributeUpdaterItemList == null) {
			this._attributeUpdaterItemList = new ArrayList();
		}
		this._attributeUpdaterItemList.add(attributeUpdaterItem);
	}
}