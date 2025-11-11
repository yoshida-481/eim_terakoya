package eim.command.common;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


/**
 * 検索条件格納クラス
 *
 *
 */
public class EIMCommandSearchTarget {


	/**
	 * オブジェクトタイプID
	 */
	private List<Long> objTypeIdList = new ArrayList<Long>();

	/**
	 * オブジェクト名称
	 */
	private List<String> objNameList = new ArrayList<String>();

	/**
	 * パス
	 */
	private List<String> pathList = new ArrayList<String>();

	/**
	 * 属性マップ(キー：属性EIMAttributeType、バリュー：属性値リスト)
	 */
	private Map<Long, Object> attrMap  = new HashMap<Long, Object>();





	/**
	 * @return attrList
	 */
	public Map<Long, Object> getAttrMap() {
		return attrMap;
	}

	/**
	 * @param attrList 設定する attrList
	 */
	public void setAttrMap(Map<Long, Object> attrMap) {
		this.attrMap = attrMap;
	}

	public void setAttrMapKeyAndValue(Long attrTypeId, Object attrValueList) {
		this.attrMap.put(attrTypeId, attrValueList);
	}

	/**
	 * add attrList
	 * @param key
	 * @param value
	 */
	public void addAttrList(Long key, Object value) {
		this.attrMap.put(key, value);
	}

	/**
	 * @return objNameList
	 */
	public List<String> getObjNameList() {
		return objNameList;
	}

	/**
	 * @param objNameList 設定する objNameList
	 */
	public void setObjNameList(List<String> objNameList) {
		this.objNameList = objNameList;
	}

	/**
	 * add objNameList
	 * @param objName
	 */
	public void addObjNameList(String objName) {
		this.objNameList.add(objName);
	}

	/**
	 * @return objTypeIdList
	 */
	public List<Long> getObjTypeIdList() {
		return objTypeIdList;
	}

	/**
	 * @param objTypeIdList 設定する objTypeIdList
	 */
	public void setObjTypeIdList(List<Long> objTypeIdList) {
		this.objTypeIdList = objTypeIdList;
	}

	/**
	 * add objTypeIdLists
	 * @param objTypeId
	 */
	public void addObjTypeIdList(Long objTypeId) {
		this.objTypeIdList.add(objTypeId);
	}

	/**
	 * add all objTypeIdList
	 * @param objTypeList
	 */
	public void addAllObjTypeIdList(List<Long> objTypeList) {
		this.objTypeIdList.addAll(objTypeList);
	}

	/**
	 * @return pathList
	 */
	public List<String> getPathList() {
		return pathList;
	}

	/**
	 * @param pathList 設定する pathList
	 */
	public void setPathList(List<String> pathList) {
		this.pathList = pathList;
	}

	/**
	 * add pathList
	 * @param path
	 */
	public void addPathList(String path) {
		this.pathList.add(path);
	}


}
