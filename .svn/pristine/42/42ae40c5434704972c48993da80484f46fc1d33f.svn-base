package common.bo;

import java.util.ArrayList;
import java.util.List;

import eim.bo.EIMObject;

/**
 * 
 * タグ付与対象一覧を構成するツリー要素クラスです。
 * 
 */
public class TagTreeItem {
	
	/**
	 * タグ付与対象一覧のEIMObjectを格納するフィールドです。
	 */
	private EIMObject _eimObject = null;
	
	/**
	 * タグ付与対象一覧において_eimObjectの直下に存在するEIMObjectを
	 * 格納したTagTreeItemのリストを格納するフィールドです。
	 */
	private List _treeItemList = null;
	
	/**
	 * コンストラクタ
	 * 
	 * @param eimObject タグ付与対象一覧のEIMObject
	 * @param treeItemList TagTreeItemのリスト
	 */
	public TagTreeItem(	EIMObject eimObject, List treeItemList)
	{
		this._eimObject = eimObject;
		this._treeItemList = treeItemList;
	}
	
	/**
	 * タグ付与対象一覧のEIMObjectを取得します。
	 * @return タグ付与対象一覧のEIMObject
	 */
	public EIMObject getEimObject() {
		return this._eimObject;
	}

	/**
	 * TagTreeItemのリストを取得します。
	 * @return TagTreeItemのリスト
	 */
	public List getTreeItemList() {
		if (this._treeItemList == null) {
			this._treeItemList = new ArrayList();
		}
		return this._treeItemList;
	}

	/**
	 * TagTreeItemのリストの設定します。
	 * @param itemList TagTreeItemのリスト
	 */
	public void setTreeItemList(List itemList) {
		this._treeItemList = itemList;
	}
	
	/**
	 * TagTreeItemのリストにTagTreeItemを追加します。
	 * @param treeItem タグ付与対象一覧を構成するツリー要素クラス
	 */
	public void addTreeItem(TagTreeItem treeItem) {
		if (this._treeItemList == null) {
			this._treeItemList = new ArrayList();
		}
		if (treeItem != null) {
			this._treeItemList.add(treeItem);
		}
	}
	
	/**
	 * 本TagTreeItemが保持するEIMObjectのオブジェクト名を取得します。
	 * <li>ソートメソッド(AppObjectUtil#getStrSortedList)で必要なため
	 * @return EIMObjectのオブジェクト名
	 */
	public String getName() {
		return this._eimObject.getName();
	}
}