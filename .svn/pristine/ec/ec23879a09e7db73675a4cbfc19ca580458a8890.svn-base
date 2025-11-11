package common.bo;

import java.util.List;

import common.util.AppConstant;

/**
 * 
 * 属性ツリーのオブジェクトクラス
 *
 * @author Jinya.Noguchi
 * @version 1.0
 */
public class AttributeTree {

	/**
	 * 属性ツリーのオブジェクトIDを格納するフィールド
	 */
	private long _id = 0;

	/**
	 * 属性ツリーのログイン言語での定義名を格納するフィールド
	 */
	private String _name = null;

	/**
	 * 属性ツリー定義名のデフォルト名を格納するフィールド
	 */	
	private String _defName = null;

	/**
	 * 分類対象(0:フォルダ /1:ドキュメント)を格納するフィールド
	 */	
	private long _classifyTarget;

	/**
	 * 属性ツリーに所属する属性アイテム(AttributeTreeItem)の
	 * 一覧を格納するフィールド
	 */	
	private List _treeItemList = null;

	/**
	 * 
	 * コンストラクタ
	 * @param id 属性ツリーのオブジェクトID
	 * @param name 属性ツリーのログイン言語での定義名
	 * @param defName 属性ツリー定義名のデフォルト名
	 * @param classifyTarget 分類対象(0:フォルダ/1:ドキュメント)
	 * @param treeItem 属性アイテム一覧 
	 */	
	public AttributeTree(long id,
						  String name,
						  String defName,
						  long classifyTarget,
						  List treeItem)
	{
		this._id = id;
		this._name = name;
		
		if (defName == null) {
			this._defName = name;
		} else {
			this._defName = defName;
		}
		
		this._classifyTarget = classifyTarget;
		this._treeItemList = treeItem;
	}
	
	/**
	* 属性ツリーのオブジェクトID取得
	* @return オブジェクトID
	*/	
	public long getId()
	{
		return this._id;
	}
	
	/**
	* 属性ツリーのログイン言語での定義名取得
	* @return 定義名
	*/	
	public String getName()
	{
		return this._name;
	}
	
	/**
	*　属性ツリー定義名のデフォルト名取得
	* @return デフォルト名
	*/	
	public String getDefName()
	{
		return this._defName;
	}
	
	/**
	*　分類対象取得
	* @return 分類対象　
	*/	
	public long getClassifyTarget()
	{
		return this._classifyTarget;
	}
	
	/**
	* 分類対象がフォルダかどうか判定
	* @return true：フォルダの場合/false：それ以外
	*/	
	public boolean isClassifyTargetFolder()
	{
		if (this._classifyTarget == AppConstant.CLASSIFY_TARGET_FOLDER) {
			return true;
		} else {
			return false;
		}
	}
	
	/**
	* 分類対象がドキュメントかどうか判定
	* @return true：ドキュメントの場合/false：それ以外
	*/	
	public boolean isClassifyTargetDocument()
	{
		if (this._classifyTarget == AppConstant.CLASSIFY_TARGET_DOCUMENT) {
			return true;
		} else {
			return false;
		}
	}
	
	/**
	* 属性ツリーに所属する属性アイテム一覧取得
	* @return 属性アイテム一覧
	*/	
	public List getTreeItemList()
	{
		return this._treeItemList;
	}

	/**
	* 属性ツリーに所属する属性アイテム一覧を設定
	* @param treeItem 属性アイテム一覧
	*/	
	public void setTreeItemList(List treeItem)
	{
		this._treeItemList = treeItem;
	}
	
}