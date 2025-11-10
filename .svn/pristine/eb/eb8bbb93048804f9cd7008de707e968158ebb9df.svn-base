package app.document.object;

import java.util.ArrayList;
import java.util.List;

import eim.bo.EIMObjectType;


/**
 * 定型ドキュメントのデータクラス
 *
 */
public class FixedForm {
	
	/** ルートオブジェクトタイプ */
	private FixedForm rootType;
	
	/** 親のオブジェクトタイプ */
	private FixedForm parent;
	
	/** サブクラスの定型ドキュメント */
	private List<FixedForm> childType;
	
	
	/** 自身が保持するオブジェクトタイプ */
	private EIMObjectType formType;
	
	/** 表示するか否かのフラグ */
	private boolean dspFlag;
	
	/** 自身が選択されているタイプか否かのフラグ */
	private boolean selectedFlag;
	
	/** ワークスペースの使用可能タイプに設定されていたが、削除され取得できなくなってしまった定型ドキュメントの数*/
	private int deletedTypeCount = -1;
	
	
	/**
	 * 定型ドキュメントのオブジェクトタイプを子含め返却
	 * @return オブジェクトタイプのリスト、順不同
	 */
	public List<EIMObjectType> getObjTypeList()
	{
		List<EIMObjectType> typeList = new ArrayList<EIMObjectType>();
		
		// 自身のオブジェクトタイプをリストに追加
		typeList.add(this.getFormType());
		
		for (FixedForm child : this.getChildType()) {
			typeList.addAll(child.getObjTypeList());
		}
		
		return typeList;
	}
	
	

	/**
	 * @return the rootType
	 */
	public FixedForm getRootType() {
		return rootType;
	}

	/**
	 * @param rootType the rootType to set
	 */
	public void setRootType(FixedForm rootType) {
		this.rootType = rootType;
	}

	
	
	/**
	 * @return the parent
	 */
	public FixedForm getParent() {
		return parent;
	}

	/**
	 * @param parent the parent to set
	 */
	public void setParent(FixedForm parent) {
		this.parent = parent;
	}

	/**
	 * @return the childType
	 */
	public List<FixedForm> getChildType() {
		return childType;
	}

	/**
	 * @param childType the childType to set
	 */
	public void setChildType(List<FixedForm> childType) {
		this.childType = childType;
	}

	/**
	 * @return the formType
	 */
	public EIMObjectType getFormType() {
		return formType;
	}

	/**
	 * @param formType the formType to set
	 */
	public void setFormType(EIMObjectType formType) {
		this.formType = formType;
	}

	/**
	 * @return the dspFlag
	 */
	public boolean getDspFlag() {
		return dspFlag;
	}
	
	/**
	 * @param dspFlag the dspFlag to set
	 */
	public void setDspFlag(boolean dspFlag) {
		this.dspFlag = dspFlag;
	}

	/**
	 * @return the selectedFlag
	 */
	public boolean getSelectedFlag() {
	    return selectedFlag;
	}

	/**
	 * @param selectedFlag the selectedFlag to set
	 */
	public void setSelectedFlag(boolean selectedFlag) {
	    this.selectedFlag = selectedFlag;
	}

	/**
	 * @return deletedTypeCount
	 */
	public int getDeletedTypeCount() {
	    return deletedTypeCount;
	}

	/**
	 * @param deletedTypeCount the deletedTypeCount to set
	 */
	public void setDeletedTypeCount(int deletedTypeCount) {
	    this.deletedTypeCount = deletedTypeCount;
	}
	

}
