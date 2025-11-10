package eim.command.common;

import java.util.HashMap;
import java.util.Map;

import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.ObjectUtils;

/**
 * オブジェクトタイプのルートオブジェクトタイプ
 * に関するデータを格納するクラス
 */
public class RootObjTypeData {
	
	// ドキュメントタイプID
	private long docTypeId;
	
	// ワークスペースタイプID
	private long workSpaceTypeId;
	
	// タグタイプID
	private long tagTypeId;
	
	// キー：オブジェクトのタイプID
	// 値  ：キーに設定したオブジェクトタイプのルートタイプID
	private Map<Long, Long> childTypeId_rootType_map = null;
	
	//コンストラクタ
	public RootObjTypeData()
	{
		
	}
	public RootObjTypeData(EIMSession sess) throws Exception
	{
		this.docTypeId = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT")).getId();
		this.workSpaceTypeId =ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_WORKSPACE")).getId();
		this.tagTypeId = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_TAG")).getId();
	}
	
	/**
	 * childTypeId_rootType_mapにキーと値のペア
	 * をセットします
	 */
	public void setKeyValue2Map(long key, long value)
	{
		if(childTypeId_rootType_map == null)
		{
			childTypeId_rootType_map = new HashMap<Long,Long>();
		}
		childTypeId_rootType_map.put(key, value);
		
	}
	
	/**
	 * @return the docTypeId
	 */
	public long getDocTypeId() {
		return docTypeId;
	}

	/**
	 * @param docTypeId the docTypeId to set
	 */
	public void setDocTypeId(long docTypeId) {
		this.docTypeId = docTypeId;
	}

	/**
	 * @return the workSpaceTypeId
	 */
	public long getWorkSpaceTypeId() {
		return workSpaceTypeId;
	}

	/**
	 * @param workSpaceTypeId the workSpaceTypeId to set
	 */
	public void setWorkSpaceTypeId(long workSpaceTypeId) {
		this.workSpaceTypeId = workSpaceTypeId;
	}

	/**
	 * @return the tagTypeId
	 */
	public long getTagTypeId() {
		return tagTypeId;
	}

	/**
	 * @param tagTypeId the tagTypeId to set
	 */
	public void setTagTypeId(long tagTypeId) {
		this.tagTypeId = tagTypeId;
	}

	/**
	 * @return the childTypeId_rootType_map
	 */
	public Map<Long, Long> getChildTypeId_rootType_map() {
		return childTypeId_rootType_map;
	}

	/**
	 * @param childTypeId_rootType_map the childTypeId_rootType_map to set
	 */
	public void setChildTypeId_rootType_map(Map<Long, Long> childTypeId_rootType_map) {
		this.childTypeId_rootType_map = childTypeId_rootType_map;
	}
	

}
