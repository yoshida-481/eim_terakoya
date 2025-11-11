package batch.maintenance.domain;

public class FolderDomain {

	/*
	 * DOMAINの定義
	 * <FolderDomain>
	 * フォルダのパス＋フォルダのID＋フォルダのセキュリティを蓄積
	 * */

	/*
	 * 定義する
	 * */

	private long ObjID;
	private String Path;
	private String Name;
	private String Security;



	/**
	 * パス属性
	 * @return Path
	 */
	public String getPath() {
	    return Path;
	}
	/**
	 * オブジェクト名
	 * @return name
	 */
	public String getObjName() {
	    return Name;
	}
	/**
	 * オブジェクトID
	 * @return ObjID
	 */
	public long getObjID() {
	    return ObjID;
	}
	/**
	 * セキュリティ
	 * @return Security
	 */
	public String getSecurity() {
	    return Security;
	}

	/**
	 * パス属性
	 * @param Path Path
	 */
	public void setPath(String Path) {
	    this.Path = Path;
	}

	/**
	 * オブジェクト名
	 * @param ObjID ObjID
	 */
	public void setObjID(long ObjID) {
	    this.ObjID = ObjID;
	}

	/**
	 * オブジェクトID
	 * @param ObjID ObjID
	 */
	public void setObjName(String Name) {
	    this.Name = Name;
	}
	/**
	 * セキュリティ
	 * @return Security
	 */
	public void setSecurity(String Security) {
		this.Security = Security;
	}
}
