package eim.command.common.util;

import eim.bo.EIMException;
import eim.bo.EIMResource;

/**
 * 
 * フォルダ作成(内部)IF用エラーメッセージキー定義
 *
 */
public class FolderServiceErrorConstants {

	public static final FolderServiceErrorConstants PARENTOBJ_NULL = new FolderServiceErrorConstants("EIM.ERROR.LOGIC.PARENTOBJ.NULL");
	public static final FolderServiceErrorConstants OBJTYPE_NULL = new FolderServiceErrorConstants("EIM.ERROR.LOGIC.OBJTYPE.NULL");
	public static final FolderServiceErrorConstants FOLDERNAME_NULL = new FolderServiceErrorConstants("EIM.ERROR.LOGIC.FOLDERNAME.NULL");
	public static final FolderServiceErrorConstants SESSION_NULL = new FolderServiceErrorConstants("EIM.ERROR.LOGIC.SESSION.NULL");
	public static final FolderServiceErrorConstants PARENTOBJ_NOTFOUND = new FolderServiceErrorConstants("EIM.ERROR.LOGIC.PARENTOBJ.NOTFOUND");
	public static final FolderServiceErrorConstants NO_CREATEROLE = new FolderServiceErrorConstants("EIM.ERROR.LOGIC.NOCREATEROLE");
	public static final FolderServiceErrorConstants STATUS_NOTEDITING = new FolderServiceErrorConstants("EIM.ERROR.LOGIC.STATUS.NOTEDITING");
	public static final FolderServiceErrorConstants NO_FOLDERMANAGEMENTSEC = new FolderServiceErrorConstants("EIM.ERROR.LOGIC.NOFOLDERMNGSEC");
	public static final FolderServiceErrorConstants PARENTOBJ_NOTFOLDER = new FolderServiceErrorConstants("EIM.ERROR.LOGIC.PARENTOBJ.NOTFOLDER");
	public static final FolderServiceErrorConstants OBJTYPE_NOTFOUND = new FolderServiceErrorConstants("EIM.ERROR.LOGIC.OBJTYPE.NOTFOUND");
	public static final FolderServiceErrorConstants OBJTYPE_NOTFOLDER = new FolderServiceErrorConstants("EIM.ERROR.LOGIC.OBJTYPE.NOTFOLDER");
	public static final FolderServiceErrorConstants INVALID_NAME = new FolderServiceErrorConstants("EIM.WARN.LOGIC.INVALIDNAME");
	public static final FolderServiceErrorConstants OVERFLOW_FOLDERNAME = new FolderServiceErrorConstants("EIM.ERROR.LOGIC.DIR.NAME.OVER");
	public static final FolderServiceErrorConstants DUPLICATE_FOLDERNAME = new FolderServiceErrorConstants("EIM.ERROR.LOGIC.OBJTYPE.NAME.DUPLICATE");
	public static final FolderServiceErrorConstants UNDER_WFFOLDER = new FolderServiceErrorConstants("EIM.ERROR.LOGIC.CANTSET.UNDERWF.WFFOLDER");
	public static final FolderServiceErrorConstants UNDER_RECYCLEBIN = new FolderServiceErrorConstants("EIM.ERROR.INPUT.CREATEFOLDERINRECYCLE");
	public static final FolderServiceErrorConstants SYSTEMERROR = new FolderServiceErrorConstants("EIM.ERROR.SYSTEMERROR");
	
	private String key;
	private String value;
	
	private FolderServiceErrorConstants(final String messageKey) {
		this.key = messageKey;
	}
	
	/**
	 * メッセージ内容を取得
	 * @return
	 * @throws EIMException 指定したキーに該当する定義が存在しない場合
	 */
	public String getValue() throws EIMException {
		
		if (this.value == null) {
			this.value = EIMResource.getMessageValue(this.key);
		}
		
		return this.value;
	}
	/**
	 * メッセージキーを取得
	 * @return
	 */
	public String getKey() {
		return this.key;
	}
}
