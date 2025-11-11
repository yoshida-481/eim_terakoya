package common.util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import eim.bo.EIMException;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.net.EIMSession;

public class ConfirmLumpUploadManager
{
	/** セッション **/
	private final EIMSession _sess;
	/** オブジェクト作成ヘルパークラス **/
	private final CreateLumpObjectHelper _createHelper;
	/** パスとオブジェクトのマップ **/
	private HashMap pathMap = new HashMap();
	/** 作成したオブジェクトのリスト **/
	private List<EIMObject> createObjectList = new ArrayList<EIMObject>();

	private static final String DOCUMENT = "document";
	private static final String FOLDER = "folder";
	
	public ConfirmLumpUploadManager(
			EIMSession _s, EIMObjectType _dType, String _oId,
			String _uId, String _p, String _exp)
	{
		_sess = _s;
		_createHelper = new CreateLumpObjectHelper(_sess, _dType, _oId, _uId, _p, _exp);
	}
	
	/**
	 * オブジェクト（ドキュメントまたはフォルダ）を登録する
	 * @param nameArray オブジェクト名の配列
	 * @param pathArray パスの配列
	 * @param typeArray "document"か"folder"の配列
	 * @return 登録したオブジェクトのリスト
	 */
	public List<EIMObject> createObject(String[] nameArray, String[] pathArray, String[] typeArray) throws EIMException, Exception
	{
		//HashMap<フルパス, オブジェクト>を作成する
		createPathMap(nameArray, pathArray, typeArray);

		//フォルダの登録は、HashMapからオブジェクトがnullのものを選択して作成する
		createFolder();
		
		for(int ii = 0; ii < nameArray.length; ii++) {
			//ドキュメントの登録
			if(typeArray[ii].equals(DOCUMENT)) {
				EIMObject createObject = _createHelper.createDocument(pathMap, nameArray[ii], pathArray[ii]);
				if (createObject != null) {
					createObjectList.add(createObject);
				}
			}
		}
		return createObjectList;
	}
	
	/**
	 * フルパスとオブジェクトを紐付けたマップを作成する
	 * @param nameArray オブジェクト名の配列
	 * @param pathArray パスの配列
	 * @param typeArray "document"か"folder"の配列
	 */
	private void createPathMap(String[] nameArray, String[] pathArray, String[] typeArray) throws Exception
	{
		for(int ii = 0; ii < pathArray.length; ii++) {
			setPathMap(getPath(nameArray[ii], pathArray[ii], typeArray[ii]));
		}
		
		ArrayList pathList = new ArrayList(pathMap.keySet());
		HashMap objMap = AppObjectUtil.getFolderObjMapByFullPath(_sess, (String[])pathList.toArray(new String[0]));
		pathMap.putAll(objMap);
	}
	
	/**
	 * パスを取得する（フォルダの場合はフォルダ名も含めたもの、ドキュメントの場合は親フォルダまで）
	 * @param name オブジェクト名
	 * @param path パス
	 * @param type "document"か"folder"
	 */
	private String getPath(String name, String path, String type)
	{
		String ret = new String(path);
		if(type.equals(FOLDER)) {
			ret += name;
		}
		return ret;
	}
	
	/**
	 * 最上位からのパスのリストを取得する
	 * @param path パス
	 */
	private void setPathMap(String path)
	{
		String[] pathArray = path.split("/");
		String tmpPath = "";
		for(int ii = 1; ii < pathArray.length; ii++) {
			tmpPath += "/" + pathArray[ii];
			pathMap.put(tmpPath, null);
		}
	}
	
	/**
	 * フォルダを登録する
	 */
	private void createFolder() throws EIMException, Exception
	{
		List list = new ArrayList(pathMap.keySet());
		list = AppObjectUtil.getStrSortedList(list, "toString", true);
		
		for(int ii = 0; ii < list.size(); ii++) {
			String path = list.get(ii).toString();
			if(pathMap.get(path) == null) {
				//フォルダ作成
				EIMObject o = _createHelper.createFolder(pathMap, path);
				pathMap.put(path, o);

				createObjectList.add(o);

				// SearchFramework 検索FW更新通知 対象：登録フォルダ
				AppUpdateNoticeUtils.updateNoticeInsert(o.getId(), "SEARCHFW_LUMP_UPLOAD_CREATE_FOLDER");
				
			}
		}
	}
}
