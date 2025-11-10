package common.util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import eim.bo.EIMException;
import eim.net.EIMSession;
import jp.co.ctc_g.eim.app.document.presentation.dto.BoxDocumentCreateDTO;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain;

public class BoxDocumentUploadManager
{
	/** セッション **/
	private EIMSession _sess;
	/** オブジェクト作成ヘルパークラス **/
	private CreateBoxDocumentObjectHelper _createHelper;
	/** パスとオブジェクトのマップ **/
	private HashMap pathMap = new HashMap();
	/** 作成したオブジェクトのリスト **/
	private List<BoxDocumentCreateDTO> createObjectList = new ArrayList<BoxDocumentCreateDTO>();

	private static final String DOCUMENT = "document";
	private static final String FOLDER = "folder";

	public BoxDocumentUploadManager(
			String _dTyp,String _oId,String _uId,String _bPath)
	{

		//オブジェクトドメイン作成
		ObjectTypeDomain objType = new ObjectTypeDomain();
		objType.setId(Long.parseLong(_dTyp));

		//ユーザードメイン（作成者）作成
		UserDomain createUser = new UserDomain();
		createUser.setId(Long.parseLong(_uId));

		_createHelper = new CreateBoxDocumentObjectHelper(objType,_oId, createUser,_bPath);

	}

	/**
	 * オブジェクト（ドキュメントまたはフォルダ）を登録する
	 * @param nameArray オブジェクト名の配列
	 * @param pathArray パスの配列
	 * @param typeArray "document"か"folder"の配列
	 * @return 登録したオブジェクトのリスト
	 */
	public List<BoxDocumentCreateDTO> createObject(String[] nameArray, String[] pathArray, String[] typeArray,Object[] attributeList) throws EIMException, Exception
	{
        //セッションの再取得
		_sess = EIMThreadContext.getEIMSession();
		//HashMap<フルパス, オブジェクト>を作成する
		createPathMap(nameArray, pathArray, typeArray);

		for(int ii = 0; ii < nameArray.length; ii++) {
			//ドキュメントの登録
			if(typeArray[ii].equals(DOCUMENT)) {
				BoxDocumentCreateDTO createObject = _createHelper.createDocument(pathMap, nameArray[ii], pathArray[ii],attributeList[ii]);
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

}
