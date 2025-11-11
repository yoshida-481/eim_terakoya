package common.util;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import eim.bo.EIMAccessRole;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMResource;
import eim.bo.EIMStatusType;
import eim.bo.EIMWorkFlow;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.ObjectUtils;
import eim.util.SecurityUtils;
import eim.util.WorkFlowUtils;
import jp.co.ctc_g.eim.framework2.common.exception.EIMException;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;


public class DocumentFromBoxUploadManager
{
	/** セッション **/
	private final EIMSession _sess;
	/** ドキュメントオブジェクトタイプ **/
	private final EIMObjectType _docObjType;
	/** フォルダオブジェクトタイプ **/
	private final EIMObjectType _folderObjType;
	/** 一時格納フォルダのパス **/
	private final String _tmpFolderPath;
	/** 実際に登録するフォルダのパス **/
	private final String _realFolderPath;
	/** セッションヘルパー */

	/** ヘルパー **/
	private final AppObjectConditionHelper _helper;
	/** 親オブジェクトのハッシュマップ<パス, フォルダオブジェクト> **/
	private HashMap parentObjMap = null;

	/** ドキュメントのワークフロー **/
	private EIMWorkFlow documentWF = null;
	/** フォルダのワークフロー **/
	private EIMWorkFlow folderWF = null;

	/** ステータス **/
	public static final String CAN_UPLOAD = "0";
	public static final String CANNOT_UPLOAD = "1";
	public static final String CONFIRM_UPLOAD = "2";
	public static final String CONFIRM_AND_CHECK_UPLOAD = "3";

	public static final String STATUS = "status";
	public static final String REASON = "reason";
	public static final String SAME_NAME_OBJ = "sameNameObj";

	/** オブジェクトタイプ **/
	private static final String DOCUMENT_OBJ_TYPE = EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT");
	private static final String FOLDER_OBJ_TYPE = EIMConfig.get("OBJECT_TYPE_NAME_FOLDER");
	private static final String TAG_OBJ_TYPE = EIMConfig.get("OBJECT_TYPE_NAME_TAG");
	private static final String[] TYPE_ARRAY = {
		DOCUMENT_OBJ_TYPE, FOLDER_OBJ_TYPE, TAG_OBJ_TYPE
	};

	public DocumentFromBoxUploadManager(EIMSession _s, EIMObjectType _type, String _tmp, String _real) throws Exception
	{
		_sess = _s;
		_docObjType = _type;
		_tmpFolderPath = _tmp;
		_realFolderPath = _real;

		_helper = new AppObjectConditionHelper(_sess);

		documentWF = WorkFlowUtils.getWorkFlowByType(_sess, _docObjType);
		_folderObjType = ObjectUtils.getObjectTypeByName(_sess, FOLDER_OBJ_TYPE);
		folderWF = WorkFlowUtils.getWorkFlowByType(_sess, _folderObjType);
	}

	/**
	 * 指定フォルダをウォーキングして親フォルダオブジェクトのマップを作成する
	 * @param root
	 * @param skipZipName
	 */
	public void createParentObjectMap(File root, String skipZipName) throws Exception
	{
		ArrayList pathList = new ArrayList();
		recursiveCreateParentPathList(root, pathList, true);
		parentObjMap = AppObjectUtil.getFolderObjMapByFullPath(_sess, (String[])pathList.toArray(new String[0]));
	}

	/**
	 * 親オブジェクトのフォルダパスのリストを作成する
	 * @param root 一時格納フォルダのルート
	 * @param pathList パスを格納するリスト
	 * @param top ルートかどうかのフラグ
	 */
	private void recursiveCreateParentPathList(File root, ArrayList pathList, boolean top)
	{
		//再帰的処理
		if(top == false) {
			addPath(pathList, getPath(root.getPath(), false));
		}
		File[] list = FileUtil.sortedListFiles(root);
		for(int ii = 0; ii < list.length; ii++) {
			addPath(pathList, getPath(list[ii].getPath(), false));
			if(list[ii].isDirectory()) {
				//フォルダの場合は子を再帰的にチェック
				File[] children = FileUtil.sortedListFiles(list[ii]);
				for(int jj = 0; jj < children.length; jj++) {
					recursiveCreateParentPathList(children[jj], pathList, false);
				}
			}
		}
	}

	/**
	 * 指定文字列が含まれていない場合は、その文字列をリストに追加する
	 * @param list リスト
	 * @param str 指定文字列
	 */
	private void addPath(ArrayList list, String str)
	{
		if(list.contains(str) == false) {
			list.add(str);
		}
	}

	/**
	 * 一次格納フォルダの中身が登録可能かどうかのチェックを行う。
	 * @param root 一時格納フォルダのルート
	 * @param top ルートかどうかのフラグ
	 */
	public ArrayList recursiveCheckAssign(File root, boolean top)
	{
		ArrayList retList = new ArrayList();
		//再帰的処理
		if(top == false) {
			retList.add(checkAndCreateData(root));
		}
		File[] list = FileUtil.sortedListFiles(root);
		for(int ii = 0; ii < list.length; ii++) {
			//自分がフォルダであろうがドキュメントであろうが先にチェックを行う
			retList.add(checkAndCreateData(list[ii]));
			if(list[ii].isDirectory()) {
				//フォルダの場合は子を再帰的にチェック
				File[] children = FileUtil.sortedListFiles(list[ii]);
				for(int jj = 0; jj < children.length; jj++) {
					retList.addAll(recursiveCheckAssign(children[jj], false));
				}
			}
		}

		return retList;
	}

	/**
	 * 登録可否チェックし、クライアント返却用のデータを作成する
	 * @param f チェック対象ファイル
	 */
	private DocumentFromBoxUploadData checkAndCreateData(File f)
	{
		//登録可否チェックを行った後、結果をデータクラスに格納する
		HashMap map = check(f);
		return new DocumentFromBoxUploadData(f, getPath(f.getPath(), false), map.get(STATUS).toString(), map.get(REASON).toString(),(Long) map.get(SAME_NAME_OBJ));
	}

	/**
	 * 登録可否のチェックを行う
	 * @param f チェック対象ファイル
	 */
	private HashMap check(File f)
	{
		HashMap ret = new HashMap();
		String fullPath = getPath(f.getPath(), true);
		String status = CANNOT_UPLOAD;

		try {
			/*
			 * 親オブジェクトのステータスチェック。以下の場合は登録不可。
			 *   1.登録ディレクトリ（上位ディレクトリを含む）がWFあり、かつステータスが「編集中」以外
			 *   2.登録ディレクトリ（上位ディレクトリを含む）がWFあり、かつステータスが「編集中」、かつ登録オブジェクトがWFあり
			 *     （登録先のトップ（_realFolderPath）がWF付かどうかもチェックする）
			 */
			String folderPath = getPath(f.getPath(), false);
			String lastFolderPath = folderPath.substring(0, folderPath.length() - 1);
			EIMObject parentObj = (EIMObject)parentObjMap.get(lastFolderPath);	//最後の"/"を削除
			if(parentObj == null)
			{
				/*
				 * 登録するオブジェクト名のチェック
				 */
				checkValidObjectName(f);

				/* 存在するフォルダが見つかるまで追う */
				EIMObject existParentObj = getExistParentObject(parentObjMap, lastFolderPath);
				if (existParentObj == null) {
					throw new Exception("EIM.ERROR.SYSTEMERROR");
				}

				/*
				 * フォルダ構成管理の有無をチェック
				 */
				if (f.isDirectory() && (this.haveFolderSecurity(existParentObj) == false)) {
					throw new Exception("EIM.ERROR.LOGIC.NOCREATEROLE");
				}

				/*
				 * セキュリティのチェック
				 */
				if (SecurityUtils.authorized(_sess, existParentObj, _sess.getUser(), EIMAccessRole.CREATE) == false) {
					throw new Exception("EIM.ERROR.LOGIC.NOCREATEROLE");
				}



				/*
				 * 登録先のトップフォルダがWF付かどうかチェックする
				 */
				if(isTypeOfFolderWithWF(existParentObj))
				{
					long statusKind = existParentObj.getStatus() != null ?
							existParentObj.getStatus().getType().getKind() : AppConstant.STATUS_TYPE_KIND_ID_NONE;
					if(statusKind != AppConstant.STATUS_TYPE_KIND_ID_EDITTING) {
						//上記1
						throw new Exception("EIM.ERROR.INPUT.WFFOLDEREXIST");
					}
					else if((f.isFile() && documentWF != null) || (f.isDirectory() && folderWF != null)) {
						//上記2
						throw new Exception("EIM.ERROR.INPUT.WFFOLDEREXIST");
					}
				}
				else if(f.isFile() && documentWF != null && folderWF != null) {
					//上記2(登録先フォルダがWF付きでなくても、一般フォルダがWF付きの場合)
					throw new Exception("EIM.ERROR.INPUT.WFFOLDEREXIST");
				}

				//親フォルダがない場合はフォルダを作成する
				status = CAN_UPLOAD;
				throw new Exception();
			}
			else
			{
				// 親オブジェクトのセキュリティチェック
				if (SecurityUtils.authorized(_sess, parentObj,_sess.getUser(), EIMAccessRole.CREATE) == false) {
					throw new Exception("EIM.ERROR.LOGIC.NOCREATEROLE");
				}
			}

			if(isTypeOfFolderWithWF(parentObj))
			{
				long statusKind = parentObj.getStatus() != null ?
						parentObj.getStatus().getType().getKind() : AppConstant.STATUS_TYPE_KIND_ID_NONE;
				if(statusKind != AppConstant.STATUS_TYPE_KIND_ID_EDITTING) {
					//上記1
					throw new Exception("EIM.ERROR.INPUT.WFFOLDEREXIST");
				}
				else if((f.isFile() && documentWF != null) || (f.isDirectory() && folderWF != null)) {
					//上記2
					throw new Exception("EIM.ERROR.INPUT.WFFOLDEREXIST");
				}
			}

			/*
			 * ファイルサイズの確認
			 */
			// ファイルサイズ判定(最小ファイルサイズ)
			long size = f.length();
			if(size <= 0) {
				throw new EIMException("EIM.ERROR.LOGIC.NOUPLOADFILE");
			}


			// ファイルサイズ判定(最大ファイルサイズ)
			long maxFileSize = Long.parseLong(ConfigUtils.getByKey("UPLOAD_FILE_SIZE_MAX"));
			if (size > maxFileSize) {
				throw new EIMException("EIM.ERROR.LOGIC.UPLOAD.FILE.SIZE.OVER", maxFileSize);
			}
			/*
			 * 登録オブジェクトがファイルの場合、
			 * 登録オブジェクトと同名のドキュメントの存在チェック。
			 * 以下の場合は登録不可。
			 *   11.登録オブジェクトがドキュメントで、かつ同名ドキュメントが存在し、かつドキュメントタイプが異なる
			 *   12.登録オブジェクトがドキュメントで、かつ同名ドキュメントが存在し、かつドキュメントタイプが等しく、かつ同名のドキュメントのステータスが「公開済」以外
			 *   13.登録オブジェクトがドキュメントで、かつ同名ドキュメントが存在し、かつドキュメントタイプが等しく、かつ（WFなし、またはWFありで同名のドキュメントのステータスが「公開済み」）、かつ同名のドキュメントがチェックアウト済み
			 * 以下の場合は登録確認
			 *   14.登録オブジェクトがドキュメントで、かつ同名ドキュメントが存在し、かつドキュメントタイプが等しく、かつ（WFなし、またはWFありで同名のドキュメントのステータスが「公開済み」）、かつ同名のドキュメントがチェックアウト済みではない
			 */
			EIMObject sameNameObj = null;

			String[] pathArray = {fullPath, fullPath, fullPath};
			List sameNameObjList = AppObjectUtil.getObjListByFullPass(_sess, pathArray, TYPE_ARRAY);
			if(f.isFile()) {
				sameNameObj = getObjectFromList(sameNameObjList, DOCUMENT_OBJ_TYPE);
				if(sameNameObj != null) {
					ret.put(SAME_NAME_OBJ,sameNameObj.getId() );

					/*
					 * 同名オブジェクトがドキュメントリンクの場合は、同一フォルダに同居が可能であるため
					 * 同名オブジェクトと見なさない。以下の条件を満たせば、同名オブジェクトチェックをしない。
					 *   A.同名のドキュメントリンクが存在する
					 *   B.同名のドキュメントが存在しない
					 */
					boolean onlyDocumentLinkExist = false;
					if(	_helper.isDocumentLink(parentObj, sameNameObj) &&
						_helper.isDocument(parentObj, sameNameObj) == false)
					{
						//ドキュメントリンクのみ存在する
						onlyDocumentLinkExist = true;
					}

					if(onlyDocumentLinkExist == false) {
						if(sameNameObj.getType().getId() != _docObjType.getId()) {
							//上記11
							throw new Exception("EIM.ERROR.INPUT.SAMENAMEDOCEXIST");
						}

						if(_helper.isTypeOfDocumentWithWorkflow(sameNameObj)) {
							EIMWorkFlow sameNameObjWfObj = WorkFlowUtils.getWorkFlowByType(_sess, sameNameObj.getType());
							List statusList = sameNameObjWfObj.getStatusTypeList();
							if(sameNameObj.getStatus().getType().getId() != ((EIMStatusType)statusList.get(statusList.size()-1)).getId()) {
								//上記12（同名オブジェクトのWFチェック）
								//「公開済」という言葉で比較は行えない（ユーザが変更できる））ため、ステータスタイプリストの
								//一番最後のステータスのIDと比較を行う
								throw new Exception("EIM.ERROR.INPUT.SAMENAMEDOCEXIST");
							}
						}
						else if(isTypeOfFolderWithWF(parentObj)) {
							EIMWorkFlow parentObjWfObj = WorkFlowUtils.getWorkFlowByType(_sess, parentObj.getType());
							List statusList = parentObjWfObj.getStatusTypeList();
							if(sameNameObj.getStatus().getType().getId() != ((EIMStatusType)statusList.get(statusList.size()-1)).getId()) {
								//上記12（親オブジェクトのWFチェック）
								//「公開済」という言葉で比較は行えない（ユーザが変更できる））ため、ステータスタイプリストの
								//一番最後のステータスのIDと比較を行う
								throw new Exception("EIM.ERROR.INPUT.SAMENAMEDOCEXIST");
							}
						}

						if(	sameNameObj.getLockUser() != null ||
							(sameNameObj.getStatus() == null // WFなしドキュメントの場合はチェックアウト可否判定をする
										&& AppLogicUtil.isCheckoutEnabledNoWFDoc(_sess, sameNameObj) == false))
						{
							//上記13
							throw new Exception("EIM.ERROR.INPUT.SAMEDOCCHECKOUT");
						}

						//上記14
						status = CONFIRM_UPLOAD;
						throw new Exception();
					}
				}
			}

			/* 登録オブジェクトがフォルダの場合、
			 * 登録オブジェクトと同名のフォルダの存在チェック。
			 * 以下の場合は登録不可。
			 *   21.登録オブジェクトがフォルダで、かつ同名フォルダが存在し、かつ同名のフォルダ（上位フォルダ含む）がWFあり
			 * 以下の場合は登録確認。
			 *   22.登録オブジェクトがフォルダで、かつ同名フォルダが存在し、かつ同名のフォルダ（上位フォルダ含む）がWFなし
			 */
			if(f.isDirectory())
			{
				sameNameObj = getObjectFromList(sameNameObjList, FOLDER_OBJ_TYPE);


				if(sameNameObj == null) {
					// 新規にフォルダを登録する場合、フォルダ構成管理権限のチェックを行う
					if (this.haveFolderSecurity(parentObj) == false) {
						throw new Exception("EIM.ERROR.LOGIC.NOCREATEROLE");
					}
				}
				else {
					// 同名のフォルダが存在する場合、存在するフォルダのセキュリティチェックを行う
					if (SecurityUtils.authorized(_sess, sameNameObj,_sess.getUser(), EIMAccessRole.CREATE) == false) {
						throw new Exception("EIM.ERROR.LOGIC.NOCREATEROLE");
					}

					if(isTypeOfFolderWithWF(sameNameObj)) {
						//上記21
						throw new Exception("EIM.ERROR.INPUT.SAMENAMEFOLDEREXIST");
					}

					//上記22
					status = CONFIRM_UPLOAD;
					throw new Exception();
				}
			}

			/*
			 * 登録オブジェクトと同名のタグの存在チェック。ある場合は登録不可。
			 */
			sameNameObj = getObjectFromList(sameNameObjList, TAG_OBJ_TYPE);
			if(sameNameObj != null) {
				throw new Exception("EIM.ERROR.INPUT.SAMENAMETAGEXIST");
			}

			/*
			 * 登録するオブジェクト名のチェック
			 */
			checkValidObjectName(f);

			//登録可能
			status = CAN_UPLOAD;
			ret.put(REASON, "");
		}
		catch(EIMException e) {
			if(status.equals(CANNOT_UPLOAD)) {
				//登録不可
				String reason = e.getMessage();
				if(reason == null || reason.length() == 0) {
					reason = EIMResource.getMessage("EIM.ERROR.SYSTEMERROR");
				}
				ret.put(REASON, reason);
			}
			else {
				//登録可能、もしくは登録確認
				ret.put(REASON, "");
			}
		}
		catch(Exception e) {
			if(status.equals(CANNOT_UPLOAD)) {
				//登録不可
				String reason = EIMResource.getMessage(e.getMessage());
				if(reason == null || reason.length() == 0) {
					reason = EIMResource.getMessage("EIM.ERROR.SYSTEMERROR");
				}
				ret.put(REASON, reason);
			}
			else {
				//登録可能、もしくは登録確認
				ret.put(REASON, "");
			}
		}
		finally {
			ret.put(STATUS, status);
		}

		return ret;
	}

	/**
	 * 存在する親オブジェクトをパスを上位にたどって行き取得する
	 * @param parentMap
	 * @param folderPath
	 * @return
	 */
	private EIMObject getExistParentObject(HashMap parentMap, String folderPath)
	{
		int searchIndex;
		String searchPath;
		String tmpPath = folderPath;

		// 存在するフォルダが見つかるまで検索する
		while ((searchIndex = tmpPath.lastIndexOf('/')) != -1)
		{
			searchPath = tmpPath.substring(0, searchIndex);
			EIMObject parentObj = (EIMObject)parentMap.get(searchPath);
			if (parentObj != null) {
				return parentObj;
			}

			// さらに上位のフォルダを検索
			tmpPath = searchPath;
		}

		return null;
	}

	/**
	 * フォルダ構成管理権限の有無を判定する
	 * @param object
	 * @return
	 * @throws Exception
	 * @throws EIMException
	 */
	private boolean haveFolderSecurity(EIMObject object)
	throws Exception, EIMException
	{
		long folderSecNo= AppObjectUtil.getIntAttr(_sess, object, EIMConfig.get("ATTR_NAME_WORKSPACE_LOW_FOLDER_SEC"), Integer.MIN_VALUE);
		if (folderSecNo != Integer.MIN_VALUE)
		{
			// 下位フォルダ管理セキュリティのロールチェック
			if (!AppSecurityUtils.authorizedLowFolderSecurity(_sess, object, _sess.getUser(), EIMAccessRole.UPDATE))
			{

				// フォルダ構成管理権限無し
				return false;
			}
		}
		return true;
	}

	/**
	 * リストから指定したタイプのオブジェクトを取得する
	 * @param list EIMObjectリスト
	 * @param typeName タイプ名（「ドキュメント」「フォルダ」「タグ」の3種類）
	 */
	private EIMObject getObjectFromList(List list, String typeName) throws Exception
	{
		for(int ii = 0; ii < list.size(); ii++) {
			EIMObject obj = (EIMObject)list.get(ii);
			if(	(typeName.equals(DOCUMENT_OBJ_TYPE) && _helper.isTypeOfDocument(obj.getType())) ||
				(typeName.equals(FOLDER_OBJ_TYPE) && _helper.isTypeOfFolder(obj.getType())) ||
				(typeName.equals(TAG_OBJ_TYPE) && _helper.isTypeOfTag(obj.getType())))
			{
				return obj;
			}
		}
		return null;
	}

	/**
	 * クライアント表示用のパス文字列を取得
	 * @param path パス
	 * @param includeObjName パス文字列にファイル・フォルダ名を含むかどうか
	 */
	private String getPath(String path, boolean includeFileName)
	{
		//"\"を"/"に変更
		String ret = path.replaceAll("\\\\", "/");

		//pathから一時格納フォルダ文字列を削除し、実際のパスを追加する
		ret = _realFolderPath + ret.replaceAll(_tmpFolderPath, "");

		//"//"の部分を"/"に修正
		ret = ret.replaceAll("//", "/");

		//必要であればドキュメント名またはフォルダ名部分を削除
		return includeFileName?
				ret :
				ret.substring(0, ret.lastIndexOf("/") + 1);
	}

	/**
	 * 指定したオブジェクトが以下のいずれかの場合はtrueを返す。
	 *   ・指定オブジェクト自身がワークフロー付きフォルダ
	 *   ・指定オブジェクトの上位にワークフロー付きフォルダがある
	 */
	private boolean isTypeOfFolderWithWF(EIMObject obj) throws Exception
	{
		if(	_helper.isTypeOfFolderWithWorkflow(obj) ||
			_helper.isTypeOfFolderUnderFolderWithWorkflow(obj))
		{
			return true;
		}
		return false;
	}

	/**
	 * 登録オブジェクトのチェック。以下の場合はエラー（登録不可）。
	 *   31.登録オブジェクトがドキュメントで、かつオブジェクト名称に拡張子が存在しない
	 *   32.登録オブジェクト名称にWindows禁則文字を使用している
	 */
	private void checkValidObjectName(File f) throws Exception
	{
		try {
			// Windows禁止文字チェック
			AppObjectUtil.checkValidateFName(_sess, f.getName());
		}
		catch(EIMException e) {
			//上記32
			throw new Exception("EIM.WARN.LOGIC.INVALIDNAME");
		}
	}
}
