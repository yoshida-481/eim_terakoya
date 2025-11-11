package eim.command.business.service.execute;

import java.util.ArrayList;
import java.util.List;

import common.util.AppObjectConditionHelper;
import common.util.AppUpdateNoticeUtils;

import eim.bo.EIMException;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMResource;
import eim.command.business.service.FolderService;
import eim.command.business.service.result.EIMCommandResult;
import eim.command.business.service.result.EIMCommandResultDocument;
import eim.command.common.util.DocumentManagementUtils;
import eim.command.common.util.EIMCommandConstant;
import eim.command.common.util.ErrorMappingUtils;
import eim.util.ObjectUtils;
import eim.util.StringUtils;

public class EIMCommandMkdirExecuter extends EIMCommandExecuter {

	// 重複チェックパラメータ
	private List<String> paths = new ArrayList<String>();
	private List<String> types = new ArrayList<String>();
	private List<String> names = new ArrayList<String>();
	
	@Override
	public void setParameter(String key, String value) {
		if(key.equals(EIMCommandConstant.PATH)) {
			paths.add(value);
		} else if(key.equals(EIMCommandConstant.TYPE)) {
			types.add(value);
		} else if(key.equals(EIMCommandConstant.NAME)) {
			names.add(value);
		}
		super.setParameter(key, value);
	}

	@Override
	public EIMCommandResult execute() throws Exception {
		
		EIMCommandResultDocument result =  new EIMCommandResultDocument(getSess());
		
		// パラメータチェック
		if (ParamCheck(result)) {
		
			// 親オブジェクトの取得
			EIMObject parentObj = getParentObject(result);
			if (result.getType() == null 
					|| !result.getType().equals(EIMResource.getMessageValue("EIM.RESULT.TYPE.ERROR"))) {
				
				// フォルダタイプの取得
				EIMObjectType objType = null;
				if (!StringUtils.isBlank(getType())) {
					objType = ObjectUtils.getObjectTypeByName(getSess(), getType());
				}
				
				// フォルダ作成
				EIMObject folderObj = createFolder(result, parentObj, objType);
				
				// 実行結果を設定
				if (folderObj != null) {
					result.setType(EIMResource.getMessageValue("EIM.RESULT.TYPE.INFO"));
					result.setTarget(folderObj);
				}

				// SearchFramework 検索FW更新通知 対象：フォルダ、親のフォルダor親ワークスペース
				AppUpdateNoticeUtils.updateNoticeInsert(folderObj.getId(), "SEARCHFW_CREATE_FOLDER");
				AppUpdateNoticeUtils.updateNoticeInsertParent(getSess(), parentObj, 
						"SEARCHFW_CREATE_FOLDER_PARENT_FOLDER", "SEARCHFW_CREATE_FOLDER_PARENT_WORKSPACE", null);
			}
			
		}
		return result;
		
	}
	
	/**
	 * パラメータチェック
	 * @param result
	 * @return
	 * @throws Exception
	 */
	private boolean ParamCheck(EIMCommandResult result) throws Exception{
		
		try {
			
			// 必須パラメータ存在チェック（値が空かどうかのチェックは行わない）
			if (getPath() == null)  {
				setErrorCodeMessage(result, "EIM-00110", new Object[]{EIMCommandConstant.PATH});
				return false;
			}
			if (getName() == null) {
				setErrorCodeMessage(result, "EIM-00110", new Object[]{EIMCommandConstant.NAME});
				return false;
			}
			
			// 各パラメータの重複チェック
			if (paths.size() > 1) {
				setErrorCodeMessage(result, "EIM-00120", new Object[]{EIMCommandConstant.PATH});
				return false;
			}
			if (types.size() > 1) {
				setErrorCodeMessage(result, "EIM-00120", new Object[]{EIMCommandConstant.TYPE});
				return false;
			}
			if (names.size() > 1) {
				setErrorCodeMessage(result, "EIM-00120", new Object[]{EIMCommandConstant.NAME});
				return false;
			}
			
		} catch (Exception e) {
			throw e;
		}
		return true;
	}
	
	/**
	 * 指定のpath値に相当する親オブジェクトを取得する
	 * @param result
	 * @return 相当する親オブジェクトがない場合はnullを返却
	 * @throws Exception
	 */
	private EIMObject getParentObject(EIMCommandResult result) throws Exception {
		
		EIMObject parentObj = null;
		String tempPath = getPath();
		if (!StringUtils.isBlank(tempPath)) {
			
			if (tempPath.equals("/")) {
				setErrorCodeMessage(result, "EIM-00640");
			}
			else {
				
				// 末尾がスラッシュであれば削除
				if (tempPath.length() > 1 && tempPath.endsWith("/")) {
					tempPath = tempPath.substring(0, tempPath.length() - 1);
				}
				// path値をパスと名称とに分割
				String path = "";
				String name = "";
				if (tempPath.lastIndexOf("/") > 0) {
					path = tempPath.substring(0, tempPath.lastIndexOf("/") + 1);
					name = tempPath.substring(tempPath.lastIndexOf("/") + 1);
				} else {
					path = "/";
					name = tempPath.substring(1, tempPath.length());
				}

				DocumentManagementUtils dmu = new DocumentManagementUtils(getSess());
				// パスと名称を条件にオブジェクト検索
				List<EIMObject> parentObjList = dmu.getObjectListByPathAndNameForMkdirCommand(path, name);
				
				if (parentObjList.size() > 0) {
					if (parentObjList.size() == 1) {
						// 検索結果が1件
						parentObj = parentObjList.get(0);
					}
					else {
						// 検索結果が複数件
						AppObjectConditionHelper helper = new AppObjectConditionHelper(getSess());
						// フォルダタイプのオブジェクトを検索結果から取得する
						for (EIMObject element : parentObjList) {
							parentObj = element;
							if (helper.isTypeOfFolder(element.getType())) {
								break;
							}
						}
					}
				}
			}
		}
		return parentObj;
	}
	
	/**
	 * フォルダ作成サービスを呼び出してフォルダを作成する
	 * @param result
	 * @param parentObj
	 * @param objType
	 * @return 作成したフォルダオブジェクト
	 * @throws EIMException
	 */
	private EIMObject createFolder(EIMCommandResult result, EIMObject parentObj, EIMObjectType objType) throws EIMException, Exception {
		
		EIMObject folderObject = null;
		// サービス生成
		FolderService service = new FolderService();
		
		try {
			if (getType() == null) {
				// フォルダタイプ指定なし
				folderObject = service.createFolder(parentObj, getName());
			}
			else {
				// フォルダタイプ指定あり
				folderObject = service.createFolder(parentObj, objType, getName());
			}
			
			if (folderObject == null) {
				setErrorCodeMessage(result, "EIM-00900");
				throw new EIMException();
			}
			
		} catch (EIMException e) {
			String errorCode = ErrorMappingUtils.getExternalErrorCode(e);
			if (errorCode == null) {
				setErrorCodeMessage(result, "EIM-00900");
				throw e;
			}
			else {
				if (errorCode.equals("EIM-00330")) {
					// ワークフローが既に開始されている
					setErrorCodeMessage(result, errorCode, new Object[]{getPath()});
				}
				else if (errorCode.equals("EIM-00370")) {
					// フォルダ構成管理権限なし
					setErrorCodeMessage(result, errorCode, new Object[]{getPath()});
				}
				else if (errorCode.equals("EIM-00380")) {
					// オブジェクトタイプの書込権限なし
					setErrorCodeMessage(result, errorCode, new Object[]{getType()});
				}
				else if (errorCode.equals("EIM-00530")) {
					// オブジェクトタイプがフォルダでない
					setErrorCodeMessage(result, errorCode, new Object[]{getType()});
				}
				else if (errorCode.equals("EIM-00560")) {
					// フォルダ名称に禁止文字あり
					setErrorCodeMessage(result, errorCode, new Object[]{getName()});
				}
				else if (errorCode.equals("EIM-00570")) {
					// オブジェクトタイプが存在しない
					setErrorCodeMessage(result, errorCode, new Object[]{getType()});
				}
				else if (errorCode.equals("EIM-00900")) {
					setErrorCodeMessage(result, errorCode, new Object[]{getType()});
					throw e;
				}
				else {
					setErrorCodeMessage(result, errorCode);
				}
			}
		}
		return folderObject;
	}
	
	/**
	 * エラー情報を設定
	 * @param result
	 * @param codeKey
	 * @throws EIMException
	 */
	private void setErrorCodeMessage(EIMCommandResult result, String codeKey) throws EIMException{
		setErrorCodeMessage(result, codeKey, (Object[])null);
	}

	/**
	 * エラー情報を設定
	 * @param result
	 * @param codeKey
	 * @param messageParams メッセージ置換パラメータ
	 * @throws EIMException
	 */
	private void setErrorCodeMessage(EIMCommandResult result, String codeKey, Object[] messageParams) throws EIMException{
		String messageKey = EIMResource.getMessageValue(codeKey);
		result.setTypeCodeMessage(EIMResource.getMessageValue("EIM.RESULT.TYPE.ERROR"), codeKey, EIMResource.getMessageValue(messageKey, messageParams));
	}
	
}
