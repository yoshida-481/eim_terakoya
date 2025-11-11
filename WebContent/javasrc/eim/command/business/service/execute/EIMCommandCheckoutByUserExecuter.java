package eim.command.business.service.execute;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;

import common.util.AppConstant;
import common.util.AppLogicUtil;
import common.util.AppObjectConditionHelper;
import common.util.AppObjectUtil;
import common.util.AppUpdateNoticeUtils;
import common.util.AttributeUtil;
import common.util.DisplayColorUtil;
import eim.bo.EIMAccessRole;
import eim.bo.EIMFile;
import eim.bo.EIMFormat;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMResource;
import eim.bo.EIMUser;
import eim.bo.EIMVersion;
import eim.command.business.service.EIMCommandService;
import eim.command.business.service.result.EIMCommandResult;
import eim.command.business.service.result.EIMCommandResultData;
import eim.command.common.DownloadDocument;
import eim.command.common.util.DocumentManagementUtils;
import eim.command.common.util.EIMCommandConstant;
import eim.command.common.util.EIMCommandObjectUtil;
import eim.command.common.util.EIMCommandResultConstant;
import eim.command.common.util.EIMCommandUtil;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.EIMConstant;
import eim.util.EIMXmlConfigAdminAuth;
import eim.util.FileUtils;
import eim.util.ObjectUtils;
import eim.util.OperationHistoryUtils;
import eim.util.SecurityUtils;
import eim.util.StringUtils;
import eim.util.UserUtils;
import eim.util.VersionUtils;

/**
 * checkoutByUserコマンド用コマンド実行実装クラス
 *
 *
 */
public class EIMCommandCheckoutByUserExecuter extends EIMCommandExecuter {

	EIMCommandResultData result = null;
	private String objId = null;
	private EIMUser processUser = null;
	private EIMUser loginUser = null;

	// 複数引数のチェック用
	private List<String> objIds = new ArrayList<String>();
	private List<String> procUsers = new ArrayList<String>();
	private List<String> paths = new ArrayList<String>();


	public EIMCommandCheckoutByUserExecuter(){
	}

	/* (非 Javadoc)
	 * @see command.business.service.execute.EIMCommandExecuter#setOtherParameter(java.lang.String, java.lang.String)
	 */
	@Override
	public void setParameter(String key, String value) {
		if(key.equals(EIMCommandConstant.OBJID)) {
			objIds.add(value);
		} else if(key.equals(EIMCommandConstant.PROCUSER)) {
			procUsers.add(value);
		} else if(key.equals(EIMCommandConstant.PATH)) {
			paths.add(value);
		}
		super.setParameter(key, value);
	}

	public EIMCommandResultData execute() throws Exception{
		//Session
		EIMSession sess = super.getSess();
		result = new EIMCommandResultData(sess);


		//オブジェクト取得用
		EIMObject obj = null;
		String folderPath = null;
		String fileName = null;

		//パラメータ解析
		if(!checkParam(sess, result))
			return result;

		//ユーザ管理権限チェック
		if(!EIMXmlConfigAdminAuth.hasSpecifiedAuth(sess.getUser(), AppConstant.ADMIN_AUTH_ID_USER)){
			setErrorResult("EIM.ERROR.CODE.NO.USER.MANAGE.AUTH", "EIM.ERROR.LOGIC.NO.USER.MANAGE.AUTH", null, super.getUser());
			return result;
		}

		//処理実施ユーザで実行
		change2ProcUser();

		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

		//指定した引数が「objId」のとき
		if(objId != null){
			obj = ObjectUtils.getObjectById(sess, Integer.valueOf(objId));

			if(obj == null) {
				// オブジェクトが取得できなかった場合はエラー
				setErrorResult("EIM.ERROR.CODE.OJBECT.NO.EXIST", "EIM.ERROR.LOGIC.OJBECT.NO.EXIST.FOR.ANY", null, objId);
				return result;
			}
			else if (!isDocumentType(obj)) {
				// オブジェクトがドキュメントでなかったらエラー
				setErrorResult("EIM.ERROR.CODE.OBJECT.ISNOT.DOCUMENT", "EIM.ERROR.LOGIC.OBJECT.ISNOT.DOCUMENT", obj, objId);
				return result;
			}
			fileName = obj.getName();
			folderPath = obj.getAttribute(EIMConfig.getValue("ATTR_NAME_DOCUMENT_PASS")).getStrings()[0];
		}else{//指定した引数が「path」のとき
			String fullPath = super.getPath();
			folderPath = fullPath.substring(0, fullPath.lastIndexOf("/") + 1);
			fileName = fullPath.substring(fullPath.lastIndexOf("/") + 1);
			DocumentManagementUtils dmu = new DocumentManagementUtils(sess);


			//パスと名称からオブジェクトを取得する
			List<EIMObject> tmpObjList = dmu.getObjectListByPathAndName(folderPath, fileName);
			if (tmpObjList == null || tmpObjList.size() <= 0) {
				// オブジェクトが取得できなかった場合はエラー
				setErrorResult("EIM.ERROR.CODE.OJBECT.NO.EXIST", "EIM.ERROR.LOGIC.OJBECT.NO.EXIST.FOR.ANY", null, super.getPath());
				return result;
			}

			boolean isDocLink = false;
			boolean hasDocumentTypeError = false;
			for (EIMObject tmpObj : tmpObjList) {

				if (isDocumentType(tmpObj)) {

					// パス属性の最初の値(key=1)と指定されたパスの値が異なればドキュメントリンクと判定
					String path = tmpObj.getAttribute(EIMConfig.getValue("ATTR_NAME_DOCUMENT_PASS")).getStrings()[0];
					if (path != null && !path.equals(folderPath)) {
						isDocLink = true;
					}
					else if (tmpObj.getLatest())
						obj = tmpObj;
				}
				else {
					hasDocumentTypeError = true;
				}
			}


			if (obj == null) {
				if (hasDocumentTypeError) {
					// オブジェクトがドキュメントでなかったらエラー
					setErrorResult("EIM.ERROR.CODE.OBJECT.ISNOT.DOCUMENT", "EIM.ERROR.LOGIC.OBJECT.ISNOT.DOCUMENT", obj, super.getPath());
					return result;
				}
				if (isDocLink) {
					// ドキュメントリンクはチェックアウトできない
					setErrorResult("EIM.ERROR.CODE.CANNOT.CHECKOUT.DOCUMENTLINK", "EIM.ERROR.LOGIC.CANNOT.CHECKOUT.DOCUMENTLINK", null);
					return result;
				}
				// オブジェクトが取得できなかった場合はエラー
				setErrorResult("EIM.ERROR.CODE.OJBECT.NO.EXIST", "EIM.ERROR.LOGIC.OJBECT.NO.EXIST.FOR.ANY", null, super.getPath());
				return result;
			}
		}

		//権限チェック
		boolean enabled = true;
		do {
			//Check REVISION_UP Role
			if(obj.getSecurity() != null
					&& !SecurityUtils.authorized(sess, obj, processUser, EIMAccessRole.REVISION_UP)) {
				enabled = false;
				break;
			}
			//ワークフロー付きフォルダ下の場合は、チェックアウトできない
			if (helper.isUnderFolderWithWorkflow(obj)) {
				enabled = false;
				break;
			}
		} while (false);
		if(!enabled)
		{
			setErrorResult("EIM.ERROR.CODE.NO.CHECKOUT.AUTH", "EIM.ERROR.LOGIC.NO.CHECKOUT.AUTH", obj);
			return result;
		}

		//Check Status (WFなしドキュメントもチェックアウトできる)
		if(obj.getStatus() != null && obj.getStatus().getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_PUBLIC)
		{
			setErrorResult("EIM.ERROR.CODE.ISNOT.OPEN.TO.PUBLIC", "EIM.ERROR.LOGIC.ISNOT.OPEN.TO.PUBLIC", obj, obj.getName());
			return result;
		}

		//Check Lock User
		EIMUser user = obj.getLockUser();
		if ((user != null)
				|| (obj.getStatus() == null // WFなしドキュメントの場合はチェックアウト可否判定をする
						&& !AppLogicUtil.isCheckoutEnabledNoWFDoc(sess, obj)))
		{
			setErrorResult("EIM.ERROR.CODE.ALREADY.CHECKOUT", "EIM.ERROR.LOGIC.ALREADY.CHECKOUT", obj, obj.getName());
			return result;
		}

		//Version
		EIMVersion version = VersionUtils.getVersion(sess, obj);

		//対象が最新のリビジョンかチェック
		if (obj.getRevision() != version.getMaxRevision()) {
			setErrorResult("EIM.ERROR.CODE.ISNOT.LATEST.REVISION","EIM.ERROR.LOGIC.ISNOT.LATEST.REVISION", obj, obj.getName());
			return result;
		}

		//Lock
		EIMObject newObj = null;
		try
		{
			newObj = VersionUtils.revisionUp(sess, obj);
		}
		catch(FileNotFoundException e)
		{
			// ファイルが見つからない場合
			setErrorResult("EIM.ERROR.CODE.NOT.GET.FILE", "EIM.ERROR.LOGIC.NOT.GET.FILE", obj);
			return result;
		}

		// WFありの場合、公開フォーマットのメタ情報と実ファイルを削除
		if (helper.isTypeOfDocumentWithWorkflow(newObj)) {
			EIMFormat newObjPubFormat = FileUtils.getFormatByName(sess, EIMConfig.get("FORMAT_NAME_PUBLIC"));
			EIMFile newObjPubMetaInfo = FileUtils.getFile(sess, newObj, newObjPubFormat);
			FileUtils.deleteFile(sess, newObj, newObjPubFormat);

			File newObjPubFile = new File(newObjPubMetaInfo.getDirectory().getPath() + FileUtils.getFileName(newObj, newObjPubMetaInfo));
			if (newObjPubFile != null) {
				newObjPubFile.delete();
			}
		}

		// 継承しない属性を削除
		AttributeUtil.deleteNonInheritAttribute(sess, newObj);

		// 属性表示色を更新（オブジェクトをもう一度取り直す）
		newObj = ObjectUtils.getObjectById(sess, newObj.getId());
		DisplayColorUtil.copyDisplayColor(sess, newObj, obj);

		//パス
		String path = StringUtils.nullToBlank(AppObjectUtil.getPath(obj));

		// パス属性は先頭の一つのみをセットする(複数値の2つ目以降は継承元ドキュメントのリンク情報のため)
		AppObjectUtil.setPath(sess, newObj, path);

		obj = VersionUtils.setLatest(sess, obj);
		//ドキュメント管理の使用でlatestflagをobjectとnewObjectで２つ立てる
		// VersionUtils.setLatestForce(sess, object);ではフラグを１つしか立てないようにしているので、
		//かならずこの後に呼ぶ
		VersionUtils.setLatestWithNoCheck(sess, newObj, true);

		// SearchFramework 検索FW更新通知 対象：対象ドキュメント、次リビジョンドキュメント
		AppUpdateNoticeUtils.updateNoticeInsert(obj.getId(), "SEARCHFW_CHECK_OUT_DOCUMENT");
		AppUpdateNoticeUtils.updateNoticeInsert(newObj.getId(), "SEARCHFW_CHECK_OUT_NEW_DOCUMENT");

		//Access
		createAccessHistory(obj, "EIM.ACCESS.TYPE.EXIF.LOCK");
		createAccessHistory(obj, "EIM.ACCESS.TYPE.EXIF.REBISIONUP");

		//Create Operation History
		createOperationHistory(EIMCommandConstant.CHECK_OUT_BY_USER_EXCOMMAND,
									EIMCommandConstant.TARGET_TO_UPDATE,
									obj);

		//ダウンロード処理
		EIMFormat format = FileUtils.getDefaultFormat(sess, newObj.getType());
		setFileInfo2Result(format, obj, result);

		return result;
	}

	// タイプがドキュメントかどうか判定
	private boolean isDocumentType(EIMObject object) throws Exception {
		EIMObjectType rootType = EIMCommandObjectUtil.getRootObjType(getSess(), object.getId());
		if (rootType != null && rootType.getName().equals(EIMConfig.getValue("OBJECT_TYPE_NAME_DOCUMENT"))) {
			return true;
		}
		return false;
	}

	//エラー結果を設定する
	private void setErrorResult(String code, String resouceKey, EIMObject obj, String... param) throws Exception{
		if(result == null){
			result = new EIMCommandResultData();
		}
		if(param.length == 0){
			result.setTypeCodeMessage(EIMCommandResultConstant.TYPE_ERROR,
					EIMResource.getMessage(code),
					EIMResource.getMessageValue(resouceKey));
			if(obj!=null)
				result.setTarget(obj);

		}else if(param.length == 1){

			result.setTypeCodeMessage(EIMCommandResultConstant.TYPE_ERROR,
					EIMResource.getMessage(code),
					EIMResource.getMessageValue(resouceKey, new Object[]{StringUtils.xmlEncode(param[0])}));
			if(obj!=null)
				result.setTarget(obj);

		}
	}

	//レスポンス用の処理

	//ファイルデータ用
	private void setFileInfo2Result(EIMFormat format, EIMObject object, EIMCommandResultData result) throws Exception{
		// ダウンロード実行
		DownloadDocument doc = new DownloadDocument(super.getSess(), format);
		result = doc.doDownloadDocument(null, object, result, false);

	}


	//パラメータ解析実行メソッド
	private boolean checkParam(EIMSession sess, EIMCommandResult result) throws Exception{

		int pathInterFlg = 0;
		int objInterFlg = 0;

		// pathが複数指定されていないか
		if (paths.size() > 1) {
			setErrorResult("EIM.ERROR.CODE.OVERLAPPING.SINGLE.PARAMETER","EIM.ERROR.LOGIC.OVERLAPPING.SINGLE.PARAMETER", null, EIMCommandConstant.PATH);
			return false;
		}

		if(super.getPath() != null) {
			//空文字列であるかチェック
			if((super.getPath()).length() == 0) {
				setErrorResult("EIM.ERROR.CODE.OJBECT.NO.EXIST", "EIM.ERROR.LOGIC.OJBECT.NO.EXIST", null);
				return false;
			}

			pathInterFlg++;
		}
		// objIdが複数指定されていないか
		if (objIds.size() > 1) {
			setErrorResult("EIM.ERROR.CODE.OVERLAPPING.SINGLE.PARAMETER","EIM.ERROR.LOGIC.OVERLAPPING.SINGLE.PARAMETER", null, EIMCommandConstant.OBJID);
			return false;
		}

		if(objIds.size() == 1) objInterFlg++;

		//オブジェクトIDとパスがともに指定されている、もしくは、どちらも指定されていない場合はエラー
		if((pathInterFlg > 0 && objInterFlg > 0)||(pathInterFlg == 0 && objInterFlg == 0)){
			setErrorResult("EIM.ERROR.CODE.INVALID.PATH.OBJID", "EIM.ERROR.LOGIC.INVALID.PATH.OBJID", null);
			return false;
		}
		if(pathInterFlg == 0 && objInterFlg > 0)
		{
			objId = objIds.get(0);
			//空文字列であるかチェック
			if(objId.length() == 0) {
				setErrorResult("EIM.ERROR.CODE.OBJID.NOT.NUM", "EIM.ERROR.LOGIC.OBJID.NOT.NUM", null);
				return false;
			}
		}

		// procUserがあるか
		if (procUsers.size() < 1) {
			setErrorResult("EIM.ERROR.CODE.NO.ESSENTIAL.PARAMETER","EIM.ERROR.LOGIC.NO.ESSENTIAL.PARAMETER", null, EIMCommandConstant.PROCUSER);
			return false;
		}
		// procUserが複数指定されていないか
		if (procUsers.size() > 1) {
			setErrorResult("EIM.ERROR.CODE.OVERLAPPING.SINGLE.PARAMETER","EIM.ERROR.LOGIC.OVERLAPPING.SINGLE.PARAMETER", null, EIMCommandConstant.PROCUSER);
			return false;
		}
		if(( processUser = UserUtils.getUserByCode(sess, procUsers.get(0))) == null){
			setErrorResult("EIM.ERROR.CODE.PROCUSER.NO.EXIST", "EIM.ERROR.LOGIC.PROCUSER.NO.EXIST", null, procUsers.get(0));
			return false;
		}

		// objIdに半角数字以外のものが指定されていないかチェック
		if(objInterFlg > 0 && !EIMCommandUtil.isOneByteNum(objId))
		{
			setErrorResult("EIM.ERROR.CODE.OBJID.NOT.NUM", "EIM.ERROR.LOGIC.OBJID.NOT.NUM", null);
			return false;
		}

		return true;
	}


	/**
	 * 操作履歴を登録
	 * @param operationTypeNo 操作種別（の番号）
	 * @param targetInfoNo 操作対象情報（の番号）
	 * @param targetObj 操作対象のEIMObject ※なければnull
	 * @throws Exception
	 */
	@Override
	protected void createOperationHistory(String operationTypeNo, String targetInfoNo, EIMObject targetObj) throws Exception {

		// セッションユーザをログインユーザに戻す
		change2LoginUser();

		// 表示するパスはオブジェクトから取得する
		String dspPath = null;
		if(targetObj != null) {
			// パス＋オブジェクト名称
			dspPath = AppObjectUtil.getPath(targetObj) + targetObj.getName();
		}

		OperationHistoryUtils.create(getSess(), EIMCommandConstant.COMMAND, operationTypeNo,
				targetInfoNo, targetObj!=null ? EIMConstant.OBJECT : null, targetObj,
				EIMCommandConstant.PROC_USER, EIMConstant.USER, processUser,
				getPath()!=null ? EIMCommandService.VERSION + ":" + dspPath : EIMCommandService.VERSION);

		// セッションユーザを再度処理実行ユーザに変更
		change2ProcUser();
	}

	/**
	 * セッションを処理実施ユーザに切替える
	 */
	public void change2ProcUser() throws Exception{
		EIMSession sess = getSess();
		if (sess != null) {
			if (loginUser == null) loginUser = sess.getUser();
			sess.setUser(processUser);
			sess.setAttribute(EIMSession.USER, processUser);
		}
	}

	/**
	 * セッションをログインユーザに切替える
	 */
	public void change2LoginUser() throws Exception{
		EIMSession sess = getSess();
		if (sess != null) {
			if (processUser == null) processUser = sess.getUser();
			sess.setUser(loginUser);
			sess.setAttribute(EIMSession.USER, loginUser);
		}
	}

}
