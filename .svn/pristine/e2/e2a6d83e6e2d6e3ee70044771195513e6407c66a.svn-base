package eim.command.business.service.execute;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import common.util.AppConstant;
import common.util.AppObjectUtil;
import common.util.AppUpdateNoticeUtils;
import common.util.OptionConfData;
import eim.bo.EIMAccessRole;
import eim.bo.EIMDirectory;
import eim.bo.EIMException;
import eim.bo.EIMFile;
import eim.bo.EIMFormat;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMRelation;
import eim.bo.EIMResource;
import eim.bo.EIMUser;
import eim.bo.EIMVersion;
import eim.command.business.service.EIMCommandService;
import eim.command.business.service.result.EIMCommandResult;
import eim.command.business.service.result.EIMCommandResultDocument;
import eim.command.common.util.DocumentManagementUtils;
import eim.command.common.util.EIMCommandConstant;
import eim.command.common.util.EIMCommandObjectUtil;
import eim.command.common.util.EIMCommandResultConstant;
import eim.command.common.util.EIMCommandUtil;
import eim.net.EIMSession;
import eim.util.AccessUtils;
import eim.util.EIMConfig;
import eim.util.EIMConstant;
import eim.util.EIMXmlConfigAdminAuth;
import eim.util.FileUtils;
import eim.util.ObjectUtils;
import eim.util.OperationHistoryUtils;
import eim.util.RelationUtils;
import eim.util.SecurityUtils;
import eim.util.StringUtils;
import eim.util.UserUtils;
import eim.util.VersionUtils;

/**
 * checkinByUserコマンド用コマンド実行実装クラス
 *
 *
 */
public class EIMCommandCheckinByUserExecuter extends EIMCommandExecuter {

	EIMCommandResultDocument result = null;
	private String objId = null;
	private String procUser = null;
	private EIMUser processUser = null;
	private EIMUser loginUser = null;
	private File tmpFile = null;

	// 複数引数のチェック用
	private List<String> objIds = new ArrayList<String>();
	private List<String> procUsers = new ArrayList<String>();
	private List<String> paths = new ArrayList<String>();

	public EIMCommandCheckinByUserExecuter(){
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
		} else if(key.equals(EIMCommandConstant.PATH)){
			paths.add(value);
		}
		super.setParameter(key, value);
	}

	@SuppressWarnings("unchecked")
	public EIMCommandResult execute() throws Exception{

		EIMSession sess = super.getSess();
		result = new EIMCommandResultDocument(sess);

		//オブジェクト取得
		EIMObject obj = null;
		String folderPath = null;
		String fileName = null;

		//パラメータ解析
		if(!checkParam(sess, result))
			return result;

		//ユーザ管理権限チェック
		if(!EIMXmlConfigAdminAuth.hasSpecifiedAuth(sess.getUser(), AppConstant.ADMIN_AUTH_ID_USER)){
			setErrorResult("EIM.ERROR.CODE.NO.USER.MANAGE.AUTH", "EIM.ERROR.LOGIC.NO.USER.MANAGE.AUTH", super.getUser());
			return result;
		}

		//処理実施ユーザで実行
		change2ProcUser();

		//指定した引数が「objId」のとき
		if(objId != null){
			obj = ObjectUtils.getObjectById(sess, Integer.valueOf(objId));

			//オブジェクトが取得できなかった場合はエラー
			if(obj == null){
				setErrorResult("EIM.ERROR.CODE.OJBECT.NO.EXIST", "EIM.ERROR.LOGIC.OJBECT.NO.EXIST.FOR.ANY", null, objId);
				return result;
			}
			else if (!isDocumentType(obj)) {
				// オブジェクトがドキュメントでなかったらエラー
				setErrorResult("EIM.ERROR.CODE.OBJECT.ISNOT.DOCUMENT", "EIM.ERROR.LOGIC.OBJECT.ISNOT.DOCUMENT", objId);
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
					else if (tmpObj.getLatest()) {
						if (obj == null) {
							obj = tmpObj;
						}
						else if (obj.getRev() < tmpObj.getRev()) {
							// 同じパスで複数のドキュメントがヒットした場合は履歴の数字が大きい方を採用
							obj = tmpObj;
						}
					}
				}
				else {
					hasDocumentTypeError = true;
				}
			}

			if (obj == null) {
				if (hasDocumentTypeError) {
					// オブジェクトがドキュメントでなかったらエラー
					setErrorResult("EIM.ERROR.CODE.OBJECT.ISNOT.DOCUMENT", "EIM.ERROR.LOGIC.OBJECT.ISNOT.DOCUMENT", super.getPath());
					return result;
				}
				if (isDocLink) {
					// ドキュメントリンクはチェックインできない
					setErrorResult("EIM.ERROR.CODE.CANNOT.CHECKIN.DOCUMENTLINK", "EIM.ERROR.LOGIC.CANNOT.CHECKIN.DOCUMENTLINK");
					return result;
				}
				// オブジェクトが取得できなかった場合はエラー
				setErrorResult("EIM.ERROR.CODE.OJBECT.NO.EXIST", "EIM.ERROR.LOGIC.OJBECT.NO.EXIST.FOR.ANY", null, super.getPath());
				return result;
			}
		}

		//アップロードファイル名取得
		String tmpFileName = super.getNames()[0];
		String tmpFileExt = StringUtils.getFileExt(tmpFileName);

		// 拡張子がありません
		if (tmpFileExt == null || tmpFileExt.equals("")){
			setErrorResult("EIM.ERROR.CODE.NO.FILE.EXTENT", "EIM.ERROR.LOGIC.NO.FILE.EXTENT.FOR.ANY", tmpFileName);
			return result;
		}

		//tmpファイルが存在しない場合、システムエラー
		if(!tmpFile.exists()){
			setErrorResult("EIM.ERROR.CODE.SYSTEM.ERROR", "EIM.ERROR.LOGIC.SYSTEM.ERROR");
			throw new Exception();
		}


		//実体ファイルのサイズがゼロかどうか
		if(tmpFile.length() == 0){
			setErrorResult("EIM.ERROR.CODE.FILE.NO.EXIST.OR.SIZEZERO", "EIM.ERROR.LOGIC.FILE.NO.EXIST.OR.SIZEZERO", tmpFileName);
			return result;
		}

		//チェックイン処理
		//チェックイン権限チェック
		if(!SecurityUtils.authorized(sess, obj, processUser, EIMAccessRole.CHECKIN))
		{
			setErrorResult("EIM.ERROR.CODE.NO.CHECKIN.AUTH", "EIM.ERROR.LOGIC.NO.CHECKIN.AUTH");
			return result;
		}

		//Version
		EIMVersion version = VersionUtils.getVersion(sess, obj);

		//ステータスのチェック
		boolean statusError = false;
		if (obj.getStatus() == null) {
			// ステータスが「改訂中」
			// statusがnull かつ 最新リビジョンでない かつ latestフラグがたっている場合
			if (obj.getRevision() != version.getMaxRevision() && obj.getLatest())
			{
				statusError = true;
			}
		}
		else if(obj.getStatus().getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_EDITTING)
		{
			// ステータスが「編集中」
			statusError = true;
		}
		if (statusError) {
			setErrorResult("EIM.ERROR.CODE.ISNOT.EDITING", "EIM.ERROR.LOGIC.NOT.UPDATING", obj.getName());
			return result;
		}

		//対象が最新のリビジョンかのチェック
		if (obj.getRevision() != version.getMaxRevision())
		{
			setErrorResult("EIM.ERROR.CODE.ISNOT.LATEST.REVISION","EIM.ERROR.LOGIC.ISNOT.LATEST.REVISION", obj.getName());
			return result;
		}

		// WF無しドキュメントの時、checkoutされていない場合はcheckinできないようにするため
		//以下の2つの場合はエラーとする
		// 1.履歴が0のみの場合はcheckoutされていないためエラー
		// 2.一つ前のリビジョンのオブジェクトにロックユーザがない場合はエラー
		if(obj.getStatus() == null)
		{
			if(version.getMaxRevision() == 0 || version.getObjectByRev(obj.getRevision() - 1).getLockUser() == null)
			{
				setErrorResult("EIM.ERROR.CODE.NOT.ALREADY.CHECKOUT", "EIM.ERROR.LOGIC.NOT.ALREADY.CHECKOUT", obj.getName());
				return result;
			}
		}


		//Check Lock User
		if(obj.getRevision() > 0)
		{
			//Lock Object
			EIMObject lockObj = version.getObjectByRev(obj.getRevision() - 1);
			// オブジェクトがロックされている場合のみ、チェックイン実施ユーザーとの比較を行う
			// チェックアウト無しでチェックインする場合は、過去ドキュメントはロックされていない
			if(lockObj != null && lockObj.getLockUser() != null
					&& lockObj.getLockUser().getId() != processUser.getId())
			{
				setErrorResult("EIM.ERROR.CODE.PROCUSER.NOTEQUAL.CHECKOUTUSER", "EIM.ERROR.LOGIC.PROCUSER.NOTEQUAL.CHECKOUTUSER", procUser);
				return result;
			}
		}

		EIMFormat formatSignEnc = null;
		if (OptionConfData.getInstance().SignAndEncrFlg) {
			long signencr = AppObjectUtil.getIntAttr(sess, obj, EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_STATUS"), AppConstant.SIGNENCR_KIND_NOT_SIGNENCR);
			//署名・暗号化中でないかチェック
			if (signencr == AppConstant.SIGNENCR_KIND_PROCESSING_SIGNENCR)
			{
				setErrorResult("EIM.ERROR.CODE.CANNOT.CHECKIN.WITH.SIGN.AND.ENCR.PROC", "EIM.ERROR.LOGIC.CANNOT.CHECKIN.WITH.SIGN.AND.ENCR.PROC");
				return result;
			}

			formatSignEnc = FileUtils.getFormatByName(sess, EIMConfig.get("FORMAT_NAME_SIGNENCR"));

			//署名・暗号化済
			if (signencr == AppConstant.SIGNENCR_KIND_SIGNENCR)
			{
				if (formatSignEnc == null)
				{
					// 署名・暗号化フォーマットが取得できない
					setErrorResult("EIM.ERROR.CODE.SIGN.AND.ENCR.FORMAT.NOTFOUND", "EIM.ERROR.LOGIC.SIGN.AND.ENCR.FORMAT.NOTFOUND");
					return result;
				}
				else if (FileUtils.getFile(sess, obj, formatSignEnc) == null)
				{
					// 署名・暗号化ファイルが取得できない
					setErrorResult("EIM.ERROR.CODE.NO.SIGN.AND.ENCR.FILE.WITHDOCNAME", "EIM.ERROR.LOGIC.NO.SIGN.AND.ENCR.FILE.WITHDOCNAME", obj.getName());
					return result;
				}
			}
		}

		//Format
		EIMFormat format = FileUtils.getDefaultFormat(sess, obj.getType());

		// 拡張子が変わる場合は変更前のファイルを削除
		EIMFile file = FileUtils.getFile(sess, obj, format);

		//Directory
		EIMDirectory dir = file.getDirectory();

		if (!file.getExt().equals(tmpFileExt)) {
			File target = new File(dir.getPath() + FileUtils.getFileName(obj, file));
			target.delete();
		}

		EIMRelation docRel = null;

		List relList = RelationUtils.getParentRelationListByRelType(sess, obj, RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT")),EIMAccessRole.READ);

		if(!relList.isEmpty()) {
			docRel = (EIMRelation)relList.get(0);
		}

		//Rename
		if(!tmpFileName.equals(obj.getName())){
			try{
				//名前変更する時の重複チェックする際、名前のみでチェックするように変更する
				obj = ObjectUtils.rename(sess, obj, docRel, tmpFileName, EIMConstant.DEPU_CHECK_NAME);
			}catch(EIMException eime){
				// 同じ親を持つ同名のオブジェクトが既に存在します。
				setErrorResult("EIM.ERROR.CODE.FILE.ALREADY.EXIST", "EIM.ERROR.LOGIC.FILE.ALREADY.EXIST", obj.getName());
				return result;
			}
		}

		//Checkin
		FileUtils.checkin(sess, obj, format, tmpFileName, tmpFile.length());
		//取得した実ファイルをファイルサーバ上に配置する（ファイルのコピー）
		File srvFile = new File(format.getDirectory().getPath() + obj.getId() + tmpFileExt);
		FileUtils.copyFile(tmpFile, srvFile);
		tmpFile.delete();

		// 作成したドキュメント自身にステータスが無い場合は、
		// WFなしドキュメントとして、即公開する

		if (obj.getStatus() == null) {

			// 公開ドキュメントとして登録
			EIMFormat publicDocumentFormat = FileUtils.getFormatByName(sess, EIMConfig.get("FORMAT_NAME_PUBLIC"));
			EIMFile pubFile = FileUtils.getFile(sess, obj, FileUtils.getDefaultFormat(sess, obj.getType()));
			File orgFile = new File(pubFile.getDirectory().getPath() + FileUtils.getFileName(obj, pubFile));
			File dstFile = new File(publicDocumentFormat.getDirectory().getPath() + obj.getId() + pubFile.getExt());
			FileUtils.copyFile(orgFile, dstFile);
			FileUtils.checkin(sess, obj, publicDocumentFormat, pubFile.getName(), pubFile.getSize());

			//Delete Relation For Old Revision
			if(obj.getRev() > 0)
			{
				//Set Latest
				obj = VersionUtils.setLatest(sess, obj);

				//Old Revision
				EIMObject latestObj = version.getObjectByRev(obj.getRev() - 1);

				//Unlock (これを実行しないとステータスが「改訂中」のまま残る)
				ObjectUtils.unLock(sess, latestObj);

				//Parent Relation
				List parentRelList = RelationUtils.getParentRelationListByRelType(sess, latestObj, RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT")),EIMAccessRole.READ);

				//Delete Relation
				for(int i = 0; i < parentRelList.size(); i++)
				{
					EIMRelation relation = (EIMRelation)parentRelList.get(i);
					RelationUtils.deleteRelation(sess, relation);
				}

				// SearchFramework 検索FW更新通知 対象：前リビジョンドキュメント
				AppUpdateNoticeUtils.updateNoticeInsert(latestObj.getId(), "SEARCHFW_CHECK_IN_OLD_DOCUMENT");
			}

		}

		//署名暗号化対応
		// 元のオブジェクトから、署名・暗号化に関する属性、ファイルを削除する
		if (OptionConfData.getInstance().SignAndEncrFlg) {
			//フォーマット設定
			formatSignEnc = FileUtils.getFormatByName(sess, EIMConfig.get("FORMAT_NAME_SIGNENCR"));

			AppObjectUtil.setAttr(sess, obj, EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_STATUS"), AppConstant.SIGNENCR_KIND_NOT_SIGNENCR);
			AppObjectUtil.deleteAttribute(sess, obj, EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_VER"));
			obj = ObjectUtils.getObjectById(sess, obj.getId());
			EIMFile signEncFile = FileUtils.getFile(sess, obj, formatSignEnc);
			if (signEncFile != null)
			{
				File substance = new File(signEncFile.getDirectory().getPath() + FileUtils.getFileName(obj, signEncFile));
				if(substance.exists())
				{
					substance.delete();
				}
				FileUtils.deleteFile(sess, obj, formatSignEnc);
			}
		}

		// SearchFramework 検索FW更新通知 対象：対象ドキュメント
		AppUpdateNoticeUtils.updateNoticeInsert(obj.getId(), "SEARCHFW_CHECK_IN_DOCUMENT");

		//Access
		this.createAccessHistory(obj, "EIM.ACCESS.TYPE.EXIF.CHECKIN");

		//Create Operation History
		this.createOperationHistory(EIMCommandConstant.CHECK_IN_BY_USER_EXCOMMAND,
									EIMCommandConstant.TARGET_TO_CREATE,
									obj);
		//正常終了
		setObjectInfo2Result(result, obj);

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
	private void setErrorResult(String code, String resouceKey, String... param) throws Exception{
		if(result == null){
			result = new EIMCommandResultDocument(super.getSess());
		}
		if(param.length == 0){
			result.setTypeCodeMessage(EIMCommandResultConstant.TYPE_ERROR,
					EIMResource.getMessage(code),
					EIMResource.getMessageValue(resouceKey));
		}else if(param.length == 1 && param[0] != null){
				result.setTypeCodeMessage(EIMCommandResultConstant.TYPE_ERROR,
					EIMResource.getMessage(code),
					EIMResource.getMessageValue(resouceKey, new Object[]{StringUtils.xmlEncode(param[0])}));
		}else if(param.length == 2 && param[0] == null && param[1] != null){
				result.setTypeCodeMessage(EIMCommandResultConstant.TYPE_ERROR,
						EIMResource.getMessage(code),
						EIMResource.getMessageValue(resouceKey, new Object[]{StringUtils.xmlEncode(param[1])}));
		}
	}

	//レスポンス用の処理
	private void setObjectInfo2Result(EIMCommandResultDocument resultData, EIMObject object) throws Exception {
		if (resultData.getType() == null)
			resultData.setType(EIMResource.getMessageValue("EIM.RESULT.TYPE.INFO"));
		resultData.setTarget(object);
	}

	//パラメータ解析メソッド
	private boolean checkParam(EIMSession sess, EIMCommandResultDocument result) throws Exception{

		int pathInterFlg = 0;
		int objInterFlg = 0;

		//pathが複数指定されていないか
		if(paths.size() > 1){
			setErrorResult("EIM.ERROR.CODE.OVERLAPPING.SINGLE.PARAMETER", "EIM.ERROR.LOGIC.OVERLAPPING.SINGLE.PARAMETER", EIMCommandConstant.PATH);
			return false;
		}

		if(super.getPath()!= null){
			//空文字列であるかチェック
			if((super.getPath()).length() == 0) {
				setErrorResult("EIM.ERROR.CODE.OJBECT.NO.EXIST", "EIM.ERROR.LOGIC.OJBECT.NO.EXIST");
				return false;
			}
			pathInterFlg++;
		}
		// objIdが複数指定されていないか
		if (objIds.size() > 1) {
			setErrorResult("EIM.ERROR.CODE.OVERLAPPING.SINGLE.PARAMETER","EIM.ERROR.LOGIC.OVERLAPPING.SINGLE.PARAMETER",EIMCommandConstant.OBJID);
			return false;
		}

		if(objIds.size() == 1) objInterFlg++;

		//オブジェクトIDとパスがともに指定されている、もしくは、どちらも指定されていない場合はエラー
		if((pathInterFlg > 0 && objInterFlg > 0)||(pathInterFlg == 0 && objInterFlg == 0)){
			setErrorResult("EIM.ERROR.CODE.INVALID.PATH.OBJID", "EIM.ERROR.LOGIC.INVALID.PATH.OBJID");
			return false;
		}
		if(pathInterFlg == 0 && objInterFlg > 0){
			objId = objIds.get(0);
			//空文字であるかチェック
			if(objId.length() == 0){
				setErrorResult("EIM.ERROR.CODE.OBJID.NOT.NUM", "EIM.ERROR.LOGIC.OBJID.NOT.NUM");
				return false;
			}
			//objIdが半角文字かどうかチェック
			if (!EIMCommandUtil.isOneByteNum(objId)){
				setErrorResult("EIM.ERROR.CODE.OBJID.NOT.NUM", "EIM.ERROR.LOGIC.OBJID.NOT.NUM", objId);
				return false;
			}
		}
		// procUserがあるか
		if (procUsers.size() < 1) {
			setErrorResult("EIM.ERROR.CODE.NO.ESSENTIAL.PARAMETER","EIM.ERROR.LOGIC.NO.ESSENTIAL.PARAMETER",EIMCommandConstant.PROCUSER);
			return false;
		}
		// procUserが複数指定されていないか
		if (procUsers.size() > 1) {
			setErrorResult("EIM.ERROR.CODE.OVERLAPPING.SINGLE.PARAMETER","EIM.ERROR.LOGIC.OVERLAPPING.SINGLE.PARAMETER",EIMCommandConstant.PROCUSER);
			return false;
		}
		if((processUser = UserUtils.getUserByCode(sess, procUsers.get(0))) == null){
			setErrorResult("EIM.ERROR.CODE.PROCUSER.NO.EXIST", "EIM.ERROR.LOGIC.PROCUSER.NO.EXIST", procUsers.get(0));
			return false;
		}
		procUser = procUsers.get(0);

		// fileがあるか
		if (super.getFiles() == null || super.getFiles().length == 0) {
			setErrorResult("EIM.ERROR.CODE.NO.ESSENTIAL.PARAMETER","EIM.ERROR.LOGIC.NO.ESSENTIAL.PARAMETER",EIMCommandConstant.FILE);
			return false;
		}
		// fileが複数指定されていないか
		if (super.getFiles().length > 1) {
			setErrorResult("EIM.ERROR.CODE.OVERLAPPING.SINGLE.PARAMETER","EIM.ERROR.LOGIC.OVERLAPPING.SINGLE.PARAMETER",EIMCommandConstant.FILE);
			return false;
		}
		//ファイル名チェック
		if(super.getNames()[0] == null || super.getNames()[0] == ""){
			setErrorResult("EIM.ERROR.CODE.NOT.GET.FILE.NAME", "EIM.ERROR.LOGIC.NOT.GET.FILE.NAME");
			return false;
		}
		tmpFile = super.getFiles()[0];

		return true;

	}

	/**
	 * アクセス履歴を処理実施ユーザで登録する
	 *
	 */
	@Override
	protected void createAccessHistory(EIMObject object, String actionKey) throws Exception{
		//履歴作成
		AccessUtils.createAccess(getSess(), object, actionKey);
	}

	/**
	 * 操作履歴を処理実施ユーザで登録
	 * @param operationTypeNo 操作種別（の番号）
	 * @param targetInfoNo 操作対象情報（の番号）
	 * @param targetObj 操作対象のEIMObject ※なければnull
	 * @throws Exception
	 */
	@Override
	protected void createOperationHistory(String operationTypeNo, String targetInfoNo, EIMObject targetObj) throws Exception {

		// セッションユーザをログインユーザに戻す
		change2LoginUser();

		OperationHistoryUtils.create(getSess(), EIMCommandConstant.COMMAND, operationTypeNo,
				targetInfoNo, targetObj!=null ? EIMConstant.OBJECT : null, targetObj,
				EIMCommandConstant.PROC_USER, EIMConstant.USER, processUser,
				getPath()!=null ? EIMCommandService.VERSION + ":" + getPath() : EIMCommandService.VERSION);

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
