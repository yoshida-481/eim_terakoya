package eim.command.business.service.execute;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.lang3.ArrayUtils;

import common.util.AppConstant;
import common.util.AppObjectConditionHelper;
import common.util.AppObjectUtil;
import common.util.AppUpdateNoticeUtils;
import common.util.DisplayColorUtil;
import eim.bo.EIMAccessRole;
import eim.bo.EIMAttribute;
import eim.bo.EIMAttributeType;
import eim.bo.EIMDirectory;
import eim.bo.EIMException;
import eim.bo.EIMFile;
import eim.bo.EIMFormat;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMRelationType;
import eim.bo.EIMResource;
import eim.bo.EIMSecurity;
import eim.command.business.service.result.EIMCommandResult;
import eim.command.business.service.result.EIMCommandResultDocumentList;
import eim.command.common.util.DocumentManagementUtils;
import eim.command.common.util.EIMCommandConstant;
import eim.command.common.util.EIMCommandResultConstant;
import eim.command.common.util.EncryptService;
import eim.net.EIMSession;
import eim.util.AttributeUtils;
import eim.util.EIMConfig;
import eim.util.FileUtils;
import eim.util.ObjectAttributeUtils;
import eim.util.ObjectUtils;
import eim.util.RelationUtils;
import eim.util.SecurityUtils;
import eim.util.StringUtils;
import eim.util.TypeConvertUtils;
import eim.util.WorkFlowUtils;


/**
 * putコマンド用コマンド実行実装クラス
 */
public class EIMCommandPutExecuter extends EIMCommandExecuter {

	private EIMObject parentObject = null;

	// 重複チェックパラメータ
	private List<String> paths = new ArrayList<String>();
	private List<String> types = new ArrayList<String>();
	private List<String> opts = new ArrayList<String>();

    public EIMCommandPutExecuter(){
    }


	/* (非 Javadoc)
	 * @see command.business.service.execute.EIMCommandExecuter#setOtherParameter(java.lang.String, java.lang.String)
	 */
	@Override
	public void setParameter(String key, String value) {
		if(key.equals(EIMCommandConstant.PATH)) {
			paths.add(value);
		} else if(key.equals(EIMCommandConstant.TYPE)) {
			types.add(value);
		} else if(key.equals(EIMCommandConstant.OPTION)) {
			opts.add(value);
		}
		super.setParameter(key, value);
	}

    public EIMCommandResult execute() throws Exception{

    	EIMCommandResultDocumentList result = null;
    	EIMObject object = null;
    	EIMObjectType objType = null;
    	DocumentManagementUtils dmu = new DocumentManagementUtils(getSess());
    	//エラー時に実体ファイルを削除するためのリスト
    	List<File> fileList = new ArrayList<File>();

    	// アップロードしたオブジェクトのリスト
    	List<EIMObject> objList = new ArrayList<EIMObject>();

    	try{
			result = new EIMCommandResultDocumentList(getSess());

    		//パラメータチェック
    		paramCheck(result,getSess());

    		//指定したパスを親オブジェクトとして設定する
    		setParentObject(getPath(), getSess(), result);

        	//入力ファイルのオブジェクトタイプを取得
        	objType = ObjectUtils.getObjectTypeByName(getSess(),getType());
        	if(objType == null){
				//オブジェクトタイプ取得できなかった時
				throw new Exception();
        	}

    		//指定したパスに対して入力されたファイルを作成していく
    		for(int i = 0;i < getFiles().length; i++){

    			//アップロードするファイルの作成
    			File uploadFile = getFiles()[i];

    			//ワークフロー付きフォルダを指定
    			AppObjectConditionHelper helper = new AppObjectConditionHelper(getSess());
    			if (helper.isTypeOfFolderWithWorkflow(parentObject) || helper.isTypeOfFolderUnderFolderWithWorkflow(parentObject)) {
    				//フォルダのワークフローが既に開始されている
    				if (parentObject.getStatus() != null && parentObject.getStatus().getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_EDITTING) {
    					setErrorCodeMessage(result, "EIM.ERROR.CODE.NO.CREATE.AUTH", "EIM.ERROR.LOGIC.NO.CREATE.AUTH", getPath());
    					throw new EIMException();
    				}
    				//ワークフロー付きドキュメント
    				if (WorkFlowUtils.getWorkFlowByType(getSess(), objType) != null) {
    					setErrorCodeMessage(result, "EIM.ERROR.CODE.CANNOT.CREATE.WFFILE.UNDER.WFFOLDER", "EIM.ERROR.LOGIC.CANNOT.CREATE.WFFILE.UNDER.WFFOLDER");
    					throw new EIMException();
    				}
    			}

				//親オブジェクトに既に同名のファイルが存在している時
    			if(dmu.existsInChildrenByName(parentObject, getNames()[i])){
    				setErrorCodeMessage(result, "EIM.ERROR.CODE.FILE.ALREADY.EXIST", "EIM.ERROR.LOGIC.FILE.ALREADY.EXIST", getNames()[i]);
    				throw new EIMException();
    			}

    			object = ObjectUtils.createObject(getSess(), objType, getNames()[i]);
    			if(object == null){
    				//オブジェクトが作成できなかった時
    				throw new Exception();
    			}

    			//作成者の設定
    			EIMAttributeType attTypeCreateUser = AttributeUtils.getAttributeTypeByName(getSess(), EIMConfig.getValue("ATTR_NAME_DOCUMENT_CREATE"));
    			ObjectAttributeUtils.setAttribute(getSess(), object, attTypeCreateUser, super.getSess().getUser().getId());

    			//ドキュメントリレーションの設定
    			EIMRelationType relTypeDocument = RelationUtils.getRelationTypeByName(getSess(), EIMConfig.getValue("OBJECT_TYPE_NAME_DOCUMENT"));
    			RelationUtils.createRelation(getSess(), relTypeDocument, parentObject, object);

    			//作成するオブジェクトのパス属性を設定する
    			String pathOrg = getPath();
    			if(!pathOrg.endsWith("/")){
    				pathOrg += "/";
    			}
    			AppObjectUtil.setPath(getSess(), object, pathOrg);

    			//引き継ぎ属性の処理
    			setHighAttrForChild(object, parentObject, getSess(), objType);

    			//セキュリティを設定する
    			setSecurity(object, parentObject, getSess());

        		//ディレクトリ作成
            	EIMFormat format = FileUtils.getDefaultFormat(getSess(), objType);
            	EIMFile file = FileUtils.getFile(getSess(), object, format);
        		EIMDirectory dir = format.getDirectory();
        		if(dir.getId() == 0){
            		//ディレクトリが存在しない場合
            			setErrorCodeMessage(result, "EIM.ERROR.CODE.PATH.ISNOT.DIRECTORY", "EIM.ERROR.LOGIC.PATH.ISNOT.DIRECTORY");
            			throw new EIMException();
            		}
    			//チェックイン実行（DBに登録）
    			FileUtils.checkin(getSess(), object, format ,getNames()[i], getFiles()[i].length());

    			//取得した実ファイルをファイルサーバ上に配置する（ファイルのコピー）
    			File serverFile = new File(format.getDirectory().getPath() + object.getId() + StringUtils.getFileExt(getNames()[i]));
    			FileUtils.copyFile(uploadFile, serverFile );
    			fileList.add(serverFile);

    			// 作成したドキュメント自身にステータスが無く、かつ上位フォルダにもステータスが無い場合は、
    			// WFなしドキュメントとして、即公開する
    			if (object.getStatus() == null && parentObject.getStatus() == null) {
    				// 公開ドキュメントとして登録
    				EIMFormat publicDocumentFormat = FileUtils.getFormatByName(getSess(), EIMConfig.get("FORMAT_NAME_PUBLIC"));
    				File orgFile = new File(file.getDirectory().getPath() + FileUtils.getFileName(object, file));
    				File dstFile = new File(publicDocumentFormat.getDirectory().getPath() + object.getId() + file.getExt());
    				FileUtils.copyFile(orgFile, dstFile);
    				fileList.add(dstFile);
    				FileUtils.checkin(getSess(), object, publicDocumentFormat, file.getName(), file.getSize());
    			}

    			//オプションが入力されてた場合署名・暗号化処理を行う
    			if (getOption() != null){
					//Verify
					if(!isEncrypted(getSess(), object)) {
						//署名・暗号化の検証が失敗しました
						setErrorCodeMessage(result, EIMCommandResultConstant.CODE_SIGNED_VERIFY_FAILED, EIMCommandResultConstant.MSG_SIGNED_VERIFY_FAILED);
						return result;
    				}
    			}

				// SearchFramework 検索FW更新通知 対象：ドキュメント、親のフォルダor親ワークスペース
				AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), "SEARCHFW_CREATE_DOCUMENT");
				AppUpdateNoticeUtils.updateNoticeInsertParent(getSess(), parentObject,
						"SEARCHFW_CREATE_DOCUMENT_PARENT_FOLDER", "SEARCHFW_CREATE_DOCUMENT_PARENT_WORKSPACE", null);

				// 操作履歴
				this.createOperationHistory(EIMCommandConstant.PUT_EXCOMMAND, EIMCommandConstant.TARGET_TO_CREATE, object);
				// アクセス履歴
				this.createAccessHistory(object, "EIM.ACCESS.TYPE.EXIF.INITIALREGIST");

				// EIMObjectの取り直し、リストにセットする(設定した属性などを適切に取得するために再度取得が必要)
				objList.add(ObjectUtils.getObjectById(super.getSess(), object.getId()));
    		}

    		//処理したEIMObjectを配列に設定する
    		result.setType(EIMResource.getMessageValue("EIM.RESULT.TYPE.INFO"));
    		result.setDispAttr(true);
    		result.setResultDocListByObjList(objList, false);

    	}catch (EIMException e){
			//実体ファイルの削除
			cleanRealFile(fileList);
			getSess().rollback();
//    		e.printStackTrace();//------------------------------------------------------------------------------------------------------------------------------------
    		//定義されたエラー群
    		return result;
    	}catch (Exception e){
			//実体ファイルの削除
			cleanRealFile(fileList);
			getSess().rollback();
//    		e.printStackTrace();//------------------------------------------------------------------------------------------------------------------------------------
    		//システムエラー
    		try{
    		setErrorCodeMessage(result, "EIM.ERROR.CODE.SYSTEM.ERROR","EIM.ERROR.LOGIC.SYSTEM.ERROR");
    		}catch(Exception ee){}
    		throw e;
		} finally {
			// 中間ファイルを削除する処理
			cleanTmpFile();
		}
		return result;
	}

    /**
     * エラー時の中間ファイルを削除する
     */
    private void cleanTmpFile(){
    	if (getFiles() != null && getFiles().length > 0) {
    		for (int i = 0; i < getFiles().length; i++) {
    			getFiles()[i].delete();
    		}
    	}
    }

    /**
     * エラー発生時に、作成されてしまった実体ファイルを削除する
     */
    private void cleanRealFile(List<File> files){
    	for(int i = 0 ; i < files.size() ; i++){
    		files.get(i).delete();
    	}
    }


    /**
     * EIMObjectに対して署名・暗号化の検証を行う
	 * @param EIMSession sess, EIMObject object
	 * @return boolean
	 * @throws Exception
     */
	private boolean isEncrypted(EIMSession sess, EIMObject object) throws Exception {

		//Format
		EIMFormat format = FileUtils.getDefaultFormat(sess, object.getType());

		//File
		EIMFile file = FileUtils.getFile(sess, object, format);

		//Substance
		File substance = new File(file.getDirectory().getPath() + FileUtils.getFileName(object, file));

		//Encrypt Service
		EncryptService es = new EncryptService(sess);

		//Verify
		int rscode = es.verify(substance);
		if(rscode != 0) {
			return false;
		}

		//Set Signed Attribute
		EIMAttributeType attType = null;

		//Sign Status
		attType = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_STATUS"));
		ObjectAttributeUtils.setAttribute(sess, object, attType, AppConstant.SIGNENCR_KIND_SIGNENCR);

		//Sign Version
		String version = es.version();
		attType = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_VER"));
		ObjectAttributeUtils.setAttribute(sess, object, attType, version);

		//Format Signed
		format = FileUtils.getFormatByName(sess, EIMConfig.get("FORMAT_NAME_SIGNENCR"));

		//Checkin
		FileUtils.checkin(sess, object, format, file.getName(), substance.length());

		//Copy Substance for Signed Format
		FileUtils.copyFile(substance, new File(format.getDirectory().getPath() + substance.getName()));

		return true;

	}

    /**
     * 上位からの引き継ぎ属性を子からピックアップする
	 * @param EIMObject childObj parentObj, EIMSession sess,EIMObjectType objType
	 * @throws Exception
     */
	@SuppressWarnings("unchecked")
    private void setHighAttrForChild(EIMObject childObj, EIMObject parentObj, EIMSession sess, EIMObjectType objType) throws Exception{

		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

		try {
			long[] parentLowAttrIds = AppObjectUtil.getIntAttrs(sess, parentObj ,helper.getAttrNameOfToLowAttr());
			if (parentLowAttrIds != null) {
				// 上位からの引継ぎ属性の設定内容に従い、parentObjから属性値をコピー
				// ただし、自身のタイプに該当属性が割り当てられているものに限る
				List<Object> parentLowAttrTypes = new ArrayList<Object>();
				{
					List<Long> parentLowAttrIdsInteger = new ArrayList<Long>(Arrays.asList(ArrayUtils.toObject(parentLowAttrIds)));
					List<EIMAttributeType> objectTypes = ObjectAttributeUtils.getAttributeTypeList(sess, objType);
					// 引継ぎ対象を自身のタイプに該当属性が割り当てられているものにフィルタリング
					SearchToLowAttr: for (Iterator<Long> i = parentLowAttrIdsInteger.iterator(); i.hasNext();) {long attrTypeIdOfParentLowAttrId = ((Long) i.next()).longValue();
						for (Iterator<EIMAttributeType> j = objectTypes.iterator(); j.hasNext();) {
							EIMAttributeType attrTypeObj = (EIMAttributeType) j.next();
							if (attrTypeObj.getId() == attrTypeIdOfParentLowAttrId) {
								parentLowAttrTypes.add(attrTypeObj);
								continue SearchToLowAttr;
							}
						}
						i.remove();// 自身のタイプに無かったので対象から削除
					}
					parentLowAttrIds = ArrayUtils.toPrimitive((Long[]) parentLowAttrIdsInteger.toArray(new Long[parentLowAttrIdsInteger.size()]));
				}
				// 「上位からの引継ぎ」属性の値を設定
				ObjectAttributeUtils.setAttribute(sess, childObj, helper.getAttrTypeOfFromHighAttr(), TypeConvertUtils.convertToBuildTypeArray(parentLowAttrIds));
				//各属性値の引継ぎ
				for (Iterator<Object> i = parentLowAttrTypes.iterator(); i.hasNext();) {
					EIMAttribute attr = parentObj.getAttribute(((EIMAttributeType)i.next()).getDefName());
					if (attr != null) {
						AppObjectUtil.setAttr(sess, childObj, attr);
					}
				}
				//リスト値表示色オブジェクトの引継ぎ
				DisplayColorUtil.inheritDisplayColor(sess, childObj, parentLowAttrTypes, parentObj);
			}
			//上位フォルダからのステータス引継ぎ
			if (parentObj.getStatus() != null) {
				WorkFlowUtils.updateObjectStatus(sess, childObj, parentObj.getStatus());
				//「上位WFフォルダ」属性も登録
				EIMAttribute attrOfHigherWFFolder = parentObj.getAttribute(helper.getAttrNameDocumentHigherWFFolder());
				if (attrOfHigherWFFolder == null) //WF付フォルダ直下
					ObjectAttributeUtils.setAttribute(sess, childObj, helper.getAttrTypeOfHigherWFFolder(), parentObj.getId());
				else //「WF付フォルダ下のフォルダ」の下
					AppObjectUtil.setAttr(sess, childObj, attrOfHigherWFFolder);
			}
		} catch (Exception e) {
			throw e;
		}
	}

    /**
     * セキュリティの設定
	 * @param EIMObject childObj parentObj, EIMSession sess,EIMObjectType objType
	 * @throws Exception
     */
    private void setSecurity(EIMObject childObj, EIMObject parentObj, EIMSession sess) throws Exception{
		//セキュリティを設定
		EIMSecurity sec = parentObj.getSecurity();
		if(sec != null)
		{
			SecurityUtils.setSecurity(sess, childObj, sec);
		}
    }

    /**
     * 親オブジェクトをセットする
	 * @param String path,EIMSession sess
	 * @throws Exception
     */
    private void setParentObject(String path, EIMSession sess, EIMCommandResult result) throws Exception, EIMException{
		DocumentManagementUtils dmu = new DocumentManagementUtils(sess);
			//pathからディレクトリとファイル名を取得する
			//入力パスの最後にスラッシュが入力されていたらスラッシュを削除する
			if(path.length() > 1 && path.endsWith("/")) {
				path = path.substring(0, path.length() - 1);
			}
			//親ディレクトを取得するためのディレクトリとパスを設定する
			String directory = "";
			String name = "";
			if(path.lastIndexOf("/") > 0) {
				directory = path.substring(0, path.lastIndexOf("/") + 1);
				name = path.substring(path.lastIndexOf("/") + 1);
			} else {
				directory = "/";
				name = path.substring(1, path.length());
			}

			// 指定したパス(親オブジェクト)が存在しなかった時はエラー
			parentObject = dmu.getObjectByPathAndName(directory, name);
			if (parentObject == null){
				setErrorCodeMessage(result, "EIM.ERROR.CODE.OJBECT.NO.EXIST","EIM.ERROR.LOGIC.OJBECT.NO.EXIST");
				throw new EIMException();

			}

    		//指定したパスがディレクトリであるかチェックする
    		if( !dmu.isFolderTypes(parentObject.getType()) ){
    			setErrorCodeMessage(result, "EIM.ERROR.CODE.PATH.ISNOT.DIRECTORY","EIM.ERROR.LOGIC.PATH.ISNOT.DIRECTORY");
				throw new EIMException();
    		}

			//指定したディレクトに更新権限があるかチェックする
			if(parentObject.getSecurity() != null) {
				if(!SecurityUtils.enabled(sess, parentObject,sess.getUser(), EIMAccessRole.CREATE)) {
					setErrorCodeMessage(result, "EIM.ERROR.CODE.NO.UPDATE.AUTH","EIM.ERROR.LOGIC.NO.UPDATE.AUTH");
					throw new EIMException();
				}
			}
			// 親オブジェクトがゴミ箱に入ってるかチェック
			if(AppObjectUtil.isObjectInRecycle(sess, parentObject)) {
				throw new Exception();
			}
    }

    /**
     * パラメータ解析
	 * @param EIMCommandResultDocument
	 * @return boolean
	 * @throws EIMException,Exception
     */
    private void paramCheck(EIMCommandResultDocumentList result, EIMSession sess) throws EIMException, Exception{

    	try{

    	//必須パラメータが入力されているか
    	//パラメータ「path」
		if (getPath() == null || getPath() == "") {
			setErrorCodeMessage(result, "EIM.ERROR.CODE.NO.ESSENTIAL.PARAMETER", "EIM.ERROR.LOGIC.NO.ESSENTIAL.PARAMETER", EIMCommandConstant.PATH);
			throw new EIMException();
		}else if (getFileCounter() < 1){
			// パラメータ「files」に指定されたファイル数
			setErrorCodeMessage(result, "EIM.ERROR.CODE.NO.ESSENTIAL.PARAMETER", "EIM.ERROR.LOGIC.NO.ESSENTIAL.PARAMETER", EIMCommandConstant.UPLOAD_FILES);
			throw new EIMException();
		}else if(getNames().length < 1){
			// ファイル名称
			setErrorCodeMessage(result, "EIM.ERROR.CODE.NOT.GET.FILE.NAME", "EIM.ERROR.LOGIC.NOT.GET.FILE.NAME");
			throw new EIMException();
		}

		// 単一パラメータの重複チェック
		if(!isNotDuplicated(result)){
			throw new EIMException();
		}

		//ファイルチェック
		for (int i = 0;i<getNames().length ;i++){
				//ファイルの名称チェック(空文字列の場合)
				if( getNames()[i] == null || getNames()[i].equals("")){
					setErrorCodeMessage(result, "EIM.ERROR.CODE.NOT.GET.FILE.NAME", "EIM.ERROR.LOGIC.NOT.GET.FILE.NAME");
					throw new EIMException();
				}
				// 拡張子がありません
				if (StringUtils.getFileExt(getNames()[i]) == null || StringUtils.getFileExt(getNames()[i]).equals("")){
					setErrorCodeMessage(result, "EIM.ERROR.CODE.NO.FILE.EXTENT", "EIM.ERROR.LOGIC.NO.FILE.EXTENT", getNames()[i]);
					throw new EIMException();
				}
				try{
					// Windows禁止文字チェック
					AppObjectUtil.checkValidateFName(sess, getNames()[i]);
				}catch(EIMException e){
					setErrorCodeMessage(result, "EIM.ERROR.CODE.STRING.UNUSABLE", "EIM.ERROR.LOGIC.STRING.UNUSABLE", getNames()[i]);
					throw e;
				}

				//tmpファイルが存在しない場合、システムエラー
				if(!getFiles()[i].exists()){
					setErrorCodeMessage(result, "EIM.ERROR.CODE.SYSTEM.ERROR", "EIM.ERROR.LOGIC.SYSTEM.ERROR");
					throw new Exception();
				}

				//実体ファイルのサイズがゼロかどうか
				if(getFiles()[i].length() == 0){
					setErrorCodeMessage(result, "EIM.ERROR.CODE.FILE.NO.EXIST.OR.SIZEZERO", "EIM.ERROR.LOGIC.FILE.NO.EXIST.OR.SIZEZERO", getNames()[i]);
					throw new EIMException();
				}
		}

		//オブジェクトタイプが指定されていない場合デフォルトでオブジェクトタイプ「ドキュメント」を設定する
		if (getType() == null || getType().equals("")){
			setType(EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT"));
		}else if(!isObjTypeDocument(getType())){
			//文書タイプが入力されていて、その文書タイプがない場合
			setErrorCodeMessage(result, "EIM.ERROR.CODE.DOCUMENT.TYPE.NO.EXIST", "EIM.ERROR.LOGIC.DOCUMENT.TYPE.NO.EXIST");
			throw new EIMException();
		}

		// オプションが入力されていて、それがsignedでない場合
		if (getOption() != null && !getOption().equals("signed")) {
			setErrorCodeMessage(result, "EIM.ERROR.CODE.INVALID.OPTION", "EIM.ERROR.LOGIC.INVALID.OPTION");
			throw new EIMException();
		}

    	}catch(EIMException e){
    		throw e;
    	}catch(Exception e){
    		throw e;
    	}
    }

	/**
	 * エラー情報を設定
	 * @param resultData
	 * @param codeKey
	 * @param messageKey
	 * @throws EIMException
	 */
	private void setErrorCodeMessage(EIMCommandResult resultData, String codeKey, String messageKey) throws EIMException{
		resultData.setTypeCodeMessage(EIMResource.getMessageValue("EIM.RESULT.TYPE.ERROR"), EIMResource.getMessageValue(codeKey), EIMResource.getMessageValue(messageKey));
	}

	/**
	 * エラー情報を設定
	 * @param resultData
	 * @param codeKey
	 * @param messageKey
	 * @param replacement メッセージ中の{0}を指定の文字列に置換する
	 * @throws EIMException
	 */
	private void setErrorCodeMessage(EIMCommandResult resultData, String codeKey, String messageKey, String replacement) throws EIMException{
		resultData.setTypeCodeMessage(EIMResource.getMessageValue("EIM.RESULT.TYPE.ERROR"), EIMResource.getMessageValue(codeKey), (EIMResource.getMessageValue(messageKey)).replace("{0}", replacement));
	}

	/**
	 * 指定された文章タイプが存在するかチェックします。
	 * @return true:存在する、false:存在しない
	 * @throws Exception
	 */
	private boolean isObjTypeDocument(String type) throws Exception
	{
		boolean result = false;

		// ただの「ドキュメント」の場合はエラー
		if(type.equals(EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT")))
		{
			result = false;
		}
		// 「一般ドキュメント」と合致する
		else if(type.equals(EIMResource.getMessage(super.getSess(), "EIM.OBJECTTYPE.GENERAL")))
		{
			result = true;
			// オブジェクトタイプ名称「ドキュメント」に変更
			setType(EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT"));
		}
		else
		{
			// ドキュメントタイプ「オブジェクト」のリストを取得
			List<EIMObjectType> objTypeList = new ArrayList<EIMObjectType>();
			EIMObjectType docType = ObjectUtils.getObjectTypeByName(super.getSess(), EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT"));
			ObjectUtils.getChildObjectTypeListRecurrently(super.getSess(), docType, objTypeList);
			objTypeList.add(docType);

			for(EIMObjectType objType : objTypeList)
			{
				// 取得したオブジェクトタイプの名称と合致する
				if(type.equals(objType.getName()))
				{
					result = true;
					break;
				}
			}
		}

		return result;
	}

	/**
	 * 単一パラメータの重複をチェックする
	 * エラーの場合はエラーコード、エラーメッセージをセットする
	 * @return true:重複なし、false:重複あり
	 * @throws Exception
	 */
	private boolean isNotDuplicated(EIMCommandResultDocumentList result) throws Exception
	{
		// path
		if(paths.size() > 1)
		{
			setErrorCodeMessage(result, "EIM.ERROR.CODE.OVERLAPPING.SINGLE.PARAMETER", "EIM.ERROR.LOGIC.OVERLAPPING.SINGLE.PARAMETER", EIMCommandConstant.PATH);
			return false;
		}

		// type
		if(types.size() > 1)
		{
			setErrorCodeMessage(result, "EIM.ERROR.CODE.OVERLAPPING.SINGLE.PARAMETER", "EIM.ERROR.LOGIC.OVERLAPPING.SINGLE.PARAMETER", EIMCommandConstant.TYPE);
			return false;
		}

		// opt
		if(opts.size() > 1)
		{
			setErrorCodeMessage(result, "EIM.ERROR.CODE.OVERLAPPING.SINGLE.PARAMETER", "EIM.ERROR.LOGIC.OVERLAPPING.SINGLE.PARAMETER", EIMCommandConstant.OPTION);
			return false;
		}

		return true;
	}

}
