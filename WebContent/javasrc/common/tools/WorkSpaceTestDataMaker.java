package common.tools;


import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import common.util.AppMessageUtils;
import common.util.AppObjectConditionHelper;
import common.util.AppObjectUtil;
import common.util.DisplayColorUtil;
import eim.bo.EIMAttribute;
import eim.bo.EIMAttributeType;
import eim.bo.EIMException;
import eim.bo.EIMFile;
import eim.bo.EIMFormat;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMRelationType;
import eim.bo.EIMSecurity;
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
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

public class WorkSpaceTestDataMaker {


	static String templateFilePath = null;
	static EIMSecurity sec = null;
	static EIMObjectType workspaceObjType = null;
	static EIMObjectType folderType = null;
	static EIMRelationType documentRelatoinType = null;
	static EIMObjectType documentObjType = null;
	static EIMFormat publicDocumentFormat = null;
	static EIMFormat materialFormat = null;
	static EIMAttributeType propertyType = null;
	static EIMAttributeType careteUserType = null;
	static List<EIMAttributeType> attrTypeList = null;
	/**
	 * 指定された条件で、ワークスペース、フォルダ、ドキュメントを作成する。
	 * args[4]のドキュメントのテンプレートファイルを配置し、そのファイルをコピーし、チェックインする。
	 * ワークスペース名、フォルダ名、ドキュメント名は、オブジェクトIDが付与される。
	 * セキュリティは固定
	 * @param args ワークスペース作成引数
	 * args[0] ワークスペース数<br>
	 * args[1] フォルダ階層の深さ<br>
	 * args[2] 1フォルダあたりのフォルダ数<br>
	 * args[3] 最下層フォルダのドキュメント数<br>
	 * args[4] テンプレートファイルフルパス<br>
	 * args[5] 総ドキュメント数チェック出力のみの場合args[5]をtrueを指定
	 * @throws Exception
	 */
	public static void main(String[] args) {

		Log log = LogFactory.getLog(WorkSpaceTestDataMaker.class);
		log.info(" start WorkSpaceTestDataMaker");
		int createWSCnt;
		int levelCnt;
		int createFolderCnt;
		int createDocCnt;

		if (args.length != 5 && args.length != 6) {
			System.out.println("引数エラー");
			return;
		}

		createWSCnt = Integer.parseInt(args[0]);
		levelCnt = Integer.parseInt(args[1]);
		createFolderCnt = Integer.parseInt(args[2]);
		createDocCnt = Integer.parseInt(args[3]);
		boolean countOutPut = (args.length == 6 && args[5].equalsIgnoreCase("true"));
		templateFilePath = args[4];
		if(!new File(templateFilePath).exists()){
			System.out.println("テンプレートファイルが存在しません。");
			return;
		}
		System.out.println("Input WorkSpace : " + createWSCnt);
		System.out.println("Input Level     : " + levelCnt);
		System.out.println("Input Folder    : " + createFolderCnt);
		System.out.println("Input Document  : " + createDocCnt);
		System.out.println("Input templateFilePath  : " + args[4]);

		// 作成されるドキュメント数
		//int totalCnt = (levelCnt ) * createFolderCnt * createDocCnt) ;


		int leafNodeFoderCount = 0;
		int befNodeFoderCount = createWSCnt;
		for(int i = 0;i < levelCnt ; i++){
			if(i == levelCnt -1){
				leafNodeFoderCount = befNodeFoderCount * createFolderCnt;
			}
			befNodeFoderCount = befNodeFoderCount * createFolderCnt;
		}
		// 作成されるドキュメント数
		int totalCnt =leafNodeFoderCount * createDocCnt;

		int taotalFolderCount = 0;
		for(int i = 1; i < levelCnt +1 ; i++){
			taotalFolderCount += Math.pow(createFolderCnt, i);
		}
		taotalFolderCount = taotalFolderCount * createWSCnt;

		System.out.println("作成ドキュメント数 : " +totalCnt);
		System.out.println("作成フォルダ数 : " +taotalFolderCount);
		if(countOutPut){
			return;
		}
		EIMSession sess = null;
		try
		{
			sess = new EIMSession();
			sess.setConsole();
			EIMThreadContext.putEIMSession(sess);
			// createWorkSpace
			List<String> ids = createWorkSpace(createWSCnt);
			log.info("workSpace make complete");
			// createFolder
			List<String> folderIds = createFolder(levelCnt, createFolderCnt, ids);
			log.info("folder make Complete");
			// createDocument
			List<String> DocumentIds = createDocument(folderIds, createDocCnt);
			log.info("document make Complete");

			System.out.println("=== 正常終了しました。===");


		}
		catch(Exception eime){
			log.warn(AppMessageUtils.makeLogMessage(eime.getMessage()), eime);
			try{
				EIMThreadContext.removeEIMSession();
				if(sess != null){
					sess.rollback();
				}
			}
			catch (Exception se) {
				log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
			}
		}
		finally{
			try{
				if(sess != null)
					sess.close();
			}
			catch(Exception e){
			}
			log.info(" end WorkSpaceTestDataMaker");
		}

	}
	/**
	 * 指定された数のワークスペースを作成する。
	 * １つでも名前が重複して作成できないときは、エラー
	 * @param createWSCnt ワークスペースを作成する数
	 * @return 生成されたワークスペース
	 * @throws Exception
	 */
	private static List<String> createWorkSpace(int createWSCnt)
	throws Exception {
		EIMSession sess = EIMThreadContext.getEIMSession();
		List<String> ret = new ArrayList<String>();
		for (int i = 0; i < createWSCnt; i++) {
			String workSpaceName = "TempWorkSpace_" + String.valueOf(i);

			// user = (EIMUser)sess.getAttribute("USER");

			// セキュリティ
			// system SECURITY
			if(sec == null)
				sec = SecurityUtils.getSecurityByName(sess,
						EIMConfig.getValue("SECURITY_NAME_SYSTEM"));

			// Object Type WorkSpace
			if(workspaceObjType == null){
				workspaceObjType = ObjectUtils.getObjectTypeByName(sess,
						EIMConfig.getValue("OBJECT_TYPE_NAME_WORKSPACE"));
			}
			// Create Object
			EIMObject object = ObjectUtils.createObject(sess, workspaceObjType,
					workSpaceName);

			// ReName Object
			ObjectUtils
			.rename(sess, object, String.valueOf(object.getId()));
			List<EIMObject> objectList = ObjectUtils.getObjectListByType(sess, workspaceObjType);
			int sameNameCount = 0;
			for(EIMObject exsitWorkspace : objectList){
				if(exsitWorkspace.getName().equals(object.getName())){
					sameNameCount += 1;
				}

			}
			if(sameNameCount != 1){
				throw new Exception("ワークスペース名が重複しています。:"+object.getId());
			}
			// Security
			SecurityUtils.setSecurity(sess, object, sec);

			// 下位フォルダ管理セキュリティ設定
			AppObjectUtil
			.setAttr(sess, object, EIMConfig
					.get("ATTR_NAME_WORKSPACE_LOW_FOLDER_SEC"), sec
					.getId());

			ret.add(String.valueOf(object.getId()));

		}


		return ret;
	}



	private static List<String> createFolder(List<String> parentObjId,
			int createFolderCnt) throws Exception {
		EIMSession sess = EIMThreadContext.getEIMSession();
		List<String> ret = new ArrayList<String>();

		// フォルダタイプ(一般フォルダ)
		if(folderType == null)
			folderType = ObjectUtils.getObjectTypeByName(sess,EIMConfig.getValue("OBJECT_TYPE_NAME_FOLDER"));

		// セキュリティ
		// system SECURITY
		if(sec == null)
			sec = SecurityUtils.getSecurityByName(sess, EIMConfig.getValue("SECURITY_NAME_SYSTEM"));

		for (String strParentObjId : parentObjId) {

			for (int i = 0; i < createFolderCnt; i++) {

				String createObjName = "tempFolder_" + strParentObjId;

				// Parent Object
				EIMObject parentObject = ObjectUtils.getObjectById(sess,
						Integer.parseInt(strParentObjId));
				if (parentObject == null) {
					System.out.println("if(parentObject == null){");
					throw new Exception("プログラムロジックエラー:ワークスペースのIDが正常に渡されていません。");
				}

				// Windows禁止文字チェック
				AppObjectUtil.checkValidateFName(sess, createObjName);

				// Path
				String path = AppObjectUtil.getPath(parentObject);
				if (path == null) {
					// ワークスペースの場合、パス属性の値を保持していない
					path = "/";
				}
				path += parentObject.getName() + "/";

				// 指定されたフォルダオブジェクトの作成
				EIMObject object = ObjectUtils.createObject(sess, folderType,
						createObjName);

				// ReName Object
				ObjectUtils.rename(sess, object, String.valueOf(object
						.getId()));

				// Relation Type Document
				if(documentRelatoinType == null)
					documentRelatoinType = RelationUtils
					.getRelationTypeByName(sess, EIMConfig.getValue("RELATION_TYPE_NAME_DOCUMENT"));

				// Create Relation
				RelationUtils.createRelation(sess, documentRelatoinType, parentObject,
						object);

				// Set Path
				AppObjectUtil.setPath(sess, object, path);

				// // Security
				SecurityUtils.setSecurity(sess, object, sec);

				// 属性情報の更新
				// UpdateAttributeHelper.updateAttribute(sess,
				// request,object, isUpdateOnlyName);
				/*
				// Access
				AccessUtils.createAccess(sess, object,
						"EIM.ACCESS.TYPE.INITIALREGIST");
				 */
				ret.add(String.valueOf(object.getId()));

				// Commit
				sess.commit();
			}
		}
		return ret;
	}

	private static List<String> createFolder(int levelCnt, int createFolderCnt,
			List<String> workSpaceIds) throws Exception {
		EIMSession sess = EIMThreadContext.getEIMSession();
		List<String> ret = new ArrayList<String>();

		List<String> parentObjIds = new ArrayList<String>();

		parentObjIds = createFolder(workSpaceIds, createFolderCnt);

		for (int j = 1; j < levelCnt; j++) {

			parentObjIds = createFolder(parentObjIds, createFolderCnt);

		}

		ret = parentObjIds;

		return ret;

	}

	private static List<String> createDocument(List<String> parentObjId,
			int createDocCnt) throws Exception {
		EIMSession sess = EIMThreadContext.getEIMSession();
		List<String> ret = new ArrayList<String>();




		// inputStream
		//DataInputStream in = new DataInputStream(new FileInputStream(path));

		// オブジェクトタイプの取得
		if(documentObjType == null)
			documentObjType = ObjectUtils.getObjectTypeByName(sess,EIMConfig.getValue("OBJECT_TYPE_NAME_DOCUMENT"));

		// セキュリティ
		// system SECURITY
		if(sec == null)
			sec = SecurityUtils.getSecurityByName(sess, EIMConfig.getValue("SECURITY_NAME_SYSTEM"));

		String property = "property";
		String strExpireDate = "";

		for (String strParentObjId : parentObjId) {

			for (int d = 0; d < createDocCnt; d++) {

				// 条件判定ヘルパー作成
				AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

				// 親オブジェクトの取得
				EIMObject parentObj = ObjectUtils.getObjectById(sess,Long.parseLong(strParentObjId));
				if (parentObj == null) {
					throw new EIMException("EIM.ERROR.LOGIC.NOFOLDER");
				}

				// パスの取得
				String path = AppObjectUtil.getPath(parentObj);
				if (path == null) {
					// ワークスペースの場合、パス属性の値を保持していない
					path = "/";
				}
				path += parentObj.getName() + "/";

				// フォーマット、ディレクトリの取得
				if(materialFormat == null)
					materialFormat = FileUtils.getDefaultFormat(sess, documentObjType);

				// ファイル
				File uploadFile = new File(templateFilePath);

				// ファイル名、拡張子の取得
				String fileName = templateFilePath.substring(templateFilePath.lastIndexOf("/") + 1);
				String fileExt = StringUtils.getFileExt(fileName);

				// 拡張子なしファイルはエラー
				String[] nameArray = fileName.split("\\.");
				if (StringUtils.isBlank(fileExt)
						|| fileExt.trim().equals(".")
						|| StringUtils.isBlank(nameArray[0])) {
					// 拡張子のないファイルは作成できない
					throw new EIMException(
					"EIM.WARN.LOGIC.CANT.CREATE.NOEXT");

				}

				// 指定されたドキュメントオブジェクトの作成
				EIMObject object = ObjectUtils.createObject(sess, documentObjType,fileName);

				// ReName Object
				ObjectUtils.rename(sess, object, String.valueOf(object.getId()) + fileExt);

				// 親オブジェクトとのリレーションを作成
				if(documentRelatoinType == null)
					documentRelatoinType = RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));

				RelationUtils.createRelation(sess, documentRelatoinType, parentObj, object);

				// パスを設定
				AppObjectUtil.setPath(sess, object, path);
				// プロパティを設定
				if (propertyType == null) {
					propertyType = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_PROP"));
				}
				ObjectAttributeUtils.setAttribute(sess, object, propertyType, property);

				// 作成者を設定
				if(careteUserType == null)
					careteUserType = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_CREATE"));
				ObjectAttributeUtils.setAttribute(sess, object, careteUserType, sess.getUser().getId());


				// その他の指定属性を設定

				// 上位フォルダからの属性引継ぎ
				long[] parentLowAttrIds = AppObjectUtil.getIntAttrs(sess,
						parentObj, helper.getAttrNameOfToLowAttr());

				if (parentLowAttrIds != null) {
					// 上位からの引継ぎ属性の設定内容に従い、parentObjから属性値をコピー
					// ただし、自身のタイプに該当属性が割り当てられているものに限る
					List parentLowAttrTypes = new ArrayList();
					{
						List parentLowAttrIdsInteger = new ArrayList(Arrays
								.asList(ArrayUtils
										.toObject(parentLowAttrIds)));
						if(attrTypeList == null){
							attrTypeList = ObjectAttributeUtils.getAttributeTypeList(sess, documentObjType);
						}
						// 引継ぎ対象を自身のタイプに該当属性が割り当てられているものにフィルタリング
						SearchToLowAttr: for (Iterator i = parentLowAttrIdsInteger
								.iterator(); i.hasNext();) {
							long attrTypeIdOfParentLowAttrId = ((Long) i
									.next()).longValue();
							for (Iterator j = attrTypeList.iterator(); j
							.hasNext();) {
								EIMAttributeType attrTypeObj = (EIMAttributeType) j
								.next();
								if (attrTypeObj.getId() == attrTypeIdOfParentLowAttrId) {
									parentLowAttrTypes.add(attrTypeObj);
									continue SearchToLowAttr;
								}
							}
							i.remove();// 自身のタイプに無かったので対象から削除
						}
						parentLowAttrIds = ArrayUtils
						.toPrimitive((Long[]) parentLowAttrIdsInteger
								.toArray(new Integer[parentLowAttrIdsInteger
								                     .size()]));
					}
					// 「上位からの引継ぎ」属性の値を設定
					ObjectAttributeUtils.setAttribute(sess, object, helper
							.getAttrTypeOfFromHighAttr(), TypeConvertUtils.convertToBuildTypeArray(parentLowAttrIds));

					// 各属性値の引継ぎ
					for (Iterator i = parentLowAttrTypes.iterator(); i
					.hasNext();) {
						EIMAttributeType attrType = (EIMAttributeType) i
						.next();
						EIMAttribute attr = parentObj.getAttribute(attrType
								.getDefName());
						if (attr != null) { // 上位に属性値がある場合は引継ぎ
							AppObjectUtil.setAttr(sess, object, attr);
						} else { // 上位が属性値を持たない場合は、属性を削除(attTypeName&attValueによる指定で設定されている場合を想定)
							AppObjectUtil.deleteAttribute(sess, object,
									attrType.getDefName());
						}
					}

					// リスト値表示色オブジェクトの引継ぎ
					DisplayColorUtil.inheritDisplayColor(sess, object,
							parentLowAttrTypes, parentObj);
				}

				// 上位フォルダからのステータス引継ぎ
				if (parentObj.getStatus() != null) {
					WorkFlowUtils.updateObjectStatus(sess, object,
							parentObj.getStatus());

					// 「上位WFフォルダ」属性も登録
					EIMAttribute attrOfHigherWFFolder = parentObj
					.getAttribute(helper
							.getAttrNameDocumentHigherWFFolder());
					if (attrOfHigherWFFolder == null) // WF付フォルダ直下
						ObjectAttributeUtils.setAttribute(sess, object,
								helper.getAttrTypeOfHigherWFFolder(),
								parentObj.getId());
					else
						// 「WF付フォルダ下のフォルダ」の下
						AppObjectUtil.setAttr(sess, object,
								attrOfHigherWFFolder);
				}

				// セキュリティを設定
				SecurityUtils.setSecurity(sess, object, sec);
				File srvFile = new File(materialFormat.getDirectory().getPath()
						+ object.getId() + fileExt);

				FileUtils.copyFile(uploadFile, srvFile);


				// チェックイン実行（DBに登録）
				FileUtils.checkin(sess, object, materialFormat, srvFile);// ここの第四引数はテーブルで論理的に保持しているのでどんな値でもよい

				// 取得した実ファイルをファイルサーバ上に配置する（ファイルのコピー）
				// ※ 将来的にSubVersion等から取得するような処理が必要になるかもしれない

				// 作成したドキュメント自身にステータスが無く、かつ上位フォルダにもステータスが無い場合は、
				// WFなしドキュメントとして、即公開する
				if (object.getStatus() == null
						&& parentObj.getStatus() == null) {

					// 公開ドキュメントとして登録
					if(publicDocumentFormat == null)
						publicDocumentFormat = FileUtils.getFormatByName(sess, EIMConfig.get("FORMAT_NAME_PUBLIC"));
					EIMFile file = FileUtils.getFile(sess, object,
							FileUtils.getDefaultFormat(sess, object
									.getType()));
					File orgFile = new File(file.getDirectory().getPath()
							+ FileUtils.getFileName(object, file));
					File dstFile = new File(publicDocumentFormat
							.getDirectory().getPath()
							+ object.getId() + file.getExt());
										FileUtils.copyFile(orgFile, dstFile);
					FileUtils.checkin(sess, object, publicDocumentFormat,
							file.getName(), file.getSize());
				}
				/*
				// Access
				AccessUtils.createAccess(sess, object,
						"EIM.ACCESS.TYPE.INITIALREGIST");
				 */
				/*
				// Create Operation History
				OperationHistoryUtils.create(sess, AppConstant.DOCUMENT,
						AppConstant.CREATE_DOCUMENT_EXCOMMAND,
						EIMConstant.TARGET_CREATE, EIMConstant.OBJECT,
						object, null, null, null, path);
				 */
				ret.add(String.valueOf(object.getId()));
				sess.commit();
			}
		}
		return ret;
	}
}
