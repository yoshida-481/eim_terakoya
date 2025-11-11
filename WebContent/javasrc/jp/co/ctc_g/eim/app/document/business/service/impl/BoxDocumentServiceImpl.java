package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.io.File;
import java.io.OutputStream;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import jakarta.servlet.http.HttpServletRequest;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.util.Assert;

import common.util.AppConstant;
import common.util.AppMessageUtils;
import common.util.AppObjectUtil;
import common.util.AppUpdateNoticeUtils;
import common.util.BoxDocumentUploadManager;
import common.util.DocumentFromBoxUploadData;
import common.util.DocumentFromBoxUploadManager;
import common.util.FileUtil;
import eim.bo.EIMAccessRole;
import eim.bo.EIMException;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMResource;
import eim.bo.EIMUser;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.EIMUtils;
import eim.util.ObjectUtils;
import eim.util.SecurityUtils;
import jp.co.ctc_g.eim.app.document.business.service.BoxDocumentService;
import jp.co.ctc_g.eim.app.document.presentation.dto.BoxDocumentCreateDTO;
import jp.co.ctc_g.eim.app.document.presentation.dto.ConfirmBoxDocumentDTO;
import jp.co.ctc_g.eim.framework2.business.dao.FileDao;
import jp.co.ctc_g.eim.framework2.business.dao.box.BoxFileDao;
import jp.co.ctc_g.eim.framework2.business.dao.box.BoxFolderDao;
import jp.co.ctc_g.eim.framework2.business.domain.box.BoxFileDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.OperationHistoryDomain;
import jp.co.ctc_g.eim.framework2.business.file.FileAccessor;
import jp.co.ctc_g.eim.framework2.business.service.FormatService;
import jp.co.ctc_g.eim.framework2.business.service.ObjectService;
import jp.co.ctc_g.eim.framework2.business.service.ObjectTypeService;
import jp.co.ctc_g.eim.framework2.business.service.OperationHistoryService;
import jp.co.ctc_g.eim.framework2.business.service.box.BoxArchiveInfoService;
import jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader;
import jp.co.ctc_g.eim.framework2.common.exception.EIMApplicationException;

/**
 * BoxファイルService実装クラス
 */
public class BoxDocumentServiceImpl implements BoxDocumentService, InitializingBean {

	/** ロガー */
	private static Log log = LogFactory.getLog(BoxDocumentServiceImpl.class);

	/** BoxファイルDao */
	private BoxFileDao boxFileDao;
	/** BoxフォルダDao */
	private BoxFolderDao boxFolderDao;
	/** オブジェクトサービス */
	private ObjectService objectService;
	/** オブジェクトタイプサービス */
	private ObjectTypeService objectTypeService;
	/** フォーマットサービス */
	private FormatService formatService;
	/** ファイルアクセサー */
	private FileAccessor fileAccessor;
	/** ファイルDAO */
	private FileDao fileDao;
	/** Box保管情報サービス */
	private BoxArchiveInfoService boxArchiveInfoService;

	/**
	 * Webアプリケーションコンテキスト起動時に実行されます。 プロパティのNullチェックをします。
	 */
	@Override
	public void afterPropertiesSet() throws Exception {
		Assert.notNull(boxFileDao, "boxFileDao must be specified.");
		Assert.notNull(objectService, "objectService must be specified.");
		Assert.notNull(objectTypeService, "objectTypeService must be specified.");
		Assert.notNull(formatService, "formatService must be specified.");
		Assert.notNull(fileAccessor, "fileAccessor must be specified.");
		Assert.notNull(fileDao, "fileDao must be specified.");
		Assert.notNull(boxArchiveInfoService, "boxArchiveInfoService must be specified.");
	}

	/**
	 * @see jp.co.ctc_g.eim.framework2.business.service.box.BoxFileService#getById(String)
	 */
	@Override
	public BoxFileDomain getById(String id) throws Exception {
		return boxFileDao.getById(id);
	}

	/**
	 * 一時格納オブジェクトを作成します。
	 *
	 * @param filesInfo
	 *            Boxファイル情報
	 * @param resquest
	 *            HTTPサーブレットリクエスト
	 * @return オブジェクトID
	 * @throws Exception
	 */
	public Long createObject(List<Map<String, Object>> files, HttpServletRequest request) throws Exception {

		Long objectId = null;

		// クライアントから受け渡された作成者IDを取得
		Map<String, Object> file = files.get(0);
		String prmCreateUserId = String.valueOf(file.get("createUserId"));

		/*
		 * 一時格納フォルダオブジェクトの作成する。
		 */

		Date now = new Date();
		SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMddHHmmss");
		String objName = "boxFileUL_" + prmCreateUserId + "_" + sdf.format(now);

		// 一時格納フォルダオブジェクトタイプの取得
		ObjectTypeDomain tmpObjType = new ObjectTypeDomain(EIMConfig.get("OBJECT_TYPE_NAME_TMP_STORE"));
		ObjectDomain object = new ObjectDomain();
		object.setName(objName);
		object.setType(tmpObjType);

		object = objectService.create(object);
		objectId = object.getId();

		return objectId;

	}

	/**
	 * BoxからEIM上へドキュメントの登録可否確認します。
	 *
	 * @param filesInfo
	 *            Boxファイル情報
	 * @param resquest
	 *            HTTPサーブレットリクエスト
	 * @return ファイル登録可否結果
	 * @throws Exception
	 */
	public List<ConfirmBoxDocumentDTO> confirmBoxDocument(Map<String, Object> filesInfo, HttpServletRequest request)
			throws Exception {

		List<ConfirmBoxDocumentDTO> results = this.uploadConfirm(filesInfo, request);

		return results;

	}

	/**
	 * BoxからEIM上へドキュメントを登録します。
	 *
	 * @param filesInfo
	 *            Boxファイル情報
	 * @param resquest
	 *            HTTPサーブレットリクエスト
	 * @return ファイル登録可否結果
	 * @throws Exception
	 */
	public List<BoxDocumentCreateDTO> createDocument(Map<String, Object> fileInfo, HttpServletRequest request)
			throws Exception {

		List<BoxDocumentCreateDTO> results = create(fileInfo, request);

		return results;

	}

	/**
	 * 一時格納フォルダを削除します。
	 * @param resquest HTTPサーブレットリクエスト
	 * @throws Exception
	 * @since Ver6.44
	 */
	public void deleteTmpFolder(HttpServletRequest request) {

		String tmpObjId = request.getAttribute("tmpObjId").toString();

		try {

			/*
			 * 一時格納フォルダを削除する
			 */
			ObjectDomain o = objectService.getById(Long.parseLong(tmpObjId));
			if (o != null) {
				File tmpStoreFolder = new File(EIMConfig.get("TEMP") + "/" + o.getId());
				if (tmpStoreFolder.exists() && FileUtil.clean(tmpStoreFolder)) {
					objectService.delete(o);
				}
			}
		} catch (Exception e) {

			log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);

		}
	}

	/**
	 * ドキュメントの登録可否確認します。
	 *
	 * @param filesInfo
	 *            Boxファイル情報
	 * @param resquest
	 *            HTTPサーブレットリクエスト
	 * @return ファイル登録可否結果
	 * @throws Exception
	 */
	private List<ConfirmBoxDocumentDTO> uploadConfirm(Map<String, Object> file, HttpServletRequest request) throws Exception  {

		EIMSession sess = null;
		EIMUser loginUser = null;

		Object[] paramId = null;

		String message = "";

		List<ConfirmBoxDocumentDTO> results = new ArrayList<>();

		try {
			// //セッション情報の取得
			sess = EIMUtils.getSession(request);
			if (sess == null) {
				throw new EIMException("EIM.ERROR.SESSIONTIMEOUT");
			}
			loginUser = (EIMUser) sess.getAttribute("USER");

			// クライアントから受け渡されたパラメタの取得
			String prmObjId = String.valueOf(file.get("objId"));
			String prmObjTypeId = String.valueOf(file.get("documentTypeId"));
			String prmCreateUserId = String.valueOf(file.get("createUserId"));
			String prmFolderName = String.valueOf(file.get("tmpObjId"));

			paramId = new Object[] { "objId=" + prmObjId, "objTypeId=" + prmObjTypeId,
					"createUserId=" + prmCreateUserId, };

			// ドキュメントタイプの取得
			EIMObjectType documentObjType = getDocumentObjType(sess, prmObjTypeId);
			if (documentObjType == null) {
				throw new EIMException("EIM.ERROR.LOGIC.NODOCUMENTTYPE");
			}

			// 親オブジェクトの取得
			EIMObject parentObj = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
			if (parentObj == null) {
				throw new EIMException("EIM.ERROR.LOGIC.NOFOLDER");
			}

			// 親オブジェクトがゴミ箱の下に移動していないかのチェック
			if (AppObjectUtil.isObjectInRecycle(sess, parentObj)) {
				throw new EIMException("EIM.ERROR.LOGIC.SELECTFOLDERDELETED");
			}

			// 作成権限のチェック
			if (!SecurityUtils.authorized(sess, parentObj, sess.getUser(), EIMAccessRole.CREATE)) {
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOCREATEROLE");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));

				sess.close();
				throw new EIMException("EIM.ERROR.LOGIC.NOCREATEROLE");

			}
			// ワークフロー付きフォルダ下の場合は、編集中以外は作成できない
			if (parentObj.getStatus() != null) {
				if (parentObj.getStatus().getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_EDITTING) {
					sess.close();
					throw new EIMException("EIM.ERROR.LOGIC.NOCREATEROLE");

				}
			}

			// パスの取得
			String path = AppObjectUtil.getPath(parentObj);
			if (path == null) {
				// ワークスペースの場合、パス属性の値を保持していない
				path = "/";
			}
			path += parentObj.getName() + "/";

			// 一時格納フォルダオブジェクトタイプの取得
			EIMObjectType tmpFolderObjType = ObjectUtils.getObjectTypeByName(sess,
					EIMConfig.get("OBJECT_TYPE_NAME_TMP_STORE"));
			if (tmpFolderObjType == null) {
				throw new EIMException("EIM.ERROR.LOGIC.NOTMPSTOREFOLDERTYPE");
			}

			Date now = new Date();

			/*
			 * DBから一時格納フォルダ情報（オブジェクト）を取得する。その中で「作成からクライアントのタイムアウト
			 * 時間以上が経過しているフォルダ」 は、今後使用されないことが確定しているため削除し、DBから一時
			 * 格納フォルダ情報（オブジェクト）も削除する。
			 * 一時格納フォルダが存在しない場合も、同様にDBから一時格納フォルダ情報（オブジェクト）を削除する。
			 */
			List tmpFolderList = ObjectUtils.getObjectListByType(sess, tmpFolderObjType, EIMAccessRole.READ);
			long timeoutMillisecond = request.getSession().getMaxInactiveInterval() * 1000;
			for (int ii = 0; ii < tmpFolderList.size(); ii++) {
				EIMObject o = (EIMObject) tmpFolderList.get(ii);
				Date createDate = o.getCreateDate();
				if (createDate.getTime() + timeoutMillisecond < now.getTime()) {
					File deleteFolder = new File(EIMConfig.get("TEMP") + "/" + o.getId());
					if (deleteFolder.exists() == false || FileUtil.clean(deleteFolder)) {
						ObjectUtils.deleteObject(sess, o);
					}
				}
			}

			/*
			 * 一時格納フォルダをウォーキングして、登録可否のチェックを行う。
			 */
			File tmpFolder = new File(EIMConfig.get("TEMP") + prmFolderName + "/");
			String tmpFolderPath = EIMConfig.get("TEMP") + prmFolderName + "/";
			DocumentFromBoxUploadManager manager = new DocumentFromBoxUploadManager(sess, documentObjType,
					tmpFolderPath, path);
			manager.createParentObjectMap(tmpFolder, prmFolderName);
			ArrayList retList = manager.recursiveCheckAssign(tmpFolder, true);
			retList = setLowerObjectCannotUpload(retList);
			retList = setUppderObjectConfirmUpload(retList);

			// レスポンスを作成
			for (int ii = 0; ii < retList.size(); ii++) {
				DocumentFromBoxUploadData data = (DocumentFromBoxUploadData) retList.get(ii);
				results.add(data.createCheckResult());
			}

			// Commit
			sess.commit();

		} catch (Exception e) {
			throw e;
		}

		return results;

	}

	/**
	 * BoxからEIM上へドキュメントを登録します。
	 *
	 * @param filesInfo
	 *            Boxファイル情報
	 * @param resquest
	 *            HTTPサーブレットリクエスト
	 * @return ファイル登録可否結果
	 * @throws Exception
	 */
	private List<BoxDocumentCreateDTO> create(Map<String, Object> fileInfo, HttpServletRequest request)
			throws Exception {

		EIMSession sess = null;

		 //セッション情報の取得
		sess = EIMUtils.getSession(request);
		if (sess == null) {
			throw new EIMException("EIM.ERROR.SESSIONTIMEOUT");
		}

		// クライアントから受け渡されたパラメタの取得
		String prmObjId = String.valueOf(fileInfo.get("objId"));
		String prmObjTypeId = String.valueOf(fileInfo.get("documentTypeId"));
		String prmCreateUserId = String.valueOf(fileInfo.get("createUserId"));
		String prmBoxPath = (String) fileInfo.get("boxPath");
		String prmObjNum = String.valueOf(fileInfo.get("objNum"));
		String tmpObjId = String.valueOf(fileInfo.get("tmpObjId"));

		// ドキュメントタイプの取得
		ObjectTypeDomain documentObjType = getDocumentObjType(prmObjTypeId);
		if (documentObjType == null) {
			throw new Exception("EIM.ERROR.LOGIC.NODOCUMENTTYPE");
		}

		/*
		 * ドキュメントの登録を行う
		 */
		List<BoxDocumentCreateDTO> createObjectList = new ArrayList<BoxDocumentCreateDTO>();
		int objNum = Integer.parseInt(prmObjNum);
		if (objNum > 0) {
			String[] nameArray = new String[objNum];
			String[] pathArray = new String[objNum];
			String[] typeArray = new String[objNum];
			Object[] attributeDomainArray = new Object[objNum];
			for (int ii = 0; ii < objNum; ii++) {
				nameArray[ii] = (String) fileInfo.get("objName_" + String.valueOf(ii));
				pathArray[ii] = (String) fileInfo.get("path_" + String.valueOf(ii));
				typeArray[ii] = (String) fileInfo.get("objType_" + String.valueOf(ii));
				attributeDomainArray[ii] = fileInfo.get("attributeList_" + String.valueOf(ii));

			}

			BoxDocumentUploadManager manager = new BoxDocumentUploadManager(prmObjTypeId,tmpObjId, prmCreateUserId, prmBoxPath);
			createObjectList = manager.createObject(nameArray, pathArray, typeArray, attributeDomainArray);

			// SearchFramework 検索FW更新通知 対象：オブジェクトの親
			if (AppUpdateNoticeUtils.doEntry()) {
				EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
				AppUpdateNoticeUtils.updateNoticeInsertParent(sess, object, "SEARCHFW_CREATE_DOCUMENT_PARENT_FOLDER",
						"SEARCHFW_CREATE_DOCUMENT_PARENT_WORKSPACE", null);
			}
		}

		/*
		 * 一時格納フォルダを削除する
		 */
		ObjectDomain o = objectService.getById(Long.parseLong(tmpObjId));
		if (o != null) {
			File tmpStoreFolder = new File(EIMConfig.get("TEMP") + "/" + o.getId());
			if (tmpStoreFolder.exists() && FileUtil.clean(tmpStoreFolder)) {
				objectService.delete(o);
			}
		}

		return createObjectList;

	}

	/**
	 * ドキュメントのEIMオブジェクトタイプを取得する。
	 */
	private EIMObjectType getDocumentObjType(EIMSession sess, String id) throws Exception {
		if (id != null && ObjectUtils.getObjectTypeById(sess, Long.parseLong(id)) == null) {
			return null;
		}

		return (id == null || id.length() == 0)
				? ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT"))
				: ObjectUtils.getObjectTypeById(sess, Long.parseLong(id));
	}

	/**
	 * ドキュメントのオブジェクトタイプドメインを取得する。
	 */
	private ObjectTypeDomain getDocumentObjType(String id) throws Exception {
		if (id != null &&  objectTypeService.getById(Long.parseLong(id)) == null) {
			return null;
		}

		return (id == null || id.length() == 0)
				? objectTypeService.getByDefinitionName(EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT"))
				: objectTypeService.getById(Long.parseLong(id));
	}

	/*
	 * 以下の場合、配下のオブジェクトのステータスを "CANNOT_UPLOAD" に変更する。 1.自分自身（data）がフォルダ
	 * 2.自分自身（data）のステータスが "CANNOT_UPLOAD"
	 * 3.自分自身（data）の配下に登録不可以外のオブジェクト（child）がある。
	 */
	private ArrayList setLowerObjectCannotUpload(ArrayList list) {
		for (int ii = 0; ii < list.size(); ii++) {
			DocumentFromBoxUploadData data = (DocumentFromBoxUploadData) list.get(ii);
			if (data.isFolder() && (data.isConfirmAssign() || data.cannotAssign())) {
				for (int jj = ii + 1; jj < list.size(); jj++) {
					DocumentFromBoxUploadData child = (DocumentFromBoxUploadData) list.get(jj);
					if (child.isChild(data.getPath())) {
						if (data.cannotAssign() && child.cannotAssign() == false) {
							child.setCannotAssign(EIMResource.getMessage("EIM.ERROR.INPUT.UPPEROBJ.CANNOTASSIGN"));
						}
					} else {
						break;
					}
				}
			}
		}

		return list;
	}

	/*
	 * 以下の場合、フォルダ（data）のステータスを "CONFIRM_AND_CHECK_UPLOAD" に変更する。
	 * 1.自分自身（data）がフォルダ 2.自分自身（data）のステータスが "CONFIRM_UPLOAD"
	 * 3.自分自身（data）の配下に登録可能なオブジェクト（child）がある。
	 */
	private ArrayList setUppderObjectConfirmUpload(ArrayList list) {
		for (int ii = 0; ii < list.size(); ii++) {
			DocumentFromBoxUploadData data = (DocumentFromBoxUploadData) list.get(ii);
			if (data.isFolder() && (data.isConfirmAssign() || data.cannotAssign())) {
				for (int jj = ii + 1; jj < list.size(); jj++) {
					DocumentFromBoxUploadData child = (DocumentFromBoxUploadData) list.get(jj);
					if (child.isChild(data.getPath())) {
						if (data.isConfirmAssign() && child.canAssign()) {
							// 上記Ａ
							data.setCheckedConfirm(EIMResource.getMessage("EIM.ERROR.INPUT.SUBORDINATEOBJ.ASSIGN"));
							break;
						}
					} else {
						break;
					}
				}
			}
		}

		return list;
	}

	/**
	 * Boxのファイル/フォルダを削除します。
	 *
	 * @param id
	 *            BoxId
	 * @param type
	 *            オブジェクトタイプ
	 * @param name
	 *            オブジェクト名
	 * @param path
	 *            オブジェクトパス
	 * @param resquest
	 *            HTTPサーブレットリクエスト
	 * @return BoxId*/
	public void delete(String id, String type, String name, String path) throws Exception {

		//Boxファイルの削除
		if (type.equals("file")) {
			deleteFile(id);
		//Boxフォルダの削除
		} else if (type.equals("folder")) {
			deleteFolder(id);
		}

		// 操作履歴を作成
		OperationHistoryService operationHistoryService = (OperationHistoryService) ApplicationContextLoader
				.getApplicationContext().getBean("operationHistoryService2");

		OperationHistoryDomain operationHistory = new OperationHistoryDomain();
		operationHistory.setApplicationTypeId(2);
		operationHistory.setOperationTypeId(2402);
		operationHistory.setRecordInfoIdA(2);
		operationHistory.setRecordObjectA(name);
		operationHistory.setDetail(path);
		operationHistoryService.create(operationHistory);
	}

	/**
	 * Boxのファイルを削除します。
	 *
	 * @param id ファイルid
	 * @return ファイルの削除結果
	 * @throws Exception */
	private void deleteFile(String id) throws Exception {
		try {
			boxFileDao.delete(id);

		} catch (EIMApplicationException e) {
			switch (e.getMessageKey()) {
			default:
				throw e;
			}
		}
	}

	/**
	 * Boxのフォルダを削除します。
	 *
	 * @param id フォルダid
	 * @return フォルダの削除結果
	 * @throws Exception */
	private void deleteFolder(String id) throws Exception {
		try {
			boxFolderDao.delete(id);

		} catch (EIMApplicationException e) {
			switch (e.getMessageKey()) {
			default:
				throw e;
			}
		}
	}

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.BoxDocumentService#download(String,
	 *      OutputStream)
	 */
	@Override
	public void download(String id, OutputStream stream) throws Exception {
		boxFileDao.download(id, stream);
	}

	/**
	 * BoxファイルDaoを設定します。
	 *
	 * @param boxFileDao
	 *            BoxファイルDao
	 */
	public void setBoxFileDao(BoxFileDao boxFileDao) {
		this.boxFileDao = boxFileDao;
	}

	/**
	 * BoxフォルダDaoを設定します。
	 * @param boxFolderDao
	 *            BoxフォルダDao
	 */
	public void setBoxFolderDao(BoxFolderDao boxFolderDao) {
		this.boxFolderDao = boxFolderDao;
	}

	/**
	 * オブジェクトサービスを設定します。
	 * @param objectService
	 *            オブジェクトサービス
	 */
	public void setObjectService(ObjectService objectService) {
		this.objectService = objectService;
	}

	/**
	 * オブジェクトサービスを設定します。
	 *
	 * @param objectTypeService
	 *            オブジェクトサービス
	 * @since Ver6.44
	 */
	public void setObjectTypeService(ObjectTypeService objectTypeService) {
		this.objectTypeService = objectTypeService;
	}

	/**
	 * フォーマットサービスを設定します。
	 * @param formatService
	 *            フォーマットサービス
	 */
	public void setFormatService(FormatService formatService) {
		this.formatService = formatService;
	}

	/**
	 * ファイルアクセサーを設定します。
	 * @param fileAccessor
	 *            ファイルアクセサー
	 */
	public void setFileAccessor(FileAccessor fileAccessor) {
		this.fileAccessor = fileAccessor;
	}

	/**
	 * ファイルDAOを設定します。
	 * @param fileDao
	 *            ファイルDAO
	 */
	public void setFileDao(FileDao fileDao) {
		this.fileDao = fileDao;
	}

	/**
	 * Box保管情報サービスを設定します。
	 * @param boxArchiveInfoService
	 *            Box保管情報サービス
	 */
	public void setBoxArchiveInfoService(BoxArchiveInfoService boxArchiveInfoService) {
		this.boxArchiveInfoService = boxArchiveInfoService;
	}

}
