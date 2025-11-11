package jp.co.ctc_g.eim.app.document.business.service.impl;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.web.multipart.MultipartFile;

import common.util.AppConstant;
import common.util.AppObjectConditionHelper;
import common.util.AppObjectUtil;
import common.util.LumpUploadManager;
import common.util.LumpUploadXmlData;
import eim.bo.EIMAccessRole;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMResource;
import eim.net.EIMSession;
import eim.util.ObjectUtils;
import jp.co.ctc_g.eim.app.document.business.domain.DocumentDomain;
import jp.co.ctc_g.eim.app.document.business.domain.FolderDomain;
import jp.co.ctc_g.eim.app.document.business.domain.PlaceDomain;
import jp.co.ctc_g.eim.app.document.business.domain.WorkspaceDomain;
import jp.co.ctc_g.eim.app.document.business.service.DocumentService;
import jp.co.ctc_g.eim.app.document.business.service.FolderService;
import jp.co.ctc_g.eim.app.document.business.service.FolderUploadService;
import jp.co.ctc_g.eim.app.document.business.service.WorkspaceService;
import jp.co.ctc_g.eim.app.document.common.util.AppDocumentLogicUtil;
import jp.co.ctc_g.eim.app.document.common.util.AppDocumentUtil;
import jp.co.ctc_g.eim.app.document.presentation.dto.ConfirmDocumentDTO;
import jp.co.ctc_g.eim.framework2.business.dao.ObjectDao;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AccessRoleTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.OperationHistoryDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.RelationTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework2.business.service.AttributeTypeService;
import jp.co.ctc_g.eim.framework2.business.service.OperationHistoryService;
import jp.co.ctc_g.eim.framework2.common.exception.EIMException;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;
import jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils;

/**
 * フォルダアップロードサービス
 *
 * @see FolderUploadService
 */
public class FolderUploadServiceImpl implements FolderUploadService {

	/** ロガー */
	private static Log log = LogFactory.getLog(FolderUploadServiceImpl.class);

	/** ワークスペースサービス */
	private WorkspaceService workspaceService;
	/** フォルダサービス */
	private FolderService folderService;
	/** ドキュメントサービス */
	private DocumentService documentService;
	/** 属性タイプサービス */
	private AttributeTypeService attributeTypeService;
	/** 操作履歴サービス */
	private OperationHistoryService operationHistoryService;

	/** オブジェクトDao */
	private static ObjectDao objectDao = null;


	@Override
	public List<FolderDomain> folderUpload(List<MultipartFile> fileList, long parentObjId, long documentTypeId,
			List<String> folderPathList, long createUserId, String property, Date expirationDate) throws Exception {
		HashMap<String, FolderDomain> folderIdMap = new HashMap<String, FolderDomain>();

		String parentFolderPath = "/";
		// Top階層の情報を退避
		// parentObjIdは、WorkspaceIDの場合と、FolderIDの場合がある
		WorkspaceDomain parentFolder = workspaceService.getById(parentObjId);
		if (parentFolder == null) {
			parentFolder = folderService.getById(parentObjId);
			if (parentFolder != null) {
				parentFolderPath = parentFolder.getPath();
			}
		}

		if (parentFolder == null) {
			return null;
		}
		parentFolderPath += parentFolder.getName() + "/";

		// 操作履歴
		OperationHistoryDomain historyDomain = new OperationHistoryDomain();
		historyDomain.setApplicationTypeId(Integer.parseInt(AppConstant.DOCUMENT));
		historyDomain.setOperationTypeId(2220);
		historyDomain.setDetail(parentFolderPath);
		operationHistoryService.create(historyDomain);

		createFolderIdMap(folderIdMap, parentFolder);
		List<FolderDomain> topFolderDomain = createFolder(folderIdMap, parentFolderPath, parentFolder, folderPathList);
		createDocument(fileList, folderPathList, folderIdMap, parentFolderPath, documentTypeId, createUserId, property,
				expirationDate);

		return topFolderDomain;

	}

	@Override
	public List<ConfirmDocumentDTO> confirmUploadTarget(List<MultipartFile> targetFileList, List<String> folderPathList,
			String parentPath, long documentTypeId) throws Exception {

		// セッション情報の取得
		EIMSession sess = jp.co.ctc_g.eim.framework.common.util.EIMThreadContext.getEIMSession();

		List<ConfirmDocumentDTO> result = new ArrayList<>();

		Date now = new Date();
		SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMddHHmmss");
		String objName = "FolderUL_" + sess.getUser().getId() + "_" + sdf.format(now);

		// 一時格納フォルダにファイルを保存する
		File folder = new File(ConfigUtils.getByKey("TEMP") + objName);

		// フォルダーを作成
		if (folder.mkdirs()) {
			System.out.println("フォルダーが作成されました: " + folder.getAbsolutePath());
		} else {
			throw new EIMException("一時ファイルの保存に失敗しました");
		}

		// フォルダパスのリストからユニークなフォルダパスを抽出して作成
		Set<String> uniqueFolderPaths = new HashSet<>(folderPathList);
		for (String relativePath : uniqueFolderPaths) {
		    Path folderPath = Paths.get(folder.getAbsolutePath(), relativePath);
		    try {
		        Files.createDirectories(folderPath);
		    } catch (IOException e) {
		        throw new EIMException("フォルダの作成に失敗しました");
		    }
		}

		for (int i = 0; i < targetFileList.size(); i++) {
			MultipartFile file = targetFileList.get(i);

			// フォルダパスを取得
			String relativePath = folderPathList.get(i);

			// オリジナルのファイル名を取得
			String fileName = file.getOriginalFilename();

			// フォルダパスとファイル名を結合して保存先のパスを生成
			Path destinationPath = Paths.get(folder.getAbsolutePath(), relativePath, fileName);

			try {
				// ファイルを保存
				try (InputStream inputStream = file.getInputStream()) {
					Files.copy(inputStream, destinationPath, StandardCopyOption.REPLACE_EXISTING);
				}
			} catch (IOException e) {
				throw new EIMException("一時ファイルの保存に失敗しました");
			}
		}

		/*
		 * 一時格納フォルダをウォーキングして、登録可否のチェックを行う。 （ZIPアップロード処理を流用）
		 */

		EIMObjectType documentObjType = ObjectUtils.getObjectTypeById(sess, documentTypeId);

		String tmpFolderPath = ConfigUtils.getByKey("TEMP") + objName + "/";
		LumpUploadManager manager = new LumpUploadManager(sess, documentObjType, tmpFolderPath, parentPath);
		manager.createParentObjectMap(folder, objName);
		ArrayList<?> retList = manager.recursiveCheckAssign(folder, true);
		retList = setLowerObjectCannotUpload(retList);
		retList = setUppderObjectConfirmUpload(retList);

		// ファイルサイズが0byteの場合は登録対象外
		@SuppressWarnings("unchecked")
		ArrayList<LumpUploadXmlData> lumpUploadXmlDataList = (ArrayList<LumpUploadXmlData>) retList;

		lumpUploadXmlDataList.stream()
		    .filter(data -> {
		        File file = new File(data.getFilePath());
		        return file.isFile() && file.length() == 0;
		    })
		    .forEach(data -> data.setCannotAssign(EIMResource.getMessage("EIM.ERROR.LOGIC.NOUPLOADFILE")));

		// レスポンスを作成
		result.addAll(lumpUploadXmlDataList.stream()
		    .map(LumpUploadXmlData::createCheckResult)
		    .collect(Collectors.toList()));

		// 一時格納フォルダ削除
		// フォルダのPathを指定して削除
		Path folderPath = Paths.get(folder.getAbsolutePath());
		deleteDirectory(folderPath);

		return result;
	}

    /**
     * 一時格納フォルダを削除します。
     *
     * @param directoryPath  削除対象ディレクトリ
     *
     * @throws IOException 削除失敗時
     */
	private void deleteDirectory(Path directoryPath) throws IOException {
		if (Files.exists(directoryPath)) {
			// ディレクトリ内のすべてのファイルとサブディレクトリを削除
			Files.walk(directoryPath).sorted(Comparator.reverseOrder()) // 子から親の順に削除
					.forEach(path -> {
						try {
							Files.delete(path);
						} catch (IOException e) {
							System.err.println("削除に失敗しました: " + path + ", エラー: " + e.getMessage());
						}
					});
		}
	}

    /**
     * 【ZIPアップロードの処理を流用】
	 * 以下の場合、配下のオブジェクトのステータスを "CANNOT_UPLOAD" に変更する。 1.自分自身（data）がフォルダ
	 * 2.自分自身（data）のステータスが "CANNOT_UPLOAD"
	 * 3.自分自身（data）の配下に登録不可以外のオブジェクト（child）がある。
     *
     * @param list  チェック対象リスト
     * @return list 確認結果
     */
	private ArrayList<?> setLowerObjectCannotUpload(ArrayList<?> list) {
		for (int ii = 0; ii < list.size(); ii++) {
			LumpUploadXmlData data = (LumpUploadXmlData) list.get(ii);
			if (data.isFolder() && (data.isConfirmAssign() || data.cannotAssign())) {
				for (int jj = ii + 1; jj < list.size(); jj++) {
					LumpUploadXmlData child = (LumpUploadXmlData) list.get(jj);
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

    /**
     * 【ZIPアップロードの処理を流用】
	 * 以下の場合、フォルダ（data）のステータスを "CONFIRM_AND_CHECK_UPLOAD" に変更する。
	 * 1.自分自身（data）がフォルダ 2.自分自身（data）のステータスが "CONFIRM_UPLOAD"
	 * 3.自分自身（data）の配下に登録可能なオブジェクト（child）がある。
     *
     * @param list  チェック対象リスト
     * @return list 確認結果
     */
	private ArrayList<?> setUppderObjectConfirmUpload(ArrayList<?> list) {
		for (int ii = 0; ii < list.size(); ii++) {
			LumpUploadXmlData data = (LumpUploadXmlData) list.get(ii);
			if (data.isFolder() && (data.isConfirmAssign() || data.cannotAssign())) {
				for (int jj = ii + 1; jj < list.size(); jj++) {
					LumpUploadXmlData child = (LumpUploadXmlData) list.get(jj);
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
	 *
	 * ドキュメントを一括登録します.
	 *
     * @param fileList       アップロードするファイルのリスト
     * @param folderPathList アップロードするファイルのフォルダパスリスト
     * @param folderIdMap    フォルダパスをキー、フォルダドメインを値とするマップ
     * @param parentPath     アップロード先親フォルダのパス
     * @param documentTypeId アップロード対象ドキュメントに付与するドキュメントタイプID
     * @param createUserId   作成ユーザーID
     * @param property       プロパティ
     * @param expirationDate 有効期限
	 * @throws Exception
	 * @throws IOException
	 */
	private void createDocument(List<MultipartFile> fileList, List<String> folderPathList,
			HashMap<String, FolderDomain> folderIdMap, String parentPath, long documentTypeId, long createUserId,
			String property, Date expirationDate) throws Exception, IOException {

		for (int i = 0; i < fileList.size(); i++) {
			MultipartFile file = fileList.get(i);
			String folderPath = folderPathList.get(i);

			String[] folderPathLv = folderPath.split("/");
			String parentFolderPath = parentPath;

			for (int j = 0; j < folderPathLv.length; j++) {
				parentFolderPath += folderPathLv[j] + "/";
			}
			FolderDomain parentFolderDomain = folderIdMap.get(parentFolderPath);

			// ドキュメントドメイン作成
			DocumentDomain documentDomain = new DocumentDomain();
			documentDomain.setName(file.getOriginalFilename());
			ObjectTypeDomain objType = new ObjectTypeDomain();
			objType.setId(documentTypeId);
			documentDomain.setType(objType);

			// 作成者
			UserDomain createUser = new UserDomain();
			createUser.setId(createUserId);
			documentDomain.setCreateUser(createUser);

			List<AttributeDomain> attrList = new ArrayList<>();
			// プロパティ
			if (property != null) {
				AttributeDomain attrProperty = new AttributeDomain();
				AttributeTypeDomain attrTypeProperty = attributeTypeService
						.getByDefinitionName(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PROP"));
				attrProperty.setAttributeType(attrTypeProperty);
				attrProperty.setString(property);
				attrList.add(attrProperty);
			}
			// 有効期限
			if (expirationDate != null) {
				AttributeDomain attrExpirationDate = new AttributeDomain();
				AttributeTypeDomain attrTypeExpirationDate = attributeTypeService
						.getByDefinitionName(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_EFFECT_DATE"));
				attrExpirationDate.setAttributeType(attrTypeExpirationDate);
				attrExpirationDate.setDate(expirationDate);
				attrList.add(attrExpirationDate);
			}

			documentDomain.setAttributeList(attrList);

			// セッション情報の取得
			EIMSession sess = jp.co.ctc_g.eim.framework.common.util.EIMThreadContext.getEIMSession();

			String targetFullPath = parentFolderDomain.getPath() + parentFolderDomain.getName() + "/"
					+ documentDomain.getName();
			EIMObject sameNameObj = AppObjectUtil.getObjListByFullPass(sess, targetFullPath,
					ConfigUtils.getByKey("OBJECT_TYPE_NAME_DOCUMENT"));

			AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
			boolean isDocLink = false;
			boolean isDocument = false;
			if (sameNameObj != null) {
				isDocLink = helper.isDocumentLink(ConvertUtils.toEIMObject(parentFolderDomain), sameNameObj);
				isDocument = helper.isDocument(ConvertUtils.toEIMObject(parentFolderDomain), sameNameObj);
			}
			if (sameNameObj == null || (isDocLink && isDocument == false)) {
				// 新規登録
				documentService.create(documentDomain, parentFolderDomain, file.getInputStream());
			} else {
				// チェックアウト＆チェックイン
				documentDomain.setId(sameNameObj.getId());
				DocumentDomain newDocumentDomain = documentService.checkout(documentDomain);
				documentDomain.setId(newDocumentDomain.getId());
				documentService.checkin(documentDomain, file.getInputStream());

				// 属性「作成者」「プロパティ」「有効期限」の設定
				// 作成者
				AttributeTypeDomain attrTypeUser = attributeTypeService
						.getByDefinitionName(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_CREATE"));

				objectDao.setAttributeSingleLong(documentDomain, attrTypeUser, createUserId);


				// プロパティ
				if (property != null) {
					AttributeTypeDomain attrTypeProperty = attributeTypeService
							.getByDefinitionName(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PROP"));
					objectDao.setAttributeSingleString(documentDomain, attrTypeProperty, property);
				}
				// 有効期限
				if (expirationDate != null) {
					AttributeTypeDomain attrTypeExpirationDate = attributeTypeService
							.getByDefinitionName(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_EFFECT_DATE"));
					objectDao.setAttributeSingleDate(documentDomain, attrTypeExpirationDate, expirationDate);
				}
			}
		}
	}

	/**
	 * フォルダ階層構造に基づいてフォルダIDマップを作成します。
	 *
	 * @param folderIdMap フォルダパスをキー、フォルダドメインを値とするマップ
	 * @param parentFolder 親フォルダドメイン
	 * @throws Exception
	 */
	private void createFolderIdMap(HashMap<String, FolderDomain> folderIdMap, WorkspaceDomain parentFolder) throws Exception{

	    createFolderIdMapRecursive(folderIdMap, parentFolder.getFolderList());
	}

	/**
	 * フォルダリストを再帰的に処理し、フォルダIDマップを作成します。
	 *
	 * @param folderIdMap フォルダパスをキー、フォルダドメインを値とするマップ
	 * @param folderList 処理対象のフォルダリスト
	 * @throws Exception フォルダ情報の取得に失敗した場合
	 */
	private void createFolderIdMapRecursive(HashMap<String, FolderDomain> folderIdMap,
	        List<FolderDomain> folderList) throws Exception {
	    if (folderList == null || folderList.isEmpty()) {
	        return;
	    }

	    for (FolderDomain folder : folderList) {
	        String folderPath = folder.getPath() + folder.getName() + "/";
	        folderIdMap.put(folderPath, folder);

	        // 子フォルダ情報を取得して再帰処理
			RelationTypeDomain relationTypeDomain = AppDocumentLogicUtil.getDocumentRelType();
			AccessRoleTypeDomain accessRoleType = new AccessRoleTypeDomain("READ");
			accessRoleType.setId(EIMAccessRole.READ);
			List<ObjectDomain> childList = AppDocumentUtil.getChildObjectByRelTypeAndParent(relationTypeDomain, folder,
					accessRoleType);

			List<FolderDomain> childFolderList = childList.stream()
				    .map(domain -> {
				        FolderDomain childFolder = new FolderDomain();
				        childFolder.setId(domain.getId());
				        childFolder.setName(domain.getName());
				        childFolder.setAttributeList(domain.getAttributeList());
				        return childFolder;
				    })
				    .collect(Collectors.toList());

	        createFolderIdMapRecursive(folderIdMap, childFolderList);
	    }
	}

	/**
	 *
	 * フォルダを登録します
	 *
	 * @param folderIdMap フォルダパスをキー、フォルダドメインを値とするマップ
	 * @param parentPath  アップロード先親フォルダのパス
	 * @param parent      親フォルダドメイン
	 * @param folderPathList アップロード対象のフォルダパスリスト
	 * @return
	 * @throws Exception
	 */
	private List<FolderDomain> createFolder(HashMap<String, FolderDomain> folderIdMap, String parentPath,
			PlaceDomain parent, List<String> folderPathList) throws Exception {

		// 重複を除去
		Set<String> UniqeFolderPathSet = new HashSet<>(folderPathList);
		List<String> UniqeFolderPathList = new ArrayList<>(UniqeFolderPathSet);
		Collections.sort(UniqeFolderPathList);

		List<FolderDomain> topFolderDomain = new ArrayList<FolderDomain>();

		// フォルダの情報を作成する
		for (String folderPath : UniqeFolderPathList) {
			PlaceDomain tmpParent = parent;
			String targetFolderKey = parentPath + folderPath;
			targetFolderKey = targetFolderKey.replace("//", "/");
			// ひとつ前の情報を取得
			if (!folderIdMap.containsKey(targetFolderKey)) {
				String[] folderPathLv = folderPath.split("/");

				int n = folderPathLv.length;
				String path = parentPath;
				// パスの最下層からルートフォルダを１つずつ巡って、ディレクトリがあるパスを探す
				for (; 0 < n; --n) {
					for (int j = 0; j < n; ++j) {
						path += folderPathLv[j] + "/";
					}

					if (folderIdMap.containsKey(path)) {
						tmpParent = folderIdMap.get(path);
						break;
					}
					path = parentPath;
				}

				// ディレクトリがある場所から、パスの最下層まで１つずつフォルダを作成
				for (; n < folderPathLv.length; ++n) {
					FolderDomain targetFolder = new FolderDomain();
					targetFolder.setName(folderPathLv[n]);
					targetFolder = folderService.create(targetFolder, tmpParent);
					String newKey = targetFolder.getPath() + targetFolder.getName() + "/";
					folderIdMap.put(newKey, targetFolder);
					tmpParent = targetFolder;

					if (n == 0) {
						topFolderDomain.add(targetFolder);
					}

				}

			}
		}

		return topFolderDomain;
	}

    /**
     * ワークスペースサービスを設定します。
     *
     * @param workspaceService ワークスペースサービス
     */
	public void setWorkspaceService(WorkspaceService workspaceService) {
		this.workspaceService = workspaceService;
	}

    /**
     * フォルダサービスを設定します。
     *
     * @param folderService フォルダサービス
     */
	public void setFolderService(FolderService folderService) {
		this.folderService = folderService;
	}

    /**
     *ドキュメントサービスを設定します。
     *
     * @param documentService ドキュメントサービス
     */
	public void setDocumentService(DocumentService documentService) {
		this.documentService = documentService;
	}

    /**
     * 属性タイプサービスを設定します。
     *
     * @param attributeTypeService 属性タイプサービス
     */
	public void setAttributeTypeService(AttributeTypeService attributeTypeService) {
		this.attributeTypeService = attributeTypeService;
	}

    /**
     * 操作履歴サービスを設定します。
     *
     * @param operationHistoryService 操作履歴サービス
     */
	public void setOperationHistoryService(OperationHistoryService operationHistoryService) {
		this.operationHistoryService = operationHistoryService;
	}

	/**
	 * @return objectDaoを設定します。
	 */
	public static void setObjectDao(ObjectDao objectDao) {
		FolderUploadServiceImpl.objectDao = objectDao;
	}
}
