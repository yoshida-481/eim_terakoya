package jp.co.ctc_g.eim.common.presentation.web.controller;

import java.util.Date;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.multipart.MultipartFile;

import eim.util.EIMConfig;
import jp.co.ctc_g.eim.app.document.business.domain.FolderDomain;
import jp.co.ctc_g.eim.app.document.business.service.FolderUploadService;
import jp.co.ctc_g.eim.app.document.business.service.impl.FolderUploadServiceImpl;
import jp.co.ctc_g.eim.app.document.presentation.dto.ConfirmDocumentDTO;
import jp.co.ctc_g.eim.framework2.common.exception.EIMException;
import jp.co.ctc_g.eim.framework2.presentation.web.controller.RestController;

/**
 * フォルダ一括アップロードを制御するコントローラクラスです。
 *
 * @since Ver6.18
 */
@Controller
@RequestMapping({ "/rest/common/lumpFolder" })
public class LumpFolderController extends RestController {

	/** ロガー */
	private static Log logger = LogFactory.getLog(FolderUploadServiceImpl.class);

	/** フォルダアップロードサービス */
	FolderUploadService folderUploadService;

    /**
     * フォルダ一括アップロードを実行します。
     *
     * @param fileList       アップロードするファイルのリスト
     * @param folderPathList アップロードすふファイルのフォルダパスリスト
     * @param parentObjId    アップロード先の親オブジェクトID
     * @param documentTypeId アップロード対象ドキュメントに付与するドキュメントタイプID
     * @param createUserId   作成ユーザーID
     * @param property       プロパティ
     * @param expirationDate 有効期限
     * @return フォルダ情報リスト
     * @throws EIMException ファイルサイズ制限超過、ファイル数制限超過、その他のエラーが発生した場合など
     * @throws Exception    システムエラーが発生した場合
     */
	@RequestMapping(value = "/upload", method = RequestMethod.POST)
	@ResponseBody
	public List<FolderDomain> upload(@RequestParam(name = "fileList", defaultValue = "") List<MultipartFile> fileList,
			@RequestParam("folderPathList") List<String> folderPathList, @RequestParam("objId") long parentObjId,
			@RequestParam("documentTypeId") long documentTypeId, @RequestParam("createUserId") long createUserId,
			@RequestParam(value = "property", required = false) String property,
			@RequestParam(value = "expirationDate", required = false) Date expirationDate) throws Exception {

		for (MultipartFile file : fileList) {
			// ファイルサイズのチェック(空のファイル)
			if (file.getSize() == 0) {
				throw new EIMException("EIM.ERROR.LOGIC.UPLOAD.FILE.SIZE.ZERO", folderPathList.get(0),
						file.getOriginalFilename());
			}

			// ファイルサイズのチェック(最大値)
			long maxFileSize = Long.parseLong(EIMConfig.get("UPLOAD_FILE_SIZE_MAX"));
			long totalFileSize = 0;
			totalFileSize += file.getSize();
			if (totalFileSize > maxFileSize) {
				throw new EIMException("EIM.ERROR.LOGIC.UPLOAD.FILE.TOTALSIZE.OVER", maxFileSize);
			}
		}

		try {
			return this.folderUploadService.folderUpload(fileList, parentObjId, documentTypeId, folderPathList,
					createUserId, property, expirationDate);
		} catch (EIMException e) {
			logger.error(e.getMessage(), e);
			throw e;
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			throw new EIMException("EIM.ERROR.SYSTEMERROR");
		}
	}

    /**
     * フォルダアップロード対象の登録可否チェックを実行します。
     *
     * @param targetFileList  アップロード対象ファイルリスト
     * @param folderPathList  アップロード対象ファイルのフォルダパスリスト
     * @param parentPath      親フォルダのパス
     * @param documentTypeId  ドキュメントタイプID
     * @return 確認結果のドキュメント情報リスト
     * @throws Exception チェック処理中にエラーが発生した場合
     */
	@RequestMapping(value = "/confirm", method = RequestMethod.POST)
	@ResponseBody
	public List<ConfirmDocumentDTO> confirmUploadTarget(@RequestParam("fileList") List<MultipartFile> targetFileList,
			@RequestParam("folderPathList") List<String> folderPathList, @RequestParam("parentPath") String parentPath,
			@RequestParam("documentTypeId") long documentTypeId) throws Exception {

		for (MultipartFile file : targetFileList) {
			// ファイルサイズのチェック(最大値)
			long maxFileSize = Long.parseLong(EIMConfig.get("UPLOAD_FILE_SIZE_MAX"));
			long totalFileSize = 0;
			totalFileSize += file.getSize();
			if (totalFileSize > maxFileSize) {
				throw new EIMException("EIM.ERROR.LOGIC.UPLOAD.FILE.TOTALSIZE.OVER", maxFileSize);
			}
		}
		List<ConfirmDocumentDTO> results = folderUploadService.confirmUploadTarget(targetFileList, folderPathList,
				parentPath, documentTypeId);

		return results;
	}

    /**
     * フォルダアップロードサービスを設定します。
     *
     * @param folderUploadService 設定するフォルダアップロードサービス
     */
	public void setFolderUploadService(FolderUploadService folderUploadService) {
		this.folderUploadService = folderUploadService;
	}

}
