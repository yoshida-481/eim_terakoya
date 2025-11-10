package jp.co.ctc_g.eim.app.document.business.service;

import java.util.Date;
import java.util.List;
import org.springframework.web.multipart.MultipartFile;

import jp.co.ctc_g.eim.app.document.business.domain.FolderDomain;
import jp.co.ctc_g.eim.app.document.presentation.dto.ConfirmDocumentDTO;

/**
 *
 * フォルダアップロードサービスクラス
 *
 */
public interface FolderUploadService {
    /**
     * 指定されたファイルリストをフォルダ構造を保持したままアップロードします。
     *
     * @param fileList       アップロードするファイルのリスト
     * @param parentObjId    アップロード先の親オブジェクトのID
     * @param documentTypeId アップロード対象に付与するドキュメントタイプのID
     * @param folderPathList アップロードするファイルのフォルダパスリスト
     * @param createUserId   作成ユーザーID
     * @param property       プロパティ
     * @param expirationDate 有効期限
     * @return アップロードされたフォルダ情報のリスト
     * @throws Exception アップロード処理中にエラーが発生した場合
     */
	public List<FolderDomain> folderUpload(List<MultipartFile> fileList, long parentObjId, long documentTypeId,
			List<String> folderPathList, long createUserId, String property, Date expirationDate) throws Exception;


    /**
     * アップロード対象のドキュメントとフォルダについて、登録可否確認を行います。
     *
     * @param files          確認対象のファイルリスト
     * @param folderPathList 確認対象のファイルのフォルダパスリスト
     * @param parentPath     アップロード先の親フォルダのパス
     * @param documentTypeId ドキュメントタイプのID
     * @param createUserId   作成ユーザーのID
     * @return 確認結果のドキュメント情報リスト
     * @throws Exception 確認処理中にエラーが発生した場合
     */
	public List<ConfirmDocumentDTO> confirmUploadTarget(List<MultipartFile> files, List<String> folderPathList,
			String parentPath, long documentTypeId) throws Exception;

}