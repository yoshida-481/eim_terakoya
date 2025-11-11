package common.util;

import java.io.File;

import jp.co.ctc_g.eim.app.document.presentation.dto.ConfirmBoxDocumentDTO;

public class DocumentFromBoxUploadData
{
	private File file;
	private String path;
	private String status;
	private String reason;
	private Long sameNameObj;

	/**
	 * コンストラクタ
	 * @param f ファイル
	 * @param checkResultMap
	 */
	public DocumentFromBoxUploadData(File f, String p, String s, String r,Long o)
	{
		file = f;
		path = p;
		status = s;
		reason = r;
		sameNameObj = o;
	}

	/**
	 * フォルダかどうか
	 */
	public boolean isFolder()
	{
		return file.isDirectory();
	}

	/**
	 * ステータスが「登録可能」かどうか
	 */
	public boolean canAssign()
	{
		return status.equals(DocumentFromBoxUploadManager.CAN_UPLOAD);
	}

	/**
	 * ステータスが「登録不可」かどうか
	 */
	public boolean cannotAssign()
	{
		return status.equals(DocumentFromBoxUploadManager.CANNOT_UPLOAD);
	}

	/**
	 * ステータスが「登録確認」かどうか
	 */
	public boolean isConfirmAssign()
	{
		return status.equals(DocumentFromBoxUploadManager.CONFIRM_UPLOAD);
	}

	/**
	 * ステータスを「登録不可」に設定する
	 */
	public void setCannotAssign(String r)
	{
		status = DocumentFromBoxUploadManager.CANNOT_UPLOAD;
		reason = r;
	}

	/**
	 * ステータスを「チェック済み登録確認」に設定する
	 */
	public void setCheckedConfirm(String r)
	{
		status = DocumentFromBoxUploadManager.CONFIRM_AND_CHECK_UPLOAD;
		reason = r;
	}

	/**
	 * パスを取得する
	 */
	public String getPath()
	{
		return path;
	}

	/**
	 * ファイルの拡張子を取得する
	 */
	public String getExtType()
	{
		String fileName = file.getName();
	    // 最後の『 . 』の位置を取得します。
	    int lastDotPosition = fileName.lastIndexOf(".");

	    // 『 . 』が存在する場合は、『 . 』以降を返します。
	    if (lastDotPosition != -1) {
	        return fileName.substring(lastDotPosition);
	    }
	    return null;
	}

	/**
	 * ファイルパスを取得する
	 */
	public String getFilePath()
	{
		String filePath = file.getPath();
		return filePath;

	}

	/**
	 * 親のパスを含んでいるかどうか（但し同一パスの場合は除く）
	 * @param parentPath 親データのパス
	 */
	public boolean isChild(String parentPath)
	{
		return path.indexOf(parentPath) == 0 && path.equals(parentPath) == false;
	}

	/**
	 * 登録可否チェック結果を作成する
	 */
	public ConfirmBoxDocumentDTO createCheckResult()
	{

		ConfirmBoxDocumentDTO  checkData = new ConfirmBoxDocumentDTO(file.getName(),path,"document",status,false,reason,sameNameObj);

		return checkData;
	}
}
