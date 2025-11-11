package common.util;

import java.io.File;

import eim.util.StringUtils;
import jp.co.ctc_g.eim.app.document.presentation.dto.ConfirmDocumentDTO;

public class LumpUploadXmlData
{
	private File file;
	private String path;
	private String status;
	private String reason;

	/**
	 * コンストラクタ
	 * @param f ファイル
	 * @param checkResultMap
	 */
	public LumpUploadXmlData(File f, String p, String s, String r)
	{
		file = f;
		path = p;
		status = s;
		reason = r;
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
		return status.equals(LumpUploadManager.CAN_UPLOAD);
	}

	/**
	 * ステータスが「登録不可」かどうか
	 */
	public boolean cannotAssign()
	{
		return status.equals(LumpUploadManager.CANNOT_UPLOAD);
	}

	/**
	 * ステータスが「登録確認」かどうか
	 */
	public boolean isConfirmAssign()
	{
		return status.equals(LumpUploadManager.CONFIRM_UPLOAD);
	}

	/**
	 * ステータスを「登録不可」に設定する
	 */
	public void setCannotAssign(String r)
	{
		status = LumpUploadManager.CANNOT_UPLOAD;
		reason = r;
	}

	/**
	 * ステータスを「チェック済み登録確認」に設定する
	 */
	public void setCheckedConfirm(String r)
	{
		status = LumpUploadManager.CONFIRM_AND_CHECK_UPLOAD;
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
	 * 表示用XML文字列を出力する
	 */
	public String toXMLString()
	{
		StringBuffer sb = new StringBuffer();

		sb.append("  <object\n");
		sb.append("   objName=\"" + StringUtils.xmlEncode(file.getName()) + "\"\n");
		sb.append("   path=\"" + StringUtils.xmlEncode(path) + "\"\n");
		sb.append("   objType=\"" + (file.isFile()? "document":"folder") + "\"\n");
		sb.append("   uploadStatus=\"" + status + "\"\n");
		sb.append("   checkinFlag=\"" + "false" + "\"\n");
		sb.append("   reason=\"" + reason + "\"\n");
		sb.append("  />\n");

		return sb.toString();
	}

	/**
	 * 登録確認結果DTOを作成する
	 */
	public ConfirmDocumentDTO createCheckResult()
	{

		ConfirmDocumentDTO  checkData = new ConfirmDocumentDTO(file.getName(),path,file.isFile()? "document":"folder",status,false,reason);

		return checkData;
	}
}
