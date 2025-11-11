package common.util;

import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import jakarta.servlet.http.HttpServletRequest;

import org.apache.commons.fileupload2.core.DiskFileItemFactory;
import org.apache.commons.fileupload2.core.FileItem;
import org.apache.commons.fileupload2.jakarta.JakartaServletDiskFileUpload;

import eim.util.EIMConfig;

/**
 * 
 * ファイルアップロードクラス
 * 
 * @deprecated 本クラスは下位バージョンとの互換性維持のために残していますが、推奨されません。
 *               Strutsの「FormFile」を使うようにして下さい。
 */
public class DocumentMultiPartFormUtils {
	private FileItem _fileItem = null;

	private List _paramField = null;

	private List _paramValue = null;
	
	private  SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMddHHmmssSSS");

	/**
	 * コンストラクタ
	 * 
	 * @param req
	 *            HTTPリクエスト
	 * @throws Exception
	 * @deprecated Strutsの「FormFile」を使うようにして下さい。
	 */
	public DocumentMultiPartFormUtils(HttpServletRequest req) throws Exception {
		_paramField = new ArrayList();
		_paramValue = new ArrayList();
		try {
			DiskFileItemFactory factory = DiskFileItemFactory.builder()
					.setBufferSize(Integer.parseInt(EIMConfig.get("UPLOAD_FILE_SIZE_10_THRESHOLD"))) // ファイルサイズ（閾値）
					.setPath(EIMConfig.get("TEMP")) // テンポラリフォルダパス
					.get();
			JakartaServletDiskFileUpload fu = new JakartaServletDiskFileUpload(factory);
			fu.setSizeMax(Long.parseLong(EIMConfig
					.get("UPLOAD_FILE_SIZE_10_MAX"))); // ファイルサイズ（最大値）

			List fileItems = fu.parseRequest(req);
			Iterator iter = fileItems.iterator();

			while (iter.hasNext()) {
				FileItem item = (FileItem) iter.next();
				if (item.isFormField()) {
					_paramField.add(item.getFieldName());
					_paramValue.add(item.getString(Charset.forName("UTF8")));
				} else {
					if (_fileItem == null) {
						_fileItem = item;
					}
				}
			}
		} catch (Exception e) {
			throw e;
		}

		return;
	}
	
	/**
	 * 
	 * パラメータ取得します。
	 * 
	 * @param fieldName
	 *            パラメータ名
	 * 
	 * @throws Exception
	 *             予期せぬ例外が発生した場合
	 * 
	 * @return パラメータ名
	 * @deprecated Strutsの「FormFile」を使うようにして下さい。
	 */
	public String getParameter(String fieldName) throws Exception {
		int i = 0;
		String paramValue = null;

		for (i = 0; i < _paramField.size(); i++) {
			String paramField = (String) _paramField.get(i);
			if (paramField.equals(fieldName)) {
				paramValue = (String) _paramValue.get(i);
				break;
			}
		}

		return paramValue;
	}

	/**
	 * 指定されたパスにファイルをアップロードします。
	 * 
	 * @param filePath
	 *            ファイルのパス
	 * @throws Exception
	 *             予期せぬ例外が発生した場合
	 * @deprecated Strutsの「FormFile」を使うようにして下さい。
	 */
	public void upload(String filePath) throws Exception {
		byte[] buff = new byte[4096];
		int len = 0;
		File file = new File(filePath);

		try {
			int total = 0;

			FileOutputStream fos = new FileOutputStream(file);
			InputStream is = _fileItem.getInputStream();
			while ((len = is.read(buff)) > 0) {
				fos.write(buff, 0, len);

				total += len;
			}
			is.close();
			fos.close();
			_fileItem.delete();

		} catch (Exception e) {
			throw e;
		}

		return;
	}
	
	/**
	 * FileItemを削除します。
	 * 
	 */
	public void deleteFileItem() throws Exception {
		if(_fileItem == null){
			return;
		}
		
		try {
			_fileItem.delete();

		} catch (Exception e) {
			// 削除できなかった場合なにもしない
		}
		return;
	}

	/**
	 * 
	 * ファイルサイズを取得します
	 * 
	 * @return ファイルサイズ。取得に失敗した場合は、0。
	 * @deprecated Strutsの「FormFile」を使うようにして下さい。
	 */
	public long getFileSize() {
		if (_fileItem == null) {
			return 0;
		}

		return _fileItem.getSize();
	}
	
	/**
	 * 
	 * パラメータフィールド名のリストを取得します
	 * 
	 * @return パラメータフィールドリスト。
	 */
	public List getParamFieldList() throws Exception{
		return _paramField;
	}
	
	/**
	 * 
	 * パラメータ値のリストを取得します
	 * 
	 * @return パラメータ値リスト。
	 */
	public List getParamValueList() throws Exception{
		return _paramValue;
	}
	
	
	/**
	 * 
	 * FileItemを取得します
	 * 
	 * @return FileItem。
	 */
	public FileItem getFileItem() {
		return _fileItem;
	}
	
}