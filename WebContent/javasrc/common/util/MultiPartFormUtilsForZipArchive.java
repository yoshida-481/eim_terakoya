package common.util;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import jakarta.servlet.http.HttpServletRequest;

import org.apache.commons.fileupload2.core.DiskFileItem;
import org.apache.commons.fileupload2.core.DiskFileItemFactory;
import org.apache.commons.fileupload2.core.FileItem;
import org.apache.commons.fileupload2.jakarta.JakartaServletDiskFileUpload;

import eim.bo.EIMException;
import eim.bo.EIMResource;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.EIMUtils;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;

/**
 * 
 * ZIPアーカイブファイルアップロードクラス
 * （MultiPartFormUtilsを継承しようとしたが、クラス変数がprivate定義されているため
 * 結局自前で作成しないといけない）
 * 
 */
public class MultiPartFormUtilsForZipArchive{
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
	 */
	public MultiPartFormUtilsForZipArchive(HttpServletRequest req) throws Exception {
		_paramField = new ArrayList();
		_paramValue = new ArrayList();
		try {
			DiskFileItemFactory factory = DiskFileItemFactory.builder()
					.setBufferSize(Integer.parseInt(EIMConfig.get("UPLOAD_FILE_SIZE_10_THRESHOLD"))) // ファイルサイズ（閾値）
					.setPath(EIMConfig.get("TEMP")) // テンポラリフォルダパス
					.get();
			JakartaServletDiskFileUpload fu = new JakartaServletDiskFileUpload(factory);
			fu.setSizeMax(-1); // ファイルサイズ（最大値）（上限無し ファイルサイズによる制限は呼び出し元で設定）

			List fileItems = fu.parseRequest(req);
			Iterator iter = fileItems.iterator();

			while (iter.hasNext()) {
				DiskFileItem item = (DiskFileItem) iter.next();
				if (item.isFormField()) {
					_paramField.add(item.getFieldName());
					_paramValue.add(item.getString(Charset.forName("UTF8")));
				} else {
					if (_fileItem == null) {
						_fileItem = item;
					}
				}
			}
		}catch (NumberFormatException nfe) {
			EIMSession sess =  EIMUtils.getSession(req);
			//設定ファイルの「UPLOAD_FILE_SIZE_MAX」もしくは「UPLOAD_FILE_SIZE_THRESHOLD」の値が不正です。
			throw new EIMException(sess, "EIM.ERROR.LOGIC.UPLOAD.FILE.SIZE.INVALID");
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
	 * 
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
	 * 指定されたパスにファイルを移動します。
	 * 
	 * @param filePath
	 *            ファイルのパス
	 * @throws Exception
	 *             予期せぬ例外が発生した場合
	 */
	public void move(String filePath) throws Exception {
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
	 * ↓↓↓ダイハツ専用↓↓↓
	 * ファイル登録の可否を判定します
	 * @param sess EIMSession
	 * @param filePath ファイルのパス
	 * @param prmExtType 判定対象ファイルの拡張子
	 * 
	 * @return ファイル判定結果文字列
	 * @throws Exception 予期せぬ例外が発生した場合
	 * 
	 */
	public String checkBeforeFileUpload(EIMSession sess, String filePath, String prmExtType) throws Exception {
		
		// 登録を許可するファイル拡張子と判定ファイルの存在パスを設定ファイルから取得
		String allowFileExtAndPath = ConfigUtils.getByKey("ALLOW_FILE_EXT_AND_PATH");
		if(allowFileExtAndPath == null || allowFileExtAndPath.equals("")) {
			return EIMResource.getMessage(sess,"EIM.ERROR.EXTENSION.UNDEFINED");
		}
		
		//「登録許可拡張子：判定ファイル」の単位に文字列を分解
		String[] tempExtAndPaths = allowFileExtAndPath.split(",");
		
		// 「登録許可拡張子：判定ファイル」をMapに格納
		Map<String, String> extAndPathMap = new HashMap<String, String>();
		for(int i = 0; i < tempExtAndPaths.length; i++) {
			String[] tempString = tempExtAndPaths[i].split(":");
			extAndPathMap.put(tempString[0], tempString[1]);
		}
		
		// 引数の拡張子に合致するOffice存在チェックファイル指定が存在しない場合、エラーメッセージを返却
		String pathString = extAndPathMap.get(prmExtType);
		if(pathString == null || pathString.equals("")) {
			return EIMResource.getMessage(sess,"EIM.ERROR.EXTENSION.RESTRICTED");
		}
		
		// 解凍用のフォルダ階層作成
		Calendar c = Calendar.getInstance();
		File folder = new File(ConfigUtils.getByKey("TEMP") + ConfigUtils.getByKey("UNZIP_FOLDER_NAME") + sdf.format(c.getTime()));
		folder.mkdirs();
		
		// ZIPファイルのオープン
		InputStream  fileIn  = null;
		OutputStream fileOut = null;
		
		boolean isExistOfficeFile = false;
		// 登録対象ファイルを解凍し、判定ファイルの存在をチェックする
		try {
			try{
				fileIn = new FileInputStream(filePath);
			}catch(FileNotFoundException fe){
				return EIMResource.getMessage(sess,"EIM.ERROR.EXTENSION.RESTRICTED");
			}
			ZipInputStream zipIn = new ZipInputStream(fileIn);
			
			ZipEntry entry = null;
			while((entry = (ZipEntry)zipIn.getNextEntry()) != null) {
				if(entry.isDirectory()) {
					//------------------------------
					// ディレクトリだった場合は、
					// 出力先ディレクトリを作成する
					//------------------------------
					String relativePath = entry.getName();
					folder = new File(folder, relativePath);
					folder.mkdirs();
				} else {
					//------------------------------
					// ファイルの場合は出力する
					// 出力先は、現在の outDirの下
					//------------------------------
					String relativePath = entry.getName();
					File outFile = new File(folder, relativePath);
					
					// 判定ファイルが存在する場合、フラグをtrueにして解凍終了
					if(relativePath.equals(pathString)) {
						isExistOfficeFile = true;
						break;
					}
					
					// 出力先のディレクトリを作成する
					File parentFile = outFile.getParentFile();
					parentFile.mkdirs();
					
					// ファイルを出力する
					fileOut = new FileOutputStream(outFile);
					
					byte[] buf = new byte[1024];
					int size = 0;
					while((size = zipIn.read(buf)) > 0){
						fileOut.write(buf, 0, size);
						}
					fileOut.close();
					fileOut = null;
					}
				zipIn.closeEntry();
			}

			// 必須ファイルが存在しなかった場合、エラーメッセージを返却
			if(!isExistOfficeFile) {
				return EIMResource.getMessage(sess,"EIM.ERROR.EXTENSION.UNEXCEPTED");
			}
			
			return "";
			
		} catch(Exception e) {
			throw e;
		} finally {
			if(folder != null ){
				try {
					// 解凍したフォルダを再帰的に削除
					this.deleteFile(folder);
				}catch(Exception e) {
					throw e;
				}
				
			}
			if(fileIn != null){
				try {
					fileIn.close();
				} catch(Exception e) {
					throw e;
				}
			}
			if(fileOut != null) {
				try {
					fileOut.close();
				} catch(Exception e) {
					throw e;
				}
			}
		}
	}
	
	// ↑↑↑ダイハツ専用↑↑↑
	
	/**
	 * ファイルまたはディレクトリを削除する
	 * 
	 * @param targetFile 削除対象のファイルまたはディレクトリ
	 * @throws Exception 予期せぬ例外
	 */
	private void deleteFile(File targetFile) throws Exception {
		
		// 削除対象が存在しない場合、処理終了
		if(!targetFile.exists()) {
			return;
		}
		
		try {
			// ファイルの場合、削除
			if(targetFile.isFile()) {
				targetFile.delete();
				
			}
			// ディレクトリの場合、再帰的に削除
			else if(targetFile.isDirectory()) {
				File[] files = targetFile.listFiles();
				for(int i = 0; i < files.length; i++) {
					deleteFile(files[i]);
				}
			}
			
			// 自ディレクトリを削除
			targetFile.delete();
			
		} catch (Exception e) {
			throw e;
		}
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
	 * 
	 */
	public long getFileSize() {
		if (_fileItem == null) {
			return 0;
		}

		return _fileItem.getSize();
	}
	
	/**
	 * ファイル名を取得します。
	 * @return
	 */
	public String getFileName() 
	{
		return _fileItem.getName() == null ? "" : _fileItem.getName();
	}
}