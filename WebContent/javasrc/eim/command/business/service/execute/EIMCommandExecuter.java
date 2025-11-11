package eim.command.business.service.execute;

import java.io.File;
import java.nio.charset.Charset;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import jakarta.servlet.http.HttpServletRequest;

import org.apache.commons.fileupload2.core.DiskFileItem;
import org.apache.commons.fileupload2.core.DiskFileItemFactory;
import org.apache.commons.fileupload2.jakarta.JakartaServletDiskFileUpload;

import eim.bo.EIMObject;
import eim.command.business.service.EIMCommandService;
import eim.command.business.service.result.EIMCommandResult;
import eim.command.common.util.EIMCommandConstant;
import eim.command.common.util.EIMCommandUtil;
import eim.net.EIMSession;
import eim.util.AccessUtils;
import eim.util.EIMConfig;
import eim.util.EIMConstant;
import eim.util.OperationHistoryUtils;

/**
 * コマンド実行クラス
 *
 *
 */
public abstract class EIMCommandExecuter {
    private EIMSession sess;
    private String message;
    private int fileCounter;
    private File[] files;
    private String[] fileNames;
    private String path;
    private String type;
    private String name;
    private String[] names;
    private String user;
    private String pass;
    private List<String> users = new ArrayList<String>();
    private List<String> passwords = new ArrayList<String>();
    private String option;
    private String format;
    private HashMap<String,String> otherParameter = new HashMap<String, String>();

    /**
     * コマンドを実行する
     * @return
     */
    abstract public EIMCommandResult execute() throws Exception;

    /**
     * リクエストパラメータをフィールドに設定する
     * @param request
     */
    @SuppressWarnings("unchecked")
    public void setRequestParameters(HttpServletRequest request) throws Exception{

		// ファイルアップロード時処理
		if(JakartaServletDiskFileUpload.isMultipartContent(request)) {

			//DiskFileItemFactory
			DiskFileItemFactory factory = DiskFileItemFactory.builder()
					.setBufferSize(Integer.parseInt(EIMConfig.get("UPLOAD_FILE_SIZE_10_THRESHOLD"))) // ファイルサイズ（閾値）
					.setPath(EIMConfig.get("TEMP")) // テンポラリフォルダパス
					.get();

			//ServletFileUpload
			JakartaServletDiskFileUpload upload = new JakartaServletDiskFileUpload(factory);
//				upload.setFileSizeMax(Integer.parseInt(EIMConfig.get("UPLOAD_FILE_SIZE_MAX")));

			//Get FileItem
			List<DiskFileItem> fileItemList = new ArrayList<DiskFileItem>();
			List<DiskFileItem> itemList = upload.parseRequest(request);
			for(Object object : itemList) {
				DiskFileItem item = (DiskFileItem)object;
				if(item.isFormField()) {
					this.setParameter(item.getFieldName(), EIMCommandUtil.decode(item.getString(Charset.forName("UTF8"))));
				} else {
					fileItemList.add(item);
				}
			}
			if(fileItemList.size() > 0) {
				// Write File to Disk
				try {
					String[] names = new String[fileItemList.size()];
					File[] files = new File[fileItemList.size()];
					for(int i = 0; i < fileItemList.size(); i++) {
						DiskFileItem fileItem = fileItemList.get(i);
						File file = File.createTempFile("EIM$", ".tmp", new File(EIMConfig.get("TEMP")));
						Path path = file.toPath();
						fileItem.write(path);

						//Delete Temporary FileItem, not Waiting for GC s.imano@2010.11.01
						fileItem.delete();

						names[i] = EIMCommandUtil.decode(fileItem.getName());
						files[i] = file;
						fileCounter++;
					}
					this.setNames(names);
					this.setFiles(files);
				} catch (Exception e) {
					e.printStackTrace();
					throw new Exception();
				}
			}
		} else {
			Map<String, String[]> paramMap = request.getParameterMap();
			if (paramMap != null && !paramMap.isEmpty()) {
				Set<String> paramKeys = paramMap.keySet();
				for (String paramKey : paramKeys) {
					String[] paramValues = paramMap.get(paramKey);
					for (int i = 0; i < paramValues.length; i++) {
						this.setParameter(paramKey, EIMCommandUtil.decode(paramValues[i]));
					}
				}
			}
		}
    }

	public void setParameter(String key, String value) {

		if(key.equals(EIMCommandConstant.USER)) {
			this.users.add(value);
		} else if(key.equals(EIMCommandConstant.PASS)) {
			this.passwords.add(value);
		} else if(key.equals(EIMCommandConstant.PATH)) {
			this.setPath(value);
		} else if(key.equals(EIMCommandConstant.NAME)) {
			this.setName(value);
		} else if(key.equals(EIMCommandConstant.TYPE)) {
			this.setType(value);
		} else if(key.equals(EIMCommandConstant.OPTION)) {
			this.setOption(value);
		} else if(key.equals(EIMCommandConstant.FORMAT)) {
			this.setFormat(value);
		} else {
			this.setOtherParameter(key, value);
		}

	}

	/**
	 * @return the sess
	 */
	public EIMSession getSess() {
		return sess;
	}

	/**
	 * @param sess the sess to set
	 */
	public void setSess(EIMSession sess) {
		this.sess = sess;
	}

	/**
	 * @return the message
	 */
	public String getMessage() {
		return message;
	}

	/**
	 * @param message the message to set
	 */
	public void setMessage(String message) {
		this.message = message;
	}

	/**
	 * @return fileCounter
	 */
	public int getFileCounter(){
		return fileCounter;
	}
	
	/**
	 * @param fileCounter the fileCounter to set
	 */
	public void setFileCounter(int fileCounter){
		this.fileCounter = fileCounter;
	}
	
	/**
	 * @return the files
	 */
	public File[] getFiles() {
		return files;
	}

	/**
	 * @param files the files to set
	 */
	public void setFiles(File[] files) {
		this.files = files;
	}

	/**
	 * @return the fileNames
	 */
	public String[] getFileNames() {
		return fileNames;
	}

	/**
	 * @param fileNames the fileNames to set
	 */
	public void setFileNames(String[] fileNames) {
		this.fileNames = fileNames;
	}

	/**
	 * @return the path
	 */
	public String getPath() {
		return path;
	}

	/**
	 * @param path the path to set
	 */
	public void setPath(String path) {
		this.path = path;
	}

	/**
	 * @return the type
	 */
	public String getType() {
		return type;
	}

	/**
	 * @param type the type to set
	 */
	public void setType(String type) {
		this.type = type;
	}

	/**
	 * @return the name
	 */
	public String getName() {
		return name;
	}

	/**
	 * @param name the name to set
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * @return the names
	 */
	public String[] getNames() {
		return names;
	}

	/**
	 * @param names the names to set
	 */
	public void setNames(String[] names) {
		this.names = names;
	}

	/**
	 * @return the user
	 */
	public String getUser() {
		return user;
	}

	/**
	 * @param user the user to set
	 */
	public void setUser(String user) {
		this.user = user;
	}

	/**
	 * @return the pass
	 */
	public String getPass() {
		return pass;
	}

	/**
	 * @param pass the pass to set
	 */
	public void setPass(String pass) {
		this.pass = pass;
	}

	/**
	 * @return the users
	 */
	public List<String> getUsers(){
		return this.users;
	}

	/**
	 * @return the passwords
	 */
	public List<String> getPasswords(){
		return this.passwords;
	}
	
	/**
	 * @return the option
	 */
	public String getOption() {
		return option;
	}

	/**
	 * @param option the option to set
	 */
	public void setOption(String option) {
		this.option = option;
	}

	/**
	 * @return the format
	 */
	public String getFormat() {
		return format;
	}

	/**
	 * @param format the format to set
	 */
	public void setFormat(String format) {
		this.format = format;
	}


	/**
	 * @return the parameters
	 */
	public String getOtherParameter(String key) {
		return this.otherParameter.get(key);
	}

	/**
	 * @param parameters the parameters to set
	 */
	public void setOtherParameter(String key, String value) {
		this.otherParameter.put(key, value);
	}

	/**
	 * 操作履歴を登録
	 * @param operationTypeNo 操作種別（の番号）
	 * @param targetInfoNo 操作対象情報（の番号）
	 * @param targetObj 操作対象のEIMObject ※なければnull
	 * @throws Exception
	 */
	protected void createOperationHistory(String operationTypeNo, String targetInfoNo, EIMObject targetObj) throws Exception {

		OperationHistoryUtils.create(getSess(), EIMCommandConstant.COMMAND, operationTypeNo, targetInfoNo,
										targetObj!=null ? EIMConstant.OBJECT : null, targetObj, null, null, null,
										EIMCommandService.VERSION + ":" + getPath());
	}

	/**
	 * アクセス履歴を登録
	 * @param object
	 * @param actionKey アクションのリソースキー
	 * @throws Exception
	 */
	protected void createAccessHistory(EIMObject object, String actionKey) throws Exception {

		AccessUtils.createAccess(getSess(), object, actionKey);
	}

}
