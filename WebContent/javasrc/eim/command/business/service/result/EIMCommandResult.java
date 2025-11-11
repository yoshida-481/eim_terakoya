package eim.command.business.service.result;

import java.io.File;

import eim.bo.EIMObject;

/**
 * コマンド実行結果格納クラス
 * 
 *
 */
public class EIMCommandResult {
    private String type;
    private String code;
    private String message;
    private EIMObject[] data;
    private File[] files;
    
    /**
     * 結果のタイプ、コード、メッセージを設定する
     * @param type
     * @param code
     * @param message
     */
	public void setTypeCodeMessage(String type, String code, String message) {
		this.type			= type;
		this.code			= code;
		this.message	= message;
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
	 * @return the code
	 */
	public String getCode() {
		return code;
	}
	/**
	 * @param code the code to set
	 */
	public void setCode(String code) {
		this.code = code;
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
	 * @return the data
	 */
	public EIMObject[] getData() {
		return data;
	}
	/**
	 * @param data the data to set
	 */
	public void setData(EIMObject[] data) {
		this.data = data;
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
}
