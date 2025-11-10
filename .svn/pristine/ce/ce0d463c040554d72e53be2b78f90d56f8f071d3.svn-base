package eim.command.business.service.result;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import common.util.AppConstant;
import common.util.AppObjectUtil;
import eim.bo.EIMFile;
import eim.bo.EIMFormat;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.command.common.util.EIMCommandDateUtil;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.FileUtils;
import eim.util.ObjectUtils;


/**
 * コマンド実行結果格納クラス
 * 
 *
 */
public class EIMCommandResultData extends EIMCommandResult {
	
	//ドキュメントデータ
	private EIMSession sess;
	private EIMObject target;
	private long signed;
	private String kind;
	private String path;
	private long fsize;
	private List<EIMObject> targetList = new ArrayList<EIMObject>();
	private String offset;
	//ファイルデータ
	private String fileName;
	private File file;

	

	public EIMCommandResultData() {
		// TODO 自動生成されたコンストラクター・スタブ
	}
		
	
	public EIMCommandResultData(EIMSession sess){
    	this.sess = sess;
    }

	/**
	 * @return the target
	 */
	public EIMObject getTarget() {
		return target;
	}

	/**
	 * @param target the target to set
	 * @throws Exception
	 */
	public void setTarget(EIMObject target) throws Exception {
		this.target = target;
		this.setEIMObjectOtherInfo();
	}

	/**
	 * @return the singed
	 */
	public long getSigned() {
		return signed;
	}

	/**
	 * @return the kind
	 */
	public String getKind() {
		return kind;
	}
	
	/**
	 * @return the path
	 */
	public String getPath() {
		return path;
	}

	/**
	 * @return the fsize
	 */
	public long getFsize() {
		return fsize;
	}

	/**
	 * @return targetList
	 */
	public List<EIMObject> getTargetList() {
		return targetList;
	}
	
	/**
	 * @param targetList 設定する targetList
	 */
	public void setTargetList(List<EIMObject> targetList) {
		this.targetList = targetList;
	}
	
	/**
	 * @param obj 追加する targetList
	 */
	public void addTargetList(EIMObject obj) {
		this.targetList.add(obj);
	}
	
	/**
	 * @return offset
	 */
	public String getOffset() {
		return offset;
	}
	
	/**
	 * 署名暗号化状態、種別、パス、ファイルサイズを設定する
	 * @throws Exception
	 */
	private void setEIMObjectOtherInfo() throws Exception {

		// 署名・暗号化状態
		this.signed = AppObjectUtil.getIntAttr(this.sess, this.target, EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_STATUS"), AppConstant.SIGNENCR_KIND_NOT_SIGNENCR);

		// 種別（フォルダ・ドキュメント・タグ）
		EIMObjectType tmpType = this.target.getType();
		while(true) {
			EIMObjectType parent = tmpType.getParent();
			if (parent == null) {
				if (tmpType.getName().equals(EIMConfig.getValue("OBJECT_TYPE_NAME_WORKSPACE")))
					this.kind = EIMConfig.getValue("EIM.COMMAND.KIND.FOLDER");
				else if (tmpType.getName().equals(EIMConfig.getValue("OBJECT_TYPE_NAME_DOCUMENT")))
					this.kind = EIMConfig.getValue("EIM.COMMAND.KIND.DOCUMENT");
				else if (tmpType.getName().equals(EIMConfig.getValue("OBJECT_TYPE_NAME_TAG")))
					this.kind = EIMConfig.getValue("EIM.COMMAND.KIND.TAG");
				break;
			}
			tmpType = parent;
		}

		// パス
		this.path = AppObjectUtil.getPath(this.target);

		// ファイルサイズ
		EIMFormat defaultFmt = FileUtils.getDefaultFormat(this.sess, ObjectUtils.getObjectTypeById(this.sess, this.target.getType().getId()));
		if (defaultFmt != null) {
			EIMFile file = FileUtils.getFile(this.sess, this.target, defaultFmt);
			this.fsize = file.getSize();

		}
		
		// オフセット
		if(this.sess != null)
		{
			this.offset = EIMCommandDateUtil.getDBServerOffsetFromSession(this.sess);
		}
	}

	public void setFileNameAndFile(String fileName, File file)
	{
		this.fileName = fileName;
		this.file = file;
	}

	/**
	 * @return file
	 */
	public File getFile() {
		return file;
	}

	/**
	 * @param file 設定する file
	 */
	public void setFile(File file) {
		this.file = file;
	}

	/**
	 * @return fileName
	 */
	public String getFileName() {
		return fileName;
	}

	/**
	 * @param fileName 設定する fileName
	 */
	public void setFileName(String fileName) {
		this.fileName = fileName;
	}

}
