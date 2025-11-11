package eim.command.business.service.result;


import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import common.util.AppConstant;
import common.util.AppObjectUtil;
import eim.bo.EIMFile;
import eim.bo.EIMFormat;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.command.common.RootObjTypeData;
import eim.command.common.util.DocumentManagementUtils;
import eim.command.common.util.EIMCommandConstant;
import eim.command.common.util.EIMCommandDateUtil;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.FileUtils;
import eim.util.ObjectUtils;

/**
 * 処理対象ドキュメント時コマンド実行結果格納クラス
 *
 *
 */
public class EIMCommandResultDocument extends EIMCommandResult {

	private EIMSession sess;
	private EIMObject target;
	private long signed;
	private String kind;
	private String path;
	private long fsize;
	private int isLinked;
	private int noWFStatus = -1;
	private String offset;
	private static Map<Long, EIMFormat> typeId_eimFormat_map = new HashMap<Long, EIMFormat>();
	private RootObjTypeData rootObjTypeData = null;
	

	public EIMCommandResultDocument(EIMSession sess){
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
		this.setEIMObjectOtherInfo(null);
	}

	/**
	 * @param target the target to set
	 * @param path
	 * @throws Exception
	 */
	public void setTarget(EIMObject target, String path) throws Exception {
		this.target = target;
		this.setEIMObjectOtherInfo(path);
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
	 * @param path the path to set
	 */
	public void setPath(String path){
		this.path = path;
	}
	
	/**
	 * @return the fsize
	 */
	public long getFsize() {
		return fsize;
	}

	/**
	 * @return isLinked
	 */
	public int getIsLinked() {
		return isLinked;
	}

	/**
	 * @param isLinked 設定する isLinked
	 */
	public void setIsLinked(int isLinked) {
		this.isLinked = isLinked;
	}

	/**
	 * @return the noWFStatus
	 */
	public int getNoWFStatus() {
		return noWFStatus;
	}

	/**
	 * @param noWFStatus the noWFStatus to set
	 */
	public void setNoWFStatus(int noWFStatus) {
		this.noWFStatus = noWFStatus;
	}
	
	/**
	 * @return offset
	 */
	public String getOffset() {
		return offset;
	}

	/**
	 * @return the rootObjTypeData
	 */
	public RootObjTypeData getRootObjTypeData() {
		return rootObjTypeData;
	}

	/**
	 * @param rootObjTypeData the rootObjTypeData to set
	 */
	public void setRootObjTypeData(RootObjTypeData rootObjTypeData) {
		this.rootObjTypeData = rootObjTypeData;
	}

	/**
	 * 以下の項目を設定する
	 *  ・署名暗号化状態
	 *  ・種別
	 *  ・パス
	 *  ・ファイルサイズ
	 *  ・オフセット(例：+09:00)
	 * @param path
	 * @param objList
	 * @throws Exception
	 */
	private void setEIMObjectOtherInfo(String _path) throws Exception {

		if(rootObjTypeData == null)
		{
			// ここでのオブジェクト取得はパフォーマンス劣化に影響を与えていますが、
			// 修正範囲が大きいため、findコマンド以外の処理では現状のままにします。
			this.target = ObjectUtils.getObjectById(sess, this.target.getId());
		}else{
			//findコマンドのkindの設定
			//ワークスペースオブジェクトならば種別を設定しない
			Map<Long, Long> map = rootObjTypeData.getChildTypeId_rootType_map();
			if(this.target.getType().getId() != rootObjTypeData.getWorkSpaceTypeId())
			{
				if(map.get(this.target.getType().getId()) == rootObjTypeData.getWorkSpaceTypeId())
				{
					this.kind = EIMConfig.getValue("EIM.COMMAND.KIND.FOLDER");
				}else if(map.get(this.target.getType().getId()) == rootObjTypeData.getDocTypeId())
				{
					this.kind = EIMConfig.getValue("EIM.COMMAND.KIND.DOCUMENT");
				}else if(map.get(this.target.getType().getId()) == rootObjTypeData.getTagTypeId())
				{
					this.kind = EIMConfig.getValue("EIM.COMMAND.KIND.TAG");
				}
			}
		}
		
		// 署名・暗号化状態
		this.signed = AppObjectUtil.getIntAttr(this.sess, this.target, EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_STATUS"), AppConstant.SIGNENCR_KIND_NOT_SIGNENCR);

		// 種別（フォルダ・ドキュメント・タグ）
		EIMObjectType tmpType = this.target.getType();
		
		//find以外のコマンドのkindの設定。本来はfindコマンドと同様にするべき。
		//ワークスペースオブジェクトならば種別を設定しない
		if(!tmpType.getDefName().equals(EIMConfig.getValue("OBJECT_TYPE_NAME_WORKSPACE"))){
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
		}
		// パス
		if(_path == null)
		{
			this.path = AppObjectUtil.getPath(this.target);
		}
		else
		{
			this.path = _path;
		}
		
		// オブジェクトから属性「サイズ」を取得
		this.fsize = AppObjectUtil.getIntAttr(this.sess, this.target, EIMConfig.get("ATTR_NAME_DOCUMENT_SIZE"), 0);
		
		// ファイルサイズ
		if (this.fsize == 0) {
			
			EIMFormat defaultFmt;
			
			Long targetTypeId = new Long(this.target.getType().getId());
			
			if (typeId_eimFormat_map.containsKey(targetTypeId)) {
				
				defaultFmt = typeId_eimFormat_map.get(targetTypeId);
				
			} else {
				
				defaultFmt = FileUtils.getDefaultFormat(this.sess, ObjectUtils.getObjectTypeById(this.sess, targetTypeId));
				typeId_eimFormat_map.put(targetTypeId, defaultFmt);
			}
		
			if (defaultFmt != null) {
				EIMFile file = FileUtils.getFile(this.sess, this.target, defaultFmt);
				if(file != null){
					this.fsize = file.getSize();
				}
			}
		}

		// オフセット
		if(this.sess != null)
		{
			this.offset = EIMCommandDateUtil.getDBServerOffsetFromSession(this.sess);
		}

	}
	
	/**
	 * WF無しドキュメントのステータスを判定し、
	 * 処理結果を格納します
	 * @param noWFOnRevisionList WF無しステータス「改定中」のドキュメント
	 * @param noWFOnRevisionList WF無しステータス「公開済」のドキュメント
	 * ステータス：「0」=編集中、
	 *             「1」=改定中、
	 *             「2」=公開済
	 */
	public void judgeNoWFStatus(List<EIMObject> noWFOnRevisionList, List<EIMObject> noWFPubList) throws Exception
	{
		// WF無しドキュメントの場合のみ処理を行う
		if(this.target.getStatus() != null)
		{
			return;
		}
		
		DocumentManagementUtils dmu = new DocumentManagementUtils(this.sess);
		EIMObjectType objType = this.target.getType();
		// オブジェクトタイプがドキュメントではない場合は判定しない
		if(dmu.isFolderTypes(objType) || dmu.isTagTypes(objType))
		{
			return;
		}
		// 対象のオブジェクトIDがどのステータスか判定します
		for(EIMObject obj : (List<EIMObject>) noWFOnRevisionList)
		{
			// WF無しステータス「改定中」
			if(obj.getId() == this.target.getId())
			{
				noWFStatus = EIMCommandConstant.NOWF_ON_REVISION;
				return;
			}
		}

		if(noWFStatus == -1){
			for(EIMObject obj : (List<EIMObject>) noWFPubList)
			{
				// WF無しステータス「公開済」
				if(obj.getId() == this.target.getId())
				{
					noWFStatus = EIMCommandConstant.NOWF_PUBLIC;
					return;
				}
			}
		}
		// noWFStatusが未選択(-1)なら、WF無しステータスは「編集中」
		if(noWFStatus == -1)
		{
			noWFStatus = EIMCommandConstant.NOWF_EDIT;
		}
	}

	
	//ソートメソッド用
	public Date getModifyDateOfTarget()
	{
		return this.target.getModifyDate();
	}
	//ソートメソッド用
	public String getNameOfTarget()
	{
		return this.target.getName();
	}


}
