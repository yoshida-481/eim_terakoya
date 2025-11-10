package eim.command.business.service.result;

import java.util.ArrayList;
import java.util.List;


import eim.bo.EIMObject;
import eim.command.common.util.EIMCommandDateUtil;
import eim.net.EIMSession;


/**
 * 処理対象ドキュメント時コマンド実行結果複数値格納クラス
 * 
 *
 */
public class EIMCommandResultDocumentList extends EIMCommandResult {
	
	private EIMSession sess;
	private boolean isDispAttr = false;
	private boolean isDispLink = false;
	private String offset;
	private List<EIMCommandResultDocument> targetList = new ArrayList<EIMCommandResultDocument>();
	
	/**
	 * コンストラクタ
	 * @param _sess
	 */
	public EIMCommandResultDocumentList(EIMSession _sess) {
		this.sess = _sess;
	}

	/**
	 * @return resultDocList
	 */
	public List<EIMCommandResultDocument> getTargetList() {
		return targetList;
	}

	/**
	 * @return sess
	 */
	public EIMSession getSess() {
		return sess;
	}
	
	/**
	 * @return resultDocList
	 */
	public List<EIMCommandResultDocument> getEIMCommandResultDoc() {
		return this.targetList;
	}

	/**
	 * @return isDispAttr
	 */
	public boolean getisDispAttr() {
		return isDispAttr;
	}

	/**
	 * @return offset
	 */
	public String getOffset() {
		return offset;
	}

	/**
	 * @return isDispLink
	 */
	public boolean getisDispLink() {
		return isDispLink;
	}

	/**
	 * @param resultDocList 設定する resultDocList
	 */
	public void setResultDocList(List<EIMCommandResultDocument> resultDocList, boolean dspLink) {
		this.targetList = resultDocList;
		this.offset = EIMCommandDateUtil.getDBServerOffsetFromSession(this.sess);
		
		// linkタグ表示
		if(dspLink)
		{
			this.isDispLink = true;
		}
	}
	
	/**
	 * 
	 * @param objList 設定する objList
	 * @throws Exception
	 */
	public void setResultDocListByObjList(List<EIMObject> objList, boolean dspLink) throws Exception{
		
		for(EIMObject obj : objList)
		{
			EIMCommandResultDocument resultDoc = new EIMCommandResultDocument(this.sess);
			resultDoc.setTarget(obj);
			
			// リストに追加
			this.targetList.add(resultDoc);
		}
		
		// offset
		this.offset = EIMCommandDateUtil.getDBServerOffsetFromSession(this.sess);
		
		// linkタグ表示
		if(dspLink)
		{
			this.isDispLink = true;
		}
	}

	/**
	 * @param isDispAttr 設定する isDispAttr
	 */
	public void setDispAttr(boolean isDispAttr) {
		this.isDispAttr = isDispAttr;
	}
	
	/**
	 * @param offset 設定する offset
	 */
	public void setOffset(String offset){
		this.offset = offset;
	}
	
}
