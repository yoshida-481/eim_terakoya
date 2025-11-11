package eim.command.common;

import java.util.ArrayList;
import java.util.List;

import eim.bo.EIMObject;


/**
 * WF無しドキュメントの検索結果格納クラス
 * 
 */
public class ResultNoWFStsObjList {

	//WFなしステータス「公開済」
	private List<EIMObject> noWFPubList = new ArrayList<EIMObject>();
	//WFなしステータス「改定中」
	private List<EIMObject> noWFOnRevList = new ArrayList<EIMObject>();
	
	public void setSearchResult(List<EIMObject> pubList, List<EIMObject> onRevList){
		this.noWFPubList = pubList;
		this.noWFOnRevList = onRevList;
	}
	
	public List<EIMObject> getNoWFPubList(){
		return this.noWFPubList;
	}
	public List<EIMObject> getNoWFOnRevList(){
		return this.noWFOnRevList;
	}
}
