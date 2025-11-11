package eim.command.business.service.execute;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import jakarta.servlet.http.HttpServletRequest;

import org.apache.commons.lang3.StringUtils;

import common.util.AppConstant;
import eim.bo.EIMAccessRole;
import eim.bo.EIMAttributeType;
import eim.bo.EIMException;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMResource;
import eim.bo.EIMSearchResultList;
import eim.bo.EIMValueType;
import eim.command.business.service.result.EIMCommandResultDocument;
import eim.command.business.service.result.EIMCommandResultDocumentList;
import eim.command.common.EIMCommandResultDocumentComparetor;
import eim.command.common.EIMCommandSearchTarget;
import eim.command.common.ResultNoWFStsObjList;
import eim.command.common.RootObjTypeData;
import eim.command.common.util.DocumentManagementUtils;
import eim.command.common.util.EIMCommandConstant;
import eim.command.common.util.EIMCommandObjectUtil;
import eim.command.common.util.EIMCommandSearchUtils;
import eim.command.common.util.EIMCommandUtil;
import eim.net.EIMSession;
import eim.util.AttributeUtils;
import eim.util.EIMConfig;
import eim.util.ObjectUtils;

/**
 * findコマンド用コマンド実行実装クラス
 *
 *
 */
public class EIMCommandFindExecuter extends EIMCommandExecuter {

	// 引数
	private Map<String,String> types = new HashMap<String,String>();
	private Map<String,String> names = new HashMap<String,String>();
	private Map<String,String> paths = new HashMap<String,String>();
	private List<String> opts = new ArrayList<String>();
	private List<String> attNames = new ArrayList<String>();
	private List<String> attValues = new ArrayList<String>();


	// オプション
	private String OPTION_L = "l";
	private String OPTION_R = "R";

	// オプションMap
	private Map<String, String> optMap = new HashMap<String, String>();

	// 検索条件データ
	private EIMCommandSearchTarget target = new EIMCommandSearchTarget();

	// 属性タイプ「パス」のID
	private long attrType_id_path;

	// WF無しオブジェクトリスト用変数
	private List<EIMObject> objListForNoWF = new ArrayList<EIMObject>();
	
	// WF有りオブジェクトリスト用変数
	private List<EIMObject> objListForWF = new ArrayList<EIMObject>();

	public EIMCommandFindExecuter() {

	}

	/**
	 * リクエストパラメータをフィールドに設定する
	 * @param request
	 */
	@SuppressWarnings("unchecked")
    public void setRequestParameters(HttpServletRequest request) throws Exception
	{
    	Map<String, String[]> paramMap = request.getParameterMap();
		if (paramMap != null && !paramMap.isEmpty()) {
			Set<String> paramKeys = paramMap.keySet();
			for (String paramKey : paramKeys) {
				String[] paramValues = paramMap.get(paramKey);
				for (int i = 0; i < paramValues.length; i++) {
					this.setGetCommandParameter(paramKey, EIMCommandUtil.decode(paramValues[i]));
				}
			}
		}

	}

	/**
	 * findコマンドの引数を設定する
	 * @param key
	 * @param value
	 */
	public void setGetCommandParameter(String key, String value)
	{
		super.setParameter(key, value);

		if(key.equals(EIMCommandConstant.TYPE))
		{
			types.put(value, null);
		}
		else if(key.equals(EIMCommandConstant.NAME))
		{
			names.put(value, null);
		}
		else if(key.equals(EIMCommandConstant.PATH))
		{
			paths.put(value, null);
		}
		else if(key.equals(EIMCommandConstant.ATTNAME))
		{
			attNames.add(value);
		}
		else if(key.equals(EIMCommandConstant.ATTVALUE))
		{
			attValues.add(value);
		}
		else if(key.equals(EIMCommandConstant.OPTION))
		{
			opts.add(value);
		}
	}

	/**
	 * 処理実行
	 * @throws Exception
	 */
	@SuppressWarnings("unchecked")
    public EIMCommandResultDocumentList execute() throws Exception
    {
    	// 返却値
		EIMCommandResultDocumentList result = new EIMCommandResultDocumentList(super.getSess());


		// 引数解析を行い、正常である場合は値を格納する
		if(!checkAndSetParameter(result))
		{
			// エラーの場合
			return result;
		}

		// 検索条件のセット
		if(!setSearchTarget(result))
		{
			// エラーの場合
			return result;
		}

		// 検索結果に属性を含むか否かを決定
		Boolean includeAttr = false;
		if(optMap.containsKey(OPTION_L))
		{
			includeAttr = true;
			result.setDispAttr(true);
		}

		// 検索の実行
		List<EIMSearchResultList> searchResult = new ArrayList<EIMSearchResultList>();
		try
		{
			
			searchResult = EIMCommandSearchUtils.seachObject(super.getSess(), this.target, includeAttr, EIMAccessRole.READ);
						
		}
		catch(EIMException e){

			// メッセージの整形
			String message = EIMResource.getMessage(super.getSess(), "EIM.ERROR.LOGIC.SEARCH.RESULT.LIMIT.OVER",new Object[]{e.getMessageParams()[0].toString(),e.getMessageParams()[1].toString()} );

			result.setTypeCodeMessage(EIMResource.getMessage(super.getSess(), "EIM.RESULT.TYPE.ERROR"),
					EIMResource.getMessage(super.getSess(), "EIM.ERROR.CODE.SEARCH.RESULT.LIMIT.OVER"),
					message);

			// 返却
			return result;
		}catch(Exception e){
			// その他のエラーの場合はシステムエラー
			
			result.setTypeCodeMessage(EIMResource.getMessage(super.getSess(), "EIM.RESULT.TYPE.ERROR"),
										EIMResource.getMessage(super.getSess(), "EIM.ERROR.CODE.SYSTEM.ERROR"),
										EIMResource.getMessage(super.getSess(), "EIM.ERROR.LOGIC.SYSTEM.ERROR"));
			
			throw e;
		}
		
		//WF無しオブジェクトリストとWF有りオブジェクトリストを分離します
		separateList(searchResult);
		
		// リンクオブジェクトについての処理(WF有り)
		List<EIMCommandResultDocument> resultDocList = new ArrayList<EIMCommandResultDocument>();
		if(objListForWF.size() != 0)
		{
			resultDocList = setDocumentListObj(objListForWF, getSess(), false);
		}
		
		// リンクオブジェクトについての処理(WF無し)
		List<EIMCommandResultDocument> resultDocNoWFList = new ArrayList<EIMCommandResultDocument>();
		if(objListForNoWF.size() != 0)
		{
			resultDocNoWFList = setDocumentListObj(objListForNoWF, getSess(), true);
		}
		
		//ソート用のリストを作成
		List<EIMCommandResultDocument> docListForSort = new ArrayList<EIMCommandResultDocument>();
		if(objListForWF.size() != 0 && objListForNoWF.size() != 0)
		{
			resultDocList.addAll(resultDocNoWFList);
			docListForSort = resultDocList;
			
		}else if(objListForWF.size() == 0 && objListForNoWF.size() != 0){
			
			docListForSort = resultDocNoWFList;
			
		}else if(objListForWF.size() != 0 && objListForNoWF.size() == 0){
			
			docListForSort = resultDocList;
			
		}else {
			//取得件数が0の場合は何もしない
		}
		
		//ソート
		if(docListForSort.size() != 0)
		{
			docListForSort = sortSearchResultByPath(docListForSort);
		}
		
		
		// 返却データのセット
		result.setType(EIMResource.getMessageValue("EIM.RESULT.TYPE.INFO"));
		result.setResultDocList(docListForSort, true);

		// 操作履歴、アクセス履歴は残さない


		return result;
	}

    /**
     * 引数解析を行う
     * @param result
     * @return
     * @throws Exception
     */
    private boolean checkAndSetParameter(EIMCommandResultDocumentList result) throws Exception
    {
		// 引数「opt」に不正な文字列が指定されているかチェックする
    	for(String opt : opts)
    	{
			// option指定された文字列を一文字ずつ切り分ける
			char[] optCharArray = opt.toCharArray();

			for (int i = 0; i < optCharArray.length; i++){
				String tmpOpt = String.valueOf(optCharArray[i]);
	    		if( !(tmpOpt.equals(OPTION_L)) &&  !(tmpOpt.equals(OPTION_R)))
	    		{
	    			result.setTypeCodeMessage(EIMResource.getMessage(super.getSess(), "EIM.RESULT.TYPE.ERROR"),
	    										EIMResource.getMessage(super.getSess(), "EIM.ERROR.CODE.INVALID.OPTION"),
	    										EIMResource.getMessage(super.getSess(), "EIM.ERROR.LOGIC.INVALID.OPTION"));
	    			return false;
	    		}
	    		else
	    		{
	    			// Mapに登録
	    			optMap.put(tmpOpt, tmpOpt);
	    		}
			}
    	}


		// 引数「attName」「attValue」の数が一致するかチェックする

    	if(attNames.size() != attValues.size())
    	{
    		result.setTypeCodeMessage(EIMResource.getMessage(super.getSess(), "EIM.RESULT.TYPE.ERROR"),
										EIMResource.getMessage(super.getSess(), "EIM.ERROR.CODE.INVALID.ATTRNAME.ATTRVALUE"),
										EIMResource.getMessage(super.getSess(), "EIM.ERROR.LOGIC.INVALID.ATTRNAME.ATTRVALUE"));
    		return false;
    	}

    	// 検索条件が「*」だったり、空文字のものをマップから削除
    	deleteAllAsteriskValueFromParamMap(types);
    	deleteAllAsteriskValueFromParamMap(names);
    	deleteAllAsteriskValueFromParamMap(paths);
    	deleteAllAsteriskValueFromAttrs(attNames, attValues);

    	// 検索条件が指定されているかチェックする
    	if(types.size() == 0 && names.size() == 0 && paths.size() == 0 && attNames.size() == 0)
    	{
    		result.setTypeCodeMessage(EIMResource.getMessage(super.getSess(), "EIM.RESULT.TYPE.ERROR"),
										EIMResource.getMessage(super.getSess(), "EIM.ERROR.CODE.NO.SEARCH.CONDITION"),
										EIMResource.getMessage(super.getSess(), "EIM.ERROR.LOGIC.NO.SEARCH.CONDITION"));
    		return false;
    	}

    	return true;
    }

    /**
     * 指定属性から、valueに「*」や空白のみが指定されているものを削除する
     * @param paramValueMap
     * @return
     * @throws Exception
     */
    private void deleteAllAsteriskValueFromAttrs(List<String> attrNames, List<String> attrValues) throws Exception
    {
    	if (attrNames.size() == 0){
    		return;
    	}

    	for (int delIdx = 0; delIdx < attrNames.size(); ){
    		// アスタリスクや空白のみなら、パラメータマップから削除
    		if (!hasNotAsteriskChar(attrValues.get(delIdx))){
    			attrNames.remove(delIdx);
    			attrValues.remove(delIdx);
    		}
    		else{
    			delIdx++;
    		}

    	}
    	return;
    }

    /**
     * パラメータマップから、引数に「*」や空白のみが指定されているものを削除する
     * @param paramValueMap
     * @return
     * @throws Exception
     */
    private void deleteAllAsteriskValueFromParamMap(Map<String,String> paramValueMap) throws Exception
    {
    	if (paramValueMap.size() == 0){
    		return;
    	}

    	Set<String> keySet = paramValueMap.keySet();
    	Iterator<String> keyIte = keySet.iterator();

    	while(keyIte.hasNext()) {
    		String valueStr = keyIte.next();

    		// アスタリスクや空白のみなら、パラメータマップから削除
    		if (!hasNotAsteriskChar(valueStr)){
    			paramValueMap.remove(valueStr);
    		}
    	}

    	return;
    }

    /**
     * 引数文字列にアスタリスク以外が存在するかをチェック
     * @param paramValue
     * @return
     * @throws Exception
     */
    private boolean hasNotAsteriskChar(String paramValue) throws Exception
    {
    	boolean result = false;
    	if (paramValue.length() == 0){
    		return result;
    	}

    	char[] charArray = paramValue.toCharArray();

    	for (int i = 0; i < charArray.length; i++){
    		if (charArray[i] != '*'){
    			result = true;
    			break;
    		}
    	}

    	return result;
    }

    /**
     * 検索条件を整形し、データクラスに格納する
     * @param result
     * @return
     * @throws Exception
     */
    private boolean setSearchTarget(EIMCommandResultDocumentList result) throws Exception
    {
    	// オブジェクトタイプID(objTypeIdに変換して格納)
    	if(!setObjTypeIdToSearchTarget(result))
    	{
    		//エラーの場合
    		return false;
    	}


    	// オブジェクト名称
    	for(String objName : names.keySet())
    	{
    		target.addObjNameList(objName);
    	}


    	// パス
    	for(String path : paths.keySet())
    	{
    		// 末尾がスラッシュなしであれば補完する
    		if (!path.endsWith("/")) {
    			path = path + "/";
    		}
    		
    		target.addPathList(path);
    	}


    	// 属性名、属性値
    	if(!setAttrKeyAndValueToSearchTarget(result))
    	{
    		//エラーの場合
    		return false;
    	}

    	return true;

    }

    /**
     * オブジェクトタイプを検索条件へセットする
     * @param result
     * @return
     * @throws Exception
     */
    private boolean setObjTypeIdToSearchTarget(EIMCommandResultDocumentList result) throws Exception
    {
    	for(String objTypeName : types.keySet())
    	{
    		EIMObjectType objType = ObjectUtils.getObjectTypeByName(super.getSess(), objTypeName);

    		// オブジェクトタイプが存在しない場合
    		if(objType == null)
    		{
    			Object[] param = {objTypeName};
    			result.setTypeCodeMessage(EIMResource.getMessage(super.getSess(), "EIM.RESULT.TYPE.ERROR"),
											EIMResource.getMessage(super.getSess(), "EIM.ERROR.CODE.OBJECTTYPE.NO.EXIST"),
											EIMResource.getMessage(super.getSess(), "EIM.ERROR.LOGIC.OBJECTTYPE.NO.EXIST", param));
    			return false;
    		}
    		else
    		{
    			// 検索条件にオブジェクトタイプIDを追加
    			this.target.addObjTypeIdList((long)objType.getId());

    			// オプション「R」が指定されている場合、子オブジェクトタイプを再帰的に取得
    			if(optMap.containsKey(OPTION_R))
    			{
    				List<EIMObjectType> tmpList = new ArrayList<EIMObjectType>();
    				tmpList.add(objType);
    				ObjectUtils.getChildObjectTypeListRecurrently(super.getSess(), objType, tmpList);

    				// 検索条件に全て追加
    				this.target.addAllObjTypeIdList(getIdListByObjTypeList(tmpList));
    			}
    		}
    	}

    	return true;
    }

    /**
     * オブジェクトタイプのリストから、オブジェクトタイプIDのリストを取得する
     * @param objTypeList
     * @return
     */
    private List<Long> getIdListByObjTypeList(List<EIMObjectType> objTypeList)
    {
    	List<Long> objIdList = new ArrayList<Long>();

    	for(EIMObjectType objType : objTypeList)
    	{
    		objIdList.add((long)objType.getId());
    	}

    	return objIdList;
    }

    /**
     * 属性名、属性値を検索条件にセットする
     * @param result
     * @return
     * @throws Exception
     */
    private boolean setAttrKeyAndValueToSearchTarget(EIMCommandResultDocumentList result) throws Exception
    {
    	// 属性名、属性値についてチェックを行う
    	for(int i = 0; i < attNames.size(); i ++)
    	{
    		String tmpAttName = attNames.get(i);
    		EIMAttributeType attrType = AttributeUtils.getAttributeTypeByName(super.getSess(), tmpAttName);

    		// EIMAttributeTypeが取得できなかった場合
    		if(attrType == null)
    		{
        		result.setTypeCodeMessage(EIMResource.getMessage(super.getSess(), "EIM.RESULT.TYPE.ERROR"),
											EIMResource.getMessage(super.getSess(), "EIM.ERROR.CODE.ATTRIBUTE.NO.EXIST"),
											EIMResource.getMessage(super.getSess(), "EIM.ERROR.LOGIC.ATTRIBUTE.NO.EXIST"));
        		return false;
    		}
    		// 日付型属性であるが、属性値のフォーマットが不正である場合
    		else if(attrType.getValueType().getId() == new EIMValueType(super.getSess(), EIMValueType.DATE).getId()){
    			try{
    				SimpleDateFormat tmpFormat =new SimpleDateFormat(EIMConfig.getValue("EIM.COMMAND.FIND.COMMAND.DATE.FORMAT"));
    				tmpFormat.parse(attValues.get(i));
        			}catch (Exception e){
            			Object[] param = {tmpAttName};
                		result.setTypeCodeMessage(EIMResource.getMessage(super.getSess(), "EIM.RESULT.TYPE.ERROR"),
                									EIMResource.getMessage(super.getSess(), "EIM.ERROR.CODE.INVALID.DATE.FORMAT"),
        											EIMResource.getMessage(super.getSess(), "EIM.ERROR.LOGIC.INVALID.DATE.FORMAT", param));
                		return false;
        			}
        		}
    		// int型属性であるが、属性値の置換が不正である場合
    		else if(attrType.getValueType().getId() == new EIMValueType(super.getSess(), EIMValueType.INTEGER).getId())
    		{
    			try
    			{
    				Integer.parseInt(attValues.get(i));
    			}
    			catch(Exception e)
    			{
    				result.setTypeCodeMessage(EIMResource.getMessage(super.getSess(), "EIM.RESULT.TYPE.ERROR"),
							EIMResource.getMessage(super.getSess(), "EIM.ERROR.CODE.INVALID.NUMERIC.ATTR"),
							EIMResource.getMessage(super.getSess(), "EIM.ERROR.LOGIC.INVALID.NUMERIC.ATTR"));
    				return false;
    			}
    		}
    		// double型属性であるが、属性値の置換が不正である場合
    		else if(attrType.getValueType().getId() == new EIMValueType(super.getSess(), EIMValueType.DOUBLE).getId())
    		{
    			try
    			{
    				Double.parseDouble(attValues.get(i));
    			}
    			catch(Exception e)
    			{
    				result.setTypeCodeMessage(EIMResource.getMessage(super.getSess(), "EIM.RESULT.TYPE.ERROR"),
							EIMResource.getMessage(super.getSess(), "EIM.ERROR.CODE.INVALID.NUMERIC.ATTR"),
							EIMResource.getMessage(super.getSess(), "EIM.ERROR.LOGIC.INVALID.NUMERIC.ATTR"));
    				return false;
    			}
    		}
    		// 不正でない場合はMapに登録
    		setAttrMap(attrType, attValues.get(i));
    	}

    	return true;

    }

    /**
     * 属性マップをセットする(キー：属性名称、バリュー：属性値(データ型へ変換していない値))
     * @param attrType
     * @param attrValue
     */
    @SuppressWarnings("unchecked")
    private void setAttrMap(EIMAttributeType attrType, String attrValue)
    {
    	// 同一のキーが存在しない場合、新しくキーをセット
    	if(!this.target.getAttrMap().containsKey(attrType.getId()))
    	{
    		List<String> tmpValueList = new ArrayList<String>();
    		tmpValueList.add(attrValue);
    		this.target.setAttrMapKeyAndValue((long)attrType.getId(), tmpValueList);
    	}

    	// 同一のキーが存在する場合、すでにあるキーのバリューへ追加
    	else
    	{
    		((List<String>)this.target.getAttrMap().get(attrType.getId())).add(attrValue);
    	}
    }

    /**
     * リンクオブジェクトも結果にセット
     * @param searchResult
     * @param sess
     * @param isNoWF true：引数searchListがWF無しドキュメントリスト、false:WF有りドキュメントリスト
     * @return
     * @throws Exception
     */
    @SuppressWarnings("unchecked")
    private List<EIMCommandResultDocument> setDocumentListObj(List searchResult, EIMSession sess, Boolean isNoWF) throws Exception
    {
    	List<EIMCommandResultDocument> result = new ArrayList<EIMCommandResultDocument>();

    	// オブジェクトタイプ「ドキュメント」を取得する
    	long docTypeId = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT")).getId();
    	
    	
    	// 検索取得したオブジェクトのルートタイプ
    	Long rootTypeId;
    	
    	// 子オブジェクトタイプIDとルートオブジェクトタイプのマップ情報
    	// (key：子オブジェクトタイプID、 value：ルートオブジェクトタイプ)
    	Map<Long, Long> childTypeId_rootType_map = new HashMap<Long, Long>();
    	RootObjTypeData rootObjTypeData = new RootObjTypeData(sess);
    	
    	
    	//WF無しステータス判定用にオブジェクトを取得
    	ResultNoWFStsObjList noWFStsObjList = new ResultNoWFStsObjList();
    	if(isNoWF){
    		noWFStsObjList = EIMCommandSearchUtils.searchObjForNoWFSts(sess, searchResult, Integer.parseInt(EIMConfig.get("GET_DATA_MAX_NUM")));
    	}
    	
    	for(EIMObject obj : (List<EIMObject>) searchResult) {
    		
			Long targetTypeId = new Long(obj.getType().getId());
			
			if (childTypeId_rootType_map.containsKey(targetTypeId)) {
				
				rootTypeId = childTypeId_rootType_map.get(targetTypeId);
				
			} else {
				
				// 対象オブジェクトのルートオブジェクトタイプを取得
				rootTypeId = new Long(EIMCommandObjectUtil.getRootObjType(sess, obj.getId()).getId());
				childTypeId_rootType_map.put(targetTypeId, rootTypeId);
				rootObjTypeData.setKeyValue2Map(targetTypeId, rootTypeId);
			}
			
			// ドキュメントリンクを保持する場合
    		// (オブジェクトタイプがドキュメントで、且つ属性「ドキュメントリンク」が「1」の場合)
			if ((rootTypeId == docTypeId)
					&& (obj.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_DOCUMENT_LINK")) != null)) {
				//オブジェクトのパス属性を取得する
				String[] tmpPaths = obj.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_PASS")).getStrings();
	    		//自身のオブジェクトが検索条件の時は当該オブジェクト自身も結果に格納する
				if (isCheckHitPath(tmpPaths[0])) {
					// 最新ではない場合は結果に含めない
					if (obj.getLatest()) {
						// obj自身を結果にセットする
						EIMCommandResultDocument resultDoc = 
							new EIMCommandResultDocument(super.getSess());
						resultDoc.setTarget(obj);
						resultDoc.setRootObjTypeData(rootObjTypeData);
						resultDoc.judgeNoWFStatus(noWFStsObjList.getNoWFOnRevList(), noWFStsObjList.getNoWFPubList());
						resultDoc.setIsLinked(AppConstant.FLAG_OFF);
						result.add(resultDoc);
					}
				}
				for (int i = 1; i < tmpPaths.length; i++) {

					// ドキュメントリンクの対象パスが検索条件に含まれている場合
					if (isCheckHitPath(tmpPaths[i])) {
						String tmpPathForVersion = tmpPaths[i];
						// パスの最後に'/'がついていた場合
						DocumentManagementUtils dmu = new DocumentManagementUtils(sess);
						if (tmpPathForVersion.endsWith("/")) {
							tmpPathForVersion = tmpPathForVersion.substring(0, tmpPathForVersion.length() - 1);
						}
						// 親オブジェクトを取得するためのディレクトリとパスを設定する
						String directory = null;
						String name = null;
						if (tmpPathForVersion.lastIndexOf("/") > 0) {
							String tmp = tmpPathForVersion.substring(0, tmpPathForVersion.lastIndexOf("/"));
							directory = tmp.substring(0,tmp.lastIndexOf("/") + 1);
							name = tmp.substring(tmp.lastIndexOf("/") + 1);
							} else {
								name = tmpPathForVersion;
							}
							EIMObject parentObj = dmu.getObjectByPathAndName(directory, name);

						if (parentObj == null) {
							// 親オブジェクトが取得できない場合は検索結果に含めない(論理削除された場合など)
							continue;
						} else {
							// 結果にセットする
							EIMCommandResultDocument resultDoc = 
								new EIMCommandResultDocument(super.getSess());
							resultDoc.setTarget(obj, tmpPaths[i]);
							resultDoc.setRootObjTypeData(rootObjTypeData);
							resultDoc.judgeNoWFStatus(noWFStsObjList.getNoWFOnRevList(), noWFStsObjList.getNoWFPubList());
							resultDoc.setIsLinked(AppConstant.FLAG_ON);
							result.add(resultDoc);
						}
					}
				}
			} else {
				// 最新ではない場合検索結果に含めない
				if(obj.getLatest())
				{
					// 自身のオブジェクトが検索条件の時は当該オブジェクト自身も結果に格納する
					// obj自身を結果にセットする
					EIMCommandResultDocument resultDoc = 
						new EIMCommandResultDocument(super.getSess());
					resultDoc.setTarget(obj);
					resultDoc.setRootObjTypeData(rootObjTypeData);
					resultDoc.judgeNoWFStatus(noWFStsObjList.getNoWFOnRevList(), noWFStsObjList.getNoWFPubList());
					resultDoc.setIsLinked(AppConstant.FLAG_OFF);
					result.add(resultDoc);
				}
			}
		}

    	return result;
    }

    /**
     * パス検索条件、パス属性検索条件で、パスがhitするかチェックする
     * パス属性検索条件は、ワイルドカード使用可能
     * @param path:検索オブジェクトのパス searchPaths:パス検索条件のパス target:パス属性のリスト
     * @return
     */
    @SuppressWarnings("unchecked")
    private boolean isCheckHitPath(String path) throws Exception{

    	if (paths.size() == 0){
    		return true;
    	}

    	if (attrType_id_path == 0) {
    		// 属性タイプ「パス」のIDを設定
    		attrType_id_path = AttributeUtils.getAttributeTypeByName(getSess(),EIMConfig.getValue("ATTR_NAME_DOCUMENT_PASS")).getId();
    	}
    	
		//属性検索条件入力時
    	for (long searchPath : target.getAttrMap().keySet()) {
    		
    		//属性がパスのvalueのリストを全検索
    		if (searchPath == attrType_id_path) {
				for (int i = 0; i < ((List<String>) target.getAttrMap().get(
						searchPath)).size(); i++) {
					String tmpString = ((List<String>) target.getAttrMap().get(
							searchPath)).get(i);
					if (hitSearchConditionPath(path, tmpString))
						return true;
				}
			}
		}
    	//パス検索条件入力時
    	for (String searchPath : paths.keySet()) {
			if (hitSearchConditionPath(path, searchPath))
				return true;
		}
		return false;
	}
    private boolean hitSearchConditionPath(String path, String conditionPath) {

		int index = conditionPath.indexOf('*');
		conditionPath = StringUtils.strip(conditionPath, "*");
		int stringNum = path.indexOf(conditionPath);
		if (index == 0) {
		//前方曖昧検索時
			if (stringNum + conditionPath.length() == path.length()) {
				return true;
			}
		}else if(index > 0) {
		//後方曖昧検索時
			if (path.indexOf(conditionPath) != -1) {
				return true;
			}
		}else if(conditionPath.equals(path)){
			//属性入力でワイルドカードで入力してなく、検索条件に合っている時
			return true;
		}
		return false;
    }


    /**
     * パス属性でソートを行う
     * パス属性を持たない場合、IDでソートする
     * @param searchResult
     * @return
     */
    @SuppressWarnings("unchecked")
    private List sortSearchResultByPath(List<EIMCommandResultDocument> searchResult)
    {
    	EIMCommandResultDocumentComparetor comp = new EIMCommandResultDocumentComparetor();
    	Object[] obj = searchResult.toArray();
    	Arrays.sort(obj, comp);

    	return Arrays.asList(obj);

    }

    /**
     * WF有りオブジェクトとWF無しオブジェクト
     * を分離します
     * @param 
     */
    @SuppressWarnings("unchecked")
    private void separateList(List searchResult)
    {
    	for(EIMObject obj: (List<EIMObject>) searchResult)
    	{
    		if(obj.getStatus() == null)
    		{
    			objListForNoWF.add(obj);
    		}else
    		{
    			objListForWF.add(obj);
    		}
    	}
    	return;
    }

}
