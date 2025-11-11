package eim.command.business.service.execute;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import common.util.AppConstant;
import common.util.AppObjectConditionHelper;
import common.util.AppObjectUtil;

import eim.bo.EIMAccessRole;
import eim.bo.EIMAttribute;
import eim.bo.EIMException;
import eim.bo.EIMObject;
import eim.bo.EIMResource;
import eim.bo.EIMSearchLimitCountCondition;
import eim.command.business.service.EIMCommandService;
import eim.command.business.service.result.EIMCommandResult;
import eim.command.business.service.result.EIMCommandResultDocument;
import eim.command.business.service.result.EIMCommandResultDocumentList;
import eim.command.common.ResultNoWFStsObjList;
import eim.command.common.util.DocumentManagementUtils;
import eim.command.common.util.EIMCommandConstant;
import eim.command.common.util.EIMCommandDateUtil;
import eim.command.common.util.EIMCommandSearchUtils;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.EIMConstant;
import eim.util.OperationHistoryUtils;
import eim.util.SecurityUtils;

/**
 * lsコマンド用コマンド実行実装クラス
 *
 *
 */
public class EIMCommandLsExecuter extends EIMCommandExecuter {

	// 引数
	private List<String> tmpPaths = new ArrayList<String>();
	private List<String> tmpOptions = new ArrayList<String>();

	// オプション
	private final String OPTION_A = "a";
	private final String OPTION_T = "t";
	private final String OPTION_R = "r";
	private final String OPTION_L = "l";

	// オプションMap
	private Map<String, String> optMap = new HashMap<String, String>();

	// ヘルパー
	DocumentManagementUtils dmu = null;

	// WF無しオブジェクトリスト用変数
	private List<EIMObject> objListForNoWF = new ArrayList<EIMObject>();
	
	// WF有りオブジェクトリスト用変数
	private List<EIMObject> objListForWF = new ArrayList<EIMObject>();

    public EIMCommandLsExecuter(){
    }

    /* (非 Javadoc)
	 * @see command.business.service.execute.EIMCommandExecuter#setParameter(java.lang.String, java.lang.String)
	 */
	@Override
	public void setParameter(String key, String value) {

		if(key.equals(EIMCommandConstant.PATH)) {
			super.setPath(value);
			this.tmpPaths.add(value);
		} else if(key.equals(EIMCommandConstant.OPTION)) {
			super.setOption(value);
			this.tmpOptions.add(value);
		} else {
			this.setOtherParameter(key, value);
		}

		super.setParameter(key, value);
	}

	/**
	 * 処理実行
	 */
	@SuppressWarnings("unchecked")
	public EIMCommandResult execute() throws Exception
	{
		// セッション
		EIMSession sess = super.getSess();

		// 返却値
		EIMCommandResultDocumentList resultList = new EIMCommandResultDocumentList(sess);

		// パラメータ解析
		if(!this.checkParameter(resultList))
		{
			return resultList;
		}

		// ヘルパー
		this.dmu = new DocumentManagementUtils(sess);

		// パスの整形
		String path = super.getPath();
		if(path != null && path.endsWith("/"))
		{
			// 指定されたパス文字列の最後が「/」の場合は、除去
			path = path.substring(0, path.length() - 1);
		}

		EIMObject obj = null;

		// パスに、トップを指定した場合
		if(path == null || path.equals(""))
		{
			// オブジェクトタイプ「ワークスペース」のEIMObjectのリストを取得する
			List<EIMObject> childObjList = dmu.getWorkSpaceList();

			// 返却データにセット
			setAndSortResultData(resultList, childObjList);

		}
		// パスに、トップ以外を指定した場合
		else
		{
			obj = getChildObjList(resultList, path);
		}

		// 属性表示の有無をセット
		boolean isDspAttr = false;
		if(optMap.containsKey(OPTION_L))
		{
			isDspAttr = true;
		}
		resultList.setDispAttr(isDspAttr);

		// 操作履歴(パスにトップを指定した場合、そのパスに合致するEIMObjectが存在しないので、表示されない)
		OperationHistoryUtils.create(sess, EIMCommandConstant.COMMAND, EIMCommandConstant.LIST_EXCOMMAND,
										EIMCommandConstant.TARGET_TO_LIST, EIMConstant.OBJECT, obj, null, null, null,
										EIMCommandService.VERSION + ":" + path);

		return resultList;
	}

	/**
	 * パラメータチェック
	 * @param resultData
	 * @return
	 * @throws EIMException
	 */
	private boolean checkParameter(EIMCommandResultDocumentList resultData) throws EIMException
	{
		// パスの指定はあるかチェック
		if (this.tmpPaths.size() == 0)
		{
			setRequiredError(resultData, EIMCommandConstant.PATH);
			return false;
		}

		// 単一指定パラメータの重複チェック
		if (this.tmpPaths.size() > 1)
		{
			setDuplicatedError(resultData, EIMCommandConstant.PATH);
			return false;
		}

		// 引数「opt」に不正な文字列が指定されているかチェックする
		for(String opt : tmpOptions)
    	{
			// option指定された文字列を一文字ずつ切り分ける
			char[] optCharArray = opt.toCharArray();

			for (int i = 0; i < optCharArray.length; i++){
				String tmpOpt = String.valueOf(optCharArray[i]);
	    		if( !(tmpOpt.equals(OPTION_A)) &&  !(tmpOpt.equals(OPTION_R)) && !(tmpOpt.equals(OPTION_T)) && !(tmpOpt.equals(OPTION_L)))
	    		{
	    			setInvalidOptionError(resultData);
	    			return false;
	    		}
	    		else
	    		{
	    			// Mapに登録
	    			optMap.put(tmpOpt, tmpOpt);
	    		}
			}


    	}
		return true;
	}

	/**
	 * EIMオブジェクトリストをソートし、返却データクラスにセットする
	 * @param resultData
	 * @param objList
	 * @return
	 * @throws EIMException
	 */
	@SuppressWarnings("unchecked")
	private EIMCommandResultDocumentList setAndSortResultData(EIMCommandResultDocumentList resultList, List<EIMObject> objList) throws Exception {

		// リストが空の場合、タイプに「正常」のみ設定し返却
		if(objList == null || objList.size() == 0)
		{
			resultList.setType(EIMResource.getMessage(super.getSess(), "EIM.RESULT.TYPE.INFO"));
			resultList.setOffset(EIMCommandDateUtil.getDBServerOffset(super.getSess()));
			return resultList;
		}

		separateList(objList);
		
		// リンクオブジェクトについての処理(WF有り)
		List<EIMCommandResultDocument> resultDocList = new ArrayList<EIMCommandResultDocument>();
		if(objListForWF.size() != 0)
		{
			resultDocList = setResultData(objListForWF, false);
		}
		
		// リンクオブジェクトについての処理(WF無し)
		List<EIMCommandResultDocument> resultDocNoWFList = new ArrayList<EIMCommandResultDocument>();
		if(objListForNoWF.size() != 0)
		{
			resultDocNoWFList = setResultData(objListForNoWF, true);
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
			docListForSort = sortResultList(docListForSort);
		}
		
		resultList.setType(EIMResource.getMessageValue("EIM.RESULT.TYPE.INFO"));
		resultList.setResultDocList(docListForSort, true);

		return resultList;

	}

	/**
	 * オプションの指定に従って、リストをソートする
	 * @param resultList
	 * @return
	 */
	@SuppressWarnings("unchecked")
	private List<EIMCommandResultDocument> sortResultList(List<EIMCommandResultDocument> resultList) throws Exception
	{
		String methodName = null;
		boolean isUp = true;
		
		if(optMap.containsKey(OPTION_T))
		{
			// ソート用メソッド名「最終更新日」
			methodName = EIMCommandConstant.SORT_METHOD_NAME_MDATE;

			if(optMap.containsKey(OPTION_R))
			{
				// 「降順」
				isUp = false;
			}
			resultList = AppObjectUtil.getDateSortedList(resultList, methodName, isUp);
		}

		else
		{
			// ソート用メソッド名「名称」
			methodName = EIMCommandConstant.SORT_METHOD_NAME_NAME;

			if(optMap.containsKey(OPTION_R))
			{
				// 「降順」
				isUp  = false;
			}
			resultList = AppObjectUtil.getStrSortedList(resultList, methodName, isUp);
		}

		return resultList;
	}


	/**
	 * EIMObjectのリストを、返却データのリストにセットする
	 * @param objList
	 * @param isNoWF objListがWF無しドキュメントのリストかどうか
	 * @return
	 * @throws Exception
	 */
	private List<EIMCommandResultDocument> setResultData(List<EIMObject> objList, boolean isNoWF) throws Exception
	{
		List<EIMCommandResultDocument> resultList = new ArrayList<EIMCommandResultDocument>();
    	//WF無しステータス判定用にオブジェクトを取得
    	ResultNoWFStsObjList noWFStsObjList = new ResultNoWFStsObjList();
    	if(isNoWF){
    		noWFStsObjList = EIMCommandSearchUtils.searchObjForNoWFSts(this.getSess(), objList, EIMSearchLimitCountCondition.NOT_SPECIFIED);
    	}
 		
		
		for(int i = 0; i < objList.size(); i ++)
		{
			EIMCommandResultDocument resultDoc = new EIMCommandResultDocument(super.getSess());
			resultDoc.setTarget(objList.get(i));
			if(isNoWF){
				resultDoc.judgeNoWFStatus(noWFStsObjList.getNoWFOnRevList(), noWFStsObjList.getNoWFPubList());
			}
			
		
			// リンクオブジェクトであるか
			int isLinked = AppConstant.FLAG_OFF;
			EIMAttribute attrDocLink = objList.get(i).getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_DOCUMENT_LINK"));
			if(attrDocLink != null)
			{
				if(attrDocLink.getInt() == AppConstant.FLAG_ON )
				{
					isLinked = AppConstant.FLAG_ON;
					resultDoc.setPath(super.getPath());
				}
			}
			resultDoc.setIsLinked(isLinked);
			resultList.add(resultDoc);

		}

		return resultList;
	}

	/**
	 * 指定されたパス以下のオブジェクトを取得する
	 * 取得に失敗した場合、nullを返す
	 * @param resultList
	 * @param path
	 * @param dms
	 * @return
	 * @throws Exception
	 */
	@SuppressWarnings("unchecked")
	private EIMObject getChildObjList(EIMCommandResultDocumentList resultList, String path) throws Exception
	{
		// パス解析
		String directory = null;
		String name = null;
		if(path.lastIndexOf("/") >= 0)
		{
			directory = path.substring(0, path.lastIndexOf("/") + 1);
			name = path.substring(path.lastIndexOf("/") + 1);
		}
		else
		{
			name = path;
		}

		// 取得するEIMObjectのリスト
		List<EIMObject> objList = new ArrayList<EIMObject>();

		// パスと名称から、EIMオブジェクトを取得する
		EIMObject object = this.dmu.getObjectByPathAndName(directory, name);

		// 取得したEIMオブジェクトのチェック
		if(!checkObjCondition(resultList, object))
		{
			return null;
		}

		// 指定オブジェクトの下位にあるオブジェクトのリスト(「ドキュメント」もしくは「リンク」リレーション)を取得する
		AppObjectConditionHelper helper = new AppObjectConditionHelper(super.getSess());
		objList = helper.getChildObjectsWithDocLinkInAccessibleStatus(object, new HashMap<EIMObject, Boolean>());


		// オプションに指定されていない場合、タグオブジェクトをリストから削除
		if(objList.size() != 0)
		{
			if(!optMap.containsKey(OPTION_A))
			{
				objList = dmu.getObjListExceptTag(objList);
			}
		}

		// 返却データにセット、ソート実行
		setAndSortResultData(resultList, objList);

		// 指定されたパスに該当するオブジェクトを返却
		return object;
	}

	private boolean checkObjCondition(EIMCommandResultDocumentList resultList, EIMObject object) throws Exception
	{
		// パスに指定したオブジェクトが存在しない場合
		if(object == null)
		{
    		resultList.setTypeCodeMessage(EIMResource.getMessage(super.getSess(), "EIM.RESULT.TYPE.ERROR"),
											EIMResource.getMessage(super.getSess(), "EIM.ERROR.CODE.OJBECT.NO.EXIST"),
											EIMResource.getMessage(super.getSess(), "EIM.ERROR.LOGIC.OJBECT.NO.EXIST"));
			return false;
		}

		// 指定されたパスがディレクトリでない場合(オブジェクトタイプがフォルダでない場合)
		if( !this.dmu.isFolderTypes(object.getType()) )
		{
			resultList.setTypeCodeMessage(EIMResource.getMessage(super.getSess(), "EIM.RESULT.TYPE.ERROR"),
											EIMResource.getMessage(super.getSess(), "EIM.ERROR.CODE.PATH.ISNOT.DIRECTORY"),
											EIMResource.getMessage(super.getSess(), "EIM.ERROR.LOGIC.PATH.ISNOT.DIRECTORY"));
			return false;
		}

		// 参照権限チェック(読み込み権限)
		if(!SecurityUtils.authorized(super.getSess(), object, super.getSess().getUser(), EIMAccessRole.READ))
		{
    		resultList.setTypeCodeMessage(EIMResource.getMessage(super.getSess(), "EIM.RESULT.TYPE.ERROR"),
											EIMResource.getMessage(super.getSess(), "EIM.ERROR.CODE.NO.READ.AUTH"),
											EIMResource.getMessage(super.getSess(), "EIM.ERROR.LOGIC.NO.READ.AUTH.PATH"));
			return false;
		}

		return true;
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
	
	/**
	 * 必須パラメータなしのエラー結果を設定
	 * @param resultData
	 * @param parameterName
	 * @throws EIMException
	 */
	private void setRequiredError(EIMCommandResultDocumentList resultData, String parameterName) throws EIMException {
		resultData.setTypeCodeMessage(EIMResource.getMessageValue("EIM.RESULT.TYPE.ERROR"),
				EIMResource.getMessageValue("EIM.ERROR.CODE.NO.ESSENTIAL.PARAMETER"),
				(EIMResource.getMessageValue("EIM.ERROR.LOGIC.NO.ESSENTIAL.PARAMETER")).replace("{0}", parameterName));
	}

	/**
	 * 単一指定パラメータの重複エラー結果を設定
	 * @param resultData
	 * @param parameterName
	 * @throws EIMException
	 */
	private void setDuplicatedError(EIMCommandResultDocumentList resultData, String parameterName) throws EIMException {
		resultData.setTypeCodeMessage(EIMResource.getMessageValue("EIM.RESULT.TYPE.ERROR"),
				EIMResource.getMessageValue("EIM.ERROR.CODE.OVERLAPPING.SINGLE.PARAMETER"),
				(EIMResource.getMessageValue("EIM.ERROR.LOGIC.OVERLAPPING.SINGLE.PARAMETER")).replace("{0}", parameterName));
	}

	/**
	 * オプション指定の不正エラー結果を設定
	 * @param resultData
	 * @throws EIMException
	 */
	private void setInvalidOptionError(EIMCommandResultDocumentList resultData) throws EIMException {
		resultData.setTypeCodeMessage(EIMResource.getMessageValue("EIM.RESULT.TYPE.ERROR"),
				EIMResource.getMessageValue("EIM.ERROR.CODE.INVALID.OPTION"),
				EIMResource.getMessageValue("EIM.ERROR.LOGIC.INVALID.OPTION"));
	}
	

}
