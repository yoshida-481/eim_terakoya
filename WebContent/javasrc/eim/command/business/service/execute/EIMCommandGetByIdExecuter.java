package eim.command.business.service.execute;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import jakarta.servlet.http.HttpServletRequest;

import common.util.AppObjectUtil;
import eim.bo.EIMObject;
import eim.bo.EIMResource;
import eim.command.business.service.EIMCommandService;
import eim.command.business.service.result.EIMCommandResult;
import eim.command.business.service.result.EIMCommandResultData;
import eim.command.common.DownloadDocument;
import eim.command.common.util.EIMCommandConstant;
import eim.command.common.util.EIMCommandUtil;
import eim.net.EIMSession;
import eim.util.EIMConstant;
import eim.util.ObjectUtils;
import eim.util.OperationHistoryUtils;

/**
 * getByIdコマンド用コマンド実行実装クラス
 * (指定されたobjIdから、ドキュメントのダウンロードを実行する)
 *
 */
public class EIMCommandGetByIdExecuter extends EIMCommandExecuter {

	private List<String> objIds = new ArrayList<String>();
	private List<String> fmts = new ArrayList<String>();
	
	// 引数
	private String objId = null;
	private String fmt = null;
	
	
	public EIMCommandGetByIdExecuter(){
		
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
					this.setGetByIdCommandParameter(paramKey, paramValues[i]);
				}
			}
		}
	}
	
	/**
	 * getByIdコマンドの引数を設定する
	 * @param key
	 * @param value
	 */
	private void setGetByIdCommandParameter(String key, String value)
	{
		super.setParameter(key, value);
		
		if(key.equals(EIMCommandConstant.OBJID))
		{
			objIds.add(value);
		}
		else if(key.equals(EIMCommandConstant.FORMAT))
		{
			fmts.add(value);
		}
	}
	
	/**
	 * @return objId
	 */
	public String getObjId() {
		return objId;
	}
	
	/**
	 * @param objId 設定する objId
	 */
	public void setObjId(String objId) {
		this.objId = objId;
	}
	
	/**
	 * @return fmt
	 */
	public String getFmt() {
		return fmt;
	}

	/**
	 * @param fmt 設定する fmt
	 */
	public void setFmt(String fmt) {
		this.fmt = fmt;
	}

	/**
	 * コマンドを実行する
	 */
	public EIMCommandResult execute() throws Exception
	{
    	// 返却値
		EIMCommandResultData result = new EIMCommandResultData();
		
		// セッション
		EIMSession sess = super.getSess();
    	
		// 必須パラメータをチェックする
		String eParam = checkEssentialParamExist();
		if(eParam != null)
		{
			Object[] eObj = {eParam};
			result.setTypeCodeMessage(EIMResource.getMessage(sess, "EIM.RESULT.TYPE.ERROR"), 
										EIMResource.getMessage(sess, "EIM.ERROR.CODE.NO.ESSENTIAL.PARAMETER"), 
										EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NO.ESSENTIAL.PARAMETER", eObj));
			return result;
		}
		
		// パラメータの重複をチェックする
    	String dParam = checkDuplicatedParam();
    	if(dParam != null)
    	{
    		Object[] dObj = {dParam};
			result.setTypeCodeMessage(EIMResource.getMessage(sess, "EIM.RESULT.TYPE.ERROR"), 
										EIMResource.getMessage(sess, "EIM.ERROR.CODE.OVERLAPPING.SINGLE.PARAMETER"), 
										EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.OVERLAPPING.SINGLE.PARAMETER", dObj));
			return result;
    	}
    	
    	// パラメータの型をチェックする
    	if(!EIMCommandUtil.isOneByteNum(objIds.get(0))){
    		Object[] tObj = {objIds.get(0)};
    		result.setTypeCodeMessage(EIMResource.getMessage(sess, "EIM.RESULT.TYPE.ERROR"),
    									EIMResource.getMessage(sess, "EIM.ERROR.CODE.OBJID.NOT.NUM"),
    									EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.OBJID.NOT.NUM", tObj));
    		return result;
    	}
    	
    	// 引数のチェックが完了したので、引数をセットする
    	objId = objIds.get(0);
    	if(fmt != null)
    	{
    		fmt = fmts.get(0);
    	}
    	
    	// objIdからEIMオブジェクトを取得する
    	EIMObject obj = ObjectUtils.getObjectById(sess, Long.parseLong(objId));
    	
    	// ドキュメントのダウンロードを実行する(object,formatのチェックは内部で行っている)
		DownloadDocument doc = new DownloadDocument(sess);
		result = doc.doDownloadDocument(super.getFormat(), obj, result, false);
		
		// 操作履歴表示のパスの取得
		String dspPath = "";
		if(obj != null)
		{
			dspPath = AppObjectUtil.getPath(obj) + obj.getName();
		}
		
		// 操作履歴
		OperationHistoryUtils.create(sess, EIMCommandConstant.COMMAND, EIMCommandConstant.GET_BY_ID_EXCOMMAND, 
										EIMCommandConstant.TARGET_TO_DOWNLOAD, EIMConstant.OBJECT, obj, null, null, null, 
										EIMCommandService.VERSION + ":" + dspPath);
    	
        return result;
    	
    }
	
	
	/**
	 * 必須パラメータをチェックする
	 * エラーである場合、不足しているパラメータを返す(エラーでない場合はnullを返す)
	 * @return
	 */
	private String checkEssentialParamExist()
	{
		//ObjIdのチェック
		if(objIds.size() < 1)
		{
			return EIMCommandConstant.OBJID;
		}
		
		return null;
	}
	
	
	/**
	 * 単一指定パラメータをチェックする
	 * エラーである場合、重複しているパラメータを返す(エラーでない場合はnullを返す)
	 * @return
	 */
	private String checkDuplicatedParam()
	{
		// ObjIdのチェック
		if(objIds.size() > 1)
		{
			return EIMCommandConstant.OBJID;
		}
		
		// Formatのチェック
		if(fmts.size() > 1)
		{
			return EIMCommandConstant.FORMAT;
		}
		
		return null;
	}
	
}