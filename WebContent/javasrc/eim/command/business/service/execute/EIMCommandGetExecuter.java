package eim.command.business.service.execute;


import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import jakarta.servlet.http.HttpServletRequest;

import eim.bo.EIMObject;
import eim.bo.EIMResource;
import eim.command.business.service.EIMCommandService;
import eim.command.business.service.result.EIMCommandResult;
import eim.command.business.service.result.EIMCommandResultData;
import eim.command.common.DownloadDocument;
import eim.command.common.util.DocumentManagementUtils;
import eim.command.common.util.EIMCommandConstant;
import eim.command.common.util.EIMCommandUtil;
import eim.net.EIMSession;
import eim.util.EIMConstant;
import eim.util.OperationHistoryUtils;

/**
 * getコマンド用コマンド実行実装クラス
 * (指定されたパスから、ドキュメントのダウンロードを実行する)
 *
 */
public class EIMCommandGetExecuter extends EIMCommandExecuter {

	private List<String> paths = new ArrayList<String>();
	private List<String> fmts = new ArrayList<String>();

	// 引数
	private String path = null;
	private String fmt = null;
	
	public EIMCommandGetExecuter(){

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
     * getコマンドの引数を設定する
     * @param key
     * @param value
     */
	private void setGetCommandParameter(String key, String value)
	{
		super.setParameter(key, value);

		if(key.equals(EIMCommandConstant.PATH))
		{
			paths.add(value);
		}
		else if(key.equals(EIMCommandConstant.FORMAT))
		{
			fmts.add(value);
		}
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
	 * @return path
	 */
	public String getPath() {
		return path;
	}

	/**
	 * @param path 設定する path
	 */
	public void setPath(String path) {
		this.path = path;
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

		// パラメータのチェック
		if(!checkParam(result))
		{
			return result;
		}
    	
		// 引数に指定されたパスから、オブジェクト名称とオブジェクト名称を除いたパスを取得する
		String directory = null;
		String name = null;

		// パス解析
		if(path.lastIndexOf("/") > 0)
		{
			directory = path.substring(0, path.lastIndexOf("/") + 1);
			name = path.substring(path.lastIndexOf("/") + 1);
		}
		else
		{
			name = path;
		}

		// パスと名称から、EIMオブジェクトを取得する
		EIMObject obj = null;
		DocumentManagementUtils dmu = new DocumentManagementUtils(sess);
		obj = dmu.getObjectByPathAndName(directory, name);

		// ドキュメントのダウンロードを実行する(object,formatのチェックは内部で行っている)
		DownloadDocument doc = new DownloadDocument(sess);
		result = doc.doDownloadDocument(super.getFormat(), obj, result, true);

		if (result != null && EIMResource.getMessage(sess, "EIM.ERROR.CODE.OBJECT.ISNOT.DOCUMENT").equals(result.getCode())) {
			// ドキュメントではないオブジェクトの場合、エラーメッセージにパスを設定
			result.setMessage(EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.OBJECT.ISNOT.DOCUMENT", new Object[]{path}));
		}
		
		// 操作履歴
		OperationHistoryUtils.create(sess, EIMCommandConstant.COMMAND, EIMCommandConstant.GET_EXCOMMAND,
										EIMCommandConstant.TARGET_TO_DOWNLOAD, EIMConstant.OBJECT, obj, null, null, null,
										EIMCommandService.VERSION + ":" + path);

        return result;
    }

	/**
	 * 必須パラメータをチェックする
	 * エラーである場合、不足しているパラメータを返す(エラーでない場合はnullを返す)
	 * @return
	 */
	private String checkEssentialParamExist()
	{
		//pathsのチェック
		if(paths.size() < 1)
		{
			return EIMCommandConstant.PATH;
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
		// pathsのチェック
		if(paths.size() > 1)
		{
			return EIMCommandConstant.PATH;
		}

		// Formatのチェック
		if(fmts.size() > 1)
		{
			return EIMCommandConstant.FORMAT;
		}

		return null;
	}

	/**
	 * パラメータのチェックを行う
	 * @param result
	 * @return
	 */
	private boolean checkParam(EIMCommandResultData result) throws Exception
	{
		// 必須パラメータをチェックする
		String eParam = checkEssentialParamExist();
		if(eParam != null)
		{
			Object[] eObj = {eParam};
			result.setTypeCodeMessage(EIMResource.getMessage(super.getSess(), "EIM.RESULT.TYPE.ERROR"),
										EIMResource.getMessage(super.getSess(), "EIM.ERROR.CODE.NO.ESSENTIAL.PARAMETER"),
										EIMResource.getMessage(super.getSess(), "EIM.ERROR.LOGIC.NO.ESSENTIAL.PARAMETER", eObj));
			return false;
		}

		// パラメータの重複をチェックする
    	String dParam = checkDuplicatedParam();
    	if(dParam != null)
    	{
    		Object[] dObj = {dParam};
			result.setTypeCodeMessage(EIMResource.getMessage(super.getSess(), "EIM.RESULT.TYPE.ERROR"),
										EIMResource.getMessage(super.getSess(), "EIM.ERROR.CODE.OVERLAPPING.SINGLE.PARAMETER"),
										EIMResource.getMessage(super.getSess(), "EIM.ERROR.LOGIC.OVERLAPPING.SINGLE.PARAMETER", dObj));
			return false;
    	}
    	
    	// 引数のチェックが完了したので、引数をセットする
    	path = paths.get(0);
    	if(fmts.size() == 1)
    	{
    		fmt = fmts.get(0);
    	}
    	
		return true;
	}

}
