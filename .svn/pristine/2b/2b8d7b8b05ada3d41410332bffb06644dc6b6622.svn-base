package eim.command.business.service;

import eim.command.business.service.execute.EIMCommandExecuter;
import eim.command.business.service.response.EIMCommandResponseMaker;
import eim.command.business.service.response.impl.EIMCommandErrorResponseMaker;
import eim.command.business.service.result.EIMCommandResult;
import eim.net.EIMSession;

/**
 * コマンド処理情報統括クラス
 * 
 *
 */
public class EIMCommand {
    private EIMCommandExecuter commandExecuter;
    private EIMCommandErrorResponseMaker errorResponseMaker;
    private EIMCommandResponseMaker responseMaker;
    private EIMCommandResult resultData;
    private String cmdStr;
	private boolean isDirectLogin = false;
	private String user = null;
	private String pass = null;

    
	/**
	 * @return the commandExecuter
	 */
	public EIMCommandExecuter getCommandExecuter() {
		return commandExecuter;
	}
	/**
	 * @param commandExecuter the commandExecuter to set
	 */
	public void setCommandExecuter(EIMCommandExecuter commandExecuter) {
		this.commandExecuter = commandExecuter;
	}
	/**
	 * @return the responseMaker
	 */
	public EIMCommandResponseMaker getResponseMaker() {
		return responseMaker;
	}
	/**
	 * @param responseMaker the responseMaker to set
	 */
	public void setResponseMaker(EIMCommandResponseMaker responseMaker) {
		this.responseMaker = responseMaker;
	}
	/**
	 * @return the resultData
	 */
	public EIMCommandResult getResultData() {
		return resultData;
	}
	/**
	 * @param resultData the resultData to set
	 */
	public void setResultData(EIMCommandResult resultData) {
		this.resultData = resultData;
	}

	
	/**
	 * コミット
	 */
	public void commit() {
		if (getCommandExecuter() != null){
			EIMSession sess = getCommandExecuter().getSess();
			if(sess == null) return;
			try {
				//System.out.println("commit transaction");
				sess.commit();
			} catch(Exception e) {
				e.printStackTrace();
			}
		}
	}
	
	/**
	 * ロールバック
	 */
	public void rollback() {
		if (getCommandExecuter() != null){
			EIMSession sess = getCommandExecuter().getSess();
			if(sess == null) return;
			try {
				sess.rollback();
			} catch(Exception e) {
				e.printStackTrace();
			}
		}
	}
	
	/**
	 * セッションクローズ
	 */
	public void close() {
		if (getCommandExecuter() != null){
			EIMSession sess = getCommandExecuter().getSess();
			if(sess != null) {
				try { sess.close(); } catch(Exception e) {}
			}
		}
	}
	/**
	 * @return the isDirectLogin
	 */
	public boolean isDirectLogin() {
		return isDirectLogin;
	}
	/**
	 * @param isDirectLogin the isDirectLogin to set
	 */
	public void setDirectLogin(boolean isDirectLogin) {
		this.isDirectLogin = isDirectLogin;
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
	 * @return the errorResponseMaker
	 */
	public EIMCommandErrorResponseMaker getErrorResponseMaker() {
		return errorResponseMaker;
	}
	/**
	 * @param errorResponseMaker the errorResponseMaker to set
	 */
	public void setErrorResponseMaker(
			EIMCommandErrorResponseMaker errorResponseMaker) {
		this.errorResponseMaker = errorResponseMaker;
	}
	/**
	 * @return the cmdStr
	 */
	public String getCmdStr() {
		return cmdStr;
	}
	/**
	 * @param cmdStr the cmdStr to set
	 */
	public void setCmdStr(String cmdStr) {
		this.cmdStr = cmdStr;
	}
	

}
