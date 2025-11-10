package eim.command.business.service;

import java.io.IOException;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import common.util.AppMessageUtils;
import eim.bo.EIMResource;
import eim.bo.EIMUser;
import eim.command.business.service.factroy.EIMCommandFactory;
import eim.command.business.service.response.impl.EIMCommandErrorResponseMaker;
import eim.command.business.service.result.EIMCommandResult;
import eim.command.common.util.EIMCommandConstant;
import eim.command.common.util.EIMCommandDateUtil;
import eim.command.common.util.EIMCommandResultConstant;
import eim.db.DBUtils;
import eim.net.EIMSession;
import eim.util.CipherUtils;
import eim.util.EIMConstant;
import eim.util.OperationHistoryUtils;
import eim.util.UserUtils;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.business.dao.UserDao;
import jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader;

public class EIMCommandService extends HttpServlet {

	private static Log log = LogFactory.getLog("EIMCommandService");

	/**
	 *
	 * Version
	 *
	 */
	public static final String VERSION = "4.05";

	/**
	 * 初期化
	 */
	public void init() throws ServletException {
		super.init();
	}


	/**
	 * サービス実行
	 */
	protected void service(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

		// コマンド処理インスタンス
		EIMCommand command = null;
		try{
			try {
				// コマンドごとの処理インスタンス取得
				EIMCommandFactory cmdFactory = new EIMCommandFactory();
				command = cmdFactory.create(request.getParameter(EIMCommandConstant.CMD));

				// factoryの中でエラー発生の場合
				if (command.getResultData() != null && command.getResultData().getType() != null && command.getResultData().getType().equals(EIMResource.getMessageValue("EIM.RESULT.TYPE.ERROR"))){
					throw new Exception();
				}

				// リクエストパラメータをコマンド実行インスタンスに設定
				command.getCommandExecuter().setRequestParameters(request);

				//重複チェック
				checkRepetition(command);

				// ログイン処理実行
				EIMSession sess = login(request, command.getCommandExecuter().getUser(), command.getCommandExecuter().getPass(), command);

				if(sess == null) {
					command.close();
					if(command.getResultData() == null) {
						command.setResultData(new EIMCommandResult());
					}
					command.getResultData().setTypeCodeMessage(EIMCommandResultConstant.TYPE_ERROR, EIMCommandResultConstant.CODE_AUTH_FAILED, EIMCommandResultConstant.MSG_AUTH_FAILED);

					throw new Exception();
				} else {
					command.getCommandExecuter().setSess(sess);
					command.setDirectLogin(true);
				}

				// コマンドを実行し、結果を取得
				command.setResultData(command.getCommandExecuter().execute());

				// エラー終了以外の場合のみ、コミット
				if(command != null){
					if(!command.getResultData().getType().equals(EIMCommandResultConstant.TYPE_ERROR)){
						command.commit();
					}
				}

			} catch(Exception e) {
				log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);

				if (command != null){
					command.rollback();
				}
				else{
					command = new EIMCommand();
				}

				// エラーレスポンス返却情報設定
				if (command.getResultData() == null){
					command.setResultData(new EIMCommandResult());
				}
				if (command.getResultData().getType() == null || !command.getResultData().getType().equals(EIMCommandResultConstant.TYPE_ERROR)){
					command.getResultData().setTypeCodeMessage(EIMCommandResultConstant.TYPE_ERROR, EIMCommandResultConstant.CODE_SYSTEM_ERROR, EIMCommandResultConstant.MSG_SYSTEM_ERROR);
				}
//				e.printStackTrace();
			} finally {
				// 中間ファイル削除
				if(command.getCommandExecuter() != null && command.getCommandExecuter().getFiles() != null && command.getCommandExecuter().getFiles().length > 0) {
					for(int i = 0; i < command.getCommandExecuter().getFiles().length; i++) {
						try { command.getCommandExecuter().getFiles()[i].delete(); } catch(Exception e) {}
					}
				}
				if (command != null){
					command.close();
				}
			}

			// レスポンス返却
			try {
				// エラーなら、エラー専用レスポンス返却処理
				if (command.getResultData().getType().equals(EIMCommandResultConstant.TYPE_ERROR)){
					if (command.getErrorResponseMaker() == null){
						command.setErrorResponseMaker(new EIMCommandErrorResponseMaker());
					}
					command.getErrorResponseMaker().makeResponse(command.getResultData(), request, response);
				}
				else{
					command.getResponseMaker().makeResponse(command.getResultData(), request, response);
				}
			}
			catch(Exception e){
//				e.printStackTrace();
				log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
				throw new ServletException();
			}
		}finally
		{
			EIMThreadContext.removeAll();
		}

		return;

	}

	/**
	 * ログイン認証
	 */
	protected EIMSession login(HttpServletRequest request, String userCode, String userPass, EIMCommand command) throws Exception {

		//HTTP Session
		HttpSession session = request.getSession(false);
		if(session == null || session.getAttribute("USER") == null) {
			session = request.getSession(true);
			session.setAttribute("langId", "JA");
			session.setAttribute("USER", new EIMUser(1, null, null, null, null, null, 0, null));
		}

		//Session
		EIMSession sess = null;

		try {

			//Console Session
			sess = new EIMSession();
			sess.setRequest(request);
			sess.setConnection(DBUtils.getDBConnection());
			sess.getDBConnection().setAutoCommit(false);

			//Get User
			EIMUser user = UserUtils.getUserByCode(sess, userCode);
			if(user == null || userCode == "" || userCode.length() < 1) {
				sess.close();
				return null;
			}

			//パスワード取得処理
			String password;
			try {
				// スレッドローカルのEIMSession変数を入れ替える
				EIMThreadContext.pushEIMSession(sess);
				UserDao userDao = (UserDao)ApplicationContextLoader.getApplicationContext().getBean("userDaoLight2");
				password = userDao.getPasswordByCode(userCode);
			} finally {
				// スレッドローカルのEIMSession変数を復帰させる
				EIMThreadContext.popEIMSession();
			}
			
			//Check Password
			if(!password.equals(CipherUtils.getMessageDigest(sess, userPass)) ||userPass == "" || userPass.length() < 1) {
				sess.close();
				return null;
			}

			//Set Session
			sess.setUser(user);
			session.setAttribute(EIMSession.USER, user);

			// Set DB Server Offset
			if(sess != null)
			{
				sess.setAttribute(EIMCommandConstant.SESSION_ATTRIBUTE_DBTZ_OFFSET_FOR_EIMCOMMAND, EIMCommandDateUtil.getDBServerOffset(sess));
			}

			// Set TimezoneOffset
			// ※ EIMログイン画面からのログイン処理では、クライアントから渡してもらっている
			// ※ Appサーバのタイムゾーンを使う
			String clTzOffset = String.valueOf((-1)*eim.util.DateUtils.getAPTzOffset());
			sess.setAttribute(EIMCommandConstant.SESSION_ATTRIBUTE_CLIENTTZ_OFFSET, clTzOffset);
			String dbTzOffset = String.valueOf(eim.util.DateUtils.selectDBTzOffset(sess));
			sess.setAttribute(EIMCommandConstant.SESSION_ATTRIBUTE_DBTZ_OFFSET, dbTzOffset);

			//Set EIMSession to EIMThreadContext
			EIMThreadContext.putEIMSession(sess);

			//Operation History
			OperationHistoryUtils.create(sess, EIMCommandConstant.COMMAND, EIMConstant.LOGIN, null, null, null, null, null, null, VERSION);

			return sess;

		} catch(Exception e) {
//			e.printStackTrace();
			try { sess.close(); } catch(Exception sce) {}
			throw e;
		}
	}
	private void checkRepetition(EIMCommand command) throws Exception{
		//user, passの重複チェック
		if(command.getCommandExecuter().getUsers().size() > 1 ){
			if(command.getResultData() == null) {
				command.setResultData(new EIMCommandResult());
			}
			command.getResultData().setTypeCodeMessage(EIMCommandResultConstant.TYPE_ERROR,
					EIMResource.getMessage("EIM.ERROR.CODE.OVERLAPPING.SINGLE.PARAMETER"),
					EIMResource.getMessageValue("EIM.ERROR.LOGIC.OVERLAPPING.SINGLE.PARAMETER", new Object[]{EIMCommandConstant.USER}));
			throw new Exception();
		}
		if(command.getCommandExecuter().getPasswords().size() > 1){
			if(command.getResultData() == null) {
				command.setResultData(new EIMCommandResult());
			}
			command.getResultData().setTypeCodeMessage(EIMCommandResultConstant.TYPE_ERROR,
					EIMResource.getMessage("EIM.ERROR.CODE.OVERLAPPING.SINGLE.PARAMETER"),
					EIMResource.getMessageValue("EIM.ERROR.LOGIC.OVERLAPPING.SINGLE.PARAMETER", new Object[]{EIMCommandConstant.PASS}));
			throw new Exception();
		}else if(command.getCommandExecuter().getUsers().size() > 0 && command.getCommandExecuter().getPasswords().size() > 0){
			//sizeが0の場合はlogin()内で処理
			command.getCommandExecuter().setUser(command.getCommandExecuter().getUsers().get(0));
			command.getCommandExecuter().setPass(command.getCommandExecuter().getPasswords().get(0));
		}


	}
}
