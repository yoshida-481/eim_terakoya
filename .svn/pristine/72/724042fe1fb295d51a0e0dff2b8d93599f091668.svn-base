<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.PasswordValidateService"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.TransactionContext"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.UserService"%>
<%@ page import = "java.util.List"%>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	//Session
	EIMSession sess = null;	
	EIMUser user = null;
	
	//Parameter
	String prmCurPass = EIMUtils.getParameter(request, "currentPassword");
	String prmNewPass = EIMUtils.getParameter(request, "newPassword");

	//Message
	String message = null;
	Object[] paramId = {
			"currentPassword=" + prmCurPass,
			"newPassword=" + prmNewPass
			};

	try{
		//Session
		sess = EIMUtils.getSession(request);
		if(sess == null)
		{
			message = EIMResource.getMessage(request, "EIM.ERROR.SESSIONTIMEOUT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.SESSIONTIMEOUT");
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}
		
		//User
		user = (EIMUser)sess.getAttribute("USER");

		//現在パスワードチェック
		if (!user.getPass().equals(CipherUtils.getMessageDigest(sess, prmCurPass))) {
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.VAL_PASSWORDINVALID");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.VAL_PASSWORDINVALID");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}

		//新しいパスワードチェック
		if(EIMThreadContext.getTransactionContext() != null){
			EIMThreadContext.removeTransactionContext();
		}
		TransactionContext transactionContext = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
		EIMThreadContext.putTransactionContext(transactionContext);
		transactionContext.setLangId(sess.getLangId());
		transactionContext.setDBConnection(sess.getDBConnection());
		transactionContext.setUser(ConvertUtils.toUserDomain(user));
		PasswordValidateService passwordValidateService = (PasswordValidateService)jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader.getApplicationContext().getBean("passwordValidateService2");
		List<String> errMsgList = passwordValidateService.validateValue(ConvertUtils.toUserDomain(user), prmNewPass);
		if(!errMsgList.isEmpty()){
			String[] errMsgArray = {String.join("\n", errMsgList)};
			message = EIMResource.getMessage(sess, "EIM.DIALOG.DISP.MESSAGE", errMsgArray);
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.DIALOG.DISP.MESSAGE", errMsgArray);
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message));
			return;
		}
		//ユーザオブジェクトに含まれるパスワード関連の属性の更新
		passwordValidateService.updateUserObjectPasswordRelated(ConvertUtils.toUserDomain(user));

		// 最新のユーザ属性の情報を再度取得してセッション上のユーザ情報に詰め直す
		UserService userService2ForAuthentication = (UserService)jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader.getApplicationContext().getBean("userService2ForAuthentication");
		UserDomain checkUser = userService2ForAuthentication.getByCode(user.getCode());
		user.setUserObject(ConvertUtils.toEIMObject(checkUser.getUserObject()));

		//新しいパスワードで更新
		user = UserUtils.updateUser(sess,user,user.getCode(),user.getDefName(),
				user.getKana(),prmNewPass,user.getMail(), user.getAdmin(), user.getDisable());

		// セッション上のユーザ情報を更新しないとDBとセッション内のパスワード情報で不一致が起こる
		session.setAttribute("USER" , user);

		//Create Operation History
		OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.UPDATE_PASSWORD, 
				EIMConstant.TARGET_UPDATE, EIMConstant.USER, user,
				null, null, null, null);

		//コミット
		sess.commit();
		out.println("<ok/>");			
	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(user.getId(), eime.getMessage(), paramId), eime);
		try{
			if(sess != null){
				sess.rollback();
			}
		}
		catch (Exception se) {
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
	}
	catch(Exception e)
	{
		out.clear();
		message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
		try{
			if(sess != null){
				sess.rollback();
			}
		}
		catch (Exception se) {
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
	}
	finally
	{
		try{
			if(sess != null){
				sess.close();
			}
		}
		catch (Exception se) {
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
	}
%>