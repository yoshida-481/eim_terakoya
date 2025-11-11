<%@ page contentType="text/xml; charset=UTF-8" %>
<%@ page import = "java.net.*" %>
<%@ page import = "java.util.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>

<%@ page import = "org.springframework.context.ApplicationContext" %>
<%@ page import = "org.springframework.security.authentication.AccountStatusException" %>
<%@ page import = "org.springframework.security.authentication.AuthenticationManager" %>
<%@ page import = "org.springframework.security.authentication.BadCredentialsException" %>
<%@ page import = "org.springframework.security.authentication.UsernamePasswordAuthenticationToken" %>
<%@ page import = "org.springframework.security.core.Authentication" %>
<%@ page import = "org.springframework.security.core.AuthenticationException" %>
<%@ page import = "org.springframework.security.core.context.SecurityContext" %>
<%@ page import = "org.springframework.security.core.context.SecurityContextHolder" %>
<%@ page import = "org.springframework.security.core.userdetails.UsernameNotFoundException" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.enumeration.SessionAttributeNameEnum" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.security.EIMUserDetails" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.security.UserRealmService" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.exception.EIMException"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.UserService"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.util.ConfigUtils"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.PasswordValidateService"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.TransactionContext"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext"%>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	EIMUser user = null;
	EIMSession sess = null;

	//Parameter
	String prmUserCode = request.getParameter("userCode");
	String prmUserPass = request.getParameter("userPass");
	String prmLang = EIMUtils.getParameter(request, "langId");
	String prmLocale = EIMUtils.getParameter(request, "locale");
	String prmTzOffset = EIMUtils.getParameter(request, "userTzOffset");

	//Message
	String message = null;
	Object[] paramId = {
			"userCode=" + prmUserCode,
			"langId=" + prmLang,
			"locale=" + prmLocale,
			"userTzOffset=" + prmTzOffset
			};
	try
	{
		//-----------------------
		// Set Cookie
		//-----------------------
		// クッキーに格納する文字列を作成(URLエンコードをする)
		String value = URLEncoder.encode(prmLang.toString(), "UTF-8");

		// 名前が"language"、値が現在の言語であるクッキーを作成
		Cookie cookie = new Cookie(EIMConfig.get("COOKIE_LANG_ID"), value);

		// クッキーの有効期間を設定 (90日)
		cookie.setMaxAge(Integer.parseInt(EIMConfig.get("COOKIE_EFFECTIVE_TERM")));

		// クッキーのパスを設定（指定パス以下でしか利用できない）
		cookie.setPath(EIMConfig.get("APPLICATION_PATH"));

		// クッキーを発行
		response.addCookie(cookie);

		// 必須チェック (半角スペースのみが有りのためStringUtils.isBlankは使えない)
		if (prmUserCode == null || prmUserCode.equals("")){
			// IDを指定して下さい。
			message = EIMResource.getMessage(prmLang, "EIM.ERROR.INPUT.NOUSERID");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
		}
		if (prmUserPass == null || prmUserPass.equals("")){
			// Passwordを指定して下さい。
			message = EIMResource.getMessage(prmLang, "EIM.ERROR.INPUT.NOPASSWORD");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
		}

		ApplicationContext context = ApplicationContextLoader.getApplicationContext();

		//-----------------------
		// Login
		//-----------------------
		
		// 属性値格納用(key：属性名、value：属性値)
		Map<String, Object> attrMap = new HashMap<String, Object>();
		
		// セッションに設定された属性情報を退避
		Enumeration<String> attrNames = session.getAttributeNames();
		String attrName = null;
		while (attrNames.hasMoreElements()) {
			attrName =  (String)attrNames.nextElement();
			attrMap.put(attrName, session.getAttribute(attrName));
		}
		
		// Login前のセッションを削除(セッションフィクセーション対応)
		request.getSession(true).invalidate();
		
		// セッションを再発行(セッションフィクセーション対応)
		session = request.getSession(true);
		
		// 退避した属性値を再発行したセッションに設定
		Set<String> keySet = attrMap.keySet();
		Iterator<String> strIterator = keySet.iterator();
		while (strIterator.hasNext()) {
			
			attrName = (String)strIterator.next();
			session.setAttribute(attrName, attrMap.get(attrName));
		}

		// ドメイン名をレルム名に変換
		UserRealmService userRealmService = context.getBean("userRealmService", UserRealmService.class);
		String convUserCode = userRealmService.convertDomainToRealm(prmUserCode);

		//Configファイルの設定を元にトランザクションコンテキストを生成する
		if(EIMThreadContext.getTransactionContext() != null)
		{
			EIMThreadContext.removeTransactionContext();
		}

		TransactionContext transactionContext = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
		EIMThreadContext.putTransactionContext(transactionContext);
		transactionContext.setLangId(prmLang);

		//パスワードの失敗回数が許容値を超えているかをチェックする
		UserService userService2ForAuthentication = (UserService)jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader.getApplicationContext().getBean("userService2ForAuthentication");
		UserDomain checkUser = userService2ForAuthentication.getByCode(prmUserCode);
		int applicationTypeId = 0;
		int recordInfoAId = 0;
		if(checkUser != null && !checkUser.isDisable()){
			try{
				applicationTypeId = Integer.valueOf(ConfigUtils.getByKey("PASSWORD_LOCK_HISTORY_APPLICATION_ID"));
				recordInfoAId = Integer.valueOf(ConfigUtils.getByKey("PASSOWRD_LOCK_HISTORY_RECORD_INFO_A_ID"));
			}catch (Exception e) {
				applicationTypeId = 0;
				recordInfoAId = 0;
			}
		}

		PasswordValidateService passwordValidateService = (PasswordValidateService)jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader.getApplicationContext().getBean("passwordValidateService2");
		if(!passwordValidateService.checkAuthenticateFailureCount(checkUser, prmLang, applicationTypeId, recordInfoAId)){
			message = EIMResource.getMessage(prmLang, "EIM.ERROR.LOGIC.LOCKED.LOGIN");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
		}

		// ログイン認証
		Authentication inputAuth = new UsernamePasswordAuthenticationToken(convUserCode, prmUserPass);
		AuthenticationManager authenticationManager = context.getBean("authenticationManager", AuthenticationManager.class);
		Authentication outputAuth;
		try {
			outputAuth = authenticationManager.authenticate(inputAuth);
		} catch (AccountStatusException
				| BadCredentialsException
				| UsernameNotFoundException e) {
			message = EIMResource.getMessage(prmLang, "EIM.ERROR.LOGIC.INVALIDPASSWORD");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			// ログイン失敗回数を記録する
			if(checkUser != null && !checkUser.isDisable()){
				//パスワード不正入力回数をカウントアップ
				passwordValidateService.setNumberOfIncorrectPasswordsEntered(checkUser, false);
			}
			return;
		} catch (AuthenticationException e) {
			Throwable cause = e.getCause();
			if (cause instanceof AccountStatusException
					|| cause instanceof BadCredentialsException
					|| cause instanceof UsernameNotFoundException) {
				message = EIMResource.getMessage(prmLang, "EIM.ERROR.LOGIC.INVALIDPASSWORD");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				// ログイン失敗回数を記録する
				if(checkUser != null && !checkUser.isDisable() && cause instanceof BadCredentialsException){
					//パスワード不正入力回数をカウントアップ
					passwordValidateService.setNumberOfIncorrectPasswordsEntered(checkUser, false);
				}
				return;
			}
			throw e;
		}

		// SecurityContextにAuthenticationオブジェクトを設定
		SecurityContext securityContext = SecurityContextHolder.getContext();
		securityContext.setAuthentication(outputAuth);
		session.setAttribute("SPRING_SECURITY_CONTEXT", securityContext);

		// Authenticationオブジェクトからユーザ情報取得
		EIMUserDetails userDetails = (EIMUserDetails) outputAuth.getPrincipal();
		UserDomain userDomain = userDetails.getUserDomain();
		String cipherPass = userDetails.getPassword();

		//パスワード不正入力回数を初期化
		passwordValidateService.setNumberOfIncorrectPasswordsEntered(checkUser, true);

		// EIMUserに変換
		user = ConvertUtils.toEIMUser(userDomain, cipherPass, prmLang);

		// Session Attribute
		session.setAttribute(SessionAttributeNameEnum.USERDOMAIN.getSymbol(), userDomain);
		session.setAttribute(SessionAttributeNameEnum.LANG.getSymbol(), prmLang);
		session.setAttribute("USER" , user);

		// Set LangId
		sess = EIMUtils.getSession(request);
		sess.setAttribute(EIMSession.LANG, prmLang);

		// Set Locale
		sess.setAttribute("locale", prmLocale);

		// Set TimezoneOffset
		sess.setAttribute("clTzOffset", prmTzOffset);

		// Set TimezoneOffset
		String dbTzOffset = String.valueOf(eim.util.DateUtils.selectDBTzOffset(sess));
		sess.setAttribute("dbTzOffset", dbTzOffset);

		// SSOでログインした際に設定されるセッション変数を削除
		session.removeAttribute("sso");

		//Create Operation History
		OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.LOGIN,
				null, null, null,
				null, null, null, null);

		//Commit
		sess.commit();

		//User Information
		out.println("<user");
			out.println(" userId=\"" + user.getId() + "\"");
			out.println(" userCode=\"" + eim.util.StringUtils.xmlEncode(user.getCode()) + "\"");
			out.println(" userName=\"" + eim.util.StringUtils.xmlEncode(user.getName()) + "\"");
			out.println(" userKana=\"" + eim.util.StringUtils.xmlEncode(user.getKana()) + "\"");
			out.println(" userMail=\"" + eim.util.StringUtils.xmlEncode(user.getMail()) + "\"");
			out.println(">");
		out.println("</user>");
	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		long uId = 0;
		if(user != null){
			uId = user.getId();
		}
		log.warn(AppMessageUtils.makeLogMessage(uId, eime.getMessage(), paramId), eime);
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
