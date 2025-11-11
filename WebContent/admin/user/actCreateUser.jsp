<%@page import="java.text.DateFormat"%>
<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>
<%@ page import = "jp.co.ctc_g.eim.framework.common.exception.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	//Session
	EIMSession sess = null;
	EIMUser loginUser = null;

	//Parameter
	String prmUserCode = EIMUtils.getParameter(request, "userCode");
	String prmUserKana = EIMUtils.getParameter(request, "userKana");
	String prmUserPass = EIMUtils.getParameter(request, "userPass");
	String prmUserMail = EIMUtils.getParameter(request, "userMail");
	String prmUserAdmin = EIMUtils.getParameter(request, "userAdmin");
	String prmUserDisable = EIMUtils.getParameter(request, "userDisable");
	String prmUserLang = EIMUtils.getParameter(request, "userLang");

	//Message
	String message = null;
	Object[] paramId = {
			"userCode=" + prmUserCode,
			"userKana=" + prmUserKana,
			"userPass=" + prmUserPass,
			"userMail=" + prmUserMail,
			"userAdmin=" + prmUserAdmin,
			"userDisable=" + prmUserDisable,
			"userLang=" + prmUserLang
			};

	try
	{
		/*
		 * param check
		 */
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
		loginUser = (EIMUser)sess.getAttribute("USER");
		if(!AdminAuthUtil.hasSpecifiedOrGeneralAuth(loginUser, AppConstant.ADMIN_AUTH_ID_USER))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.NOTADMINISTRATOR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.NOTADMINISTRATOR");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message));
			return;
		}

		/*
		 * Create User
		 */
		int prmOtherCnt = LanguageFieldUtil.getOtherCnt(request);
		String prmDefName = LanguageFieldUtil.getDefName(request, prmOtherCnt);

		//Create User
		EIMUser user = UserUtils.createUser(sess,
											prmUserCode,
											prmDefName,
											prmUserKana,
											prmUserPass,
											prmUserMail,
											Integer.parseInt(prmUserAdmin),
											Integer.parseInt(prmUserDisable),
											prmUserLang);
		/*
		 * Create User Other
		 */
		String nowLId = (String)sess.getAttribute(EIMSession.LANG);
		String nowLangName = user.getName();
		for(int i = 0; i < prmOtherCnt; i++)
		{
			String prmOtherName = LanguageFieldUtil.getOtherName(request,i);
			String prmOtherLId = LanguageFieldUtil.getOtherLId(request,i);

			// Get NowLangName
			if(nowLId.equals(prmOtherLId)){
				nowLangName = prmOtherName;
			}
			// Create User Other
			UserUtils.addOtherUserName(sess, user.getId(), prmOtherLId, prmOtherName);
		}

		// オブジェクトタイプから属性を取得
		// List<EIMAttributeType> ...
		EIMObject objectUser = user.getUserObject();
		List attTypes = ObjectAttributeUtils.getAttributeTypeList(sess, objectUser.getType());

		// 属性と比較し情報を登録・削除
		for (int i = 0; i < attTypes.size(); i++) {
			EIMAttributeType attType = (EIMAttributeType)attTypes.get(i);

			List<String> valueList = new ArrayList();
			int pos = 0;
			
			while(true) {
				String param = EIMUtils.getParameter(request, "attType_" + attType.getId() + "_" + pos);
				if ( param == null || param.length() <= 0) {
					break;
				}
				valueList.add(param);
				pos++;
			}

			// 入力された値で属性値を更新する
			if (valueList != null && valueList.size() > 0 ) {
				switch (attType.getValueType().getId()) {
					// 数値型
					case EIMValueType.INTEGER:
						long[] intValueList = new long[valueList.size()];
						
						for(int j=0; j<valueList.size(); j++)
						{
							intValueList[j] = Long.parseLong(valueList.get(j));
						}
						ObjectAttributeUtils.setAttribute(sess, objectUser, attType, TypeConvertUtils.convertToBuildTypeArray(intValueList));
						
						break;
					// 文字列型とテキスト型
					case EIMValueType.STRING:
					case EIMValueType.TEXT:
						String[] strValueList = (String[])valueList.toArray(new String[0]);
						ObjectAttributeUtils.setAttribute(sess, objectUser, attType, strValueList);
						break;
					// 日付型
					case EIMValueType.DATE:
						// 既に登録済みの値で、ログインユーザが当該属性値を変更していない場合でも、画面の表示値を新たな入力値として、
						// ログインユーザのクライアント日時からDBサーバ日時に変換した上でDB更新する。
						Date[] dateValueList = new Date[valueList.size()];
						for(int j=0; j<valueList.size(); j++)
						{						
							// GMT変換
							Date a = StringUtils.getDateFromString(valueList.get(j), EIMResource.getMessage(sess, "EIM.FORMAT.DATE"));
							dateValueList[j] = DateUtils.editExpirationDate(sess, a);
							
						}
						ObjectAttributeUtils.setAttribute(sess, objectUser, attType, dateValueList);

						break;

					// ダブル型
					case EIMValueType.DOUBLE:
						double[] doubleValueList = new double[valueList.size()];
						for(int j=0; j<valueList.size(); j++)
						{
							doubleValueList[j] = Double.parseDouble(valueList.get(j));
						}
						ObjectAttributeUtils.setAttribute(sess, objectUser, attType, doubleValueList);
						break;
				}
			}
		}

		//Create Operation History
		OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.CREATE_USER,
				EIMConstant.TARGET_CREATE, EIMConstant.USER, user,
				null, null, null, null );

		//Commit
		sess.commit();

		//かな
		String userKana = "";
		if(user.getKana() != null)
		{
			userKana = user.getKana();
		}

		//Mail
		String userMail = "";
		if(user.getMail() != null)
		{
			userMail = user.getMail();
		}

		String disable = "";
		if (user.getDisable() == 1)
		{
			disable = "on";
		}
		else
		{
			disable = "off";
		}

		//XML
		out.println("<user");
			out.println(" userId=\"" + user.getId() + "\"");
			out.println(" userCode=\"" + StringUtils.xmlEncode(user.getCode()) + "\"");
			out.println(" userName=\"" + StringUtils.xmlEncode(nowLangName) + "\"");
			out.println(" userKana=\"" + StringUtils.xmlEncode(userKana) + "\"");
			out.println(" userMail=\"" + StringUtils.xmlEncode(userMail) + "\"");
			out.println(" userAdmin=\"" + user.getAdmin() + "\"");
			out.println(" userLang=\"" + StringUtils.xmlEncode(user.getLang()) + "\"");
			out.println(" userDisable=\"" + disable + "\"");
			out.println(">");
		out.println("</user>");

	}
	catch(EIMSysException eimse) {
		out.clear();
		message = EIMResource.getMessageValue(sess, "EIM.ERROR.SYSTEMERROR");
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.error(AppMessageUtils.makeLogMessage(loginUser.getId(), eimse.getMessage(), paramId), eimse);
		try{
			if(sess != null){
				sess.rollback();
			}
		}
		catch (Exception se) {
			out.clear();
			message = EIMResource.getMessageValue(sess, "EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
	}
	catch(EIMAppException eimae)
	{
		out.clear();
		message = eimae.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), eimae.getMessage(), paramId), eimae);
		try{
			if(sess != null){
				sess.rollback();
			}
		}
		catch (Exception se) {
			out.clear();
			message = EIMResource.getMessageValue(sess, "EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), eime.getMessage(), paramId), eime);
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
