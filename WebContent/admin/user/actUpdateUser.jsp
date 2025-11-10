<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>
<%@ page import = "jp.co.ctc_g.eim.framework.common.exception.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>
<%@ page import = "jp.co.ctc_g.eim.framework.common.util.EIMThreadContext"%>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	//Session
	EIMSession sess = null;
	EIMUser loginUser = null;
	boolean sessPutFlag = false;
	
	//Parameter
	String prmUserId = EIMUtils.getParameter(request, "userId");
	int prmOtherCnt = LanguageFieldUtil.getOtherCnt(request);
	String prmDefName = LanguageFieldUtil.getDefName(request, prmOtherCnt);
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
			"userId=" + prmUserId,
			"otherCnt=" + prmOtherCnt,
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
		 * Parameter check
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
		
		// ThreadLocalコンテキストにセッションを設定
		if(EIMThreadContext.getEIMSession() == null)
		{
			EIMThreadContext.putEIMSession(sess);
			sessPutFlag = true;
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

		//User
		EIMUser user = UserUtils.getUserById(sess, Long.parseLong(prmUserId), true);
		if(user == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.USER.NOTFOUND");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.USER.NOTFOUND");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}

		if(prmUserDisable.equals("1"))
		{
			if(user.getId() == AppConstant.SYSYEM_USER_ID)
			{
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOBELONG.INVALIDITY.USER");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOBELONG.INVALIDITY.USER");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				return;
			}
		}

		//Update
		/*
		 * Update User
		 */
		user = UserUtils.updateUser(sess,
									user,
									prmUserCode,
									prmDefName,
									prmUserKana,
									prmUserPass,
									prmUserMail,
									Integer.parseInt(prmUserAdmin),
									Integer.parseInt(prmUserDisable),
									prmUserLang);

		/*
		 * Update User Other
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

			//Update
			UserUtils.updateOtherUserName(sess, user.getId(), prmOtherLId, prmOtherName);
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
			
			// パラメータとの比較
			if (valueList == null || valueList.size() == 0 ) {
				if (attType.getValueType().getId() != EIMValueType.OBJECT ){
					// ない場合
					ObjectAttributeUtils.deleteAttribute(sess, objectUser, attType);
				}
			} else {
				// ある場合
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
		
		// SearchFramework 検索FW更新通知 対象：ユーザ
		AppUpdateNoticeUtils.updateNoticeInsert(user.getId(), "SEARCHFW_USER_EDIT_USER");
		
		// ユーザを無効にした場合にセキュリティのアクセスエントリから削除するために各更新通知を呼ぶ
		
		List groups = GroupUtils.getGroupByUser(sess, user);
		for (int i = 0; i < groups.size(); i++) {
			EIMGroup group = (EIMGroup)groups.get(i);
			// SearchFramework 検索FW更新通知 対象：グループ
			AppUpdateNoticeUtils.updateNoticeInsert(group.getId(), "SEARCHFW_USER_EDIT_GROUP");
		}
		
		List roles = RoleUtils.getRoleByUser(sess, user);
		for (int j = 0; j < roles.size(); j++) {
			EIMRole role = (EIMRole)roles.get(j);
			// SearchFramework 検索FW更新通知 対象：ロール
			AppUpdateNoticeUtils.updateNoticeInsert(role.getId(), "SEARCHFW_USER_EDIT_ROLE");
		}
		
		// 全セキュリティを取得し、アクセスエントリのユーザを比較して対象のセキュリティを取得
		List<EIMSecurity> securityList = SecurityUtils.getSecurityList(sess);
		for (EIMSecurity security : securityList) {
			
			// 全セキュリティ取得
			List<EIMAccessEntry> accessEntryList = SecurityUtils.getAccessEntryList(sess, security);
			for (EIMAccessEntry accessEntry : accessEntryList) {
				
				// ユーザチェック
				if (accessEntry.getUser() == null ) {
					continue;
				}
				if (accessEntry.getUser().getId() == user.getId()) {
					// SearchFramework 検索FW更新通知 対象：セキュリティ
					AppUpdateNoticeUtils.updateNoticeInsert(security.getId(), "SEARCHFW_SECURITY_DELACENTRY_SECURITY");
					break;
				}
			}
		}
		
		OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.UPDATE_USER,
				EIMConstant.TARGET_UPDATE, EIMConstant.USER, user,
				null, null, null, null);

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

		//所属グループID、ロール名称の取得
		String[] groupIdName = AppUserUtil.getBelongGroupIdName(sess, user);
		String groupId = groupIdName[0];
		String groupName = groupIdName[1];

		//所属ロールID、ロール名称の取得
		String[] roleIdName = AppUserUtil.getBelongRoleIdName(sess, user);
		String roleId = roleIdName[0];
		String roleName = roleIdName[1];

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
			out.println(" userPass=\"" + StringUtils.xmlEncode(user.getPass()) + "\"");
			out.println(" userMail=\"" + StringUtils.xmlEncode(userMail) + "\"");
			out.println(" userAdmin=\"" + user.getAdmin() + "\"");
			out.println(" userLang=\"" + user.getLang() + "\"");
			out.println(" groupId=\"" + groupId + "\"");
			out.println(" groupName=\"" + StringUtils.xmlEncode(groupName) + "\"");
			out.println(" roleId=\"" + roleId + "\"");
			out.println(" roleName=\"" + StringUtils.xmlEncode(roleName) + "\"");
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
			if(sessPutFlag) {
				EIMThreadContext.removeEIMSession();
				sessPutFlag = false;
			}
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