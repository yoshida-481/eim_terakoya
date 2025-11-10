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
	String prmRoleId = EIMUtils.getParameter(request, "roleId");
	String roleParentId = EIMUtils.getParameter(request, "roleParentId");

	//Message
	String message = null;
	Object[] paramId = {
			"roleId=" + prmRoleId,
			"roleParentId=" + roleParentId
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

		//Role
		EIMRole role = RoleUtils.getRoleById(sess, Long.parseLong(prmRoleId), true);
		if(role == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.ROLE.NOTFOUND");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.ROLE.NOTFOUND");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}

		//Parent Role
		long prmRoleParentId = 0;
		EIMRole parentRole = null;
		if(roleParentId != null && !roleParentId.equals("")) {

			prmRoleParentId = Long.parseLong(roleParentId);
			parentRole = RoleUtils.getRoleById(sess, prmRoleParentId);

			if(parentRole == null)
			{
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.ROLE.NOTFOUND");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.ROLE.NOTFOUND");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				return;
			}

			// 親に指定したロールが自分に属するロールの場合はエラー
			if (AppObjectUtil.isChildRole(sess, parentRole, role)) {
				// 親ロールに設定したロールは、選択ロールに属するロールです。
				message = EIMResource.getMessage(sess, "EIM.ERROR.INPUT.NOTSETPARENTROLE");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.INPUT.NOTSETPARENTROLE");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				return;
			}
		}

		//Get old info
		long oldParentRoleId = role.getParentId();
		EIMRole oldParentRole = null;
		if(oldParentRoleId != 0)
		{
			oldParentRole = RoleUtils.getRoleById(sess, oldParentRoleId);
		}

		/*
		 * Update Role
		 */
		int prmOtherCnt = LanguageFieldUtil.getOtherCnt(request);
		String prmDefName = LanguageFieldUtil.getDefName(request, prmOtherCnt);

		//Update
		role = RoleUtils.updateRole(sess, role, prmDefName, prmRoleParentId);

		/*
		 * Update Role Other
		 */
		for(int i = 0; i < prmOtherCnt; i++)
		{
			String prmOtherName = LanguageFieldUtil.getOtherName(request,i);
			String prmOtherLId = LanguageFieldUtil.getOtherLId(request,i);

			//Update
			RoleUtils.updateOtherRoleName(sess, role.getId(), prmOtherLId, prmOtherName);
		}

		// オブジェクトタイプから属性を取得
		// List<EIMAttributeType> ...
		EIMObject objectRole = role.getRoleObject();
		List attTypes = ObjectAttributeUtils.getAttributeTypeList(sess, objectRole.getType());

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
					ObjectAttributeUtils.deleteAttribute(sess, objectRole, attType);
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
						ObjectAttributeUtils.setAttribute(sess, objectRole, attType, TypeConvertUtils.convertToBuildTypeArray(intValueList));
						
						break;
					// 文字列型とテキスト型
					case EIMValueType.STRING:
					case EIMValueType.TEXT:
						String[] strValueList = (String[])valueList.toArray(new String[0]);
						ObjectAttributeUtils.setAttribute(sess, objectRole, attType, strValueList);
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
						ObjectAttributeUtils.setAttribute(sess, objectRole, attType, dateValueList);

						break;

					// ダブル型
					case EIMValueType.DOUBLE:
						double[] doubleValueList = new double[valueList.size()];
						for(int j=0; j<valueList.size(); j++)
						{
							doubleValueList[j] = Double.parseDouble(valueList.get(j));
						}
						ObjectAttributeUtils.setAttribute(sess, objectRole, attType, doubleValueList);
						break;
				}
			}
		}
		
		// SearchFramework 検索FW更新通知 対象：ロール
		AppUpdateNoticeUtils.updateNoticeInsert(role.getId(), "SEARCHFW_USER_EDIT_ROLE");
		
		OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.UPDATE_ROLE,
				EIMConstant.TARGET_UPDATE, EIMConstant.ROLE, role,
				null, null, null, null);

		if(parentRole != null)
		{
			if(oldParentRole != null)
			{
				//Change parent group
				if(oldParentRole.getId() != parentRole.getId())
				{
					OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.UPDATE_PARENT_ROLE,
							EIMConstant.TARGET_PARENT_ROLE, EIMConstant.ROLE, parentRole,
							EIMConstant.TARGET_UPDATE, EIMConstant.ROLE, role, null);
				}
			}
			//Add parent group
			else
			{
				OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.UPDATE_PARENT_ROLE,
						EIMConstant.TARGET_PARENT_ROLE, EIMConstant.ROLE, parentRole,
						EIMConstant.TARGET_UPDATE, EIMConstant.ROLE, role, null);
			}
		}
		else
		{
			//Delete parent group
			if(oldParentRole != null)
			{
				OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.UPDATE_PARENT_ROLE,
						EIMConstant.TARGET_PARENT_ROLE, null, null,
						EIMConstant.TARGET_UPDATE, EIMConstant.ROLE, role, null);
			}
		}


		//Commit
		sess.commit();

		//XML
		out.println("<role");
			out.println(" roleId=\"" + role.getId() + "\"");
			out.println(" roleParentId=\"" + role.getParentId() + "\"");
			out.println(" roleName=\"" + StringUtils.xmlEncode(role.getName()) + "\"");
			out.println(">");
		out.println("</role>");

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
