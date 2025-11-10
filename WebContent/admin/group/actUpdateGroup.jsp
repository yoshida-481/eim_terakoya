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
	String prmGroupId = EIMUtils.getParameter(request, "groupId");
	String prmParentGroupId = EIMUtils.getParameter(request, "parentGroupId");
	String prmDefinitionName = EIMUtils.getParameter(request, "definitionName");

	//Message
	String message = null;
	Object[] paramId = {
			"groupId=" + prmGroupId,
			"parentGroupId=" + prmParentGroupId
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

		//Group
		EIMGroup group = GroupUtils.getGroupById(sess, Long.parseLong(prmGroupId), true);
		if(group == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.GROUP.NOTFOUND");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.GROUP.NOTFOUND");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}

		//Parent Group
		EIMGroup parentGroup = null;
		if(prmParentGroupId != null && !prmParentGroupId.equals(""))
		{
			parentGroup = GroupUtils.getGroupById(sess, Long.parseLong(prmParentGroupId));
			if(parentGroup == null)
			{
				message = EIMResource.getMessage(sess, "EIM.ERROR.GROUP.NOTFOUND");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.GROUP.NOTFOUND");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				return;
			}

			// 親に指定したグループが自分に属するグループの場合はエラー
			if (AppObjectUtil.isChildGroup(sess, parentGroup, group)) {
				// 親グループに設定したグループは、選択グループに属するグループです。
				message = EIMResource.getMessage(sess, "EIM.ERROR.INPUT.NOTSETPARENTGROUP");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.INPUT.NOTSETPARENTGROUP");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				return;
			}
		}

		//Get old info
		EIMGroup oldParentGroup = group.getParent();

		/*
		 * Update Group
		 */
		int prmOtherCnt = LanguageFieldUtil.getOtherCnt(request);

		//Update
		group = GroupUtils.updateGroup(sess, group, prmDefinitionName, parentGroup);

		/*
		 * Update Group Other
		 */
		for(int i = 0; i < prmOtherCnt; i++)
		{
			String prmOtherName = LanguageFieldUtil.getOtherName(request,i);
			String prmOtherLId = LanguageFieldUtil.getOtherLId(request,i);

			//Update
			GroupUtils.updateOtherGroupName(sess, group.getId(), prmOtherLId, prmOtherName);
		}

		// オブジェクトタイプから属性を取得
		// List<EIMAttributeType> ...
		EIMObject objectGroup = group.getGroupObject();
		List attTypes = ObjectAttributeUtils.getAttributeTypeList(sess, objectGroup.getType());

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
				// オブジェクト型属性の時は、属性値を削除しない。
				// コード型属性とユーザ型属性については、deleteAttributeで削除していない為、当該の条件で対処としない。
				if (attType.getValueType().getId() != EIMValueType.OBJECT ){
					// ない場合
					ObjectAttributeUtils.deleteAttribute(sess, objectGroup, attType);
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
						ObjectAttributeUtils.setAttribute(sess, objectGroup, attType, TypeConvertUtils.convertToBuildTypeArray(intValueList));
						
						break;
					// 文字列型とテキスト型
					case EIMValueType.STRING:
					case EIMValueType.TEXT:
						String[] strValueList = (String[])valueList.toArray(new String[0]);
						ObjectAttributeUtils.setAttribute(sess, objectGroup, attType, strValueList);
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
						ObjectAttributeUtils.setAttribute(sess, objectGroup, attType, dateValueList);

						break;

					// ダブル型
					case EIMValueType.DOUBLE:
						double[] doubleValueList = new double[valueList.size()];
						for(int j=0; j<valueList.size(); j++)
						{
							doubleValueList[j] = Double.parseDouble(valueList.get(j));
						}
						ObjectAttributeUtils.setAttribute(sess, objectGroup, attType, doubleValueList);
						break;
				}
			}
		}
		
		// SearchFramework 検索FW更新通知 対象：グループ
		AppUpdateNoticeUtils.updateNoticeInsert(group.getId(), "SEARCHFW_USER_EDIT_GROUP");
		
		//Create Operation History
		OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.UPDATE_GROUP,
				EIMConstant.TARGET_UPDATE, EIMConstant.GROUP, group,
				null, null, null, null);

		if(parentGroup != null)
		{
			if(oldParentGroup != null)
			{
				//Change parent group
				if(oldParentGroup.getId() != parentGroup.getId())
				{
					OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.UPDATE_PARENT_GROUP,
							EIMConstant.TARGET_PARENT_GROUP, EIMConstant.GROUP, parentGroup,
							EIMConstant.TARGET_UPDATE, EIMConstant.GROUP, group, null);
				}
			}
			//Add parent group
			else
			{
				OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.UPDATE_PARENT_GROUP,
						EIMConstant.TARGET_PARENT_GROUP, EIMConstant.GROUP, parentGroup,
						EIMConstant.TARGET_UPDATE, EIMConstant.GROUP, group, null);
			}
		}
		else
		{
			//Delete parent group
			if(oldParentGroup != null)
			{
				OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.UPDATE_PARENT_GROUP,
						EIMConstant.TARGET_PARENT_GROUP, null, null,
						EIMConstant.TARGET_UPDATE, EIMConstant.GROUP, group, null);
			}
		}

		//Commit
		sess.commit();

		//XML
		out.println("<group");
			out.println(" groupId=\"" + group.getId() + "\"");
			out.println(" groupName=\"" + StringUtils.xmlEncode(GroupUtils.getOtherGroupName(sess, group.getId(), sess.getLangId())) + "\"");
			out.println(" parentGroupName=\"" + StringUtils.xmlEncode(EntryUtil.getParentGroupName(group)) + "\"");
			out.println(">");
		out.println("</group>");
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
