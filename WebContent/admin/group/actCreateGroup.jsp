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
	String prmParentGroupId = request.getParameter("parentGroupId");
	String prmDefinitionName = EIMUtils.getParameter(request, "definitionName");

	//Message
	String message = null;
	Object[] paramId = {
			"parentGroupId=" + prmParentGroupId
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
		}

		/*
		 * Create Group
		 */
		int prmOtherCnt = LanguageFieldUtil.getOtherCnt(request);
		
		//Create
		EIMGroup group = GroupUtils.createGroup(sess, prmDefinitionName, parentGroup);

		/*
		 * Create Group Other
		 */
		for(int i = 0; i < prmOtherCnt; i++)
		{
			String prmOtherName = LanguageFieldUtil.getOtherName(request,i);
			String prmOtherLId = LanguageFieldUtil.getOtherLId(request,i);

			// Create Object Type
			GroupUtils.addOtherGroupName(sess, group.getId(), prmOtherLId, prmOtherName);
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
		
		//Create Operation History
		if(parentGroup != null)
		{
			OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.CREATE_GROUP,
					EIMConstant.TARGET_PARENT_GROUP, EIMConstant.GROUP, parentGroup,
					EIMConstant.TARGET_CREATE, EIMConstant.GROUP, group, null );
		}
		else
		{
			OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.CREATE_GROUP,
					EIMConstant.TARGET_CREATE, EIMConstant.GROUP, group,
					null, null, null, null );
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
