<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "common.util.*" %>
<%@ page import = "common.bo.*" %>

<%@ page import = "java.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>

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
	String prmAttTypeId = EIMUtils.getParameter(request, "attTypeId");
	int prmValType = Integer.parseInt(EIMUtils.getParameter(request, "valType"));
	String prmBeforeValue = EIMUtils.getParameter(request, "beforeValue");
	String prmAfterValue = EIMUtils.getParameter(request, "afterValue");
	boolean prmIsDspColor = Boolean.valueOf(EIMUtils.getParameter(request, "isDspColor")).booleanValue();
	String prmColor = prmIsDspColor? EIMUtils.getParameter(request, "color") : "-";

	//Message
	String message = null;
	Object[] paramId = {
			"attTypeId=" + prmAttTypeId,
			"valType=" + prmValType,
			"beforeValue=" + prmBeforeValue,
			"afterValue=" + prmAfterValue,
			"isDspColor=" + prmIsDspColor,
			"color=" + prmColor
			};

	try
	{
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
		if(!AdminAuthUtil.hasSpecifiedOrGeneralAuth(loginUser, AppConstant.ADMIN_AUTH_ID_ATTRIBUTE))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.NOTADMINISTRATOR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.NOTADMINISTRATOR");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message));
			return;
		}

		//Get Attribute Type
		EIMAttributeType attType = AttributeUtils.getAttributeTypeById(sess, Long.parseLong(prmAttTypeId));

		if (attType == null) {
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.ATTTYPE.NOTFOUND");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.ATTTYPE.NOTFOUND");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}

		//Get Attribute Value Master
		AttributeValueMaster attValMst = AttributeMasterUtil.getAttributeValueMasterByAttTypeId(sess, attType.getId());

		if (attValMst == null) {
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.ATTRIBUTE.NONE.MASTER");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.ATTRIBUTE.NONE.MASTER");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}

		StringBuffer strBuf = new StringBuffer();
		boolean isNoValueUpdate = false;
		boolean isNotFoundError = true;
		boolean isExistsError = false;
		long inputValueInt;
		double inputValueDouble;
		int updateIndex = 0;

		Date beforeValueDate = null;
		Date afterValueDate = null;
		long[] intValues = null;
		double[] doubleValues = null;
		String[] strValues = null;
		Date[] dateValues = null;
		String[] textValues = null;
		List valueList = new ArrayList();
		List colorList = new ArrayList();
		List settingList = new ArrayList();

		strBuf.append("<definitionValuesList>");

		try
		{
			// 対象リスト値に変更がない場合
			if (prmBeforeValue.equals(prmAfterValue)) {
				isNoValueUpdate = true;
			}

			String[] colorGetValues = null;
			long[] settingGetValues = null;

			switch (prmValType) {
			// 数値型
			case EIMValueType.INTEGER:

				inputValueInt = Long.parseLong(prmAfterValue);

				long[] intGetValues = attValMst.getInts();
				colorGetValues = attValMst.getColors();
				settingGetValues = attValMst.getSettings();

				for (int i = 0; i < intGetValues.length; i++) {

					// 更新対象リスト値存在チェック
					if (intGetValues[i] == Long.parseLong(prmBeforeValue)) {

						isNotFoundError = false;
						updateIndex = i;

						// 重複チェック
						for (int j = 0; j < intGetValues.length; j++) {

							if (!isNoValueUpdate && (intGetValues[j] == inputValueInt)) {
								isExistsError = true;
								break;
							}

							valueList.add(new Long(intGetValues[j]));
							colorList.add(colorGetValues[j]);
							settingList.add(new Long(settingGetValues[j]));
						}
						break;
					}
				}

				if (!isNotFoundError && !isExistsError) {

					valueList.set(updateIndex, new Long(inputValueInt));
					colorList.set(updateIndex, prmColor);
					settingList.set(updateIndex, prmIsDspColor? new Long(1) : new Long(0));

					intValues = new long[valueList.size()];
					colorGetValues = (String[])colorList.toArray(new String[colorList.size()]);

					for (int i = 0; i < valueList.size(); i++) {

						Long inte = (Long)valueList.get(i);
						intValues[i] = inte.longValue();

						strBuf.append("<definitionValue");
							strBuf.append(" value=\"" + intValues[i] + "\"");
							strBuf.append(" color=\"" + colorGetValues[i] + "\"");
						strBuf.append(">");
						strBuf.append("</definitionValue>");
					}

					// Update Master
					AttributeMasterUtil.setIntAttributeValues(sess, attValMst, intValues);

					// 表示色をDBに格納
					AttributeMasterUtil.setColorAttributeValues(sess, attValMst, colorList);
					// 表示設定をDBに格納
					AttributeMasterUtil.setSettingAttributeValues(sess, attValMst, settingList);
				}

				break;
			// 文字列型
			case EIMValueType.STRING:

				String[] strGetValues = attValMst.getStrings();
				colorGetValues = attValMst.getColors();
				settingGetValues = attValMst.getSettings();

				for (int i = 0; i < strGetValues.length; i++) {

					// 更新対象リスト値存在チェック
					if (strGetValues[i].equals(prmBeforeValue)) {

						isNotFoundError = false;
						updateIndex = i;

						// 重複チェック
						for (int j = 0; j < strGetValues.length; j++) {

							if (!isNoValueUpdate && (strGetValues[j].equals(prmAfterValue))) {
								isExistsError = true;
								break;
							}

							valueList.add(strGetValues[j]);
							colorList.add(colorGetValues[j]);
							settingList.add(new Long(settingGetValues[j]));
						}
						break;
					}
				}

				if (!isNotFoundError && !isExistsError) {

					valueList.set(updateIndex, prmAfterValue);
					colorList.set(updateIndex, prmColor);
					settingList.set(updateIndex, prmIsDspColor? new Long(1) : new Long(0));

					strValues = (String[])valueList.toArray(new String[valueList.size()]);
					colorGetValues = (String[])colorList.toArray(new String[colorList.size()]);

					for (int i = 0; i < valueList.size(); i++) {

						strBuf.append("<definitionValue");
							strBuf.append(" value=\"" + StringUtils.xmlEncode(strValues[i]) + "\"");
							strBuf.append(" color=\"" + colorGetValues[i] + "\"");
						strBuf.append(">");
						strBuf.append("</definitionValue>");
					}

					// Update Master
					AttributeMasterUtil.setStrAttributeValues(sess, attValMst, strValues);

					// 表示色をDBに格納
					AttributeMasterUtil.setColorAttributeValues(sess, attValMst, colorList);
					// 表示設定をDBに格納
					AttributeMasterUtil.setSettingAttributeValues(sess, attValMst, settingList);
				}

				break;
			// 日付型
			case EIMValueType.DATE:

				// 更新対象リスト値(DB登録用に変換)
				beforeValueDate = DateUtils.editExpirationDate(sess,
						StringUtils.getDateFromString(prmBeforeValue, EIMResource.getMessage(sess, "EIM.FORMAT.DATE")));
				// 更新対象リスト値(判定用に変換)
				String judgeBeforeValue = StringUtils.getDateStringByFormat(beforeValueDate, EIMResource.getMessage(sess, "EIM.FORMAT.DATE"));

				// 更新リスト値(DB登録用に変換)
				afterValueDate = DateUtils.editExpirationDate(sess,
						StringUtils.getDateFromString(prmAfterValue, EIMResource.getMessage(sess, "EIM.FORMAT.DATE")));
                // 更新対象リスト値(判定用に変換)
				String judgeAfterValue = StringUtils.getDateStringByFormat(afterValueDate, EIMResource.getMessage(sess, "EIM.FORMAT.DATE"));

				Date[] dateGetValues = attValMst.getDates();
				colorGetValues = attValMst.getColors();
				settingGetValues = attValMst.getSettings();

				for (int i = 0; i < dateGetValues.length; i++) {

					String judgeGetValue = StringUtils.getDateStringByFormat(dateGetValues[i], EIMResource.getMessage(sess, "EIM.FORMAT.DATE"));

					// 更新対象リスト値存在チェック
					if (judgeGetValue.equals(judgeBeforeValue)) {

						isNotFoundError = false;
						updateIndex = i;

						// 重複チェック
						for (int j = 0; j < dateGetValues.length; j++) {

							String judgeGetValue2 = StringUtils.getDateStringByFormat(dateGetValues[j], EIMResource.getMessage(sess, "EIM.FORMAT.DATE"));

							if (!isNoValueUpdate && (judgeGetValue2.equals(judgeAfterValue))) {
								isExistsError = true;
								break;
							}

							valueList.add(dateGetValues[j]);
							colorList.add(colorGetValues[j]);
							settingList.add(new Long(settingGetValues[j]));
						}
						break;
					}
				}

				if (!isNotFoundError && !isExistsError) {

					valueList.set(updateIndex, afterValueDate);
					colorList.set(updateIndex, prmColor);
					settingList.set(updateIndex, prmIsDspColor? new Long(1) : new Long(0));

					dateValues = (Date[])valueList.toArray(new Date[valueList.size()]);
					colorGetValues = (String[])colorList.toArray(new String[colorList.size()]);

					for (int i = 0; i < valueList.size(); i++) {

						strBuf.append("<definitionValue");
							strBuf.append(" value=\"" + DateUtils.getDBTzToCLTzDate(sess, dateValues[i], "EIM.FORMAT.DATE") + "\"");
							strBuf.append(" color=\"" + colorGetValues[i] + "\"");
						strBuf.append(">");
						strBuf.append("</definitionValue>");
					}

					// Update Master
					AttributeMasterUtil.setDateAttributeValues(sess, attValMst, dateValues);

					// 表示色をDBに格納
					AttributeMasterUtil.setColorAttributeValues(sess, attValMst, colorList);
					// 表示設定をDBに格納
					AttributeMasterUtil.setSettingAttributeValues(sess, attValMst, settingList);
				}

				break;
			// テキスト型
			case EIMValueType.TEXT:

				String[] textGetValues = attValMst.getTexts();
				colorGetValues = attValMst.getColors();
				settingGetValues = attValMst.getSettings();

				for (int i = 0; i < textGetValues.length; i++) {

					// 更新対象リスト値存在チェック
					if (textGetValues[i].equals(prmBeforeValue)) {

						isNotFoundError = false;
						updateIndex = i;

						// 重複チェック
						for (int j = 0; j < textGetValues.length; j++) {

							if (!isNoValueUpdate && (textGetValues[j].equals(prmAfterValue))) {
								isExistsError = true;
								break;
							}

							valueList.add(textGetValues[j]);
							colorList.add(colorGetValues[j]);
							settingList.add(new Long(settingGetValues[j]));
						}
						break;
					}
				}

				if (!isNotFoundError && !isExistsError) {

					valueList.set(updateIndex, prmAfterValue);
					colorList.set(updateIndex, prmColor);
					settingList.set(updateIndex, prmIsDspColor? new Long(1) : new Long(0));

					textValues = (String[])valueList.toArray(new String[valueList.size()]);
					colorGetValues = (String[])colorList.toArray(new String[colorList.size()]);

					for (int i = 0; i < valueList.size(); i++) {

						strBuf.append("<definitionValue");
							strBuf.append(" value=\"" + StringUtils.xmlEncode(textValues[i]) + "\"");
							strBuf.append(" color=\"" + colorGetValues[i] + "\"");
						strBuf.append(">");
						strBuf.append("</definitionValue>");
					}

					// Update Master
					AttributeMasterUtil.setTextAttributeValues(sess, attValMst, textValues);

					// 表示色をDBに格納
					AttributeMasterUtil.setColorAttributeValues(sess, attValMst, colorList);
					// 表示設定をDBに格納
					AttributeMasterUtil.setSettingAttributeValues(sess, attValMst, settingList);
				}

				break;

				// 実数型
			case EIMValueType.DOUBLE:

				inputValueDouble = Double.parseDouble(prmAfterValue);
				double oldDoubleValue = Double.parseDouble(prmBeforeValue);
				if(inputValueDouble == oldDoubleValue) {
					isNoValueUpdate = true;
				}

				double[] doubleGetValues = attValMst.getDoubles();
				colorGetValues = attValMst.getColors();
				settingGetValues = attValMst.getSettings();

				for (int i = 0; i < doubleGetValues.length; i++) {

					// 更新対象リスト値存在チェック
					if (doubleGetValues[i] == Double.parseDouble(prmBeforeValue)) {

						isNotFoundError = false;
						updateIndex = i;

						// 重複チェック
						for (int j = 0; j < doubleGetValues.length; j++) {

							if (!isNoValueUpdate && (doubleGetValues[j] == inputValueDouble)) {
								isExistsError = true;
								break;
							}

							valueList.add(new Double(doubleGetValues[j]));
							colorList.add(colorGetValues[j]);
							settingList.add(new Long(settingGetValues[j]));
						}
						break;
					}
				}

				if (!isNotFoundError && !isExistsError) {

					valueList.set(updateIndex, new Double(inputValueDouble));
					colorList.set(updateIndex, prmColor);
					settingList.set(updateIndex, prmIsDspColor? new Long(1) : new Long(0));

					doubleValues = new double[valueList.size()];
					colorGetValues = (String[])colorList.toArray(new String[colorList.size()]);

					for (int i = 0; i < valueList.size(); i++) {

						doubleValues[i] = (Double)valueList.get(i);

						strBuf.append("<definitionValue");
							strBuf.append(" value=\"" + FormatUtil.getDoubleFormatedString(doubleValues[i]) + "\"");
							strBuf.append(" color=\"" + colorGetValues[i] + "\"");
						strBuf.append(">");
						strBuf.append("</definitionValue>");
					}

					// Update Master
					AttributeMasterUtil.setDoubleAttributeValues(sess, attValMst, doubleValues);

					// 表示色をDBに格納
					AttributeMasterUtil.setColorAttributeValues(sess, attValMst, colorList);
					// 表示設定をDBに格納
					AttributeMasterUtil.setSettingAttributeValues(sess, attValMst, settingList);
				}

				break;
			}
		}
		catch (EIMException eime)
		{
			// Case get No Value
			if (eime.getMessageKey().equals("EIM.ERROR.COMMON.API.NO.ATTRIBUTE.MASTER.VALUE")) {
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.ATTRLISTVALUE.NOTFOUND");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.ATTRLISTVALUE.NOTFOUND");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				return;
			}
		}
		catch(Exception e)
		{
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
			return;
		}


        // End Root Node
		strBuf.append("</definitionValuesList>");

		// 更新対象リスト値存在なしエラー
		if (isNotFoundError) {
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.ATTRLISTVALUE.NOTFOUND");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.ATTRLISTVALUE.NOTFOUND");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}
  		// 重複エラー
		else if (isExistsError) {
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.ATTRLISTVALUE.EXISTS");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.ATTRLISTVALUE.EXISTS");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		} else {
			out.println(strBuf);
		}

		//Create Operation History
		OperationHistoryUtils.create(sess, AppConstant.SYSTEM,
				EIMConstant.UPDATE_ATTRIBUTE_MASTER, EIMConstant.TARGET_PARENT_ATTRIBUTE,
				EIMConstant.ATTRIBUTE_TYPE, attType,
				EIMConstant.TARGET_UPDATE, EIMConstant.ATTRIBUTE_MASTER,
				prmAfterValue, null);

		//Commit
		sess.commit();

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
