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
	String prmValue = EIMUtils.getParameter(request, "value");
	int prmDropIndex = Integer.parseInt(EIMUtils.getParameter(request, "dropIndex"));
	String prmColor = EIMUtils.getParameter(request, "color");
	Long displaySetting = prmColor=="-"? new Long(0):new Long(1);

	//Message
	String message = null;
	Object[] paramId = {
			"attTypeId=" + prmAttTypeId,
			"valType=" + prmValType,
			"value=" + prmValue,
			"dropIndex=" + prmDropIndex,
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
		boolean isNotFoundError = true;
		long dropValueInt;
		double dropValueDouble;

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
			String[] colorGetValues = null;
			long[] settingGetValues = null;

			switch (prmValType) {
			// 数値型
			case EIMValueType.INTEGER:

				dropValueInt = Long.parseLong(prmValue);

				long[] intGetValues = attValMst.getInts();
				colorGetValues = attValMst.getColors();
				settingGetValues = attValMst.getSettings();

				for (int i = 0; i < intGetValues.length; i++) {

					// 順序更新対象リスト値存在チェック
					if (intGetValues[i] == dropValueInt) {
						isNotFoundError = false;
					} else {
						valueList.add(new Long(intGetValues[i]));
						colorList.add(colorGetValues[i]);
						settingList.add(new Long(settingGetValues[i]));
					}
				}

				if (!isNotFoundError) {

					if (prmDropIndex <= valueList.size()) {
						valueList.add(prmDropIndex, new Long(dropValueInt));
						colorList.add(prmDropIndex, prmColor);
						settingList.add(displaySetting);
					} else {
						valueList.add(new Long(dropValueInt));
						colorList.add(prmColor);
						settingList.add(displaySetting);
					}

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

					// 順序更新対象リスト値存在チェック
					if (strGetValues[i].equals(prmValue)) {
						isNotFoundError = false;
					} else {
						valueList.add(strGetValues[i]);
						colorList.add(colorGetValues[i]);
						settingList.add(new Long(settingGetValues[i]));
					}
				}

				if (!isNotFoundError) {

					if (prmDropIndex <= valueList.size()) {
						valueList.add(prmDropIndex, prmValue);
						colorList.add(prmDropIndex, prmColor);
						settingList.add(displaySetting);
					} else {
						valueList.add(prmValue);
						colorList.add(prmColor);
						settingList.add(displaySetting);
					}

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

				// 序更新対象リスト値(DB登録用に変換)
				Date dropValueDate = DateUtils.editExpirationDate(sess,
						StringUtils.getDateFromString(prmValue, EIMResource.getMessage(sess, "EIM.FORMAT.DATE")));
				// 序更新対象リスト値(判定用に変換)
				String judgeDropValue = StringUtils.getDateStringByFormat(dropValueDate, EIMResource.getMessage(sess, "EIM.FORMAT.DATE"));

				Date[] dateGetValues = attValMst.getDates();
				colorGetValues = attValMst.getColors();
				settingGetValues = attValMst.getSettings();

				for (int i = 0; i < dateGetValues.length; i++) {

					String judgeGetValue = StringUtils.getDateStringByFormat(dateGetValues[i], EIMResource.getMessage(sess, "EIM.FORMAT.DATE"));

					// 順序更新対象リスト値存在チェック
					if (judgeGetValue.equals(judgeDropValue)) {
						isNotFoundError = false;
					} else {
						valueList.add(dateGetValues[i]);
						colorList.add(colorGetValues[i]);
						settingList.add(new Long(settingGetValues[i]));
					}
				}

				if (!isNotFoundError) {

					if (prmDropIndex <= valueList.size()) {
						valueList.add(prmDropIndex, dropValueDate);
						colorList.add(prmDropIndex, prmColor);
						settingList.add(displaySetting);
					} else {
						valueList.add(dropValueDate);
						colorList.add(prmColor);
						settingList.add(displaySetting);
					}

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

					// 順序更新対象リスト値存在チェック
					if (textGetValues[i].equals(prmValue)) {
						isNotFoundError = false;
					} else {
						valueList.add(textGetValues[i]);
						colorList.add(colorGetValues[i]);
						settingList.add(new Long(settingGetValues[i]));
					}
				}

				if (!isNotFoundError) {

					if (prmDropIndex <= valueList.size()) {
						valueList.add(prmDropIndex, prmValue);
						colorList.add(prmDropIndex, prmColor);
						settingList.add(displaySetting);
					} else {
						valueList.add(prmValue);
						colorList.add(prmColor);
						settingList.add(displaySetting);
					}

					textValues = (String[])valueList.toArray(new String[valueList.size()]);

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

				// 数値型
			case EIMValueType.DOUBLE:

				dropValueDouble = Double.parseDouble(prmValue);

				double[] doubleGetValues = attValMst.getDoubles();
				colorGetValues = attValMst.getColors();
				settingGetValues = attValMst.getSettings();

				for (int i = 0; i < doubleGetValues.length; i++) {

					// 順序更新対象リスト値存在チェック
					if (doubleGetValues[i] == dropValueDouble) {
						isNotFoundError = false;
					} else {
						valueList.add(new Double(doubleGetValues[i]));
						colorList.add(colorGetValues[i]);
						settingList.add(new Long(settingGetValues[i]));
					}
				}

				if (!isNotFoundError) {

					if (prmDropIndex <= valueList.size()) {
						valueList.add(prmDropIndex, new Double(dropValueDouble));
						colorList.add(prmDropIndex, prmColor);
						settingList.add(displaySetting);
					} else {
						valueList.add(new Double(dropValueDouble));
						colorList.add(prmColor);
						settingList.add(displaySetting);
					}

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
		} else {
			out.println(strBuf);
		}

		//Create Operation History
		OperationHistoryUtils.create(sess, AppConstant.SYSTEM,
				EIMConstant.UPDATE_ATTRIBUTE_ORDER, EIMConstant.TARGET_PARENT_ATTRIBUTE,
				EIMConstant.ATTRIBUTE_TYPE, attType,
				EIMConstant.TARGET_UPDATE, EIMConstant.ATTRIBUTE_MASTER,
				prmValue, Integer.toString(prmDropIndex + 1));

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
