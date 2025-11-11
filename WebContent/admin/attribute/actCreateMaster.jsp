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
	boolean prmIsDspColor = Boolean.valueOf(EIMUtils.getParameter(request, "isDspColor")).booleanValue();
	String prmColor = prmIsDspColor? EIMUtils.getParameter(request, "color") : "-";

	//Message
	String message = null;
	Object[] paramId = {
			"attTypeId=" + prmAttTypeId,
			"valTypeId=" + prmValType,
			"value=" + prmValue,
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
		boolean isExistsError = false;
		long inputValueInt;
		double inputValueDouble;
		Date inputValueDate = null;
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
				try
				{
					inputValueInt = Long.parseLong(prmValue);
				}
				catch(Exception e)
				{
					out.clear();
					message = EIMResource.getMessage(sess, "EIM.ERROR.INPUT.ILLEGAL.VALUE");
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					return;
				}

				long[] intGetValues = attValMst.getInts();
				colorGetValues = attValMst.getColors();
				settingGetValues = attValMst.getSettings();

				for (int i = 0; i < intGetValues.length; i++) {
					// 重複チェック
					if (intGetValues[i] == inputValueInt) {
						isExistsError = true;
						break;
					}

					valueList.add(new Long(intGetValues[i]));
					colorList.add(colorGetValues[i]);
					settingList.add(new Long(settingGetValues[i]));

					strBuf.append("<definitionValue");
						strBuf.append(" value=\"" + intGetValues[i] + "\"");
						strBuf.append(" color=\"" + colorGetValues[i] + "\"");
					strBuf.append(">");
					strBuf.append("</definitionValue>");
				}

				if (!isExistsError) {

					valueList.add(new Long(inputValueInt));
					colorList.add(prmIsDspColor? prmColor : "-");
					settingList.add(prmIsDspColor? new Long(1) : new Long(0));

					strBuf.append("<definitionValue");
						strBuf.append(" value=\"" + inputValueInt + "\"");
						strBuf.append(" color=\"" + prmColor + "\"");
					strBuf.append(">");
					strBuf.append("</definitionValue>");

					intValues = new long[valueList.size()];

					for (int i = 0; i < valueList.size(); i++) {
						Long inte = (Long)valueList.get(i);
						intValues[i] = inte.longValue();
					}

					// Create Master
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
					// 重複チェック
					if (strGetValues[i].equals(prmValue)) {
						isExistsError = true;
						break;
					}

					valueList.add(strGetValues[i]);
					colorList.add(colorGetValues[i]);
					settingList.add(new Long(settingGetValues[i]));

					strBuf.append("<definitionValue");
						strBuf.append(" value=\"" + StringUtils.xmlEncode(strGetValues[i]) + "\"");
						strBuf.append(" color=\"" + colorGetValues[i] + "\"");
					strBuf.append(">");
					strBuf.append("</definitionValue>");
				}

				if (!isExistsError) {

					valueList.add(prmValue);
					colorList.add(prmIsDspColor? prmColor : "-");
					settingList.add(prmIsDspColor? new Long(1) : new Long(0));

					strBuf.append("<definitionValue");
						strBuf.append(" value=\"" + StringUtils.xmlEncode(prmValue) + "\"");
						strBuf.append(" color=\"" + prmColor + "\"");
					strBuf.append(">");
					strBuf.append("</definitionValue>");

					strValues = (String[])valueList.toArray(new String[valueList.size()]);

					// Create Master
					AttributeMasterUtil.setStrAttributeValues(sess, attValMst, strValues);

					// 表示色をDBに格納
					AttributeMasterUtil.setColorAttributeValues(sess, attValMst, colorList);
					// 表示設定をDBに格納
					AttributeMasterUtil.setSettingAttributeValues(sess, attValMst, settingList);
				}

				break;
			// 日付型
			case EIMValueType.DATE:
				inputValueDate = DateUtils.editExpirationDate(sess,
						StringUtils.getDateFromString(prmValue, EIMResource.getMessage(sess, "EIM.FORMAT.DATE")));

				String judgeInputValue = StringUtils.getDateStringByFormat(inputValueDate, EIMResource.getMessage(sess, "EIM.FORMAT.DATE"));

				Date[] dateGetValues = attValMst.getDates();
				colorGetValues = attValMst.getColors();
				settingGetValues = attValMst.getSettings();

				for (int i = 0; i < dateGetValues.length; i++) {
					String judgeGetValue = StringUtils.getDateStringByFormat(dateGetValues[i], EIMResource.getMessage(sess, "EIM.FORMAT.DATE"));
					// 重複チェック
					if (judgeGetValue.equals(judgeInputValue)) {
						isExistsError = true;
						break;
					}

					valueList.add(dateGetValues[i]);
					colorList.add(colorGetValues[i]);
					settingList.add(new Long(settingGetValues[i]));

					strBuf.append("<definitionValue");
						// GMT対応
						strBuf.append(" value=\"" + DateUtils.getDBTzToCLTzDate(sess, dateGetValues[i], "EIM.FORMAT.DATE") + "\"");
						strBuf.append(" color=\"" + colorGetValues[i] + "\"");
					strBuf.append(">");
					strBuf.append("</definitionValue>");
				}

				if (!isExistsError) {

					valueList.add(inputValueDate);
					colorList.add(prmIsDspColor? prmColor : "-");
					settingList.add(prmIsDspColor? new Long(1) : new Long(0));

					// GMT対応
					prmValue = DateUtils.getDBTzToCLTzDate(sess, inputValueDate, "EIM.FORMAT.DATE");

					strBuf.append("<definitionValue");
						strBuf.append(" value=\"" + prmValue + "\"");
						strBuf.append(" color=\"" + prmColor + "\"");
					strBuf.append(">");
					strBuf.append("</definitionValue>");

					dateValues = (Date[])valueList.toArray(new Date[valueList.size()]);

					// Create Master
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
					// 重複チェック
					if (textGetValues[i].equals(prmValue)) {
						isExistsError = true;
						break;
					}

					valueList.add(textGetValues[i]);
					colorList.add(colorGetValues[i]);
					settingList.add(new Long(settingGetValues[i]));

					strBuf.append("<definitionValue");
						strBuf.append(" value=\"" + StringUtils.xmlEncode(textGetValues[i]) + "\"");
						strBuf.append(" color=\"" + colorGetValues[i] + "\"");
					strBuf.append(">");
					strBuf.append("</definitionValue>");
				}

				if (!isExistsError) {

					valueList.add(prmValue);
					colorList.add(prmIsDspColor? prmColor : "-");
					settingList.add(prmIsDspColor? new Long(1) : new Long(0));

					strBuf.append("<definitionValue");
						strBuf.append(" value=\"" + StringUtils.xmlEncode(prmValue) + "\"");
						strBuf.append(" color=\"" + prmColor + "\"");
					strBuf.append(">");
					strBuf.append("</definitionValue>");

					textValues = (String[])valueList.toArray(new String[valueList.size()]);

					// Create Master
					AttributeMasterUtil.setTextAttributeValues(sess, attValMst, textValues);

					// 表示色をDBに格納
					AttributeMasterUtil.setColorAttributeValues(sess, attValMst, colorList);
					// 表示設定をDBに格納
					AttributeMasterUtil.setSettingAttributeValues(sess, attValMst, settingList);
				}

				break;

				// 数値型
			case EIMValueType.DOUBLE:
				try
				{
					inputValueDouble = Double.parseDouble(prmValue);
				}
				catch(Exception e)
				{
					out.clear();
					message = EIMResource.getMessage(sess, "EIM.ERROR.INPUT.ILLEGAL.VALUE");
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					return;
				}
				double[] doubleGetValues = attValMst.getDoubles();
				colorGetValues = attValMst.getColors();
				settingGetValues = attValMst.getSettings();

				for (int i = 0; i < doubleGetValues.length; i++) {
					// 重複チェック
					if (doubleGetValues[i] == inputValueDouble) {
						isExistsError = true;
						break;
					}

					valueList.add(new Double(doubleGetValues[i]));
					colorList.add(colorGetValues[i]);
					settingList.add(new Long(settingGetValues[i]));

					strBuf.append("<definitionValue");
						strBuf.append(" value=\"" + FormatUtil.getDoubleFormatedString(doubleGetValues[i]) + "\"");
						strBuf.append(" color=\"" + colorGetValues[i] + "\"");
					strBuf.append(">");
					strBuf.append("</definitionValue>");
				}

				if (!isExistsError) {

					valueList.add(new Double(inputValueDouble));
					colorList.add(prmIsDspColor? prmColor : "-");
					settingList.add(prmIsDspColor? new Long(1) : new Long(0));

					strBuf.append("<definitionValue");
						strBuf.append(" value=\"" + FormatUtil.getDoubleFormatedString(inputValueDouble) + "\"");
						strBuf.append(" color=\"" + prmColor + "\"");
					strBuf.append(">");
					strBuf.append("</definitionValue>");

					doubleGetValues = new double[valueList.size()];

					for (int i = 0; i < valueList.size(); i++) {
						doubleGetValues[i] = (Double)valueList.get(i);
					}

					// Create Master
					AttributeMasterUtil.setDoubleAttributeValues(sess, attValMst, doubleGetValues);

					// 表示色をDBに格納
					AttributeMasterUtil.setColorAttributeValues(sess, attValMst, colorList);
					// 表示設定をDBに格納
					AttributeMasterUtil.setSettingAttributeValues(sess, attValMst, settingList);
				}

				break;
			}

			//表示色指定

		}
		catch (EIMException eime)
		{
			// Case get No Value
			if (eime.getMessageKey().equals("EIM.ERROR.COMMON.API.NO.ATTRIBUTE.MASTER.VALUE")) {
				// Create Master
				switch (prmValType) {
				// 数値型
				case EIMValueType.INTEGER:
					intValues = new long[1];
					intValues[0] = Long.parseLong(prmValue);
					AttributeMasterUtil.setIntAttributeValues(sess, attValMst, intValues);
					break;
				// 文字列型
				case EIMValueType.STRING:
					strValues = new String[1];
					strValues[0] = prmValue;
					AttributeMasterUtil.setStrAttributeValues(sess, attValMst, strValues);
					break;
				// 日付型
				case EIMValueType.DATE:
					dateValues = new Date[1];
					inputValueDate = DateUtils.editExpirationDate(sess,
							StringUtils.getDateFromString(prmValue, EIMResource.getMessage(sess, "EIM.FORMAT.DATE")));
					dateValues[0] = inputValueDate;
					prmValue = DateUtils.getDBTzToCLTzDate(sess, inputValueDate, "EIM.FORMAT.DATE");
					AttributeMasterUtil.setDateAttributeValues(sess, attValMst, dateValues);
					break;
				// テキスト型
				case EIMValueType.TEXT:
					textValues = new String[1];
					textValues[0] = prmValue;
					AttributeMasterUtil.setTextAttributeValues(sess, attValMst, textValues);
					break;
				// ダブル型
				case EIMValueType.DOUBLE:
					doubleValues = new double[1];
					doubleValues[0] = Double.parseDouble(prmValue);
					AttributeMasterUtil.setDoubleAttributeValues(sess, attValMst, doubleValues);
					break;
				}

				// 表示色をDBに格納
				colorList = new ArrayList();
				colorList.add(prmColor);
				AttributeMasterUtil.setColorAttributeValues(sess, attValMst, colorList);
				// 表示設定をDBに格納
				settingList = new ArrayList();
				settingList.add(prmIsDspColor? new Long(1) : new Long(0));
				AttributeMasterUtil.setSettingAttributeValues(sess, attValMst, settingList);

				strBuf.append("<definitionValue");
					strBuf.append(" value=\"" + StringUtils.xmlEncode(prmValue) + "\"");
					strBuf.append(" color=\"" + prmColor + "\"");
				strBuf.append(">");
				strBuf.append("</definitionValue>");
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

		// 重複エラー
		if (isExistsError) {
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
				EIMConstant.CREATE_ATTRIBUTE_MASTER, EIMConstant.TARGET_PARENT_ATTRIBUTE,
				EIMConstant.ATTRIBUTE_TYPE, attType,
				EIMConstant.TARGET_CREATE, EIMConstant.ATTRIBUTE_MASTER,
				prmValue, null);

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
