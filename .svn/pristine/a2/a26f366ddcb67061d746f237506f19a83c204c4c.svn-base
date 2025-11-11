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

	//Message
	String message = null;
	Object[] paramId = {
			"attTypeId=" + prmAttTypeId,
			"valType=" + prmValType,
			"value=" + prmValue
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
		long delValueInt;
		double delValueDouble;
		int deleteIndex = 0;

		long[] intValues = null;
		double[] doubleValues = null;
		String[] strValues = null;
		Date[] dateValues = null;
		String[] textValues = null;
		List valueList = new ArrayList();

		strBuf.append("<definitionValuesList>");

		try
		{
			String[] colorGetValues = null;
			List colorValueList = new ArrayList();

			switch (prmValType) {
			// 数値型
			case EIMValueType.INTEGER:

				delValueInt = Long.parseLong(prmValue);

				long[] intGetValues = attValMst.getInts();
				colorGetValues = attValMst.getColors();

				for (int i = 0; i < intGetValues.length; i++) {

					// 削除対象リスト値存在チェック
					if (isNotFoundError && intGetValues[i] == delValueInt) {
						isNotFoundError = false;
						deleteIndex = i;
					}
					valueList.add(new Long(intGetValues[i]));
					colorValueList.add(colorGetValues[i]);
				}

				if (!isNotFoundError) {

					valueList.remove(deleteIndex);
					colorValueList.remove(deleteIndex);

					intValues = new long[valueList.size()];

					for (int i = 0; i < valueList.size(); i++) {

						Long inte = (Long)valueList.get(i);
						intValues[i] = inte.longValue();

						strBuf.append("<definitionValue");
							strBuf.append(" value=\"" + intValues[i] + "\"");
							strBuf.append(" color=\"" + colorValueList.get(i) + "\"");
						strBuf.append(">");
						strBuf.append("</definitionValue>");
					}

					// Delete Master
					AttributeMasterUtil.setIntAttributeValues(sess, attValMst, intValues);
					AttributeMasterUtil.setColorAttributeValues(sess, attValMst, colorValueList);
				}

				break;
			// 文字列型
			case EIMValueType.STRING:

				String[] strGetValues = attValMst.getStrings();
				colorGetValues = attValMst.getColors();

				for (int i = 0; i < strGetValues.length; i++) {

					// 削除対象リスト値存在チェック
					if (isNotFoundError && strGetValues[i].equals(prmValue)) {
						isNotFoundError = false;
						deleteIndex = i;
					}
					valueList.add(strGetValues[i]);
					colorValueList.add(colorGetValues[i]);
				}

				if (!isNotFoundError) {

					valueList.remove(deleteIndex);
					colorValueList.remove(deleteIndex);

					strValues = (String[])valueList.toArray(new String[valueList.size()]);

					for (int i = 0; i < valueList.size(); i++) {

						strBuf.append("<definitionValue");
							strBuf.append(" value=\"" + StringUtils.xmlEncode(strValues[i]) + "\"");
							strBuf.append(" color=\"" + colorValueList.get(i) + "\"");
						strBuf.append(">");
						strBuf.append("</definitionValue>");
					}

					// Delete Master
					AttributeMasterUtil.setStrAttributeValues(sess, attValMst, strValues);
					AttributeMasterUtil.setColorAttributeValues(sess, attValMst, colorValueList);
				}

				break;
			// 日付型
			case EIMValueType.DATE:

				// 削除対象リスト値(DB登録用に変換)
				Date delValueDate = DateUtils.editExpirationDate(sess,
						StringUtils.getDateFromString(prmValue, EIMResource.getMessage(sess, "EIM.FORMAT.DATE")));
				// 削除対象リスト値(判定用に変換)
				String judgeDelValue = StringUtils.getDateStringByFormat(delValueDate, EIMResource.getMessage(sess, "EIM.FORMAT.DATE"));

				Date[] dateGetValues = attValMst.getDates();
				colorGetValues = attValMst.getColors();

				for (int i = 0; i < dateGetValues.length; i++) {

					String judgeGetValue = StringUtils.getDateStringByFormat(dateGetValues[i], EIMResource.getMessage(sess, "EIM.FORMAT.DATE"));

					// 削除対象リスト値存在チェック
					if (isNotFoundError && judgeGetValue.equals(judgeDelValue)) {
						isNotFoundError = false;
						deleteIndex = i;
					}
					valueList.add(dateGetValues[i]);
					colorValueList.add(colorGetValues[i]);
				}

				if (!isNotFoundError) {

					valueList.remove(deleteIndex);
					colorValueList.remove(deleteIndex);

					dateValues = (Date[])valueList.toArray(new Date[valueList.size()]);

					for (int i = 0; i < valueList.size(); i++) {

						strBuf.append("<definitionValue");
							strBuf.append(" value=\"" + DateUtils.getDBTzToCLTzDate(sess, dateValues[i], "EIM.FORMAT.DATE") + "\"");
							strBuf.append(" color=\"" + colorValueList.get(i) + "\"");
						strBuf.append(">");
						strBuf.append("</definitionValue>");
					}

					// Delete Master
					AttributeMasterUtil.setDateAttributeValues(sess, attValMst, dateValues);
					AttributeMasterUtil.setColorAttributeValues(sess, attValMst, colorValueList);
				}
				break;
			// テキスト型
			case EIMValueType.TEXT:

				String[] textGetValues = attValMst.getTexts();
				colorGetValues = attValMst.getColors();

				for (int i = 0; i < textGetValues.length; i++) {

					// 削除対象リスト値存在チェック
					if (isNotFoundError && textGetValues[i].equals(prmValue)) {
						isNotFoundError = false;
						deleteIndex = i;
					}
					valueList.add(textGetValues[i]);
					colorValueList.add(colorGetValues[i]);
				}

				if (!isNotFoundError) {

					valueList.remove(deleteIndex);
					colorValueList.remove(deleteIndex);

					textValues = (String[])valueList.toArray(new String[valueList.size()]);

					for (int i = 0; i < valueList.size(); i++) {

						strBuf.append("<definitionValue");
							strBuf.append(" value=\"" + StringUtils.xmlEncode(textValues[i]) + "\"");
							strBuf.append(" color=\"" + colorValueList.get(i) + "\"");
						strBuf.append(">");
						strBuf.append("</definitionValue>");
					}

					// Delete Master
					AttributeMasterUtil.setTextAttributeValues(sess, attValMst, textValues);
					AttributeMasterUtil.setColorAttributeValues(sess, attValMst, colorValueList);
				}

				break;

				// ダブル型
			case EIMValueType.DOUBLE:

				delValueDouble = Double.parseDouble(prmValue);

				double[] doubleGetValues = attValMst.getDoubles();
				colorGetValues = attValMst.getColors();

				for (int i = 0; i < doubleGetValues.length; i++) {

					// 削除対象リスト値存在チェック
					if (isNotFoundError && doubleGetValues[i] == delValueDouble) {
						isNotFoundError = false;
						deleteIndex = i;
					}
					valueList.add(new Double(doubleGetValues[i]));
					colorValueList.add(colorGetValues[i]);
				}

				if (!isNotFoundError) {

					valueList.remove(deleteIndex);
					colorValueList.remove(deleteIndex);

					doubleValues = new double[valueList.size()];

					for (int i = 0; i < valueList.size(); i++) {

						doubleValues[i] = (Double)valueList.get(i);

						strBuf.append("<definitionValue");
							strBuf.append(" value=\"" + FormatUtil.getDoubleFormatedString(doubleValues[i]) + "\"");
							strBuf.append(" color=\"" + colorValueList.get(i) + "\"");
						strBuf.append(">");
						strBuf.append("</definitionValue>");
					}

					// Delete Master
					AttributeMasterUtil.setDoubleAttributeValues(sess, attValMst, doubleValues);
					AttributeMasterUtil.setColorAttributeValues(sess, attValMst, colorValueList);
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

		// 削除対象リスト値存在なしエラー
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
				EIMConstant.DELETE_ATTRIBUTE_MASTER, EIMConstant.TARGET_PARENT_ATTRIBUTE,
				EIMConstant.ATTRIBUTE_TYPE, attType,
				EIMConstant.TARGET_UPDATE, EIMConstant.ATTRIBUTE_MASTER,
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
