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
			"valType=" + prmValType,
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


		boolean isNotFoundError = true;
		long inputValueInt;
		Date inputValueDate = null;
		double inputValueDouble;

		try
		{
			switch (prmValType) {
			// 数値型
			case EIMValueType.INTEGER:

				inputValueInt = Long.parseLong(prmValue);

				long[] intGetValues = attValMst.getInts();

				for (int i = 0; i < intGetValues.length; i++) {
					// リスト値存在チェック
					if (intGetValues[i] == inputValueInt) {
						isNotFoundError = false;
						break;
					}
				}

				if (!isNotFoundError) {
					out.println("<definitionValue");
						out.println(" value=\"" + inputValueInt + "\"");
						out.println(" isDspColor=\"" + prmIsDspColor + "\"");
						out.println(" color=\"" + prmColor + "\"");
					out.println(">");
					out.println("</definitionValue>");
				}

				break;
			// 文字列型
			case EIMValueType.STRING:

				String[] strGetValues = attValMst.getStrings();

				for (int i = 0; i < strGetValues.length; i++) {
					// リスト値存在チェック
					if (strGetValues[i].equals(prmValue)) {
						isNotFoundError = false;
						break;
					}
				}

				if (!isNotFoundError) {
					out.println("<definitionValue");
						out.println(" value=\"" + StringUtils.xmlEncode(prmValue) + "\"");
						out.println(" isDspColor=\"" + prmIsDspColor + "\"");
						out.println(" color=\"" + prmColor + "\"");
					out.println(">");
					out.println("</definitionValue>");
				}

				break;
			// 日付型
			case EIMValueType.DATE:
				inputValueDate = DateUtils.editExpirationDate(sess,
						StringUtils.getDateFromString(prmValue, EIMResource.getMessage(sess, "EIM.FORMAT.DATE")));

				String judgeInputValue = StringUtils.getDateStringByFormat(inputValueDate, EIMResource.getMessage(sess, "EIM.FORMAT.DATE"));

				Date[] dateGetValues = attValMst.getDates();

				for (int i = 0; i < dateGetValues.length; i++) {
					String judgeGetValue = StringUtils.getDateStringByFormat(dateGetValues[i], EIMResource.getMessage(sess, "EIM.FORMAT.DATE"));
					// リスト値存在チェック
					if (judgeGetValue.equals(judgeInputValue)) {
						isNotFoundError = false;
						break;
					}
				}

				if (!isNotFoundError) {

					prmValue = DateUtils.getDBTzToCLTzDate(sess, inputValueDate, "EIM.FORMAT.DATE");

					out.println("<definitionValue");
						out.println(" value=\"" + prmValue + "\"");
						out.println(" isDspColor=\"" + prmIsDspColor + "\"");
						out.println(" color=\"" + prmColor + "\"");
					out.println(">");
					out.println("</definitionValue>");
				}

				break;
			// テキスト型
			case EIMValueType.TEXT:

				String[] textGetValues = attValMst.getTexts();

				for (int i = 0; i < textGetValues.length; i++) {
					// リスト値存在チェック
					if (textGetValues[i].equals(prmValue)) {
						isNotFoundError = false;
						break;
					}
				}

				if (!isNotFoundError) {
					out.println("<definitionValue");
						out.println(" value=\"" + StringUtils.xmlEncode(prmValue) + "\"");
						out.println(" isDspColor=\"" + prmIsDspColor + "\"");
						out.println(" color=\"" + prmColor + "\"");
					out.println(">");
					out.println("</definitionValue>");
				}

				break;

				// ダブル型
			case EIMValueType.DOUBLE:

				inputValueDouble = Double.parseDouble(prmValue);

				double[] doubleGetValues = attValMst.getDoubles();

				for (int i = 0; i < doubleGetValues.length; i++) {
					// リスト値存在チェック
					if (doubleGetValues[i] == inputValueDouble) {
						isNotFoundError = false;
						break;
					}
				}

				if (!isNotFoundError) {
					out.println("<definitionValue");
						out.println(" value=\"" + FormatUtil.getDoubleFormatedString(inputValueDouble) + "\"");
						out.println(" isDspColor=\"" + prmIsDspColor + "\"");
						out.println(" color=\"" + prmColor + "\"");
					out.println(">");
					out.println("</definitionValue>");
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

		// Not found value
		if (isNotFoundError) {
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.ATTRLISTVALUE.NOTFOUND");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.ATTRLISTVALUE.NOTFOUND");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}

		//Commit
		sess.commit();

	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), eime.getMessage(), paramId), eime);
	}
	catch(Exception e)
	{
		out.clear();
		message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
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
