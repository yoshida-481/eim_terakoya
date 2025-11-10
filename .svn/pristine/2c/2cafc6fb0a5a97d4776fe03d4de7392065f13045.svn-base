<%@ page contentType="text/xml; charset=UTF-8" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>
<%@ page import = "common.bo.*" %>
<%@ page import = "java.util.*" %>

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

	//Message
	String message = null;
	Object[] paramId = {
			"attTypeId=" + prmAttTypeId
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
		if(!AdminAuthUtil.hasSpecifiedOrGeneralAuth(loginUser, AppConstant.ADMIN_AUTH_ID_ATTRIBUTE))
		{
			// 参照処理のためエラーメッセージ等は出力しない。
			// 該当データを表示させない
			out.println("<OK></OK>");
			return;
		}

		/*
		 * Get Attribute Type
		 */
		//Attribute Type
		EIMAttributeType attType = AttributeUtils.getAttributeTypeById(sess, Long.parseLong(prmAttTypeId));
		if(attType == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.ATTTYPE.NOTFOUND");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.ATTTYPE.NOTFOUND");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}

		//Get Attribute Value Master
		AttributeValueMaster attValMst = null;
		if(sess.getAttribute("ADMIN_APP_ID").equals(AppConstant.ADMIN_APP_ID_DOCUMENT)) {
			attValMst = AttributeMasterUtil.getAttributeValueMasterByAttTypeId(sess, attType.getId());
		}

		//Root Node
		out.println("<definitionValuesList>");

		if (attValMst == null) {
			//End Root Node
			out.println("</definitionValuesList>");
			return;
		}

		List valueList = new ArrayList();

		String[] strValues = null;

		try
		{

			switch (attType.getValueType().getId()) {
			// 数値型
			case EIMValueType.INTEGER:
				long[] intValues = attValMst.getInts();
				for (int i = 0; i < intValues.length; i++) {
					valueList.add(new Long(intValues[i]));
				}
				break;
			// 文字列型
			case EIMValueType.STRING:
				strValues = attValMst.getStrings();
				for (int i = 0; i < strValues.length; i++) {
					valueList.add(strValues[i]);
				}
				break;
			// 日付型
			case EIMValueType.DATE:
				Date[] dateValues = attValMst.getDates();
				for (int i = 0; i < dateValues.length; i++) {
					String dspDate = DateUtils.getDBTzToCLTzDate(sess, dateValues[i], "EIM.FORMAT.DATE");
					valueList.add(dspDate);
				}
				break;
			// テキスト型
			case EIMValueType.TEXT:
				strValues = attValMst.getTexts();
				for (int i = 0; i < strValues.length; i++) {
					valueList.add(strValues[i]);
				}
				break;
				// ダブル型
			case EIMValueType.DOUBLE:
				double[] doubleValues = attValMst.getDoubles();
				for (int i = 0; i < doubleValues.length; i++) {
					valueList.add(FormatUtil.getDoubleFormatedString(new Double(doubleValues[i])));
				}
				break;
			}
		}
		catch (EIMException eime)
		{
			// Case No Value
			if (eime.getMessageKey().equals("EIM.ERROR.COMMON.API.NO.ATTRIBUTE.MASTER.VALUE")) {
				//End Root Node
				out.println("</definitionValuesList>");
			}

			return;
		}

		String[] colorGetValues = attValMst.getColors();

		for (int i = 0; i < valueList.size(); i++) {
			out.println("<definitionValue");
				out.println(" value=\"" + StringUtils.xmlEncode(valueList.get(i).toString()) + "\"");
				out.println(" color=\"" + colorGetValues[i] + "\"");
			out.println(">");
			out.println("</definitionValue>");

		}

		//End Root Node
		out.println("</definitionValuesList>");

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
