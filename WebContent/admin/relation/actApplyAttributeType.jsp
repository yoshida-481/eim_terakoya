<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>

<%@page import="java.sql.Timestamp"%>
<%@page import="java.util.List" %>
<%@page import="java.util.Date" %>

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
	String prmRelationTypeId = request.getParameter("relationTypeId");
	String prmAttTypeId = request.getParameter("attTypeId");

	//Message
	String message = null;
	Object[] paramId = {
			"relTypeId=" + prmRelationTypeId,
			"attTypeId=" + prmAttTypeId,
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
		if(!AdminAuthUtil.hasSpecifiedOrGeneralAuth(loginUser, AppConstant.ADMIN_AUTH_ID_DOCUMENTTYPE))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.NOTADMINISTRATOR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.NOTADMINISTRATOR");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message));
			return;
		}

		//Relation Type
		EIMRelationType relationType = RelationUtils.getRelationTypeById(sess, Long.parseLong(prmRelationTypeId));

		if(relationType == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.RELTYPE.NOTFOUND");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.RELTYPE.NOTFOUND");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}

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

		//Apply
		RelationAttributeUtils.applyAttributeType(sess, relationType, attType);

		// 属性タイプのデフォルト値
		StringBuffer attValueListStr = new StringBuffer();
		if( attType != null )
		{
			List defaultValueList = attType.getDefaultValueList();
			
			if (defaultValueList != null || defaultValueList.size() > 0 ) {
				switch (attType.getValueType().getId()) {
					// 数値型
					case EIMValueType.INTEGER:
						for( int j=0; j<defaultValueList.size(); j++ )
						{
							long valueInt = Long.parseLong(defaultValueList.get(j).toString());
							attValueListStr.append(" <defValue value=\"" + valueInt + "\"/>");
						}
						
						break;
						// 文字列型とテキスト型
					case EIMValueType.STRING:
					case EIMValueType.TEXT:
						for( int j=0; j<defaultValueList.size(); j++ )
						{
							String valueStrt = defaultValueList.get(j).toString();
							attValueListStr.append(" <defValue value=\"" + StringUtils.xmlEncode(valueStrt) + "\"/>");
						}
						break;
					// 日付型
					case EIMValueType.DATE:
						// タイムゾーン対応した値を取得
						for( int j=0; j<defaultValueList.size(); j++ )
						{
							Date a = new Date(((Timestamp)defaultValueList.get(j)).getTime());
							attValueListStr.append(" <defValue value=\"" + DateUtils.getDBTzToCLTzDate(sess, a) + "\"/>");
						}
						break;
					// ダブル型
					case EIMValueType.DOUBLE:
						for( int j=0; j<defaultValueList.size(); j++ )
						{
							Double b = Double.parseDouble(defaultValueList.get(j).toString());
							attValueListStr.append(" <defValue value=\"" + FormatUtil.getDoubleFormatedString(b) + "\"/>");
						}
						break;
				}
			}
		}
		//複数値属性の入力欄が[追加]ボタンを押下してからでないと入力できないこと防止の為、空のカラム追加
		if (attValueListStr.length() == 0) {
			attValueListStr.append(" <defValue value=\"\"/>");
		}
		
		//Create Operation History
		OperationHistoryUtils.create(sess, AppConstant.SYSTEM, EIMConstant.APPLY_ATTRIBUTE_TO_RELATION_TYPE,
				EIMConstant.TARGET_UPDATE, EIMConstant.RELATION_TYPE, relationType,
				null, EIMConstant.ATTRIBUTE_TYPE, attType, null);
		//Commit
		sess.commit();

		//XML
		out.println("<attType");
			out.println(" attTypeId=\"" + attType.getId() + "\"");
			out.println(" attTypeName=\"" + StringUtils.xmlEncode(attType.getName()) + "\"");
			out.println(" valTypeId=\"" + attType.getValueType().getId() + "\"");
			out.println(" valTypeName=\"" + StringUtils.xmlEncode(attType.getValueType().getName()) + "\"");
			out.println(">");
			
			// デフォルト値のリスト
			out.println("<defValueList>");
				out.println(attValueListStr.toString());
			out.println("</defValueList>");
			
		out.println("</attType>");

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
