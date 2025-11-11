<%@ page contentType="text/xml; charset=UTF-8" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>

<%@page import="java.sql.Timestamp"%>

<%
//-------------------------------------------------------------------------
//ワークスペース管理機能追加のため、admin/object/dspAttributeType.jspをコピー
// →ユーザの権限判定を削除
//※使用可能タイプ選択画面＞詳細情報＞属性情報取得
//														(2012/02/17)
//-------------------------------------------------------------------------

	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");
	
	//Session
	EIMSession sess = null;
	EIMUser loginUser = null;
	
	//Parameter
	String prmObjTypeId = request.getParameter("objTypeId");

	//Message
	String message = null;
	Object[] paramId = {
			"objTypeId=" + prmObjTypeId
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
		
		//Object Type
		EIMObjectType objType = ObjectUtils.getObjectTypeById(sess, Long.parseLong(prmObjTypeId));
		if(objType == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOTYPE");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOTYPE");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			return;
		}
		
		//All Attribute Types
		List attTypeList = ObjectAttributeUtils.getAttributeTypeList(sess, objType);

		String[] essentialAttributes = AppConstant.ESSENTIAL_ATTRIBUTE_DEFNAME;
		
		//Root Node
		out.println("<attTypes>");
		
		// 入力規則の存在判定用HashSet
		
		HashSet masterExistSet = new HashSet();
		masterExistSet = AttributeMasterUtil.getMasterExistSetByAttTypeIds(sess, attTypeList);
		
		for(int i = 0; i < attTypeList.size(); i++)
		{
			//Attribute Type
			EIMAttributeType attType = (EIMAttributeType)attTypeList.get(i);

			String isEssential = "false";
			
			//必須属性かどうか
			String defName = attType.getDefName();
			
			for (int j=0; j<essentialAttributes.length; j++)
			{
				if (defName.equals(essentialAttributes[j]))
					isEssential = "true";
			}
			
			// 入力規則
			String inputRuleValue;
			if (masterExistSet.contains(new Long(attType.getId()))) {
				inputRuleValue = EIMResource.getMessage(sess, "EIM.LABEL.ATTRIBUTE.INPUT.RULE.LIST");	// リスト定義
			} else {
				inputRuleValue = EIMResource.getMessage(sess, "EIM.LABEL.ATTRIBUTE.INPUT.RULE.NONE");	// なし
			}
			
			// 複数値属性
			String isMultipleValue;
			if (attType.isMultiple()) {
				isMultipleValue = EIMResource.getMessage(sess, "EIM.LABEL.ATTRIBUTE.MULTIPLE.MAINTENANCE");	// 保持する
			} else {
				isMultipleValue = EIMResource.getMessage(sess, "EIM.LABEL.ATTRIBUTE.MULTIPLE.NONE");		// なし
			}
			
			// 属性タイプのデフォルト値(デフォルト値が取り込まれていないので再取得)
			EIMAttributeType attTypeNew = AttributeUtils.getAttributeTypeById(sess, attType.getId());
	
			StringBuffer attValueListStr = new StringBuffer();
			if( attTypeNew != null )
			{
				List defaultValueList = attTypeNew.getDefaultValueList();
				
				if (defaultValueList != null || defaultValueList.size() > 0 ) {
					switch (attTypeNew.getValueType().getId()) {
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
			
			//XML Out
			out.println("<attType");
				out.println(" attTypeId=\"" + attType.getId() + "\"");
				out.println(" attTypeName=\"" + StringUtils.xmlEncode(attType.getName()) + "\"");
				out.println(" objTypeId=\"" + attType.getObjectType().getId() + "\"");
				out.println(" objTypeName=\"" + StringUtils.xmlEncode(attType.getObjectType().getName()) + "\"");
				out.println(" valTypeId=\"" + attType.getValueType().getId() + "\"");
				out.println(" valTypeName=\"" + StringUtils.xmlEncode(attType.getValueType().getName()) + "\"");
				out.println(" attTypeEssential=\"" + isEssential + "\"");
				
				//out.println(" attFieldName=\"" + "attType_" + attType.getId() + "\"");
				
				out.println(" inputRuleValue=\"" + inputRuleValue + "\"");		// 入力規則
				out.println(" isMultipleValue=\"" + isMultipleValue + "\"");	// 複数値属性
				
				out.println(">");
				
				// デフォルト値のリスト
				out.println("<defValueList>");
					out.println(attValueListStr.toString());
				out.println("</defValueList>");

			out.println("</attType>");
		}
		
		//End Root Node
		out.println("</attTypes>");
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
